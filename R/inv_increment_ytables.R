# Copyright (C) 2026  Peter Biber, Astor Torano Caicoya
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


#' Estimate the Volume Increment of a Single Inventory With Yield Tables
#'
#' Covers the whole chain from a prepared tree list to a ready-to-aggregate
#' increment result. It is the yield-table counterpart of
#' \code{\link{inv_increment_repeated_survey}} and
#' \code{\link{inv_increment_gnfi3}}: all three return an object of class
#' \code{fe_increment}, carry their report header in \code{meta}, and are
#' consumed the same way.
#'
#' The symmetry stops where the method does. A yield-table increment is not
#' estimated per tree but for a \emph{cohort}: one species in the main stand of
#' one inventory plot, on the ideal (virtual monospecific) area it occupies
#' there. The estimate needs the cohort's age and its mean height, which give
#' the site index; age and site index give the per-hectare increment of the
#' yield table, which is then corrected by the stocking level (the cohort's
#' basal area over the basal area the table expects). Consequently this bundle
#' holds no per-tree increments, the method is confined to the main stand, and
#' the class tables cannot be filtered to arbitrary collectives the way the
#' per-tree strands can.
#'
#' A cohort has one age and one quadratic mean diameter, so the class an
#' evaluation is grouped by -- age class or diameter class -- is a \emph{label}
#' on the result, not part of the estimate. Both labels are carried, and
#' \code{\link{increment_ytables_base_table}} groups by whichever is asked for
#' without estimating anything again.
#'
#' @param inv An \code{\link{fe_inventory}} object -- the inventory the tree
#'   list was pulled from. Supplies the report header and the reference area for
#'   the operation-wide per-hectare increment.
#'
#' @param inv_trees Tree data frame as pulled from \code{inv} after height
#'   completion (\code{\link{height_complete_inventory}},
#'   \code{\link{fill_heights_back}}) and \code{\link{trees_add_essentials}}.
#'
#' @param ytable_selection A data frame that assigns names of existing
#'   \code{\link[ForestElementsR]{fe_yield_table}} objects to species codes.
#'   Must have a column \code{species_id} which contains the species codes (in a
#'   valid species coding provided by the package \code{ForestElementsR}) and a
#'   column \code{ytable_name} with the name of the corresponding
#'   \code{\link[ForestElementsR]{fe_yield_table}} object. See
#'   \code{\link[ForestElementsR]{yield_tables_for_species}} for predefined
#'   table selections, and \code{\link{ytables_bavrn_state_var_1_feneu}} for the
#'   selection FeNEU ships. If \code{species_id} is not unique, the function
#'   terminates with an error.
#'
#' @param plot_area_weight Logical, if TRUE (default), the single plots are
#'   weighted with the area they represent. If FALSE all plots are equally
#'   weighted.
#'
#' @return An object of class \code{fe_increment_ytables}, which inherits from
#'   \code{fe_increment}. A list with the elements
#'   \describe{
#'     \item{cohorts}{one row per inventory plot and species: age, quadratic
#'       mean diameter and height, ideal area, site index, stocking level and
#'       the annual increment. This is the estimate itself,}
#'     \item{overview}{the species-group overview of the whole inventory,}
#'     \item{meta}{the figures the reports print in their header,}
#'     \item{ytables_used}{the yield tables actually used, per species group,}
#'     \item{collective}{the collective descriptor, always the main stand.}
#'   }
#'
#' @seealso \code{\link{increment_ytables_base_table}} for the class tables,
#'   \code{\link{output_increment_overview_ytables_pdf}} for the overview
#'   report, \code{\link{inv_increment_gnfi3}} for the per-tree single-inventory
#'   counterpart.
#'
#' @family increment
#'
#' @export
#'
#' @examples
#'   inc <- inv_increment_ytables(
#'     inv       = data_ex3_sample_fe_inventory,
#'     inv_trees = data_ex3_sample_trees_essentials,
#'     ytable_selection = ytables_bavrn_state_var_1_feneu
#'   )
#'
#'   inc
#'
#'   inc$overview
#'
inv_increment_ytables <- function(inv,
                                  inv_trees,
                                  ytable_selection,
                                  plot_area_weight = TRUE) {

  stopifnot(is_fe_inventory(inv))

  # Check yield table selection against data
  check_rslt <- check_ytable_selection_vs_data(inv_trees, ytable_selection)
  if (!check_rslt$success) stop(check_rslt$message)

  # The estimate: one row per plot and species, carrying both class labels.
  # Determining the site index is by far the most expensive step of the whole
  # increment side, which is why it happens exactly once here and every later
  # aggregation works off the result.
  # Deliberately not piped: apply_ytables() wraps its body in
  # suppressWarnings() to hide the species-cast chatter, and R evaluates a
  # piped argument lazily inside the callee -- which would swallow the
  # multiple-ages warning plot_agg_for_yt() raises. Evaluating it first keeps
  # that warning visible.
  cohorts <- plot_agg_for_yt(inv_trees)
  cohorts <- apply_ytables(cohorts, ytable_selection = ytable_selection)

  overview <- .ytables_overview_from_cohorts(
    cohorts, plot_area_weight = plot_area_weight, fe_inv = inv
  )

  structure(
    list(
      cohorts      = cohorts,
      overview     = overview,
      meta         = list(source  = "ytable",
                          year    = max(inv_trees$time_yr, na.rm = TRUE),
                          area_ha = sum(inv$area_rep_ha),
                          n_plots = nrow(inv)),
      ytables_used = .ytables_used_list(inv_trees, ytable_selection),
      collective   = list(kind = "main_stand", label = "Hauptbestand",
                          definition = "layer_key == 1 & !removal")
    ),
    class = c("fe_increment_ytables", "fe_increment", "list")
  )
}


#' Test for the Class fe_increment_ytables
#'
#' @param x An object
#'
#' @return \code{TRUE} if \code{x} is an \code{fe_increment_ytables} object,
#'   \code{FALSE} otherwise
#'
#' @export
#'
#' @examples
#'   inc <- inv_increment_ytables(
#'     data_ex3_sample_fe_inventory, data_ex3_sample_trees_essentials,
#'     ytables_bavrn_state_var_1_feneu
#'   )
#'   is_fe_increment_ytables(inc)
#'
is_fe_increment_ytables <- function(x) {
  inherits(x, "fe_increment_ytables")
}


#' Print an fe_increment_ytables Object
#'
#' @param x An \code{fe_increment_ytables} object
#'
#' @param ... Other parameters (not used)
#'
#' @return \code{x}, invisibly
#'
#' @export
#'
#' @examples
#'   inv_increment_ytables(
#'     data_ex3_sample_fe_inventory, data_ex3_sample_trees_essentials,
#'     ytables_bavrn_state_var_1_feneu
#'   )
#'
print.fe_increment_ytables <- function(x, ...) {
  m   <- x$meta
  tot <- x$overview[x$overview$is_total_row, ]

  cat("<fe_increment_ytables>\n")
  cat("  Survey       :", m$year, "\n")
  cat(sprintf("  Area / plots : %.1f ha / %d\n", m$area_ha, m$n_plots))
  cat("  Method       : yield tables, single inventory\n")
  cat("  Collective   :", x$collective$label, "\n")
  cat("  Cohorts      :", nrow(x$cohorts), "(plot x species)\n")
  if (nrow(tot) > 0)
    cat(sprintf("  Increment    : %.2f m3/ha/yr\n", tot$zv_m3_ha_yr[1]))
  cat("  Use increment_ytables_base_table() on this object.\n")

  invisible(x)
}



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Follows: Internal functions to be applied in sequence to the pulled trees
# data frame after height completion and added essentials

# They bring us to age class and species group-wise volume increments

# ------------------------------------------------------------------------------



# The internal helpers in this file carry no @family tag on purpose. A family
# tag would list them in the "See Also" of every exported member of that
# family, offering the reader a link to something they cannot call.
#' Check Compatibility of a Yield Table Selection With Given Data
#'
#' @param inv_dat Inventory data frame (pulled from an
#'   \code{\link{fe_inventory}} object, with completed heights and essentials
#'   added
#'
#' @param ytable_selection Yield table selection to be tested against the data;
#'   see documentation of \code{\link{inv_increment_ytables}} for details
#'
#' @return A list comprising a flag called \code{success} (\code{TRUE} if check
#'   result ok, \code{FALSE} otherwise), and a character string called
#'   \code{message} that contains an error message in case the check is not
#'   successful
#'
#' @keywords internal
#'
check_ytable_selection_vs_data <- function(inv_dat, ytable_selection) {

  # initialize result
  check_rslt <- list(success = TRUE, message = "")

  # 1. Do the codings match?
  if (
    ForestElementsR::fe_species_get_coding(inv_dat$species_id) !=
    ForestElementsR::fe_species_get_coding(ytable_selection$species_id)
  ) {
    check_rslt$success <- FALSE
    check_rslt$message <-
      "species codings of ytable_selection do not match inventory data"

    return(check_rslt)
  }

  # 2. Duplicated species_id?
  if (sum(duplicated(ytable_selection$species_id))) {
    check_rslt$success <- FALSE
    check_rslt$message <- "duplicates in species_id of ytable_selection"

    return(check_rslt)
  }

  # 3. Are all species of the inventory data covered by the yield table
  #    selection
  crit <- purrr::map_lgl(
    unclass(inv_dat$species_id),
    .f = function(.x) !(.x %in% unclass(ytable_selection$species_id))
  )
  if (any(crit)) {
    # Name the uncovered species (code, name, tree count) so the caller can see
    # exactly what to add to the selection, rather than getting a bare message.
    unc_vec  <- inv_dat$species_id[crit]
    unc_uni  <- unc_vec[!duplicated(unclass(unc_vec))]
    unc_code <- unclass(unc_uni)
    unc_name <- tryCatch(format(unc_uni, spec_lang = "ger"),
                         error = function(e) as.character(unc_code))
    unc_n    <- as.integer(table(unclass(unc_vec))[as.character(unc_code)])
    ord      <- order(-unc_n, unc_code)
    detail   <- paste0(unc_code[ord], " (", unc_name[ord], ", ",
                       unc_n[ord], " trees)", collapse = "; ")

    check_rslt$success <- FALSE
    check_rslt$message <- paste0(
      "ytable_selection does not cover all species in data: ", detail
    )

    return(check_rslt)
  }

  # We arrive here only when everything works
  check_rslt
}



#' Reconstruct the Numeric Breaks of a Factor Produced by cut()
#'
#' Levels generated by \code{\link[base]{cut}} read \code{"(a,b]"}, so the break
#' vector can be read back off them. Used to classify an aggregated value with
#' exactly the breaks the incoming tree data were classified with, instead of
#' guessing an interval that might differ from the caller's.
#'
#' @param f An ordered factor as produced by \code{cut()}.
#'
#' @return A numeric vector of breaks, or \code{NULL} if the levels do not have
#'   the expected shape.
#'
#' @keywords internal
#'
.breaks_from_cut_levels <- function(f) {
  lv <- levels(f)
  if (length(lv) == 0L) return(NULL)
  if (!all(grepl("^\\([^,]+,[^]]+\\]$", lv))) return(NULL)
  lo <- suppressWarnings(as.numeric(sub("^\\(([^,]+),.*$", "\\1", lv)))
  hi <- suppressWarnings(as.numeric(sub("^.*,([^]]+)\\]$", "\\1", lv)))
  if (anyNA(lo) || anyNA(hi)) return(NULL)
  c(lo[1], hi)
}


#' Aggregate Inventory Data on Plot Level for Yield Table Application
#'
#' The aggregation includes only the main stand (layer key 1 and non-removal
#' trees).
#'
#' One cohort is one species on one inventory plot: a yield-table increment is
#' estimated for the main stand of a plot, split into the ideal (virtual
#' monospecific) area of each species. Such a cohort has one age and one
#' quadratic mean diameter by construction, so the class a report groups by is a
#' \emph{label} attached to the cohort, never part of its definition. Both
#' labels are therefore carried here, and the estimate is the same whichever of
#' them a table is later grouped by.
#'
#' @param x Tree data frame as pulled from an \code{fe_inventory} object after
#'   height completion (\code{\link{height_complete_inventory}}), and applying
#'   \code{\link{trees_add_essentials}}.
#'
#' @return An aggregated data frame with the plot level information required for
#'   applying yield tables, one row per plot and species, carrying both
#'   \code{age_class} and \code{d_q_class}.
#'
#' @keywords internal
#'
plot_agg_for_yt <- function(x) {

  # Main stand only.
  x <- x |> dplyr::filter(.data$layer_key == 1 & !.data$removal)

  # Yield-table increments are read off the table at the stand's age, so an age
  # is required. Checked here, after the main-stand filter, i.e. on exactly the
  # collective that is evaluated - an inventory whose understorey lacks ages can
  # still be evaluated for the main stand.
  .assert_age_complete(x, "inv_increment_ytables")

  # Report data that carry more than one age for the same species in the main
  # stand of one plot. Structurally there is one age per cohort; where the data
  # say otherwise the ages are averaged below, which is defensible but should
  # not happen silently.
  multi_age <- x |>
    dplyr::group_by(.data$plot_id, .data$layer_key, .data$species_id) |>
    dplyr::summarise(n_age = dplyr::n_distinct(.data$age_yr), .groups = "drop")
  n_multi <- sum(multi_age$n_age > 1)
  if (n_multi > 0L)
    warning(fe_msg("ytables_multiple_ages", n_multi, nrow(multi_age)),
            call. = FALSE)

  # The quadratic mean diameter class of each cohort, formed exactly as the
  # increment base tables form it (one per plot x species_group x layer).
  dq_classes <- back_table_dclass(x) |>
    dplyr::select("plot_id", "species_group", "layer_key", "d_q_class")
  x <- x |> dplyr::left_join(
    dq_classes, by = c("plot_id", "species_group", "layer_key")
  )

  # Breaks of the incoming age classification, so the cohort's own age class is
  # cut with the same interval the caller used on the trees.
  age_breaks <- .breaks_from_cut_levels(x$age_class)
  age_levels <- levels(x$age_class)

  suppressWarnings(suppressMessages( # Don't display species-cast chatter (now
                    # messages, formerly warnings) - this is part of the concept here
    x_list <- x |>
      dplyr::group_by(
        .data$plot_id, .data$area_rep_ha, .data$layer_key, .data$species_id,
        .data$species_group, .data$time_yr, .data$d_q_class
      ) |>
      dplyr::summarise(
        # Area-weighted, not weighted by stem number: the age describes the
        # ideal area the species occupies in this plot's main stand, and that
        # area is what the yield table is read for. d_q_cm and h_q_m below are
        # not averages but quadratic means computed over the cohort's trees
        # directly, which is why they need no such weight.
        age_yr      = stats::weighted.mean(
                        .data$age_yr,
                        w = .data$standing_area_m2 * .data$n_rep_ha
                      ),
        standing_area_m2_ha = sum(.data$standing_area_m2 * .data$n_rep_ha),
        d_q_cm      = ForestElementsR::d_q(.data$dbh_cm, n_rep = .data$n_rep_ha),
        h_q_m       = ForestElementsR::h_q(.data$height_m,
                                           .data$dbh_cm,
                                           n_rep = .data$n_rep_ha),
        g_m2_ha     = sum(.data$g_m2     * .data$n_rep_ha),
        v_hub_m3_ha = sum(.data$v_hub_m3 * .data$n_rep_ha),
      ) |>
      dplyr::select(-"v_hub_m3_ha") |>
      dplyr::group_by(.data$plot_id) |>
      dplyr::group_split() |>
      purrr::map(
        .f = function(.x) {
          .x <- .x |> dplyr::ungroup() # for safety reasons
          st_area_total <- sum(.x$standing_area_m2_ha)
          # Teilfläche can be max 1 ha
          .x$area_share_ha <- .x$standing_area_m2_ha / st_area_total
          # The rep area of the plot per species_group and age class must be
          # reduced accordingly
          .x$area_rep_ha_share <- .x$area_rep_ha * .x$area_share_ha
          # 'expd' means expanded to one virtual monospecific ha
          .x$g_m2_expd_ha <- .x$g_m2_ha / .x$area_share_ha
          .x |> dplyr::relocate("area_rep_ha_share",
                                .after = "area_rep_ha")
        }
      )
  )) # suppressWarnings + suppressMessages

  out <- do.call(rbind, x_list)

  # The cohort's own age class, cut with the breaks the trees were classified
  # with. Derived here rather than inherited from the trees: where the data
  # carry several ages for one cohort, the trees may sit in different classes
  # while the cohort - which is what the yield table is read for - has exactly
  # one age and therefore one class.
  if (!is.null(age_breaks)) {
    out$age_class <- cut(out$age_yr, breaks = age_breaks,
                         ordered_result = TRUE)
    levels(out$age_class) <- age_levels
  }

  out |> dplyr::relocate("age_class", .after = "age_yr")
}



#' Apply the Selected Yield Tables on Plot Level
#'
#' @param x A data frame, the output of \code{\link{plot_agg_for_yt}}
#'
#' @param ytable_selection A data frame assigning yield table names to species
#'   groups; typically produced by \code{\link{inv_increment_ytables}}.
#'
#' @return An extended version of the input \code{x}, containing yield table
#'   names, site index, stocking level and yield table increments. The
#'   increment \code{iv_hub_m3_ha_yr_yt} is the table's unreduced periodic
#'   annual increment (\code{pai_m3_ha_yr}, Vorratsfestmeter mit Rinde),
#'   corrected by the stocking level and then reduced exactly once to harvested
#'   volume under bark (Erntefestmeter ohne Rinde) via
#'   \code{\link[ForestElementsR]{v_red_harvest_ubark}}. The pre-reduced
#'   \code{red_pai_m3_ha_yr} is not used, because it is not guaranteed to be
#'   present in every yield table.
#'
#' @keywords internal
#'
apply_ytables <- function(x, ytable_selection) {

  suppressWarnings(suppressMessages( # Don't display species-cast chatter (now
                    # messages, formerly warnings) - this is part of the concept here
    x |>
      dplyr::left_join(ytable_selection, by = "species_id") |>
      dplyr::mutate(
        site_index = purrr::pmap_dbl(
          .l = list(
            age = .data$age_yr, h_q_m = .data$h_q_m, ytable = .data$ytable_name
          ),
          .f = function(age, h_q_m, ytable) {
            ForestElementsR::site_index(
              age,
              h_q_m,
              eval(parse(text = paste0("ForestElementsR::", ytable))),
              "h_q_m"
            )
          },
          .progress = "Bonitierung"
        ),
        # Keep site index inside reasonable bounds
        # Unfortunately, we have to make a difference between Assmann-Franz and
        # all other tables, because they are using different site indexing
        # sytems
        site_index = purrr::map2_dbl(
          .x = .data$site_index,
          .y = .data$ytable_name,
          .f = function(.x, .y) {
            if (
              .y != "fe_ytable_spruce_assmann_franz_mean_yield_level_1963"
            ) {
              ifelse(
                .x < -1, # site index over the top
                -1,
                ifelse(
                  .x >   # under the bottom use bottom
                    eval(
                      parse(text = paste0(
                        "max(ForestElementsR::", .y, "$site_indexes)"
                      ))
                    ),
                  eval(
                    parse(text = paste0(
                      "max(ForestElementsR::", .y, "$site_indexes)"
                    ))
                  ),
                  .x # nothing to do
                )
              )
            } else { # Assmann-Franz site index (top height at age 100)
              ifelse(
                .x > 44, # site index over the top
                44,
                ifelse(
                  .x < eval( # under the bottom use bottom
                    parse(text = paste0(
                      "min(ForestElementsR::", .y, "$site_indexes)"
                    ))
                  ),
                  eval( # under the bottom
                    parse(text = paste0(
                      "min(ForestElementsR::", .y, "$site_indexes)"
                    ))
                  ),
                  .x # nothing to do
                )
              )
            }
          } # .f
        ),
        # Now the stocking level (German "Bestockungsgrad")
        # Previously, the stocking level was set to 0 for d_q_cm < 6.1
        # (the v_gri merchantable wood volume threshold), but this mechanism
        # is currently disabled
        stocking_level = purrr::pmap_dbl(
          .l = list(
            ba = .data$g_m2_expd_ha, age = .data$age_yr, si = .data$site_index,
            ytable = .data$ytable_name
          ),
          .f = function(ba, age, si, ytable) {
            ForestElementsR::stocking_level(
              ba, age, si,
              eval(parse(text = paste0("ForestElementsR::", ytable)))
            )
          },
          .progress = "Bestockungsgrad"
        ),
        # The actual volume increment. We deliberately look up the *unreduced*
        # periodic annual increment ("pai_m3_ha_yr", Vorratsfestmeter mit Rinde)
        # and reduce it ourselves with v_red_harvest_ubark() below. The yield
        # tables also carry a pre-reduced "red_pai_m3_ha_yr" (Erntefestmeter ohne
        # Rinde), but "pai" is guaranteed to exist in *every* yield table whereas
        # "red_pai" is not -- so taking pai and reducing it exactly once is the
        # robust choice that never double-reduces an already-reduced value.
        iv_hub_m3_ha_yr_yt = purrr::pmap_dbl(
          .l = list(age    = .data$age_yr,
                    si     = .data$site_index,
                    ytable = .data$ytable_name,
                    sl     = .data$stocking_level),
          .f = function(age, si, ytable, sl) {
            ForestElementsR::ytable_lookup(
              age, si, "pai_m3_ha_yr",
              eval(
                parse(text = paste0("ForestElementsR::", ytable)))
              ) * min(1, sl)
          },
          .progress = "Zuwachsnachschlag"
        ),
        # Reduce the unreduced pai once to harvested volume under bark (Efm o.R.)
        iv_hub_m3_ha_yr_yt = ForestElementsR::v_red_harvest_ubark(
          .data$species_group, .data$iv_hub_m3_ha_yr_yt
        )
    )
  )) # suppressWarnings + suppressMessages
}


#' Aggregate Yield Table Increments From Plot Level to Inventory Level
#'
#' @param x Output of apply_ytables()
#'
#' @param plot_area_weight Logical, if TRUE, the single plots are
#'   weighted with the area they represent. If FALSE all plots are equally
#'   weighted
#'
#' @return A data frame (tibble) with the grouping variables
#'   \code{species_group}, and \code{age_class}. For each so-defined group the
#'   tibble provides the area/ha, the average site index, and the yield table
#'   average increments per year and ha.
#'
#' @keywords internal
#'
agg_ha_yt_inc_main_stand_age_class <- function(x, plot_area_weight) {

  if (plot_area_weight) x$weights <- x$area_rep_ha_share
  else                  x$weights <- 1

  x |>
    dplyr::group_by(.data$species_group, .data$age_class) |>
    dplyr::summarise(
      area_ha            = sum(.data$area_rep_ha_share),
      site_index         = stats::weighted.mean(.data$site_index,
                                                .data$weights),
      iv_hub_m3_ha_yr_yt = stats::weighted.mean(.data$iv_hub_m3_ha_yr_yt,
                                                .data$weights)
    )
}



# Internal worker for increment_ytables_base_table(): aggregate the per-cohort
# yield-table increments (output of apply_ytables, extended with a .w weight
# column) to one grouping level. Mirrors the column set of the increment
# main-stand base tables: area_ha is the ideal (virtual monospecific) species
# area, iv_m3_ha_yr_virt the increment per that ideal area, iv_m3_ha_yr the
# increment over the whole operation area (ref_area). Plus the yield-table
# specific site_index (Ertragsklasse). No confidence interval (yield tables
# carry none).
.increment_ytables_level <- function(per, group_cols, ref_area) {
  per |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_plot_class   = dplyr::n_distinct(.data$plot_id),
      area_ha        = sum(.data$area_rep_ha_share),
      site_index     = stats::weighted.mean(.data$site_index, .data$.w),
      iv_m3_yr_total = sum(.data$area_rep_ha_share * .data$iv_hub_m3_ha_yr_yt),
      .groups        = "drop"
    ) |>
    dplyr::mutate(
      iv_m3_ha_yr      = .data$iv_m3_yr_total / ref_area,
      iv_m3_ha_yr_virt = .data$iv_m3_yr_total / .data$area_ha,
      # No CI for yield tables; keep the columns (as NA) so the shared PDF
      # template's CI merge finds them and simply renders nothing.
      ci95_iv_m3_yr_total = NA_real_,
      ci95_iv_m3_ha_yr    = NA_real_
    )
}



#' Main-Stand Yield-Table Increment Base Table by Species Group and Age or
#' Diameter Class
#'
#' Yield-table (\dQuote{Ertragstafel}) counterpart of
#' \code{\link{increment_base_table_main_stand}}. It breaks the yield-table
#' increment estimate down by species group and age or quadratic-mean-diameter
#' class, for the main stand only, and returns the same four-element layout as
#' the other increment base tables so it flows through
#' \code{\link{output_increment_base_table}} and
#' \code{\link{output_increment_base_table_pdf}} unchanged.
#'
#' Yield tables are age-indexed and their increment is expressed per hectare of a
#' (virtual) monospecific stand, so this table exists \emph{only} for the main
#' stand and always relates to ideal species areas -- there is no
#' \dQuote{all layers} variant. Each species-group cohort on a plot receives its
#' yield-table increment (see \code{\link{inv_increment_ytables}}), which is then
#' aggregated to the inventory level. For \code{by_class = "dq"} the cohorts are
#' grouped by their quadratic mean diameter class (one per plot x species group,
#' as in \code{\link{back_table_dclass}}); the underlying per-cohort increment is
#' still the age-indexed yield-table value.
#'
#' The confidence-interval columns are absent by design: only the repeated-
#' inventory method carries an empirical CI.
#'
#' @param inv_dat Normally the result of \code{\link{inv_increment_ytables}} --
#'   the estimate is then taken from it and nothing is computed twice, which
#'   matters because determining the site index is the most expensive step of
#'   the increment side. Alternatively a tree data frame as pulled from an
#'   \code{\link{fe_inventory}} object after height completion
#'   (\code{\link{height_complete_inventory}}) and
#'   \code{\link{trees_add_essentials}}, in which case
#'   \code{ytable_selection} is required and the estimate is made here.
#'
#' @param ytable_selection A data frame assigning yield-table names to species,
#'   e.g. \code{\link{ytables_bavrn_state_var_1_feneu}}. Must cover all main-stand
#'   species in the data (see \code{\link{inv_increment_ytables}}). Not needed
#'   when \code{inv_dat} is an \code{fe_increment_ytables} bundle.
#'
#' @param by_class Either \code{"age"} (default) or \code{"dq"}. Selects how the
#'   result is grouped, not what is estimated: a cohort has one age and one
#'   quadratic mean diameter, so both are labels on the same estimate and the
#'   two axes reach the same inventory total.
#'
#' @param plot_area_weight Logical; if \code{TRUE} (default) plots are weighted by
#'   their represented area, otherwise equally. Ignored when \code{inv_dat} is a
#'   bundle, which was built with its own setting.
#'
#' @param fe_inv Optional \code{\link{fe_inventory}} object. If supplied, the
#'   operation-wide per-ha increment (\code{iv_m3_ha_yr}) relates to the full
#'   operation area and the header meta (year, area, point count) is filled;
#'   otherwise the stocked main-stand ideal area is used as the reference. Taken
#'   from the bundle when one is supplied.
#'
#' @returns A list of four tibbles (\code{detail}, \code{total},
#'   \code{all_species}, \code{all_total}) plus a \code{collective} element (fixed
#'   to \dQuote{Hauptbestand}), a \code{meta} element (marking the source as
#'   \code{"ytable"} so the PDF is titled \dQuote{Ertragstafelschätzung}), and a
#'   \code{ytables_used} element (the yield tables actually used, carried through
#'   so the PDF can list them). Each
#'   table carries \code{n_plot_class}, \code{iv_m3_yr_total}, \code{iv_m3_ha_yr}
#'   (over the operation area), \code{area_ha} (ideal species area), and
#'   \code{iv_m3_ha_yr_virt} (increment per ideal area). The per-species levels
#'   (\code{detail}, \code{total}) additionally carry \code{site_index} (mean
#'   Ertragsklasse); it is omitted from the cross-species \code{all_species} /
#'   \code{all_total} block, where averaging a site index would be meaningless.
#'
#' @family increment
#'
#' @export
#'
#' @examples
#'   inc <- inv_increment_ytables(
#'     data_ex3_sample_fe_inventory, data_ex3_sample_trees_essentials,
#'     ytables_bavrn_state_var_1_feneu
#'   )
#'
#'   # Both axes rest on the same estimate
#'   increment_ytables_base_table(inc, by_class = "age")$all_total
#'   increment_ytables_base_table(inc, by_class = "dq")$all_total
#'
increment_ytables_base_table <- function(inv_dat, ytable_selection,
                                         by_class = c("age", "dq"),
                                         plot_area_weight = TRUE,
                                         fe_inv = NULL) {

  by_class  <- match.arg(by_class)
  class_col <- if (by_class == "age") "age_class" else "d_q_class"

  # The usual case: the estimate has already been made and comes in as a bundle.
  # Taking it from there is not only cheaper, it also guarantees that the class
  # tables and the overview of one evaluation rest on the very same figures.
  if (is_fe_increment_ytables(inv_dat)) {
    bundle       <- inv_dat
    per          <- bundle$cohorts
    ytables_used <- bundle$ytables_used
    if (is.null(fe_inv)) {
      ref_area <- bundle$meta$area_ha
      n_plots  <- bundle$meta$n_plots
      inv_year <- bundle$meta$year
    }
  } else {
    bundle <- NULL
    if (missing(ytable_selection))
      stop(fe_msg("ytables_selection_required"), call. = FALSE)

    # Coverage check (same as inv_increment_ytables()).
    chk <- check_ytable_selection_vs_data(inv_dat, ytable_selection)
    if (!chk$success) stop(chk$message)

    # Per-cohort yield-table increments. The cohort is one species on one plot
    # and carries both class labels, so by_class selects how the result is
    # grouped, not what is estimated.
    # Not piped, for the same reason as in inv_increment_ytables().
    per          <- plot_agg_for_yt(inv_dat)
    per          <- apply_ytables(per, ytable_selection = ytable_selection)
    ytables_used <- .ytables_used_list(inv_dat, ytable_selection)
  }
  per$.w <- if (plot_area_weight) per$area_rep_ha_share else 1

  # Reference area for the operation-wide per-ha increment. An explicitly
  # supplied inventory wins; otherwise the bundle's header figures are used,
  # and failing that the stocked main-stand ideal area.
  if (!is.null(fe_inv)) {
    ref_area <- sum(fe_inv$area_rep_ha)
    n_plots  <- nrow(fe_inv)
    inv_year <- max(per$time_yr, na.rm = TRUE)
  } else if (is.null(bundle)) {
    ref_area <- sum(per$area_rep_ha_share)
    n_plots  <- NULL
    inv_year <- NULL
  }

  # Column order = row order in the rendered table. site_index
  # ("Ertragskl. (mittl.)") sits right below n_plot_class ("Anz. Pkte."),
  # because determining the site index (Ertragsklasse) is the prerequisite for
  # the yield-table increment. It is carried only in the per-species levels
  # (detail / total): averaging a site index across species groups is
  # meaningless, so it is dropped from the cross-species "Summe" block
  # (all_species / all_total) -- which makes output_base_table() omit the row
  # there entirely (the two blocks are pivoted independently).
  col_order <- function(df, keys, with_si) {
    cols <- c(keys, "n_plot_class",
              if (with_si) "site_index",
              "iv_m3_yr_total", "ci95_iv_m3_yr_total",
              "iv_m3_ha_yr", "ci95_iv_m3_ha_yr",
              "area_ha", "iv_m3_ha_yr_virt")
    dplyr::select(df, dplyr::all_of(cols))
  }

  detail      <- .increment_ytables_level(per, c("species_group", class_col),
                                          ref_area) |>
    col_order(c("species_group", class_col), with_si = TRUE)
  total       <- .increment_ytables_level(per, "species_group", ref_area) |>
    col_order("species_group", with_si = TRUE)
  all_species <- .increment_ytables_level(per, class_col, ref_area) |>
    col_order(class_col, with_si = FALSE)
  all_total   <- .increment_ytables_level(
    dplyr::mutate(per, .allg = 1L), ".allg", ref_area
  ) |>
    dplyr::select(-".allg") |>
    col_order(character(0), with_si = FALSE)

  meta <- if (!is.null(n_plots))
    list(source  = "ytable",
         year    = inv_year,
         area_ha = ref_area,
         n_plots = n_plots)
  else list(source = "ytable")

  list(
    detail       = detail,
    total        = total,
    all_species  = all_species,
    all_total    = all_total,
    collective   = list(kind = "main_stand", label = "Hauptbestand",
                        definition = "layer_key == 1 & !removal"),
    meta         = meta,
    # The yield tables actually used, carried through to the PDF so the class
    # table can list them (one per line) as the enterprise overview does.
    ytables_used = ytables_used
  )
}




# # ha wise aggregation over species groups
# # x: Output of agg_ha_yt_inc_main_stand_age_class()
# agg_ha_yt_inc_main_stand <- function(x) {
#
#   x |>
#     group_by(species_group) |>
#     summarise(
#       site_index         = weighted.mean(site_index, area_ha),
#       iv_hub_m3_ha_yr_yt = weighted.mean(iv_hub_m3_ha_yr_yt, area_ha),
#       area_ha            = sum(area_ha)
#     ) |>
#     relocate(area_ha, .before = site_index)
# }



# Internal: the yield tables actually used, one row per (species group, yield
# table), with the German (name_orig) and international (name_international)
# names. Shared by the yield-table overview and the yield-table class tables.
# Joining on the fe_species species_id avoids casting it to character.
#
# Rows are ordered by species group, and WITHIN a group by descending
# significance of the served species, so that when a species group uses several
# tables the one covering the more important species (larger represented standing
# area) is listed first. The significance weight is the represented standing area
# (standing_area_m2 * n_rep_ha) of the main-stand species the table is applied to
# -- a rough but low-effort proxy for a species' importance in the enterprise.
.ytables_used_list <- function(inv_dat, ytable_selection) {
  ms <- inv_dat |> dplyr::filter(.data$layer_key == 1, !.data$removal)

  # Significance weight per species (represented standing area).
  wt <- ms |>
    dplyr::group_by(.data$species_id, .data$species_group) |>
    dplyr::summarise(
      .wt = sum(.data$standing_area_m2 * .data$n_rep_ha, na.rm = TRUE),
      .groups = "drop"
    )

  wt |>
    dplyr::left_join(ytable_selection, by = "species_id") |>
    dplyr::filter(!is.na(.data$ytable_name)) |>
    dplyr::group_by(.data$species_group, .data$ytable_name) |>
    dplyr::summarise(.wt = sum(.data$.wt), .groups = "drop") |>
    dplyr::mutate(
      name_orig = vapply(
        .data$ytable_name,
        function(n) eval(parse(text = paste0("ForestElementsR::", n)))$name_orig,
        character(1)
      ),
      name_international = vapply(
        .data$ytable_name,
        function(n) {
          eval(parse(text = paste0("ForestElementsR::", n)))$name_international
        },
        character(1)
      )
    ) |>
    # species group in coding order; within a group, most significant table first
    dplyr::arrange(.data$species_group, dplyr::desc(.data$.wt)) |>
    dplyr::select("species_group", "ytable_name", "name_orig",
                  "name_international")
}



#' Species-Group Overview From the Per-Cohort Yield-Table Increments
#'
#' Aggregates the cohort-level estimate (\code{\link{plot_agg_for_yt}} followed
#' by \code{\link{apply_ytables}}) to one row per species group plus a total
#' row. All volume figures are harvested volume under bark (Erntefestmeter ohne
#' Rinde); the collective is the main stand, because yield-table species areas
#' are only formed there.
#'
#' @param cohorts Per-cohort estimate, one row per inventory plot and species.
#'
#' @param plot_area_weight Logical, if TRUE (default), cohorts are weighted with
#'   the ideal area they represent; if FALSE all count equally.
#'
#' @param fe_inv Optional \code{\link{fe_inventory}} object. If supplied, the
#'   per-hectare total is expressed over the \emph{entire} operation area, and
#'   the area of points that carry no living main stand -- treeless points as
#'   well as points whose only trees are removals or belong to other layers --
#'   appears as a separate \code{is_no_main_stand_row} so that the areas add up
#'   to the operation total. Those points contribute no yield-table increment.
#'
#' @return A tibble with one row per species group, an optional
#'   \code{is_no_main_stand_row}, and a total row whose \code{site_index} is
#'   \code{NA} because the index scales differ between yield tables.
#'
#' @keywords internal
#'
.ytables_overview_from_cohorts <- function(cohorts, plot_area_weight = TRUE,
                                           fe_inv = NULL) {

  # Cohort level to species-group level. The weight is the cohort's ideal area
  # share, or 1 when the plots are to count equally.
  zb <- cohorts
  zb$.w <- if (plot_area_weight) zb$area_rep_ha_share else 1
  zb <- zb |>
    dplyr::group_by(.data$species_group, .data$age_class) |>
    dplyr::summarise(
      area_ha            = sum(.data$area_rep_ha_share),
      site_index         = stats::weighted.mean(.data$site_index, .data$.w),
      iv_hub_m3_ha_yr_yt = stats::weighted.mean(.data$iv_hub_m3_ha_yr_yt,
                                                .data$.w),
      .groups            = "drop"
    )

  # Overview per species group
  by_sp <- zb |>
    dplyr::group_by(.data$species_group) |>
    dplyr::summarise(
      site_index  = stats::weighted.mean(.data$site_index, .data$area_ha),
      zv_m3_ha_yr = stats::weighted.mean(.data$iv_hub_m3_ha_yr_yt, .data$area_ha),
      zv_m3_yr    = sum(.data$area_ha * .data$iv_hub_m3_ha_yr_yt),
      area_ha     = sum(.data$area_ha),
      .groups     = "drop"
    ) |>
    dplyr::arrange(.data$species_group) |>
    dplyr::mutate(is_total_row = FALSE, is_no_main_stand_row = FALSE) |>
    dplyr::relocate("area_ha", .after = "species_group")

  main_stand_area <- sum(by_sp$area_ha)
  total_zv        <- sum(by_sp$zv_m3_yr)

  # With the full inventory supplied, the per-ha total is expressed over the
  # entire operation area. The area of points carrying no living main stand
  # (treeless points, or points whose only trees are removals or belong to
  # other layers) is shown as a separate row so the areas add up to the
  # operation total; those points contribute no yield-table increment.
  no_ms_row  <- NULL
  total_area <- main_stand_area
  if (!is.null(fe_inv)) {
    total_area <- sum(fe_inv$area_rep_ha)
    residual   <- total_area - main_stand_area
    if (residual < 0) residual <- 0                 # guard rounding noise
    if (residual > 1e-6) {
      no_ms_row <- tibble::tibble(
        species_group        = by_sp$species_group[NA_integer_],
        area_ha              = residual,
        site_index           = NA_real_,
        zv_m3_ha_yr          = NA_real_,
        zv_m3_yr             = NA_real_,
        is_total_row         = FALSE,
        is_no_main_stand_row = TRUE
      )
    }
  }

  total_row <- tibble::tibble(
    species_group        = by_sp$species_group[NA_integer_],
    area_ha              = total_area,
    site_index           = NA_real_,                # index scales differ
    zv_m3_ha_yr          = total_zv / total_area,
    zv_m3_yr             = total_zv,
    is_total_row         = TRUE,
    is_no_main_stand_row = FALSE
  )
  dplyr::bind_rows(by_sp, no_ms_row, total_row)
}

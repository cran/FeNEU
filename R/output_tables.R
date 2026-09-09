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

#' Derive Renderer Meta-Information from an Output Table
#'
#' Reads the self-describing attributes an output base/structure table carries
#' (\code{class_type} and \code{tree_selection}, stamped by
#' \code{\link{output_base_table}} / \code{\link{output_structure_table}}) and
#' turns them into everything the \code{*_pdf()} functions need without a
#' user-supplied \code{type} or \code{layer_filter} argument.
#'
#' @param x An output base or structure table.
#' @param fn_name Calling function name, used only in the error message.
#'
#' @return A list with \code{type} (\code{"age"}/\code{"dq"}), \code{cohort}
#'   (the raw tree-selection tag: \code{"mainstand"}, \code{"alllayers"}, or a
#'   raw \code{tree_filter} expression), and \code{file_tag} (the cohort in a
#'   file-name-safe form).
#'
#' @keywords internal
#'
.table_pdf_meta <- function(x, fn_name) {
  type <- attr(x, "class_type")
  if (is.null(type)) stop(fe_msg("pdf_class_type_missing", fn_name), call. = FALSE)

  # Cohort tag: prefer the tag stamped by the *_table_*() functions; fall back
  # to content-based detection (main-stand tables carry area_ha) for objects
  # built without it.
  cohort <- attr(x, "tree_selection")
  if (is.null(cohort)) {
    cohort <- if ("area_ha" %in% x$variable) "mainstand" else "alllayers"
  }

  # The two standard tags are already file-safe; a raw custom tree_filter
  # expression is sanitised so it can go into a file name.
  file_tag <- if (cohort %in% c("mainstand", "alllayers")) cohort
              else .safe_filename_part(cohort, "filter")

  list(type = type, cohort = cohort, file_tag = file_tag)
}

#' Warn When Structure Table Input Shows No Concentric-Circle Structure
#'
#' Structure tables break an inventory down by single-tree diameter class within
#' age or dq classes. That is worth doing for a sample inventory, where the
#' diameter distribution is estimated from a sample of trees; for a standwise
#' inventory the same table can be computed but says much less. Nothing is
#' refused here - the table is produced either way - the caller is only told
#' what the data look like.
#'
#' What can actually be observed is \code{dbh_trshld}: \code{\link{pull_trees}}
#' fills that column with each tree's concentric-circle threshold if - and only
#' if - the plot is an \code{fe_ccircle_spatial}, and leaves it \code{NA}
#' otherwise. An all-\code{NA} column therefore means "no concentric-circle
#' structure in these data", which today implies a standwise inventory but will
#' not stay equivalent to it: a sample inventory taken by relascope would look
#' exactly the same here while being a perfectly legitimate case. That is why
#' this is a warning naming the observation, not a rule refusing a combination -
#' the wording has to survive the day FeNEU supports more (type x method)
#' pairs. Deciding it properly needs the inventory type declared on the
#' \code{\link{fe_inventory}} object and carried down; that is a post-CRAN item
#' recorded in CLAUDE.md. An attribute cannot do the carrying, incidentally -
#' measured: \code{group_by()} plus \code{ungroup()} already drops one, and the
#' aggregation code groups constantly.
#'
#' @param inv_trees_plus A tree list as passed to a \code{structure_table_*()}
#'   function.
#' @param fn_name Calling function name, used only in the message.
#'
#' @return Invisibly \code{TRUE}; called for the warning it may raise.
#'
#' @keywords internal
#'
.warn_if_not_sample_inventory <- function(inv_trees_plus, fn_name) {
  trshld <- inv_trees_plus[["dbh_trshld"]]
  if (is.null(trshld) || all(is.na(trshld)))
    warning(fe_msg("structure_table_maybe_standwise", fn_name), call. = FALSE)
  invisible(TRUE)
}

#' Format Structure Tables for Output (Strukturtabellen)
#'
#' A data frame with group-wise aggregated inventory information separated per
#' species is returned.
#'
#' @param structure_table structure table generated from
#'  \code{\link{structure_table_d_q_class_main_stand}},
#'  \code{\link{structure_table_age_class_main_stand}},
#'  \code{\link{structure_table_d_q_class}}, or
#'  \code{\link{structure_table_age_class}}. This is a list with *detail*,
#'  *total*, *all_species*, *all_total*, *species_total*, *species_dbh_total*,
#'  *grand_total*, and *grand_dbh_total* data frames; a bare *detail* data
#'  frame is also accepted (in which case no cross-species "Summe" block or
#'  "Summe" rows are added, and the total column uses \code{rowSums}).
#'
#'
#' @return A data frame containing aggregated information by species group,
#'   separated by age or dq class (rows) and classified by single tree
#'   diameter class (columns). When \code{all_species} is supplied, a trailing
#'   cross-species "Summe" group (with an \code{NA} species group) is appended.
#'   When \code{species_total} is supplied, each species' block gets its own
#'   trailing "Summe" row (grand total across all classes and dbh classes for
#'   that species); when \code{species_dbh_total} is also supplied, that same
#'   row's dbh_class columns are filled with the species' per-dbh_class sums/
#'   weighted means across all classes (mirroring BaySF's own report), rather
#'   than being left blank. When \code{grand_total}/\code{grand_dbh_total} are
#'   supplied, the cross-species "Summe" block gets the equivalent trailing
#'   "Summe" row - the true grand total across every species, class, and dbh
#'   class combined.
#'
#' @section Attached attributes:
#'   The returned object carries a \code{class_type} attribute (\code{"age"} or
#'   \code{"dq"}) recording its class axis, and propagates the
#'   \code{tree_selection} attribute set by the \code{*_table_*()} functions.
#'   \code{\link{output_base_table_pdf}} /
#'   \code{\link{output_structure_table_pdf}} read both (via the internal helper
#'   \code{.table_pdf_meta}) instead of taking a \code{type} or a cohort/layer
#'   argument.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#' # The prepared tree list ships with the package; see
#' # ?data_ex3_trees_essentials for the chain that builds it. Three inventory
#' # points are enough to show the shape of the result, and they keep the
#' # example quick; the function takes a whole tree list alike.
#' trees_complete <- data_ex3_sample_trees_essentials |>
#'   dplyr::filter(plot_id %in% unique(plot_id)[1:3])
#'
#' structure_table <- trees_complete |>
#'    structure_table_age_class_main_stand()
#'
#' structure_table |> output_structure_table()
#'
output_structure_table <- function(structure_table) {

  # Accept either the list produced by structure_table_*() or a bare detail
  # data frame (older callers). When all_species is present it is appended as
  # a trailing cross-species "Summe" group.
  if (is.data.frame(structure_table)) {
    detail            <- structure_table
    all_species       <- NULL
    total             <- NULL
    all_total         <- NULL
    species_total     <- NULL
    species_dbh_total <- NULL
    grand_total       <- NULL
    grand_dbh_total   <- NULL
  } else {
    detail            <- structure_table$detail
    all_species       <- structure_table$all_species
    total             <- structure_table$total
    all_total         <- structure_table$all_total
    species_total     <- structure_table$species_total
    species_dbh_total <- structure_table$species_dbh_total
    grand_total       <- structure_table$grand_total
    grand_dbh_total   <- structure_table$grand_dbh_total
  }

  if (!is.null(all_species)) {
    all_species <- all_species |>
      dplyr::mutate(species_group = detail$species_group[NA_integer_])
    combined <- dplyr::bind_rows(detail, all_species)
  } else {
    combined <- detail
  }

  # main_class name needed for the weighted-mean join below
  main_class <- if ("d_q_class" %in% names(combined)) "d_q_class" else "age_class"

  # first pivot all variables to rows
  restruct_first <- combined |>
    tidyr::pivot_longer(
      cols = 4:(ncol(combined)),
      names_to = "variable",
      values_to = "values"
    )

  #  second pivot the single dbh classes to columns. names_sort = TRUE forces
  #  the new columns into the dbh_class factor-level order (ascending Bhd
  #  classes); pivot_wider's default (FALSE) would order them by first
  #  appearance in the data, which scrambles the Bhd-Klassen columns in the
  #  rendered table. The static base table avoids this only because it
  #  completes + arranges its class axis before pivoting.
  restruct_l28 <- restruct_first |>
    tidyr::pivot_wider(
      names_from = "dbh_class",
      values_from = c("values"),
      names_sort = TRUE
    )

  # Complete the dbh-class axis: every level of the dbh_class factor gets a
  # column, including one no tree falls into. Without this the column sequence
  # jumps (ex3 skips (15,20] and (55,60] altogether), which reads as if those
  # classes did not exist rather than as if they were empty - and it makes the
  # column set depend on the inventory. names_sort = TRUE above already puts the
  # observed columns in factor-level order; the added ones are slotted into the
  # same order here, so the axis is both complete and ascending.
  dbh_levels <- levels(combined$dbh_class)
  if (!is.null(dbh_levels)) {
    for (lv in setdiff(dbh_levels, names(restruct_l28)))
      restruct_l28[[lv]] <- NA_real_
    restruct_l28 <- restruct_l28 |>
      dplyr::relocate(dplyr::all_of(dbh_levels), .after = "variable")
  }

  # The dbh-class columns are everything besides the three key columns. Chosen
  # by exclusion rather than by the "(a,b]" spelling of the class labels - the
  # same rule the base table follows, and for the same reason: a class level
  # that is NA pivots to a column literally named "NA", which a spelling test
  # would silently leave out.
  dbh_cols <- setdiff(names(restruct_l28),
                      c("species_group", main_class, "variable"))

  # Provisional total: the plain sum across the dbh classes. It is overwritten
  # below for every variable the properly aggregated total / all_total elements
  # carry, and survives only for those they do not (per_species,
  # tot_area_class) - which are additive across dbh classes, so summing them is
  # correct.
  restruct_l28 <- restruct_l28 |>
    dplyr::mutate(total = dplyr::across(dplyr::all_of(dbh_cols)) |>
      rowSums(na.rm = TRUE) |>
      round(digits = 2))

  # The "total" column is taken from total / all_total instead of from that row
  # sum wherever those elements provide it. Summing across dbh classes is valid
  # only for the additive quantities (V, G, N, area): a mean tree (d_q_cm,
  # h_q_m) cannot be summed at all; n_plot would count an inventory point once
  # per dbh class it happens to occupy; each per-hectare figure has its own
  # reference area, so their sum is meaningless; and a confidence interval is
  # not additive either. total / all_total are the very same aggregation minus
  # the dbh_class grouping - that is, the base table's class-wise values - so
  # taking the column from there also makes both table families agree by
  # construction rather than by coincidence.
  if (!is.null(total)) {
    proper_vars <- setdiff(names(total), c("species_group", main_class))

    correct <- total |>
      tidyr::pivot_longer(dplyr::all_of(proper_vars),
                          names_to = "variable", values_to = "correct")

    sp_rows <- restruct_l28 |>
      dplyr::filter(!is.na(.data$species_group)) |>
      dplyr::left_join(correct, by = c("species_group", main_class, "variable")) |>
      dplyr::mutate(
        total = dplyr::if_else(.data$variable %in% proper_vars,
                               .data$correct, .data$total)
      ) |>
      dplyr::select(-"correct")

    summe_rows <- restruct_l28 |>
      dplyr::filter(is.na(.data$species_group))

    if (!is.null(all_total) && nrow(summe_rows) > 0) {
      proper_vars_all <- setdiff(names(all_total), main_class)

      correct_summe <- all_total |>
        tidyr::pivot_longer(dplyr::all_of(proper_vars_all),
                            names_to = "variable", values_to = "correct")

      summe_rows <- summe_rows |>
        dplyr::left_join(correct_summe, by = c(main_class, "variable")) |>
        dplyr::mutate(
          total = dplyr::if_else(.data$variable %in% proper_vars_all,
                                 .data$correct, .data$total)
        ) |>
        dplyr::select(-"correct")
    }

    restruct_l28 <- dplyr::bind_rows(sp_rows, summe_rows)
  }

  # The whole table's ideal area, needed for the area shares of the pooling
  # rows below. Only the main-stand variants compute areas at all.
  grand_area_ha <- if (!is.null(grand_total) && "area_ha" %in% names(grand_total))
                     grand_total$area_ha[[1]] else NULL

  # The area share of a pooling row. Everywhere else per_species is a cell's
  # share of the ideal area of its own class; a row that pools every class has
  # the whole table as its enclosing scope, so it is the share of the whole
  # table's ideal area. Derived from the area row that is already there rather
  # than aggregated separately, which keeps the two in step by construction:
  # the share of a species is its area over the grand area, and the dbh-class
  # cells of that row are the same division applied cell by cell, so they still
  # add up to the row's own total.
  add_area_share <- function(rows) {
    if (is.null(grand_area_ha) || !("area_ha" %in% rows$variable)) return(rows)
    share <- rows |>
      dplyr::filter(.data$variable == "area_ha") |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~ .x / grand_area_ha * 100),
        variable = "per_species"
      )
    dplyr::bind_rows(rows, share)
  }

  # Append a trailing "Summe" row within each species' block: the species'
  # grand total across ALL classes and ALL dbh classes (species_total, one
  # level up from `total`) lands in the "total" column. If species_dbh_total
  # is also supplied, its per-(species, dbh_class) sums/weighted means -
  # collapsed across all age/dq classes - fill in the "(...)" dbh-class
  # columns of that same row too, mirroring BaySF's own report (whose Summe
  # line sums every Bhd-Klasse, not just a single grand total). Without
  # species_dbh_total those columns are simply NA (bind_rows fills them
  # automatically).
  if (!is.null(species_total)) {
    lvl     <- levels(restruct_l28[[main_class]])
    new_lvl <- if ("Summe" %in% lvl) lvl else c(lvl, "Summe")

    numeric_vars <- setdiff(names(species_total), "species_group")
    species_summe_rows <- species_total |>
      tidyr::pivot_longer(
        cols      = dplyr::all_of(numeric_vars),
        names_to  = "variable",
        values_to = "total"
      ) |>
      dplyr::mutate(
        !!main_class := factor("Summe", levels = new_lvl, ordered = TRUE)
      )

    if (!is.null(species_dbh_total)) {
      dbh_vars <- setdiff(names(species_dbh_total), c("species_group", "dbh_class"))
      dbh_wide <- species_dbh_total |>
        tidyr::pivot_longer(
          cols      = dplyr::all_of(dbh_vars),
          names_to  = "variable",
          values_to = "val"
        ) |>
        tidyr::pivot_wider(names_from = "dbh_class", values_from = "val")

      species_summe_rows <- species_summe_rows |>
        dplyr::left_join(dbh_wide, by = c("species_group", "variable"))
    }

    # Added after the dbh columns are in place, so the share row gets them too.
    species_summe_rows <- add_area_share(species_summe_rows)

    restruct_l28 <- restruct_l28 |>
      dplyr::mutate(
        !!main_class := factor(as.character(.data[[main_class]]),
                               levels = new_lvl, ordered = TRUE)
      ) |>
      dplyr::bind_rows(species_summe_rows)
  }

  # Append the equivalent trailing "Summe" row to the cross-species "Summe"
  # block itself (species_group is NA): the grand total across ALL species,
  # classes, and dbh classes (grand_total), with grand_dbh_total filling the
  # per-dbh_class columns the same way species_dbh_total does for individual
  # species. all_total is NOT this grand total - it stays broken down by
  # class (used above only to correct d_q_cm/h_q_m per class) - so without
  # grand_total/grand_dbh_total this row was simply missing entirely.
  if (!is.null(grand_total)) {
    lvl     <- levels(restruct_l28[[main_class]])
    new_lvl <- if ("Summe" %in% lvl) lvl else c(lvl, "Summe")

    grand_summe_row <- grand_total |>
      tidyr::pivot_longer(
        cols      = dplyr::everything(),
        names_to  = "variable",
        values_to = "total"
      ) |>
      dplyr::mutate(
        species_group = detail$species_group[NA_integer_],
        !!main_class := factor("Summe", levels = new_lvl, ordered = TRUE)
      )

    if (!is.null(grand_dbh_total)) {
      dbh_vars <- setdiff(names(grand_dbh_total), "dbh_class")
      dbh_wide <- grand_dbh_total |>
        tidyr::pivot_longer(
          cols      = dplyr::all_of(dbh_vars),
          names_to  = "variable",
          values_to = "val"
        ) |>
        tidyr::pivot_wider(names_from = "dbh_class", values_from = "val")

      grand_summe_row <- grand_summe_row |>
        dplyr::left_join(dbh_wide, by = "variable")
    }

    grand_summe_row <- add_area_share(grand_summe_row)

    restruct_l28 <- restruct_l28 |>
      dplyr::mutate(
        !!main_class := factor(as.character(.data[[main_class]]),
                               levels = new_lvl, ordered = TRUE)
      ) |>
      dplyr::bind_rows(grand_summe_row)
  }

  # pct_anteil_v ("%Anteil"): the standing-volume (Vorrat) distribution across
  # the dbh classes - each dbh_class cell of the "v_hub_m3" row as a % of that
  # same row's own total. This is exactly what BaySFs Strukturtabelle shows
  # (verified: Fichte Akl I 13.328 / 21.801 = 61.1 %); the row is placed under
  # V and is Vorrat-based, NOT basal-area-based. Row-normalized, so every
  # dbh_class row sums to 100 % - individual age/dq class rows and the per-
  # species / cross-species "Summe" rows alike. BaySF leaves the Total column
  # blank on the individual class rows and prints 100.0 only on the "Summe"
  # rows, so we do the same.
  # By exclusion rather than by the "(a,b]" spelling, for the same reason as in
  # output_base_table(): a class column whose level is NA is named "NA" and
  # would silently drop out of the normalisation.
  dbh_cols <- setdiff(names(restruct_l28),
                      c("species_group", "variable", "total", main_class))
  main_class_sym <- main_class
  pct_rows <- restruct_l28 |>
    dplyr::filter(.data$variable == "v_hub_m3") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(dbh_cols),
        ~ dplyr::if_else(!is.na(.data$total) & .data$total > 0,
                          100 * .x / .data$total, NA_real_)
      ),
      total    = dplyr::if_else(as.character(.data[[main_class_sym]]) == "Summe" &
                                  !is.na(.data$total) & .data$total > 0,
                                100, NA_real_),
      variable = "pct_anteil_v"
    )
  restruct_l28 <- dplyr::bind_rows(restruct_l28, pct_rows)

  out <- restruct_l28 |>
    dplyr::arrange(is.na(.data$species_group), .data$species_group, .data[[main_class]])

  # Carry the tree-cohort tag (set by the structure_table_*() functions) through
  # to the PDF renderer for a self-describing, collision-free file name. When the
  # caller passed a bare detail data frame (older path) the attribute is NULL.
  attr(out, "tree_selection") <- attr(structure_table, "tree_selection")
  # Record the class axis (age / dq) so the PDF renderer no longer needs a
  # user-supplied `type` argument - the object describes itself.
  attr(out, "class_type") <- if (identical(main_class, "age_class")) "age" else "dq"
  out
}


#' Format Base Tables for Output (Basistabellen)
#'
#' @param base_table the output from either
#'  \code{\link{base_table_d_q_class}}, \code{\link{base_table_age_class}},
#'  \code{\link{base_table_d_q_class_main_stand}}, or
#'  \code{\link{base_table_age_class_main_stand}}
#'
#'
#' @return A data frame containing aggregated information by species group,
#'   classified by age or dq class (rows)
#'
#' @section Attached attributes:
#'   The returned object carries a \code{class_type} attribute (\code{"age"} or
#'   \code{"dq"}) recording its class axis, and propagates the
#'   \code{tree_selection} attribute set by the \code{*_table_*()} functions.
#'   \code{\link{output_base_table_pdf}} /
#'   \code{\link{output_structure_table_pdf}} read both (via the internal helper
#'   \code{.table_pdf_meta}) instead of taking a \code{type} or a cohort/layer
#'   argument.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#' # The prepared tree list ships with the package; see
#' # ?data_ex3_trees_essentials for the chain that builds it.
#' trees_complete <- data_ex3_sample_trees_essentials
#'
#' base_table  <- trees_complete |>
#'    base_table_age_class_main_stand()
#'
#' base_table |> output_base_table()
#'
output_base_table <- function(base_table) {

  stopifnot(("d_q_class" %in% names(base_table$detail)) |
            ("age_class" %in% names(base_table$detail)))

  if ("d_q_class" %in% names(base_table$detail)) {
    lvl <- levels(base_table$detail$d_q_class)
    base_table$detail <- base_table$detail |>
      dplyr::ungroup() |>
      tidyr::complete(.data$species_group, d_q_class = lvl) |>
      dplyr::mutate(d_q_class = ordered(.data$d_q_class, levels = lvl)) |>
      dplyr::arrange(.data$species_group, .data$d_q_class)
  }
  if ("age_class" %in% names(base_table$detail)) {
    lvl <- levels(base_table$detail$age_class)
    base_table$detail <- base_table$detail |>
      dplyr::ungroup() |>
      tidyr::complete(.data$species_group, age_class = lvl) |>
      dplyr::mutate(age_class = ordered(.data$age_class, levels = lvl)) |>
      dplyr::arrange(.data$species_group, .data$age_class)
  }

  # first pivot all variables to rows
  restruct_first <- base_table$detail |>
    tidyr::pivot_longer(
      cols      = 3:(ncol(base_table$detail)),
      names_to  = "variable",
      values_to = "values"
    )

  # Column name for class variable
  if ("d_q_class" %in%  names(base_table$detail)) {
    class_name <- "d_q_class"
  }
  if ("age_class" %in%  names(base_table$detail)) {
    class_name <- "age_class"
  }

  #  second pivot the single dbh classes to columns
  restruct_l26 <- restruct_first |>
    tidyr::pivot_wider(
      names_from  = dplyr::all_of(class_name),
      values_from = c("values")
    )

  # pivot the total table
  restruct_l26_total <- base_table$total |>
    tidyr::pivot_longer(
      cols      = 2:(ncol(base_table$total)),
      names_to  = "variable",
      values_to = "total"
    )

  # Join both: this is the per species-group part of the table
  species_part <- restruct_l26 |>
    dplyr::left_join(restruct_l26_total)

  # If the base table carries the cross-species aggregation (all_species /
  # all_total, produced by the base_table_*() functions), build the "Summe"
  # block and append it at the bottom. The block is tagged with a typed NA
  # species_group so it binds cleanly with the species rows and is rendered as
  # the last group by the template. Older base tables without these elements
  # simply fall through to the species part (backward compatible).
  if (!is.null(base_table$all_species)) {

    # class columns of the Summe block (one value per age / dq class)
    summe_l26 <- base_table$all_species |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols      = -dplyr::all_of(class_name),
        names_to  = "variable",
        values_to = "values"
      ) |>
      tidyr::pivot_wider(
        names_from  = dplyr::all_of(class_name),
        values_from = c("values")
      )

    # the rightmost total cell of the Summe block
    summe_total <- base_table$all_total |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols      = dplyr::everything(),
        names_to  = "variable",
        values_to = "total"
      )

    summe_part <- summe_l26 |>
      dplyr::left_join(summe_total, by = "variable") |>
      dplyr::mutate(
        species_group = base_table$detail$species_group[NA_integer_],
        .before = 1
      )

    combined_base <- dplyr::bind_rows(species_part, summe_part)

  } else {
    combined_base <- species_part
  }

  # %Anteil ("Mischungsanteil"): mirrors BaySFs Basistabelle, which shows TWO
  # DIFFERENT share rows that meet at 100 % in the Summe/Summe corner
  # (verified against the reference report, Nordhalben L.26):
  #  - each SPECIES row -> "%Anteil der Baumartengruppe" (pct_anteil_bag):
  #    the species' basal area (G) as a share of the cross-species G total OF
  #    THAT CLASS COLUMN. Column-normalized, so down each column (incl. the
  #    Summe column) the species sum to 100 %. Its Summe/total cell is the
  #    species' G as a share of the grand-total G (e.g. Fichte 45.7 %).
  #  - the cross-species Summe row -> "%Anteil der Klasse" (pct_anteil_class):
  #    that class' G as a share of the grand-total G (row-normalized across
  #    classes). Its Summe cell is 100 %.
  # The per-class cross-species G totals and the grand total both live in the
  # Summe block's own "g_m2" row (species_group == NA), so no extra
  # aggregation is needed.
  # Identify the class columns by exclusion, NOT by their "(a,b]" spelling.
  # Trees whose age is unknown land in an age_class of NA, which pivots to a
  # column literally named "NA". A "^\\(" pattern misses that column, so its
  # cells were never normalised and the raw basal area was relabelled as a
  # percentage (a G of 90 m2 printed as "90.1 %").
  class_cols <- setdiff(names(combined_base),
                        c("species_group", "variable", "total"))
  g_rows     <- combined_base |> dplyr::filter(.data$variable == "g_m2")
  summe_g    <- g_rows |> dplyr::filter(is.na(.data$species_group))

  if (nrow(summe_g) == 1 && !is.na(summe_g$total) && summe_g$total > 0) {
    g_grand     <- summe_g$total
    g_per_class <- summe_g[, class_cols]  # cross-species G per class (1 row)

    # species rows: column-normalized share of the Baumartengruppe. denom is a
    # length-1 scalar (the Summe row's cell for that column) that broadcasts
    # over the species; is.finite() then maps 0- and NA-denominators to NA.
    sp_pct <- g_rows |>
      dplyr::filter(!is.na(.data$species_group)) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(class_cols),
          ~ {
            val <- 100 * .x / g_per_class[[dplyr::cur_column()]]
            dplyr::if_else(is.finite(val), val, NA_real_)
          }
        ),
        total    = 100 * .data$total / g_grand,   # g_grand guaranteed > 0 above
        variable = "pct_anteil_bag"
      )

    # Summe row: row-normalized share of the (age/dq) class of the grand total
    summe_pct <- summe_g |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(class_cols), ~ 100 * .x / g_grand),
        total    = 100,
        variable = "pct_anteil_class"
      )

    # Re-sort (stable, NA/Summe last) so each species' block - now including
    # its pct row - stays contiguous for pack_rows() in the Rmd template.
    combined_base <- dplyr::bind_rows(combined_base, sp_pct, summe_pct) |>
      dplyr::arrange(is.na(.data$species_group), .data$species_group)
  }

  # Carry the tree-cohort tag (set by the base_table_*() functions) through to
  # the PDF renderer so it can build a self-describing, collision-free file name.
  attr(combined_base, "tree_selection") <- attr(base_table, "tree_selection")
  # Record the class axis (age / dq) so the PDF renderer no longer needs a
  # user-supplied `type` argument - the object describes itself.
  attr(combined_base, "class_type") <- if (identical(class_name, "age_class")) "age" else "dq"
  combined_base
}



#' Format Increment Base Tables for Output
#'
#' Increment counterpart of \code{\link{output_base_table}}. It restructures the
#' four-element increment base table into the same display layout (variables to
#' rows, age or diameter classes to columns, plus the appended \dQuote{Summe}
#' cross-species block) used for the static base tables.
#'
#' Because the increment base tables share the exact four-element layout
#' (\code{list(detail, total, all_species, all_total)}) and the same
#' \code{species_group} / class-column convention as the static ones, this
#' function reuses the generic restructuring of \code{\link{output_base_table}}.
#' It is exposed as its own increment-family entry point so the increment output
#' API stays discoverable and can diverge later (e.g. combining a value with its
#' confidence interval) without touching the static side.
#'
#' @param increment_base_table the output from either
#'   \code{\link{increment_base_table}} or
#'   \code{\link{increment_base_table_main_stand}}
#'
#' @return A data frame with one row per species group and variable, the class
#'   levels as columns plus a \code{total} column, and the cross-species
#'   \dQuote{Summe} block appended at the bottom (tagged with an \code{NA}
#'   \code{species_group}). The input's \code{collective} descriptor and its
#'   \code{meta} element are carried through as \code{"collective"} and
#'   \code{"meta"} attributes on the returned data frame, so the PDF step can
#'   label the collective (Hauptbestand / Alle Schichten / custom) and fill the
#'   header meta block without re-deriving them from the columns.
#'
#' @family increment
#'
#' @export
#'
#' @examples
#'   inv_b <- data_ex3_sample_fe_inventory
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   inc <- inv_increment_gnfi3(inv_b, inv_b_trees)
#'
#'   # Plain and main-stand increment base tables, restructured for output
#'   increment_base_table(inc, by_class = "age") |>
#'     output_increment_base_table()
#'
#'   increment_base_table_main_stand(inc, by_class = "age") |>
#'     output_increment_base_table()
#'
output_increment_base_table <- function(increment_base_table) {
  out <- output_base_table(increment_base_table)
  # Carry the collective descriptor and the header meta through as attributes
  # so the PDF step can label the collective (Hauptbestand / Alle Schichten /
  # custom) and fill the header block (previous inventory, period,
  # Berechnungs-/Ergänzungsmethode, area, points) without the caller supplying
  # them. Both mirror the same list-element -> attribute handoff.
  attr(out, "collective") <- increment_base_table$collective
  attr(out, "meta")       <- increment_base_table$meta
  # Yield-table class tables carry the list of used yield tables; pass it along
  # so the PDF step can print it (NULL for the repeated-survey / gnfi3 tables).
  attr(out, "ytables_used") <- increment_base_table$ytables_used
  out
}





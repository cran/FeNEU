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

#' Take the Per-Tree Frame Out of an Increment Bundle
#'
#' The class-table functions work on a per-tree increment frame. They accept
#' either that frame directly or one of the bundles that carry it -- the result
#' of \code{\link{inv_increment_repeated_survey}} or of
#' \code{\link{inv_increment_gnfi3}}, both of class \code{fe_increment}.
#'
#' Not every \code{fe_increment} object holds trees: the yield-table strand
#' estimates at stand level and has no per-tree increment at all. Such an object
#' is refused here with a pointer to the function that does fit, rather than
#' failing later on a missing column.
#'
#' @param x The object passed by the caller.
#'
#' @param fn_name Name of the calling function, for the error message.
#'
#' @return A per-tree increment data frame, with its \code{inc_meta} attribute
#'   intact.
#'
#' @keywords internal
#'
.increment_bundle_trees <- function(x, fn_name) {

  if (!inherits(x, "fe_increment")) return(x)

  if (is.null(x$trees)) {
    stop(fe_msg("increment_bundle_without_trees", fn_name, class(x)[1]),
         call. = FALSE)
  }

  x$trees
}


#' Aggregate Per-Tree gnfi3 Increments to the Inventory Level
#'
#' Shared aggregation core for the single-inventory gnfi3 increment. Takes a
#' per-tree frame that already carries \code{iv_hub_m3_ha_yr} (the tree's
#' increment scaled to a per-hectare contribution) and aggregates it to the
#' level of the whole inventory, overall and by species group.
#'
#' Plots without trees of a given species group enter with an increment of zero
#' in the numerator but with their full area in the denominator, so the species
#' groups sum to the overall value.
#'
#' Confidence intervals are deliberately \code{NA} throughout: a gnfi3 estimate
#' carries no empirical between-plot variation of a \emph{measured} increment.
#' The columns exist so that the result is structurally identical to the
#' corresponding block of the repeated-survey overview.
#'
#' @param trees_with_iv Per-tree frame with at least \code{plot_id},
#'   \code{species_group}, \code{v_hub_m3}, \code{n_rep_ha} and
#'   \code{iv_hub_m3_ha_yr}.
#'
#' @param area_info One row per inventory plot, with \code{plot_id} and
#'   \code{area_rep_ha}. This is the authoritative plot list: plots without any
#'   tree must be present here, otherwise their area is missing from the
#'   denominator.
#'
#' @param fill_label Value written into the \code{fill_option} column, which
#'   identifies the estimate downstream.
#'
#' @return A list with the elements \code{overall} and
#'   \code{by_species_group}, structured exactly like one collective block of
#'   \code{\link{inv_inc_big_overview}}.
#'
#' @keywords internal
#'
.gnfi3_overview_core <- function(trees_with_iv, area_info, fill_label) {

  area_total <- sum(area_info$area_rep_ha)
  n_plots    <- nrow(area_info)

  # The per-tree frame carries an area_rep_ha column of its own, so joining
  # area_info under that name would silently produce .x/.y suffixes. Rename the
  # plot-level area to a name that cannot collide.
  area_info <- area_info |>
    dplyr::select("plot_id", area_rep_ha_plot = "area_rep_ha")

  # --- Overall ---------------------------------------------------------
  # Plot-level increment per plot (0 for plots with no living trees), then
  # area-weighted via the shared ratio estimator.
  plot_iv <- trees_with_iv |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::summarise(
      iv_m3_ha_yr_plot = sum(.data$iv_hub_m3_ha_yr),
      n_trees          = dplyr::n(),
      .groups          = "drop"
    ) |>
    dplyr::right_join(area_info, by = "plot_id") |>
    dplyr::mutate(
      iv_m3_ha_yr_plot = dplyr::coalesce(.data$iv_m3_ha_yr_plot, 0),
      n_trees          = dplyr::coalesce(.data$n_trees, 0L)
    )

  # se_area_weighted_grouped() is reused only for its total/mean ratio-estimator
  # output; the CI columns are set to NA below (see the function head).
  se_overall <- plot_iv |>
    se_area_weighted_grouped("iv_m3_ha_yr_plot", "area_rep_ha_plot")

  agg_by_total <- tibble::tibble(
    fill_option         = fill_label,
    n                   = nrow(trees_with_iv),
    iv_m3_yr_total      = se_overall$total,
    ci95_iv_m3_yr_total = NA_real_,
    area_total_ha       = area_total,
    iv_m3_ha_yr         = se_overall$mean_ha,
    ci95_iv_m3_ha_yr    = NA_real_
  )

  agg_by_groups <- tibble::tibble(
    fill_option    = fill_label,
    id_able_tree   = NA,
    tree_status    = "living",
    n              = nrow(trees_with_iv),
    iv_m3_yr_total = se_overall$total,
    area_total_ha  = area_total,
    iv_m3_ha_yr    = se_overall$mean_ha
  )

  v_total <- trees_with_iv |>
    dplyr::left_join(area_info, by = "plot_id") |>
    dplyr::summarise(
      v_hub_m3 = sum(.data$v_hub_m3 * .data$n_rep_ha * .data$area_rep_ha_plot)
    ) |>
    dplyr::mutate(
      area_total_ha = area_total,
      v_hub_m3_ha   = .data$v_hub_m3 / .data$area_total_ha
    )

  n_plots_overall <- tibble::tibble(
    n_plots         = n_plots,
    area_total_ha   = area_total,
    avg_rep_ha_plot = area_total / n_plots
  )

  # --- By species group -------------------------------------------------
  plot_iv_sg <- trees_with_iv |>
    dplyr::group_by(.data$plot_id, .data$species_group) |>
    dplyr::summarise(
      iv_m3_ha_yr_plot = sum(.data$iv_hub_m3_ha_yr),
      .groups          = "drop"
    ) |>
    tidyr::complete(
      species_group,
      plot_id = area_info$plot_id,
      fill    = list(iv_m3_ha_yr_plot = 0)
    ) |>
    dplyr::left_join(area_info, by = "plot_id")

  se_sg <- plot_iv_sg |>
    dplyr::group_by(species_group) |>
    se_area_weighted_grouped("iv_m3_ha_yr_plot", "area_rep_ha_plot")

  n_per_sg <- trees_with_iv |>
    dplyr::count(.data$species_group, name = "n")

  agg_by_total_sg <- se_sg |>
    dplyr::left_join(n_per_sg, by = "species_group") |>
    dplyr::mutate(
      fill_option   = fill_label,
      n             = dplyr::coalesce(.data$n, 0L),
      area_total_ha = area_total
    ) |>
    dplyr::transmute(
      fill_option         = .data$fill_option,
      species_group       = .data$species_group,
      n                   = .data$n,
      iv_m3_yr_total      = .data$total,
      ci95_iv_m3_yr_total = NA_real_,
      area_total_ha       = .data$area_total_ha,
      iv_m3_ha_yr         = .data$mean_ha,
      ci95_iv_m3_ha_yr    = NA_real_
    )

  agg_by_groups_sg <- agg_by_total_sg |>
    dplyr::transmute(
      fill_option    = .data$fill_option,
      species_group  = .data$species_group,
      id_able_tree   = NA,
      tree_status    = "living",
      n              = .data$n,
      iv_m3_yr_total = .data$iv_m3_yr_total,
      area_total_ha  = .data$area_total_ha,
      iv_m3_ha_yr    = .data$iv_m3_ha_yr
    )

  v_total_sg <- trees_with_iv |>
    dplyr::left_join(area_info, by = "plot_id") |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(
      v_hub_m3 = sum(.data$v_hub_m3 * .data$n_rep_ha * .data$area_rep_ha_plot),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      area_total_ha = area_total,
      v_hub_m3_ha   = .data$v_hub_m3 / .data$area_total_ha
    )

  list(
    overall = list(
      iv_by_groups = agg_by_groups,
      iv_total     = agg_by_total,
      v_total      = v_total,
      n_plots      = n_plots_overall
    ),
    by_species_group = list(
      iv_by_groups = agg_by_groups_sg,
      iv_total     = agg_by_total_sg,
      v_total      = v_total_sg
    )
  )
}


#' Estimate the Volume Increment of a Single Inventory With the BWI 3 Growth
#' Functions
#'
#' Covers the whole chain from a prepared tree list to a ready-to-aggregate
#' increment result, using the single-tree growth functions of the third German
#' National Forest Inventory \insertCite{bwi3_methods_2017}{FeNEU}. It is the
#' single-inventory counterpart of
#' \code{\link{inv_increment_repeated_survey}}: both return the same kind of
#' object, so everything downstream -- the increment base tables, the overview,
#' and their PDF reports -- is served in exactly the same way.
#'
#' The increment is estimated \emph{per tree} and for the whole population, not
#' only for the collective that happens to be reported afterwards. Any
#' collective can therefore be aggregated from the result: the main stand, all
#' layers, or a selection of your own via the \code{tree_filter} argument of the
#' base-table functions.
#'
#' Because the estimate ages each tree by \code{dt} years, the age of every tree
#' is required. If ages are missing, the function stops rather than silently
#' evaluating the part of the population that happens to carry one.
#'
#' @param inv An \code{\link{fe_inventory}} object -- the inventory the tree
#'   list was pulled from. Supplies the authoritative plot list (including plots
#'   without trees) and the header figures of the reports.
#'
#' @param inv_trees Tree data frame as pulled from \code{inv} after height
#'   completion (\code{\link{height_complete_inventory}},
#'   \code{\link{fill_heights_back}}) and \code{\link{trees_add_essentials}}.
#'
#' @param dt Time span in years the estimate spans. Positive values look
#'   forward, negative values backward. Default is five years; the further such
#'   an estimate reaches, the less it can account for removals, mortality and
#'   ingrowth.
#'
#' @param d_q_interval Width of the quadratic mean diameter classes in cm,
#'   passed to \code{\link{back_table_dclass}}. Default is 10.
#'
#' @param keep_steps Logical. If \code{TRUE}, the intermediate results are
#'   returned in the \code{steps} element. They are useful for inspection but
#'   can become large for real inventories, so the default is \code{FALSE},
#'   which leaves \code{steps} as \code{NULL}.
#'
#' @return An object of class \code{fe_increment_gnfi3}, which inherits from
#'   \code{fe_increment}. A list with the elements
#'   \describe{
#'     \item{trees}{the per-tree increments, covering all layers,}
#'     \item{overview}{the aggregation to inventory level, overall and by
#'       species group,}
#'     \item{meta}{the figures the reports print in their header,}
#'     \item{steps}{the intermediate results, or \code{NULL} (see
#'       \code{keep_steps}).}
#'   }
#'
#' @seealso \code{\link{inv_increment_repeated_survey}} for the repeated-survey
#'   counterpart, \code{\link{increment_base_table}} and
#'   \code{\link{increment_base_table_main_stand}} for the class tables,
#'   \code{\link{output_increment_overview_gnfi3}} for the species-group
#'   overview.
#'
#' @family increment
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#'   inc <- inv_increment_gnfi3(
#'     inv       = data_ex3_sample_fe_inventory,
#'     inv_trees = data_ex3_sample_trees_essentials
#'   )
#'
#'   inc
#'
#'   # Increment of the whole inventory, m3 per hectare and year
#'   inc$overview$overall$iv_total$iv_m3_ha_yr
#'
#'   # Broken down by species group and age class
#'   increment_base_table(inc, by_class = "age")$all_total
#'
inv_increment_gnfi3 <- function(inv,
                                inv_trees,
                                dt           = 5,
                                d_q_interval = 10,
                                keep_steps   = FALSE) {

  stopifnot(is_fe_inventory(inv))
  stopifnot(is.numeric(dt), length(dt) == 1L, dt != 0)

  # Per-tree increments plus the class axes and the header meta. This is the
  # step that also feeds the second-only plots of a repeated survey, which is
  # why it lives on its own and is left untouched here.
  trees <- inv_inc_tree_extend_single(
    inv_trees, dt = dt, d_q_interval = d_q_interval, inv = inv,
    .caller = "inv_increment_gnfi3"
  )

  # The authoritative plot list comes from the inventory, not from the tree
  # list: a plot without trees carries no row there, but its area belongs in
  # the denominator all the same.
  area_info <- inv |>
    dplyr::select(-"plot") |>
    dplyr::select("plot_id", "area_rep_ha")

  # Aggregated straight from the per-tree frame rather than from a base table.
  # The inventory-level figure must not depend on which class axis a caller
  # happens to pick, nor inherit that table's age requirement. That the two
  # agree is a property worth testing, not a way to compute one from the other.
  overview <- .gnfi3_overview_core(trees, area_info, fill_label = "gnfi3")

  structure(
    list(
      trees    = trees,
      overview = overview,
      meta     = attr(trees, "inc_meta"),
      steps    = if (keep_steps) list(extended = trees) else NULL
    ),
    class = c("fe_increment_gnfi3", "fe_increment", "list")
  )
}


#' Test for the Class fe_increment_gnfi3
#'
#' @param x An object
#'
#' @return \code{TRUE} if \code{x} is an \code{fe_increment_gnfi3} object,
#'   \code{FALSE} otherwise
#'
#' @export
#'
#' @examples
#'   inc <- inv_increment_gnfi3(
#'     data_ex3_sample_fe_inventory, data_ex3_sample_trees_essentials
#'   )
#'   is_fe_increment_gnfi3(inc)
#'
is_fe_increment_gnfi3 <- function(x) {
  inherits(x, "fe_increment_gnfi3")
}


#' Print an fe_increment_gnfi3 Object
#'
#' @param x An \code{fe_increment_gnfi3} object
#'
#' @param ... Other parameters (not used)
#'
#' @return \code{x}, invisibly
#'
#' @export
#'
#' @examples
#'   inv_increment_gnfi3(
#'     data_ex3_sample_fe_inventory, data_ex3_sample_trees_essentials
#'   )
#'
print.fe_increment_gnfi3 <- function(x, ...) {
  m  <- x$meta
  iv <- x$overview$overall$iv_total

  cat("<fe_increment_gnfi3>\n")
  if (!is.null(m$year))
    cat("  Survey       :", m$year, "\n")
  if (!is.null(m$area_ha) && !is.null(m$n_plots))
    cat(sprintf("  Area / plots : %.1f ha / %d\n", m$area_ha, m$n_plots))
  cat("  Method       : BWI 3 growth functions, single inventory\n")
  cat("  Trees        :", nrow(x$trees), "\n")
  if (nrow(iv) > 0)
    cat(sprintf("  Increment    : %.2f m3/ha/yr\n", iv$iv_m3_ha_yr[1]))
  cat("  Steps kept   :", if (is.null(x$steps)) "no" else "yes", "\n")
  cat("  Use increment_base_table() on this object.\n")

  invisible(x)
}

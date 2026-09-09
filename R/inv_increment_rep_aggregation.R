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



#' Consolidating a Mix of Real and Estimated Tree-Level Increments for
#' Further Processing
#'
#' In an increment evaluation workflow, the intended use of this function is to
#' be applied to the output of
#' \code{\link{inv_inc_fill_gaps_gnfi3}}. In parallel to the
#' increment values that were directly calculated, the latter has estimated
#' the increment with the tree growth functions from the Third German National
#' Forest Inventory \insertCite{bwi3_methods_2017}{FeNEU} for different
#' cohorts of trees (in different ways, see
#' \code{\link{inv_inc_fill_gaps_gnfi3}}). Here, the user decides which
#' increments to use for which cohort in subsequent evaluations.
#'
#' The \code{fill_option} "standard" takes the inventory increment for all
#' trees that can be potentially identified across inventories and are present
#' at least in the second survey (i.e. re-measured or ingrown), as long as they
#' do not have a removal flag at the second survey. For such trees
#' that are, however, present in the first survey but are absent or have a removal
#' flag at the second survey, an NFI based forward increment estimate for half
#' the period between the surveys is used. Trees that cannot be identified
#' across inventories and are present in the second survey, obtain a backwards
#' increment estimate for the whole period. The option "min_estimates" uses
#' NFI estimates only for trees that cannot be identified across surveys.
#' Therefore, trees that only exist in the first survey obtain a zero increment.
#' The third option "all_estimates" uses NFI based estimates for all trees.
#'
#' @param tree_inc_rep_gapfill Output object of
#'   \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @param fill_option Three options, "standard" (default), "min_estimates",
#'   and "all_estimates". See Details.
#'
#' @returns A data frame with every line representing a tree, actually all
#'   output components from \code{\link{inv_inc_fill_gaps_gnfi3}} combined
#'   into one data frame. Importantly, there are three additional columns:
#'   \code{iv_hub_m3_per} and \code{iv_hub_m3_yr} are the periodic and annual
#'   increment values to continue calculating with (coming either from the
#'   inventory or from the estimate), and \code{iv_filled} is logical; if
#'   TRUE, the increment is estimated with the German NFI growth functions.
#'   If FALSE the increment is directly calculated from the survey data.
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#'
#'   # Identify two subsequent inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # Matching and gap filling for this pair are shipped ready-made, see
#'   # ?data_ex3_increment_interim for how they were built
#'   inc_repinv    <- data_ex3_increment_matched
#'   inc_repinv_fl <- data_ex3_increment_fills
#'
#'   # Finally, consolidate the tree increments, try all fill options
#'   inv_inc_tree_consolidate(inc_repinv_fl, fill_option = "standard")
#'
#'   inv_inc_tree_consolidate(inc_repinv_fl, fill_option = "min_estimates")
#'
#'   inv_inc_tree_consolidate(inc_repinv_fl, fill_option = "all_estimates")
#'
#'   # The option 'all_estimates' must throw an error if the gap filling was
#'   # not done for the trees which have dbh values in both surveys
#'   inc_repinv_fl <- inv_inc_fill_gaps_gnfi3(
#'     inc_repinv, inv_a_trees, inv_b_trees, include_reptrees = FALSE
#'   )
#'   try(
#'     inv_inc_tree_consolidate(inc_repinv_fl, fill_option = "all_estimates")
#'   )
#'
#' @family increment
inv_inc_tree_consolidate <- function(
    tree_inc_rep_gapfill,
    fill_option = c("standard", "min_estimates", "all_estimates")
) {

  fill_option <- match.arg(fill_option)

  # Combine the different input list elements (only the per-tree data frames;
  # metadata elements such as inv_period are kept aside)
  df_elements <- tree_inc_rep_gapfill[
    vapply(tree_inc_rep_gapfill, is.data.frame, logical(1))
  ]
  work_dat <- dplyr::bind_rows(df_elements)

  # Helper: identify implausible trees (may not have these columns if coming
  # from older data, so default to FALSE)
  has_impl_cols <- all(
    c("volume_change_implausible", "species_mismatch") %in% names(work_dat)
  )
  if (has_impl_cols) {
    is_implausible <- work_dat$volume_change_implausible |
      work_dat$species_mismatch
  } else {
    is_implausible <- rep(FALSE, nrow(work_dat))
  }

  # Depending on fill_option, construct the increments that count
  if (fill_option == "standard") {
    # Take the original increment from idable trees present in both or the
    # second inventory, unless they are implausible.
    # Replace the rest with gnfi3 estimates
    use_gnfi3 <-
      is.na(work_dat$tree_exists_1st) |                       # non-idable
      (is.na(work_dat$tree_exists_1st) | !work_dat$tree_exists_2nd) | # missing
      (work_dat$tree_exists_1st &
         (work_dat$tree_exists_2nd & work_dat$tree_removal_2nd)) | # removed/mort
      is_implausible                                           # implausible

    work_dat <- work_dat |>
      dplyr::mutate(
        iv_hub_m3_ha_per = ifelse(
          use_gnfi3,
          .data$iv_hub_m3_ha_per_gnfi3,
          .data$iv_hub_m3_ha_per_rep
        ),
        iv_hub_m3_ha_yr = ifelse(
          use_gnfi3,
          .data$iv_hub_m3_ha_yr_gnfi3,
          .data$iv_hub_m3_ha_yr_rep
        ),
        iv_filled = use_gnfi3
      )
  }

  if (fill_option == "min_estimates") {
    # Take the original increment from all trees where it is available, even
    # though it is rough for trees that were not present in both inventories.
    # Take gnfi3 estimates for trees where no match was possible due to the
    # survey system, and for implausible trees.
    use_gnfi3 <- is.na(work_dat$tree_exists_1st) | is_implausible

    work_dat <- work_dat |>
      dplyr::mutate(
        iv_hub_m3_ha_per = ifelse(
          use_gnfi3,
          .data$iv_hub_m3_ha_per_gnfi3,
          .data$iv_hub_m3_ha_per_rep
        ),
        iv_hub_m3_ha_yr = ifelse(
          use_gnfi3,
          .data$iv_hub_m3_ha_yr_gnfi3,
          .data$iv_hub_m3_ha_yr_rep
        ),
        iv_filled = use_gnfi3
      )
  }

  if (fill_option == "all_estimates") {
    # Only use gnfi3 increments, nothing else
    ## In case no estimated increments were provided for trees that had a
    ## dbh value in both inventories, throw an error and terminate.
    if (any(is.na(work_dat$iv_hub_m3_ha_per_gnfi3)) |
        any(is.na(work_dat$iv_hub_m3_ha_yr_gnfi3)))
      stop(fe_msg("fill_all_estimates_has_na"))

    work_dat <- work_dat |>
      dplyr::mutate(
        iv_hub_m3_ha_per = .data$iv_hub_m3_ha_per_gnfi3,
        iv_hub_m3_ha_yr  = .data$iv_hub_m3_ha_yr_gnfi3,
        iv_filled     = TRUE
      )
  }

  # Carry the calculation method (from the gap-fill input) and the chosen
  # fill option along as an attribute, so the downstream header meta of the
  # increment base tables can name the Berechnungs- and Ergänzungsmethode
  # without the caller re-supplying them. Attached last, after all dplyr
  # steps (which would drop it), and read again at the start of
  # inv_inc_tree_extend().
  attr(work_dat, "inc_meta") <- list(
    method      = tree_inc_rep_gapfill$method,
    fill_option = fill_option
  )
  work_dat
}




#' Extend Tree-Level Increments With Detailed Tree Information, Especially
#' Age and Mean Diameter Classes for Further Aggregation
#'
#' In an increment evaluation workflow, the intended use of this function is to
#' be applied to the output of \code{\link{inv_inc_tree_consolidate}}
#'
#' @section Coverage -- read this before aggregating:
#' The result covers \strong{only the plots measured in both inventories}. Plots
#' that exist just in the second survey are not part of the repeated-survey
#' matching and therefore absent here. Feeding this frame straight into
#' \code{\link{increment_base_table}} or
#' \code{\link{increment_base_table_main_stand}} \emph{understates} every
#' per-hectare figure, because those tables divide by the full operation area
#' (from the \code{"inc_meta"} attribute) while the second-only plots contribute
#' no increment -- their area sits in the denominator with nothing in the
#' numerator. Pass the result through
#' \code{\link{inv_inc_tree_extend_combined}} first, or, simpler, use
#' \code{\link{inv_increment_repeated_survey}}, which runs the whole chain
#' including that step.
#'
#' @param tree_inc_cons A data frame with consolidated tree level increments,
#'   typically the output of \code{\link{inv_inc_tree_consolidate}}
#'
#' @param inv_1st_trees Tree data frame as pulled and extended from the the
#'   \code{\link{fe_inventory}} object that represents the first of two
#'   subsequent surveys. Should be the same as previously used as input for
#'   \code{\link{inv_increment_repsurv_ccirc}}.
#'
#' @param inv_2nd_trees Tree data frame as pulled and extended from the the
#'   \code{\link{fe_inventory}} object that represents the second of two
#'   subsequent surveys. Should be the same as previously used as input for
#'   \code{\link{inv_increment_repsurv_ccirc}}.
#'
#' @param inv_2nd \code{\link{fe_inventory}} object that represents the second
#'   of the two inventories of interest.
#'
#' @param d_q_interval Numeric value, represents the width (cm) of the
#'   quadratic mean diameter (d_q) classes to be added to the output data frame.
#'   This argument is passed to the function \code{\link{back_table_dclass}}
#'   which is internally called.
#'
#' @returns A data frame where each row represents a tree, with extended
#'   information about increment and other tree level variables. The two
#'   columns \code{time_yr_prev} and \code{time_yr_2nd} are uniform across
#'   all rows and describe the survey years of the inventory pair (1st and
#'   2nd survey, respectively); their difference is the inter-survey
#'   period. \code{time_yr} is the time of the tree's actual observation
#'   in the join partner inventory and equals \code{time_yr_prev} for trees
#'   that exist only in the first survey. The result also carries an
#'   \code{"inc_meta"} attribute (survey years, period, area, point count,
#'   Berechnungs-/Ergänzungsmethode) that the increment base tables promote
#'   into their header meta block.
#'
#' @export
#'
#' @examples
#'   # Identify two subsequent inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # Matching and gap filling for this pair are shipped ready-made, see
#'   # ?data_ex3_increment_interim for how they were built
#'   inc_repinv    <- data_ex3_increment_matched
#'   inc_repinv_fl <- data_ex3_increment_fills
#'
#'   # Consolidate tree increments
#'   inc_repinv_cons <- inv_inc_tree_consolidate(inc_repinv_fl)
#'
#'   # Extend with tree information required for further aggregation
#'   inv_inc_tree_extend(inc_repinv_cons, inv_a_trees, inv_b_trees, inv_b)
#'
#' @family increment
inv_inc_tree_extend <- function(
    tree_inc_cons, inv_1st_trees, inv_2nd_trees, inv_2nd, d_q_interval = 10
) {
  # Read the calculation-method / fill-option meta carried by
  # inv_inc_tree_consolidate() before any operation touches the object.
  cons_meta <- attr(tree_inc_cons, "inc_meta")

  # Count rows for checksumming later
  nrow_cons <- nrow(tree_inc_cons)

  # For the last time in an increment evaluation workflow, we have to make
  # a case distinction: 1) Trees that are present in the first inventory only,
  # and 2) trees that are present in the second inventory (no matter if they
  # are present in the 1st or not).
  tree_inc_1st_only <- tree_inc_cons |>
    dplyr::filter(.data$tree_exists_1st & !.data$tree_exists_2nd)

  tree_inc_2nd <- tree_inc_cons |>
    dplyr::filter(.data$tree_exists_2nd)

  # Both increment data frames have to be joined with the correct tree list
  ## tree_inc_1st_only with the first survey, because this is the only place
  ## where information about these trees is available
  tree_inc_1st_only <- tree_inc_1st_only |> dplyr::left_join(
    inv_1st_trees,
    by = c("plot_id_1st" = "plot_id", "tree_id_1st" = "tree_id")
  )
  ## tree_inc_2nd has to be joined with the second survey, because we have to
  ## relate to the younger survey as much as possible
  tree_inc_2nd <- tree_inc_2nd |> dplyr::left_join(
    inv_2nd_trees,
    by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
  )

  # Both data frames need to be brushed up (each one slightly different) in
  # order to become compatible for row binding.
  # Note: time_yr_1st and time_yr_2nd are kept as stable metadata columns
  # describing the inventory period (uniform across all rows; guaranteed by
  # fe_inventory). time_yr_1st is renamed to time_yr_prev for clarity of
  # the per-tree interpretation (it is the time of the tree's prior
  # observation); time_yr_2nd is kept under its original name. time_yr is
  # the time of the tree's observation in the join partner inventory (i.e.
  # the 1st survey for 1st-only trees, the 2nd survey otherwise).
  ## 1st only
  tree_inc_1st_only <- tree_inc_1st_only |>
    dplyr::mutate(
      tree_id = paste0(.data$tree_id_1st, "_prev")
    ) |>
    dplyr::rename(
      plot_id      = "plot_id_2nd",
      time_yr_prev = "time_yr_1st"
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        "tree_id_1st", "tree_id_2nd", "plot_id_1st", "iv_hub_m3_ha_per_rep",
        "iv_hub_m3_ha_yr_rep", "iv_hub_m3_ha_per_gnfi3",
        "iv_hub_m3_ha_per_gnfi3"
      ))
    ) |>
    dplyr::select(
      "plot_id", "tree_id", "time_yr_prev", "time_yr", "time_yr_2nd",
      "tree_exists_1st", "tree_exists_2nd", "iv_hub_m3_ha_per",
      "iv_hub_m3_ha_yr", tidyselect::everything()
    )

  ## 2nd
  tree_inc_2nd <- tree_inc_2nd |>
    dplyr::select(
      -dplyr::all_of(c(
        "tree_id_1st", "plot_id_1st", "iv_hub_m3_ha_per_rep",
        "iv_hub_m3_ha_yr_rep", "iv_hub_m3_ha_per_gnfi3",
        "iv_hub_m3_ha_per_gnfi3"
      ))
    ) |>
    dplyr::rename(
      tree_id      = "tree_id_2nd",
      plot_id      = "plot_id_2nd",
      time_yr_prev = "time_yr_1st"
    )  |>
    dplyr::select(
      "plot_id", "tree_id", "time_yr_prev", "time_yr", "time_yr_2nd",
      "tree_exists_1st", "tree_exists_2nd", "iv_hub_m3_ha_per",
      "iv_hub_m3_ha_yr", tidyselect::everything()
    )

  # Construct quadratic mean diameter (d_q) classes and add them to rlst.
  # We are doing this still separately for both data frames, because we do not
  # want d_q to be calculated across both groups.
  #
  # The helper's d_q_cm is renamed BEFORE the join (as in
  # inv_inc_tree_extend_single()). Relying on the join's .x/.y suffixes instead
  # would tie this function to whether the tree data happen to carry a d_q_cm
  # column of their own: they do when the heights were folded into the tibble
  # directly, but not when the canonical write-back chain
  # (fill_heights_back() -> pull_trees() -> trees_add_essentials()) was used,
  # and then no suffixes are generated and the rename fails with
  # "Column `d_q_cm.x` doesn't exist". An input d_q_cm now simply survives
  # under its own name; nothing downstream reads it, the class axis is built
  # from d_q_class.
  dq_classes <- back_table_dclass(tree_inc_1st_only, d_q_interval) |>
    dplyr::rename(d_q_cm_for_class = "d_q_cm")
  tree_inc_1st_only <- tree_inc_1st_only |>
    dplyr::inner_join(
      dq_classes, by = c("plot_id", "species_group", "layer_key")
    )

  dq_classes <- back_table_dclass(tree_inc_2nd, d_q_interval) |>
    dplyr::rename(d_q_cm_for_class = "d_q_cm")
  tree_inc_2nd <- tree_inc_2nd |>
    dplyr::inner_join(
      dq_classes, by = c("plot_id", "species_group", "layer_key")
    )

  # Connect both data frames
  rslt <- dplyr::bind_rows(tree_inc_1st_only, tree_inc_2nd)

  # area_rep_ha has to be set consequently to the values of the second
  # survey, because that's what counts in upscaling
  rslt <- rslt |>
    dplyr::select(-"area_rep_ha") |>
    dplyr::left_join(
      inv_2nd |>
        dplyr::select(-"plot") |>
        dplyr::select("plot_id", "area_rep_ha")
    )

  # Checksum check - issue warning if not ok
  if (nrow_cons != nrow(rslt))
    warning(
      paste0(
        "Checksum mismatch (", nrow_cons, " != ", nrow(rslt),
        ") in inv_inc_tree_extend()."
      )
    )

  # Assemble the header meta for the increment base tables and carry it along
  # as an attribute (read again at the start of increment_base_table() and
  # promoted to a list element there). The survey years come from the uniform
  # time_yr_* columns, area and point count from the second inventory, and the
  # calculation method / fill option from the consolidation step.
  year_curr <- max(rslt$time_yr_2nd,  na.rm = TRUE)
  year_prev <- max(rslt$time_yr_prev, na.rm = TRUE)
  attr(rslt, "inc_meta") <- list(
    year          = year_curr,
    inv_year_prev = year_prev,
    period_yr     = year_curr - year_prev,
    area_ha       = sum(inv_2nd$area_rep_ha),
    n_plots       = nrow(inv_2nd),
    method        = cons_meta$method,
    fill_option   = cons_meta$fill_option
  )

  return(rslt)
}



#' Extend Single-Inventory gnfi3 Tree Increments With Detailed Tree Information
#' for Further Aggregation
#'
#' Single-inventory counterpart to \code{\link{inv_inc_tree_extend}}. Where the
#' latter consolidates a repeated survey (with its match-based and gnfi3-filled
#' increments) into the per-tree contract consumed by
#' \code{\link{inv_inc_tree_2_plot}}, this function produces the very same
#' contract for an inventory that was \emph{not} repeated. Every tree increment
#' is estimated with the Third German National Forest Inventory growth functions
#' \insertCite{bwi3_methods_2017}{FeNEU} via \code{\link{tree_inc_gnfi_2012}},
#' so the resulting \code{iv_filled} column is uniformly \code{TRUE}.
#'
#' The point of this function is the shared seam: once the single-inventory
#' source carries the same columns as the repeated-survey source, the downstream
#' aggregation (\code{\link{inv_inc_tree_2_plot}} and everything built on top of
#' it) is written and maintained only once, regardless of where the increment
#' came from. The estimation method (gnfi3) is reflected in the per-tree
#' \code{_gnfi3} columns that ride along; the per-ha increment that the contract
#' uses (\code{iv_hub_m3_ha_yr}) is the per-tree value scaled by
#' \code{n_rep_ha}, clamped at zero (negative differences are improbable but
#' possible near age 0).
#'
#' The estimation direction lives in the sign of \code{dt} (see
#' \code{\link{tree_inc_gnfi_2012}}). The default \code{dt = 5} looks five years
#' \emph{forward}: for a single inventory the planning perspective is inherently
#' forward-looking, so the increment of the coming years is the quantity of
#' interest. The backward convention \code{dt = -5} used by
#' \code{\link{inv_inc_big_overview_2nd_only}} addresses a different situation --
#' there the gnfi3 estimate stands in for a repeated survey, whose view is
#' naturally backward (in the assumption that the recent past increment is a
#' good proxy for the coming years). Either way a short window keeps the bias
#' from the unknown future or past mortality and removal history small.
#'
#' @param inv_trees Tree data frame as pulled from an \code{\link{fe_inventory}}
#'   object after height completion (\code{\link{height_complete_inventory}})
#'   and \code{\link{trees_add_essentials}}. This is the same kind of object
#'   that \code{\link{inv_inc_tree_extend}} expects as its \code{inv_2nd_trees}
#'   argument. Trees flagged for removal (\code{removal == TRUE}) are dropped:
#'   a single-inventory estimate concerns the currently living stand, and this
#'   keeps the source consistent with the yield-table path.
#'
#' @param dt Time span for the gnfi3 estimate (years), passed to
#'   \code{\link{tree_inc_gnfi_2012}}. Positive values project forward in time,
#'   negative values backward. Default \code{5} (forward), matching the
#'   forward-looking planning perspective of a single-inventory increment
#'   estimate. Must be non-zero.
#'
#' @param d_q_interval Numeric, width (cm) of the quadratic mean diameter (d_q)
#'   classes added to the output. Passed to \code{\link{back_table_dclass}}.
#'
#' @param inv Optional \code{\link{fe_inventory}} object (the inventory the trees
#'   were pulled from). If supplied, a minimal header meta (\code{year},
#'   \code{area_ha}, \code{n_plots}) is attached as the \code{"inc_meta"}
#'   attribute so the increment base tables can express per-ha increments over
#'   the full operation area and fill the header. Default \code{NULL}: no meta,
#'   per-ha then over the stocked collective area.
#'
#' @returns A data frame where each row represents a tree, carrying the per-tree
#'   increment contract consumed by \code{\link{inv_inc_tree_2_plot}}:
#'   \code{plot_id}, \code{species_group}, \code{age_class}, \code{d_q_class},
#'   \code{layer_key}, \code{removal}, \code{standing_area_m2}, \code{n_rep_ha},
#'   \code{area_rep_ha}, the periodic and annual per-ha increments
#'   \code{iv_hub_m3_ha_per} / \code{iv_hub_m3_ha_yr}, and \code{iv_filled}
#'   (always \code{TRUE} here). The per-tree \code{_gnfi3} estimation columns
#'   from \code{\link{tree_inc_gnfi_2012}} are retained.
#'
#' @references \insertAllCited{}
#'
# Internal step of two chains: inv_increment_gnfi3() builds its whole result on
# it, and inv_inc_tree_extend_combined() uses it for the backward estimate of
# the second-only plots of a repeated survey. Users reach it through
# inv_increment_gnfi3(). No @family tag, so it stays out of the "See also" of
# the user-facing increment functions.
#' @keywords internal
#'
# No @examples here: the function is not exported any more, so R CMD check would
# run the example with only the exported objects on the search path and fail on
# the bare call. The code path is covered by the examples of
# inv_increment_gnfi3() and by test-inv_inc_tree_extend_single.R.
#'
#' @param .caller Name the age check reports as the failing function. Defaults
#'   to this function's own name; \code{\link{inv_increment_gnfi3}} passes its
#'   own, so that a user who called that one is not pointed at an internal
#'   step they cannot see.
#'
inv_inc_tree_extend_single <- function(inv_trees, dt = 5, d_q_interval = 10,
                                       inv = NULL,
                                       .caller = "inv_inc_tree_extend_single") {

  stopifnot(is.numeric(dt), length(dt) == 1L, dt != 0)
  abs_dt <- abs(dt)

  # A single-inventory forward (or backward) estimate concerns the currently
  # living stand, so trees flagged for removal (Ausscheider / harvest marking)
  # are dropped. This keeps the gnfi3 source consistent with the yield-table
  # path (which also filters !removal) and with the backward-estimated
  # second-only plots -- all single-inventory estimates share the same living
  # collective. The repeated-survey path (inv_inc_tree_extend / _combined)
  # deliberately keeps removed trees (Weg B) and is unaffected, because it does
  # not route through here for its matched trees.
  if ("removal" %in% names(inv_trees))
    inv_trees <- inv_trees |> dplyr::filter(!.data$removal)

  # The gnfi3 increment ages each tree by dt and takes the volume difference,
  # so an age is required. Checked here, after the removal filter, i.e. on
  # exactly the collective that is evaluated.
  .assert_age_complete(inv_trees, .caller)

  # Per-tree gnfi3 increment, scaled to a per-ha contribution via n_rep_ha.
  # iv_filled is uniformly TRUE: in a single inventory every increment is an
  # estimate, there is no measured re-survey increment to prefer.
  work_dat <- inv_trees |>
    tree_inc_gnfi_2012(dt = dt) |>
    dplyr::mutate(
      iv_hub_m3_ha_yr  = .data$n_rep_ha * pmax(0, .data$iv_hub_m3_yr_gnfi3),
      iv_hub_m3_ha_per = .data$iv_hub_m3_ha_yr * abs_dt,
      iv_filled        = TRUE
    )

  # Construct quadratic mean diameter (d_q) classes, exactly as in
  # inv_inc_tree_extend(). Renaming the helper's d_q_cm before the join keeps
  # the result independent of whether inv_trees already carries a d_q_cm
  # column (it does when the heights were folded into the tibble directly, but
  # not after the canonical write-back chain).
  dq_classes <- back_table_dclass(work_dat, d_q_interval) |>
    dplyr::rename(d_q_cm_for_class = "d_q_cm")

  work_dat <- work_dat |>
    dplyr::inner_join(
      dq_classes, by = c("plot_id", "species_group", "layer_key")
    )

  # Mark the source as gnfi3 so the downstream base-table title reads
  # "-- Schaetzung nach BWI 3" (the single-inventory counterpart of the
  # repeated survey's "-- Wiederholungsinventur" suffix). This holds whether
  # or not an inventory is supplied.
  #
  # If the inventory is supplied, additionally attach a minimal header meta
  # (current year, operation area and point count) -- the same mechanism the
  # repeated-survey extend uses. It lets the increment base tables express
  # per-ha increments over the full operation area and fill the header. A
  # single inventory has no previous survey / calculation method / fill option,
  # so those fields stay absent (no "Wiederholungsinventur" suffix).
  if (!is.null(inv)) {
    attr(work_dat, "inc_meta") <- list(
      source  = "gnfi3",
      year    = max(inv_trees$time_yr, na.rm = TRUE),
      area_ha = sum(inv$area_rep_ha),
      n_plots = nrow(inv)
    )
  } else {
    attr(work_dat, "inc_meta") <- list(source = "gnfi3")
  }

  # area_rep_ha is already the second (and only) survey's representation area,
  # so unlike the repeated-survey extend there is nothing to re-attach here.
  work_dat
}



#' Fold Second-Only Plots Into the Repeated-Survey Tree-Level Increments
#'
#' Extends the per-tree increment frame of a repeated survey -- the output of
#' \code{\link{inv_inc_tree_extend}}, which covers only the plots measured in
#' \emph{both} inventories -- with the plots that exist only in the second survey
#' (new establishments, sample extensions). These second-only plots receive a
#' short-window backward gnfi3 estimate via
#' \code{\link{inv_inc_tree_extend_single}} (default \code{dt = -5}, the same
#' convention as \code{\link{inv_inc_big_overview_2nd_only}}), and the two
#' per-tree frames are row-bound into one.
#'
#' The point of this function is the shared per-tree contract: both
#' \code{\link{inv_inc_tree_extend}} and \code{\link{inv_inc_tree_extend_single}}
#' emit the same columns consumed by \code{\link{inv_inc_tree_2_plot}}, so the
#' combined frame runs through \code{\link{increment_base_table}} and
#' \code{\link{increment_base_table_main_stand}} \emph{unchanged}. The resulting
#' base tables then cover the \emph{whole enterprise} (matched plus second-only
#' plots) instead of only the matched subset, so their per-hectare increment
#' relates to the full operation area -- consistent with the enterprise-level
#' \code{\link{inv_inc_big_overview}} (where
#' \code{combined = matches_only + second_only}).
#'
#' Only living trees (\code{!removal}) of the second-only plots enter the
#' estimate, mirroring \code{\link{inv_inc_big_overview_2nd_only}}: a plot without
#' a predecessor survey has no between-inventory removals to capture, so a removal
#' flag there marks a tree that is not part of the period's standing increment.
#' The matched frame is left exactly as it is (it keeps its removed trees, whose
#' increment is part of the period's production -- the \dQuote{Weg B} convention).
#'
#' The header meta carried by \code{tree_inc_extd} already describes the whole
#' enterprise (its area and point count come from the full second inventory
#' passed to \code{\link{inv_inc_tree_extend}}), so it is simply re-attached to
#' the combined frame (row-binding drops attributes).
#'
#' @param tree_inc_extd Per-tree increment frame of the matched plots, the
#'   output of \code{\link{inv_inc_tree_extend}}. Its \code{"inc_meta"} attribute
#'   is carried over to the result.
#'
#' @param inc_repinv_fl Output of \code{\link{inv_inc_fill_gaps_gnfi3}}. Only its
#'   \code{plot_matches$in_b_only} element is used, to identify the second-only
#'   plots.
#'
#' @param inv_2nd_trees Tree data frame of the second survey (as pulled,
#'   height-completed and passed through \code{\link{trees_add_essentials}}); the
#'   same object supplied to \code{\link{inv_inc_tree_extend}} as its
#'   \code{inv_2nd_trees} argument.
#'
#' @param dt Time span (years) for the second-only backward gnfi3 estimate,
#'   passed to \code{\link{inv_inc_tree_extend_single}}. Negative for a backward
#'   projection. Default \code{-5}, matching
#'   \code{\link{inv_inc_big_overview_2nd_only}}.
#'
#' @param d_q_interval Numeric, width (cm) of the quadratic mean diameter (d_q)
#'   classes, passed on to \code{\link{inv_inc_tree_extend_single}}.
#'
#' @returns A per-tree increment data frame with the same contract as
#'   \code{\link{inv_inc_tree_extend}}, now spanning matched and second-only
#'   plots, carrying the (whole-enterprise) \code{"inc_meta"} attribute of
#'   \code{tree_inc_extd}. If there are no second-only plots (or none with living
#'   trees), \code{tree_inc_extd} is returned unchanged.
#'
#' @family increment
#'
#' @export
#'
#' @examples
#'   # Identify two subsequent inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   suppressWarnings(suppressMessages({
#'     # Matching and gap filling are shipped ready-made for this pair
#'     inc_rep <- data_ex3_increment_matched
#'     inc_fl  <- data_ex3_increment_fills
#'     inc_extd <- inv_inc_tree_consolidate(inc_fl) |>
#'       inv_inc_tree_extend(inv_a_trees, inv_b_trees, inv_b)
#'   }))
#'
#'   # Fold in the second-only plots -> whole-enterprise per-tree frame
#'   inc_extd_all <- inv_inc_tree_extend_combined(
#'     inc_extd, inc_fl, inv_b_trees
#'   )
#'
#'   # Base tables now cover matched + second-only plots (run unchanged)
#'   increment_base_table(inc_extd_all, by_class = "age")$all_total
#'
inv_inc_tree_extend_combined <- function(
    tree_inc_extd, inc_repinv_fl, inv_2nd_trees,
    dt = -5, d_q_interval = 10
) {

  if (dt > 0) {
    warning(
      "dt > 0 passed to inv_inc_tree_extend_combined(): second-only plots ",
      "get a backward gnfi3 estimate, so a negative dt is expected. ",
      "Pass a negative dt to suppress this.",
      call. = FALSE
    )
  }

  # Meta already describes the whole enterprise (area and point count come from
  # the full second inventory in inv_inc_tree_extend()). Keep it aside because
  # bind_rows() drops attributes, and re-attach it to the result.
  inc_meta <- attr(tree_inc_extd, "inc_meta")

  # Second-only plot ids from the plot matching stored in the gap-fill object.
  b_only_ids <- inc_repinv_fl$plot_matches$in_b_only$plot_id

  # No second-only plots: nothing to fold in, return the matched frame as is.
  if (length(b_only_ids) == 0) {
    return(tree_inc_extd)
  }

  # Living trees on the second-only plots (see the !removal reasoning in the
  # function head).
  trees_2nd_only <- inv_2nd_trees |>
    dplyr::filter(.data$plot_id %in% b_only_ids, !.data$removal)

  # No living trees on the second-only plots: nothing to add.
  if (nrow(trees_2nd_only) == 0) {
    return(tree_inc_extd)
  }

  # Extend the second-only trees to the shared per-tree contract via a
  # short-window backward gnfi3 estimate. No inv = ... here: the combined
  # meta is taken from tree_inc_extd, not rebuilt from a single inventory.
  extd_2nd_only <- inv_inc_tree_extend_single(
    trees_2nd_only, dt = dt, d_q_interval = d_q_interval
  )

  # Split the point count into repeatedly measured (both surveys) and
  # BWI-3-backward-estimated (second-only) plots, so the base-table header can
  # report how the enterprise-wide estimate is composed. inc_meta$n_plots
  # already counts the full second inventory (matched + second-only), so the
  # repeated count is the remainder.
  if (!is.null(inc_meta) && !is.null(inc_meta$n_plots)) {
    inc_meta$n_plots_backward <- length(b_only_ids)
    inc_meta$n_plots_repeated <- inc_meta$n_plots - length(b_only_ids)
  }

  # Row-bind both sources (identical per-tree contract) and re-attach the
  # whole-enterprise meta.
  combined <- dplyr::bind_rows(tree_inc_extd, extd_2nd_only)
  attr(combined, "inc_meta") <- inc_meta
  combined
}



#' Aggregate Single Tree Increments to ha-Values per Species Group, Age- or
#' Diameter Class and Layer on Plot Level
#'
#' The ha-related values calculated here are to be understood as the
#' contributions of each tree cohort (as defined by species group, (age- or
#' diameter-) class, and layer) to the whole ha-wise increment of the plot.
#' Importantly, they do not relate to virtual area shares of the cohorts.
#' In an increment evaluation workflow, the intended use of this function is to
#' be applied to the output of
#' \code{\link{inv_inc_tree_extend}}
#'
#'
#' @param tree_inc_extd Data frame, output of \code{\link{inv_inc_tree_extend}}
#'
#' @param by_class Two options "age" (default), and "dq", which will aggregate
#'   the increments by age-, and mean diameter classes, respectively.
#'
#' @returns A data frame with increment sums grouped by \code{plot_id},
#'  \code{species_group}, \code{d_q_class} or \code{age_class},
#'  \code{layer_key}, \code{removal}
#'
#' @export
#'
#' @examples
#'   # Identify two subsequent inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # Matching and gap filling for this pair are shipped ready-made, see
#'   # ?data_ex3_increment_interim for how they were built
#'   inc_repinv    <- data_ex3_increment_matched
#'   inc_repinv_fl <- data_ex3_increment_fills
#'
#'   # Consolidate and extend tree increments
#'   inc_repinv_ce <- inv_inc_tree_consolidate(inc_repinv_fl) |>
#'     inv_inc_tree_extend(inv_a_trees, inv_b_trees, inv_b)
#'
#'   # Finally, aggregate on plot level
#'   inv_inc_tree_2_plot(inc_repinv_ce, "age")
#'   inv_inc_tree_2_plot(inc_repinv_ce, "dq")
#'
#' @family increment
inv_inc_tree_2_plot <- function(tree_inc_extd, by_class = c("age", "dq")) {

  by_class <- match.arg(by_class)

  # Grouping levels depend on choice of by_class
  if (by_class == "age")
    work_dat <- tree_inc_extd |>
      dplyr::group_by(
        plot_id, species_group, age_class, layer_key, removal
      )
  else
    work_dat <- tree_inc_extd |>
      dplyr::group_by(
        plot_id, species_group, d_q_class, layer_key, removal
      )

  # Calculate increment sums according to the selected grouping level
  work_dat <- work_dat |>
    dplyr::summarise(
      standing_area_m2_per_ha = sum(.data$standing_area_m2 * .data$n_rep_ha),
      iv_hub_m3_ha_per = sum(.data$iv_hub_m3_ha_per),
      iv_hub_m3_ha_yr  = sum(.data$iv_hub_m3_ha_yr),
      # number of trees where increment was estimated with the gnfi3 method
      n_iv_filled      = sum(.data$iv_filled),
      # Total number of (increment) trees in the group
      n_iv_total       = dplyr::n()
    ) |>
    dplyr::ungroup()

  # Get the representation areas of all plots
  plot_areas <- tree_inc_extd |>
    dplyr::select("plot_id", "area_rep_ha") |>
    dplyr::distinct()

  # Merge the areas in and return
  work_dat |>
    dplyr::left_join(plot_areas, by = "plot_id")
}



# Internal worker for increment_base_table(): area-weighted aggregation of the
# plot-level increment frame (output of inv_inc_tree_2_plot) to the inventory
# level for one grouping (a character vector of column names, length >= 1).
#
# Full-area-denominator convention (sum-consistency, as in inv_inc_big_overview):
# every plot contributes its representation area to every observed group, with a
# zero increment where the group is absent on that plot. This is realised with a
# zero-filled plot x group grid so that the area-weighted mean and its CI use the
# full plot set. Totals stay linear in the per-plot values, so species rows sum
# to the cross-species row and so on.
.increment_base_table_level <- function(inc2, group_cols, total_area_ha = NULL) {

  plot_areas <- inc2 |>
    dplyr::distinct(.data$plot_id, .data$area_rep_ha)

  # Reference area for the per-ha increment: the full operation area if supplied
  # (from inc_meta), otherwise the stocked collective area. Dividing the total
  # (and its CI) by this known constant gives the per-ha increment and its CI.
  ref_area <- if (is.null(total_area_ha)) sum(plot_areas$area_rep_ha) else
    total_area_ha

  # Per (plot, group) increment plus estimate counts
  per_plot <- inc2 |>
    dplyr::group_by(.data$plot_id, dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      iv_plot     = sum(.data$iv_hub_m3_ha_yr),
      n_iv_filled = sum(.data$n_iv_filled),
      n_iv_total  = sum(.data$n_iv_total),
      .groups     = "drop"
    )

  # Group-level counts: number of inventory points falling into the group
  # (n_plot_class), plus the estimate counts.
  counts <- per_plot |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_plot_class = dplyr::n_distinct(.data$plot_id),
      n_iv_filled  = sum(.data$n_iv_filled),
      n_iv_total   = sum(.data$n_iv_total),
      .groups      = "drop"
    )

  # Zero-filled plot x (observed group) grid for the full-area denominator
  groups_obs <- per_plot |>
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)))

  grid <- tidyr::crossing(plot_areas, groups_obs) |>
    dplyr::left_join(
      per_plot |> dplyr::select(
        "plot_id", dplyr::all_of(group_cols), "iv_plot"
      ),
      by = c("plot_id", group_cols)
    ) |>
    dplyr::mutate(iv_plot = dplyr::coalesce(.data$iv_plot, 0))

  # Area-weighted aggregation via the shared primitive; per-ha over ref_area.
  grid |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    se_area_weighted_grouped("iv_plot", "area_rep_ha") |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(group_cols)),
      iv_m3_yr_total      = .data$total,
      ci95_iv_m3_yr_total = .data$margin_total,
      iv_m3_ha_yr         = .data$total / ref_area,
      ci95_iv_m3_ha_yr    = .data$margin_total / ref_area
    ) |>
    dplyr::left_join(counts, by = group_cols) |>
    dplyr::select(
      dplyr::all_of(group_cols),
      "n_plot_class", "n_iv_total", "n_iv_filled",
      "iv_m3_yr_total", "ci95_iv_m3_yr_total",
      "iv_m3_ha_yr", "ci95_iv_m3_ha_yr"
    )
}



#' Require a Complete Stand Age for an Age-Based Increment Estimate
#'
#' Two of FeNEU's three increment strands - the yield-table estimate and the
#' gnfi3 (BWI 3) estimate - are driven by stand age: they age a tree forward (or
#' backward) and take the difference. A tree without an age simply yields
#' \code{NA} there, and because the aggregation drops \code{NA}s, the resulting
#' table looks complete while silently covering only part of the collective.
#' Rather than let that happen, the affected functions stop.
#'
#' The check runs on the collective the calling function actually evaluates
#' (after its own layer / removal filtering), so an inventory whose understorey
#' lacks ages can still be evaluated for the main stand.
#'
#' @param x A trees data frame.
#' @param fn_name Calling function name, used in the error message.
#'
#' @return \code{x}, invisibly and unchanged, when every tree carries an age.
#'
#' @keywords internal
#'
.assert_age_complete <- function(x, fn_name) {
  if (!("age_yr" %in% names(x)))
    stop(fe_msg("increment_age_col_missing", fn_name), call. = FALSE)

  n_missing <- sum(is.na(x$age_yr))
  if (n_missing > 0L)
    stop(fe_msg("increment_age_required", fn_name, n_missing, nrow(x)),
         call. = FALSE)

  invisible(x)
}



# Build the `collective` descriptor for a plain increment base table from the
# (possibly defused) tree_filter. The default filter TRUE (all trees, all
# layers, including trees removed between the inventories) denotes the "Alle
# Schichten" collective; the legacy standing-only filter !removal is mapped to
# the same label; any other filter is reported verbatim as a user-defined
# collective. Carried as a list element so downstream output and PDF rendering
# can label the collective without guessing from the columns.
.collective_from_filter <- function(filt_quo) {
  expr <- rlang::quo_get_expr(filt_quo)
  if (isTRUE(expr) || identical(expr, quote(!.data$removal))) {
    list(kind = "all_layers", label = "Alle Schichten",
         definition = if (isTRUE(expr)) "alle B\u00e4ume" else "!removal")
  } else {
    def <- gsub("\\.data\\$", "", rlang::as_label(filt_quo))
    list(
      kind       = "custom",
      label      = paste0("Benutzerdefiniertes Kollektiv: ", def),
      definition = def
    )
  }
}



# If the increment base table was built from a gnfi3 single-inventory source
# (inc_meta$source == "gnfi3"), blank the confidence-interval columns. Only the
# repeated-inventory method carries an empirical CI; for gnfi3 a spatial-sampling
# CI over the plots would capture only the between-plot variation of the modelled
# increments (not their model uncertainty) and would misleadingly look like a
# measurement-based CI. This matches the "Verfahren 2 & 3 tragen kein CI"
# convention already applied in the method overviews.
.drop_ci_for_gnfi3 <- function(bt) {
  if (!identical(bt$meta$source, "gnfi3")) return(bt)
  ci_cols <- c("ci95_iv_m3_yr_total", "ci95_iv_m3_ha_yr")
  for (el in c("detail", "total", "all_species", "all_total")) {
    present <- intersect(ci_cols, names(bt[[el]]))
    if (length(present)) bt[[el]][present] <- NA_real_
  }
  bt
}



#' Inventory-Level Increment Base Table by Species Group and Age or Diameter
#' Class
#'
#' Aggregates tree-level increments to the inventory level in the same
#' four-element layout as the static \code{\link{base_table_age_class}} /
#' \code{\link{base_table_d_q_class}} family, but for the annual volume increment
#' (\code{iv_hub_m3_ha_yr}) instead of standing volume. It is the increment
#' counterpart of the static base tables and the per-subunit breakdown analogue
#' of \code{\link{inv_inc_big_overview_matches_only}}.
#'
#' The function operates on the per-tree increment frame produced by
#' \code{\link{inv_increment_repeated_survey}} (repeated survey) or
#' \code{\link{inv_increment_gnfi3}} (single inventory). Both carry the
#' same per-tree contract, so the same base table is produced regardless of the
#' increment's origin. Internally the trees are first aggregated to plot level
#' with \code{\link{inv_inc_tree_2_plot}}, then area-weighted to the inventory
#' level via the shared primitive \code{\link{se_area_weighted_grouped}}.
#'
#' This is the \dQuote{plain} aggregation: each plot contributes its single
#' representation area (\code{area_rep_ha}); there is no split into virtual
#' monospecific areas (that is the job of a separate \code{_main_stand} variant).
#' The per-hectare values therefore relate to one hectare of total forest, and
#' the per-ha denominator is the full area across all plots in the input. Because
#' totals are linear in the per-plot values, the species rows of \code{detail}
#' sum (in \code{iv_m3_yr_total}) to the matching \code{all_species} row, and
#' \code{total} sums to \code{all_total} -- the central sum-consistency invariant
#' (confidence intervals are not additive and are computed independently per
#' level).
#'
#' @param tree_inc_extd Either an increment bundle -- the output of
#'   \code{\link{inv_increment_repeated_survey}} or of
#'   \code{\link{inv_increment_gnfi3}}, which is the usual case, its per-tree
#'   frame being taken from it -- or a bare per-tree increment data frame.
#'
#' @param by_class Either \code{"age"} (default) or \code{"dq"}, selecting
#'   aggregation by age class or by quadratic mean diameter class. Passed to
#'   \code{\link{inv_inc_tree_2_plot}}.
#'
#' @param tree_filter Expression describing which trees to keep, internally
#'   passed to \code{\link[dplyr]{filter}} on \code{tree_inc_extd}. Default
#'   \code{TRUE}, i.e. all trees of all layers -- including those removed between
#'   the two inventories, whose increment is part of the period's total
#'   production. Pass e.g. \code{!.data$removal} to restrict to standing trees.
#'
#' @returns A list of four tibbles, mirroring the static base table family:
#'   \describe{
#'     \item{detail}{One row per species group and class.}
#'     \item{total}{One row per species group, across all classes.}
#'     \item{all_species}{One row per class, across all species groups (the body
#'       of the \dQuote{Summe} block).}
#'     \item{all_total}{One row, the grand total.}
#'   }
#'   Each carries \code{n_plot_class} (the number of inventory points falling
#'   into the group), the annual increment as both an absolute total
#'   (\code{iv_m3_yr_total}) and per hectare of operation area
#'   (\code{iv_m3_ha_yr}) with their 95% confidence half-widths
#'   (\code{ci95_*}), and the counts \code{n_iv_filled} / \code{n_iv_total}
#'   (gnfi3-estimated vs. total tree increments in the group). The plain table
#'   carries no per-species \code{area_ha} (that is the main-stand variant's
#'   job). The list additionally carries a \code{collective}
#'   element (a list with \code{kind}, \code{label}, \code{definition})
#'   describing the tree collective the table represents: \code{"all_layers"}
#'   for the default \code{tree_filter}, otherwise a \code{"custom"} collective
#'   reporting the filter. It is carried through to the output and PDF steps so
#'   the collective can be labelled without re-deriving it. A \code{meta}
#'   element carries the header meta attached by \code{\link{inv_inc_tree_extend}}
#'   (survey years, period, area, point count, Berechnungs-/Ergänzungsmethode),
#'   or \code{NULL} for a single-inventory source.
#'
#' @family increment
#'
#' @export
#'
#' @examples
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # Prepare a single inventory's tree data (warnings/messages are ok here)
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # Single-inventory increment, then the inventory base table
#'   inc <- inv_increment_gnfi3(inv_b, inv_b_trees)
#'   bt  <- increment_base_table(inc, by_class = "age")
#'   bt$detail
#'   bt$all_total
#'
increment_base_table <- function(tree_inc_extd,
                                 by_class = c("age", "dq"),
                                 tree_filter = TRUE) {

  # Accept a per-tree increment bundle -- inv_increment_repeated_survey() or
  # inv_increment_gnfi3(), both of class fe_increment -- as well as a bare
  # per-tree frame; the bundle just carries that frame in $trees, with its
  # inc_meta attribute intact.
  tree_inc_extd <- .increment_bundle_trees(tree_inc_extd, "increment_base_table")

  by_class  <- match.arg(by_class)
  class_col <- if (by_class == "age") "age_class" else "d_q_class"
  filt_quo  <- rlang::enquo(tree_filter)

  # Header meta carried along from inv_inc_tree_extend() (NULL for a single
  # inventory that did not set it). Read before any operation on the object.
  inc_meta <- attr(tree_inc_extd, "inc_meta")

  # Plot-level aggregation (per species group x class x layer x removal),
  # after applying the tree filter.
  work_dat <- tree_inc_extd |> dplyr::filter(!!filt_quo)

  # An age-class breakdown of trees without an age is not a table with a gap,
  # it is a table that silently drops part of the collective: the NA ages form
  # no class, so their increment disappears from the class columns while the
  # total still counts it. Stop instead. Checked after the tree filter, i.e. on
  # exactly the collective that is evaluated, so a filtered subset with complete
  # ages remains evaluable. The dq breakdown is unaffected -- it needs no age.
  if (by_class == "age")
    .assert_age_complete(work_dat, "increment_base_table")

  inc2 <- work_dat |> inv_inc_tree_2_plot(by_class = by_class)

  # All-total level: no grouping, so aggregate the per-plot increment sums
  # directly via the shared primitive.
  plot_iv_all <- inc2 |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::summarise(
      iv_plot     = sum(.data$iv_hub_m3_ha_yr),
      area_rep_ha = unique(.data$area_rep_ha),
      .groups     = "drop"
    )
  # Per-ha reference area: full operation area if inc_meta carries it, else the
  # stocked collective area (single-inventory fall-back).
  ref_area_all <- if (is.null(inc_meta$area_ha))
    sum(plot_iv_all$area_rep_ha) else inc_meta$area_ha

  all_total_dat <- plot_iv_all |>
    se_area_weighted_grouped("iv_plot", "area_rep_ha") |>
    dplyr::transmute(
      iv_m3_yr_total      = .data$total,
      ci95_iv_m3_yr_total = .data$margin_total,
      iv_m3_ha_yr         = .data$total / ref_area_all,
      ci95_iv_m3_ha_yr    = .data$margin_total / ref_area_all
    ) |>
    dplyr::mutate(
      n_plot_class = dplyr::n_distinct(inc2$plot_id),
      n_iv_filled  = sum(inc2$n_iv_filled),
      n_iv_total   = sum(inc2$n_iv_total)
    ) |>
    dplyr::select(
      "n_plot_class", "n_iv_total", "n_iv_filled",
      "iv_m3_yr_total", "ci95_iv_m3_yr_total",
      "iv_m3_ha_yr", "ci95_iv_m3_ha_yr"
    )

  ta <- inc_meta$area_ha
  .drop_ci_for_gnfi3(list(
    detail      = .increment_base_table_level(inc2, c("species_group", class_col), ta),
    total       = .increment_base_table_level(inc2, "species_group", ta),
    all_species = .increment_base_table_level(inc2, class_col, ta),
    all_total   = all_total_dat,
    collective  = .collective_from_filter(filt_quo),
    meta        = inc_meta
  ))
}



# Internal worker for increment_base_table_main_stand(): like
# .increment_base_table_level(), but reports the per-hectare increment relative
# to the cohort's *virtual monospecific area* instead of the total forest area.
#
# The ideal (virtual monospecific) area per group is built as in the static
# main-stand base tables: a per-plot correction factor normalises the plot's
# main-stand standing area to one hectare, and each cohort gets its
# standing-area share of the plot's representation area. Unlike the static
# side, inc2 here includes trees removed between the inventories -- so both the
# increment and the ideal area span the same collective (numerator and
# denominator consistent, no zero-area edge case). Removed trees contribute
# their inv-1 standing area; surviving/ingrowth trees their inv-2 area (a mild
# temporal mix, documented in the "ideell" footnote). iv_m3_yr_total and its CI
# come from the same full-area ratio estimator as the plain table; the
# per-ideal-ha increment (iv_m3_ha_yr_virt) is iv_m3_yr_total / ideal area and
# carries NO CI. group_cols must have length >= 1 (the grand total is obtained
# via a constant dummy group).
.increment_base_table_main_level <- function(inc2, group_cols,
                                             total_area_ha = NULL) {

  plot_areas <- inc2 |>
    dplyr::distinct(.data$plot_id, .data$area_rep_ha)

  # Reference area for the operation-wide per-ha increment (same as the plain
  # table): the full operation area if supplied, else the stocked main-stand
  # area. Distinct from the per-species virtual area computed below.
  ref_area <- if (is.null(total_area_ha)) sum(plot_areas$area_rep_ha) else
    total_area_ha

  # Per (plot, group): increment, standing area (for the virtual area), counts
  per_plot <- inc2 |>
    dplyr::group_by(.data$plot_id, dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      iv_plot     = sum(.data$iv_hub_m3_ha_yr),
      sa_plot     = sum(.data$standing_area_m2_per_ha),
      n_iv_filled = sum(.data$n_iv_filled),
      n_iv_total  = sum(.data$n_iv_total),
      .groups     = "drop"
    )

  # Per-plot correction factor: normalise the plot's main-stand standing area
  # to one (virtual) hectare.
  corr <- inc2 |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::summarise(
      corr_fac = 10000 / sum(.data$standing_area_m2_per_ha),
      .groups  = "drop"
    )

  # Virtual monospecific area per group
  virt <- per_plot |>
    dplyr::left_join(corr, by = "plot_id") |>
    dplyr::left_join(plot_areas, by = "plot_id") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      area_ha = sum(.data$sa_plot / 10000 * .data$corr_fac * .data$area_rep_ha),
      .groups = "drop"
    )

  # Per-class point count + estimate counts
  counts <- per_plot |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_plot_class = dplyr::n_distinct(.data$plot_id),
      n_iv_filled  = sum(.data$n_iv_filled),
      n_iv_total   = sum(.data$n_iv_total),
      .groups      = "drop"
    )

  # Total + CI via the full-area ratio estimator (zero-filled plot x group grid)
  groups_obs <- per_plot |>
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)))
  grid <- tidyr::crossing(plot_areas, groups_obs) |>
    dplyr::left_join(
      per_plot |> dplyr::select("plot_id", dplyr::all_of(group_cols), "iv_plot"),
      by = c("plot_id", group_cols)
    ) |>
    dplyr::mutate(iv_plot = dplyr::coalesce(.data$iv_plot, 0))

  # Two per-ha increments: over the operation area (iv_m3_ha_yr, like the plain
  # table) and over the per-species virtual area (iv_m3_ha_yr_virt).
  grid |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    se_area_weighted_grouped("iv_plot", "area_rep_ha") |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(group_cols)),
      iv_m3_yr_total      = .data$total,
      ci95_iv_m3_yr_total = .data$margin_total,
      iv_m3_ha_yr         = .data$total / ref_area,
      ci95_iv_m3_ha_yr    = .data$margin_total / ref_area
    ) |>
    dplyr::left_join(virt,   by = group_cols) |>
    dplyr::left_join(counts, by = group_cols) |>
    dplyr::mutate(
      # Increment per ideal species area. No CI here: the ideal area is a
      # methodologically soft reference (see the "ideell" footnote), so a CI
      # would only add false precision.
      iv_m3_ha_yr_virt = .data$iv_m3_yr_total / .data$area_ha
    ) |>
    dplyr::select(
      dplyr::all_of(group_cols),
      "n_plot_class", "n_iv_total", "n_iv_filled",
      "iv_m3_yr_total", "ci95_iv_m3_yr_total",
      "iv_m3_ha_yr", "ci95_iv_m3_ha_yr",
      "area_ha", "iv_m3_ha_yr_virt"
    )
}



#' Inventory-Level Main-Stand Increment Base Table With Virtual Species Areas
#'
#' Main-stand counterpart of \code{\link{increment_base_table}}: it restricts
#' the aggregation to the main stand (\code{layer_key == 1}, standing trees) and
#' expresses the per-hectare increment relative to the cohort's \emph{virtual
#' monospecific area} rather than to one hectare of total forest. This mirrors
#' the static \code{\link{base_table_age_class_main_stand}} /
#' \code{\link{base_table_d_q_class_main_stand}} family. As there, species areas
#' are only formed for the main stand, because area shares of species in mixed
#' multi-layer stands are methodologically dubious (the same stance the Third
#' German National Forest Inventory took).
#'
#' The virtual areas are built exactly as on the static side: a per-plot
#' correction factor normalises the plot's main-stand standing area to one
#' hectare, and each cohort receives its standing-area share of the plot's
#' representation area. Unlike the static side, the main stand here includes
#' trees removed between the two inventories: their increment counts, and their
#' (inv-1) standing area counts towards the ideal species area, so numerator and
#' denominator span the same collective.
#'
#' Because every tree that contributes increment also carries a positive
#' standing area (removed trees at inv-1, surviving/ingrowth trees at inv-2),
#' every group that appears has a positive ideal area -- so the \dQuote{species
#' extinct but residual increment} edge case (division by a zero area) cannot
#' arise. It also keeps strong species-share shifts (e.g. after a storm) from
#' producing implausibly large per-ideal-hectare increments, because the removed
#' area enters the denominator as well.
#'
#' @param tree_inc_extd Either an increment bundle -- the output of
#'   \code{\link{inv_increment_repeated_survey}} or of
#'   \code{\link{inv_increment_gnfi3}}, which is the usual case, its per-tree
#'   frame being taken from it -- or a bare per-tree increment data frame.
#'
#' @param by_class Either \code{"age"} (default) or \code{"dq"}, passed to
#'   \code{\link{inv_inc_tree_2_plot}}.
#'
#' @returns A list of four tibbles (\code{detail}, \code{total},
#'   \code{all_species}, \code{all_total}) like \code{\link{increment_base_table}}
#'   (with \code{iv_m3_ha_yr} and its CI over the whole operation area),
#'   additionally holding \code{area_ha} (the ideal monospecific species area)
#'   and \code{iv_m3_ha_yr_virt} (the increment per that ideal area, without a
#'   CI). Unlike the static main-stand base tables it deliberately carries no
#'   per-species area-share column (\code{per_species}): in the increment context
#'   the ideal area is that of a period collective (including removed trees), so a
#'   percentage share would be more misleading than informative. Like
#'   \code{\link{increment_base_table}}, the list also carries a
#'   \code{collective} element (here fixed to the \code{"main_stand"} collective,
#'   \dQuote{Hauptbestand}) and a \code{meta} element with the header meta
#'   attached by \code{\link{inv_inc_tree_extend}} (\code{NULL} for a
#'   single-inventory source).
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
#'   bt  <- increment_base_table_main_stand(inc, by_class = "age")
#'   bt$detail
#'   bt$total
#'
increment_base_table_main_stand <- function(tree_inc_extd,
                                            by_class = c("age", "dq")) {

  # Accept a per-tree increment bundle as well as a bare per-tree frame (see
  # increment_base_table()).
  tree_inc_extd <- .increment_bundle_trees(
    tree_inc_extd, "increment_base_table_main_stand"
  )

  by_class  <- match.arg(by_class)
  class_col <- if (by_class == "age") "age_class" else "d_q_class"

  # Header meta carried along from inv_inc_tree_extend() (NULL for a single
  # inventory that did not set it). Read before any operation on the object.
  inc_meta <- attr(tree_inc_extd, "inc_meta")

  # Operation area for the operation-wide per-ha increment (NULL -> stocked
  # main-stand area fall-back inside the level worker).
  ta <- inc_meta$area_ha

  # Main stand -- ALL trees, including those removed between the inventories.
  # Their increment counts, and their (inv-1) standing area counts towards the
  # ideal species area, so numerator and denominator span the same collective
  # (no zero-area edge case).
  work_dat <- tree_inc_extd |> dplyr::filter(.data$layer_key == 1)

  # See increment_base_table(): an age-class breakdown requires every evaluated
  # tree to carry an age. Checked after the main-stand filter, so an inventory
  # whose understorey lacks ages stays evaluable for the main stand.
  if (by_class == "age")
    .assert_age_complete(work_dat, "increment_base_table_main_stand")

  inc2 <- work_dat |> inv_inc_tree_2_plot(by_class = by_class)

  # Note: unlike the static main-stand base tables, the increment tables do NOT
  # report a per-species area share ("Ant. (%)"). In the increment context the
  # ideal area is that of a period collective (it includes the standing areas of
  # trees removed between the inventories), so a tidy percentage would invite the
  # wrong reading "current species mixture". The absolute ideal area
  # (area_ha, "Fl. (ha) ideell") and the ideal-area increment (iv_m3_ha_yr_virt,
  # "zv/ha ideell") are kept.
  detail_dat      <- .increment_base_table_main_level(
    inc2, c("species_group", class_col), ta
  )
  total_dat       <- .increment_base_table_main_level(inc2, "species_group", ta)
  all_species_dat <- .increment_base_table_main_level(inc2, class_col, ta)

  # all_total: grand total (constant dummy group)
  all_total_dat <- inc2 |>
    dplyr::mutate(summe_grp = 1L) |>
    .increment_base_table_main_level("summe_grp", ta) |>
    dplyr::select(-"summe_grp")

  .drop_ci_for_gnfi3(list(
    detail      = detail_dat,
    total       = total_dat,
    all_species = all_species_dat,
    all_total   = all_total_dat,
    collective  = list(
      kind       = "main_stand",
      label      = "Hauptbestand",
      definition = "layer_key == 1 & !removal"
    ),
    meta        = inc_meta
  ))
}



#' Compute the Summaric Increment per Plot, Overall and by Species Group
#'
#' Computes a pure balance increment per plot that does not require any tree
#' matching assumptions. The formula is:
#' \code{inc_summaric = (V_end - V_start + V_removed) / period}, where
#' \code{V_end} is the total standing volume at the second inventory (excluding
#' removed trees), \code{V_start} is the total standing volume at the first
#' inventory, and \code{V_removed} is the forward-estimated volume of trees
#' that were removed between inventories. The forward estimation relies on
#' gnfi3-based estimates from the "standard" fill option, which is why the
#' input must come from \code{\link{inv_inc_tree_consolidate}} with
#' \code{fill_option = "standard"}, extended by
#' \code{\link{inv_inc_tree_extend}}.
#'
#' The classical German forestry term for this quantity is
#' "ertragsgeschichtlicher Zuwachs" (literally: yield-history increment).
#' There is no established English equivalent; we use the term "summaric
#' increment" here to express that this increment is derived purely from
#' summary-level volume balances between two inventories, without relying on
#' individual tree identity across surveys.
#'
#' Methodological role. The ertragsgeschichtliche increment is the
#' **Goldstandard** for the Betriebsebenen-Zuwachs — the best estimate of
#' the enterprise-wide annual increment that two inventories can yield.
#' Unlike the four single-tree methods (\code{rep_classic}, \code{rep_mean},
#' \code{rep_end}, \code{rep_trans}) it is a pure mass balance over the
#' measured tree population; it does not depend on assumptions about
#' which inventory's representation number to use for trees that change
#' circles, and it does not need a per-tree pairing across inventories.
#' For that reason it stands architecturally apart from the per-tree
#' pipeline (\code{\link{inv_inc_tree_consolidate}} and
#' \code{\link{inv_inc_tree_extend}}): summaric pulls its inputs
#' (\code{matched_trees}, \code{fills_forward}, and the inv-trees lookups
#' for species groups) directly, so its balance always sees the full
#' measured population including the unmatchable inner-circle cohort —
#' independent of any fill_option choice on the per-tree side.
#'
#' Computational structure on each plot:
#' \itemize{
#'   \item \code{v_hub_m3_ha_1st}: sum of \code{n_rep_ha_1st * v_hub_m3_1st}
#'     over all inv-1 measurements present in \code{matched_trees},
#'     including the rows flagged \code{is_unmatchable_1st = TRUE} (e.g.
#'     inner-circle trees recorded without polar coordinates).
#'   \item \code{v_hub_m3_ha_2nd}: sum of \code{n_rep_ha_2nd * v_hub_m3_2nd}
#'     over all living inv-2 measurements (no removal flag), including
#'     unmatchable inv-2 rows and the inv-2 records of pseudo-ingrowth-
#'     reclassified trees (the latter contribute only their inv-2 volume
#'     after the local undo described below).
#'   \item \code{v_hub_m3_ha_aus}: sum of \code{n_rep_ha_1st * v_hub_m3_1st}
#'     plus the half-period gnfi3 forward estimate, over the
#'     verified-ausscheider subset (\code{tree_exists_1st} is TRUE and
#'     either \code{tree_exists_2nd} is FALSE or the tree carries a
#'     removal flag at inv 2). The forward estimate is pulled from
#'     \code{fills_forward}. Unmatchable rows have NA on the counterpart
#'     side and are filtered out by NA-semantics so they never inflate
#'     \code{v_aus}.
#' }
#' Pseudo-ingrowth-reclassified trees have their reclassification
#' \emph{locally undone} inside summaric so that their inv-1 reconstruction
#' (gnfi3 backward) does not enter the balance. They contribute only via
#' their measured inv-2 volume — exactly the methodological treatment we
#' want for the unmatchable inner-circle cohort.
#'
#' @param matched_trees Data frame, the \code{$tree_increments} element of
#'   \code{\link{inv_increment_repsurv_ccirc}}'s output. Carries all rows
#'   needed by the summaric balance — including the unmatchable inner-
#'   circle cohort (\code{is_unmatchable_1st = TRUE} / \code{NA} on the
#'   counterpart side of \code{tree_exists_*}).
#'
#' @param fills_forward Data frame, the \code{$fills_forward} element of
#'   \code{\link{inv_inc_fill_gaps_gnfi3}}'s output. Provides the
#'   half-period gnfi3 forward increment estimate (\code{iv_hub_m3_ha_per_gnfi3})
#'   for the \code{v_aus} term. summaric joins the relevant value back to
#'   \code{matched_trees} by \code{(plot_id_1st, tree_id_1st)}.
#'
#' @param inv_1st_trees Tree data frame as pulled and extended from the
#'   first \code{\link{fe_inventory}} object. Used for the species-group
#'   lookup of trees that exist only in the first inventory (removed trees).
#'
#' @param inv_2nd_trees Tree data frame for the second inventory. Used for
#'   the species-group lookup of trees that exist in the second inventory
#'   (the dominant case — both-exist trees, ingrowth, and unmatchable inv-2).
#'
#' @returns A list with two tibbles:
#'   \describe{
#'     \item{overall}{One row per plot, columns \code{plot_id},
#'       \code{v_hub_m3_ha_1st}, \code{v_hub_m3_ha_2nd},
#'       \code{v_hub_m3_ha_aus}, \code{inc_summaric_m3_ha_yr}.}
#'     \item{by_species_group}{One row per (plot, species_group) combination,
#'       same volume / increment columns as \code{overall} plus
#'       \code{species_group}. Combinations where a species group is absent
#'       on a plot are not represented; consumers that need a complete grid
#'       (e.g. for area-weighted aggregation across all plots) must fill in
#'       zeros.}
#'   }
#'
#' @export
#'
#' @examples
#'   # Identify two subsequent inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # Pull and prepare tree data
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # Matching and gap filling for this pair are shipped ready-made, see
#'   # ?data_ex3_increment_interim for how they were built
#'   inc_repinv    <- data_ex3_increment_matched
#'   inc_repinv_fl <- data_ex3_increment_fills
#'
#'   # Summaric goes directly off matched_trees + fills_forward —
#'   # no consolidate / extend in between.
#'   inv_inc_summaric(
#'     inc_repinv$tree_increments,
#'     inc_repinv_fl$fills_forward,
#'     inv_a_trees, inv_b_trees
#'   )
#'
#' @family increment
inv_inc_summaric <- function(
    matched_trees, fills_forward, inv_1st_trees, inv_2nd_trees
) {

  if (nrow(matched_trees) == 0)
    stop(fe_msg("matched_trees_empty"), call. = FALSE)

  # Inventory period. time_yr_1st and time_yr_2nd are uniform across all
  # rows of matched_trees (set per-plot from the fe_inventory time_yr
  # slot in match_trees_on_plot_repsurv_ccirc, then constant within a
  # matched plot pair). Take the first non-NA pair.
  period_yr <- matched_trees$time_yr_2nd[1] - matched_trees$time_yr_1st[1]
  if (is.na(period_yr) || period_yr <= 0)
    stop(fe_msg("inv_period_invalid"), call. = FALSE)

  # Pull the per-tree half-period gnfi3 forward estimate for the v_aus
  # term out of fills_forward and join it back onto matched_trees by
  # (plot_id_1st, tree_id_1st). Rows that fills_forward does not contain
  # (= every row that is not a verified ausscheider) get NA, which is
  # safely ignored by the v_aus filter below.
  iv_per_lookup <- fills_forward |>
    dplyr::select(
      "plot_id_1st", "tree_id_1st",
      iv_hub_m3_ha_per = "iv_hub_m3_ha_per_gnfi3"
    )

  work_dat <- matched_trees |>
    dplyr::left_join(
      iv_per_lookup, by = c("plot_id_1st", "tree_id_1st")
    ) |>
    dplyr::mutate(.is_removal = .data$tree_removal_2nd %in% TRUE)

  # Species-group lookup. The species group is taken from inv-2 for
  # trees that exist in inv-2 (the dominant case — re-measured, real
  # ingrowth, unmatchable inv-2, and pseudo-ingrowth-corrected) and
  # from inv-1 for the rest (verified ausscheider and 1st-only-
  # unmatchable).
  sp_1st <- inv_1st_trees |>
    dplyr::select("plot_id", "tree_id", species_group_1 = "species_group")
  sp_2nd <- inv_2nd_trees |>
    dplyr::select("plot_id", "tree_id", species_group_2 = "species_group")

  work_dat <- work_dat |>
    dplyr::left_join(
      sp_1st, by = c("plot_id_1st" = "plot_id", "tree_id_1st" = "tree_id")
    ) |>
    dplyr::left_join(
      sp_2nd, by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    ) |>
    dplyr::mutate(
      species_group = dplyr::coalesce(.data$species_group_2,
                                       .data$species_group_1),
      plot_id       = .data$plot_id_2nd
    )

  # Pseudo-ingrowth correction is for per-tree methods only. The
  # ertragsgeschichtliche balance is by definition built on actually
  # measured volumes: trees that were not identified at the first
  # survey (the inv_1 inner-circle cohort) were not measured, so they
  # must enter the summaric balance solely via the inv_2 sum — neither
  # in v_hub_m3_ha_1st nor in v_hub_m3_ha_aus. Increment estimates for
  # half the period (gnfi3 forward) are reserved for trees that were
  # unambiguously identified as disappearing between the two surveys.
  # We therefore undo reclassify_pseudo_ingrowth_ccirc() locally on the
  # working copy. The flag column stays untouched in matched_trees, so
  # callers that want to see the corrected state still can.
  if ("pseudo_ingrowth_corrected" %in% names(work_dat)) {
    is_pseudo <- work_dat$pseudo_ingrowth_corrected %in% TRUE
    if (any(is_pseudo)) {
      work_dat$tree_exists_1st[is_pseudo] <- FALSE
      work_dat$v_hub_m3_1st[is_pseudo]    <- NA_real_
      work_dat$n_rep_ha_1st[is_pseudo]    <- NA_real_
    }
  }

  # Reusable summarisation; the grouping varies between overall and the
  # species-group breakdown but the volume contributions are identical.
  summarise_summaric <- function(dat, grouping_vars) {
    dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
      dplyr::summarise(
        # V at inventory 1: all trees that existed at inv 1
        # Trees without coordinates (tree_exists_1st = NA) have
        # v_hub_m3_1st = NA and are correctly excluded by the is.na check
        v_hub_m3_ha_1st = sum(
          ifelse(!is.na(.data$v_hub_m3_1st),
                 .data$v_hub_m3_1st * .data$n_rep_ha_1st, 0)
        ),
        # V at inventory 2: all living trees at inv 2 (excluding removals)
        v_hub_m3_ha_2nd = sum(
          ifelse(!is.na(.data$v_hub_m3_2nd) & !.data$.is_removal,
                 .data$v_hub_m3_2nd * .data$n_rep_ha_2nd, 0)
        ),
        # V removed: trees that existed at inv 1 and are *verifiably*
        # missing or flagged as removed at inv 2. Volume at time of
        # removal is estimated as the volume at inv 1 plus the forward
        # increment (gnfi3 estimate for half the period). The filter
        # uses `tree_exists_2nd %in% FALSE` so that NA values — which
        # mark unmatchable rows whose inv_2 status is unknown by design
        # (NA coords on inv_2 in baysf, or held-aside inner-circle in
        # generic + inner_circle_non_matchable, or synthesized 1st-only-
        # unmatchable rows added in match_trees_on_inventory_repsurv_ccirc)
        # — never fire this branch. That keeps unmatchable trees out of
        # v_aus automatically, without needing an explicit
        # `!is_unmatchable_1st` guard: NA-semantics carries the meaning.
        # Their inv_1 volume still contributes to v_1st above, and the
        # aggregate v_2 − v_1 balance recovers the unmatchable-cohort
        # change. The unavoidable residual is the half-period forward
        # extrapolation of genuinely deceased unmatchable inv_1 trees —
        # small per tree, documented as the inherent cost of
        # "Ausscheider bekommen wir nie" for unmatchable cohorts.
        v_hub_m3_ha_aus = sum(
          ifelse(
            .data$tree_exists_1st %in% TRUE &
              (.data$tree_exists_2nd %in% FALSE | .data$.is_removal),
            .data$n_rep_ha_1st * .data$v_hub_m3_1st + .data$iv_hub_m3_ha_per,
            0
          )
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        inc_summaric_m3_ha_yr = (.data$v_hub_m3_ha_2nd -
                                   .data$v_hub_m3_ha_1st +
                                   .data$v_hub_m3_ha_aus) / period_yr
      )
  }

  list(
    overall = summarise_summaric(work_dat, "plot_id"),
    by_species_group = summarise_summaric(
      work_dat, c("plot_id", "species_group")
    )
  )
}



#' Inventory-Wide Increment Overview for the Matched Plots of a Repeated
#' Survey
#'
#' Produces the area-weighted increment overview from those plots that
#' appear in *both* inventories — i.e. the subset for which repeated
#' tree-level measurements are available. The companion function
#' \code{\link{inv_inc_big_overview_2nd_only}} covers the plots that only
#' exist in the second survey (new establishments, sample extensions);
#' the top-level \code{\link{inv_inc_big_overview_matches_only}} combines both.
#'
#' @param tree_inc_rep_gapfill Output object of
#'   \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @param inv_1st_trees Tree data frame as pulled and extended from the the
#'   \code{\link{fe_inventory}} object that represents the first of two
#'   subsequent surveys. Should be the same as previously used as input for
#'   \code{\link{inv_increment_repsurv_ccirc}}.
#'
#' @param inv_2nd_trees Tree data frame as pulled and extended from the the
#'   \code{\link{fe_inventory}} object that represents the second of two
#'   subsequent surveys. Should be the same as previously used as input for
#'   \code{\link{inv_increment_repsurv_ccirc}}.
#'
#' @param inv_2nd \code{\link{fe_inventory}} object that represents the second
#'   of the two inventories of interest.
#'
#' @param style Two options \code{"generic"} and \code{"baysf"}. The former
#'   groups the most important part of the output by the tree cohorts that
#'   are internally distinguished when calculating/estimating increments. The
#'   latter applies a similar grouping as common in the Bavarian State Forest.
#'   The two styles differ only in the \code{tree_status} categories used in
#'   the \code{iv_by_groups} breakdown:
#'   \describe{
#'     \item{\code{"baysf"}}{\code{"living"}, \code{"missing"},
#'       \code{"rem_mort"}, \code{"no_coord"} (table-63-like, kept stable).}
#'     \item{\code{"generic"}}{\code{"regular"}, \code{"ingrown"},
#'       \code{"missing"}, \code{"rem_mort_regular"},
#'       \code{"rem_mort_ingrown"}, \code{"no_track"}, plus
#'       \code{"pseudo_ingrown"} for trees that
#'       \code{\link{reclassify_pseudo_ingrowth_ccirc}} reclassified from
#'       ingrowth to re-measured (inner-circle Inv-1 cohort). They carry a
#'       regular increment but are split out here so the pseudo-ingrowth
#'       contribution is visible in the aggregate. Only the \code{"generic"}
#'       style breaks this cohort out; under \code{"baysf"} they remain part
#'       of \code{"living"}.}
#'   }
#'
#' @param inc_summaric Output of \code{\link{inv_inc_summaric}}, i.e. a list
#'   with the elements \code{$overall} (per-plot summaric increment) and
#'   \code{$by_species_group} (per plot × species group). Its area-weighted
#'   mean is appended as an additional row \code{"inc_summaric"} in the
#'   \code{iv_total} elements of both \code{$overall} and
#'   \code{$by_species_group} (one row per species group there). The
#'   summaric increment is the most reliable overall increment estimate
#'   available from a repeated survey, as it relies purely on volume
#'   balances. The tree-level methods exist because they enable breakdowns
#'   by layers, diameter class, etc., which a pure balance can also
#'   provide once it is partitioned (here: by species group).
#'
#' @returns A list with three top-level elements:
#'   \describe{
#'     \item{overall}{A sub-list with
#'       \describe{
#'         \item{iv_by_groups}{Increments aggregated by fill_option, tree
#'           identification status, and tree status category}
#'         \item{iv_total}{Total increments per fill_option, plus an
#'           additional row \code{"inc_summaric"} with the summaric
#'           (yield-history) increment as the most reliable reference value}
#'         \item{v_total}{Total standing volume at the second inventory}
#'         \item{n_plots}{Number of plots and total represented area}
#'       }
#'     }
#'     \item{by_species_group}{A sub-list with the same structure as
#'       \code{overall} (minus \code{n_plots}, which would be redundant),
#'       but every tibble is additionally grouped by \code{species_group}.
#'       The total area (\code{area_total_ha}) stays at the area across all
#'       plots, so that plots without trees of a given species group still
#'       contribute their area to the denominator. Consequently, sums of
#'       per-species values (\code{iv_m3_yr_total}, \code{iv_m3_ha_yr},
#'       \code{v_hub_m3_ha}) across species groups match the corresponding
#'       overall values, and the same applies row by row to the appended
#'       \code{"inc_summaric"} entries.}
#'     \item{method}{The increment method used in the upstream
#'       \code{\link{inv_increment_repsurv_ccirc}} call (carried through
#'       \code{\link{inv_inc_fill_gaps_gnfi3}}).}
#'   }
#'
#' @export
#'
#' @examples
#'   # Identify two subsequent inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # Matching and gap filling for this pair are shipped ready-made, see
#'   # ?data_ex3_increment_interim for how they were built
#'   inc_repinv    <- data_ex3_increment_matched
#'   inc_repinv_fl <- data_ex3_increment_fills
#'
#'   # Compute the summaric increment per plot (required for big_overview).
#'   # Note: summaric reads matched_trees and fills_forward directly — it
#'   # does NOT go through the consolidate / extend per-tree pipeline,
#'   # so its balance always sees the full measured population.
#'   inc_sum <- inv_inc_summaric(
#'     inc_repinv$tree_increments, inc_repinv_fl$fills_forward,
#'     inv_a_trees, inv_b_trees
#'   )
#'
#'   # Finally generate the matched-plots overview, use both styles
#'   inv_inc_big_overview_matches_only(
#'     inc_repinv_fl, inv_a_trees, inv_b_trees, inv_b,
#'     style = "generic", inc_summaric = inc_sum
#'   )
#'   inv_inc_big_overview_matches_only(
#'     inc_repinv_fl, inv_a_trees, inv_b_trees, inv_b,
#'     style = "baysf", inc_summaric = inc_sum
#'   )
#'
#' @family increment
inv_inc_big_overview_matches_only <- function(
    tree_inc_rep_gapfill, inv_1st_trees, inv_2nd_trees, inv_2nd,
    style = c("generic",  "baysf"),
    inc_summaric
) {

  style <- match.arg(style)

  # Consolidate increments with all available fill options (i.e.
  # using gnfi3 estimates)
  work_dat_std <- inv_inc_tree_consolidate(
    tree_inc_rep_gapfill, fill_option = "standard"
  ) |>
    dplyr::mutate(fill_option = "standard")

  work_dat_min <- inv_inc_tree_consolidate(
    tree_inc_rep_gapfill, fill_option = "min_estimates"
  ) |>
    dplyr::mutate(fill_option = "min_estimates")

  work_dat_all <- inv_inc_tree_consolidate(
    tree_inc_rep_gapfill, fill_option = "all_estimates"
  ) |>
    dplyr::mutate(fill_option = "all_estimates")

  # Make one big data frame
  work_dat <- dplyr::bind_rows(work_dat_std, work_dat_min, work_dat_all)

  # No NAs in the volume increment is a crucial condition
  if (any(is.na(work_dat$iv_hub_m3_ha_yr)))
    stop(
      paste0(
        "NA increment values not acceptable for aggregation. ",
        "Check 'include_reptrees' option used with inv_inc_fill_gaps_gnfi3() ",
        "(must be TRUE)."
      )
    )

  # All three versions of work_dat should have equal length
  check_vec <- c(nrow(work_dat_std), nrow(work_dat_min), nrow(work_dat_all))
  if (length(unique(check_vec)) != 1)
    warning(
      paste0(
        "Deviating tree numbers for different fill_options in ",
        "inv_inc_big_overview_matches_only."
      )
    )

  # Extend the tree information in order to obtain upscaleable information
  # n_rep ha
  work_dat <- FeNEU::inv_inc_tree_extend(
    work_dat, inv_1st_trees, inv_2nd_trees, inv_2nd
  )

  # The generic tree_status below distinguishes pseudo-ingrowth via the
  # pseudo_ingrowth_corrected flag. That flag is always added upstream by
  # reclassify_pseudo_ingrowth_ccirc() and carried through the fill-gap and
  # consolidate steps, but guard defensively: ensure the column exists and
  # coerce NA to FALSE so the case_when branch is clean.
  if (!"pseudo_ingrowth_corrected" %in% names(work_dat)) {
    work_dat$pseudo_ingrowth_corrected <- FALSE
  }
  work_dat$pseudo_ingrowth_corrected <-
    work_dat$pseudo_ingrowth_corrected %in% TRUE

  # Introduce tree status categories
  if (style == "baysf") {
    # Categories equal/similar to those used by the BaySF in their table 63
    work_dat <- work_dat |>
      dplyr::mutate(
        # TRUE for trees that are uniquely identified in at least one inventory
        id_able_tree =
          !is.na(.data$tree_exists_1st), # & !is.na(.data$tree_exists_2nd),
        # Status entries for presence in inventories
        # Could be simplified, but tried to cover each case specifically
        tree_status = dplyr::case_when(
          !.data$tree_exists_2nd  ~ "missing",
          .data$tree_exists_2nd & is.na(.data$tree_exists_1st) ~ "no_coord",
          .data$tree_exists_2nd & .data$tree_exists_1st &
            .data$tree_removal_2nd ~ "rem_mort",
          .data$tree_exists_2nd & !.data$tree_exists_1st &
            .data$tree_removal_2nd ~ "rem_mort",
          .data$tree_exists_2nd & !.data$tree_exists_1st &
            !.data$tree_removal_2nd ~ "living",
          .data$tree_exists_2nd & .data$tree_exists_1st &
            !.data$tree_removal_2nd ~ "living",
          # Default for being sure nothing has been missed
          .default = "other"
        )
      )
  } else {
    # Generic categories, very good for diagnostic purposes
    work_dat <- work_dat |>
      dplyr::mutate(
        # TRUE for trees that are uniquely identified in at least one inventory
        id_able_tree =
          !is.na(.data$tree_exists_1st), # & !is.na(.data$tree_exists_2nd),
        # Status entries for presence in inventories
        tree_status = dplyr::case_when(
          !.data$tree_exists_2nd  ~ "missing",
          .data$tree_exists_2nd & is.na(.data$tree_exists_1st) ~ "no_track",
          .data$tree_exists_2nd & .data$tree_exists_1st &
            .data$tree_removal_2nd ~ "rem_mort_regular",
          .data$tree_exists_2nd & !.data$tree_exists_1st &
            .data$tree_removal_2nd ~ "rem_mort_ingrown",
          .data$tree_exists_2nd & !.data$tree_exists_1st &
            !.data$tree_removal_2nd ~ "ingrown",
          # Pseudo-ingrowth: re-classified inner-circle trees that look like
          # re-measured trees (both surveys TRUE) but conceptually belong to
          # the unmatchable Inv-1 inner-circle cohort. Must be tested BEFORE
          # the generic "regular" branch, which they would otherwise match.
          .data$tree_exists_2nd & .data$tree_exists_1st &
            !.data$tree_removal_2nd & .data$pseudo_ingrowth_corrected ~
            "pseudo_ingrown",
          .data$tree_exists_2nd & .data$tree_exists_1st &
            !.data$tree_removal_2nd ~ "regular",
          # Default for being sure nothing has been missed
          .default = "other"
        )
      )
  }

  # Connect with plot representation areas from the second inventory.
  # The area_rep_ha contained in work_dat is from the first inventory
  # for trees that were existing in the first survey only; the
  # aggregated evaluation, however, must fully relate to the
  # area_rep_ha from the second survey.
  #
  # Plot universe: all plots that are matched between the two
  # inventories -- including those without trees in either inventory
  # (treeless matched plots are still valid sample points and must
  # count toward the matched plot total and area, with iv = 0). The
  # matched plot ids come directly from plot_matches, so the count is
  # consistent with match_plot_statistics() and the BaySF reference
  # statistics, independent of whether a plot has trees in work_dat.
  matched_plot_ids <- tree_inc_rep_gapfill$plot_matches$matches$plot_id_inv_b

  area_info_2nd <- inv_2nd |>
    dplyr::select(-"plot") |>
    dplyr::filter(.data$plot_id %in% matched_plot_ids) |>
    dplyr::rename(area_rep_ha_2nd = "area_rep_ha")

  area_total_2nd <- sum(area_info_2nd$area_rep_ha_2nd)
  n_matched_plots <- length(matched_plot_ids)

  work_dat <- work_dat |>
    dplyr::left_join(area_info_2nd, by = "plot_id")

  # Big aggregation
  ## by_groups (fill_option, idable, tree_status)
  agg_by_groups <- work_dat |>
    dplyr::group_by(fill_option, id_able_tree, tree_status) |>
    dplyr::summarise(
      n = dplyr::n(),
      iv_m3_yr_total = sum(
        .data$iv_hub_m3_ha_yr * .data$area_rep_ha
      ),
    ) |>
    dplyr::mutate(
      area_total_ha = area_total_2nd,
      iv_m3_ha_yr   = .data$iv_m3_yr_total / .data$area_total_ha
    )

  ## Warn if the tree_status category "other" turns up;
  ## this points out a logical flaw
  if ("other" %in% agg_by_groups$tree_status)
    warning(fe_msg("tree_status_other"))

  ## total (fill_option only)
  agg_by_total <- agg_by_groups |>
    dplyr::group_by(fill_option) |>
    dplyr::summarise(
      n              = sum(.data$n),
      iv_m3_yr_total = sum(.data$iv_m3_yr_total),
      iv_m3_ha_yr    = sum(.data$iv_m3_ha_yr)
    ) |>
    dplyr::mutate(area_total_ha = area_total_2nd) |>
    dplyr::relocate("area_total_ha", .before = "iv_m3_ha_yr")

  # Area-weighted standard errors per fill_option
  # First aggregate per-ha increments to the plot level
  plot_iv <- work_dat |>
    dplyr::group_by(fill_option, plot_id) |>
    dplyr::summarise(
      iv_m3_ha_yr_plot = sum(.data$iv_hub_m3_ha_yr),
      .groups = "drop"
    ) |>
    dplyr::left_join(area_info_2nd, by = "plot_id")

  # Compute SE for each fill_option via the shared area-weighted primitive
  se_by_fill <- plot_iv |>
    dplyr::group_by(fill_option) |>
    se_area_weighted_grouped("iv_m3_ha_yr_plot", "area_rep_ha_2nd") |>
    dplyr::transmute(
      fill_option         = .data$fill_option,
      ci95_iv_m3_ha_yr    = .data$margin_mean,
      ci95_iv_m3_yr_total = .data$margin_total
    )

  agg_by_total <- agg_by_total |>
    dplyr::left_join(se_by_fill, by = "fill_option")

  # As part of the aggregation, make a data frame about the standing volume
  # We have to filter one of the three fill_options (otherwise our results
  # would be three times too big)
  v_total <- work_dat |>
    dplyr::filter(.data$fill_option == "standard") |>
    dplyr::filter(.data$tree_exists_2nd & !.data$removal) |>
    dplyr::summarise(
      v_hub_m3 = sum(.data$v_hub_m3 * .data$n_rep_ha * .data$area_rep_ha)
    ) |>
    dplyr::mutate(
      area_total_ha = area_total_2nd,
      v_hub_m3_ha   = .data$v_hub_m3 / .data$area_total_ha
    )

  # Number of plots and average area. n_plots is the raw matched plot
  # count (= nrow(plot_matches$matches)), including matched plots
  # without trees. Consistent with match_plot_statistics() and the
  # BaySF reference statistics.
  n_plots <- tibble::tibble(
    n_plots = n_matched_plots,
    area_total_ha = area_total_2nd
  ) |>
    dplyr::mutate(
      avg_rep_ha_plot = .data$area_total_ha / .data$n_plots
    )

  # Aggregate the summaric increment and append to iv_total as reference
  inc_summaric_with_area <- inc_summaric$overall |>
    dplyr::left_join(area_info_2nd, by = "plot_id")

  inc_summaric_total <- inc_summaric_with_area |>
    dplyr::summarise(
      n_plots        = dplyr::n(),
      iv_m3_yr_total = sum(
        .data$inc_summaric_m3_ha_yr * .data$area_rep_ha_2nd
      ),
      area_total_ha = sum(.data$area_rep_ha_2nd)
    ) |>
    dplyr::mutate(
      iv_m3_ha_yr = .data$iv_m3_yr_total / .data$area_total_ha
    )

  se_inc_sum <- se_area_weighted(
    inc_summaric_with_area$inc_summaric_m3_ha_yr,
    inc_summaric_with_area$area_rep_ha_2nd
  )

  inc_summaric_row <- tibble::tibble(
    fill_option         = "inc_summaric",
    n                   = nrow(work_dat_std),
    ci95_iv_m3_yr_total = se_inc_sum$margin_total,
    area_total_ha       = inc_summaric_total$area_total_ha,
    iv_m3_ha_yr         = inc_summaric_total$iv_m3_ha_yr,
    iv_m3_yr_total      = inc_summaric_total$iv_m3_yr_total,
    ci95_iv_m3_ha_yr    = se_inc_sum$margin_mean
  )
  agg_by_total <- dplyr::bind_rows(agg_by_total, inc_summaric_row) |>
    dplyr::select(
      "fill_option", "n", "iv_m3_yr_total",
      "ci95_iv_m3_yr_total", "area_total_ha", "iv_m3_ha_yr",
      "ci95_iv_m3_ha_yr"
    )

  # Breakdown by species_group.
  # Invariant: sums across species_group rows equal the overall values.
  # Denominators therefore stay at the full area across ALL plots — plots
  # where a given species_group is absent still contribute their area, with
  # zero increment / volume for that group.
  all_plots <- area_info_2nd |>
    dplyr::select("plot_id", "area_rep_ha_2nd")

  agg_by_groups_sg <- work_dat |>
    dplyr::group_by(
      .data$fill_option, .data$species_group, .data$id_able_tree,
      .data$tree_status
    ) |>
    dplyr::summarise(
      n              = dplyr::n(),
      iv_m3_yr_total = sum(.data$iv_hub_m3_ha_yr * .data$area_rep_ha),
      .groups        = "drop"
    ) |>
    dplyr::mutate(
      area_total_ha = area_total_2nd,
      iv_m3_ha_yr   = .data$iv_m3_yr_total / .data$area_total_ha
    )

  agg_by_total_sg <- agg_by_groups_sg |>
    dplyr::group_by(fill_option, species_group) |>
    dplyr::summarise(
      n              = sum(.data$n),
      iv_m3_yr_total = sum(.data$iv_m3_yr_total),
      iv_m3_ha_yr    = sum(.data$iv_m3_ha_yr),
      .groups        = "drop"
    ) |>
    dplyr::mutate(area_total_ha = area_total_2nd)

  # Plot-level increments per species_group for area-weighted SE.
  # Plots without trees of a given species_group must enter with iv = 0.
  plot_iv_sg <- work_dat |>
    dplyr::group_by(
      .data$fill_option, .data$species_group, .data$plot_id
    ) |>
    dplyr::summarise(
      iv_m3_ha_yr_plot = sum(.data$iv_hub_m3_ha_yr),
      .groups          = "drop"
    ) |>
    tidyr::complete(
      tidyr::nesting(fill_option, species_group),
      plot_id = all_plots$plot_id,
      fill    = list(iv_m3_ha_yr_plot = 0)
    ) |>
    dplyr::left_join(all_plots, by = "plot_id")

  se_by_fill_sg <- plot_iv_sg |>
    dplyr::group_by(fill_option, species_group) |>
    se_area_weighted_grouped("iv_m3_ha_yr_plot", "area_rep_ha_2nd") |>
    dplyr::transmute(
      fill_option         = .data$fill_option,
      species_group       = .data$species_group,
      ci95_iv_m3_ha_yr    = .data$margin_mean,
      ci95_iv_m3_yr_total = .data$margin_total
    )

  agg_by_total_sg <- agg_by_total_sg |>
    dplyr::left_join(
      se_by_fill_sg, by = c("fill_option", "species_group")
    ) |>
    dplyr::select(
      "fill_option", "species_group", "n", "iv_m3_yr_total",
      "ci95_iv_m3_yr_total", "area_total_ha", "iv_m3_ha_yr",
      "ci95_iv_m3_ha_yr"
    )

  v_total_sg <- work_dat |>
    dplyr::filter(.data$fill_option == "standard") |>
    dplyr::filter(.data$tree_exists_2nd & !.data$removal) |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(
      v_hub_m3 = sum(.data$v_hub_m3 * .data$n_rep_ha * .data$area_rep_ha),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      area_total_ha = area_total_2nd,
      v_hub_m3_ha   = .data$v_hub_m3 / .data$area_total_ha
    )

  # inc_summaric rows per species_group: same area-weighting principle as
  # for the other species-group aggregates — plots without trees of a
  # species_group enter with inc_summaric = 0 in the numerator but with
  # their full area in the denominator.
  inc_summaric_sg_full <- inc_summaric$by_species_group |>
    tidyr::complete(
      .data$species_group,
      plot_id = all_plots$plot_id,
      fill = list(inc_summaric_m3_ha_yr = 0)
    ) |>
    dplyr::left_join(all_plots, by = "plot_id")

  inc_summaric_total_sg <- inc_summaric_sg_full |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(
      iv_m3_yr_total = sum(
        .data$inc_summaric_m3_ha_yr * .data$area_rep_ha_2nd
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      area_total_ha = area_total_2nd,
      iv_m3_ha_yr   = .data$iv_m3_yr_total / .data$area_total_ha
    )

  se_inc_sum_sg <- inc_summaric_sg_full |>
    dplyr::group_by(species_group) |>
    se_area_weighted_grouped("inc_summaric_m3_ha_yr", "area_rep_ha_2nd") |>
    dplyr::transmute(
      species_group       = .data$species_group,
      ci95_iv_m3_ha_yr    = .data$margin_mean,
      ci95_iv_m3_yr_total = .data$margin_total
    )

  # n per species_group in the standard fill option (analogue of
  # nrow(work_dat_std) in the overall inc_summaric row).
  n_per_sg_std <- work_dat |>
    dplyr::filter(.data$fill_option == "standard") |>
    dplyr::count(species_group, name = "n_std")

  inc_summaric_rows_sg <- inc_summaric_total_sg |>
    dplyr::left_join(se_inc_sum_sg, by = "species_group") |>
    dplyr::left_join(n_per_sg_std, by = "species_group") |>
    dplyr::mutate(
      fill_option = "inc_summaric",
      n           = dplyr::coalesce(.data$n_std, 0L)
    ) |>
    dplyr::select(
      "fill_option", "species_group", "n", "iv_m3_yr_total",
      "ci95_iv_m3_yr_total", "area_total_ha", "iv_m3_ha_yr",
      "ci95_iv_m3_ha_yr"
    )

  agg_by_total_sg <- dplyr::bind_rows(agg_by_total_sg, inc_summaric_rows_sg)

  return(
    list(
      overall = list(
        iv_by_groups = agg_by_groups,
        iv_total     = agg_by_total,
        v_total      = v_total,
        n_plots      = n_plots
      ),
      by_species_group = list(
        iv_by_groups = agg_by_groups_sg,
        iv_total     = agg_by_total_sg,
        v_total      = v_total_sg
      ),
      method = tree_inc_rep_gapfill$method
    )
  )
}



#' Inventory-Wide Increment Overview for Plots Only Present in the Second
#' Inventory
#'
#' Counterpart to \code{\link{inv_inc_big_overview_matches_only}} for those plots that
#' only appear in the second survey (typical case: sample extensions and
#' newly established plots between two inventories). Their increment cannot
#' be observed directly; it is estimated per tree by applying the
#' third-German-NFI growth functions \emph{backwards} over a short period
#' (\code{dt}, default 5 years) and aggregating across plots with the same
#' area-weighted machinery as in \code{\link{inv_inc_big_overview_matches_only}}.
#'
#' Why a short period and not the full inter-survey distance: on plots
#' without a previous observation we do not know the mortality and removal
#' history. A backward projection over ~10 years would attribute much of
#' the standing volume to "still living" trees and miss the unknown
#' fraction that would actually have died or been harvested in that span.
#' Over a shorter window (~5 years) that bias stays small and the
#' resulting per-year growth rate is a defensible estimate for the
#' representative area of the second-only plots.
#'
#' The output structure mirrors \code{\link{inv_inc_big_overview_matches_only}} so that
#' the two can later be combined into one whole-enterprise table by the
#' caller. There is, however, no \code{fill_option} dimension here: only
#' one estimator is in play (backward gnfi3 over \code{dt} years). A single
#' label \code{"gnfi3_backward"} is used in the \code{fill_option} column
#' so that a row bind with the matched-plots output stays unambiguous.
#'
#' Current scope and limitations (v1):
#' - Only living trees of the second inventory enter the estimate (filter
#'   \code{!removal}). Removal trees on second-only plots are ignored;
#'   their contribution to the period increment would need a separate
#'   mortality model and is left out for now.
#' - The estimate is therefore a "living-tree backward" growth quantity,
#'   not a full balance increment. Be careful when combining the per-ha
#'   results with the matched-plot \code{inc_summaric} reference value,
#'   which does include mortality.
#' - **The \code{ci95_*} columns are present for structural
#'   compatibility with \code{\link{inv_inc_big_overview_matches_only}} but contain
#'   \code{NA} throughout.** All per-plot increments here come from a
#'   model (backward gnfi3 over a short window), not from empirical
#'   re-measurement; the only source of plot-to-plot scatter is then
#'   what the model itself produces on top of the actual tree sample.
#'   A confidence interval computed from that scatter would suggest a
#'   precision the underlying numbers do not have. Downstream code that
#'   merges this output with the matched-plots overview must therefore
#'   treat the 2nd-only CI columns as missing-on-purpose.
#'
#' @param inc_repinv_fl Output of \code{\link{inv_inc_fill_gaps_gnfi3}}.
#'   Used for the \code{inv_period} and \code{plot_matches} elements only;
#'   the per-tree increment data frames inside are not consumed here.
#'
#' @param inv_2nd_trees Tree data frame from the second inventory, as
#'   produced by \code{pull_trees() |> height_complete_inventory() |>
#'   trees_add_essentials()}.
#'
#' @param inv_2nd The corresponding \code{\link{fe_inventory}} object.
#'
#' @param dt Estimation period for the backward gnfi3 projection, in years,
#'   passed through to \code{\link{tree_inc_gnfi_2012}}. Default \code{-5}
#'   — see the function description for the rationale. The sign matters:
#'   negative values mean backward projection (the only direction that
#'   makes sense here), positive values would project the trees into the
#'   future and are flagged via a warning. Magnitudes other than 5 are
#'   accepted but should be motivated explicitly.
#'
#' @returns A list with the same top-level structure as
#'   \code{\link{inv_inc_big_overview_matches_only}}: \code{$overall} (containing
#'   \code{iv_by_groups}, \code{iv_total}, \code{v_total}, \code{n_plots})
#'   and \code{$by_species_group} (containing \code{iv_by_groups},
#'   \code{iv_total}, \code{v_total}). All m3/ha values relate to the
#'   total area across all second-only plots, even when grouped by
#'   species; plots without trees of a given species_group contribute
#'   their area but a zero numerator, so sums of per-species values
#'   match the corresponding overall values.
#'
#'   If there are no second-only plots, an empty-shaped list is returned
#'   with zero rows in the tables and \code{area_total_ha = 0} in
#'   \code{n_plots}; this lets downstream combination code merge the two
#'   overviews without special-casing.
#'
#' @export
#'
#' @examples
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   suppressWarnings(suppressMessages({
#'     # Matching and gap filling are shipped ready-made for this pair
#'     inc_rep <- data_ex3_increment_matched
#'     inc_fl  <- data_ex3_increment_fills
#'   }))
#'
#'   bo_2nd <- inv_inc_big_overview_2nd_only(inc_fl, inv_b_trees, inv_b)
#'   bo_2nd$overall$iv_total
#'   bo_2nd$by_species_group$iv_total
#'
#' @family increment
inv_inc_big_overview_2nd_only <- function(
    inc_repinv_fl, inv_2nd_trees, inv_2nd, dt = -5
) {

  if (dt > 0) {
    warning(
      "dt > 0 passed to inv_inc_big_overview_2nd_only(): the function ",
      "projects forward in time, but only backward estimates make sense ",
      "for second-only plots. Pass a negative dt to suppress this.",
      call. = FALSE
    )
  }

  unmatched_plot_ids <- inc_repinv_fl$plot_matches$in_b_only$plot_id

  # Plot areas, restricted to second-only plots
  area_info_2nd <- inv_2nd |>
    dplyr::select(-"plot") |>
    dplyr::filter(.data$plot_id %in% unmatched_plot_ids) |>
    dplyr::rename(area_rep_ha_2nd = "area_rep_ha")
  area_total_2nd <- sum(area_info_2nd$area_rep_ha_2nd)

  # Early return: no second-only plots
  if (length(unmatched_plot_ids) == 0 || nrow(area_info_2nd) == 0) {
    return(empty_big_overview_2nd_only(area_total_2nd))
  }

  # Per-tree backward gnfi3: tree_inc_gnfi_2012() with negative dt
  # projects |dt| years into the past. Scale per-tree iv to per-ha-per-yr
  # via n_rep_ha. Negative differences (improbable but possible near
  # age 0) are clipped to zero.
  trees_with_iv <- inv_2nd_trees |>
    dplyr::filter(
      .data$plot_id %in% unmatched_plot_ids,
      !.data$removal
    ) |>
    tree_inc_gnfi_2012(dt = dt) |>
    dplyr::mutate(
      iv_hub_m3_ha_yr = .data$n_rep_ha * pmax(0, .data$iv_hub_m3_yr_gnfi3)
    )

  # ---------------------------------------------------------------------
  # Aggregation
  # ---------------------------------------------------------------------
  # Shared with inv_increment_gnfi3(): the aggregation of per-tree gnfi3
  # increments to inventory level is the same operation here and there, the
  # only difference being the plot set it runs on (second-only plots here, the
  # whole inventory there) and the label it writes into fill_option. The
  # primitive expects the area column to be named area_rep_ha.
  .gnfi3_overview_core(
    trees_with_iv,
    area_info  = area_info_2nd |>
      dplyr::rename(area_rep_ha = "area_rep_ha_2nd") |>
      dplyr::select("plot_id", "area_rep_ha"),
    fill_label = "gnfi3_backward"
  )
}



# Internal helper: zero-row, schema-correct shell for the case when there are
# no second-only plots. Lets downstream code treat the empty case uniformly.
empty_big_overview_2nd_only <- function(area_total_ha = 0) {

  iv_total_tmpl <- tibble::tibble(
    fill_option         = character(),
    n                   = integer(),
    iv_m3_yr_total      = numeric(),
    ci95_iv_m3_yr_total = numeric(),
    area_total_ha       = numeric(),
    iv_m3_ha_yr         = numeric(),
    ci95_iv_m3_ha_yr    = numeric()
  )

  iv_by_groups_tmpl <- tibble::tibble(
    fill_option    = character(),
    id_able_tree   = logical(),
    tree_status    = character(),
    n              = integer(),
    iv_m3_yr_total = numeric(),
    area_total_ha  = numeric(),
    iv_m3_ha_yr    = numeric()
  )

  v_total_tmpl <- tibble::tibble(
    v_hub_m3      = numeric(),
    area_total_ha = numeric(),
    v_hub_m3_ha   = numeric()
  )

  list(
    overall = list(
      iv_by_groups = iv_by_groups_tmpl,
      iv_total     = iv_total_tmpl,
      v_total      = v_total_tmpl,
      n_plots      = tibble::tibble(
        n_plots         = 0L,
        area_total_ha   = area_total_ha,
        avg_rep_ha_plot = NA_real_
      )
    ),
    by_species_group = list(
      iv_by_groups = iv_by_groups_tmpl |>
        tibble::add_column(species_group = character(), .after = "fill_option"),
      iv_total     = iv_total_tmpl |>
        tibble::add_column(species_group = character(), .after = "fill_option"),
      v_total      = v_total_tmpl |>
        tibble::add_column(species_group = character(), .before = "v_hub_m3")
    )
  )
}



#' Inventory-Wide Increment Overview for a Repeated Survey, Combining
#' Matched and Second-Only Plots
#'
#' Top-level wrapper that runs both
#' \code{\link{inv_inc_big_overview_matches_only}} (for the plots that
#' appear in both surveys) and \code{\link{inv_inc_big_overview_2nd_only}}
#' (for the plots that only appear in the second survey, with a
#' short-window backward gnfi3 estimate) and combines their results into
#' one whole-enterprise view. Designed as the data source for
#' user-facing summary tables of the whole forest enterprise.
#'
#' Output layout (the central design decision): three sub-overview
#' slots that all keep the same internal shape (\code{$overall} +
#' \code{$by_species_group}), plus the \code{inv_period} and
#' \code{method} info passed through so downstream renderers can label
#' survey years and the increment method without re-deriving them.
#' \describe{
#'   \item{matches_only}{Verbatim output of
#'     \code{\link{inv_inc_big_overview_matches_only}}.}
#'   \item{second_only}{Verbatim output of
#'     \code{\link{inv_inc_big_overview_2nd_only}}.}
#'   \item{combined}{Per-row addition of matches_only and second_only.
#'     One row per matched \code{fill_option} (\code{combined_standard},
#'     \code{combined_min_estimates}, \code{combined_all_estimates},
#'     \code{combined_inc_summaric}), each summing the matched
#'     fill_option result with the single \code{gnfi3_backward}
#'     second_only contribution. \code{iv_m3_yr_total} and
#'     \code{area_total_ha} are additive; \code{iv_m3_ha_yr} is the
#'     resulting ratio. Sum-consistency: each combined row's totals
#'     equal the corresponding matches_only row plus the second_only
#'     row.}
#'   \item{inv_period}{The same \code{inv_period} list element that is
#'     already carried by \code{\link{inv_increment_repsurv_ccirc}} and
#'     \code{\link{inv_inc_fill_gaps_gnfi3}}, with the two survey years
#'     and the period length.}
#'   \item{method}{The increment method used in the upstream
#'     \code{\link{inv_increment_repsurv_ccirc}} call (one of
#'     \code{"rep_classic"}, \code{"rep_mean"}, \code{"rep_end"},
#'     \code{"rep_trans"}); carried through so user-facing renderers
#'     can label the table accordingly.}
#' }
#'
#' **CI95 in \code{$combined}:** the \code{ci95_*} columns are kept for
#' structural compatibility but are \code{NA} throughout. The
#' second_only contribution carries no empirical CI by contract (see
#' \code{\link{inv_inc_big_overview_2nd_only}}); an area-weighted
#' pooling of variances with one variance unknown would require
#' assumptions that we are not willing to make here. Users who need a
#' CI on the combined value can fall back to the matches_only CI as a
#' lower bound, and document the choice.
#'
#' **\code{iv_by_groups} in \code{$combined}** is deliberately omitted.
#' The matched side breaks down by \code{id_able_tree} × \code{tree_status}
#' (categories that the second_only side does not share), so a mixed
#' breakdown would have a heterogeneous schema. Users that need that
#' detail should look at \code{$matches_only$overall$iv_by_groups}
#' separately.
#'
#' @param tree_inc_rep_gapfill Output of
#'   \code{\link{inv_inc_fill_gaps_gnfi3}}.
#' @param inv_1st_trees,inv_2nd_trees Tree data frames as in
#'   \code{\link{inv_inc_big_overview_matches_only}}.
#' @param inv_2nd \code{\link{fe_inventory}} object of the second survey.
#' @param style Passed through to
#'   \code{\link{inv_inc_big_overview_matches_only}}.
#' @param inc_summaric Output of \code{\link{inv_inc_summaric}}, passed
#'   through to \code{\link{inv_inc_big_overview_matches_only}}.
#' @param dt_2nd_only Estimation period in years for the second-only
#'   backward gnfi3 projection (negative for backward). Default
#'   \code{-5}. Passed through to
#'   \code{\link{inv_inc_big_overview_2nd_only}}.
#'
#' @returns A list with the three top-level slots described above.
#'
#' @export
#'
#' @examples
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   suppressWarnings(suppressMessages({
#'     inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'     inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'     # Matching and gap filling are shipped ready-made for this pair
#'     inc_rep <- data_ex3_increment_matched
#'     inc_fl  <- data_ex3_increment_fills
#'     inc_sum <- inv_inc_summaric(
#'       inc_rep$tree_increments, inc_fl$fills_forward,
#'       inv_a_trees, inv_b_trees
#'     )
#'   }))
#'
#'   bo <- inv_inc_big_overview(
#'     inc_fl, inv_a_trees, inv_b_trees, inv_b,
#'     style = "generic", inc_summaric = inc_sum
#'   )
#'   bo$combined$overall$iv_total
#'
#' @family increment
inv_inc_big_overview <- function(
    tree_inc_rep_gapfill, inv_1st_trees, inv_2nd_trees, inv_2nd,
    style = c("generic", "baysf"),
    inc_summaric,
    dt_2nd_only = -5
) {
  style <- match.arg(style)

  bo_m <- inv_inc_big_overview_matches_only(
    tree_inc_rep_gapfill, inv_1st_trees, inv_2nd_trees, inv_2nd,
    style = style, inc_summaric = inc_summaric
  )
  bo_2 <- inv_inc_big_overview_2nd_only(
    tree_inc_rep_gapfill, inv_2nd_trees, inv_2nd, dt = dt_2nd_only
  )

  # When 2nd-only is empty, the shell from empty_big_overview_2nd_only()
  # leaves the species_group columns as bare character(). Cast them to
  # the matched side's fe_species_* prototype so every downstream
  # consumer of bo_2 -- combine_matches_and_2nd_only_overviews() here
  # and output_increment_overall()'s build_table_* joins later -- sees a
  # consistent type and does not fail on dplyr's "incompatible types"
  # check.
  bo_2 <- recast_2nd_only_species_group(bo_2, bo_m)

  combined <- combine_matches_and_2nd_only_overviews(bo_m, bo_2)

  list(
    matches_only = bo_m,
    second_only  = bo_2,
    combined     = combined,
    inv_period   = tree_inc_rep_gapfill$inv_period,
    method       = tree_inc_rep_gapfill$method
  )
}


# Internal: when bo_2 is the zero-row shell from
# empty_big_overview_2nd_only(), its species_group columns are bare
# character(). Cast them to the matched side's prototype so dplyr
# joins across the two sides succeed. Subsetting with [0] gives a
# zero-length vector of the right class without a vctrs dependency.
recast_2nd_only_species_group <- function(bo_2, bo_m) {
  proto_iv <- bo_m$by_species_group$iv_total$species_group[0]
  proto_by <- bo_m$by_species_group$iv_by_groups$species_group[0]
  proto_v  <- bo_m$by_species_group$v_total$species_group[0]

  if (nrow(bo_2$by_species_group$iv_total) == 0L) {
    bo_2$by_species_group$iv_total$species_group <- proto_iv
  }
  if (nrow(bo_2$by_species_group$iv_by_groups) == 0L) {
    bo_2$by_species_group$iv_by_groups$species_group <- proto_by
  }
  if (nrow(bo_2$by_species_group$v_total) == 0L) {
    bo_2$by_species_group$v_total$species_group <- proto_v
  }
  bo_2
}


# Internal helper: build the $combined slot from the two sub-overviews.
# Areas, totals, and counts are additive per row; iv_m3_ha_yr is the
# resulting ratio; ci95_* are NA throughout because second_only carries
# no empirical CI. The second_only side contributes exactly one row
# (fill_option = "gnfi3_backward") that is added into each matched
# fill_option row to produce the combined_<fill_option> rows.
combine_matches_and_2nd_only_overviews <- function(bo_m, bo_2) {

  # The single second_only row (one entry per overall, one per species
  # group). Empty when there are no second-only plots — handled below.
  so_iv_total_ovr <- bo_2$overall$iv_total
  so_iv_total_sg  <- bo_2$by_species_group$iv_total
  so_v_total_ovr  <- bo_2$overall$v_total
  so_v_total_sg   <- bo_2$by_species_group$v_total
  so_n_plots      <- bo_2$overall$n_plots

  # When bo_2 comes from the zero-row shell `empty_big_overview_2nd_only()`,
  # its species_group columns are bare `character()`. The matched side
  # carries a typed `fe_species_*` vector, so the dplyr::full_join() and
  # dplyr::anti_join() below would refuse to merge across types. Recast
  # the empty species_group columns to the matched side's prototype so
  # the joins go through. Subsetting with [0] yields a zero-length vector
  # of the right class without needing a vctrs dependency.
  if (nrow(so_iv_total_sg) == 0L) {
    so_iv_total_sg$species_group <-
      bo_m$by_species_group$iv_total$species_group[0]
  }
  if (nrow(so_v_total_sg) == 0L) {
    so_v_total_sg$species_group <-
      bo_m$by_species_group$v_total$species_group[0]
  }

  # Scalars from second_only (default to 0 when 2nd_only is empty)
  so_n_ovr      <- if (nrow(so_iv_total_ovr)) so_iv_total_ovr$n[1L]            else 0L
  so_iv_yr_ovr  <- if (nrow(so_iv_total_ovr)) so_iv_total_ovr$iv_m3_yr_total[1L] else 0
  so_area_ovr   <- if (nrow(so_iv_total_ovr)) so_iv_total_ovr$area_total_ha[1L]  else 0
  so_v_hub_ovr  <- if (nrow(so_v_total_ovr))  so_v_total_ovr$v_hub_m3[1L]        else 0
  so_n_plots_v  <- if (nrow(so_n_plots))      so_n_plots$n_plots[1L]             else 0L

  # ---- $combined$overall$iv_total --------------------------------------
  combined_iv_total_ovr <- bo_m$overall$iv_total |>
    dplyr::mutate(
      fill_option         = paste0("combined_", .data$fill_option),
      n                   = .data$n + so_n_ovr,
      iv_m3_yr_total      = .data$iv_m3_yr_total + so_iv_yr_ovr,
      area_total_ha       = .data$area_total_ha + so_area_ovr,
      iv_m3_ha_yr         = .data$iv_m3_yr_total / .data$area_total_ha,
      ci95_iv_m3_yr_total = NA_real_,
      ci95_iv_m3_ha_yr    = NA_real_
    )

  # ---- $combined$overall$v_total ---------------------------------------
  combined_v_total_ovr <- tibble::tibble(
    v_hub_m3      = bo_m$overall$v_total$v_hub_m3 + so_v_hub_ovr,
    area_total_ha = bo_m$overall$v_total$area_total_ha + so_area_ovr
  ) |>
    dplyr::mutate(v_hub_m3_ha = .data$v_hub_m3 / .data$area_total_ha)

  # ---- $combined$overall$n_plots ---------------------------------------
  combined_n_plots <- tibble::tibble(
    n_plots       = bo_m$overall$n_plots$n_plots + so_n_plots_v,
    area_total_ha = bo_m$overall$n_plots$area_total_ha + so_area_ovr
  ) |>
    dplyr::mutate(
      avg_rep_ha_plot = ifelse(
        .data$n_plots > 0L,
        .data$area_total_ha / .data$n_plots,
        NA_real_
      )
    )

  # ---- $combined$by_species_group$iv_total ----------------------------
  # Full join on species_group so that species present only in one side
  # still appear; the missing side contributes 0.
  sg_join_iv <- dplyr::full_join(
    bo_m$by_species_group$iv_total |>
      dplyr::select("fill_option", "species_group",
                    n_m              = "n",
                    iv_yr_total_m    = "iv_m3_yr_total",
                    area_total_ha_m  = "area_total_ha"),
    so_iv_total_sg |>
      dplyr::select("species_group",
                    n_so             = "n",
                    iv_yr_total_so   = "iv_m3_yr_total",
                    area_total_ha_so = "area_total_ha"),
    by = "species_group"
  )

  combined_iv_total_sg <- sg_join_iv |>
    dplyr::mutate(
      # If species_group only exists on second_only side, fill_option is
      # NA from the matched side — drop those rows because we want one
      # combined_<X> row per matched fill_option × species_group, and
      # those species are already represented via the broader
      # cross-product below if they appear on the matched side at all.
      # However: a species_group that exists ONLY on 2nd_only does need
      # representation. We expand it across all matched fill_options.
      .keep = "all"
    )

  # Handle species_groups that only exist on the second_only side: replicate
  # one row per matched fill_option, with the matched contribution = 0.
  # Use dplyr::anti_join() rather than base::setdiff() because species_group
  # is an S3 fe_species_* vector that base set ops would coerce to character.
  matched_fill_opts <- unique(bo_m$overall$iv_total$fill_option)
  only_so_rows_raw <- so_iv_total_sg |>
    dplyr::anti_join(
      bo_m$by_species_group$iv_total |>
        dplyr::distinct(.data$species_group),
      by = "species_group"
    )
  only_so_rows <- if (nrow(only_so_rows_raw) > 0L) {
    # tidyr::crossing handles the vctrs species class for the join.
    tidyr::crossing(
      fill_option   = matched_fill_opts,
      only_so_rows_raw |> dplyr::select("species_group", "n",
                                        "iv_m3_yr_total", "area_total_ha")
    ) |>
      dplyr::transmute(
        fill_option      = .data$fill_option,
        species_group    = .data$species_group,
        n_m              = 0L,
        iv_yr_total_m    = 0,
        area_total_ha_m  = 0,
        n_so             = .data$n,
        iv_yr_total_so   = .data$iv_m3_yr_total,
        area_total_ha_so = .data$area_total_ha
      )
  } else {
    NULL
  }

  sg_combined <- dplyr::bind_rows(
    combined_iv_total_sg |>
      dplyr::filter(!is.na(.data$fill_option)),
    only_so_rows
  ) |>
    dplyr::mutate(
      dplyr::across(c("n_so", "iv_yr_total_so", "area_total_ha_so"),
                    \(x) dplyr::coalesce(x, 0)),
      fill_option         = paste0("combined_", .data$fill_option),
      n                   = .data$n_m + .data$n_so,
      iv_m3_yr_total      = .data$iv_yr_total_m + .data$iv_yr_total_so,
      area_total_ha       = .data$area_total_ha_m + .data$area_total_ha_so,
      iv_m3_ha_yr         = .data$iv_m3_yr_total / .data$area_total_ha,
      ci95_iv_m3_yr_total = NA_real_,
      ci95_iv_m3_ha_yr    = NA_real_
    ) |>
    dplyr::select(
      "fill_option", "species_group", "n", "iv_m3_yr_total",
      "ci95_iv_m3_yr_total", "area_total_ha", "iv_m3_ha_yr",
      "ci95_iv_m3_ha_yr"
    )

  # ---- $combined$by_species_group$v_total -----------------------------
  combined_v_total_sg <- dplyr::full_join(
    bo_m$by_species_group$v_total |>
      dplyr::select("species_group",
                    v_hub_m3_m      = "v_hub_m3",
                    area_total_ha_m = "area_total_ha"),
    so_v_total_sg |>
      dplyr::select("species_group",
                    v_hub_m3_so      = "v_hub_m3",
                    area_total_ha_so = "area_total_ha"),
    by = "species_group"
  ) |>
    dplyr::mutate(
      dplyr::across(
        c("v_hub_m3_m", "v_hub_m3_so",
          "area_total_ha_m", "area_total_ha_so"),
        \(x) dplyr::coalesce(x, 0)
      ),
      v_hub_m3      = .data$v_hub_m3_m + .data$v_hub_m3_so,
      area_total_ha = .data$area_total_ha_m + .data$area_total_ha_so,
      v_hub_m3_ha   = .data$v_hub_m3 / .data$area_total_ha
    ) |>
    dplyr::select("species_group", "v_hub_m3", "area_total_ha",
                  "v_hub_m3_ha")

  list(
    overall = list(
      iv_total = combined_iv_total_ovr,
      v_total  = combined_v_total_ovr,
      n_plots  = combined_n_plots
    ),
    by_species_group = list(
      iv_total = sg_combined,
      v_total  = combined_v_total_sg
    )
  )
}




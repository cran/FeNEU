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







#' Close Tree Increment Gaps with Estimates from National Forest Inventory
#' Functions
#'
#' When calculating tree level increments from two subsequent inventories, there
#' are typically trees that were present in one of the inventories only. For
#' different types of such trees, we apply here the single tree growth
#' functions from the third German National Forest Inventory
#' \insertCite{bwi3_methods_2017}{FeNEU} as implemented
#' in the Package \code{ForestElementsR}, namely the functions
#' \code{\link[ForestElementsR]{d_age_gnfi3}} and
#' \code{\link[ForestElementsR]{h_age_gnfi3}}.
#'
#' The increment estimates performed here are only applied to inventory plots
#' that match in both inventories. This is guaranteed by requiring
#' \code{tree_inc_rep} as input object. This is the output of
#' \code{\link{inv_increment_repsurv_ccirc}} which ensures plot matching.
#'
#' @param tree_inc_rep Output of \code{\link{inv_increment_repsurv_ccirc}}
#'
#' @param inv_1st_trees Extended tree data frame for the first inventory,
#'   exactly as it was used as an input to
#'   \code{\link{inv_increment_repsurv_ccirc}} in order to generate
#'   \code{tree_inc_rep}.
#'
#' @param inv_2nd_trees Extended tree data frame for the second inventory,
#'   exactly as it was used as an input to
#'   \code{\link{inv_increment_repsurv_ccirc}} in order to generate
#'   \code{tree_inc_rep}.
#'
#' @param include_reptrees If \code{TRUE} (default), backward increment
#'   estimates are also made for trees that are present in both inventories and
#'   therefore have "real" increments. If \code{FALSE} the columns for
#'   backward estimates will be set to \code{NA}. This might save computation
#'   time when dealing with extensive amounts of data.
#'
#' @returns A list of eight elements: five data frames with the gap-filling
#'   estimates (\code{fills_forward}, \code{fills_backward},
#'   \code{fills_backward_nomatch}, \code{fills_backward_implausible},
#'   \code{fills_backward_reptrees}), followed by \code{inv_period},
#'   \code{plot_matches} and \code{method}, which are carried through from
#'   \code{tree_inc_rep}.
#'
#'   The five data frames have exactly the same structure, i.e. the same
#'   columns as \code{tree_inc_rep$tree_increments}, however with two
#'   additional columns, \code{iv_hub_m3_ha_per_gnfi3} and
#'   \code{iv_hub_m3_ha_yr_gnfi3}. The data frame \code{fills_forward} relates to
#'   trees that were present in the first, but missing in the second inventory.
#'   Their increment is projected forward up to the mid of the period between
#'   both inventories. The second data frame \code{fills_backward} comprises
#'   trees that were present in the second, but not present in the first
#'   inventory. Their increment is estimated backwards for the whole period. The
#'   third data frame \code{fills_backward_nomatch} comprises trees present
#'   in the second inventory, which were, however, not recorded in a way that
#'   makes them identifyable in the first inventory (typically, such trees are
#'   under a certain dbh threshold). For such trees backward increment estimates
#'   are made for the whole period. The fourth data frame
#'   \code{fills_backward_implausible} comprises trees that were present in both
#'   inventories but flagged as implausible during tree matching (due to
#'   excessive volume shrinkage or species mismatch). These trees received a
#'   zero increment from the repeated measurement; their increment is estimated
#'   backwards from the second inventory's values for the whole period. The
#'   fifth data frame \code{fills_backward_reptrees} relates to plausible trees
#'   that were present in both inventories. While directly calculated increments
#'   are available for these trees, backward estimates are calculated if
#'   \code{include_reptrees} is \code{TRUE} (the default); they are what
#'   \code{fill_option = "all_estimates"} in
#'   \code{\link{inv_inc_tree_consolidate}} draws on. With
#'   \code{include_reptrees = FALSE}, the columns reserved for the increment
#'   estimates are set to NA. The three remaining elements are
#'   \code{inv_period} (carried through from
#'   \code{\link{inv_increment_repsurv_ccirc}}), \code{plot_matches}
#'   (the plot match overview, also from \code{inv_increment_repsurv_ccirc};
#'   downstream consumers such as
#'   \code{\link{inv_inc_big_overview_2nd_only}} use the
#'   \code{in_b_only} entry there to identify plots that only appear in the
#'   second inventory), and \code{method} (the increment method used in
#'   the upstream \code{\link{inv_increment_repsurv_ccirc}} call --
#'   one of \code{"rep_classic"}, \code{"rep_mean"}, \code{"rep_end"},
#'   \code{"rep_trans"}).
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
#'   # The matching this builds on is shipped ready-made for the pair, see
#'   # ?data_ex3_increment_interim
#'   inc_repinv <- data_ex3_increment_matched
#'
#'   # Finally, fill the gaps. include_reptrees = FALSE estimates only where
#'   # there is a gap; the default TRUE also estimates for trees measured
#'   # twice, which is what fill_option = "all_estimates" later feeds on.
#'   inv_inc_fill_gaps_gnfi3(
#'     inc_repinv, inv_a_trees, inv_b_trees, include_reptrees = TRUE
#'   )
#'
#' @family increment
inv_inc_fill_gaps_gnfi3 <- function(
    tree_inc_rep, inv_1st_trees, inv_2nd_trees, include_reptrees = TRUE
) {

  # Reduce tree data frames to essential columns only.
  # This avoids naming conflicts in downstream left_joins when
  # the tree data has extra columns (e.g. pre-saved data with
  # v_hub_m3_at_thrsh or other analysis-specific additions).
  inv_1st_trees <- inv_1st_trees |>
    dplyr::select(
      "plot_id", "tree_id", "species_id", "dbh_cm",
      "height_m", "age_yr", "v_hub_m3"
    )
  inv_2nd_trees <- inv_2nd_trees |>
    dplyr::select(
      "plot_id", "tree_id", "species_id", "dbh_cm",
      "height_m", "age_yr", "v_hub_m3", "n_rep_ha", "time_yr"
    )

  #  1) Only trees that are contained in tree_inc_rep
  ## 1.1) Trees that exist in the 1st inventory only get a forward increment
  ##      estimate for half of the time span between the two inventories. I.e.
  ##      they are assumed to have been harvested or to have died in the middle
  ##      of the time span
  fills_forward <- inv_inc_fill_gaps_forward(tree_inc_rep, inv_1st_trees)

  ## 1.2) Trees that exist in 2nd inventory only get a backward increment
  ##      estimate for the whole time span between the two inventories.
  ##      The idea:
  ##      when crossing the diameter threshold of their circle between both
  ##      inventories, they became eligible for survey, but they were actually
  ##      growing for the whole inventory period
  fills_backward <- inv_inc_fill_gaps_backward(tree_inc_rep, inv_2nd_trees)

  # 2) Trees that are not contained in tree_inc_rep
  ##   These are trees (typically below a certain dbh threshold) that cannot
  ##   be matched across two subsequent inventories. Therefore, such trees are
  ##   taken only from the second survey and get a backward estimate for the
  ##   whole time span
  fills_backward_nomatch <- inv_inc_fill_gaps_backward_nomatch(
    tree_inc_rep, inv_2nd_trees
  )

  # 3) Trees flagged as implausible (excessive volume shrinkage or species
  #    mismatch) that are present in both inventories. They received zero
  #    increment from the repeated measurement and get backward estimates
  #    from the second inventory's values.
  fills_backward_implausible <- inv_inc_fill_backward_implausible(
    tree_inc_rep, inv_2nd_trees
  )

  # 4) Trees that exist in both inventories obtain backward estimates only if
  #    explicitly desired by the user (include_reptrees = TRUE). Otherwise,
  #    the resulting data frame has the same format as the others, but NA
  #    entries in the columns for the backward estimates.
  #    Implausible trees are excluded here to avoid double counting.
  fills_backward_reptrees <- inv_inc_fill_backward_reptrees(
    tree_inc_rep, inv_2nd_trees, include_reptrees
  )

  # Checksum - the row counts of all backward fills must add up to the row count
  # of inv_2nd_trees (filtered for matching plots)
  # Checksum mechanism deactivated. Needs revision after increment overhaul
  # of 24 Nov 2025
  if (FALSE) {
    checksum_1 <- nrow(fills_backward) + nrow(fills_backward_nomatch) +
      nrow(fills_backward_reptrees)

    checksum_2 <- inv_2nd_trees |>
      dplyr::filter(    # Take only matching plots in both inventories
        .data$plot_id %in% tree_inc_rep$plot_matches$matches$plot_id_inv_b
      ) |>
      nrow()

    if (checksum_1 != checksum_2)
      warning(fe_msg("checksum_mismatch", checksum_1, checksum_2))
  }

  # Assemble result object and return; carry through the inv_period info
  # and plot_matches so downstream consumers (in particular
  # inv_inc_big_overview_matches_only(), inv_inc_big_overview_2nd_only(),
  # and the combined inv_inc_big_overview() on top of them) don't need to
  # re-derive them from the fe_inventory objects.
  list(
    fills_forward              = fills_forward,
    fills_backward             = fills_backward,
    fills_backward_nomatch     = fills_backward_nomatch,
    fills_backward_implausible = fills_backward_implausible,
    fills_backward_reptrees    = fills_backward_reptrees,
    inv_period                 = tree_inc_rep$inv_period,
    plot_matches               = tree_inc_rep$plot_matches,
    method                     = tree_inc_rep$method
  )
}



# The internal helpers in this file carry no @family tag on purpose. A family
# tag would list them in the "See Also" of every exported member of that
# family, offering the reader a link to something they cannot call.
#' Forward-Estimate Increments for Trees that were Present in the First of Two
#' Subsequent Inventories
#'
#' Called from within \code{\link{inv_inc_fill_gaps_gnfi3}}.
#' Takes into account only trees that have coordinates, but were only present
#' in the first inventory, or present in the second inventory but with a
#' removal flag. A forward estimate of the volume increment is done
#' for half the inventory period, as such trees are assumed to have died or
#' have been harvested at the middle of the period. The mean annual increment
#' relates to the full inventory period as it has to represent the trees'
#' contribution to the mean annual increment on the stand level. The single tree
#' increments are upscaled to an area of 1 ha.
#'
#' @param tree_inc_rep See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @param inv_1st_trees See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @returns The data frame \code{tree_inc_rep$tree_increments} filtered for
#'   trees that are only existing in the first inventory. It contains the two
#'   additional columns \code{iv_hub_m3_ha_per_gnfi3}, and
#'   \code{iv_hub_m3_ha_yr_gnfi3} which represent the periodic and the mean
#'   annual forward estimates of their volume increment.
#'
#' @keywords internal
#'
inv_inc_fill_gaps_forward <- function(tree_inc_rep, inv_1st_trees) {

  # In order to keep the ouput compact, the output will reduced to these
  # column names plus the two newly calculated ones
  col_names_to_keep <- names(tree_inc_rep$tree_increments)

  # Join with the tree information and calculate. The filter uses
  # `%in% TRUE` / `%in% FALSE` patterns so that NA values in
  # tree_exists_* (= unmatchable rows whose counterpart side is unknown
  # by design) are treated as not-matching and naturally excluded from
  # forward-fill — no explicit `!is_unmatchable_1st` guard needed.
  rslt_dat <- tree_inc_rep$tree_increments |>
    dplyr::filter(
      (.data$tree_exists_1st %in% TRUE & .data$tree_exists_2nd %in% FALSE) |
      (.data$tree_exists_1st %in% TRUE & .data$tree_exists_2nd %in% TRUE &
         .data$tree_removal_2nd %in% TRUE)
    ) |>
    dplyr::left_join(
      inv_1st_trees,
      by = c("plot_id_1st" = "plot_id", "tree_id_1st" = "tree_id")
    ) |>
    dplyr::mutate(
      # Warnings are suppressed in some places below, because they come from
      # intended species casts in the ForestElementsR functions
      dbh_cm_middle = suppressWarnings(ForestElementsR::d_age_gnfi3(
        .data$species_id,
        .data$age_yr + (.data$time_yr_2nd - .data$time_yr_1st) / 2,
        .data$dbh_cm, .data$age_yr
      )),
      height_m_middle = suppressWarnings(ForestElementsR::h_age_gnfi3(
        .data$species_id,
        .data$age_yr + (.data$time_yr_2nd - .data$time_yr_1st) / 2,
        .data$height_m, .data$age_yr
      )),
      v_m3_middle = suppressWarnings(ForestElementsR::v_gri(
        .data$species_id, .data$dbh_cm_middle, .data$height_m_middle
      )),
      v_hub_m3_middle = suppressWarnings(ForestElementsR::v_red_harvest_ubark(
        .data$species_id, .data$v_m3_middle
      )),
      # Increment over half the inventory period with safety catch for
      # (very improbable) values < 0
      iv_hub_m3_ha_per_gnfi3 =
        .data$n_rep_ha_1st * pmax(0, .data$v_hub_m3_middle - .data$v_hub_m3),

      # Annual increment over the whole inventory period (must be divided bv
      # the whole time span even though the tree "grew" only half the time)
      iv_hub_m3_ha_yr_gnfi3 = .data$iv_hub_m3_ha_per_gnfi3 /
        (.data$time_yr_2nd - .data$time_yr_1st)
    )

  # Keep the required columns and return
  rslt_dat |> dplyr::select(
    dplyr::all_of(c(col_names_to_keep, "iv_hub_m3_ha_per_gnfi3", "iv_hub_m3_ha_yr_gnfi3"))
  )
}





#' Backward-Estimate Increments for Trees that were Present in the Second of Two
#' Subsequent Inventories
#'
#' Called from within \code{\link{inv_inc_fill_gaps_gnfi3}}.
#' Takes into account only trees that have coordinates, but were only present
#' in the second inventory. A backward estimate of the
#' volume increment is done for the whole inventory period. The idea behind this
#' approach: When crossing the diameter threshold of their circle between both
#' inventories, these trees became eligible for survey, but they were actually
#' growing for the whole inventory period. The single tree increments are
#' upscaled to an area of 1 ha.
#'
#' @param tree_inc_rep See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @param inv_2nd_trees See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @returns The data frame \code{tree_inc_rep$tree_increments} filtered for
#'   trees that are only existing in the second inventory. It contains the two
#'   additional columns \code{iv_hub_m3_ha_per_gnfi3}, and
#'   \code{iv_hub_m3_ha_yr_gnfi3} which represent the periodic and the mean
#'   annualbackward estimates of their volume increment.
#'
#' @keywords internal
#'
inv_inc_fill_gaps_backward <- function(tree_inc_rep, inv_2nd_trees) {

  # if (.progress) .progress <- "fill back regular"

  # In order to keep the ouput compact, the output will reduced to these
  # column names plus the two newly calculated ones
  col_names_to_keep <- names(tree_inc_rep$tree_increments)

  # Join with the tree information and calculate. We include both
  # verified ingrowth (`tree_exists_1st %in% FALSE`) and unmatchable
  # 2nd-only rows (`is.na(tree_exists_1st)`, e.g. inner-circle inv 2
  # trees without polar coordinates plus pseudo-ingrowth candidates
  # before reclassification): all of them get a gnfi3 backward estimate
  # over the full inter-survey period as the "assume the tree existed
  # at inv 1 with smaller, model-implied size" estimator. For
  # unmatchable inv 2 trees that's exactly the treatment required by
  # the methodological spec ("Unmatchable Bäume der zweiten Inventur
  # werden mit gnfi3-Methoden über die gesamte Periode
  # zurückgerechnet"). Rows where `tree_exists_1st %in% TRUE` (i.e.
  # matched both-exist trees, including reclassified pseudo-ingrowth)
  # take their per-tree increment from the rep-side formula and are
  # explicitly excluded here.
  rslt_dat <- tree_inc_rep$tree_increments |>
    dplyr::filter(
      !(.data$tree_exists_1st %in% TRUE) & .data$tree_exists_2nd %in% TRUE
    ) |>
    dplyr::left_join(
      inv_2nd_trees,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    ) |>
    dplyr::mutate(
      # Warnings are suppressed in some places below, because they come from
      # intended species casts in the ForestElementsR functions
      dbh_cm_start = suppressWarnings(ForestElementsR::d_age_gnfi3(
        .data$species_id,
        pmax(0, .data$age_yr + .data$time_yr_1st - .data$time_yr_2nd),
        .data$dbh_cm, .data$age_yr
      )),
      height_m_start = suppressWarnings(ForestElementsR::h_age_gnfi3(
        .data$species_id,
        pmax(0, .data$age_yr + .data$time_yr_1st - .data$time_yr_2nd),
        .data$height_m, .data$age_yr
      )),
      v_m3_start = suppressWarnings(ForestElementsR::v_gri(
        .data$species_id, .data$dbh_cm_start, .data$height_m_start
      )),
      v_hub_m3_start = suppressWarnings(ForestElementsR::v_red_harvest_ubark(
        .data$species_id, .data$v_m3_start
      )),
      # Increment over the inventory period with safety catch for
      # (improbable) values < 0
      iv_hub_m3_ha_per_gnfi3 =
        .data$n_rep_ha_2nd * pmax(0, .data$v_hub_m3 - .data$v_hub_m3_start),

      # Annual increment over the whole inventory period
      iv_hub_m3_ha_yr_gnfi3 = .data$iv_hub_m3_ha_per_gnfi3 /
        (.data$time_yr_2nd - .data$time_yr_1st)
    )

  # Keep the required columns and return
  rslt_dat |> dplyr::select(
    dplyr::all_of(c(col_names_to_keep, "iv_hub_m3_ha_per_gnfi3", "iv_hub_m3_ha_yr_gnfi3"))
  )
}



#' Backward-Estimate Increments for Trees That Cannot Be Matched Across
#' Inventories
#'
#' Called from within \code{\link{inv_inc_fill_gaps_gnfi3}}. Takes into
#' account only trees from the second inventory that cannot be matched across
#' inventories due to the implemented survey system. Typically, such trees
#' are below a certain dbh threshold and therefore not surveyed in a way that
#' would make them uniquely identifyable. These trees obtain a backward estimate
#' of their volume increment. The single tree increments are upscaled to an area
#' of 1 ha.
#'
#' @param tree_inc_rep See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @param inv_2nd_trees See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @returns A data frame with the same structure as
#'   \code{tree_inc_rep$tree_increments}, however containg only trees that are
#'   not present in this data frame but present in the second inventory. These
#'   are trees that cannot be matched in subsequent inventories due to the
#'   survey system implemented. Therefore, increments from repeated inventories
#'   are not available for these trees. Consequently, the columns
#'   \code{iv_hub_m3_ha_per_rep} and \code{iv_hub_m3_ha_yr_rep} contain
#'   \code{NA} only. However, The columns \code{iv_hub_m3_ha_per_gnfi3} and
#'   \code{iv_hub_m3_ha_yr_gnfi3} contain backwards estimates of their volume
#'   increment.
#'
#' @keywords internal
#'
#'
inv_inc_fill_gaps_backward_nomatch <- function(tree_inc_rep, inv_2nd_trees) {

  # In contrast to the other fill-gaps functions, we are using col_names_to_keep
  # here for enforcing the the same order of the columns in the output
  col_names_to_keep <- names(tree_inc_rep$tree_increments)
  col_names_to_keep <- c(
    col_names_to_keep, "iv_hub_m3_ha_per_gnfi3", "iv_hub_m3_ha_yr_gnfi3"
  )

  # Take only those trees that are present in the second inventory, but outside
  # the previous repeated increment calculation. However, restrict to those
  # plots only that were present in both inventories
  rslt_dat <- inv_2nd_trees |>
    dplyr::filter(    # Take only matching plots in both inventories
      .data$plot_id %in% tree_inc_rep$plot_matches$matches$plot_id_inv_b
    ) |>
    dplyr::left_join( # get the plot ids from the first inventory
      tree_inc_rep$plot_matches$matches, by = c("plot_id" = "plot_id_inv_b")
    ) |>
    dplyr::rename(plot_id_1st = "plot_id_inv_a") |>
    dplyr::anti_join( # Find the no-matching trees
      tree_inc_rep$tree_increments,
      by = c("plot_id" = "plot_id_2nd", "tree_id" = "tree_id_2nd")
    ) |>
    dplyr::rename_with(
      ~ "time_yr_2nd", "time_yr"
    ) |>
    dplyr::mutate(
      # Not a problem to take the 1st element only, because time_yr_1st must be
      # equal throughout the data
      time_yr_1st = tree_inc_rep$tree_increments[1, ]$time_yr_1st
    ) |>
    # Here comes the actual calculation
    dplyr::mutate(
      # Warnings are suppressed in some places below, because they come from
      # intended species casts in the ForestElementsR functions
      dbh_cm_start = suppressWarnings(ForestElementsR::d_age_gnfi3(
        .data$species_id,
        pmax(0, .data$age_yr + .data$time_yr_1st - .data$time_yr_2nd),
        .data$dbh_cm, .data$age_yr
      )),
      height_m_start = suppressWarnings(ForestElementsR::h_age_gnfi3(
        .data$species_id,
        pmax(0, .data$age_yr + .data$time_yr_1st - .data$time_yr_2nd),
        .data$height_m, .data$age_yr
      )),
      v_m3_start = suppressWarnings(ForestElementsR::v_gri(
        .data$species_id, .data$dbh_cm_start, .data$height_m_start
      )),
      v_hub_m3_start = suppressWarnings(ForestElementsR::v_red_harvest_ubark(
        .data$species_id, .data$v_m3_start
      )),
      # Increment over the inventory period with safety catch for
      # possible values < 0 (could come from imprecisions near age 0)
      iv_hub_m3_ha_per_gnfi3 =
        .data$n_rep_ha * pmax(0, .data$v_hub_m3 - .data$v_hub_m3_start),

      # Annual increment over the whole inventory period
      iv_hub_m3_ha_yr_gnfi3 = .data$iv_hub_m3_ha_per_gnfi3 /
        (.data$time_yr_2nd - .data$time_yr_1st)
    )

  # Format the resulting data frame in order to match with the others
  rslt_dat <- rslt_dat |>
    dplyr::mutate(v_hub_m3_at_thrsh = NA_real_) |>
    dplyr::select(
      "plot_id_1st", "plot_id", "tree_id",
      "time_yr_1st", "time_yr_2nd",
      "v_hub_m3", "n_rep_ha", "v_hub_m3_at_thrsh",
      "iv_hub_m3_ha_per_gnfi3", "iv_hub_m3_ha_yr_gnfi3"
    ) |>
    dplyr::rename_with(~ "plot_id_2nd",  "plot_id")  |>
    dplyr::rename_with(~ "tree_id_2nd",  "tree_id")  |>
    dplyr::rename_with(~ "v_hub_m3_2nd", "v_hub_m3") |>
    dplyr::rename_with(~ "n_rep_ha_2nd", "n_rep_ha") |>
    dplyr::mutate(
      tree_id_1st                  = NA_character_,
      tree_exists_1st              = NA,
      tree_exists_2nd              = TRUE,
      tree_removal_2nd             = NA,
      v_hub_m3_1st                 = NA,
      n_rep_ha_1st                 = NA,
      volume_change_rate_per_decade = NA_real_,
      volume_change_implausible    = FALSE,
      species_mismatch             = FALSE,
      # These trees are never matched between the two inventories
      # (typically below the smallest dbh threshold), and therefore can
      # never be pseudo-ingrowth. Explicitly set FALSE here so the flag
      # column propagates as a proper logical with no NA entries after
      # bind_rows() in inv_inc_tree_consolidate(); downstream consumers
      # (e.g. inv_inc_summaric()) rely on a clean TRUE/FALSE.
      pseudo_ingrowth_corrected    = FALSE,
      # is_unmatchable_1st / _2nd: these rows describe trees the survey
      # design simply did not pick up at inv_1 (e.g. below the smallest
      # circle's dbh threshold). They are not "unmatchable in our sense"
      # — that term is reserved for trees whose matching key was
      # unreliable. Setting both to FALSE keeps the column TRUE/FALSE
      # clean and signals "regular below-threshold ingrowth, not part of
      # the unmatchable-inner-cohort". The summaric and fills_forward
      # guards then leave these rows alone.
      is_unmatchable_1st           = FALSE,
      is_unmatchable_2nd           = FALSE,
      iv_hub_m3_ha_per_rep         = NA,
      iv_hub_m3_ha_yr_rep          = NA,
    ) |>
    dplyr::select(dplyr::any_of(col_names_to_keep))

  return(rslt_dat)
}



#' Backward-Estimate Increments for Implausible Re-Measured Trees
#'
#' Called from within \code{\link{inv_inc_fill_gaps_gnfi3}}. Handles trees that
#' were present in both inventories but flagged as implausible during tree
#' matching (due to excessive volume shrinkage or species mismatch). These trees
#' received a zero increment from the repeated measurement and need a
#' model-based backward estimate. The second inventory's values are taken as
#' ground truth and the increment is estimated backwards for the whole inventory
#' period. The single tree increments are upscaled to an area of 1 ha.
#'
#' @param tree_inc_rep See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @param inv_2nd_trees See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @returns The data frame \code{tree_inc_rep$tree_increments} filtered for
#'   implausible re-measured trees. It contains the two additional columns
#'   \code{iv_hub_m3_ha_per_gnfi3}, and \code{iv_hub_m3_ha_yr_gnfi3} which
#'   represent the periodic and the mean annual backward estimates of their
#'   volume increment.
#'
#' @keywords internal
#'
inv_inc_fill_backward_implausible <- function(tree_inc_rep, inv_2nd_trees) {

  col_names_to_keep <- names(tree_inc_rep$tree_increments)

  # Filter for implausible re-measured trees that are NOT removals.
  # Removed trees are already handled by inv_inc_fill_gaps_forward();
  # including them here would cause double-counting in bind_rows().
  rslt_dat <- tree_inc_rep$tree_increments |>
    dplyr::filter(
      (.data$volume_change_implausible | .data$species_mismatch) &
        !.data$tree_removal_2nd
    ) |>
    dplyr::left_join(
      inv_2nd_trees,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    ) |>
    dplyr::mutate(
      # Warnings are suppressed in some places below, because they come from
      # intended species casts in the ForestElementsR functions
      dbh_cm_start = suppressWarnings(ForestElementsR::d_age_gnfi3(
        .data$species_id,
        pmax(0, .data$age_yr + .data$time_yr_1st - .data$time_yr_2nd),
        .data$dbh_cm, .data$age_yr
      )),
      height_m_start = suppressWarnings(ForestElementsR::h_age_gnfi3(
        .data$species_id,
        pmax(0, .data$age_yr + .data$time_yr_1st - .data$time_yr_2nd),
        .data$height_m, .data$age_yr
      )),
      v_m3_start = suppressWarnings(ForestElementsR::v_gri(
        .data$species_id, .data$dbh_cm_start, .data$height_m_start
      )),
      v_hub_m3_start = suppressWarnings(ForestElementsR::v_red_harvest_ubark(
        .data$species_id, .data$v_m3_start
      )),
      # Increment over the inventory period with safety catch for
      # (improbable) values < 0
      iv_hub_m3_ha_per_gnfi3 =
        .data$n_rep_ha_2nd * pmax(0, .data$v_hub_m3 - .data$v_hub_m3_start),

      # Annual increment over the whole inventory period
      iv_hub_m3_ha_yr_gnfi3 = .data$iv_hub_m3_ha_per_gnfi3 /
        (.data$time_yr_2nd - .data$time_yr_1st)
    )

  # Keep the required columns and return
  rslt_dat |> dplyr::select(
    dplyr::all_of(c(col_names_to_keep, "iv_hub_m3_ha_per_gnfi3", "iv_hub_m3_ha_yr_gnfi3"))
  )
}



#' Backward-Estimate Increments for Trees That Were Measured in Both
#' Inventories (Optional)
#'
#' Called from within \code{\link{inv_inc_fill_gaps_gnfi3}}. Covered are
#' all trees that were present and matching in both inventories as long as
#' they have no removal flag for the second inventory. In principle,
#' there are no increment gaps to fill for these trees. However, it might be
#' useful to compare estimates and real increments under certain circumstances.
#' Note that the tree level increments are upscaled to an area of 1 ha. Hereby,
#' we use the real representation numbers of each tree at the first and the
#' second survey, i.e. we do not check whether the growth estimate crossed a
#' representation threshold. Negative increments are possible and not wrong.
#'
#' @param tree_inc_rep See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @param inv_2nd_trees See entry at \code{\link{inv_inc_fill_gaps_gnfi3}}
#'
#' @param include_reptrees If \code{TRUE}, backward increment
#'   estimates are also made for trees that are present in both inventories and
#'   therefore have "real" increments. If \code{FALSE}, the columns for backward
#'   estimates will be set to \code{NA}. This might save computation time
#'   when dealing with extensive amounts of data.
#'
#' @returns A data frame with the same structure as the output of
#'   the other fill-gap functions. However only if the parameter
#'   \code{include_reptrees} is set on \code{TRUE}, the backwards estimates will
#'   actually be calculated (which can take considerable time). Otherwise, the
#'   columns reserved for the estimates will be filled with NA values.
#'
#' @keywords internal
#'
inv_inc_fill_backward_reptrees <- function(
  tree_inc_rep, inv_2nd_trees, include_reptrees
) {

  # if (.progress) .progress <- "fill back reptrees"

  col_names_to_keep <- names(tree_inc_rep$tree_increments)
  col_names_to_keep <- c(
    col_names_to_keep, "iv_hub_m3_ha_per_gnfi3", "iv_hub_m3_ha_yr_gnfi3"
  )

  if (!include_reptrees) {
    # In this case, simply fill the estimate columns with NAs
    rslt_dat <- tree_inc_rep$tree_increments |>
      dplyr::filter(
        .data$tree_exists_1st &
          (.data$tree_exists_2nd & !.data$tree_removal_2nd) &
          !.data$volume_change_implausible & !.data$species_mismatch
      ) |>
      dplyr::mutate(
        iv_hub_m3_ha_per_gnfi3 = NA,
        iv_hub_m3_ha_yr_gnfi3  = NA
      ) |>
      dplyr::select(dplyr::all_of(col_names_to_keep))
  } else {
    # Perform the backwards estimates only if explicit user wish
    ## Join with the tree information and estimate from 2nd inventory backwards
    rslt_dat <- tree_inc_rep$tree_increments |>
      dplyr::filter(
        .data$tree_exists_1st &
          (.data$tree_exists_2nd & !.data$tree_removal_2nd) &
          !.data$volume_change_implausible & !.data$species_mismatch
      ) |>
      dplyr::left_join(
        inv_2nd_trees,
        by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
      ) |>
      dplyr::mutate(
        # Warnings are suppressed in some places below, because they come from
        # intended species casts in the ForestElementsR functions
        dbh_cm_start = suppressWarnings(ForestElementsR::d_age_gnfi3(
          .data$species_id,
          pmax(0, .data$age_yr + .data$time_yr_1st - .data$time_yr_2nd),
          .data$dbh_cm, .data$age_yr
        )),
        height_m_start = suppressWarnings(ForestElementsR::h_age_gnfi3(
          .data$species_id,
          pmax(0, .data$age_yr + .data$time_yr_1st - .data$time_yr_2nd),
          .data$height_m, .data$age_yr
        )),
        v_m3_start = suppressWarnings(ForestElementsR::v_gri(
          .data$species_id, .data$dbh_cm_start, .data$height_m_start
        )),
        v_hub_m3_start = suppressWarnings(ForestElementsR::v_red_harvest_ubark(
          .data$species_id, .data$v_m3_start
        )),
        # Periodic increment
        iv_hub_m3_ha_per_gnfi3 =
          .data$n_rep_ha_2nd * .data$v_hub_m3 -
          .data$n_rep_ha_1st * .data$v_hub_m3_start,

        # Average annual increment over the whole inventory period
        iv_hub_m3_ha_yr_gnfi3 = .data$iv_hub_m3_ha_per_gnfi3 /
          (.data$time_yr_2nd - .data$time_yr_1st)
    )

    rslt_dat <- rslt_dat |> dplyr::select(dplyr::all_of(col_names_to_keep))
  }

  return(rslt_dat)
}





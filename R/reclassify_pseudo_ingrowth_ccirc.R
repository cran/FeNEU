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


#' Reclassify Pseudo-Ingrowth Trees on Concentric-Circle Plots
#'
#' In repeated inventories with concentric sampling circles, trees that grow
#' across the dbh threshold of the innermost circle (typically 12 cm) between
#' two surveys are often mis-classified as ingrowth — even though they
#' physically existed during the first survey, just on the inner circle, where
#' they may not have been individually identifiable. This function corrects
#' such pseudo-ingrowth trees so that subsequent steps of the increment
#' calculation (\code{\link{tree_inc_repsurv}} and \code{\link{inv_inc_summaric}})
#' treat them as re-measured trees with a circle transition rather than as
#' fresh ingrowth.
#'
#' A tree is classified as pseudo-ingrowth if all of the following hold:
#' \itemize{
#'   \item It is currently classified as ingrowth
#'         (\code{tree_exists_1st == FALSE} and \code{tree_exists_2nd == TRUE}).
#'   \item Its polar distance from the plot centre at the second survey
#'         (\code{R} from \code{tree_positions}) is smaller than the radius
#'         of the innermost circle of the plot's
#'         \code{circle_definition}.
#' }
#' The innermost circle is treated as perfectly concentric. In practice it is
#' sometimes laid out slightly off-centre (to avoid trampling small trees),
#' but the displacement is random in direction and magnitude, so individual
#' mis-classifications average out across many plots.
#'
#' For pseudo-ingrowth trees, the first-survey volume is reconstructed via
#' the gnfi3 growth functions
#' (\code{\link[ForestElementsR]{d_age_gnfi3}},
#'  \code{\link[ForestElementsR]{h_age_gnfi3}},
#'  \code{\link[ForestElementsR]{v_gri}}, and
#'  \code{\link[ForestElementsR]{v_red_harvest_ubark}})
#' over the full inter-survey period
#' (\code{dt = -(time_yr_2nd - time_yr_1st)}). The estimated volume is
#' clamped at zero. The first-survey representation number is always set
#' to the inner circle's value, because the very premise of pseudo-ingrowth
#' identification is that the tree was sampled on the inner circle at the
#' first survey (Inv-2 position inside r_inner, plus unmatchable). The
#' gnfi3 back-estimated dbh is only used for the volume; the sampling
#' circle is a fact of the survey design, not a model output.
#'
#' The function leaves all other trees in \code{matched_trees} untouched.
#' It always adds the column \code{pseudo_ingrowth_corrected} (boolean), with
#' \code{TRUE} for the reclassified trees and \code{FALSE} for all others.
#' This flag is meant to remain visible downstream so consumers can analyse
#' the corrected subset (e.g. for a separate tree-status category in
#' aggregation overviews).
#'
#' Downstream consumer contract:
#' \itemize{
#'   \item The per-tree methods (\code{\link{tree_inc_repsurv}} with
#'         \code{rep_classic}, \code{rep_mean}, \code{rep_end},
#'         \code{rep_trans}) consume the corrected state and benefit from
#'         the reclassification — pseudo-ingrowth trees are processed
#'         through the standard both-exist circle-transition branches.
#'   \item \code{\link{inv_inc_summaric}} explicitly undoes the
#'         reclassification on its working copy via the
#'         \code{pseudo_ingrowth_corrected} flag, because the
#'         ertragsgeschichtliche balance is by definition built on
#'         actually measured volumes and the inv_1 inner-circle cohort
#'         was never measured. There the affected trees enter only via
#'         the inv_2 volume sum, as they did before the reclassification.
#' }
#'
#' The reclassification is only meaningful when inner-circle trees of the
#' first inventory are inherently unmatchable:
#' \itemize{
#'   \item For \code{tree_id_style == "baysf"} this is always the case
#'         (BaySF data have no identifiers for inner-circle trees in the
#'         first survey).
#'   \item For \code{tree_id_style == "generic"} only when the survey design
#'         marks inner-circle identifiers as unreliable. In that case the
#'         caller must set \code{inner_circle_non_matchable = TRUE} both
#'         here and in the upstream
#'         \code{\link{match_trees_on_plot_repsurv_ccirc}} call so that
#'         Inv-1 inner-circle trees are dropped from the match.
#' }
#' In all other situations the function simply adds the
#' \code{pseudo_ingrowth_corrected} column as all-\code{FALSE} and leaves
#' the data untouched.
#'
#' @param matched_trees A data frame as produced by
#'   \code{\link{match_trees_on_inventory_repsurv_ccirc}}.
#'
#' @param inv_2nd \code{\link{fe_inventory}} object representing the later of
#'   the two inventories. Used to access \code{tree_positions} (for the polar
#'   distance \code{R}) and \code{circle_definition} (for the inner radius
#'   and the per-circle representation numbers).
#'
#' @param inv_2nd_trees Extended tree data frame for the second inventory
#'   (same object that was passed to
#'   \code{\link{match_trees_on_inventory_repsurv_ccirc}}). Provides
#'   \code{species_id}, \code{age_yr}, \code{dbh_cm}, \code{height_m}, and
#'   \code{v_hub_m3} for the gnfi3 backward estimate.
#'
#' @param tree_id_style Character string indicating how trees are uniquely
#'   identified on inventory plot level. Must be the same value that was
#'   used when matching. Options are \code{"baysf"} (default) and
#'   \code{"generic"}.
#'
#' @param inner_circle_non_matchable Logical, default \code{FALSE}. Only
#'   relevant for \code{tree_id_style == "generic"}: must be \code{TRUE}
#'   if the survey design renders inner-circle identifiers unreliable.
#'   Ignored for \code{tree_id_style == "baysf"}, where inner-circle trees
#'   of the first inventory are always treated as unmatchable.
#'
#' @returns The input \code{matched_trees} with two changes:
#' \itemize{
#'   \item For pseudo-ingrowth trees: \code{tree_exists_1st} flipped to
#'         \code{TRUE}; \code{v_hub_m3_1st} set to the gnfi3-back-estimated
#'         volume (clamped at 0); \code{n_rep_ha_1st} set to the
#'         representation number corresponding to the back-estimated dbh on
#'         the same plot.
#'   \item A new column \code{pseudo_ingrowth_corrected} (logical), with
#'         \code{TRUE} for reclassified trees and \code{FALSE} for all
#'         others.
#' }
#'   Note that \code{tree_id_1st} deliberately stays \code{NA} on these rows.
#'   The reclassification asserts that the tree \emph{existed} at inv 1, not
#'   that an individual inv-1 record was identified — there is none, which is
#'   exactly why the tree was unmatchable. A reclassified row therefore carries
#'   \code{tree_exists_1st = TRUE} together with \code{tree_id_1st = NA}, and
#'   any downstream count that keys on \code{tree_id_1st} will not see it. This
#'   is why \code{n_in_both_1st} and \code{n_in_both_2nd} of
#'   \code{\link{match_tree_statistics}} differ by exactly the number of
#'   pseudo-ingrowth trees.
#'
#'   The existing \code{v_hub_m3_at_thrsh} value is preserved as-is — it
#'   was already correctly computed for the ingrowth case by
#'   \code{calc_v_hub_m3_at_thrsh}, and that same threshold volume is what
#'   the circle-transition branch of \code{\link{tree_inc_repsurv}} needs
#'   after reclassification.
#'
#' @family increment
#' @family repeated inventory
#'
#' @export
#'
#' @examples
#'   # Prepare two matching inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   inv_pt_mtch <- match_2_inventories(inv_a, inv_b, "plot_id", "baysf")
#'   mtch_trs <- match_trees_on_inventory_repsurv_ccirc(
#'     inv_pt_mtch, inv_a, inv_b, inv_a_trees, inv_b_trees,
#'     tree_id_style = "baysf"
#'   )
#'
#'   # Reclassify pseudo-ingrowth — default tree_id_style "baysf"
#'   mtch_trs_corr <- reclassify_pseudo_ingrowth_ccirc(
#'     mtch_trs, inv_b, inv_b_trees, tree_id_style = "baysf"
#'   )
#'   sum(mtch_trs_corr$pseudo_ingrowth_corrected)
#'
reclassify_pseudo_ingrowth_ccirc <- function(
    matched_trees, inv_2nd, inv_2nd_trees,
    tree_id_style = c("baysf", "generic"),
    inner_circle_non_matchable = FALSE
) {

  tree_id_style <- match.arg(tree_id_style)

  # Short-circuit when the correction does not apply: just attach the flag
  # column so downstream consumers can always rely on its presence.
  applies <- (tree_id_style == "baysf") ||
    (tree_id_style == "generic" && isTRUE(inner_circle_non_matchable))

  if (!applies) {
    return(
      matched_trees |> dplyr::mutate(pseudo_ingrowth_corrected = FALSE)
    )
  }

  # Candidate rows: currently classified as ingrowth (1st absent, 2nd present).
  # Candidates are 2nd-only-real rows: matchable inv-2 record (verified
  # by R+angle / tree_id), no inv-1 partner. NA-semantics on
  # tree_exists_* means we must use %in% TRUE / %in% FALSE here — a
  # plain `!tree_exists_1st & tree_exists_2nd` would evaluate to NA on
  # 2nd-only-unmatchable rows (tree_exists_1st = NA), and NA values
  # would later leak into pseudo_indices via the subset and break the
  # subassignment `rslt$<col>[pseudo_indices] <- ...`.
  # 2nd-only-unmatchable trees are intentionally not candidates: they
  # already carry is_unmatchable_2nd = TRUE from the matching step and
  # are handled by summaric / fills_backward via that flag.
  is_candidate <- matched_trees$tree_exists_1st %in% FALSE &
                  matched_trees$tree_exists_2nd %in% TRUE

  # Nothing to do if there is no candidate at all.
  if (!any(is_candidate)) {
    return(
      matched_trees |> dplyr::mutate(pseudo_ingrowth_corrected = FALSE)
    )
  }

  candidate_rows <- matched_trees[is_candidate, , drop = FALSE]

  # --- Per-plot lookup: inner-circle radius and circle_definition --------
  plot_ids   <- inv_2nd$plot_id
  plot_objs  <- inv_2nd$plot

  # The inner circle is the row in circle_definition with the smallest
  # dbh_lower (typically 0). c_area is in hectares, so radius is derived
  # via r = sqrt(c_area * 10000 / pi). We also pre-compute the inner-
  # circle representation number per plot, applying slope correction if
  # a slope column is present (mirroring n_rep_ha.fe_ccircle_spatial).
  # Some plots (e.g. fe_ccircle_spatial_notrees, or degenerate entries)
  # may have an empty or missing circle_definition. For those we propagate
  # NA, which makes the downstream pseudo-ingrowth check
  # (`R < r_inner_m`) evaluate to NA and the row is then left untouched
  # by the !is.na(...) guard further down — a safe no-op.
  inner_lookup <- tibble::tibble(
    plot_id = plot_ids,
    r_inner_m       = purrr::map_dbl(plot_objs, function(p) {
      cd <- p$circle_definition
      if (is.null(cd) || nrow(cd) == 0L) return(NA_real_)
      i  <- which.min(cd$dbh_lower)
      if (length(i) == 0L) return(NA_real_)
      sqrt(cd$c_area[i] * 10000 / pi)
    }),
    n_rep_ha_inner  = purrr::map_dbl(plot_objs, function(p) {
      cd <- p$circle_definition
      if (is.null(cd) || nrow(cd) == 0L) return(NA_real_)
      i  <- which.min(cd$dbh_lower)
      if (length(i) == 0L) return(NA_real_)
      slope_corr <- 1
      if ("slope" %in% names(cd) && !is.na(cd$slope[i])) {
        slope_corr <- cos(cd$slope[i] * pi / 180)
      }
      1 / (cd$c_area[i] * slope_corr)
    }),
    # Lower BHD bound of the middle (second-smallest) circle. Used by the
    # pseudo-ingrowth check below: a true pseudo-ingrowth tree must be on
    # the second-smallest sampling circle at inv_2 (i.e. it just crossed
    # the inner-to-middle threshold). NA if the plot has fewer than two
    # circles in its circle_definition.
    middle_min_dbh  = purrr::map_dbl(plot_objs, function(p) {
      cd <- p$circle_definition
      if (is.null(cd) || nrow(cd) == 0L) return(NA_real_)
      thr <- sort(cd$dbh_lower)
      if (length(thr) >= 2L) thr[2] else NA_real_
    }),
    # Lower BHD bound of the third-smallest circle (= upper bound of the
    # middle circle range). Inf if no third circle exists, so the
    # `dbh_cm_2nd < middle_max_dbh` check accepts everything above
    # middle_min_dbh on a 2-circle design.
    middle_max_dbh  = purrr::map_dbl(plot_objs, function(p) {
      cd <- p$circle_definition
      if (is.null(cd) || nrow(cd) == 0L) return(NA_real_)
      thr <- sort(cd$dbh_lower)
      if (length(thr) >= 3L) thr[3] else Inf
    })
  )

  # --- Per-(plot, tree) lookup of polar distance R at the 2nd inventory ---
  tree_R_lookup <- purrr::map2_dfr(
    plot_objs, plot_ids,
    function(p, pid) {
      tp <- p$tree_positions
      if (is.null(tp) || nrow(tp) == 0) {
        return(tibble::tibble(
          plot_id = character(),
          tree_id = character(),
          R       = numeric()
        ))
      }
      tp_df <- sf::st_drop_geometry(tp)
      if (!("R" %in% names(tp_df))) {
        # Fall back: compute R from geometry vs. plot centre. The plot
        # centre is the geometry of the innermost circle row in
        # circle_definition (which carries the centre coordinate copied
        # in by the fe_ccircle_spatial constructor).
        cd_geom <- sf::st_geometry(p$circle_definition)
        if (length(cd_geom) == 0) {
          # Last resort: assume centre at the centroid of tree positions.
          centre <- sf::st_centroid(sf::st_union(tp))
        } else {
          centre <- cd_geom[1]
        }
        R_vals <- as.numeric(sf::st_distance(tp, centre))
      } else {
        R_vals <- tp_df$R
      }
      tibble::tibble(
        plot_id = pid,
        tree_id = tp_df$tree_id,
        R       = R_vals
      )
    }
  )

  # --- Attach R, r_inner, inner-circle n_rep_ha, and the inv-2 dbh ------
  # The inv-2 dbh is needed for the second pseudo-ingrowth criterion
  # (tree must sit on the second-smallest sampling circle at inv 2).
  dbh_lookup <- inv_2nd_trees |>
    dplyr::select("plot_id", "tree_id", dbh_cm_2nd = "dbh_cm")

  candidate_rows <- candidate_rows |>
    dplyr::left_join(
      tree_R_lookup,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    ) |>
    dplyr::left_join(
      inner_lookup,
      by = c("plot_id_2nd" = "plot_id")
    ) |>
    dplyr::left_join(
      dbh_lookup,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    )

  # Pseudo-ingrowth identification requires BOTH criteria:
  #
  #   1. Position inside the inner circle (R < r_inner_m). The tree's
  #      inv-2 polar position lies in the area where inv 1 sampled the
  #      inner-circle cohort — so its missing inv-1 partner is plausibly
  #      explained by the inv-1 inner-circle records being unmatchable.
  #
  #   2. inv-2 BHD puts the tree on the *second-smallest* sampling
  #      circle (middle_min_dbh <= dbh_cm_2nd < middle_max_dbh). A true
  #      pseudo-ingrowth tree has just crossed the inner-to-middle
  #      threshold during the inter-survey period. Trees that sit on
  #      a larger circle at inv 2 cannot be pseudo-ingrowth — they were
  #      either there at inv 1 too (and the match failed for some other
  #      reason, e.g. coordinate drift between inventories) or they
  #      would have made a double-circle jump in one period, which we
  #      explicitly accept as a vanishingly rare approximation error.
  #
  # Without criterion (2), large outer-circle trees with a position
  # coincidentally close to plot centre got falsely reclassified as
  # pseudo-ingrowth, leading to extreme rep_classic per-tree iv values
  # (n_inner * v_back can dwarf n_outer * v_2 for a large tree) — see
  # the Nordhalben diagnosis on 11.06.2026 in CLAUDE.md.
  #
  # NA in any of the four input quantities (R, r_inner_m, dbh_cm_2nd,
  # middle_min_dbh / middle_max_dbh) is treated as "not pseudo" via
  # `!is.na(...)` guards.
  is_pseudo <- !is.na(candidate_rows$R) &
    !is.na(candidate_rows$r_inner_m) &
    candidate_rows$R < candidate_rows$r_inner_m &
    !is.na(candidate_rows$dbh_cm_2nd) &
    !is.na(candidate_rows$middle_min_dbh) &
    candidate_rows$dbh_cm_2nd >= candidate_rows$middle_min_dbh &
    candidate_rows$dbh_cm_2nd <  candidate_rows$middle_max_dbh

  if (!any(is_pseudo)) {
    # No pseudo-ingrowth identified -- attach the flag column and return.
    return(
      matched_trees |> dplyr::mutate(pseudo_ingrowth_corrected = FALSE)
    )
  }

  # --- Backward gnfi3 estimate for the pseudo-ingrowth trees -------------
  pseudo_rows <- candidate_rows[is_pseudo, , drop = FALSE]

  # Get the second-inventory tree attributes needed for the gnfi3 backward.
  inv_2nd_trees_red <- inv_2nd_trees |>
    dplyr::select(
      "plot_id", "tree_id",
      "species_id", "age_yr", "dbh_cm", "height_m", "v_hub_m3"
    )

  pseudo_rows <- pseudo_rows |>
    dplyr::left_join(
      inv_2nd_trees_red,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    )

  # Per-row dt: the (negative) inter-survey period. time_yr_1st and
  # time_yr_2nd are always set on matched_trees.
  dt <- -(pseudo_rows$time_yr_2nd - pseudo_rows$time_yr_1st)

  # The gnfi3 model calls. Mirror the inline pattern in
  # inv_inc_fill_gaps_backward() to stay consistent with the rest of
  # the package. Warnings are suppressed because they stem from intended
  # species casts in the ForestElementsR functions.
  target_age <- pmax(0, pseudo_rows$age_yr + dt)

  dbh_cm_back <- suppressWarnings(
    ForestElementsR::d_age_gnfi3(
      pseudo_rows$species_id, target_age,
      pseudo_rows$dbh_cm, pseudo_rows$age_yr
    )
  )
  height_m_back <- suppressWarnings(
    ForestElementsR::h_age_gnfi3(
      pseudo_rows$species_id, target_age,
      pseudo_rows$height_m, pseudo_rows$age_yr
    )
  )
  v_m3_back <- suppressWarnings(
    ForestElementsR::v_gri(
      pseudo_rows$species_id, dbh_cm_back, height_m_back
    )
  )
  v_hub_m3_back <- suppressWarnings(
    ForestElementsR::v_red_harvest_ubark(
      pseudo_rows$species_id, v_m3_back
    )
  )
  v_hub_m3_back <- pmax(0, v_hub_m3_back)

  # First-survey representation number is the inner circle's value,
  # which was already attached to the candidate row via inner_lookup
  # above. We deliberately do NOT derive it from the back-estimated dbh
  # via findInterval: the sampling circle is a fact of the inv_1 survey
  # design (the pseudo-ingrowth tree was on the inner circle, period —
  # that is exactly why it was unmatchable). If the gnfi3 back-estimate
  # happens to push the dbh above the inner-circle threshold, that is a
  # model artefact, not evidence about which circle sampled the tree.
  n_rep_ha_back <- pseudo_rows$n_rep_ha_inner

  # --- Patch the matched_trees rows in place -----------------------------
  # Indices into the original matched_trees data frame for the pseudo set.
  candidate_indices <- which(is_candidate)
  pseudo_indices    <- candidate_indices[is_pseudo]

  rslt <- matched_trees
  rslt$tree_exists_1st[pseudo_indices] <- TRUE
  rslt$v_hub_m3_1st[pseudo_indices]    <- v_hub_m3_back
  rslt$n_rep_ha_1st[pseudo_indices]    <- n_rep_ha_back

  # Flag column: TRUE only for pseudo-ingrowth, FALSE everywhere else.
  rslt$pseudo_ingrowth_corrected <- FALSE
  rslt$pseudo_ingrowth_corrected[pseudo_indices] <- TRUE

  # Mark the inv_2 side of pseudo-ingrowth trees as unmatchable_2nd. The
  # tree's inv_2 record is the visible part of an inv_1 inner-circle cohort
  # member (the same population whose un-flagged remnants on inv_2 already
  # carry is_unmatchable_2nd = TRUE from the matching step). Grouping both
  # via the same flag keeps the "inv_2 trees whose record stems from the
  # unidentifiable inner-circle cohort" cohort fully recoverable downstream
  # — for cohort analyses, for diagnostic counts, and as the design hook
  # for a possible future separate tree-status category in
  # inv_inc_big_overview_matches_only().
  # Note: only pseudo-ingrowth-corrected rows get flipped here; rows that
  # were not reclassified (e.g. real new ingrowth with position outside
  # r_inner) keep their is_unmatchable_2nd as the matching step left it.
  if ("is_unmatchable_2nd" %in% names(rslt)) {
    rslt$is_unmatchable_2nd[pseudo_indices] <- TRUE
  } else {
    # Defensive: if upstream didn't add the column (older matched_trees
    # objects produced before this change), introduce it now with the
    # natural semantics.
    rslt$is_unmatchable_2nd <- FALSE
    rslt$is_unmatchable_2nd[pseudo_indices] <- TRUE
  }

  rslt
}

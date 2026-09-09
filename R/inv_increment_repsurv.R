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



#' Calculate Tree-Wise Increments From Two Subsequent Inventories Based
#' on Concentric Circle Plots With Spatial Information
#'
#' Main function to be used when calculating tree-level increments based on
#' repeated inventories with concentric circle sample plots
#' (\code{\link[ForestElementsR]{fe_ccircle_spatial}}).
#'
#' Even though the volume increments are calculated on tree level here, they are
#' upscaled to 1 ha. This is important, because otherwise the increment
#' represented by trees that change their representation number per ha between
#' both inventories cannot be calculated correctly. Trees present in the first
#' survey only always get zero increment; such values can be replaced by
#' model-based estimates in a subsequent step. The function allows to choose
#' between four methods of increment calculation (parameter \code{method}):
#' \itemize{
#'   \item \code{rep_classic}: The volume increment, \eqn{iv}, of trees that are
#'     present in both inventories is calculated as
#'     \eqn{iv=n[2]\cdot v[2]-n[1] \cdot v[1]}, with \eqn{n} and \eqn{v} being
#'     the representation number per hectare and the volume of the tree at the
#'     first and the second survey, respectively. The volume increment of the
#'     ingrowth trees is \eqn{iv=n[2]*v[2]}, i.e. such trees contribute to the
#'     increment with their full volume. This method is equivalent to the
#'     classic stand level increment calculation (German "Ertragsgeschichtlicher
#'     Zuwachs"), however, some trees (and plots) may also have negative
#'     increments. This might look strange, but is fully valid from a
#'     statistical angle of view.
#'   \item \code{rep_mean}: The volume increment, \eqn{iv}, of trees that are
#'     present in both inventories is calculated as
#'     \eqn{iv=(n[2]+n[1])/2\cdot (v[2]-v[1])}, and the volume increment of
#'     ingrowth trees is calculated as \eqn{iv=n[2]*(v[2]-v_t)}, where
#'     \eqn{v_t} is the tree's volume when it crossed the sampling
#'     threshold. This method is not compatible to the classic stand level
#'     increment calculation. Guarantees \eqn{iv>=0} when
#'     \code{clamp_plausible_shrinkage = TRUE}.
#'   \item \code{rep_end}: The volume increment, \eqn{iv}, of trees that are
#'     present in both inventories is calculated as
#'     \eqn{iv=n[2]\cdot (v[2]-v[1])}, and the volume increment of
#'     ingrowth trees is calculated as \eqn{iv=n[2]*(v[2]-v_t)}, where
#'     \eqn{v_t} is the tree's volume when it crossed the sampling
#'     threshold. This method is not compatible to the classic stand level
#'     increment calculation. Guarantees \eqn{iv>=0} when
#'     \code{clamp_plausible_shrinkage = TRUE}.
#'   \item \code{rep_trans}: The volume increment, \eqn{iv}, of trees that are
#'     present in both inventories, and did not cross a sampling threshold,
#'     is calculated as \eqn{iv=n\cdot (v[2]-v[1])}, with \eqn{n=n[1]=n[2]}.
#'     For trees that crossed a sampling threshold, the calculation is
#'     \eqn{iv=n[2]\cdot (v[2]-v_t) + n[1]\cdot (v_t-v[1])}.
#'     The volume increment of ingrowth trees is calculated as
#'     \eqn{iv=n[2]*(v[2]-v_t)}, where \eqn{v_t} is the tree's volume when it
#'     crossed the sampling threshold. This method is not compatible to the
#'     classic stand level increment calculation and always guarantees
#'     \eqn{iv>=0} (negative volume differences are clamped to zero regardless
#'     of \code{clamp_plausible_shrinkage}).
#' }
#'
#' @section Position in the workflow:
#' This is the \emph{first} of six steps that lead from a pair of inventories to
#' the increment tables and their PDFs. Unless a single stage has to be inspected
#' or varied, call \code{\link{inv_increment_repeated_survey}} instead: it runs
#' the whole chain in one call, hands the tree lists on where they are needed,
#' and -- importantly -- includes the plots that exist only in the second survey,
#' which a hand-built chain easily forgets (see the coverage warning in
#' \code{\link{inv_inc_tree_extend}}).
#'
#' @param inv_1st \code{\link{fe_inventory}} object representing the earlier of
#'   the two inventories. All plots of this inventory must have the class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}.
#'
#' @param inv_2nd \code{\link{fe_inventory}} object representing the later of
#'   the two inventories. All plots of this inventory must have the class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}.
#'
#' @param inv_1st_trees A data frame resulting from applying the functions
#'   \code{\link{pull_trees}}, \code{\link{height_complete_inventory}}, and
#'   \code{\link{trees_add_essentials}} to \code{inv_1st}. While this could be
#'   done internally inside this function, this has typically happened earlier
#'   already in an inventory evaluation workflow. As this is a considerably
#'   time-consuming process, we deem the redundancy acceptable.
#'
#' @param inv_2nd_trees A data frame resulting from applying the functions
#'   \code{\link{pull_trees}}, \code{\link{height_complete_inventory}}, and
#'   \code{\link{trees_add_essentials}} to \code{inv_2nd}. While this could be
#'   done internally inside this function, this has typically happened earlier
#'   already in an inventory evaluation workflow. As this is a considerably
#'   time-consuming process, we deem the redundancy acceptable.
#'
#' @param match_type Character string indicating the method for matching
#'   inventory plots from the two subsequent inventories. The option
#'   "center_coord" matches points whose center coordinates have a distance of
#'   no more than \code{coord_tolerance}, while the option "plot_id" (default)
#'   matches plots with the same id.
#'
#' @param plot_id_style Character string indicating the plot_id style to be
#'   used. Only relevant when \code{match_type == "plot_id"}. The available
#'   options are "baysf" (default) and "generic". For a match, the latter
#'   ("generic") requires the full plot_ids of two plots to be equal in both
#'   inventories. The former ("baysf") only requires the last group of digits
#'   in the plot_id for a match due to the specific BaySF data format.
#'
#' @param tree_id_style Character string that indicates how trees are uniquely
#'   identified on inventory plot level. This is required for matching trees
#'   on the same plot in both inventories. The options are "baysf" (default)
#'   and "generic". In the BaySF case, trees are matched by their polar
#'   coordinates, while in the generic case, the tree_ids must mark the same
#'   trees in both inventories.
#'
#' @param method Character string, choices are \code{"rep_classic"} (default),
#'   \code{"rep_mean"}, \code{"rep_end"}, and \code{"rep_trans"}. See Details.
#'
#' @param inner_circle_non_matchable Logical, default \code{FALSE}. Controls
#'   the handling of pseudo-ingrowth trees — trees whose dbh crossed the
#'   inner circle's threshold between the two surveys and would otherwise
#'   be counted as fresh ingrowth although they did exist at the first
#'   survey on the inner circle. For \code{tree_id_style == "baysf"}
#'   inner-circle trees of the first inventory are inherently unmatchable,
#'   and pseudo-ingrowth identification (via Inv-2 position) and gnfi3
#'   backward reconstruction always run. For \code{tree_id_style ==
#'   "generic"} set this argument to \code{TRUE} when the survey design
#'   does not yield reliable tree identifiers on the inner circle (e.g.
#'   because very small trees cannot practically be tagged in the field).
#'   In that case, Inv-1 inner-circle trees are dropped from the match
#'   pool and pseudo-ingrowth identification runs analogously to the BaySF
#'   case. See \code{\link{reclassify_pseudo_ingrowth_ccirc}} for the
#'   reconstruction details.
#'
#' @param shrinkage_rate_per_decade Numeric, the maximum plausible volume
#'   shrinkage rate per decade for re-measured trees. Trees exceeding this
#'   threshold are flagged as implausible. Uses an exponential decay model
#'   internally, scaled to the actual survey interval. Default is \code{0.10}
#'   (i.e. 10 percent per decade).
#'
#' @param species_check Character string controlling how species consistency
#'   is checked for re-measured trees. Options are \code{"species_group"}
#'   (default, flags trees whose species group differs between surveys),
#'   \code{"species"} (flags trees whose species id differs), and
#'   \code{"warn_only"} (only issues a message, does not flag).
#'
#' @param clamp_plausible_shrinkage Logical. If \code{TRUE} (default),
#'   re-measured trees with plausible volume shrinkage (i.e. below the
#'   implausibility threshold) receive a zero increment instead of a negative
#'   one. If \code{FALSE}, plausible shrinkage is passed through to the
#'   increment for \code{rep_classic}, \code{rep_mean}, and \code{rep_end}.
#'   Has no effect on \code{rep_trans}, which always clamps negative volume
#'   differences to zero. Trees flagged as implausible always receive a zero
#'   increment regardless of this setting.
#'
#' @param coord_tolerance Maximum distance allowed for a match of two inventory
#'   plots in the distance unit of the coordinate reference system of
#'   \code{inv_a} and \code{inv_b}; in virtually all relevant cases it is m.
#'   Default is 10, i.e. inventory plots whose centers have a distance of up to
#'   10 meters will be accepted as matches.
#'
#' @param progress_bar Boolean, if \code{TRUE} (default), a progress bar is
#'   shown during time critical steps of the execution.
#'
#' @returns A list with six elements: i) \code{tree_increments} is a data frame
#'   that identifies all trees that were appropriate for increment calculation
#'   with their plot and tree id in both inventories, and contains their annual
#'   and periodic volume increment (in m³/ha under bark after harvest). Note
#'   that in case of decreasing representation numbers between both surveys,
#'   increments can be also become negative which is statistically correct. ii)
#'   \code{plot_matches} informs about the outcome of the required plot matching
#'   across both inventories. It is the output of
#'   \code{\link{match_2_inventories}} which is called internally. iii)
#'   \code{match_plot_stats} which gives an overview of the plot matching
#'   results in terms of plot numbers and representation areas (i.e. the output
#'   of an internal call to \code{\link{match_plot_statistics}}). iv)
#'   \code{match_tree_stats} provides similar overview statistics on tree level.
#'   v) \code{inv_period} is the output of \code{\link{inv_period}}, computed
#'   once here so downstream functions can access the survey years and the
#'   inter-survey period without having to carry the \code{fe_inventory}
#'   objects further. vi) \code{method} is the increment method that was
#'   used in this call (one of \code{"rep_classic"}, \code{"rep_mean"},
#'   \code{"rep_end"}, \code{"rep_trans"}); carried through so downstream
#'   consumers can label outputs accordingly.
#'
#' @family increment
#' @family repeated inventory
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
#'   # The two match types. "center_coord" pairs plots by their centres within
#'   # a tolerance, "plot_id" by their identifiers.
#'   inv_increment_repsurv_ccirc(
#'     inv_a, inv_b, inv_a_trees, inv_b_trees,
#'     match_type = "center_coord", coord_tolerance = 10,
#'     method = "rep_classic"
#'   )
#'
#'   # The four methods differ only in how the increment of a single tree is
#'   # formed; "rep_classic" above and "rep_trans" here are the two that matter
#'   # in practice, "rep_mean" and "rep_end" are set the same way. See the
#'   # increment vignette for the formulae.
#'   inv_increment_repsurv_ccirc(
#'     inv_a, inv_b, inv_a_trees, inv_b_trees,
#'     match_type = "plot_id", plot_id_style = "baysf",
#'     method = "rep_trans"
#'   )
#'
inv_increment_repsurv_ccirc <- function(
    inv_1st, inv_2nd, inv_1st_trees, inv_2nd_trees,
    match_type = c("plot_id", "center_coord"),
    plot_id_style = c("baysf", "generic"),
    tree_id_style = c("baysf", "generic"),
    method = c("rep_classic", "rep_mean", "rep_end", "rep_trans"),
    inner_circle_non_matchable = FALSE,
    shrinkage_rate_per_decade = 0.10,
    species_check = c("species_group", "species", "warn_only"),
    clamp_plausible_shrinkage = TRUE,
    coord_tolerance = 10,
    progress_bar = TRUE
) {

  stopifnot(is_fe_inventory(inv_1st))
  stopifnot(is_fe_inventory(inv_2nd))

  match_type    <- match.arg(match_type)
  plot_id_style <- match.arg(plot_id_style)
  tree_id_style <- match.arg(tree_id_style)
  method        <- match.arg(method)
  species_check <- match.arg(species_check)

  # Call internal sub function for input check that either terminates with an
  # error or gives back a vector with the first and the second inventory year
  # (in that order) in case of success
  inv_years  <- inv_increment_repsurv_ccirc_check_input(inv_1st, inv_2nd)

  # Inventory period info — computed once here so downstream consumers can
  # rely on it without having to carry the fe_inventory objects further.
  inv_period_info <- inv_period(inv_1st, inv_2nd)

  # Try to match both inventories on plot level
  match_rslt_plots <- match_2_inventories(
    inv_1st, inv_2nd, match_type, plot_id_style, coord_tolerance
  )
  # Throw an error and return match_rslt (for user's convenience), if proceeding
  # makes no sense (no or multiple matches)
  proceed <- tryCatch({
    stopifnot(
      (nrow(match_rslt_plots$matches) > 0) &
        all(!match_rslt_plots$multiple_matches)
    )
    TRUE
  }, error = function(e) {
    message(fe_msg("inv_matching_problematic", e$message))
    FALSE
  })
  if (!proceed) return(match_rslt_plots)

  # Find all matching trees in both inventories and get all information required
  # for calculating their volume increment
  match_rslt_trees <- match_trees_on_inventory_repsurv_ccirc(
    match_rslt_plots, inv_1st, inv_2nd, inv_1st_trees, inv_2nd_trees,
    tree_id_style,
    inner_circle_non_matchable = inner_circle_non_matchable,
    shrinkage_rate_per_decade  = shrinkage_rate_per_decade,
    species_check              = species_check,
    progress_bar               = progress_bar
  )

  # Reclassify pseudo-ingrowth trees (small trees that crossed the inner
  # circle's dbh threshold between the surveys and would otherwise be
  # counted as fresh ingrowth, although they did exist at the first
  # survey on the inner circle). For trees with an Inv-2 position inside
  # the plot's inner circle radius, the first-survey volume is
  # reconstructed via gnfi3 backward over the full inter-survey period
  # and the row is flipped to "present in both inventories". Adds the
  # boolean column pseudo_ingrowth_corrected for downstream visibility.
  # The correction applies in tree_id_style == "baysf" always, and in
  # tree_id_style == "generic" only when inner_circle_non_matchable was
  # set; in all other cases the call only adds the FALSE flag column.
  match_rslt_trees <- reclassify_pseudo_ingrowth_ccirc(
    match_rslt_trees, inv_2nd, inv_2nd_trees,
    tree_id_style              = tree_id_style,
    inner_circle_non_matchable = inner_circle_non_matchable
  )

  # Calculate single tree increments
  tree_increments <- tree_inc_repsurv(
    match_rslt_trees, method, clamp_plausible_shrinkage
  )

  # Generate tree match overview statistics
  match_tree_stats <- match_tree_statistics(
    match_rslt_trees, inv_1st_trees, inv_2nd_trees
  )

  # Generate plot match overview statistics
  match_plot_stats <- match_plot_statistics(match_rslt_plots, inv_1st, inv_2nd)

  # Assemble output object
  rslt_list <- list(
    tree_increments  = tree_increments,
    plot_matches     = match_rslt_plots,
    match_plot_stats = match_plot_stats,
    match_tree_stats = match_tree_stats,
    inv_period       = inv_period_info,
    method           = method
  )

  return(rslt_list)
}




# The internal helpers in this file carry no @family tag on purpose. A family
# tag would list them in the "See Also" of every exported member of that
# family, offering the reader a link to something they cannot call.
#' Check Input Provided for inv_increment_repsurv_ccirc
#'
#' Two sample plot based inventories that are considered repeated inventories of
#' the same forest area are checked for important preconditions for matching
#' them plotwise in a subsequent step (appropriate classes, consistency with
#' regard to inventory times). If the check is successful, this does not
#' guarantee succesful matching later, but if it is not, matching makes no
#' sense at all. The function belongs in the context of calculating increment
#' from repated sample inventories.
#'
#' The function is typically called from within
#' \code{\link{inv_increment_repsurv_ccirc}}. Note that the check only covers
#' the two \code{\link{fe_inventory}} objects handed to
#' \code{\link{inv_increment_repsurv_ccirc}}. It does not check the two
#' corresponding data frames that are also expected as an input to
#' \code{\link{inv_increment_repsurv_ccirc}} for reasons of computation
#' efficiency. However, as long as the user makes sure that these data frames
#' are generated from the \code{\link{fe_inventory}} objects, checking the
#' latter is sufficient.
#'
#' @param inv_1st \code{\link{fe_inventory}} object representing the earlier of
#'   the two inventories. All plots of this inventory must have the class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}, otherwise the check will
#'   terminate early.
#'
#' @param inv_2nd \code{\link{fe_inventory}} object representing the later of
#'   the two inventories. All plots of this inventory must have the class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}, otherwise the check will
#'   terminate early.
#'
#' @return A two-element vector containing the year of the first and the second
#'   inventory if all checks are successful, i.e. both inventories fulfill all
#'   preconditions for meaningfully matching them. Otherwise, the function
#'   terminates with an error.
#'
#' @keywords internal
#'
#'
inv_increment_repsurv_ccirc_check_input <- function(inv_1st, inv_2nd) {

  # Both inventories must be fe_inventory objects
  stopifnot(is_fe_inventory(inv_1st))
  stopifnot(is_fe_inventory(inv_2nd))

  # All inventory plots must have class fe_ccircle_spatial (_notrees
  # is included)
  test_1st <- inv_1st$plot |>
    purrr::map_lgl(.f = ForestElementsR::is_fe_ccircle_spatial)
  test_2nd <- inv_2nd$plot |>
    purrr::map_lgl(.f = ForestElementsR::is_fe_ccircle_spatial)

  if (any(!test_1st)) stop(
    "'is_fe_ccircle_spatial' is FALSE for plots in the first survey.",
    call. = TRUE
  )
  if (any(!test_2nd)) stop(
    "'is_fe_ccircle_spatial' is FALSE for plots in the second survey.",
    call. = TRUE
  )

  # Extract the survey years (uniformity is part of fe_inventory validation
  # but get_inv_year() double-checks defensively). inv_period() then enforces
  # 1st < 2nd and a minimum interval of one year.
  period <- inv_period(inv_1st, inv_2nd)

  # Give back the two inventory years in ascending order (kept for
  # backwards compatibility of this exported helper).
  return(c(period$time_yr_1st, period$time_yr_2nd))
}




#' Match Trees on Subsequent Inventories of One Plot
#'
#' Identifying trees that do or do not occur in two subsequent observations of
#' one inventory plot of class
#' \code{\link[ForestElementsR]{fe_ccircle_spatial}}. Takes into account only
#' trees with an entry in the \code{tree_positions} slot of the
#' \code{\link[ForestElementsR]{fe_ccircle_spatial}} object. Trees that have
#' a removal flag in the first observation are excluded, because they are not
#' relevant for increment calculations.
#'
#' Typically called from within
#' \code{\link{match_trees_on_inventory_repsurv_ccirc}} which in turn is called
#' by \code{\link{inv_increment_repsurv_ccirc}}.
#' The function does not check whether the two input inventory plots
#' (\code{plot_in_inv_1st}, \code{plot_in_inv_2nd}) are really matching, i.e.
#' represent two subsequent surveys of the same plot. This must be made sure
#' before calling it.
#'
#' @param plot_in_inv_1st An inventory plot represented as an object of class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}} or its child class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial_notrees}}.
#'
#' @param plot_in_inv_2nd An inventory plot representing a later survey of plot
#'   \code{plot_in_inv_1st}. Must be an object of class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}} or its child class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial_notrees}}.
#'
#' @param tree_id_style Character string that indicates how trees are uniquely
#'   identified on inventory plot level. This is required for matching trees
#'   on the same plot in both inventories. The options are "baysf" (default)
#'   and "generic". In the BaySF case, trees are matched by their polar
#'   coordinates, while in the generic case, the tree_ids must mark the same
#'   trees in both inventories.
#'
#' @param inner_circle_non_matchable Logical, default \code{FALSE}. Only
#'   relevant for \code{tree_id_style == "generic"}: set to \code{TRUE} when
#'   the survey design renders inner-circle tree identifiers unreliable —
#'   e.g. when very small trees cannot practically be individually tagged
#'   in the field. In that case, all trees of the first inventory that
#'   belong to the inner circle by dbh (\code{dbh_cm <
#'   min(circle_definition$dbh_lower[dbh_lower > 0])}) are removed from
#'   the match pool, so the matching behaves analogously to the BaySF case
#'   on the inner circle. Inner-circle trees of the second inventory are
#'   left untouched; they will simply not find a partner. Has no effect
#'   for \code{tree_id_style == "baysf"} (where inner-circle trees of the
#'   first inventory are inherently unmatchable already).
#'
#' @section Trees sharing one position:
#'   With \code{tree_id_style == "baysf"} trees are matched on their exact
#'   polar coordinates. If two trees of the same plot and survey carry the
#'   same distance and azimuth, that position identifies neither of them, and
#'   an exact-coordinate join would pair every inv-1 tree at the spot with
#'   every inv-2 tree at the spot — a Cartesian product that lets a tree enter
#'   the increment more than once.
#'
#'   The rule is therefore: \strong{a position shared by more than one tree of
#'   the same plot and survey is treated as no position at all}. The affected
#'   trees are handled exactly like trees recorded without coordinates — they
#'   are excluded from the match and flagged \code{is_unmatchable_1st} resp.
#'   \code{is_unmatchable_2nd}. Their measured volumes still reach the
#'   standing-volume aggregates, they simply contribute no individual
#'   increment, and they never become pseudo-ingrowth candidates (their
#'   \code{tree_exists_*} stays \code{NA}). Each occurrence raises a warning
#'   naming the plot and the trees.
#'
#'   Duplicated coordinates are a defect of the source data, not a modelling
#'   choice, and they are rare (single-digit counts per real inventory). This
#'   rule keeps the increment computation well-defined and conservative rather
#'   than resolving the ambiguity by guessing which tree is which.
#'
#' @returns A data frame with seven columns, \code{tree_id_1st},
#'   \code{tree_id_2nd}, \code{tree_exists_1st}, \code{tree_exists_2nd},
#'   \code{time_yr_1st}, and \code{time_yr_2nd}. The first two contain the ids
#'   of the plot and the matching trees in the first inventory. If a tree is not
#'   present in one inventory, its id value is \code{NA}. Similarly, the third
#'   and the fourth column have the value \code{TRUE} or \code{FALSE} if a tree
#'   exists in an inventory or not. The column \code{tree_removal_2nd} indicates
#'   whether a tree has been registed as removed (\code{TRUE}) or not
#'   (\code{FALSE}) in the second inventory.
#'   The tree_id and the tree_exists columns are somewhat redundant, but their
#'   different formats may be useful in subsequent evaluations. The sixth and
#'   the seventh column contain the years of both inventories. These two columns
#'   do never have NA values.
#'
#' @family increment
#' @family repeated inventory
#' @family inventory matches
#'
#' @export
#'
#' @examples
#'   # Standard case: Match two single plots, both with the same set of trees
#'
#'   ## Select two matching plots from two subsequent inventories
#'   plot_1st <- data_ex3_previous_sample_fe_inventory |>
#'     dplyr::filter(plot_id == "900100002_1_99900002") |>
#'     purrr::pluck("plot", 1)
#'
#'   plot_2nd <- data_ex3_sample_fe_inventory |>
#'     dplyr::filter(plot_id == "900000002_1_99900002") |>
#'     purrr::pluck("plot", 1)
#'
#'   ## Here's the actual matching
#'   match_trees_on_plot_repsurv_ccirc(
#'     plot_1st, plot_2nd, tree_id_style = "baysf"
#'   )
#'
#'
#'   # Standard case: Match two single plots, some trees however missing in
#'   #                the first or the second inventory
#'
#'   ## Construct the data from the example above by clipping out trees from
#'   ## plot_1st
#'   plot_2nd   <- plot_1st
#'
#'   miss_index <- c(1, 2, 4, 5)
#'   plot_1st$trees <- plot_1st$trees |>
#'     dplyr::arrange(tree_id) |>
#'     dplyr::slice(- miss_index)
#'   plot_1st$tree_positions <- plot_1st$tree_positions |>
#'     dplyr::arrange(tree_id) |>
#'     dplyr::slice(- miss_index)
#'
#'   miss_index <- c(3, 7, 8)
#'   plot_2nd$trees <- plot_2nd$trees |>
#'     dplyr::arrange(tree_id) |>
#'     dplyr::slice(- miss_index)
#'   plot_2nd$tree_positions <- plot_2nd$tree_positions |>
#'     dplyr::arrange(tree_id) |>
#'     dplyr::slice(- miss_index)
#
#'   ## Matching works with both styles in this example
#'   match_trees_on_plot_repsurv_ccirc(
#'     plot_1st, plot_2nd, tree_id_style = "baysf"
#'   )
#'   match_trees_on_plot_repsurv_ccirc(
#'     plot_1st, plot_2nd, tree_id_style = "generic"
#'   )
#'
#'
#'   # Special case: First survey has no trees (all ingrowth)
#'
#'   ## Construct an artifical example
#'   plot_1st <- ForestElementsR::spruce_pine_ccircle_spatial_notrees
#'   plot_2nd <- ForestElementsR::spruce_pine_ccircle_spatial
#'
#'   ## Matching works with both styles in this example
#'   match_trees_on_plot_repsurv_ccirc(
#'     plot_1st, plot_2nd, tree_id_style = "baysf"
#'   )
#'   match_trees_on_plot_repsurv_ccirc(
#'     plot_1st, plot_2nd, tree_id_style = "generic"
#'   )
#'
#'
#'   # Special case: Second survey has no trees (all felled)
#'
#'   ## Construct an artifical example
#'   plot_1st <- ForestElementsR::spruce_pine_ccircle_spatial
#'   plot_2nd <- ForestElementsR::spruce_pine_ccircle_spatial_notrees
#'
#'   ## Matching works with both styles in this example
#'   match_trees_on_plot_repsurv_ccirc(
#'     plot_1st, plot_2nd, tree_id_style = "baysf"
#'   )
#'   match_trees_on_plot_repsurv_ccirc(
#'     plot_1st, plot_2nd, tree_id_style = "generic"
#'   )
#'
#'
match_trees_on_plot_repsurv_ccirc <- function(
    plot_in_inv_1st, plot_in_inv_2nd, tree_id_style = c("baysf", "generic"),
    inner_circle_non_matchable = FALSE
) {

  stopifnot(ForestElementsR::is_fe_ccircle_spatial(plot_in_inv_1st))
  stopifnot(ForestElementsR::is_fe_ccircle_spatial(plot_in_inv_2nd))

  # ---------------------------------------------------------------------------
  # Build the per-plot matching pool.
  #
  # We start from the full tree list (`$trees`) instead of `$tree_positions`,
  # because the `fe_ccircle_spatial` constructor filters trees without
  # polar coordinates out of `tree_positions` before the matching can see
  # them. In real BaySF inventories, the innermost circle's trees (BHD < 12
  # cm) are recorded without polar coordinates by survey convention; they
  # exist in `$trees` but never appear in `$tree_positions`. Starting from
  # `$trees` keeps these unmatchable trees in the pool, so they can carry
  # their measured volumes through the rest of the pipeline. The
  # downstream NA-semantics on `tree_exists_*` and the `is_unmatchable_*`
  # flags identify them correctly.
  #
  # Concretely: pull `$trees` for each plot, then left-join the polar
  # coordinates from `$tree_positions` (dropping its sf geometry) so that
  # the resulting pool has one row per tree with `R` / `angle` set to NA
  # for trees without coordinates.
  # ---------------------------------------------------------------------------

  build_pool <- function(plot_obj) {
    if (is.null(plot_obj$trees) || nrow(plot_obj$trees) == 0L) return(NULL)
    pool <- plot_obj$trees
    if (!is.null(plot_obj$tree_positions) &&
        nrow(plot_obj$tree_positions) > 0L) {
      coords <- sf::st_drop_geometry(plot_obj$tree_positions)
      keep <- intersect(c("tree_id", "R", "angle"), names(coords))
      coords <- coords[, keep, drop = FALSE]
      pool <- pool |> dplyr::left_join(coords, by = "tree_id")
    }
    # Ensure R / angle columns exist (NA where coordinates are missing).
    if (!"R"     %in% names(pool)) pool$R     <- NA_real_
    if (!"angle" %in% names(pool)) pool$angle <- NA_real_
    pool
  }

  tree_pos_1st <- build_pool(plot_in_inv_1st)
  tree_pos_2nd <- build_pool(plot_in_inv_2nd)

  # Remove any trees from first survey that have a removal flag — such
  # trees are not relevant for increment calculation. (They are still
  # represented at inv 2 via the second-survey pool with `removal = TRUE`,
  # where the v_aus aggregate of inv_inc_summaric picks them up.)
  if (!is.null(tree_pos_1st) && "removal" %in% names(tree_pos_1st)) {
    tree_pos_1st <- tree_pos_1st |>
      dplyr::filter(!.data$removal %in% TRUE)
    if (nrow(tree_pos_1st) == 0L) tree_pos_1st <- NULL
  }

  # If both inventories have empty / missing pools, matching makes no
  # sense.
  if (is.null(tree_pos_1st) && is.null(tree_pos_2nd)) return(NULL)

  # A logical variable 'tree_exists' is added in order to be safely able to
  # check the full_join result below. Still, one of the two tree_pos objects
  # can be NULL.
  if (!is.null(tree_pos_1st))
    tree_pos_1st <- tree_pos_1st |> dplyr::mutate(tree_exists = TRUE)
  if (!is.null(tree_pos_2nd))
    tree_pos_2nd <- tree_pos_2nd |> dplyr::mutate(tree_exists = TRUE)

  # If only one of the pools is populated, the other gets an empty-shape
  # twin so the downstream full_join stays well-defined. Effect: all
  # trees on the populated side become 1st-only / 2nd-only rows.
  if (!is.null(tree_pos_1st) && is.null(tree_pos_2nd))
    tree_pos_2nd <- dplyr::slice_head(tree_pos_1st, n = 0)
  if (is.null(tree_pos_1st) && !is.null(tree_pos_2nd))
    tree_pos_1st <- dplyr::slice_head(tree_pos_2nd, n = 0)

  # ---------------------------------------------------------------------------
  # Prepare join based on selected style. The choice of join key and the
  # treatment of unmatchable trees differ between styles, but the goal is
  # the same: at the end of this block we want a `rslt` data frame in which
  # every relevant tree appears exactly once, with two boolean flags
  # `is_unmatchable_1st` / `is_unmatchable_2nd` indicating whether the row's
  # inv_1 resp. inv_2 record had a matching key that could in principle have
  # joined (FALSE) or not (TRUE).
  #
  # Why this matters: a row with `tree_exists_1st = TRUE, tree_exists_2nd =
  # FALSE` and `is_unmatchable_1st = FALSE` is a *real* disappearance — the
  # tree had a valid matching key but no inv_2 partner was found, so it
  # legitimately enters the v_aus aggregate in `inv_inc_summaric()` with a
  # gnfi3-forward half-period estimate. The same row pattern but with
  # `is_unmatchable_1st = TRUE` is an *unidentifiable* tree — we could not
  # match it at all, so we cannot tell whether it died, moved circles, or
  # survived; in that case the tree must only contribute to v_1 (via its
  # measured inv_1 volume) and be excluded from v_aus. The summaric guard
  # uses the flag to make exactly this distinction.
  #
  # The flag is also load-bearing for `reclassify_pseudo_ingrowth_ccirc()`
  # on the inv_2 side: trees that the helper reclassifies as pseudo-
  # ingrowth (visible at inv_2 but in the inv_1 inner-circle cohort) get
  # `is_unmatchable_2nd = TRUE` set there, so the cohort of "inv_2 trees
  # whose inv_2 record stems from the unmatchable inner-circle population"
  # is fully identifiable in the output.
  # ---------------------------------------------------------------------------
  tree_id_style <- match.arg(tree_id_style)

  # Helper: derive the inner-circle dbh threshold for a plot from its
  # circle_definition. Returns NA when the plot has no concentric design or
  # an empty/degenerate circle_definition (no-op for inner_circle handling).
  get_inner_max_dbh <- function(cd) {
    if (is.null(cd) || nrow(cd) == 0L) return(NA_real_)
    thrshs_above_inner <- cd$dbh_lower[cd$dbh_lower > 0]
    if (length(thrshs_above_inner) == 0L) return(NA_real_)
    min(thrshs_above_inner)
  }

  # These hold-aside containers are only used in the generic +
  # inner_circle_non_matchable path; they collect inner-circle rows that
  # are bound back to the join result after the matchable rows have been
  # joined.
  unmatch_1st_held <- NULL
  unmatch_2nd_held <- NULL

  if (tree_id_style == "baysf") {
    # ---------------------------------------------------------------------
    # baysf style: join on polar coordinates (R, angle).
    # Trees with NA in R or angle naturally never join (NA != NA in the
    # dplyr full_join), so they emerge as 1st-only / 2nd-only rows. In
    # real BaySF inventories the importer does not put inner-circle
    # trees into tree_positions at all (BHD < 12 cm cohort is recorded
    # without individual coordinates), so this branch produces no
    # unmatchable rows there — the filter is just a robust no-op.
    # In synthetic / simulated datasets the importer may still carry
    # inner-circle trees into tree_positions with NA in R / angle; for
    # those the flag below correctly marks them as unmatchable so the
    # summaric balance and the fills_forward step handle them as such.
    # ---------------------------------------------------------------------
    join_what <- dplyr::join_by(!!rlang::sym("R"), !!rlang::sym("angle"))

    # A position that two trees of the same plot share cannot identify either
    # of them. Joining on the exact coordinates would pair every inv-1 tree at
    # that spot with every inv-2 tree at that spot -- a Cartesian product, so a
    # tree would enter the increment more than once. Such trees are therefore
    # treated exactly like trees recorded without coordinates: the key is
    # cleared so it cannot join, and the tree is flagged unmatchable. Its
    # measured volume still reaches the v_1 / v_2 aggregates, and because its
    # tree_exists_* stays NA it never becomes a pseudo-ingrowth candidate.
    #
    # Duplicated polar coordinates are a defect of the source data (two trees
    # recorded at the same spot), rare but real: Nordhalben 1176/1223 carries
    # 7 + 2 cases, Pegnitz 1155/1203 8 + 5. Reporting them per plot is
    # affordable at that frequency.
    ambiguous_position <- function(pool) {
      has <- !is.na(pool$R) & !is.na(pool$angle)
      key <- paste(pool$R, pool$angle)
      has & key %in% unique(key[has][duplicated(key[has])])
    }

    if (!is.null(tree_pos_1st)) {
      amb <- ambiguous_position(tree_pos_1st)
      tree_pos_1st$R[amb]     <- NA_real_
      tree_pos_1st$angle[amb] <- NA_real_
      tree_pos_1st$is_unmatchable_1st <-
        is.na(tree_pos_1st$R) | is.na(tree_pos_1st$angle)
      if (any(amb))
        warning(fe_msg("tree_position_ambiguous",
                       as.character(plot_in_inv_1st$stand_id), "1",
                       paste(tree_pos_1st$tree_id[amb], collapse = ", ")),
                call. = FALSE)
    }
    if (!is.null(tree_pos_2nd)) {
      amb <- ambiguous_position(tree_pos_2nd)
      tree_pos_2nd$R[amb]     <- NA_real_
      tree_pos_2nd$angle[amb] <- NA_real_
      tree_pos_2nd$is_unmatchable_2nd <-
        is.na(tree_pos_2nd$R) | is.na(tree_pos_2nd$angle)
      if (any(amb))
        warning(fe_msg("tree_position_ambiguous",
                       as.character(plot_in_inv_2nd$stand_id), "2",
                       paste(tree_pos_2nd$tree_id[amb], collapse = ", ")),
                call. = FALSE)
    }
  }
  else {
    # ---------------------------------------------------------------------
    # generic style: join on tree_id. By construction tree_id is always
    # present (validated by fe_ccircle_spatial), so a naive join would
    # match every persistent tree. When the survey design renders
    # inner-circle identifiers unreliable (e.g. very small trees that
    # cannot be tagged individually), the caller signals this via
    # inner_circle_non_matchable = TRUE. In that case we explicitly hold
    # the inner-circle rows aside (split-and-bind): the matchable trees
    # go through the join, then the held rows are bound back as 1st-only
    # / 2nd-only with their is_unmatchable_* flag set to TRUE. This
    # keeps their measured volumes available for the summaric v_1 / v_2
    # aggregates while preventing spurious matches.
    # ---------------------------------------------------------------------
    join_what <- dplyr::join_by(!!rlang::sym("tree_id"))

    if (isTRUE(inner_circle_non_matchable)) {
      inner_max_1st <- get_inner_max_dbh(plot_in_inv_1st$circle_definition)
      inner_max_2nd <- get_inner_max_dbh(plot_in_inv_2nd$circle_definition)

      if (!is.null(tree_pos_1st) && !is.na(inner_max_1st) &&
          "dbh_cm" %in% names(tree_pos_1st)) {
        is_inner <- !is.na(tree_pos_1st$dbh_cm) &
          tree_pos_1st$dbh_cm < inner_max_1st
        if (any(is_inner)) {
          unmatch_1st_held <- tree_pos_1st[is_inner, , drop = FALSE]
          unmatch_1st_held$is_unmatchable_1st <- TRUE
          tree_pos_1st <- tree_pos_1st[!is_inner, , drop = FALSE]
          if (nrow(tree_pos_1st) == 0L) tree_pos_1st <- NULL
        }
      }
      if (!is.null(tree_pos_2nd) && !is.na(inner_max_2nd) &&
          "dbh_cm" %in% names(tree_pos_2nd)) {
        is_inner <- !is.na(tree_pos_2nd$dbh_cm) &
          tree_pos_2nd$dbh_cm < inner_max_2nd
        if (any(is_inner)) {
          unmatch_2nd_held <- tree_pos_2nd[is_inner, , drop = FALSE]
          unmatch_2nd_held$is_unmatchable_2nd <- TRUE
          tree_pos_2nd <- tree_pos_2nd[!is_inner, , drop = FALSE]
          if (nrow(tree_pos_2nd) == 0L) tree_pos_2nd <- NULL
        }
      }

      # If hold-asides emptied one side completely, fall back to an empty
      # version of the other side so the full_join stays well-defined.
      if (is.null(tree_pos_1st) && !is.null(tree_pos_2nd))
        tree_pos_1st <- dplyr::slice_head(tree_pos_2nd, n = 0)
      if (!is.null(tree_pos_1st) && is.null(tree_pos_2nd))
        tree_pos_2nd <- dplyr::slice_head(tree_pos_1st, n = 0)
    }

    # Initialise flags on the remaining (matchable) trees as FALSE so the
    # column exists after the join. This is symmetric to how baysf
    # handles trees with valid coordinates.
    if (!is.null(tree_pos_1st))
      tree_pos_1st$is_unmatchable_1st <- FALSE
    if (!is.null(tree_pos_2nd))
      tree_pos_2nd$is_unmatchable_2nd <- FALSE
  }

  # Make a full join of the matchable trees. `na_matches = "never"` is
  # essential: dplyr's default is "na", which would match NA-coord rows
  # of inv 1 against NA-coord rows of inv 2 (Cartesian product) and
  # silently match unmatchable trees to each other. With "never", any
  # NA in the join key prevents the match — exactly what we need so
  # that NA-coord rows surface as 1st-only / 2nd-only unmatchable rows.
  rslt <- dplyr::full_join(
    tree_pos_1st, tree_pos_2nd,
    by = join_what,
    suffix = c("_1st", "_2nd"),
    na_matches = "never"
  )

  # ---------------------------------------------------------------------------
  # Bind the held-aside inner-circle rows back as 1st-only / 2nd-only rows.
  # The held rows came from tree_pos_1st / tree_pos_2nd before the join, so
  # their non-key columns need the "_1st" / "_2nd" suffix to align with
  # rslt's post-join schema. The other side's columns are filled with NA
  # by bind_rows, which is the correct semantic: a 1st-only unmatchable
  # row has no inv_2 information, and vice versa.
  # ---------------------------------------------------------------------------
  bind_held_rows <- function(rslt, held, side = c("1st", "2nd"), join_key) {
    if (is.null(held) || nrow(held) == 0L) return(rslt)
    side <- match.arg(side)
    suffix <- paste0("_", side)
    keep_unsuffixed <- c(join_key, "is_unmatchable_1st", "is_unmatchable_2nd")
    held_renamed <- held |>
      dplyr::rename_with(
        ~ paste0(.x, suffix),
        .cols = !tidyselect::any_of(keep_unsuffixed)
      )
    dplyr::bind_rows(rslt, held_renamed)
  }
  # join_key matters for the generic case where the key is "tree_id" and
  # stays unsuffixed in the join result. For baysf, the key columns are
  # R + angle which DO get suffixed (they appear on both sides), but we
  # never bind held rows there.
  if (tree_id_style != "baysf") {
    rslt <- bind_held_rows(rslt, unmatch_1st_held, "1st", "tree_id")
    rslt <- bind_held_rows(rslt, unmatch_2nd_held, "2nd", "tree_id")
  }

  # ---------------------------------------------------------------------------
  # Equalize step — produce a clean schema with NA semantics for tree_exists:
  #   tree_exists_1st = TRUE  : row carries inv_1 data (whether or not it
  #                              matched an inv_2 partner)
  #   tree_exists_1st = FALSE : row carries no inv_1 data and we verified
  #                              that the tree is genuinely absent at inv_1
  #                              (= "real ingrowth" from a matchable inv_2
  #                              record that found no inv_1 partner)
  #   tree_exists_1st = NA    : row carries no inv_1 data because the
  #                              counterpart on the other side was
  #                              unmatchable — we could not look (e.g. NA
  #                              coordinates on inv_2 in baysf, or held-
  #                              aside inv_2 inner-circle in generic +
  #                              inner_circle_non_matchable). The inv_1
  #                              status is genuinely unknown for this row.
  # Symmetric for tree_exists_2nd.
  #
  # Downstream consumers (inv_inc_summaric, fills_*, tree_inc_repsurv) use
  # `%in% TRUE` / `%in% FALSE` filters, which treat NA as not-matching,
  # so unmatchable rows are naturally excluded from v_aus, forward fills,
  # backward fills, and per-tree increment formulas — without needing
  # explicit `!is_unmatchable_*` guards. The is_unmatchable_* flags
  # remain in the schema as explicit cohort markers (TRUE/FALSE only,
  # never NA), useful for diagnostics, the pseudo-ingrowth-corrected
  # marker assignment, and possible future per-status aggregation.
  #
  # Concretely, after the full_join (and possibly a bind_rows for held-
  # aside generic+flag rows):
  #   - is_unmatchable_1st / _2nd may be NA on the "other-side" of a 1st-
  #     only or 2nd-only row (the field came only from one side of the
  #     join). We coerce those NA values to FALSE because the flag's
  #     "doesn't apply to this row's missing side" interpretation is
  #     clearest as FALSE.
  #   - tree_exists_1st / _2nd may be NA on the missing side. Coerce to
  #     FALSE for rows whose counterpart is_unmatchable_* is FALSE
  #     (genuine absence) and keep NA when the counterpart is TRUE
  #     (unmatchable, so we cannot verify).
  # ---------------------------------------------------------------------------
  if (tree_id_style == "baysf") {
    rslt <- rslt |>
      dplyr::mutate(
        is_unmatchable_1st = ifelse(
          !is.na(.data$is_unmatchable_1st), .data$is_unmatchable_1st, FALSE
        ),
        is_unmatchable_2nd = ifelse(
          !is.na(.data$is_unmatchable_2nd), .data$is_unmatchable_2nd, FALSE
        ),
        tree_exists_1st = ifelse(
          !is.na(.data$tree_exists_1st),
          .data$tree_exists_1st,
          # tree_exists_1st was NA → row is 2nd-only. NA stays only if the
          # 2nd side was unmatchable (we could not look for an inv_1
          # partner); else FALSE (we verified that no inv_1 tree matched).
          ifelse(.data$is_unmatchable_2nd, NA, FALSE)
        ),
        tree_exists_2nd = ifelse(
          !is.na(.data$tree_exists_2nd),
          .data$tree_exists_2nd,
          ifelse(.data$is_unmatchable_1st, NA, FALSE)
        )
      )
  }
  else { # tree_id_style == "generic"
    # Generic style: the join key was tree_id (unsuffixed after the join).
    # Copy it into tree_id_1st / tree_id_2nd and mask to NA on the side
    # the row does not exist. Same NA-semantics treatment of
    # tree_exists_* as in the baysf branch.
    rslt <- rslt |>
      dplyr::rename(tree_id_1st = "tree_id") |>
      dplyr::mutate(tree_id_2nd = .data$tree_id_1st) |>
      dplyr::mutate(
        is_unmatchable_1st = ifelse(
          !is.na(.data$is_unmatchable_1st), .data$is_unmatchable_1st, FALSE
        ),
        is_unmatchable_2nd = ifelse(
          !is.na(.data$is_unmatchable_2nd), .data$is_unmatchable_2nd, FALSE
        ),
        tree_exists_1st = ifelse(
          !is.na(.data$tree_exists_1st),
          .data$tree_exists_1st,
          ifelse(.data$is_unmatchable_2nd, NA, FALSE)
        ),
        tree_exists_2nd = ifelse(
          !is.na(.data$tree_exists_2nd),
          .data$tree_exists_2nd,
          ifelse(.data$is_unmatchable_1st, NA, FALSE)
        ),
        # Mask tree_id to NA on the side(s) where the row carries no data.
        # %in% TRUE keeps NA tree_exists as "not present", which matches
        # the semantics of the unmatchable case as well — the row truly
        # has no tree_id on that side.
        tree_id_1st = ifelse(
          .data$tree_exists_1st %in% TRUE, .data$tree_id_1st, NA
        ),
        tree_id_2nd = ifelse(
          .data$tree_exists_2nd %in% TRUE, .data$tree_id_2nd, NA
        )
      )
  }

  # This might seem redundant, but I want to nail the information that a tree
  # that exists in the second survey independently from a variable name that
  # exists at other places
  rslt$tree_removal_2nd <- rslt$removal_2nd

  # For the tree-level increment calculation (done in another function)
  # we need to make sure that the column time_yr is never NA. Therefore, we
  # take the information from the plot object's slot time_yr
  rslt$time_yr_1st <- plot_in_inv_1st$time_yr
  rslt$time_yr_2nd <- plot_in_inv_2nd$time_yr

  # Final output column selection — includes the two unmatchable flags so
  # they travel through the rest of the pipeline.
  rslt <- rslt |>
    dplyr::select(
      "tree_id_1st", "tree_id_2nd",
      "tree_exists_1st", "tree_exists_2nd",
      "is_unmatchable_1st", "is_unmatchable_2nd",
      "tree_removal_2nd", "time_yr_1st", "time_yr_2nd"
    )

  return(rslt)
}


#' Internal Helper Function for Calculating Tree Volumes at Sampling Thresholds
#'
#' Required for all tree-increment calculation options in
#' \code{\link{tree_inc_repsurv}}, where sampling thresholds have to be taken
#' into account.
#'
#' @keywords internal
calc_v_hub_m3_at_thrsh <- function(
    tree_matches, inv_1st, inv_2nd, inv_1st_trees, inv_2nd_trees
) {
  # Lookup: plot_id -> sorted dbh_lower from circle_definition of 2nd survey
  thrsh_lookup <- tibble::tibble(
    plot_id   = inv_2nd$plot_id,
    dbh_lower = purrr::map(inv_2nd$plot, function(p) {
      sort(p$circle_definition$dbh_lower)
    })
  )

  # Ingrowth: threshold = dbh_lower of the circle the tree is on in 2nd survey
  get_thrsh_ingrowth <- function(plot_id, dbh_2nd) {
    thr <- thrsh_lookup$dbh_lower[thrsh_lookup$plot_id == plot_id][[1]]
    thr[findInterval(dbh_2nd, thr)]
  }

  # Circle-transition: threshold = smallest dbh_lower strictly above dbh_cm_1st
  get_thrsh_circle_tr <- function(plot_id, dbh_1st) {
    thr <- thrsh_lookup$dbh_lower[thrsh_lookup$plot_id == plot_id][[1]]
    candidates <- thr[thr > dbh_1st]
    if (length(candidates) == 0) NA_real_ else min(candidates)
  }

  tree_info_1st <- inv_1st_trees |>
    dplyr::select("plot_id", "tree_id", "species_id", "dbh_cm", "height_m")
  tree_info_2nd <- inv_2nd_trees |>
    dplyr::select("plot_id", "tree_id", "species_id", "dbh_cm", "height_m")

  tree_matches <- tree_matches |>
    dplyr::left_join(
      tree_info_1st,
      by = c("plot_id_1st" = "plot_id", "tree_id_1st" = "tree_id")
    ) |>
    dplyr::rename(
      dbh_cm_1st   = "dbh_cm",
      height_m_1st = "height_m"
    ) |>
    dplyr::left_join(
      tree_info_2nd,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id"),
      suffix = c("", "_2nd")
    ) |>
    dplyr::rename(
      dbh_cm_2nd   = "dbh_cm",
      height_m_2nd = "height_m"
    )

  species_id_eff <- dplyr::coalesce(
    tree_matches$species_id_2nd,
    tree_matches$species_id
  )

  # %in% TRUE / FALSE patterns: NA in tree_exists_* (= unmatchable rows
  # whose counterpart is unknown by design) is treated as not-matching, so
  # is_ingrowth and both_exist are strict booleans without NA — safe for
  # logical indexing further down.
  is_ingrowth  <- tree_matches$tree_exists_1st %in% FALSE &
                  tree_matches$tree_exists_2nd %in% TRUE

  # Detect circle transitions via dbh and circle definition thresholds
  # (findInterval on dbh_lower) rather than n_rep_ha comparison. This avoids
  # false positives from slope differences and correctly identifies trees
  # that are already in the outermost circle (no transition possible).
  both_exist <- tree_matches$tree_exists_1st %in% TRUE &
                tree_matches$tree_exists_2nd %in% TRUE
  is_circle_tr <- rep(FALSE, nrow(tree_matches))
  if (any(both_exist)) {
    is_circle_tr[both_exist] <- mapply(
      function(plot_id, dbh_1st, dbh_2nd) {
        thr <- thrsh_lookup$dbh_lower[thrsh_lookup$plot_id == plot_id][[1]]
        findInterval(dbh_2nd, thr) > findInterval(dbh_1st, thr)
      },
      plot_id = tree_matches$plot_id_2nd[both_exist],
      dbh_1st = tree_matches$dbh_cm_1st[both_exist],
      dbh_2nd = tree_matches$dbh_cm_2nd[both_exist]
    )
  }

  dbh_thrsh <- rep(NA_real_, nrow(tree_matches))
  if (any(is_ingrowth))
    dbh_thrsh[is_ingrowth] <- mapply(
      get_thrsh_ingrowth,
      plot_id = tree_matches$plot_id_2nd[is_ingrowth],
      dbh_2nd = tree_matches$dbh_cm_2nd[is_ingrowth]
    )
  if (any(is_circle_tr))
    dbh_thrsh[is_circle_tr] <- mapply(
      get_thrsh_circle_tr,
      plot_id = tree_matches$plot_id_2nd[is_circle_tr],
      dbh_1st = tree_matches$dbh_cm_1st[is_circle_tr]
    )

  height_thrsh <- rep(NA_real_, nrow(tree_matches))
  if (any(is_ingrowth))
    height_thrsh[is_ingrowth] <-
      dbh_thrsh[is_ingrowth] *
      tree_matches$height_m_2nd[is_ingrowth] /
      tree_matches$dbh_cm_2nd[is_ingrowth]
  if (any(is_circle_tr)) {
    d1 <- tree_matches$dbh_cm_1st[is_circle_tr]
    d2 <- tree_matches$dbh_cm_2nd[is_circle_tr]
    h1 <- tree_matches$height_m_1st[is_circle_tr]
    h2 <- tree_matches$height_m_2nd[is_circle_tr]
    dt <- dbh_thrsh[is_circle_tr]
    height_thrsh[is_circle_tr] <- h1 + (dt - d1) / (d2 - d1) * (h2 - h1)
  }

  needs_thrsh <- is_ingrowth | is_circle_tr
  # Exclude cases where no threshold could be determined (e.g. circle

  # transition trees whose dbh_1st already exceeds all dbh_lower thresholds).
  # These trees keep v_hub_m3_at_thrsh = NA and are handled in tree_inc_repsurv.
  has_valid_thrsh <- needs_thrsh & !is.na(dbh_thrsh) & !is.na(species_id_eff)

  v_m3_at_thrsh <- rep(NA_real_, nrow(tree_matches))
  if (any(has_valid_thrsh))
    v_m3_at_thrsh[has_valid_thrsh] <- ForestElementsR::v_gri(
      species_id_eff[has_valid_thrsh],
      dbh_thrsh[has_valid_thrsh],
      height_thrsh[has_valid_thrsh]
    )

  v_hub_m3_at_thrsh <- rep(NA_real_, nrow(tree_matches))
  if (any(has_valid_thrsh))
    v_hub_m3_at_thrsh[has_valid_thrsh] <- ForestElementsR::v_red_harvest_ubark(
      species_id_eff[has_valid_thrsh], v_m3_at_thrsh[has_valid_thrsh]
    )

  tree_matches |>
    dplyr::select(
      -"dbh_cm_1st", -"height_m_1st", -"dbh_cm_2nd", -"height_m_2nd",
      -dplyr::any_of(c("species_id", "species_id_2nd"))
    ) |>
    dplyr::mutate(v_hub_m3_at_thrsh = v_hub_m3_at_thrsh)
}


#' Match Trees on Subsequent Inventories on Inventory Level
#'
#' Typically called from within \code{\link{inv_increment_repsurv_ccirc}}. The
#' consistency of the input to this function is not checked here. In essence,
#' the function wraps serial calls of
#' \code{\link{match_trees_on_plot_repsurv_ccirc}}.
#'
#' @param inv_point_matches Output of \code{\link{match_2_inventories}}
#'   (typical) or \code{\link{match_2_inventories_by_center_coord}}, or
#'   \code{\link{match_2_inventories_by_plot_id}}
#'
#' @param inv_1st \code{\link{fe_inventory}} object representing the earlier of
#'   the two inventories. All plots of this inventory must have the class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}.
#'
#' @param inv_2nd \code{\link{fe_inventory}} object representing the later of
#'   the two inventories. All plots of this inventory must have the class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}.
#'
#' @param inv_1st_trees A data frame resulting from applying the functions
#'   \code{\link{pull_trees}}, \code{\link{height_complete_inventory}}, and
#'   \code{\link{trees_add_essentials}} to \code{inv_1st}. While this could be
#'   done internally inside this function, this has typically happened earlier
#'   already in an inventory evaluation workflow. As this is a considerably
#'   time-consuming process, we deem the redundancy acceptable.
#'
#' @param inv_2nd_trees A data frame resulting from applying the functions
#'   \code{\link{pull_trees}}, \code{\link{height_complete_inventory}}, and
#'   \code{\link{trees_add_essentials}} to \code{inv_2nd}. While this could be
#'   done internally inside this function, this has typically happened earlier
#'   already in an inventory evaluation workflow. As this is a considerably
#'   time-consuming process, we deem the redundancy acceptable.
#'
#' @param tree_id_style Character string that indicates how trees are uniquely
#'   identified on inventory plot level. This is required for matching trees
#'   on the same plot in both inventories. The options are "baysf" (default)
#'   and "generic". In the BaySF case, trees are matched by their polar
#'   coordinates, while in the generic case, the tree_ids must mark the same
#'   trees in both inventories.
#'
#' @param inner_circle_non_matchable Logical, default \code{FALSE}. Passed
#'   through to \code{\link{match_trees_on_plot_repsurv_ccirc}}. See its
#'   documentation for the full meaning.
#'
#' @param shrinkage_rate_per_decade Numeric, the maximum plausible volume
#'   shrinkage rate per decade. Default is \code{0.10}. See
#'   \code{\link{inv_increment_repsurv_ccirc}} for details.
#'
#' @param species_check Character string controlling species consistency
#'   checks. Options: \code{"species_group"} (default), \code{"species"},
#'   \code{"warn_only"}. See \code{\link{inv_increment_repsurv_ccirc}} for
#'   details.
#'
#' @param progress_bar Boolean, if \code{TRUE} (default), a progress bar is
#'   shown during time critical steps of the execution.
#'
#' @returns A data frame identifying all matching trees with their tree and
#'   plot ids in both inventories, the calendar years of both inventories,
#'   and columns with flags that indicate in which of both inventories
#'   a tree exists (can be only one of them or both). Additionally contains
#'   the columns \code{volume_change_rate_per_decade} (the observed volume
#'   change rate scaled to a decade, based on an exponential model; positive
#'   values indicate growth, negative values indicate shrinkage),
#'   \code{volume_change_implausible} (logical, \code{TRUE} if shrinkage
#'   exceeds the threshold), and \code{species_mismatch} (logical,
#'   \code{TRUE} if species identity changed between surveys according to
#'   the \code{species_check} setting).
#'
#' @family increment
#' @family repeated inventory
#' @family inventory matches
#'
#' @export
#'
#' @examples
#'   # Prepare everything required
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # Match inventory points
#'   inv_pt_mtch <- match_2_inventories(inv_a, inv_b, "plot_id", "baysf")
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # Finally, make the match
#'   match_trees_on_inventory_repsurv_ccirc(
#'     inv_pt_mtch, inv_a, inv_b, inv_a_trees, inv_b_trees,
#'     tree_id_style = "baysf"
#'   )
#'
match_trees_on_inventory_repsurv_ccirc <- function(
    inv_point_matches, inv_1st, inv_2nd, inv_1st_trees, inv_2nd_trees,
    tree_id_style = c("baysf", "generic"),
    inner_circle_non_matchable = FALSE,
    shrinkage_rate_per_decade = 0.10,
    species_check = c("species_group", "species", "warn_only"),
    progress_bar = TRUE
) {

  stopifnot(is_fe_inventory(inv_1st))
  stopifnot(is_fe_inventory(inv_2nd))

  tree_id_style <- match.arg(tree_id_style)
  species_check <- match.arg(species_check)

  # Run through all matching points and match the trees there
  ## Find the positions of the matching plots in both fe_inventory objects
  index_1st <- match(
    inv_point_matches$matches$plot_id_inv_a, inv_1st$plot_id
  )
  index_2nd <- match(
    inv_point_matches$matches$plot_id_inv_b, inv_2nd$plot_id
  )

  ## Arrange the information required for the next steps
  plot_1st    <- inv_1st[index_1st, ]$plot
  plot_2nd    <- inv_2nd[index_2nd, ]$plot
  plot_id_1st <- inv_1st[index_1st, ]$plot_id
  plot_id_2nd <- inv_2nd[index_2nd, ]$plot_id

  ## Match the trees on plot level first, then combine everything into a
  ## data frame
  tree_matches <- purrr::pmap(
    list(
      plot_1st, plot_2nd, plot_id_1st, plot_id_2nd
    ),
    .f = function(
      plot_1st, plot_2nd, plot_id_1st, plot_id_2nd,
      tree_id_style, inner_circle_non_matchable
    ) {
        rslt <- match_trees_on_plot_repsurv_ccirc(
          plot_1st, plot_2nd, tree_id_style,
          inner_circle_non_matchable = inner_circle_non_matchable
        )
        if (!is.null(rslt)) { # Null, when two plots without trees are matched
           rslt <- rslt |> dplyr::mutate(
             plot_id_1st = plot_id_1st, plot_id_2nd = plot_id_2nd
           )
        }
        return(rslt)
    },
    tree_id_style              = tree_id_style,
    inner_circle_non_matchable = inner_circle_non_matchable,
    .progress = progress_bar
  )
  ## Remove NULL entries (occur when two plots with no trees came together)
  ## from tree_matches
  tree_matches <- Filter(Negate(is.null), tree_matches)
  ## Combine tree_matches into one data frame
  tree_matches <- tree_matches |> purrr::list_rbind()


  # Get the tree-level information for calculating volume increment and
  # consolidate it
  ## Reduce both tree data frames to the necessary columns
  ## Not necessary to get time_yr here, because this is complete in tree_matches
  ## and not required for the joins below
  inv_1st_trees_red <- inv_1st_trees |>
    dplyr::select("plot_id", "tree_id", "v_hub_m3", "n_rep_ha")
  inv_2nd_trees_red <- inv_2nd_trees |>
    dplyr::select("plot_id", "tree_id", "v_hub_m3", "n_rep_ha")

  tree_matches_1st <- tree_matches |>
    dplyr::left_join(
      inv_1st_trees_red,
      by = c("plot_id_1st" = "plot_id", "tree_id_1st" = "tree_id")
    )
  tree_matches_2nd <- tree_matches |>
    dplyr::left_join(
      inv_2nd_trees_red,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    )

  tree_matches <- dplyr::full_join(
    tree_matches_1st, tree_matches_2nd,
    by = c(
      "tree_id_1st", "tree_id_2nd", "tree_exists_1st", "tree_exists_2nd",
      "tree_removal_2nd",
      # is_unmatchable_1st / _2nd are part of the per-plot match output
      # and travel through the inventory-level merge unchanged — they
      # belong to the BY columns so dplyr does not suffix them.
      "is_unmatchable_1st", "is_unmatchable_2nd",
      "plot_id_1st", "plot_id_2nd", "time_yr_1st", "time_yr_2nd"
    ),
    suffix = c("_1st", "_2nd")
  )

  tree_matches <- calc_v_hub_m3_at_thrsh(
    tree_matches, inv_1st, inv_2nd, inv_1st_trees, inv_2nd_trees
  )

  # --- Plausibility checks for re-measured trees ---
  # Only trees present in both inventories can be implausible. Use
  # `%in% TRUE` so unmatchable rows (NA tree_exists_*) are excluded.
  is_remeasured <- tree_matches$tree_exists_1st %in% TRUE &
                   tree_matches$tree_exists_2nd %in% TRUE

  # Volume change rate per decade (exponential model)
  # rate = (v2/v1)^(10 / interval) - 1, negative means shrinkage
  interval_yr <- tree_matches$time_yr_2nd - tree_matches$time_yr_1st
  v_ratio <- ifelse(
    is_remeasured & tree_matches$v_hub_m3_1st > 0,
    tree_matches$v_hub_m3_2nd / tree_matches$v_hub_m3_1st,
    NA_real_
  )
  tree_matches$volume_change_rate_per_decade <- ifelse(
    is_remeasured & !is.na(v_ratio),
    v_ratio ^ (10 / interval_yr) - 1,
    NA_real_
  )

  # Volume change implausibility: shrinkage rate exceeds threshold
  tree_matches$volume_change_implausible <- ifelse(
    is_remeasured,
    !is.na(tree_matches$volume_change_rate_per_decade) &
      tree_matches$volume_change_rate_per_decade < - shrinkage_rate_per_decade,
    FALSE
  )

  # Species mismatch check: join species info separately to avoid column name
  # conflicts with calc_v_hub_m3_at_thrsh() which also joins species_id
  sp_1st <- inv_1st_trees |>
    dplyr::select("plot_id", "tree_id", "species_id", "species_group")
  sp_2nd <- inv_2nd_trees |>
    dplyr::select("plot_id", "tree_id", "species_id", "species_group")

  sp_check <- tree_matches |>
    dplyr::select("plot_id_1st", "tree_id_1st", "plot_id_2nd", "tree_id_2nd") |>
    dplyr::left_join(
      sp_1st, by = c("plot_id_1st" = "plot_id", "tree_id_1st" = "tree_id")
    ) |>
    dplyr::rename(species_id_1st = "species_id",
                  species_group_1st = "species_group") |>
    dplyr::left_join(
      sp_2nd, by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    ) |>
    dplyr::rename(species_id_2nd = "species_id",
                  species_group_2nd = "species_group")

  if (species_check == "species") {
    tree_matches$species_mismatch <- ifelse(
      is_remeasured,
      sp_check$species_id_1st != sp_check$species_id_2nd,
      FALSE
    )
  } else if (species_check == "species_group") {
    tree_matches$species_mismatch <- ifelse(
      is_remeasured,
      sp_check$species_group_1st != sp_check$species_group_2nd,
      FALSE
    )
  } else { # warn_only
    tree_matches$species_mismatch <- FALSE
    n_sp_diff <- sum(
      is_remeasured &
        sp_check$species_id_1st != sp_check$species_id_2nd,
      na.rm = TRUE
    )
    if (n_sp_diff > 0) {
      message(
        n_sp_diff, " re-measured tree(s) with different species codes ",
        "in both inventories."
      )
    }
  }

  # Report implausible trees
  n_vol_impl <- sum(tree_matches$volume_change_implausible, na.rm = TRUE)
  n_sp_impl  <- sum(tree_matches$species_mismatch, na.rm = TRUE)
  n_any_impl <- sum(
    tree_matches$volume_change_implausible | tree_matches$species_mismatch,
    na.rm = TRUE
  )
  if (n_any_impl > 0) {
    message(
      n_any_impl, " re-measured tree(s) flagged as implausible (",
      n_vol_impl, " volume change, ", n_sp_impl, " species mismatch)."
    )
  }

  tree_matches
}


#' Provide an Overview of the Tree Matching Results on Inventory Level
#'
#' Typically called from within \code{\link{inv_increment_repsurv_ccirc}}.
#' The returned vector summarises the matching outcome from the perspective
#' of both inventories and breaks the matched-trees population into the
#' status categories that the rest of the pipeline distinguishes — so users
#' can see at a glance how many trees were unambiguously re-measured, how
#' many were verified ingrowth / disappearances, how many fall into the
#' unmatchable inner-circle cohort, and how many were reclassified as
#' pseudo-ingrowth by \code{\link{reclassify_pseudo_ingrowth_ccirc}}.
#'
#' @param tree_matches Data frame, output of
#'   \code{\link{match_trees_on_inventory_repsurv_ccirc}}, optionally with
#'   the \code{pseudo_ingrowth_corrected} column added by
#'   \code{\link{reclassify_pseudo_ingrowth_ccirc}}.
#'
#' @param inv_1st_trees A data frame resulting from applying the functions
#'   \code{\link{pull_trees}}, \code{\link{height_complete_inventory}}, and
#'   \code{\link{trees_add_essentials}} to the first of the two
#'   \code{\link{fe_inventory}} objects \code{tree_matches} relates to.
#'
#' @param inv_2nd_trees A data frame resulting from applying the functions
#'   \code{\link{pull_trees}}, \code{\link{height_complete_inventory}}, and
#'   \code{\link{trees_add_essentials}} to the second of the two
#'   \code{\link{fe_inventory}} objects \code{tree_matches} relates to.
#'
#' @returns A named numeric vector with the following entries:
#'   \describe{
#'     \item{n_total_1st}{Total number of trees in \code{inv_1st_trees}.}
#'     \item{n_in_match_1st}{Trees of the 1st survey that appear in
#'       \code{tree_matches} (i.e. their plot is part of the matched plot
#'       set).}
#'     \item{n_in_both_1st}{Trees of the 1st survey that were unambiguously
#'       re-measured: rows with \code{tree_exists_1st} and
#'       \code{tree_exists_2nd} both \code{TRUE} that carry an identified
#'       inv-1 record (\code{tree_id_1st} not \code{NA}).}
#'     \item{n_total_2nd}{Total number of trees in \code{inv_2nd_trees}.}
#'     \item{n_in_match_2nd}{Trees of the 2nd survey that appear in
#'       \code{tree_matches}. Counted from the inv-2 side and therefore
#'       \emph{not} the same population as \code{n_in_match_1st}: 1st-only
#'       rows are absent here and 2nd-only rows are absent there, so the two
#'       numbers routinely differ.}
#'     \item{n_in_both_2nd}{The inv-2 side of the re-measured trees. This is
#'       \strong{not} a mirror of \code{n_in_both_1st} and the two are
#'       \emph{expected} to differ as soon as pseudo-ingrowth occurs:
#'       \code{\link{reclassify_pseudo_ingrowth_ccirc}} sets
#'       \code{tree_exists_1st = TRUE} on 2nd-only rows to record that the
#'       tree did exist at inv 1, but it leaves \code{tree_id_1st} at
#'       \code{NA}, because no individual inv-1 record was identified (that
#'       is precisely why the tree was unmatchable). Such rows therefore
#'       count towards \code{n_in_both_2nd} but cannot count towards
#'       \code{n_in_both_1st}, and
#'       \code{n_in_both_2nd - n_in_both_1st = n_pseudo_ingrowth} holds by
#'       construction. A difference of exactly that size is correct
#'       behaviour, not a matching defect.}
#'     \item{n_unmatchable_1st}{Inv-1 trees that carry a measured value but
#'       could not be matched to inv 2 because their matching key was
#'       unreliable (typically inner-circle trees recorded without polar
#'       coordinates). They show up in \code{tree_matches} as 1st-only
#'       rows with \code{tree_exists_2nd = NA} and
#'       \code{is_unmatchable_1st = TRUE}.}
#'     \item{n_unmatchable_2nd}{Inv-2 counterpart: 2nd-only rows with
#'       \code{tree_exists_1st = NA} and \code{is_unmatchable_2nd = TRUE}.
#'       Trees reclassified by
#'       \code{\link{reclassify_pseudo_ingrowth_ccirc}} also carry the
#'       \code{is_unmatchable_2nd} flag and are included in this count.}
#'     \item{n_verified_ingrowth}{2nd-only rows that the inv 2 match pool
#'       saw and verified to have no inv 1 partner (genuine new trees
#'       above the smallest sampling threshold).}
#'     \item{n_verified_removal}{1st-only rows with verified absence at
#'       inv 2 (\code{tree_exists_2nd \%in\% FALSE}, i.e. matchable inv 1
#'       record but no inv 2 partner found).}
#'     \item{n_pseudo_ingrowth}{Number of 2nd-only inv 2 records that
#'       \code{\link{reclassify_pseudo_ingrowth_ccirc}} reclassified as
#'       pseudo-ingrowth (inner-circle inv 1 origin reconstructed via
#'       gnfi3 backward). 0 when the reclassification step is absent or
#'       did not fire.}
#'   }
#'
#' @keywords internal
#'
#'
match_tree_statistics <- function(tree_matches, inv_1st_trees, inv_2nd_trees) {

  n_total_1st    <- nrow(inv_1st_trees)

  # n_in_match: trees of inv_*_trees whose plot is part of the matched
  # plot set (i.e. they have a row in tree_matches on the respective
  # side). To keep the counts aligned with inv_*_trees we restrict to rows
  # with a non-NA tree_id_* on the respective side.
  #
  # This is also why the 1st- and 2nd-side counts are not mirror images.
  # reclassify_pseudo_ingrowth_ccirc() sets tree_exists_1st = TRUE on
  # 2nd-only rows (the tree did exist at inv 1) but leaves tree_id_1st at
  # NA - it never had an identified inv_1 record, which is exactly why it
  # was unmatchable. Those rows are therefore counted on the inv_2 side
  # only, and n_in_both_2nd - n_in_both_1st = n_pseudo_ingrowth.
  n_in_match_1st <- tree_matches |>
    dplyr::filter(!is.na(.data$tree_id_1st)) |>
    dplyr::inner_join(
      inv_1st_trees,
      by = c("plot_id_1st" = "plot_id", "tree_id_1st" = "tree_id")
    ) |>
    nrow()

  n_in_both_1st <- tree_matches |>
    dplyr::filter(
      .data$tree_exists_1st %in% TRUE & .data$tree_exists_2nd %in% TRUE &
        !is.na(.data$tree_id_1st)
    ) |>
    dplyr::inner_join(
      inv_1st_trees,
      by = c("plot_id_1st" = "plot_id", "tree_id_1st" = "tree_id")
    ) |>
    nrow()

  n_total_2nd <- nrow(inv_2nd_trees)

  n_in_match_2nd <- tree_matches |>
    dplyr::filter(!is.na(.data$tree_id_2nd)) |>
    dplyr::inner_join(
      inv_2nd_trees,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    ) |>
    nrow()

  n_in_both_2nd <- tree_matches |>
    dplyr::filter(
      .data$tree_exists_1st %in% TRUE & .data$tree_exists_2nd %in% TRUE &
        !is.na(.data$tree_id_2nd)
    ) |>
    dplyr::inner_join(
      inv_2nd_trees,
      by = c("plot_id_2nd" = "plot_id", "tree_id_2nd" = "tree_id")
    ) |>
    nrow()

  # Status-category counts. The %in% TRUE / %in% FALSE pattern is
  # NA-safe so unmatchable rows are correctly classified.
  n_unmatchable_1st <- sum(tree_matches$is_unmatchable_1st %in% TRUE)
  n_unmatchable_2nd <- sum(tree_matches$is_unmatchable_2nd %in% TRUE)
  n_verified_ingrowth <- sum(
    tree_matches$tree_exists_1st %in% FALSE &
      tree_matches$tree_exists_2nd %in% TRUE
  )
  n_verified_removal <- sum(
    tree_matches$tree_exists_1st %in% TRUE &
      tree_matches$tree_exists_2nd %in% FALSE
  )
  n_pseudo_ingrowth <- if ("pseudo_ingrowth_corrected" %in% names(tree_matches)) {
    sum(tree_matches$pseudo_ingrowth_corrected %in% TRUE)
  } else {
    0L
  }

  c(
    "n_total_1st"         = n_total_1st,
    "n_in_match_1st"      = n_in_match_1st,
    "n_in_both_1st"       = n_in_both_1st,
    "n_total_2nd"         = n_total_2nd,
    "n_in_match_2nd"      = n_in_match_2nd,
    "n_in_both_2nd"       = n_in_both_2nd,
    "n_unmatchable_1st"   = n_unmatchable_1st,
    "n_unmatchable_2nd"   = n_unmatchable_2nd,
    "n_verified_ingrowth" = n_verified_ingrowth,
    "n_verified_removal"  = n_verified_removal,
    "n_pseudo_ingrowth"   = n_pseudo_ingrowth
  )
}




#' Calculate the Volume Increment of the Single Trees in a Repeated Inventory
#' with a Concentric Sampling Circle Design
#'
#' Provided for convenience, but typically not called directly but from within
#' the function \code{\link{inv_increment_repsurv_ccirc}}.
#'
#' Even though the volume increments are calculated on tree level here, they are
#' upscaled to 1 ha. This is important, because otherwise the increment
#' represented by trees that change their representation number per ha between
#' both inventories cannot be calculated correctly. Trees present in the first
#' survey only always get zero increment; such values can be replaced by
#' model-based estimates in a subsequent step. The function allows to choose
#' between four methods of increment calculation (parameter \code{method}):
#' \itemize{
#'   \item \code{rep_classic}: The volume increment, \eqn{iv}, of trees that are
#'     present in both inventories is calculated as
#'     \eqn{iv=n[2]\cdot v[2]-n[1] \cdot v[1]}, with \eqn{n} and \eqn{v} being
#'     the representation number per hectare and the volume of the tree at the
#'     first and the second survey, respectively. The volume increment of the
#'     ingrowth trees is \eqn{iv=n[2]*v[2]}, i.e. such trees contribute to the
#'     increment with their full volume. This method is equivalent to the
#'     classic stand level increment calculation (German "Ertragsgeschichtlicher
#'     Zuwachs"), however, some trees (and plots) may also have negative
#'     increments. This might look strange, but is fully valid from a
#'     statistical angle of view.
#'   \item \code{rep_mean}: The volume increment, \eqn{iv}, of trees that are
#'     present in both inventories is calculated as
#'     \eqn{iv=(n[2]+n[1])/2\cdot (v[2]-v[1])}, and the volume increment of
#'     ingrowth trees is calculated as \eqn{iv=n[2]*(v[2]-v_t)}, where
#'     \eqn{v_t} is the tree's volume when it crossed the sampling
#'     threshold. This method is not compatible to the classic stand level
#'     increment calculation. Guarantees \eqn{iv>=0} when
#'     \code{clamp_plausible_shrinkage = TRUE}.
#'   \item \code{rep_end}: The volume increment, \eqn{iv}, of trees that are
#'     present in both inventories is calculated as
#'     \eqn{iv=n[2]\cdot (v[2]-v[1])}, and the volume increment of
#'     ingrowth trees is calculated as \eqn{iv=n[2]*(v[2]-v_t)}, where
#'     \eqn{v_t} is the tree's volume when it crossed the sampling
#'     threshold. This method is not compatible to the classic stand level
#'     increment calculation. Guarantees \eqn{iv>=0} when
#'     \code{clamp_plausible_shrinkage = TRUE}.
#'   \item \code{rep_trans}: The volume increment, \eqn{iv}, of trees that are
#'     present in both inventories, and did not cross a sampling threshold,
#'     is calculated as \eqn{iv=n\cdot (v[2]-v[1])}, with \eqn{n=n[1]=n[2]}.
#'     For trees that crossed a sampling threshold, the calculation is
#'     \eqn{iv=n[2]\cdot (v[2]-v_t) + n[1]\cdot (v_t-v[1])}.
#'     The volume increment of ingrowth trees is calculated as
#'     \eqn{iv=n[2]*(v[2]-v_t)}, where \eqn{v_t} is the tree's volume when it
#'     crossed the sampling threshold. This method is not compatible to the
#'     classic stand level increment calculation and always guarantees
#'     \eqn{iv>=0} (negative volume differences are clamped to zero regardless
#'     of \code{clamp_plausible_shrinkage}).
#' }
#'
#' @param matched_trees A data frame being the output of
#'   \code{\link{match_trees_on_inventory_repsurv_ccirc}}.
#'
#' @param method Character string, choices are \code{"rep_classic"} (default),
#'   \code{"rep_mean"}, \code{"rep_end"}, and \code{"rep_trans"}. These methods
#'   differ in how sampling thresholds are handled (see Details.)
#'
#' @param clamp_plausible_shrinkage Logical. If \code{TRUE} (default),
#'   re-measured trees with plausible volume shrinkage (below the
#'   implausibility threshold set in
#'   \code{\link{match_trees_on_inventory_repsurv_ccirc}}) receive a zero
#'   increment. If \code{FALSE}, plausible shrinkage is passed through to the
#'   increment for \code{rep_classic}, \code{rep_mean}, and \code{rep_end}.
#'   Has no effect on \code{rep_trans}, which always clamps negative volume
#'   differences to zero (a warning is issued in that case). Trees flagged as
#'   implausible always receive a zero increment regardless of this setting.
#'
#' @returns The input data frame with two additional columns:
#'   \code{iv_hub_m3_ha_per_rep} and \code{iv_hub_m3_ha_yr_rep}. They represent
#'   the periodic and the (mean) annual tree volume increment in m³/ha under
#'   bark and after harvest.
#'
#' @export
#'
#' @examples
#'   # When called directly, considerable preparation is required
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # Match inventory points
#'   inv_pt_mtch <- match_2_inventories(inv_a, inv_b, "plot_id", "baysf")
#'
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # Now, match both inventories on tree level
#'   mtch_trs <- match_trees_on_inventory_repsurv_ccirc(
#'     inv_pt_mtch, inv_a, inv_b, inv_a_trees, inv_b_trees,
#'     tree_id_style = "baysf"
#'   )
#'
#'   # Finally, calculate the increments
#'   tree_inc_repsurv(mtch_trs, method = "rep_classic")
#'   tree_inc_repsurv(mtch_trs, method = "rep_mean")
#'   tree_inc_repsurv(mtch_trs, method = "rep_end")
#'   tree_inc_repsurv(mtch_trs, method = "rep_trans")
#'
#' @family increment
tree_inc_repsurv <- function(
    matched_trees,
    method = c("rep_classic", "rep_mean", "rep_end", "rep_trans"),
    clamp_plausible_shrinkage = TRUE
) {

  method <- match.arg(method)

  if (!clamp_plausible_shrinkage && method == "rep_trans") {
    warning(
      "method 'rep_trans' always clamps negative volume differences to zero; ",
      "clamp_plausible_shrinkage = FALSE has no effect for this method."
    )
  }

  # Trees flagged as implausible get zero increment regardless of method
  is_implausible <- matched_trees$volume_change_implausible |
    matched_trees$species_mismatch

  if (method == "rep_classic") {
    incr <- matched_trees |>
      dplyr::mutate(
        iv_hub_m3_ha_per_rep = dplyr::case_when(
          # Implausible trees get zero, to be replaced by model estimates
          is_implausible ~ 0,
          # Regular case, tree present at both surveys
          (.data$tree_exists_1st & .data$tree_exists_2nd) ~
            ifelse(
              clamp_plausible_shrinkage &
                .data$v_hub_m3_2nd < .data$v_hub_m3_1st,
              0,
              .data$n_rep_ha_2nd * .data$v_hub_m3_2nd -
                .data$n_rep_ha_1st * .data$v_hub_m3_1st
            ),
          # Tree present at first survey only
          ## Gets zero increment, can be replaced by model estimates later
          (.data$tree_exists_1st & !.data$tree_exists_2nd) ~ 0,
          # Tree present at second survey only (= ingrowth)
          ## whole Volume is taken as the increment
          (!.data$tree_exists_1st & .data$tree_exists_2nd) ~
            .data$n_rep_ha_2nd * .data$v_hub_m3_2nd,

          # Default case (should never happen)
          TRUE ~ NA_real_
        ),
        iv_hub_m3_ha_yr_rep =
          .data$iv_hub_m3_ha_per_rep / (.data$time_yr_2nd - .data$time_yr_1st)
      )
  } # end: method == "rep_classic"

  else if (method == "rep_mean") {
    incr <- matched_trees |>
      dplyr::mutate(
        iv_hub_m3_ha_per_rep = dplyr::case_when(
          # Implausible trees get zero, to be replaced by model estimates
          is_implausible ~ 0,
          # Regular case, tree present at both surveys
          ## The tree's increment is calculated first and weighted with the
          ## average representation number of both surveys.
          (.data$tree_exists_1st & .data$tree_exists_2nd) ~
            ifelse(
              clamp_plausible_shrinkage &
                .data$v_hub_m3_2nd < .data$v_hub_m3_1st,
              0,
              (.data$v_hub_m3_2nd - .data$v_hub_m3_1st) *
                (.data$n_rep_ha_2nd + .data$n_rep_ha_1st) / 2
            ),
          # Tree present at first survey only
          ## Gets zero increment, can be replaced by model estimates later
          (.data$tree_exists_1st & !.data$tree_exists_2nd) ~ 0,
          # Tree present at second survey only (= ingrowth)
          ## Take the increment since threshold crossing only
          (!.data$tree_exists_1st & .data$tree_exists_2nd) ~
            pmax(0, .data$v_hub_m3_2nd - .data$v_hub_m3_at_thrsh) *
              .data$n_rep_ha_2nd,

          # Default case (should never happen)
          TRUE ~ NA_real_
        ),
        iv_hub_m3_ha_yr_rep =
          .data$iv_hub_m3_ha_per_rep / (.data$time_yr_2nd - .data$time_yr_1st)
      )
  } # end: method == "rep_mean"

  else if (method == "rep_end") {
    incr <- matched_trees |>
      dplyr::mutate(
        iv_hub_m3_ha_per_rep = dplyr::case_when(
          # Implausible trees get zero, to be replaced by model estimates
          is_implausible ~ 0,
          # Regular case, tree present at both surveys
          (.data$tree_exists_1st & .data$tree_exists_2nd) ~
            ifelse(
              clamp_plausible_shrinkage &
                .data$v_hub_m3_2nd < .data$v_hub_m3_1st,
              0,
              .data$n_rep_ha_2nd * (.data$v_hub_m3_2nd - .data$v_hub_m3_1st)
            ),
          # Tree present at first survey only
          ## Gets zero increment, can be replaced by model estimates later
          (.data$tree_exists_1st & !.data$tree_exists_2nd) ~ 0,
          # Tree present at second survey only (= ingrowth)
          (!.data$tree_exists_1st & .data$tree_exists_2nd) ~
            pmax(0, .data$v_hub_m3_2nd - .data$v_hub_m3_at_thrsh) *
              .data$n_rep_ha_2nd,

          # Default case (should never happen)
          TRUE ~ NA_real_
        ),
        iv_hub_m3_ha_yr_rep =
          .data$iv_hub_m3_ha_per_rep / (.data$time_yr_2nd - .data$time_yr_1st)
      )
  } # end: method == "rep_end"

  else { # method == "rep_trans"
    incr <- matched_trees |>
      dplyr::mutate(
        iv_hub_m3_ha_per_rep = dplyr::case_when(
          # Implausible trees get zero, to be replaced by model estimates
          is_implausible ~ 0,
          # Re-measured without circle transition (n1 == n2)
          (.data$tree_exists_1st & .data$tree_exists_2nd &
             .data$n_rep_ha_1st == .data$n_rep_ha_2nd) ~
            .data$n_rep_ha_2nd * pmax(0, .data$v_hub_m3_2nd - .data$v_hub_m3_1st),
          # Re-measured with circle transition (n1 > n2)
          (.data$tree_exists_1st & .data$tree_exists_2nd &
             .data$n_rep_ha_1st > .data$n_rep_ha_2nd) ~
            .data$n_rep_ha_2nd * pmax(0, .data$v_hub_m3_2nd - .data$v_hub_m3_at_thrsh) +
            .data$n_rep_ha_1st * pmax(0, .data$v_hub_m3_at_thrsh - .data$v_hub_m3_1st),
          # Re-measured with reverse circle change (n1 < n2, data anomaly)
          # Treated like no circle transition, using n from 2nd survey
          (.data$tree_exists_1st & .data$tree_exists_2nd &
             .data$n_rep_ha_1st < .data$n_rep_ha_2nd) ~
            .data$n_rep_ha_2nd * pmax(0, .data$v_hub_m3_2nd - .data$v_hub_m3_1st),
          # Tree present at first survey only
          ## Gets zero increment, can be replaced by model estimates later
          (.data$tree_exists_1st & !.data$tree_exists_2nd) ~ 0,
          # Tree present at second survey only (= ingrowth)
          (!.data$tree_exists_1st & .data$tree_exists_2nd) ~
            pmax(0, .data$v_hub_m3_2nd - .data$v_hub_m3_at_thrsh) *
              .data$n_rep_ha_2nd,

          # Default case (should never happen)
          TRUE ~ NA_real_
        ),
        iv_hub_m3_ha_yr_rep =
          .data$iv_hub_m3_ha_per_rep / (.data$time_yr_2nd - .data$time_yr_1st)
      )
  } # end: method == "rep_trans"

  # Check for NA increments and issue a warning if there are any
  n_na <- is.na(incr$iv_hub_m3_ha_yr_rep)

  if (sum(n_na) > 0)
    message(fe_msg("na_increments_found", sum(n_na)))

  incr
}









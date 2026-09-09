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

#' Increment From a Repeated Inventory -- the Whole Chain in One Call
#'
#' Calculates the volume increment of a pair of subsequent inventories and
#' returns everything the output functions need. This is the standard entry
#' point for the repeated-survey increment (\dQuote{Verfahren 1}): it runs the
#' six-step chain
#' \code{\link{inv_increment_repsurv_ccirc}} ->
#' \code{\link{inv_inc_fill_gaps_gnfi3}} ->
#' \code{\link{inv_inc_tree_consolidate}} ->
#' \code{\link{inv_inc_tree_extend}} ->
#' \code{\link{inv_inc_tree_extend_combined}} ->
#' \code{\link{inv_inc_big_overview}} in the right order, with the tree lists
#' handed on where they are needed, so that callers do not have to reproduce it.
#'
#' The individual step functions stay exported and are the expert-level
#' interface: use them when a single stage has to be inspected, varied or
#' replaced. For everything else this function is the intended route, and it is
#' the one whose result is guaranteed to be internally consistent.
#'
#' @section Which inventories are admissible:
#' This is a \strong{sample inventory} method and applies only to sample
#' inventories in concentric circles (\emph{Stichprobeninventur}, taxonomy axis
#' \emph{sample} x \emph{concentric}). Both \code{fe_inventory} objects must
#' consist \emph{entirely} of
#' \code{\link[ForestElementsR]{fe_ccircle_spatial}} plots (its \code{_notrees}
#' child included) -- the tree matching relies on the plot geometry and the tree
#' positions those objects carry. Stand-wise inventories (\emph{standwise} x
#' \emph{relascope}, plain \code{fe_stand} plots) carry no tree positions and
#' therefore have no repeated-survey increment; they are rejected with an error
#' by \code{\link{inv_increment_repsurv_ccirc_check_input}}, which runs first.
#' The two surveys must further be a genuine pair: the same enterprise, the
#' first one earlier than the second, at least one year apart.
#'
#' @section Whole-enterprise coverage:
#' The per-tree result covers the whole enterprise, not just the plots measured
#' twice. Plots that exist only in the second survey (new establishments, sample
#' extensions) cannot have a repeated-survey increment by construction, so they
#' receive a backward gnfi3 estimate via
#' \code{\link{inv_inc_tree_extend_combined}}. This matters for more than
#' completeness: the increment base tables express their per-hectare increment
#' over the \emph{full} operation area, so leaving these plots out would put
#' their area into the denominator while their increment is missing from the
#' numerator, and every per-hectare figure would come out too low. With them
#' folded in, the base tables reproduce the enterprise level
#' (\code{\link{inv_inc_big_overview}}'s \code{combined} rows) exactly. Set
#' \code{include_second_only = FALSE} only to reproduce the matched-plots-only
#' view, and be aware of the bias just described.
#'
#' @section Progress reporting:
#' The chain reports its seven phases through the \pkg{progressr} framework.
#' With the default \code{progress_bar = TRUE} the function displays a console
#' bar itself, so an interactive user needs no ceremony. With
#' \code{progress_bar = FALSE} it only \emph{signals} progress and the caller
#' decides what to do with it -- notably a Shiny app, which wraps the call in
#' \code{progressr::withProgressShiny()} and gets its own progress bar without
#' FeNEU knowing anything about Shiny. Always pass \code{progress_bar = FALSE}
#' when supplying a handler yourself, otherwise two handlers compete. The phase
#' labels are translated (\code{options(fe_lang = "de")}).
#'
#' The progress bars of the inner steps are switched off deliberately
#' (\code{\link{inv_increment_repsurv_ccirc}} is called with
#' \code{progress_bar = FALSE}): the tree matching would otherwise draw its own
#' per-plot bar over the phase bar of this function. Progress is reported at
#' this level only.
#'
#' @param inv_1st,inv_2nd The two \code{\link{fe_inventory}} objects, first and
#'   second survey. See \emph{Which inventories are admissible}.
#'
#' @param inv_1st_trees,inv_2nd_trees The matching tree data frames, as pulled
#'   from the two inventories and prepared with the canonical chain
#'   \code{\link{pull_trees}} -> \code{\link{height_complete_inventory}} ->
#'   \code{\link{fill_heights_back}} -> \code{\link{pull_trees}} ->
#'   \code{\link{trees_add_essentials}}.
#'
#' @param method The repeated-survey increment method, one of
#'   \code{"rep_classic"} (default), \code{"rep_mean"}, \code{"rep_end"},
#'   \code{"rep_trans"}. Passed to \code{\link{inv_increment_repsurv_ccirc}}.
#'
#' @param fill_option How the gnfi3 gap-filling estimates are used, one of
#'   \code{"standard"} (default), \code{"min_estimates"}, \code{"all_estimates"}.
#'   Passed to \code{\link{inv_inc_tree_consolidate}}, where the three options
#'   are described. Note that the second-only plots always carry gnfi3 estimates,
#'   whatever this argument says -- they have no measured increment that could be
#'   preferred over an estimate.
#'
#' @param match_type,plot_id_style,tree_id_style,coord_tolerance Plot- and
#'   tree-matching arguments, passed to
#'   \code{\link{inv_increment_repsurv_ccirc}}.
#'
#' @param inner_circle_non_matchable,shrinkage_rate_per_decade,species_check,clamp_plausible_shrinkage
#'   Further arguments of \code{\link{inv_increment_repsurv_ccirc}}, passed
#'   through unchanged.
#'
#' @param include_reptrees Passed to \code{\link{inv_inc_fill_gaps_gnfi3}}.
#'   Default \code{TRUE}, which is required for
#'   \code{fill_option = "all_estimates"}.
#'
#' @param include_second_only Logical, whether plots that exist only in the
#'   second survey are folded in. Default \code{TRUE}; see \emph{Whole-enterprise
#'   coverage}.
#'
#' @param d_q_interval Numeric, width (cm) of the quadratic mean diameter
#'   classes. Passed to \code{\link{inv_inc_tree_extend}}.
#'
#' @param dt_second_only Time span (years, negative = backward) of the gnfi3
#'   estimate for the second-only plots. Default \code{-5}. It reaches both
#'   places that estimate those plots -- the per-tree frame (via
#'   \code{\link{inv_inc_tree_extend_combined}}) and the enterprise-level
#'   overview (\code{\link{inv_inc_big_overview}}'s \code{dt_2nd_only}) -- so
#'   that the base tables and the enterprise level keep agreeing whatever value
#'   is chosen.
#'
#' @param style Species grouping style of the enterprise-level overview,
#'   \code{"baysf"} (default) or \code{"generic"}. Passed to
#'   \code{\link{inv_inc_big_overview}}.
#'
#' @param progress_bar Logical, whether the function displays a console progress
#'   bar itself. Default \code{TRUE}. Set to \code{FALSE} when you supply your
#'   own \pkg{progressr} handler -- in particular in a Shiny app, see
#'   \emph{Progress reporting}. The progress bars of the inner steps are always
#'   suppressed, independently of this argument.
#'
#' @param keep_steps Logical. If \code{TRUE}, the intermediate results of the
#'   chain are returned in the \code{steps} element. They are useful for
#'   inspection, but they hold several per-tree frames and grow with the
#'   inventory -- for a real inventory they dominate the size of the returned
#'   object. The default is \code{FALSE}, which leaves \code{steps} as
#'   \code{NULL}.
#'
#' @returns An object of class \code{fe_increment_repsurv} (which inherits from
#'   \code{fe_increment}), a list with
#'   \describe{
#'     \item{trees}{The per-tree increment data frame covering the whole
#'       enterprise, carrying the \code{"inc_meta"} attribute. This is the input
#'       for \code{\link{increment_base_table}} and
#'       \code{\link{increment_base_table_main_stand}}.}
#'     \item{overview}{The enterprise-level aggregate, output of
#'       \code{\link{inv_inc_big_overview}}, input for
#'       \code{\link{output_increment_overall}}.}
#'     \item{summaric}{The summaric (\emph{ertragsgeschichtlicher}) increment,
#'       output of \code{\link{inv_inc_summaric}} -- the gold standard at
#'       enterprise level.}
#'     \item{meta}{The header meta (survey years, period, area, point count,
#'       calculation method, fill option).}
#'     \item{steps}{The intermediate results of the chain (\code{tree_level},
#'       \code{filled}, \code{consolidated}, \code{extended_matched}) for
#'       inspection, or \code{NULL} -- see \code{keep_steps}.}
#'   }
#'
#' @seealso \code{\link{increment_base_table}},
#'   \code{\link{increment_base_table_main_stand}},
#'   \code{\link{output_increment_overall}},
#'   \code{\link{output_increment_base_table_pdf}}
#'
#' @family increment
#'
#' @export
#'
#' @examples
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # The prepared tree lists ship with the package; how they were built with
#'   # the canonical chain is shown in ?data_ex3_trees_essentials
#'   inv_a_trees <- data_ex3_previous_sample_trees_essentials
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   # The whole chain in one call
#'   suppressWarnings(suppressMessages({
#'     inc <- inv_increment_repeated_survey(
#'       inv_a, inv_b, inv_a_trees, inv_b_trees,
#'       method = "rep_classic", fill_option = "standard",
#'       progress_bar = FALSE
#'     )
#'   }))
#'
#'   # Enterprise level, and the base tables built on the same object
#'   inc$overview$combined$overall$iv_total
#'   increment_base_table(inc, by_class = "age")$all_total
#'
inv_increment_repeated_survey <- function(
    inv_1st, inv_2nd, inv_1st_trees, inv_2nd_trees,
    method      = c("rep_classic", "rep_mean", "rep_end", "rep_trans"),
    fill_option = c("standard", "min_estimates", "all_estimates"),
    match_type    = c("plot_id", "center_coord"),
    plot_id_style = c("baysf", "generic"),
    tree_id_style = c("baysf", "generic"),
    inner_circle_non_matchable = FALSE,
    shrinkage_rate_per_decade  = 0.10,
    species_check = c("species_group", "species", "warn_only"),
    clamp_plausible_shrinkage  = TRUE,
    coord_tolerance     = 10,
    include_reptrees    = TRUE,
    include_second_only = TRUE,
    d_q_interval        = 10,
    dt_second_only      = -5,
    style               = c("baysf", "generic"),
    progress_bar        = TRUE,
    keep_steps          = FALSE
) {

  method        <- match.arg(method)
  fill_option   <- match.arg(fill_option)
  match_type    <- match.arg(match_type)
  plot_id_style <- match.arg(plot_id_style)
  tree_id_style <- match.arg(tree_id_style)
  species_check <- match.arg(species_check)
  style         <- match.arg(style)

  # Admissibility (sample inventory, all plots fe_ccircle_spatial, genuine
  # survey pair) is checked here, before any work is done, so an inadmissible
  # inventory fails immediately rather than somewhere down the chain.
  inv_increment_repsurv_ccirc_check_input(inv_1st, inv_2nd)

  # The chain is wrapped so that progress_bar = TRUE can show a console bar
  # without the caller having to know about progressr. The body only *signals*
  # progress (seven phases, one per step below); who displays it is decided
  # outside: progressr::with_progress() here when progress_bar = TRUE, or the
  # caller's own handler (e.g. progressr::withProgressShiny()) when it is FALSE.
  run <- function() {

  p <- progressr::progressor(steps = 7L)

  # --- 1. Tree-level increments from the repeated survey ----------------------
  p(message = fe_msg("progress_inc_match"))
  tree_level <- inv_increment_repsurv_ccirc(
    inv_1st, inv_2nd, inv_1st_trees, inv_2nd_trees,
    match_type    = match_type,
    plot_id_style = plot_id_style,
    tree_id_style = tree_id_style,
    method        = method,
    inner_circle_non_matchable = inner_circle_non_matchable,
    shrinkage_rate_per_decade  = shrinkage_rate_per_decade,
    species_check              = species_check,
    clamp_plausible_shrinkage  = clamp_plausible_shrinkage,
    coord_tolerance            = coord_tolerance,
    # The inner per-plot bar would compete with the progressr signalling of
    # this function; progress is reported at this level instead.
    progress_bar               = FALSE
  )

  # inv_increment_repsurv_ccirc() returns the plot match result (not a tree
  # increment list) when matching failed; pass that back unchanged so the
  # caller can inspect it, exactly as the low-level function does.
  if (is.null(tree_level$tree_increments)) return(tree_level)

  # --- 2. gnfi3 estimates for gap filling -------------------------------------
  p(message = fe_msg("progress_inc_fill"))
  filled <- inv_inc_fill_gaps_gnfi3(
    tree_level, inv_1st_trees, inv_2nd_trees,
    include_reptrees = include_reptrees
  )

  # --- 3. Consolidate per tree ------------------------------------------------
  p(message = fe_msg("progress_inc_consolidate"))
  consolidated <- inv_inc_tree_consolidate(filled, fill_option = fill_option)

  # --- 4. Extend with the attributes the aggregation needs --------------------
  p(message = fe_msg("progress_inc_extend"))
  extended_matched <- inv_inc_tree_extend(
    consolidated, inv_1st_trees, inv_2nd_trees, inv_2nd,
    d_q_interval = d_q_interval
  )

  # --- 5. Fold in the plots that exist only in the second survey --------------
  p(message = fe_msg("progress_inc_second_only"))
  trees <- if (include_second_only) {
    inv_inc_tree_extend_combined(
      extended_matched, filled, inv_2nd_trees,
      dt = dt_second_only, d_q_interval = d_q_interval
    )
  } else {
    extended_matched
  }

  # --- 6. Enterprise level ----------------------------------------------------
  p(message = fe_msg("progress_inc_summaric"))
  summaric <- inv_inc_summaric(
    tree_level$tree_increments, filled$fills_forward,
    inv_1st_trees, inv_2nd_trees
  )

  p(message = fe_msg("progress_inc_overview"))
  # dt_second_only must reach BOTH sides that estimate the second-only plots:
  # the per-tree frame (via inv_inc_tree_extend_combined() above) and the
  # enterprise-level overview here. Passing it to only one of them would make
  # the base tables and the enterprise level disagree silently as soon as the
  # default is changed -- the very convergence the second-only fold establishes.
  overview <- inv_inc_big_overview(
    filled, inv_1st_trees, inv_2nd_trees, inv_2nd,
    style = style, inc_summaric = summaric,
    dt_2nd_only = dt_second_only
  )

  structure(
    list(
      trees    = trees,
      overview = overview,
      summaric = summaric,
      meta     = attr(trees, "inc_meta"),
      steps    = if (keep_steps) {
        list(
          tree_level       = tree_level,
          filled           = filled,
          consolidated     = consolidated,
          extended_matched = extended_matched
        )
      } else {
        NULL
      }
    ),
    class = c("fe_increment_repsurv", "fe_increment", "list")
  )

  } # end run()

  # with_progress() returns the value of the expression, so an early return
  # inside run() (a failed plot match) travels out unchanged either way.
  if (isTRUE(progress_bar)) progressr::with_progress(run()) else run()
}



#' Is an Object an fe_increment_repsurv?
#'
#' @param x Object to test.
#'
#' @returns Logical.
#'
#' @family increment
#'
#' @export
#'
#' @examples
#'   is_fe_increment_repsurv(1L)
#'
is_fe_increment_repsurv <- function(x) {
  inherits(x, "fe_increment_repsurv")
}



#' Print an fe_increment_repsurv Object
#'
#' @param x An \code{\link{inv_increment_repeated_survey}} result.
#' @param ... Not used.
#'
#' @returns \code{x}, invisibly.
#'
#' @export
#'
print.fe_increment_repsurv <- function(x, ...) {
  m <- x$meta

  # Enterprise-wide increment per hectare: the summaric (ertragsgeschichtlich)
  # balance -- second-survey volume minus first plus removals. It is the
  # enterprise reference figure and, unlike the per-tree route, does not depend
  # on the chosen rep method or fill option (those only govern the
  # disaggregation onto sub-units), which is why it is the figure shown here.
  iv_ha <- tryCatch({
    oc <- x$overview$combined$overall$iv_total
    oc$iv_m3_ha_yr[oc$fill_option == "combined_inc_summaric"][1]
  }, error = function(e) NA_real_)

  cat("<fe_increment_repsurv>\n")
  cat("  Surveys      : ", m$inv_year_prev, " -> ", m$year,
      "  (period ", m$period_yr, " yr)\n", sep = "")
  cat("  Area / plots : ", round(m$area_ha, 1), " ha / ", m$n_plots, "\n",
      sep = "")
  cat("  Method       : ", m$method, " / fill: ", m$fill_option, "\n", sep = "")
  cat("  Trees        : ", nrow(x$trees), "\n", sep = "")
  if (length(iv_ha) == 1L && !is.na(iv_ha))
    cat(sprintf("  Increment    : %.2f m3/ha/yr (summaric balance)\n", iv_ha))
  cat("  Steps kept   : ", if (is.null(x$steps)) "no" else "yes", "\n", sep = "")
  cat("  Use increment_base_table() / output_increment_overall() on this",
      "object.\n")
  invisible(x)
}

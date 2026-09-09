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

#' Restructure an Increment Big-Overview Into a Render-Ready Object
#'
#' Companion to \code{\link{inv_inc_big_overview}}: takes its rich
#' multi-list output and produces a render-ready intermediate object that
#' downstream functions (\code{\link{output_increment_overall_pdf}}, Shiny
#' tables, csv exports) can consume without redoing the restructuring.
#'
#' The intermediate object collects three parallel tables -- one for
#' matched plots, one for second-only plots, one for the combined
#' enterprise view -- with identical column shape. This lets the renderer
#' stack them vertically with column-aligned numbers. CI95 columns are
#' present in all three tables but only filled for the matches_only
#' table; the gnfi3-based second_only and combined contributions stay
#' empty by contract (\code{NA}) because their per-plot scatter is a
#' model artefact and an empirical CI would mislead.
#'
#' Language is deliberately a render-time concern, not a
#' restructuring-time one: the \code{species_group} column is passed
#' through as the original \code{fe_species_*} S3 vector, and the
#' rendering layer (\code{\link{output_increment_overall_pdf}} or any
#' alternative consumer) sets \code{options(fe_spec_lang = ...)} plus
#' \code{format()} to choose the display language. This keeps the
#' intermediate object reusable for multi-language exports from the
#' same data.
#'
#' @param bo Either an \code{fe_increment_repsurv} bundle, the output of
#'   \code{\link{inv_increment_repeated_survey}} (the usual case -- its
#'   \code{overview} element is taken from it, and \code{fill_option} defaults
#'   to the one the bundle was built with), or a bare
#'   \code{\link{inv_inc_big_overview}} result.
#' @param fill_option Which matched fill_option to display in the
#'   "Single-tree based" block of \code{table_matched} and the
#'   matching column block of \code{table_combined}. Default
#'   \code{"standard"} -- the practical recommendation. The
#'   "Summaric / Yield-history" block always uses the
#'   \code{inc_summaric} row from the matched overview.
#' @param user_subtitle Optional character giving an extra subtitle for
#'   the output (e.g. \code{"Forstbetrieb Neustadt"}). Default
#'   \code{NULL}.
#'
#' @returns A list with the elements:
#'   \describe{
#'     \item{meta}{A list with \code{title} (named character vector
#'       indexed by language code), \code{user_subtitle},
#'       \code{method} (the increment method, pulled from \code{bo$method}),
#'       \code{fill_option}, \code{inv_year_prev}, \code{inv_year_curr},
#'       \code{period_yr}, and \code{plot_match_stats} (a list with
#'       \code{n_total_2nd}, \code{n_matched}, \code{n_2nd_only},
#'       \code{area_total_2nd_ha}, \code{area_matched_ha},
#'       \code{area_2nd_only_ha}).}
#'     \item{table_matched}{One row per species_group plus a summary row
#'       (\code{is_total_row = TRUE}), with the eight numeric columns
#'       described below.}
#'     \item{table_2nd_only}{Same column shape as \code{table_matched}.
#'       The "Single-tree based" block holds the gnfi3-backward
#'       estimate; the "Summaric" block is all \code{NA} (there is no
#'       yield-history balance for plots with only one observation).}
#'     \item{table_combined}{Same column shape; columns hold the
#'       per-row sum of \code{table_matched} and \code{table_2nd_only}.
#'       All CI95 columns are \code{NA}.}
#'   }
#'   The eight numeric columns in each table:
#'   \code{iv_m3_yr_total_st}, \code{ci95_iv_m3_yr_total_st},
#'   \code{iv_m3_ha_yr_st}, \code{ci95_iv_m3_ha_yr_st} for the
#'   single-tree block; suffix \code{_sum} (for "summaric") on the same
#'   four for the yield-history block.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Matching and gap filling of the ex3 pair are shipped ready-made, so the
#' # overall figures can be built without running the whole chain first.
#' inv_b       <- data_ex3_sample_fe_inventory
#' inv_a_trees <- data_ex3_previous_sample_trees_essentials
#' inv_b_trees <- data_ex3_sample_trees_essentials
#' inc_rep     <- data_ex3_increment_matched
#' inc_fl      <- data_ex3_increment_fills
#'
#' inc_sum <- inv_inc_summaric(
#'   inc_rep$tree_increments, inc_fl$fills_forward, inv_a_trees, inv_b_trees
#' )
#' bo <- inv_inc_big_overview(
#'   inc_fl, inv_a_trees, inv_b_trees, inv_b,
#'   style = "generic", inc_summaric = inc_sum
#' )
#'
#' out <- output_increment_overall(
#'   bo,
#'   fill_option   = "standard",
#'   user_subtitle = "Forstbetrieb Neustadt"
#' )
#' str(out, max.level = 2)
#' }
#' @family increment
output_increment_overall <- function(
    bo,
    fill_option   = c("standard", "min_estimates", "all_estimates"),
    user_subtitle = NULL
) {
  # Accept the bundled result of inv_increment_repeated_survey() as well as a
  # bare inv_inc_big_overview() result. The bundle also knows which fill option
  # it was built with, so that argument becomes optional here.
  if (is_fe_increment_repsurv(bo)) {
    if (missing(fill_option) && !is.null(bo$meta$fill_option))
      fill_option <- bo$meta$fill_option
    bo <- bo$overview
  } else if (inherits(bo, "fe_increment")) {
    # Another increment bundle -- currently inv_increment_gnfi3(). This report
    # contrasts the matched plots with those only the second survey covers, a
    # distinction that exists in a repeated survey and nowhere else. Refuse
    # clearly instead of failing on the missing $matches_only element.
    stop(fe_msg("increment_overall_needs_repsurv", class(bo)[1]), call. = FALSE)
  }

  fill_option <- match.arg(fill_option)
  if (!is.null(user_subtitle))
    user_subtitle <- as.character(user_subtitle)

  # --- Meta block ----------------------------------------------------
  ip    <- bo$inv_period
  m_np  <- bo$matches_only$overall$n_plots
  s_np  <- bo$second_only$overall$n_plots

  meta <- list(
    title = c(
      en = "Increment Overview",
      de = "Zuwachs\u00fcbersicht"
    ),
    user_subtitle = user_subtitle,
    method        = bo$method,
    fill_option   = fill_option,
    inv_year_prev = ip$time_yr_1st,
    inv_year_curr = ip$time_yr_2nd,
    period_yr     = ip$period_yr,
    plot_match_stats = list(
      n_total_2nd       = m_np$n_plots + s_np$n_plots,
      n_matched         = m_np$n_plots,
      n_2nd_only        = s_np$n_plots,
      area_total_2nd_ha = m_np$area_total_ha + s_np$area_total_ha,
      area_matched_ha   = m_np$area_total_ha,
      area_2nd_only_ha  = s_np$area_total_ha
    )
  )

  # Common species ordering: take it from $combined, which covers the
  # full set of species (any species in matched or second_only).
  species_order <- bo$combined$by_species_group$iv_total |>
    dplyr::filter(.data$fill_option == paste0("combined_", fill_option)) |>
    dplyr::pull("species_group")
  # Defensive fallback if combined is empty for some reason
  if (length(species_order) == 0L) {
    species_order <- unique(
      bo$matches_only$by_species_group$iv_total$species_group
    )
  }

  # --- Tables --------------------------------------------------------
  table_matched  <- build_table_matched(bo, fill_option, species_order)
  table_2nd_only <- build_table_2nd_only(bo, species_order)
  table_combined <- build_table_combined(bo, fill_option, species_order)

  # Canonical column order: keys first, then the single-tree block, then
  # the summaric block.
  col_order <- c(
    "species_group", "is_total_row",
    "iv_m3_yr_total_st", "ci95_iv_m3_yr_total_st",
    "iv_m3_ha_yr_st",    "ci95_iv_m3_ha_yr_st",
    "iv_m3_yr_total_sum", "ci95_iv_m3_yr_total_sum",
    "iv_m3_ha_yr_sum",    "ci95_iv_m3_ha_yr_sum"
  )
  table_matched  <- table_matched[,  col_order]
  table_2nd_only <- table_2nd_only[, col_order]
  table_combined <- table_combined[, col_order]

  list(
    meta           = meta,
    table_matched  = table_matched,
    table_2nd_only = table_2nd_only,
    table_combined = table_combined
  )
}


# Build a per-species block in the canonical row order
# (species_order first, then a total row). species_group / is_total_row
# are the join keys. Missing species in `iv_total_sg` get 0 in the iv
# columns and NA in the ci95 columns. Suffix is "_st" for single-tree
# and "_sum" for the summaric block.
.build_block <- function(iv_total_sg, iv_total_ovr, fill_option,
                         species_order, suffix) {

  # Pull per-species rows of the selected fill_option
  sg_raw <- iv_total_sg |>
    dplyr::filter(.data$fill_option == !!fill_option) |>
    dplyr::select(
      "species_group",
      iv_m3_yr_total      = "iv_m3_yr_total",
      ci95_iv_m3_yr_total = "ci95_iv_m3_yr_total",
      iv_m3_ha_yr         = "iv_m3_ha_yr",
      ci95_iv_m3_ha_yr    = "ci95_iv_m3_ha_yr"
    )

  # Order frame in canonical species order; missing species get 0 / NA
  ord_df <- tibble::tibble(species_group = species_order)
  sg <- ord_df |>
    dplyr::left_join(sg_raw, by = "species_group") |>
    dplyr::mutate(
      is_total_row   = FALSE,
      iv_m3_yr_total = dplyr::coalesce(.data$iv_m3_yr_total, 0),
      iv_m3_ha_yr    = dplyr::coalesce(.data$iv_m3_ha_yr, 0)
    )

  # Total row from the overall iv_total. When the filter returns no
  # rows -- which happens e.g. for the "gnfi3_backward" fill_option on
  # an empty second_only side -- dplyr::transmute leaves
  # species_group as logical(0), which dplyr::bind_rows() then refuses
  # to combine with sg's typed fe_species_* species_group. Synthesise
  # a zero-valued total row with species_group cast to sg's prototype
  # (NA of the right class) in that case.
  ovr <- iv_total_ovr |>
    dplyr::filter(.data$fill_option == !!fill_option) |>
    dplyr::transmute(
      species_group       = NA,
      is_total_row        = TRUE,
      iv_m3_yr_total      = .data$iv_m3_yr_total,
      ci95_iv_m3_yr_total = .data$ci95_iv_m3_yr_total,
      iv_m3_ha_yr         = .data$iv_m3_ha_yr,
      ci95_iv_m3_ha_yr    = .data$ci95_iv_m3_ha_yr
    )
  if (nrow(ovr) == 0L) {
    ovr <- tibble::tibble(
      species_group       = sg$species_group[NA_integer_],
      is_total_row        = TRUE,
      iv_m3_yr_total      = 0,
      ci95_iv_m3_yr_total = NA_real_,
      iv_m3_ha_yr         = 0,
      ci95_iv_m3_ha_yr    = NA_real_
    )
  }

  out <- dplyr::bind_rows(sg, ovr)
  # Apply the column-name suffix for the block
  rn <- c(
    paste0("iv_m3_yr_total_", suffix),
    paste0("ci95_iv_m3_yr_total_", suffix),
    paste0("iv_m3_ha_yr_", suffix),
    paste0("ci95_iv_m3_ha_yr_", suffix)
  )
  names(out)[match(
    c("iv_m3_yr_total", "ci95_iv_m3_yr_total",
      "iv_m3_ha_yr", "ci95_iv_m3_ha_yr"),
    names(out)
  )] <- rn
  out
}


# Empty-value template for the "summaric" block -- used to align column
# shape across the three tables when the block is not applicable
# (table_2nd_only has no yield-history component).
.na_sum_block <- function(n_rows) {
  tibble::tibble(
    iv_m3_yr_total_sum      = rep(NA_real_, n_rows),
    ci95_iv_m3_yr_total_sum = rep(NA_real_, n_rows),
    iv_m3_ha_yr_sum         = rep(NA_real_, n_rows),
    ci95_iv_m3_ha_yr_sum    = rep(NA_real_, n_rows)
  )
}


# Build the matched-plots table: single-tree block from fill_option,
# summaric block from the inc_summaric row of the matched overview.
build_table_matched <- function(bo, fill_option, species_order) {
  st <- .build_block(
    bo$matches_only$by_species_group$iv_total,
    bo$matches_only$overall$iv_total,
    fill_option, species_order, suffix = "st"
  )
  sm <- .build_block(
    bo$matches_only$by_species_group$iv_total,
    bo$matches_only$overall$iv_total,
    "inc_summaric", species_order, suffix = "sum"
  )
  st |>
    dplyr::left_join(sm, by = c("species_group", "is_total_row"))
}


# Build the second-only table; summaric block stays NA (no yield-history
# for one-observation plots).
build_table_2nd_only <- function(bo, species_order) {
  st <- .build_block(
    bo$second_only$by_species_group$iv_total,
    bo$second_only$overall$iv_total,
    "gnfi3_backward", species_order, suffix = "st"
  )
  st |>
    dplyr::bind_cols(.na_sum_block(nrow(st)))
}


# Build the combined table; CI95 columns are NA by contract.
build_table_combined <- function(bo, fill_option, species_order) {
  st <- .build_block(
    bo$combined$by_species_group$iv_total,
    bo$combined$overall$iv_total,
    paste0("combined_", fill_option), species_order, suffix = "st"
  )
  sm <- .build_block(
    bo$combined$by_species_group$iv_total,
    bo$combined$overall$iv_total,
    "combined_inc_summaric", species_order, suffix = "sum"
  )
  out <- st |>
    dplyr::left_join(sm, by = c("species_group", "is_total_row"))
  # CI95 are NA by contract in combined -- enforce defensively
  out$ci95_iv_m3_yr_total_st  <- NA_real_
  out$ci95_iv_m3_ha_yr_st     <- NA_real_
  out$ci95_iv_m3_yr_total_sum <- NA_real_
  out$ci95_iv_m3_ha_yr_sum    <- NA_real_
  out
}

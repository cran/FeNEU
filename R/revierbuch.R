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


# -----------------------------------------------------------------------------
# 1.  revierbuch_extract_circles()  --  internal helper
# -----------------------------------------------------------------------------

#' Extract All Concentric-Circle Definitions From an fe_inventory
#'
#' @param fe_inv An \code{\link{fe_inventory}} object whose \code{plot} column
#'   contains \code{fe_ccircle_spatial} objects.
#'
#' @return A tibble with columns \code{plot_id}, \code{dbh_lower},
#'   \code{c_area_m2}, \code{radius_m}, \code{is_largest_circle}. Plots that
#'   are not \code{fe_ccircle_spatial} are silently skipped (a single summary
#'   warning is issued if any are found).
#'
#' @keywords internal
#' @noRd
revierbuch_extract_circles <- function(fe_inv) {

  results <- purrr::map2(
    fe_inv$plot_id, fe_inv$plot,
    function(pid, plt) {
      if (!ForestElementsR::is_fe_ccircle_spatial(plt)) return(NULL)
      plt$circle_definition |>
        dplyr::select("dbh_lower", c_area_m2 = "c_area") |>
        dplyr::mutate(
          plot_id  = pid,
          radius_m = sqrt(.data$c_area_m2 / pi)
        )
    }
  )

  n_skipped <- sum(purrr::map_lgl(results, is.null))
  if (n_skipped > 0) {
    warning(
      n_skipped, " plot(s) are not fe_ccircle_spatial and were skipped.",
      call. = FALSE
    )
  }

  purrr::compact(results) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::mutate(
      is_largest_circle = .data$c_area_m2 == max(.data$c_area_m2)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "plot_id", "dbh_lower", "c_area_m2", "radius_m", "is_largest_circle"
    )
}


# -----------------------------------------------------------------------------
# 2.  revierbuch_circle_fractions()  --  internal helper
# -----------------------------------------------------------------------------

#' Compute the Overlap Fraction of Each Plot Circle With Each Stand Polygon
#'
#' For every (plot x concentric-circle x stand-polygon) combination, computes
#' what fraction of the circle area falls inside the stand.
#'
#' @param fe_inv     An \code{\link{fe_inventory}} object.
#' @param stands_sf  An \code{sf} object with stand polygons.
#' @param stand_id_col Character; name of the column in \code{stands_sf} that
#'   uniquely identifies each stand.
#'
#' @return A tibble with columns \code{plot_id}, \code{stand_id},
#'   \code{dbh_lower}, \code{fraction}, \code{is_largest_circle}.
#'   Only plot x stand combinations with \code{fraction > 0} are returned.
#'
#' @keywords internal
#' @noRd
revierbuch_circle_fractions <- function(fe_inv, stands_sf, stand_id_col) {

  # Plot centre points (sf, CRS from inventory data)
  centers_sf   <- pull_centers(fe_inv)
  circle_defs  <- revierbuch_extract_circles(fe_inv)

  if (nrow(circle_defs) == 0) {
    stop("No fe_ccircle_spatial plots found in fe_inv.", call. = FALSE)
  }

  # Build one-sf-row per (plot x circle) carrying centre geometry
  circles_sf <- centers_sf |>
    dplyr::right_join(sf::st_drop_geometry(circle_defs), by = "plot_id") |>
    sf::st_as_sf()

  # Buffer each point to its circle radius (vector dist = one per row)
  circles_sf <- sf::st_buffer(circles_sf, dist = circles_sf$radius_m)

  # Align stands CRS to circles CRS
  stands_aligned <- sf::st_transform(stands_sf, sf::st_crs(circles_sf))

  # Keep only the stand id column + geometry
  stands_id_geom <- stands_aligned[, stand_id_col, drop = FALSE]

  # Intersection of all circles with all stands
  inters <- sf::st_intersection(circles_sf, stands_id_geom)

  if (nrow(inters) == 0) {
    warning(
      "No intersection found between inventory plots and stands_sf. ",
      "Check that coordinate systems are compatible.",
      call. = FALSE
    )
    return(
      tibble::tibble(
        plot_id           = character(),
        stand_id          = character(),
        dbh_lower         = numeric(),
        fraction          = numeric(),
        is_largest_circle = logical()
      )
    )
  }

  inters |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      inter_area_m2 = as.numeric(sf::st_area(inters)),
      fraction      = .data$inter_area_m2 / .data$c_area_m2
    ) |>
    dplyr::rename(stand_id = !!stand_id_col) |>
    dplyr::filter(.data$fraction > 0) |>
    dplyr::select(
      "plot_id", "stand_id", "dbh_lower", "fraction", "is_largest_circle"
    )
}


# -----------------------------------------------------------------------------
# 2b.  revierbuch_wzp_fractions()  --  internal helper
# -----------------------------------------------------------------------------

#' Normalise a Plot-to-Stand Lookup to a Named Character Vector
#'
#' Accepts the several lookup forms allowed by \code{stand_register_pdf()} and
#' returns a named character vector \code{c(plot_id = stand_id, ...)}, or
#' \code{NULL} to signal "derive from the plot_id suffix".
#'
#' @param stand_id_lookup One of:
#'   \itemize{
#'     \item \code{NULL} — passed through (derive from \code{plot_id} suffix).
#'     \item An unnamed length-1 character — aggregate \emph{all} plots into a
#'       single stand with that name.
#'     \item A named character vector \code{c(plot_id = stand_id, ...)}.
#'     \item A data frame / tibble with columns \code{plot_id} and
#'       \code{stand_id} (e.g.\ read from a CSV).
#'   }
#' @param plot_ids Character vector of all \code{plot_id}s in the inventory,
#'   used for the aggregate-all case.
#'
#' @return A named character vector, or \code{NULL}.
#'
#' @keywords internal
#' @noRd
.revierbuch_normalize_lookup <- function(stand_id_lookup, plot_ids) {

  if (is.null(stand_id_lookup)) return(NULL)

  # Data frame / tibble with plot_id + stand_id columns
  if (is.data.frame(stand_id_lookup)) {
    if (!all(c("plot_id", "stand_id") %in% names(stand_id_lookup))) {
      stop(
        "A data-frame stand_id_lookup must have columns 'plot_id' and ",
        "'stand_id'.",
        call. = FALSE
      )
    }
    return(stats::setNames(
      as.character(stand_id_lookup$stand_id),
      as.character(stand_id_lookup$plot_id)
    ))
  }

  if (!is.character(stand_id_lookup)) {
    stop(
      "stand_id_lookup must be NULL, a named character vector, a data frame ",
      "with plot_id/stand_id columns, or a single unnamed stand name.",
      call. = FALSE
    )
  }

  # Unnamed length-1 character -> aggregate all plots into one stand
  if (length(stand_id_lookup) == 1 && is.null(names(stand_id_lookup))) {
    return(stats::setNames(rep(stand_id_lookup, length(plot_ids)), plot_ids))
  }

  # Named character vector -> use as-is
  if (is.null(names(stand_id_lookup))) {
    stop(
      "A multi-element character stand_id_lookup must be named ",
      "(names = plot_id, values = stand_id).",
      call. = FALSE
    )
  }

  stand_id_lookup
}


# -----------------------------------------------------------------------------
# 2b.  revierbuch_wzp_fractions()  --  internal helper
# -----------------------------------------------------------------------------

#' Build a Fractions Tibble for Stand-Wise Inventories
#'
#' Creates the same \code{fractions} structure expected by
#' \code{revierbuch_build_stand_tables()} and
#' \code{revierbuch_wzp_weighted_trees()}, but without any spatial computation.
#' Each plot is assigned fraction 1 to its stand; stand membership is either
#' read from the embedded \code{plot_id} (format \code{"<stpnr>_<stand_id>"})
#' or supplied explicitly via \code{stand_id_lookup} (see
#' \code{.revierbuch_normalize_lookup()} for the accepted forms).
#'
#' @param fe_inv          An \code{\link{fe_inventory}} object from a
#'   stand-wise import (\code{standard = "silvarith"} or
#'   \code{"relascope"}).
#' @param stand_id_lookup Optional plot-to-stand lookup.  See
#'   \code{.revierbuch_normalize_lookup()}.  When \code{NULL} (default), the
#'   stand ID is extracted as everything after the first \code{"_"} in
#'   \code{plot_id}.
#'
#' @return A tibble with columns \code{plot_id}, \code{stand_id},
#'   \code{fraction} (always 1), \code{is_largest_circle} (always
#'   \code{TRUE}).  Plots not covered by the lookup are dropped (with a
#'   warning).
#'
#' @keywords internal
#' @noRd
revierbuch_wzp_fractions <- function(fe_inv, stand_id_lookup = NULL) {

  plots  <- dplyr::tibble(plot_id = fe_inv$plot_id)
  lookup <- .revierbuch_normalize_lookup(stand_id_lookup, fe_inv$plot_id)

  if (is.null(lookup)) {
    plots <- plots |>
      dplyr::mutate(stand_id = sub("^[^_]+_", "", .data$plot_id))
  } else {
    plots <- plots |>
      dplyr::mutate(stand_id = unname(lookup[.data$plot_id]))

    unmapped <- plots$plot_id[is.na(plots$stand_id)]
    if (length(unmapped) > 0) {
      warning(
        length(unmapped), " plot(s) are not covered by stand_id_lookup and ",
        "were dropped (e.g. ",
        paste(utils::head(unmapped, 3), collapse = ", "), ").",
        call. = FALSE
      )
      plots <- plots |> dplyr::filter(!is.na(.data$stand_id))
    }
  }

  plots |> dplyr::mutate(fraction = 1, is_largest_circle = TRUE)
}


# -----------------------------------------------------------------------------
# 3.  revierbuch_weighted_trees()  --  internal helper
# -----------------------------------------------------------------------------

#' Produce a Tree Tibble With Stand Assignments and Adjusted n_rep_ha
#'
#' Pulls trees from \code{fe_inv}, completes heights, adds basal area and
#' volume per tree, then joins with \code{fractions} to weight each tree's
#' representation factor by the fraction of its sampling circle that lies
#' inside each stand.
#'
#' @param fe_inv    An \code{\link{fe_inventory}} object.
#' @param fractions Output of \code{revierbuch_circle_fractions()}.
#'
#' @return A tibble extending the output of
#'   \code{\link{trees_add_essentials}} with additional columns
#'   \code{stand_id} and \code{n_rep_ha_adj}.
#'
#' @keywords internal
#' @noRd
revierbuch_weighted_trees <- function(fe_inv, fractions) {

  # Subset fe_inv to only plots that overlap with any stand
  plots_needed <- unique(fractions$plot_id)
  fe_inv_sub   <- fe_inv[fe_inv$plot_id %in% plots_needed, ]

  # Run the pipeline on the subsetted inventory (not the full fe_inv)
  trees <- fe_inv_sub |>
    pull_trees(.progress = FALSE) |>
    height_complete_inventory(method = "NFI") |>
    dplyr::mutate(
      height_m = ifelse(!.data$h_m_tree, .data$h_est_m, .data$height_m)
    ) |>
    trees_add_essentials(method = "BaySF")

  # Join with circle fractions on plot_id + sampling-circle threshold
  trees |>
    dplyr::left_join(
      fractions,
      by = c("plot_id", "dbh_trshld" = "dbh_lower")
    ) |>
    dplyr::filter(!is.na(.data$stand_id)) |>    # keep only trees in >=1 stand
    dplyr::mutate(n_rep_ha_adj = .data$n_rep_ha * .data$fraction)
}


# -----------------------------------------------------------------------------
# 3b.  revierbuch_wzp_weighted_trees()  --  internal helper
# -----------------------------------------------------------------------------

#' Produce a Weighted-Tree Tibble for Stand-Wise Inventories
#'
#' Analogous to \code{revierbuch_weighted_trees()} but for WZP inventories:
#' trees are joined to stands via \code{plot_id} alone (no
#' \code{dbh_trshld}/\code{dbh_lower} join needed), and
#' \code{n_rep_ha_adj = n_rep_ha} (fraction is always 1 for WZP).
#'
#' @param fe_inv    An \code{\link{fe_inventory}} object.
#' @param fractions Output of \code{revierbuch_wzp_fractions()}.
#'
#' @return A tibble with the same structure as the output of
#'   \code{revierbuch_weighted_trees()}.
#'
#' @keywords internal
#' @noRd
revierbuch_wzp_weighted_trees <- function(fe_inv, fractions) {

  plots_needed <- unique(fractions$plot_id)
  fe_inv_sub   <- fe_inv[fe_inv$plot_id %in% plots_needed, ]

  trees <- fe_inv_sub |>
    pull_trees(.progress = FALSE) |>
    height_complete_inventory(method = "NFI") |>
    dplyr::mutate(
      height_m = ifelse(!.data$h_m_tree, .data$h_est_m, .data$height_m)
    ) |>
    trees_add_essentials(method = "BaySF")

  # WZP tree records carry an embedded stand_id (= bestand); drop it so the
  # fractions' assignment (which honours stand_id_lookup) is authoritative.
  trees |>
    dplyr::select(-tidyselect::any_of("stand_id")) |>
    dplyr::left_join(fractions, by = "plot_id") |>
    dplyr::filter(!is.na(.data$stand_id)) |>
    dplyr::mutate(n_rep_ha_adj = .data$n_rep_ha)
}


# -----------------------------------------------------------------------------
# 4.  revierbuch_build_stand_tables()  --  internal helper
# -----------------------------------------------------------------------------

#' Aggregate Weighted Trees to Per-Stand Summary Tables
#'
#' @param weighted_trees  Output of \code{revierbuch_weighted_trees()}.
#' @param fractions       Output of \code{revierbuch_circle_fractions()}.
#' @param increment_data  Optional tibble with columns \code{stand_id},
#'   \code{species_group}, \code{zuwachs_m3_ha_yr} (and optionally
#'   \code{site_index}, \code{stocking_level} when from yield tables).
#' @param ytable_data     Optional tibble from
#'   \code{revierbuch_increment_ytables_per_stand()} with additional columns
#'   \code{site_index} and \code{stocking_level}.
#'
#' @return A named list of per-stand summary tibbles (one element per stand).
#'
#' @keywords internal
#' @noRd
revierbuch_build_stand_tables <- function(weighted_trees, fractions,
                                          increment_data = NULL,
                                          ytable_data    = NULL,
                                          layer_filter   = c("OS", "all",
                                                              "gesamt")) {

  layer_filter <- match.arg(layer_filter)

  # Effective number of plots per stand (sum of largest-circle fractions)
  n_eff <- fractions |>
    dplyr::filter(.data$is_largest_circle) |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::summarise(n_eff_plots = sum(.data$fraction), .groups = "drop")

  # Stand area (ha) = sum of fraction * area_rep_ha across contributing plots
  # Each plot's representative area is split proportionally by the largest-circle
  # fraction falling inside each stand
  rep_area <- weighted_trees |>
    dplyr::distinct(.data$plot_id, .data$area_rep_ha)
  stand_areas <- fractions |>
    dplyr::filter(.data$is_largest_circle) |>
    dplyr::left_join(rep_area, by = "plot_id") |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::summarise(
      stand_area_ha = sum(.data$fraction * .data$area_rep_ha, na.rm = TRUE),
      .groups = "drop"
    )

  # Pre-filter trees based on layer_filter
  wt_to_agg <- weighted_trees
  if (layer_filter == "OS") {
    wt_to_agg <- weighted_trees |> dplyr::filter(.data$layer_key == 1)
  }

  # Determine grouping columns
  grp_cols <- if (layer_filter == "all") {
    c("stand_id", "species_group", "layer_key")
  } else {
    c("stand_id", "species_group")
  }

  # Per-stand aggregation (by species_group, optionally by layer_key)
  agg <- wt_to_agg |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::summarise(
      N_per_ha    = sum(.data$n_rep_ha_adj),
      G_per_ha_m2 = sum(.data$g_m2 * .data$n_rep_ha_adj),
      V_per_ha_m3 = ifelse(
        any(!is.na(.data$v_m3)),
        sum(.data$v_m3 * .data$n_rep_ha_adj, na.rm = TRUE),
        NA_real_
      ),
      d_q_cm      = ForestElementsR::d_q(.data$dbh_cm, .data$n_rep_ha_adj),
      alter_yr    = ifelse(
        any(!is.na(.data$age_yr)),
        stats::weighted.mean(.data$age_yr, .data$n_rep_ha_adj, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    # Divide sums by n_eff_plots to get per-ha values
    dplyr::left_join(n_eff, by = "stand_id") |>
    dplyr::mutate(
      N_per_ha    = .data$N_per_ha    / .data$n_eff_plots,
      G_per_ha_m2 = .data$G_per_ha_m2 / .data$n_eff_plots,
      V_per_ha_m3 = .data$V_per_ha_m3 / .data$n_eff_plots
    ) |>
    dplyr::select(-"n_eff_plots")

  # Mixture percentage (Anteil) based on basal area share within stand
  # Expand N/ha and V/ha to virtual monospecific ha
  agg <- agg |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::mutate(
      g_share     = .data$G_per_ha_m2 / sum(.data$G_per_ha_m2),
      anteil_pct  = round(.data$g_share * 100, 0),
      N_per_ha    = .data$N_per_ha    / .data$g_share,
      V_per_ha_m3 = .data$V_per_ha_m3 / .data$g_share
    ) |>
    dplyr::select(-"g_share") |>
    dplyr::ungroup()

  # Attach Zuwachs if provided
  if (!is.null(increment_data)) {
    # Select only the common columns for joining
    inc_join_cols <- c("stand_id", "species_group", "zuwachs_m3_ha_yr")
    agg <- dplyr::left_join(
      agg,
      increment_data[, intersect(names(increment_data), inc_join_cols)],
      by = c("stand_id", "species_group")
    )
    # In "all" layer mode, Zuwachs only applies to OS (layer_key == 1)
    if (layer_filter == "all") {
      agg <- agg |>
        dplyr::mutate(
          zuwachs_m3_ha_yr = ifelse(
            .data$layer_key == 1, .data$zuwachs_m3_ha_yr, NA_real_
          )
        )
    }
  } else {
    agg <- dplyr::mutate(agg, zuwachs_m3_ha_yr = NA_real_)
  }

  # Attach Bonitaet (site_index) and Bestockungsgrad if ytable_data provided
  if (!is.null(ytable_data)) {
    yt_cols <- ytable_data |>
      dplyr::select(
        "stand_id", "species_group", "site_index", "stocking_level"
      )
    agg <- dplyr::left_join(agg, yt_cols, by = c("stand_id", "species_group"))

    # In "all" layer mode, site_index/stocking_level only apply to OS
    if (layer_filter == "all") {
      agg <- agg |>
        dplyr::mutate(
          site_index     = ifelse(
            .data$layer_key == 1, .data$site_index, NA_real_
          ),
          stocking_level = ifelse(
            .data$layer_key == 1, .data$stocking_level, NA_real_
          )
        )
    }

    # Compute stand-level Bestockungsgrad (weighted average by G_per_ha_m2)
    stand_bestockungsgrad <- agg |>
      dplyr::filter(!is.na(.data$stocking_level)) |>
      dplyr::group_by(.data$stand_id) |>
      dplyr::summarise(
        stand_bestockungsgrad = round(stats::weighted.mean(
          .data$stocking_level, .data$G_per_ha_m2, na.rm = TRUE
        ), 2),
        .groups = "drop"
      )
    agg <- dplyr::left_join(agg, stand_bestockungsgrad, by = "stand_id")
  } else {
    agg <- agg |>
      dplyr::mutate(
        site_index            = NA_real_,
        stocking_level        = NA_real_,
        stand_bestockungsgrad = NA_real_
      )
  }

  # Round for display
  agg <- agg |>
    dplyr::mutate(
      N_per_ha    = round(.data$N_per_ha,    0),
      G_per_ha_m2 = round(.data$G_per_ha_m2, 1),
      V_per_ha_m3 = round(.data$V_per_ha_m3, 0),
      d_q_cm      = round(.data$d_q_cm,      1),
      alter_yr    = round(.data$alter_yr,     0)
    )

  # Join stand areas
  agg <- dplyr::left_join(agg, stand_areas, by = "stand_id")

  # Species Profile Index (Pretzsch) -- all non-removal trees with heights
  spi_per_stand <- purrr::map_dfr(unique(weighted_trees$stand_id), function(sid) {
    wt <- dplyr::filter(weighted_trees,
                        .data$stand_id == sid,
                        !.data$removal,
                        !is.na(.data$height_m))
    spi_val <- if (nrow(wt) < 2L) NA_real_ else {
      tryCatch(
        round(ForestElementsR::species_profile(
          species_id = wt$species_id,
          heights    = wt$height_m,
          weights    = wt$n_rep_ha_adj
        ), 2),
        error = function(e) NA_real_
      )
    }
    tibble::tibble(stand_id = sid, spi = spi_val)
  })
  agg <- dplyr::left_join(agg, spi_per_stand, by = "stand_id")

  # Split into a named list, one element per stand
  stands_present <- unique(agg$stand_id)
  result <- purrr::set_names(
    purrr::map(stands_present, function(sid) {
      dplyr::filter(agg, .data$stand_id == sid)
    }),
    stands_present
  )

  result
}


# -----------------------------------------------------------------------------
# 5.  revierbuch_increment_per_stand()  --  internal helper
# -----------------------------------------------------------------------------

#' Aggregate Repeated-Survey Increment to Per-Stand Level
#'
#' Works only for sample inventories of concentric sample circles
#' (\code{fe_ccircle_spatial}). Uses the \code{"rep_trans"} representative-tree
#' method, whose per-tree increments cannot become negative even for the small
#' samples a single stand contributes -- important here because a stand register
#' aggregates only a few plots per stand. See
#' \code{\link{inv_increment_repsurv_ccirc}} for the representative-tree
#' increment methods (\code{rep_classic}, \code{rep_mean}, \code{rep_end},
#' \code{rep_trans}).
#'
#' @param fe_inv      The inventory the stand register reports on (the later,
#'   current survey; the increment's second survey).
#' @param fe_inv_prev The previous (earlier) \code{\link{fe_inventory}}; the
#'   increment is computed over the last completed period
#'   \code{fe_inv_prev -> fe_inv}.
#' @param fractions Output of \code{revierbuch_circle_fractions()}.
#'
#' @return A tibble with columns \code{stand_id}, \code{species_group},
#'   \code{zuwachs_m3_ha_yr}.
#'
#' @keywords internal
#' @noRd
revierbuch_increment_per_stand <- function(fe_inv, fe_inv_prev, fractions) {

  # Subset both inventories to only plots that overlap with the stands
  plots_needed    <- unique(fractions$plot_id)
  fe_inv_sub      <- fe_inv[fe_inv$plot_id           %in% plots_needed, ]
  fe_inv_prev_sub <- fe_inv_prev[fe_inv_prev$plot_id %in% plots_needed, ]

  # Prepare tree data for both surveys (on the subsets)
  prep_trees <- function(inv) {
    inv |>
      pull_trees(.progress = FALSE) |>
      height_complete_inventory(method = "NFI") |>
      dplyr::mutate(
        height_m = ifelse(!.data$h_m_tree, .data$h_est_m, .data$height_m)
      ) |>
      trees_add_essentials(method = "BaySF")
  }

  trees_1st <- prep_trees(fe_inv_prev_sub)  # earlier survey
  trees_2nd <- prep_trees(fe_inv_sub)       # later survey = the reported inventory

  # Backward look: the reported inventory (fe_inv) is the SECOND survey, so the
  # increment is that of the last completed period (fe_inv_prev -> fe_inv).
  # rep_trans keeps per-tree increments non-negative even for small samples.
  inc <- inv_increment_repsurv_ccirc(
    inv_1st       = fe_inv_prev_sub,
    inv_2nd       = fe_inv_sub,
    inv_1st_trees = trees_1st,
    inv_2nd_trees = trees_2nd,
    method        = "rep_trans",
    progress_bar  = FALSE
  )

  # Largest-circle fractions for plot weighting
  n_eff <- fractions |>
    dplyr::filter(.data$is_largest_circle) |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::summarise(n_eff_plots = sum(.data$fraction), .groups = "drop")

  # inc$tree_increments has: plot_id (from the 2nd survey = the reported
  # inventory fe_inv, so it matches the stand fractions built from fe_inv),
  # species_id, iv_hub_m3_ha_yr_rep (annual increment per ha from this tree)
  inc_trees <- inc$tree_increments |>
    dplyr::mutate(
      species_group = ForestElementsR::as_fe_species_bavrn_state_short(
        .data$species_id
      )
    )

  # Join with largest-circle fractions
  inc_plot <- inc_trees |>
    dplyr::left_join(
      fractions |>
        dplyr::filter(.data$is_largest_circle) |>
        dplyr::select("plot_id", "stand_id", "fraction"),
      by = "plot_id"
    ) |>
    dplyr::filter(!is.na(.data$stand_id))

  # Per-stand x species_group weighted increment
  inc_plot |>
    dplyr::group_by(.data$stand_id, .data$species_group) |>
    dplyr::summarise(
      zuwachs_sum = sum(
        .data$iv_hub_m3_ha_yr_rep * .data$fraction, na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(n_eff, by = "stand_id") |>
    dplyr::mutate(
      zuwachs_m3_ha_yr = round(.data$zuwachs_sum / .data$n_eff_plots, 1)
    ) |>
    dplyr::select("stand_id", "species_group", "zuwachs_m3_ha_yr")
}


# -----------------------------------------------------------------------------
# 6.  revierbuch_increment_gnfi_per_stand()  --  internal helper
# -----------------------------------------------------------------------------

#' Aggregate GNFI-Based Increment to Per-Stand Level
#'
#' Uses the single-tree growth functions from the 3rd German National Forest
#' Inventory to estimate volume increment for each tree, then aggregates per
#' stand and species group.
#'
#' @param weighted_trees Output of \code{revierbuch_weighted_trees()}.
#' @param fractions      Output of \code{revierbuch_circle_fractions()}.
#' @param dt             Numeric; time span in years for the forward projection
#'   (default 5).
#'
#' @return A tibble with columns \code{stand_id}, \code{species_group},
#'   \code{zuwachs_m3_ha_yr}.
#'
#' @keywords internal
#' @noRd
revierbuch_increment_gnfi_per_stand <- function(weighted_trees, fractions,
                                                dt = 5) {

  # n_eff per stand (sum of largest-circle fractions)
  n_eff <- fractions |>
    dplyr::filter(.data$is_largest_circle) |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::summarise(n_eff_plots = sum(.data$fraction), .groups = "drop")

  # Filter to main stand (layer_key == 1) and non-removal trees
  wt_main <- weighted_trees |>
    dplyr::filter(.data$layer_key == 1, !.data$removal)

  # Apply per-tree GNFI increment
  wt_inc <- tree_inc_gnfi_2012(wt_main, dt)

  # Aggregate per stand x species_group
  agg <- wt_inc |>
    dplyr::group_by(.data$stand_id, .data$species_group) |>
    dplyr::summarise(
      zuwachs_sum = sum(
        .data$iv_hub_m3_yr_gnfi3 * .data$n_rep_ha_adj, na.rm = TRUE
      ),
      standing_area_sum = sum(
        .data$standing_area_m2 * .data$n_rep_ha_adj
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(n_eff, by = "stand_id") |>
    dplyr::mutate(
      # Per-ha values (actual mixed stand)
      zuwachs_mixed     = .data$zuwachs_sum / .data$n_eff_plots,
      standing_area_ha  = .data$standing_area_sum / .data$n_eff_plots
    )

  # Expand to virtual monospecific ha (same as ha_inc_main_stand_age_class)
  agg |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::mutate(
      area_share       = .data$standing_area_ha /
        sum(.data$standing_area_ha),
      zuwachs_m3_ha_yr = round(.data$zuwachs_mixed / .data$area_share, 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::select("stand_id", "species_group", "zuwachs_m3_ha_yr")
}


# -----------------------------------------------------------------------------
# 7.  revierbuch_increment_ytables_per_stand()  --  internal helper
# -----------------------------------------------------------------------------

#' Aggregate Yield-Table-Based Increment to Per-Stand Level
#'
#' For each stand and species group, computes quadratic mean diameter, height,
#' and age, then looks up site index, stocking level, and periodic annual
#' increment from yield tables.
#'
#' @param weighted_trees  Output of \code{revierbuch_weighted_trees()}.
#' @param fractions       Output of \code{revierbuch_circle_fractions()}.
#' @param ytable_selection A data frame mapping species codes to yield table
#'   names (see \code{\link{inv_increment_ytables}}).
#'
#' @return A tibble with columns \code{stand_id}, \code{species_group},
#'   \code{zuwachs_m3_ha_yr}, \code{site_index}, \code{stocking_level}.
#'
#' @keywords internal
#' @noRd
revierbuch_increment_ytables_per_stand <- function(weighted_trees, fractions,
                                                   ytable_selection) {

  # n_eff per stand
  n_eff <- fractions |>
    dplyr::filter(.data$is_largest_circle) |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::summarise(n_eff_plots = sum(.data$fraction), .groups = "drop")

  # Filter to main stand (layer_key == 1) and non-removal trees
  wt_main <- weighted_trees |>
    dplyr::filter(.data$layer_key == 1, !.data$removal)

  # Aggregate per stand x species_id (not species_group, because ytable
  # lookup needs species_id)
  suppressWarnings(
    stand_sp <- wt_main |>
      dplyr::group_by(
        .data$stand_id, .data$species_id, .data$species_group
      ) |>
      dplyr::summarise(
        age_yr              = stats::weighted.mean(
          .data$age_yr, w = .data$n_rep_ha_adj, na.rm = TRUE
        ),
        d_q_cm              = ForestElementsR::d_q(
          .data$dbh_cm, n_rep = .data$n_rep_ha_adj
        ),
        h_q_m               = ForestElementsR::h_q(
          .data$height_m, .data$dbh_cm, n_rep = .data$n_rep_ha_adj
        ),
        standing_area_m2_ha = sum(
          .data$standing_area_m2 * .data$n_rep_ha_adj
        ),
        g_m2_ha             = sum(.data$g_m2 * .data$n_rep_ha_adj),
        .groups = "drop"
      ) |>
      # Divide by n_eff_plots to get per-ha values
      dplyr::left_join(n_eff, by = "stand_id") |>
      dplyr::mutate(
        standing_area_m2_ha = .data$standing_area_m2_ha / .data$n_eff_plots,
        g_m2_ha             = .data$g_m2_ha / .data$n_eff_plots
      )
  )

  # Compute area share per species within each stand
  stand_sp <- stand_sp |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::mutate(
      area_share = .data$standing_area_m2_ha /
        sum(.data$standing_area_m2_ha),
      # Expanded basal area to virtual monospecific ha
      g_m2_expd_ha = .data$g_m2_ha / .data$area_share
    ) |>
    dplyr::ungroup()

  # Join yield table selection
  suppressWarnings(
    stand_sp <- stand_sp |>
      dplyr::left_join(ytable_selection, by = "species_id")
  )

  # Skip species without yield tables
  stand_sp <- stand_sp |>
    dplyr::filter(!is.na(.data$ytable_name))

  if (nrow(stand_sp) == 0) {
    return(tibble::tibble(
      stand_id        = character(),
      species_group   = structure(character(), class = class(
        weighted_trees$species_group
      )),
      zuwachs_m3_ha_yr = numeric(),
      site_index       = numeric(),
      stocking_level   = numeric()
    ))
  }

  # Compute site index (same logic as apply_ytables())
  stand_sp <- stand_sp |>
    dplyr::mutate(
      site_index = purrr::pmap_dbl(
        .l = list(
          age = .data$age_yr, h_q_m = .data$h_q_m, ytable = .data$ytable_name
        ),
        .f = function(age, h_q_m, ytable) {
          ForestElementsR::site_index(
            age, h_q_m,
            eval(parse(text = paste0("ForestElementsR::", ytable))),
            "h_q_m"
          )
        }
      ),
      # Clamp site index to valid range
      site_index = purrr::map2_dbl(
        .x = .data$site_index,
        .y = .data$ytable_name,
        .f = function(.x, .y) {
          if (.y != "fe_ytable_spruce_assmann_franz_mean_yield_level_1963") {
            max_si <- eval(parse(text = paste0(
              "max(ForestElementsR::", .y, "$site_indexes)"
            )))
            min(max(-.x, -1), max_si)
            # clamp to [-1, max_si]
            max(min(.x, max_si), -1)
          } else {
            min_si <- eval(parse(text = paste0(
              "min(ForestElementsR::", .y, "$site_indexes)"
            )))
            max(min(.x, 44), min_si)
          }
        }
      )
    )

  # Compute stocking level
  stand_sp <- stand_sp |>
    dplyr::mutate(
      stocking_level = purrr::pmap_dbl(
        .l = list(
          ba = .data$g_m2_expd_ha, age = .data$age_yr,
          si = .data$site_index, ytable = .data$ytable_name
        ),
        .f = function(ba, age, si, ytable) {
          ForestElementsR::stocking_level(
            ba, age, si,
            eval(parse(text = paste0("ForestElementsR::", ytable)))
          )
        }
      )
    )

  # Lookup increment from yield table
  stand_sp <- stand_sp |>
    dplyr::mutate(
      zuwachs_m3_ha_yr = purrr::pmap_dbl(
        .l = list(
          age = .data$age_yr, si = .data$site_index,
          ytable = .data$ytable_name, sl = .data$stocking_level
        ),
        .f = function(age, si, ytable, sl) {
          ForestElementsR::ytable_lookup(
            age, si, "pai_m3_ha_yr",
            eval(parse(text = paste0("ForestElementsR::", ytable)))
          ) * min(1, sl)
        }
      ),
      # Reduce to harvested under bark
      zuwachs_m3_ha_yr = ForestElementsR::v_red_harvest_ubark(
        .data$species_group, .data$zuwachs_m3_ha_yr
      ),
      zuwachs_m3_ha_yr = round(.data$zuwachs_m3_ha_yr, 1)
    )

  # Aggregate from species_id level to species_group level
  # (weighted by area_share within each stand)
  stand_sp |>
    dplyr::group_by(.data$stand_id, .data$species_group) |>
    dplyr::summarise(
      zuwachs_m3_ha_yr = stats::weighted.mean(
        .data$zuwachs_m3_ha_yr, .data$area_share, na.rm = TRUE
      ),
      site_index = stats::weighted.mean(
        .data$site_index, .data$area_share, na.rm = TRUE
      ),
      stocking_level = stats::weighted.mean(
        .data$stocking_level, .data$area_share, na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      zuwachs_m3_ha_yr = round(.data$zuwachs_m3_ha_yr, 1),
      site_index       = round(.data$site_index, 1),
      stocking_level   = round(.data$stocking_level, 2)
    )
}


# -----------------------------------------------------------------------------
# 8.  stand_register_pdf()  --  exported user function
# -----------------------------------------------------------------------------

#' Generate a Stand Register (Revierbuch) PDF From an fe_inventory
#'
#' Creates a multi-page PDF in which each page summarises the forest inventory
#' statistics of one stand.  Statistics include stem number, basal area,
#' standing volume, and quadratic mean DBH per tree species and canopy layer.
#' Optionally, annual volume increment, site index (Bonitaet), and stocking
#' level (Bestockungsgrad) are added.
#'
#' Stands can be delineated in two ways, and that choice selects the mode:
#' \describe{
#'   \item{Spatially, for sample inventories}{Supply \code{stands_sf}, an
#'     \code{sf} object of stand polygons, together with \code{stand_id_col}.
#'     Plots are assigned to stands by intersecting their sampling circles with
#'     the polygons.}
#'   \item{Tabularly, for stand-wise inventories}{Leave \code{stands_sf = NULL}.
#'     Stand membership is then taken from a table -- either
#'     \code{stand_id_lookup}, or, if that is \code{NULL} as well, from the
#'     stand id embedded in \code{plot_id} (\code{"<stpnr>_<stand_id>"}).
#'     Stand-level attributes come from \code{stand_data}, for instance the
#'     table returned by
#'     \code{\link{import_standwise_relascope_format1_metadata}}.  No geometry
#'     is needed.}
#' }
#'
#' @details
#' **Assigning plots to stands:** In the spatial mode, plots whose concentric
#' sampling circles straddle a stand boundary are split: each tree's
#' representation factor (\code{n_rep_ha}) is multiplied by the fraction of its
#' sampling circle that falls inside the stand, so a boundary plot contributes
#' proportionally to both adjacent stands.  In the stand-wise mode there is no
#' geometry and hence no boundary handling -- every angle-count point belongs
#' wholly to exactly one stand.
#'
#' **Volume calculation:** Heights are completed via
#' \code{\link{height_complete_inventory}} (NFI method) before volume is
#' estimated with \code{ForestElementsR::v_gri()}.
#'
#' **Increment estimation:** Increment is optional and can come from three
#' sources.  Note that only two of them are values of \code{increment_method};
#' the repeated-survey increment is selected by supplying \code{fe_inv_prev}.
#' \describe{
#'   \item{Repeated survey (\code{fe_inv_prev})}{Increment from two inventory
#'     periods.  Sample inventories only.}
#'   \item{\code{increment_method = "ytables"}}{Yield-table-based increment.
#'     Also computes per-species site index and stand-level stocking level.
#'     Requires \code{ytable_selection}.  Available in both modes.}
#'   \item{\code{increment_method = "gnfi"}}{Increment estimate with the
#'     single-tree growth functions of the 3rd German National Forest Inventory.
#'     Sample inventories only.}
#' }
#'
#' **Restrictions in the stand-wise mode:** \code{increment_method = "gnfi"}
#' raises an error, and a supplied \code{fe_inv_prev} raises a warning and is
#' ignored (the increment is then omitted).  Both need spatially explicit,
#' repeatedly measured single trees, which stand-wise angle-count inventories do
#' not provide.  Effectively, \code{"none"} and \code{"ytables"} remain.
#'
#' @param fe_inv       An \code{\link{fe_inventory}} object containing the
#'   (first) inventory.
#' @param stands_sf    An \code{sf} object with stand polygons, or \code{NULL}
#'   (default).  Supplying it selects the spatial mode; \code{NULL} selects the
#'   tabular, stand-wise mode, in which stand membership is read from
#'   \code{stand_id_lookup} or from the \code{plot_id} structure
#'   (\code{"<stpnr>_<stand_id>"}).
#' @param stand_id_col Character; name of the column that uniquely identifies a
#'   stand.  Required when \code{stands_sf} is provided, where it must name a
#'   column of \code{stands_sf}.  It is \strong{also} used as the stand-id
#'   column of \code{stand_data}, in both modes; when it is \code{NULL},
#'   \code{stand_data} is expected to carry a column named \code{stand_id}.
#' @param stand_id_lookup Optional plot-to-stand lookup for stand-wise inventories
#'   (\code{stands_sf = NULL}).  One of:
#'   \itemize{
#'     \item A named character vector \code{c(plot_id = stand_id, ...)}.
#'     \item A data frame / tibble with columns \code{plot_id} and
#'       \code{stand_id} (e.g.\ read from a CSV with
#'       \code{read.csv()} / \code{readr::read_csv()}).
#'     \item A single unnamed string -- aggregates \emph{all} plots into one
#'       stand of that name (the "all together" case).
#'   }
#'   When both \code{stands_sf} and \code{stand_id_lookup} are \code{NULL},
#'   stand IDs are extracted from the embedded \code{plot_id} suffix.  Plots
#'   not covered by an explicit lookup are dropped with a warning.
#' @param tab_title    Character; used in the PDF title and output filename.
#' @param output_dir   Path to the directory where the PDF will be written.
#'   Default is \code{NA}, which writes to the session's temporary directory.
#' @param fe_inv_prev  Optional \strong{previous} (earlier)
#'   \code{\link{fe_inventory}} object.  When provided, annual volume increment
#'   per stand is computed over the last completed period
#'   (\code{fe_inv_prev -> fe_inv}, a backward look) via
#'   \code{\link{inv_increment_repsurv_ccirc}} and added to the tables.  This
#'   repeated-survey increment works \strong{only} for sample inventories of
#'   concentric sample circles (\code{fe_ccircle_spatial}); for stand-wise
#'   inventories it is ignored with a warning.  The \code{"rep_trans"} method is
#'   used because its per-tree increments cannot become negative even for the
#'   small per-stand samples a stand register aggregates; see
#'   \code{\link{inv_increment_repsurv_ccirc}} for the available
#'   representative-tree increment methods.
#' @param ytable_selection Optional yield table selection data frame mapping
#'   species codes to yield table names.  When provided, site index (Bonitaet)
#'   and stocking level (Bestockungsgrad) are computed per species and stand.
#'   See \code{\link{inv_increment_ytables}} for the expected format.
#' @param increment_method Character; one of \code{"none"} (default),
#'   \code{"ytables"}, or \code{"gnfi"}.  Determines how model-based volume
#'   increment is estimated.  Ignored when \code{fe_inv_prev} is provided (which
#'   uses repeated-survey increment instead).  For stand-wise inventories
#'   (\code{stands_sf = NULL}) only \code{"none"} and \code{"ytables"} are
#'   valid; \code{"gnfi"} requires spatially explicit data and raises an error
#'   there.
#' @param dt Numeric; time span in years for the GNFI forward projection.
#'   Only used when \code{increment_method = "gnfi"}.  Default is 5.
#' @param stand_data   Optional data frame with stand-level metadata, printed in
#'   the header of each stand's page.  It must contain the stand-id column named
#'   by \code{stand_id_col} (or, when that is \code{NULL}, a column called
#'   \code{stand_id}), plus any metadata columns.  The columns
#'   \code{bestandsform}, \code{nutzungsart}, \code{beschreibung},
#'   \code{massnahmen}, \code{vj_ziel} and \code{bestockungsgrad} are displayed
#'   with proper labels; every other column appears under its raw name.  For
#'   stand-wise inventories this is typically the table returned by
#'   \code{\link{import_standwise_relascope_format1_metadata}}, whose stand key
#'   is named \code{bestand} -- pass \code{stand_id_col = "bestand"} in that
#'   case.
#' @param layer_filter Character; one of \code{"OS"} (default, main canopy
#'   layer only), \code{"all"} (split by all canopy layers), or
#'   \code{"gesamt"} (aggregate across all layers into one row per species).
#'
#' @return The path to the rendered PDF file (invisibly).
#'
#' Deliberately not a member of the pdf_output family: the Revierbuch is a
#' post-CRAN feature, so this function must not appear in the cross-links of the
#' user-facing PDF functions.
#'
#' @family revierbuch
#'
#' @keywords internal
#'
stand_register_pdf <- function(fe_inv,
                            stands_sf        = NULL,
                            stand_id_col     = NULL,
                            tab_title,
                            output_dir       = NA,
                            fe_inv_prev      = NULL,
                            ytable_selection = NULL,
                            increment_method = c("none", "ytables", "gnfi"),
                            dt               = 5,
                            stand_data       = NULL,
                            stand_id_lookup  = NULL,
                            layer_filter     = c("OS", "all", "gesamt")) {

  check_pdf_dependencies("stand_register_pdf")

  stopifnot(is_fe_inventory(fe_inv))

  is_wzp <- is.null(stands_sf)

  if (!is_wzp) {
    stopifnot(inherits(stands_sf, "sf"))
    stopifnot(is.character(stand_id_col), length(stand_id_col) == 1)
    stopifnot(stand_id_col %in% names(stands_sf))
  }

  # The mode is inferred from the absence of `stands_sf`, but the inventory
  # itself knows what it is. Cross-check the two, otherwise a sample inventory
  # passed without `stands_sf` would silently be treated as stand-wise: stand
  # ids would be parsed out of `plot_id`, yielding a plausible-looking but
  # meaningless stand register.
  is_ccircle <- any(vapply(
    fe_inv$plot,
    function(p) inherits(p, "fe_ccircle_spatial") ||
                inherits(p, "fe_ccircle_spatial_notrees"),
    logical(1)
  ))

  if (is_wzp && is_ccircle && is.null(stand_id_lookup)) {
    stop(
      "fe_inv holds concentric sample circles, but neither stands_sf nor ",
      "stand_id_lookup was supplied. Pass stands_sf to delineate stands ",
      "spatially, or stand_id_lookup to assign plots to stands from a table. ",
      "Without either, stand ids would be taken from the plot_id suffix, ",
      "which is meaningful only for stand-wise inventories.",
      call. = FALSE
    )
  }
  if (!is_wzp && !is_ccircle) {
    stop(
      "stands_sf was supplied, but fe_inv carries no concentric sample ",
      "circles (it is a stand-wise inventory). Stand polygons cannot be ",
      "intersected with its plots. Omit stands_sf and use stand_id_lookup, ",
      "or rely on the stand id embedded in plot_id.",
      call. = FALSE
    )
  }

  increment_method <- match.arg(increment_method)
  layer_filter     <- match.arg(layer_filter)

  if (increment_method == "ytables" && is.null(ytable_selection)) {
    stop("ytable_selection is required when increment_method = 'ytables'.",
         call. = FALSE)
  }

  # Stand-wise inventories carry no spatial / repeated-survey
  # information, so single-tree increment models cannot be applied. Only
  # yield-table-based increment is valid here.
  if (is_wzp && increment_method == "gnfi") {
    stop(
      "For stand-wise inventories, only yield-table-based increment ",
      "is supported (increment_method = \"ytables\"). The single-tree GNFI ",
      "model requires spatially explicit data that stand-wise inventories ",
      "do not provide.",
      call. = FALSE
    )
  }

  if (is.na(output_dir)) output_dir <- tempdir()
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  tab_title <- as.character(tab_title)

  # -- Fractions and weighted trees -------------------------------------------
  if (is_wzp) {
    message("Assigning plots to stands (stand-wise mode) ...")
    fractions <- revierbuch_wzp_fractions(fe_inv, stand_id_lookup)
    message("Pulling and weighting trees ...")
    wtrees    <- revierbuch_wzp_weighted_trees(fe_inv, fractions)
  } else {
    message("Computing circle-stand overlap fractions ...")
    fractions <- revierbuch_circle_fractions(fe_inv, stands_sf, stand_id_col)
    if (nrow(fractions) == 0) {
      stop("No plots overlap with the provided stands. Cannot generate PDF.",
           call. = FALSE)
    }
    message("Pulling and weighting trees ...")
    wtrees <- revierbuch_weighted_trees(fe_inv, fractions)
  }

  # -- Increment computation --------------------------------------------------
  inc_data   <- NULL
  yt_data    <- NULL

  if (!is.null(fe_inv_prev)) {
    if (is_wzp) {
      warning(
        "fe_inv_prev (repeated-survey increment) is not supported for ",
        "stand-wise inventories; it works only for concentric sample ",
        "circles. Increment will be omitted.",
        call. = FALSE
      )
    } else {
      stopifnot(is_fe_inventory(fe_inv_prev))
      message("Computing increment from previous inventory ",
              "(repeated survey, rep_trans) ...")
      inc_data <- revierbuch_increment_per_stand(fe_inv, fe_inv_prev, fractions)
    }

  } else if (increment_method == "ytables") {
    message("Computing yield-table-based increment ...")
    yt_data <- revierbuch_increment_ytables_per_stand(
      wtrees, fractions, ytable_selection
    )
    inc_data <- yt_data  # contains zuwachs_m3_ha_yr + site_index + stocking_level

  } else if (increment_method == "gnfi") {
    message("Computing GNFI-based increment (dt = ", dt, " yr) ...")
    inc_data <- revierbuch_increment_gnfi_per_stand(wtrees, fractions, dt)
  }

  # If ytable_selection is provided but method is not "ytables",
  # still compute site_index and stocking_level for display
  if (!is.null(ytable_selection) && is.null(yt_data)) {
    message("Computing site index and stocking level from yield tables ...")
    yt_data <- revierbuch_increment_ytables_per_stand(
      wtrees, fractions, ytable_selection
    )
  }

  # -- Build per-stand summary tables -----------------------------------------
  message("Building per-stand summary tables ...")
  stand_list <- revierbuch_build_stand_tables(
    weighted_trees = wtrees,
    fractions      = fractions,
    increment_data = inc_data,
    ytable_data    = yt_data,
    layer_filter   = layer_filter
  )

  has_zuwachs  <- !is.null(inc_data)
  has_bonitaet <- !is.null(yt_data)

  # Determine increment method label for display
  increment_label <- NULL
  if (!is.null(fe_inv_prev)) {
    increment_label <- "Wiederholungsaufnahme"
  } else if (increment_method == "ytables") {
    increment_label <- "Ertragstafeln"
  } else if (increment_method == "gnfi") {
    increment_label <- "BWI 3 (GNFI)"
  }

  # Layer filter label for display
  layer_label <- switch(layer_filter,
    OS     = "Oberschicht",
    all    = "Alle Schichten",
    gesamt = "Gesamt"
  )

  # -- Prepare stand metadata for Rmd -----------------------------------------
  # Label lookup for recognized column names
  label_map <- c(
    bestandsform    = "Bestandsform",
    nutzungsart     = "Nutzungsart",
    beschreibung    = "Beschreibung",
    massnahmen      = "Massnahmen",
    vj_ziel         = "VJ-Ziel",
    bestockungsgrad = "Bestockungsgrad"
  )

  # For WZP, stand_id_col defaults to "stand_id" for stand_data lookup
  meta_id_col <- if (!is.null(stand_id_col)) stand_id_col else "stand_id"

  stand_metadata <- NULL
  if (!is.null(stand_data)) {
    stopifnot(is.data.frame(stand_data))
    stopifnot(meta_id_col %in% names(stand_data))

    meta_cols <- setdiff(names(stand_data), meta_id_col)
    stand_ids <- stand_data[[meta_id_col]]

    stand_metadata <- purrr::set_names(
      purrr::map(seq_len(nrow(stand_data)), function(i) {
        vals <- purrr::map(meta_cols, function(col) {
          v <- stand_data[[col]][i]
          if (is.na(v) || v == "") return(NULL)
          label <- if (col %in% names(label_map)) label_map[[col]] else col
          list(label = label, value = as.character(v))
        })
        purrr::compact(vals)
      }),
      stand_ids
    )
  }

  # -- Render PDF -------------------------------------------------------------
  message("Rendering PDF ...")
  outfile <- paste0("revierbuch_ger_", tab_title, ".pdf")

  path <- rmarkdown::render(
    input = system.file("rmd/revierbuch_stand_ger.Rmd", package = "FeNEU"),
    params = list(
      stand_list      = stand_list,
      title           = tab_title,
      has_zuwachs     = has_zuwachs,
      has_bonitaet    = has_bonitaet,
      stand_metadata  = stand_metadata,
      increment_label = increment_label,
      layer_label     = layer_label
    ),
    output_format = "pdf_document",
    output_file   = outfile,
    output_dir    = output_dir,
    quiet         = TRUE
  )

  message("PDF written to: ", path)
  invisible(path)
}

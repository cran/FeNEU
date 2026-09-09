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
#' Add Essential Information to Deadwood Data
#'
#' Adds representative plot area (from an \code{fe_inventory} object) and
#' fixed L46 diameter classes to a deadwood tibble imported from BaySF
#' \code{fdinvtot}. No further transformations are performed; volume and
#' number per hectare are already provided in the raw file.
#'
#' @param deadwood_dat Tibble containing deadwood data imported and renamed
#'   from fdinvtot.txt. Must include \code{plot_id} and \code{d_mean_cm}.
#' @param inventory_plots Tibble containing plot-level data from an
#'   \code{fe_inventory}
#'
#' @return Tibble identical to \code{deadwood_dat} but with two additional
#'   variables:
#'   \describe{
#'     \item{area_rep_ha}{Representative area of the sample plot (ha)}
#'     \item{dclass_46}{L46 diameter class (I, II, III)}
#'   }
#'
#' @keywords internal
#'
deadwood_add_essentials <- function(deadwood_dat, inventory_plots) {


  dead_dbh_breaks <- c(0,36,48,999)


  deadwood_dat |>
    # bring in representative area for each plot
    dplyr::left_join(
      inventory_plots |>
        dplyr::select("plot_id", "area_rep_ha"),
      by = "plot_id"
    ) |>
    # add the fixed L46 diameter classes
    dplyr::mutate(
      dclass_46 = cut(.data$d_mean_cm, dead_dbh_breaks)
    )

}




#' Aggregation of Deadwood Information on User-Defined Grouping Level
#'
#' Aggregates deadwood data (volume and number per hectare) to any grouping
#' structure defined by the user. The function mirrors the structure of
#' \code{single_tree_aggregation()} for live trees but uses ha-based deadwood
#' metrics. Output values are sums of volume and number per hectare and the
#' corresponding quadratic mean diameter.
#'
#' @param dead_trees_ext Tibble containing deadwood objects with essential
#'   variables (d_mean_cm, v_ha, n_ha).
#' @param ... Grouping variables passed to \code{dplyr::group_by()}.
#'
#' @return Tibble with aggregated deadwood information on the defined grouping
#'   level.
#'
#' @keywords internal
#'
single_dead_tree_aggregation <- function(dead_trees_ext,
                                         ...
                                         ) {

  dead_trees_agg <- dead_trees_ext |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      v_hub_m3 = sum(.data$v_ha),
      n_rep_ha = sum(.data$n_ha),
      d_q_cm   = ForestElementsR::d_q(
        .data$d_mean_cm,
        .data$n_ha
      ),
      .groups = "drop"
    )

  dead_trees_agg
}


#' Deadwood summary table by species group and diameter class
#'
#' Produces a structural deadwood summary at the level of species groups and
#' fixed diameter classes. All metrics are calculated in absolute values
#' (volume and number) based on representative area and then converted to
#' percentages on state and species-group level. The resulting table is still
#' in long format; reshaping to an L46-like wide table is done by
#' \code{structure_deadwood_output_table()}.
#'
#' @param dead_trees_plus Tibble with preprocessed deadwood data including
#'   \code{plot_id}, \code{dw_state}, \code{species_group}, \code{dclass_46},
#'   \code{v_ha}, \code{n_ha}, and \code{area_rep_ha}.
#' @param tree_filter Logical expression selecting the deadwood objects to be
#'   used. Passed to \code{dplyr::filter()}.
#'
#' @return A list with two tibbles:
#'   \describe{
#'     \item{detail}{Long-format table grouped by state × species group ×
#'       diameter class, including absolute metrics, percentages, and
#'       expansion variables.}
#'     \item{total}{State × species-group totals (absolute volume and number).}
#'   }
#'
#' @keywords internal
#'
structure_table_deadwood_dclass <- function(dead_trees_plus,
                                            tree_filter = TRUE) {

  # Filter data
  work_dat <- dead_trees_plus |>
    dplyr::filter({{ tree_filter }}) |>
    dplyr::ungroup()

  # Add a group for all classes together (dw_state = 100)
  work_dat <- dplyr::bind_rows(
    work_dat,
    work_dat |> dplyr::mutate(dw_state = 100)
  )

  # 1) Absolute values per state × species × diameter class
  group_abs <- work_dat |>
    dplyr::group_by(
      .data$dw_state,
      .data$species_group,
      .data$dclass_46
    ) |>
    dplyr::summarise(
      v_m3        = sum(.data$v_ha * .data$area_rep_ha),   # absolute volume
      n_stems     = sum(.data$n_ha * .data$area_rep_ha),   # absolute stems
      n_points    = dplyr::n_distinct(.data$plot_id),
      area_ha     = sum(.data$area_rep_ha),
      .groups     = "drop"
    )

  # Global totals from the "all classes together" group (dw_state = 100)
  globals <- group_abs |>
    dplyr::filter(.data$dw_state == 100) |>
    dplyr::summarise(
      total_volume = sum(.data$v_m3,    na.rm = TRUE),
      total_N      = sum(.data$n_stems, na.rm = TRUE)
    )

  total_volume <- globals$total_volume
  total_N      <- globals$total_N

  # 2) Species-level totals (per state × species group) - for the 'total' table
  species_totals <- group_abs |>
    dplyr::group_by(.data$dw_state, .data$species_group) |>
    dplyr::summarise(
      v_m3_species    = sum(.data$v_m3),
      n_stems_species = sum(.data$n_stems),
      area_ha_species = sum(.data$area_ha),
      .groups         = "drop"
    )

  # 3) State-level totals (per state across all species)
  state_totals <- group_abs |>
    dplyr::group_by(.data$dw_state) |>
    dplyr::summarise(
      v_m3_state    = sum(.data$v_m3),
      n_stems_state = sum(.data$n_stems),
      .groups       = "drop"
    )

  # 4) Start "detail" without percentages: just attach totals
  detail <- group_abs |>
    dplyr::left_join(
      species_totals,
      by = c("dw_state", "species_group")
    ) |>
    dplyr::left_join(
      state_totals,
      by = "dw_state"
    )

  # 5) Horizontal sums for all species groups (BaGr-Summen pro Klasse)
  species_group_total <- detail |>
    dplyr::group_by(.data$dw_state, .data$dclass_46) |>
    dplyr::summarise(
      v_m3        = sum(.data$v_m3),
      n_stems     = sum(.data$n_stems),
      n_points    = sum(.data$n_points),
      area_ha     = sum(.data$area_ha),
      .groups     = "drop"
    ) |>
    dplyr::mutate(
      species_group = 999
    ) |>
    dplyr::left_join(
      state_totals,
      by = "dw_state"
    )

  # 6) Combine real detail + species-total rows (STILL without %)
  detail <- dplyr::bind_rows(detail, species_group_total)

  # 7) Now compute all percentages in ONE place

  # 7a) %Ges (global) – same denominator for everyone
  detail <- detail |>
    dplyr::mutate(
      v_m3_pct_state = 100 * .data$v_m3    / total_volume,
      n_pct_state    = 100 * .data$n_stems / total_N
    )

  # 7b) %Ba (within Baumartengruppe) – sums to 100 horizontally per BaGr
  detail <- detail |>
    dplyr::group_by(.data$dw_state, .data$species_group) |>
    dplyr::mutate(
      v_m3_pct_species = ifelse(
        sum(.data$v_m3, na.rm = TRUE) > 0,
        100 * .data$v_m3    / sum(.data$v_m3,    na.rm = TRUE),
        NA_real_
      ),
      n_pct_species = ifelse(
        sum(.data$n_stems, na.rm = TRUE) > 0,
        100 * .data$n_stems / sum(.data$n_stems, na.rm = TRUE),
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  # 8) Compact species-total table (state × species group)
  total <- species_totals |>
    dplyr::left_join(
      state_totals,
      by = "dw_state"
    )

  # Return both
  list(
    detail = detail,
    total  = total
  )
}





#' Restructure Deadwood Detail Table to L46-style Output Table
#'
#' Takes the long-format detail table from
#' \code{structure_table_deadwood_dclass()} and produces a wide-format
#' structure table similar to the L_46 deadwood tables of the Bavarian
#' State Forests. The function reshapes diameter classes into columns,
#' computes totals for volume and number, and adds plot-level expansion
#' variables (points and representative area). No readable formatting is
#' applied at this stage.
#'
#' @param deadwood_structure_table Long-format structure table table from
#'   \code{structure_table_deadwood_dclass()} containing deadwood metrics per
#'   state × species group × diameter class.
#' @param dead_trees_plus The preprocessed deadwood tibble containing
#'   \code{plot_id} and \code{area_rep_ha} (after \code{deadwood_add_essentials()}).
#'
#' @return A wide-format tibble with one row per state × species group and
#'   one block of columns per diameter class, plus totals and plot-level
#'   expansion variables.
#'
#' @keywords internal
#'
structure_deadwood_output_table <- function(deadwood_structure_table,
                                            dead_trees_plus) {

  detail <- deadwood_structure_table$detail

  n_points_global <- dead_trees_plus |>
    dplyr::distinct(.data$plot_id, .keep_all = TRUE) |>
    dplyr::summarise(
      n_total     = dplyr::n(),                # number of distinct points
      area_total  = sum(.data$area_rep_ha)     # total representative area
    )

  # 1. Compute Punkte and Total Area per dw_state × species_group (state-wise)
  point_area_state <- dead_trees_plus |>
    dplyr::group_by(.data$dw_state, .data$species_group) |>
    dplyr::summarise(
      n_points = dplyr::n_distinct(.data$plot_id),
      area_ha  = sum(.data$area_rep_ha),
      .groups  = "drop"
    )

  # 1b. Gesamt (dw_state = 100) per species_group across all states
  point_area_total_species <- dead_trees_plus |>
    dplyr::group_by(.data$species_group) |>
    dplyr::summarise(
      n_points = dplyr::n_distinct(.data$plot_id),
      area_ha  = sum(.data$area_rep_ha),
      .groups  = "drop"
    ) |>
    dplyr::mutate(dw_state = 100) |>
    dplyr::relocate("dw_state", "species_group")

  # 1c. Gesamt + Summe (dw_state = 100, species_group = 999) – overall totals
  point_area_total_summe <- dead_trees_plus |>
    dplyr::summarise(
      n_points = dplyr::n_distinct(.data$plot_id),
      area_ha  = sum(.data$area_rep_ha),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      dw_state      = 100,
      species_group = 999
    ) |>
    dplyr::relocate("dw_state", "species_group")

  # 1d. Combine all point/area info
  point_area_info <- dplyr::bind_rows(
    point_area_state,
    point_area_total_species,
    point_area_total_summe
  )

  # 2. Drop d_q_cm columns if present
  detail <- detail |>
    dplyr::select(-dplyr::matches("^d_q_cm"), dplyr::everything())

  # 3. Pivot metrics to long form
  metrics_long <- detail |>
    tidyr::pivot_longer(
      cols = c(
        .data$v_m3,
        .data$v_m3_pct_state,
        .data$v_m3_pct_species,
        .data$n_stems,
        .data$n_pct_state,
        .data$n_pct_species
      ),
      names_to  = "metric",
      values_to = "value"
    )

  # Ensure factor ordering of diameter classes
  dbh_classes <- sort(unique(as.character(metrics_long$dclass_46)))

  # 4. Pivot diameter classes into wide blocks
  metrics_wide <- metrics_long |>
    tidyr::pivot_wider(
      id_cols     = c(.data$dw_state, .data$species_group),
      names_from  = c(.data$metric, .data$dclass_46),
      values_from = .data$value,
      names_glue  = "{metric}_{dclass_46}"
    )

  # 5. Compute totals: only for v_m3_* columns (Summe über Durchmesserklassen)
  vol_cols <- grep("^v_m3_", names(metrics_wide), value = TRUE)
  n_cols <- grep("^n_stems_", names(metrics_wide), value = TRUE)

  metrics_wide <- metrics_wide |>
    dplyr::mutate(
      total_v_m3    = rowSums(dplyr::across(dplyr::all_of(vol_cols)), na.rm = TRUE),
      total_n_stems = rowSums(dplyr::across(dplyr::all_of(n_cols)),  na.rm = TRUE)
    )


  # 6. Join Punkte and Fläche to wide table
  final_out <- metrics_wide |>
    dplyr::left_join(
      point_area_info,
      by = c("dw_state", "species_group")
    )

  final_out |>
    dplyr::mutate(
      n_points_tot = n_points_global$n_total,
      area_ha_tot  = n_points_global$area_total
    )

}





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
#' Add Essential Information to Regeneration Data
#'
#' `r lifecycle::badge("experimental")`
#'
#' Adds fixed L43 height classes and a main-stand mixture type to a regeneration
#' tibble created from BaySF tree data via \code{pull_trees(..., small_trees = TRUE)}.
#' No further transformations of stem numbers are performed.
#'
#' The height classes follow Bavarian forests standard Table L43:
#' \itemize{
#'   \item{0.2 m - 1.3 m}
#'   \item{1.3 m - 5.0 m}
#' }
#'
#' The mixture type is joined in from plot-level information derived from the
#' main stand (e.g. trees with \code{layer_key} 1 and 2), typically computed
#' using \code{pull_trees(..., small_trees = FALSE)}.
#'
#' @param inv_trees_reg Tibble containing regeneration trees from
#'   \code{pull_trees(..., small_trees = TRUE)}. Must include at least
#'   \code{plot_id} and \code{height_m}.
#'
#' @param method Character string selecting the species grouping (coding)
#'   applied to \code{species_id}. \code{"BaySF"} uses the Bavarian State
#'   Forest short coding (\code{as_fe_species_bavrn_state_short}); any other
#'   value uses the TUM WWK short coding (\code{as_fe_species_tum_wwk_short}).
#'   Note that wood densities are only available for the WWK method.
#'
#' @return Tibble identical to \code{inv_trees_reg} but with two additional
#'   variables:
#'   \describe{
#'     \item{hclass_43}{L43 height class (0.2-1.3, 1.3-5.0)}
#'     \item{mixture_type_43}{Mixture type derived from the main stand}
#'   }
#'
#' @keywords internal
#'
regeneration_add_essentials <- function(inv_trees_reg, method = method) {

  h_breaks <- c(0.2, 1.3, 5.0)

  # Introduce species groups
  if(method == "BaySF") {

    inv_trees_plus <- inv_trees_reg |>
      dplyr::mutate(
        species_group =
          ForestElementsR::as_fe_species_bavrn_state_short(.data$species_id))

    warning(fe_msg("wood_densities_wwk_only"))

  } else {

    inv_trees_plus <- inv_trees_reg |>
      dplyr::mutate(
        species_group =
          ForestElementsR::as_fe_species_tum_wwk_short(.data$species_id))

  }

  inv_trees_plus |>
    # add the fixed L43 height classes
    dplyr::filter(.data$layer_key == 3) |>
    dplyr::mutate(
      hclass_43 = cut(
        .data$height_m,
        breaks = h_breaks,
        labels = c("0.2-1.3", "1.3-5.0"),
        include.lowest = TRUE,
        right = FALSE
      )
    )
}



#' Summarise Circle Geometry for Regeneration Expansion Factors
#'
#' Takes a tibble of circle definitions (one row per concentric circle) as
#' extracted from an \code{fe_inventory} object and summarises the geometry
#' information to the plot level. For each plot, the function returns the
#' minimum circle area and the plot-level slope where the regeneration is
#' accounted for
#'
#'
#' The input \code{pulled_circles} should contain at least the variables
#' \code{plot_id}, \code{slope} and \code{c_area}, typically obtained via
#' \code{pull_circle_definitions()}. Geometry columns (sf) should already be
#' dropped before passing the tibble.
#'
#' @param pulled_circles Tibble with one row per circle, containing
#'   \code{plot_id}, \code{slope} and \code{c_area}. Usually created by
#'   \code{pull_circle_definitions()}.
#'
#' @return A tibble with one row per plot, containing:
#' \describe{
#'   \item{plot_id}{Unique plot identifier}
#'   \item{slope}{The plot-level slope (degrees), extracted as the first
#'     non-missing slope value per plot}
#'   \item{min_c_area}{The minimum circle area of all concentric circles
#'     for that plot}
#' }
#'
#' @keywords internal
#'
small_circle_geometry <- function(pulled_circles) {

  circle_def_summary <- pulled_circles |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::summarise(
      slope = dplyr::first(stats::na.omit(.data$slope)),
      min_c_area = if (all(is.na(.data$c_area))) {
        NA_real_
      } else {
        min(.data$c_area, na.rm = TRUE)
      },
      .groups = "drop"
    )

  circle_def_summary
}




#' Aggregation of Regeneration Information on User-Defined Grouping Level
#'
#' `r lifecycle::badge("experimental")`
#'
#' Aggregates regeneration data (real counts, calculated stems per hectare,
#' and sampled area) to any grouping structure defined by the user.
#' Works with the tibble returned by `pull_trees(..., small_trees = TRUE)`
#' and uses all trees with `layer_key == 3`.
#'
#' If `pulled_circles` is provided (a tibble of circle definitions with
#' `plot_id`, `slope` and `c_area`), the function approximates the counted
#' stems ("erfasst") per group from `n_rep_ha` and the minimum circle area
#' per plot by inverting the original expansion formula.
#'
#' @param inv_trees_reg Tibble returned by `pull_trees(..., small_trees = TRUE)`,
#'   containing regeneration trees with variables `n_rep_ha` and `area_rep_ha`.
#' @param ... Grouping variables passed to `dplyr::group_by()`.
#' @param pulled_circles Optional tibble with circle definitions (one row per
#'   circle), typically the output of `pull_circle_definitions()`.
#'
#' @return Tibble with aggregated regeneration information on the defined
#'   grouping level.
#'
#' @keywords internal
#'
single_regeneration_aggregation <- function(inv_trees_reg,
                                            ...,
                                            pulled_circles = NULL){
  has_circles <- !is.null(pulled_circles)

  # restrict to regeneration layer
  inv_reg <- inv_trees_reg |>
    dplyr::filter(.data$layer_key == 3)

  # if circle info is provided, summarise by plot and join
  if (has_circles) {
    circle_def_summary <- small_circle_geometry(pulled_circles)

    inv_reg <- inv_reg |>
      dplyr::left_join(circle_def_summary, by = "plot_id")
  }

  regeneration_agg <- inv_reg |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      n_measrured = if (has_circles) {
        # Invert the horizontal-projection correction applied to small-tree
        # n_rep_ha in make_object_lists()
        #   n_rep_ha = tree_count / (min_c_area * cos(slope))
        # to recover the counted stem number:
        #   tree_count = n_rep_ha * min_c_area * cos(slope),  slope in radians
        # (single cosine, first power, degrees -> radians via pi / 180).
        sum(
          .data$n_rep_ha * .data$min_c_area *
            cos(.data$slope * (pi / 180)),
          na.rm = TRUE
        )
      } else {
        dplyr::n()
      },
      n_calc_ha = sum(.data$n_rep_ha * .data$area_rep_ha, na.rm = TRUE),
      area_ha   = sum(.data$area_rep_ha, na.rm = TRUE),
      .groups   = "drop"
    )

  regeneration_agg
}

#' Restructure Regeneration Table with Mixture Classes (Long Format)
#'
#' `r lifecycle::badge("experimental")`
#'
#' Computes species mixing classes from the main stand (layer_key == 1)
#' based on basal area proportions (g_per), then attaches the mixture
#' class to the regeneration (layer_key == 3), and finally aggregates
#' regeneration using `single_regeneration_aggregation()`.
#'
#' All regeneration metrics are then aggregated to the level
#' mixture_class × species_group × hclass_43. In addition, the function
#' computes:
#' \itemize{
#'   \item an area-based percentage `percHoe`, expressing the share of VVJ
#'         area in each group relative to the total VVJ area of the table,
#'   \item the number of sample circles `n_inv_points` per mixture_class × species_group.
#' }
#'
#' If `pulled_circles` is provided (a tibble with circle geometry), the
#' regeneration aggregation will correct the counted stems ("erfasst")
#' using the inverted expansion formula.
#'
#' @param inv_dat_trees_ext_reg Tibble from `pull_trees(..., small_trees = TRUE)`
#'   containing small trees with height classes and n_rep_ha.
#' @param inv_dat_trees_ext Tibble from `pull_trees()` with the full stand,
#'   used to derive mixture classes based on basal area.
#' @param pulled_circles Optional tibble of circle definitions, typically the
#'   output of `pull_circle_definitions()`. If provided, real counts are
#'   corrected in the aggregation.
#'
#' @return Tibble in long format with one row per
#'   mixture_class × species_group × hclass_43 containing
#'   `n_measrured`, `n_calc_ha`, `area_ha`, `percHoe` and `n_inv_points`.
#'
#' @keywords internal
#'
structure_regeneration_output_table <- function(inv_dat_trees_ext_reg,
                                                inv_dat_trees_ext,
                                                pulled_circles = NULL) {

  # 1. MAIN STAND MIXTURE: basal area per species (one row per plot x species)
  #    Direct computation avoids internal grouping conflicts in single_tree_aggregation
  main_stand_g <- inv_dat_trees_ext |>
    dplyr::filter(.data$layer_key == 1) |>
    dplyr::group_by(.data$plot_id, .data$species_group) |>
    dplyr::summarise(
      g_m2 = sum(.data$g_m2 * .data$n_rep_ha * .data$area_rep_ha, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::mutate(
      total_g_m2 = sum(.data$g_m2, na.rm = TRUE),
      g_per      = 100 * .data$g_m2 / .data$total_g_m2
    ) |>
    dplyr::ungroup()

  # 2. MIXTURE CLASS PER PLOT (one row per plot_id)
  mixture_per_plot <- main_stand_g |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::summarise(
      species_mix = paste0(
        format(.data$species_group, spec_lang = "ger")[.data$g_per >= 10],
        collapse = "-"
      ),
      n_species_10     = sum(.data$g_per >= 10, na.rm = TRUE),
      max_g_per        = max(.data$g_per, na.rm = TRUE),
      dominant_species = format(.data$species_group, spec_lang = "ger")[which.max(.data$g_per)],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mixture_class = dplyr::case_when(
        .data$max_g_per >= 90   ~ paste0("single_", .data$dominant_species),
        .data$n_species_10 >= 2 ~ paste0("mix_", .data$species_mix),
        TRUE                    ~ paste0("other_", .data$species_mix)
      )
    )

  # 3. REGENERATION AGGREGATION (pass circles if provided)
  if (!is.null(pulled_circles)) {
    regen_agg_plot <- inv_dat_trees_ext_reg |>
      single_regeneration_aggregation(
        .data$species_group,
        .data$plot_id,
        .data$hclass_43,
        pulled_circles = pulled_circles
      )
  } else {
    regen_agg_plot <- inv_dat_trees_ext_reg |>
      single_regeneration_aggregation(
        .data$species_group,
        .data$plot_id,
        .data$hclass_43
      )
  }

  # 4. JOIN MIXTURE TYPE TO REGENERATION (still plot-level)
  regen_with_mix <- regen_agg_plot |>
    dplyr::left_join(mixture_per_plot, by = "plot_id")

  # 5. NUMBER OF SAMPLE CIRCLES ("Zahl der Probkreise")
  #    per mixture_class × species_group
  inv_point_counts <- regen_with_mix |>
    dplyr::group_by(.data$mixture_class, .data$species_group) |>
    dplyr::summarise(
      n_inv_points = dplyr::n_distinct(.data$plot_id),
      .groups = "drop"
    )

  # 6. AGGREGATE TO mixture_class × species_group × hclass_43
  regen_long <- regen_with_mix |>
    dplyr::group_by(
      .data$mixture_class,
      .data$species_group,
      .data$hclass_43
    ) |>
    dplyr::summarise(
      n_measrured = sum(.data$n_measrured, na.rm = TRUE),
      n_calc_ha   = sum(.data$n_calc_ha,   na.rm = TRUE),
      area_ha     = sum(.data$area_ha,     na.rm = TRUE),
      .groups     = "drop"
    ) |>
    # attach n_inv_points (same for all height classes within mixture × species)
    dplyr::left_join(
      inv_point_counts,
      by = c("mixture_class", "species_group")
    )

  # 7. GLOBAL VVJ AREA FOR %Hö (over whole table)
  total_vvj_area <- sum(regen_long$area_ha, na.rm = TRUE)

  regen_long <- regen_long |>
    dplyr::mutate(
      percHoe = 100 * .data$area_ha / total_vvj_area
    )

  # 8. attach mixture_per_plot as attribute so helpers can reuse it
  attr(regen_long, "mixture_per_plot") <- mixture_per_plot

  regen_long

}

#' Count inventory points by regeneration plants per point (long format)
#'
#' `r lifecycle::badge("experimental")`
#'
#' Counts the number of inventory points ("Probkreise") by classes of
#' plant numbers per point, analogous to the "Zahl der Probkreise nach
#' Pflanzenzahl je Probekreis" block in L43.
#'
#' For each mixture_class × species_group, the function:
#'  1. approximates the counted plants (`tree_count`) per inventory point,
#'  2. sums plants per point across all regeneration trees on that point,
#'  3. assigns each point to a plant-number class (e.g. 0, 1-9, 10-19, ...),
#'  4. counts how many points fall into each class.
#'
#' An additional "total" block aggregates across all mixture classes per species.
#'
#' @param inv_dat_trees_ext_reg Tibble from `pull_trees(..., small_trees = TRUE)`
#'   containing regeneration trees, including `plot_id`, `layer_key`,
#'   `species_group`, `n_rep_ha`.
#' @param mixture_per_plot Tibble with `plot_id` and `mixture_class` as created
#'   inside `structure_regeneration_output_table()`.
#' @param pulled_circles Tibble of circle definitions with `plot_id`, `slope`,
#'   and `c_area`, typically the output of `pull_circle_definitions()`.
#'
#' @return Tibble with one row per mixture_class × species_group × plant_count_class
#'   (plus an overall "total" block), containing:
#'   \describe{
#'     \item{mixture_class}{Mixture block ("total" or specific mixture)}
#'     \item{species_group}{Species identifier}
#'     \item{plant_count_class}{Class of plants per point (character)}
#'     \item{n_inv_points}{Number of inventory points in this class}
#'   }
#'
#' @keywords internal
#'
count_reg_points_by_plants_long <- function(inv_dat_trees_ext_reg,
                                            mixture_per_plot,
                                            pulled_circles) {

  # summarise circle geometry (min area and slope per plot)
  circle_def_summary <- small_circle_geometry(pulled_circles)

  # 1) restrict to regeneration layer and attach mixture + circle info
  reg_with_info <- inv_dat_trees_ext_reg |>
    dplyr::filter(.data$layer_key == 3) |>
    dplyr::left_join(mixture_per_plot,   by = "plot_id") |>
    dplyr::left_join(circle_def_summary, by = "plot_id")

  # 2) approximate counted plants ("tree_count") per row via inverse expansion.
  # Inverse of the small-tree horizontal-projection correction in
  # make_object_lists() (n_rep_ha = tree_count / (min_c_area * cos(slope))):
  #   tree_count = n_rep_ha * min_c_area * cos(slope),  slope in radians.
  reg_with_info <- reg_with_info |>
    dplyr::mutate(
      tree_count = .data$n_rep_ha * .data$min_c_area *
        cos(.data$slope * (pi / 180))
    )

  # 3) plants per point (plot_id) for each species × mixture_class
  plants_per_point <- reg_with_info |>
    dplyr::group_by(
      .data$mixture_class,
      .data$species_group,
      .data$plot_id
    ) |>
    dplyr::summarise(
      plants_per_point = sum(.data$tree_count, na.rm = TRUE),
      .groups = "drop"
    )

  # 4) define classes of plants per point (adaptable)
  breaks <- c(-Inf, 1, 2, 3, 4, 5, 6, 8, 10, 15, 20, 30, 50, Inf)
  labels <- c("0", "1", "2", "3", "4", "5", "6-7", "8-9", "10-14", "15-19", "20-29", "30-49", ">=50")

  plants_per_point <- plants_per_point |>
    dplyr::mutate(
      plant_count_class = cut(
        .data$plants_per_point,
        breaks = breaks,
        labels = labels,
        right  = FALSE
      )
    )

  # 5) counts per mixture_class × species_group × class
  by_mixture <- plants_per_point |>
    dplyr::group_by(
      .data$mixture_class,
      .data$species_group,
      .data$plant_count_class
    ) |>
    dplyr::summarise(
      n_inv_points = dplyr::n(),
      .groups = "drop"
    )

  # 6) overall "total" across all mixtures per species × class
  overall <- plants_per_point |>
    dplyr::group_by(
      .data$species_group,
      .data$plant_count_class
    ) |>
    dplyr::summarise(
      n_inv_points = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(mixture_class = "total")

  dplyr::bind_rows(overall, by_mixture)
}

#' Restructure Regeneration Aggregation to L43-style Wide Table
#'
#' `r lifecycle::badge("experimental")`
#'
#' Takes the aggregated long-format regeneration table produced by
#' `structure_regeneration_output_table()` and reshapes it into a
#' wide-format table similar to the Bavarian L43 tables.
#'
#' The function:
#'  1. Builds the top block "total" (across all mixture classes)
#'  2. Binds mixture-level blocks below
#'  3. Pivots height classes into separate columns using the pattern
#'     "\{.value\}_\{hclass_43\}"
#'  4. Computes horizontal sums across height classes for
#'     n_measrured, n_calc_ha, area_ha, and percHoe
#'  5. Derives block-wise `stammzahl_je_ha` and `Dominanz` from the
#'     horizontal sums
#'  6. Optionally joins a wide Probkreise block derived from
#'     `count_reg_points_by_plants_long()`
#'
#' @param regen_long Tibble from `structure_regeneration_output_table()`,
#'   containing at least mixture_class, species_group, hclass_43,
#'   n_measrured, n_calc_ha, area_ha, and percHoe.
#' @param reg_points_long Optional tibble from
#'   `count_reg_points_by_plants_long()`, containing mixture_class,
#'   species_group, plant_count_class, n_inv_points.
#'
#' @return Wide-format tibble with one row per mixture_class × species_group,
#'   including all height-class columns, horizontal sum columns,
#'   block-wise `stammzahl_je_ha`, `Dominanz`, and (if provided)
#'   the `n_inv_points_*` columns.
#'
#' @keywords internal
#'
structure_regeneration_output_wide <- function(regen_long,
                                               reg_points_long = NULL) {

  # filter undefined height classes
  regen_long <- regen_long |>
    dplyr::filter(!is.na(.data$hclass_43))

  # global VVJ area for safety
  total_vvj_area <- sum(regen_long$area_ha, na.rm = TRUE)

  # 1) Top block: Sum over all mixture classes
  overall_grouped <- regen_long |>
    dplyr::group_by(
      .data$species_group,
      .data$hclass_43
    ) |>
    dplyr::summarise(
      n_measrured = sum(.data$n_measrured, na.rm = TRUE),
      n_calc_ha   = sum(.data$n_calc_ha,   na.rm = TRUE),
      area_ha     = sum(.data$area_ha,     na.rm = TRUE),
      .groups     = "drop"
    ) |>
    dplyr::mutate(
      mixture_class = "total",
      percHoe       = 100 * .data$area_ha / total_vvj_area
    )

  # 2) The mixture-specific blocks (already per mixture_class × species × hclass)
  by_mixture_grouped <- regen_long |>
    dplyr::select(
      "mixture_class",
      "species_group",
      "hclass_43",
      "n_measrured",
      "n_calc_ha",
      "area_ha",
      "percHoe"
    )

  # 3) Stack: total + all mixture classes
  regen_grouped <- dplyr::bind_rows(
    overall_grouped,
    by_mixture_grouped
  )

  # 4) Pivot to wide format (L43-style column names)
  regen_wide <- regen_grouped |>
    tidyr::pivot_wider(
      id_cols    = c(.data$mixture_class, .data$species_group),
      names_from = .data$hclass_43,
      values_from = c(
        .data$n_measrured,
        .data$n_calc_ha,
        .data$area_ha,
        .data$percHoe
      ),
      names_glue = "{.value}_{hclass_43}"
    ) |>
    # 5) Horizontal sums across height classes
    dplyr::mutate(
      n_measrured_total = rowSums(
        dplyr::across(tidyselect::starts_with("n_measrured_")),
        na.rm = TRUE
      ),
      n_calc_ha_total   = rowSums(
        dplyr::across(tidyselect::starts_with("n_calc_ha_")),
        na.rm = TRUE
      ),
      area_ha_total     = rowSums(
        dplyr::across(tidyselect::starts_with("area_ha_")),
        na.rm = TRUE
      ),
      percHoe_total     = rowSums(
        dplyr::across(tidyselect::starts_with("percHoe_")),
        na.rm = TRUE
      )
    ) |>
    # 6) Block-wise stems per hectare and dominance
    dplyr::group_by(.data$mixture_class) |>
    dplyr::mutate(
      stammzahl_je_ha = dplyr::if_else(
        .data$area_ha_total > 0,
        .data$n_calc_ha_total / .data$area_ha_total,
        NA_real_
      ),
      Dominanz = {
        block_total_n <- sum(.data$n_calc_ha_total, na.rm = TRUE)
        if (block_total_n > 0) {
          100 * .data$n_calc_ha_total / block_total_n
        } else {
          NA_real_
        }
      }
    ) |>
    dplyr::ungroup()

  # 7) Optionally join the Probkreise-by-plant-class block
  if (!is.null(reg_points_long)) {
    reg_points_wide <- reg_points_by_plants_wide(reg_points_long)

    regen_wide <- regen_wide |>
      dplyr::left_join(
        reg_points_wide,
        by = c("mixture_class", "species_group")
      )
  }

  regen_wide
}



#' Wide table of inventory-point counts by plants per point
#'
#' `r lifecycle::badge("experimental")`
#'
#' Converts the long-format output of `count_reg_points_by_plants_long()`
#' into a wide table with one row per mixture_class × species_group and
#' one column per plant-number class.
#'
#' @param reg_points_long Tibble from `count_reg_points_by_plants_long()`,
#'   containing mixture_class, species_group, plant_count_class, n_inv_points.
#'
#' @return Wide-format tibble with one row per mixture_class × species_group and
#'   columns `n_inv_points_<class>` for each plant-number class.
#'
#' @keywords internal
reg_points_by_plants_wide <- function(reg_points_long) {

  reg_points_long |>
    tidyr::pivot_wider(
      id_cols    = c(.data$mixture_class, .data$species_group),
      names_from = .data$plant_count_class,
      values_from = .data$n_inv_points,
      names_glue = "n_inv_points_{plant_count_class}",
      values_fill = list(n_inv_points = 0)
    )
}



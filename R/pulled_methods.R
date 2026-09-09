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


#' Add Essential Information Like Species Groups, DBH-Classes, Basal Areas, and
#' Volumes to a Trees Data Frame
#'
#' Function for intermediate calculations between pulling a trees data frame
#' from an \code{\link{fe_inventory}} object with \code{\link{pull_trees}} and
#' making aggregated evaluations and plots. Made for avoiding double
#' calculations of tree volume, basal area, and species groups.
#'
#' @param inv_trees Data frame which covers trees from an inventory (each row is
#'   a tree), obtained from an \code{\link{fe_inventory}} object with
#'   \code{\link{pull_trees}} and then \strong{height-completed}. The column
#'   \code{height_m} must be present and must not contain any missing values.
#'   Note that \code{\link{pull_trees}} alone may legitimately return trees with
#'   missing heights (they are not filled in during import); complete them first,
#'   e.g. with \code{\link{height_complete_inventory}} followed by
#'   \code{\link{fill_heights_back}} (or the \code{h_est_m} fallback shown in the
#'   examples). A missing \code{height_m} column, or any \code{NA} height, is
#'   rejected with an informative error.
#'
#' @param method Character string indicating the method to be used for the
#'   species groups calculation. Default is "WWK", which is the method used
#'   for the species coding tum_wwk_short. Additionally "BaySF" is available,
#'   for classifying species according to the groups of the Bavarian State
#'   Forest Enterprise (bavrn_state_short).
#'
#' @param dbh_interval Integer indicating the interval breaks for the dbh
#'   classes to be formed. The breaks are internally handed over
#'   to \code{\link[base]{cut}}as its parameter \code{breaks}.
#'   Default is 5-cm-classes, where diameters   over 60 cm are all put
#'   into one class.
#'
#' @param age_interval Integer indicating the interval breaks for the age
#'   classes to be formed. The breaks are internally handed over
#'   to \code{\link[base]{cut}} as its parameter \code{breaks}. Default is
#'   20-years-classes, where ages over 160 years are all put into one class.
#'
#' @return The original data frame but with the additional columns
#'   \describe{
#'     \item{species_group: }{Translation of the original species ids into
#'       the \code{fe_species_tum_wwk_short}} coding, which is useful for
#'       aggregated evaluations,
#'     \item{dbh_class: }{The dbh class each tree belongs to according to
#'       \code{dbh_breaks} as an ordered factor,}
#'     \item{age_class: }{The age class each tree belongs to according to
#'       \code{age_breaks} as an ordered factor,}
#'     \item{g_m2: }{Basal area of each tree in m²,}
#'     \item{v_m3: }{Standing wood volume of each tree over bark in m³
#'       (calculated with \code{\link[ForestElementsR]{v_gri}),}}
#'     \item{v_hub_m3: }{Harvested wood volume of each tree under bark in m³
#'       (calculated with \code{\link[ForestElementsR]{v_red_harvest_ubark}}).}
#'     \item{standing_area_m2: }{Tree specific standing areas, calculated with
#'       the function \code{\link[ForestElementsR]{standing_area_gnfi3}}}
#'   }
#'
#' @export
#'
#' @examples
#' # Complete heights are necessary for trees_add_essentials. The h_q fallback
#' # warning is expected here (some species x layer groups lack a measured height).
#' trees_with_heights <- suppressWarnings(
#'   data_ex3_sample_fe_inventory |>
#'     pull_trees() |>
#'     height_complete_inventory(method = "Bavaria") |>
#'     dplyr::mutate(height_m = ifelse(!h_m_tree, h_est_m, height_m))
#' )
#'
#' # Actual application
#' trees_add_essentials(trees_with_heights)
#'
trees_add_essentials <- function(inv_trees,
                                 method = c("WWK", "BaySF"),
                                 dbh_interval = 5,
                                 age_interval = 20) {

  # Heights must be complete: reject a missing height_m column outright, and
  # reject any NA values (pull_trees() alone may legitimately return missing
  # heights; they must be completed beforehand).
  if (is.null(inv_trees$height_m))
    stop(fe_msg("height_m_col_missing"), call. = FALSE)
  if (any(is.na(inv_trees$height_m)))
    stop(fe_msg("height_m_missing"), call. = FALSE)

  method <- match.arg(method)

  # Define breaks from given intervals
  dbh_breaks <- append(seq(0,  60, dbh_interval), 999)
  age_breaks <- append(seq(0, 160, age_interval), 999)

  # Introduce species groups
  if(method == "BaySF") {

    inv_trees_plus <- inv_trees |>
      dplyr::mutate(
        species_group =
          ForestElementsR::as_fe_species_bavrn_state_short(.data$species_id))

  } else {

    inv_trees_plus <- inv_trees |>
      dplyr::mutate(
        species_group =
          ForestElementsR::as_fe_species_tum_wwk_short(.data$species_id))

  }

  # calculate basal area, volume
  inv_trees_plus <- inv_trees_plus |>
      dplyr::mutate(
      dbh_class = cut(.data$dbh_cm, breaks = dbh_breaks, ordered_result = TRUE),
      age_class = cut(.data$age_yr, breaks = age_breaks, ordered_result = TRUE),
      g_m2 = (.data$dbh_cm / 100)^2 * pi / 4,
      v_m3 = ForestElementsR::v_gri(
        .data$species_id,
        .data$dbh_cm,
        .data$height_m
      ),
      v_hub_m3 = ForestElementsR::v_red_harvest_ubark(
        .data$species_id,
        .data$v_m3
      ),
      standing_area_m2 = ForestElementsR::standing_area_gnfi3(
        .data$species_id, .data$dbh_cm
      )
    ) |>
    dplyr::select(
      "plot_id", "area_rep_ha", "layer_key",
      "species_id", "species_group", dplyr::everything()
    ) |>
    dplyr::relocate("dbh_class", .after = "dbh_cm") |>
    dplyr::relocate("age_class", .after = "age_yr")

  inv_trees_plus
}


#' Create a Background Table for All Mean Diameter Class Based Evaluations From
#' an Inventory Tree Data Frame
#'
#' Calculates the quadratic mean diameter on the nested levels of layer in
#' species group in plot, attributes these values to categories which are the
#' basis for further aggregation with other functions. The function was designed
#' for producing often required information only once.
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @param d_q_interval Integer indicating the interval breaks for the d_q
#'   (quadratic mean diameter) classes to be formed. The breaks are internally
#'   handed over  to \code{\link[base]{cut}} as its parameter \code{breaks}.
#'   Default is 10-cm-classes, where all d_q values over 60 cm are all put
#'   into the same class.
#'
#' @return A data frame which reports for each plot, species group and layer the
#'   actual quadratic mean diameter and the d_q-class (as an ordered factor) it
#'   belongs to.
#'
#' @export
#'
#' @examples
#' # The prepared tree list ships with the package; see
#' # ?data_ex3_trees_essentials for the chain that builds it.
#' data_ex3_sample_trees_essentials |>
#'   back_table_dclass()
#'
back_table_dclass <- function(inv_trees_plus,
                              d_q_interval = 10) {
  d_q_breaks <- append(seq(0, 60, d_q_interval), 999)

  inv_trees_plus |>
    dplyr::group_by(.data$plot_id, .data$species_group, .data$layer_key) |>
    dplyr::summarise(d_q_cm = ForestElementsR::d_q(
      .data$dbh_cm,
      .data$n_rep_ha
    )) |>
    dplyr::mutate(d_q_class = cut(.data$d_q_cm,
      breaks = d_q_breaks,
      ordered_result = TRUE
    )) |>
    dplyr::ungroup() # If not ungrouped strange things might happen at the next
                     # Level of evaluation
}


#' Aggregated Information From Single Tree to Inventory Level by User Defined
#' Grouping
#'
#' Workhorse function used in several functions which produce base or structure
#' tables
#'
#' @param inv_trees_ext A tibble representing the trees of an inventory (or any
#'   subset), typically the outbput of \code{\link{trees_add_essentials}},
#'   possibly merged with the output of \code{\link{base_table_d_q_class}}
#'
#' @param ... Grouping variables defining the level of aggregation, internally
#'   passed to \code{\link[dplyr]{group_by}}
#'
#' @param .sub_plot_areas Logical, if \code{TRUE}, sub-plot area shares and
#'   area-related values on sub-plot level will be provided in addition
#'   (default = FALSE)
#'
#' @return A tibble containing the aggregated information
#'
#' @keywords internal
#'
single_tree_aggregation <- function(inv_trees_ext,
                                    ...,
                                    .sub_plot_areas = FALSE,
                                    .standard_error = TRUE) {


  # Relative (percent) 95% confidence interval of volume sums via the shared
  # area-weighted primitive. Plots with different representation areas
  # (e.g. slope correction) are correctly accounted for by the ratio
  # estimator. Groups with only one plot get NA_real_ for the CI from the
  # primitive itself, so no manual ifelse is needed here.
  inv_trees_err <- inv_trees_ext |>
    dplyr::mutate(
      v_ha_class = .data$v_hub_m3 * .data$n_rep_ha
    ) |>
    dplyr::group_by(.data$plot_id, ...) |>
    dplyr::summarize(
      v_ha_plot_class = sum(.data$v_ha_class),
      area_rep_ha     = unique(.data$area_rep_ha),
      .groups         = "drop"
    ) |>
    dplyr::group_by(...) |>
    se_area_weighted_grouped("v_ha_plot_class", "area_rep_ha") |>
    dplyr::transmute(
      ...,
      n_plot      = .data$n_plots,
      conf_95_per = .data$ci_pct
    )

  # Standard aggregations
  inv_trees_agg <- inv_trees_ext |>
    dplyr::mutate(
      v_tot_class = .data$v_hub_m3 * .data$n_rep_ha * .data$area_rep_ha
    ) |>
    dplyr::group_by(...) |>
    dplyr::summarize(
      v_hub_m3 = sum(.data$v_tot_class),
      g_m2 = sum(.data$g_m2 * .data$n_rep_ha * .data$area_rep_ha),
      n = sum(.data$n_rep_ha * .data$area_rep_ha),
      d_q_cm = ForestElementsR::d_q(
        .data$dbh_cm,
        .data$n_rep_ha * .data$area_rep_ha
      ),
      h_q_m = ForestElementsR::h_q(
        .data$height_m, .data$dbh_cm,
        .data$n_rep_ha * .data$area_rep_ha
      ),
    ) |>
    dplyr::left_join(inv_trees_err)

  # Calculate sub-plot area shares and area-related values if asked for
  if (.sub_plot_areas) {
    # Correction factors for standing areas on plot level to make them add up to
    # 1 ha
    corr_fac_dat <- inv_trees_ext |>
      dplyr::group_by(.data$plot_id) |>
      dplyr::summarise(
        corr_fac = 10000 / sum(.data$standing_area_m2 * .data$n_rep_ha)
      )

    # Calculate the areas
    areas_dat <- inv_trees_ext |>
      dplyr::inner_join(corr_fac_dat) |>
      dplyr::group_by(...) |>
      dplyr::summarise(area_ha = sum(
        .data$standing_area_m2 / 10000 *
          .data$n_rep_ha *
          .data$corr_fac *
          .data$area_rep_ha
      ))

    # ha-Values
    inv_trees_agg <- inv_trees_agg |>
      dplyr::inner_join(areas_dat) |>
      dplyr::group_by(...) |>
      dplyr::mutate(
        v_hub_m3_ha = .data$v_hub_m3 / .data$area_ha,
        g_m2_ha = .data$g_m2 / .data$area_ha,
        n_ha = .data$n / .data$area_ha
      )
  }

  inv_trees_agg
}


#' Aggregated hq and dq values from Single Tree to Inventory Level by User
#' Defined Grouping
#'
#' Workhorse function used to complete heights at inventory level
#'
#' @param inv_trees_ext A tibble representing the trees of an inventory (or any
#'   subset), typically the outbput of \code{\link{trees_add_essentials}},
#'   possibly merged with the output of \code{\link{base_table_d_q_class}}
#'
#' @param ... Grouping variables defining the level of aggregation, internally
#'   passed to \code{\link[dplyr]{group_by}}
#'
#' @return A tibble containing the aggregated information for dq and hq
#'
#' @keywords internal
#'
dq_hq_perclass <- function(inv_trees_ext,
                           ...) {
  # Standard aggregations
  inv_trees_agg <- inv_trees_ext |>
    dplyr::group_by(...) |>
    dplyr::summarize(
      d_q_cm = ForestElementsR::d_q(
        .data$dbh_cm,
        .data$n_rep_ha * .data$area_rep_ha
      ),
      h_q_m = ForestElementsR::h_q(
        .data$height_m, .data$dbh_cm,
        .data$n_rep_ha * .data$area_rep_ha,
        na_h.rm = TRUE
      ),
    )

  # Create an empty list to store the warning messages

    warning_list <- list()

  if (is.na(mean(inv_trees_agg$h_q_m))) {
    inv_trees_agg <- inv_trees_agg |>
      dplyr::mutate(h_q_m = ifelse(is.na(.data$h_q_m),
        ForestElementsR::h_q_from_d_q(
          .data$species_group,
          .data$d_q_cm
        ),
        .data$h_q_m
      ))

    withCallingHandlers(
      {
        class_no_heights <- inv_trees_agg |>
          dplyr::filter(is.na(.data$h_q_m))
      },
      warning = function(w) {
        w <-
          paste(
            "no heights to estimate hq, species =", class_no_heights$species_group,
            ", layers =", class_no_heights$layer_key, ", hq estimated from dq"
          )
        warning_list <<- c(warning_list, conditionMessage(w))
      }
    )
  }

    # file_conn <- file("D:/neu_FE/feneu/warnings.txt", "w")
    # sink(file_conn, type = "message")
    # close(file_conn)

  inv_trees_agg
}
























































































































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

#' Pull Circle Definitions From an fe_inventory Object
#'
#' Extracts per-plot slope and minimum circle area from the circle definition
#' of an \code{fe_inventory} object. Useful for reconstructing small-tree
#' expansion factors or for diagnostic purposes.
#'
#' @param forest_inventory Object of class \code{fe_inventory}.
#' @param .progress Logical, if TRUE a progress bar is shown.
#'
#' @return Tibble with one row per plot, containing at least
#'   \code{plot_id}, \code{slope} and \code{min_c_area}.
#'
#' @export
#'
#' @examples
#' pull_circle_definitions(data_ex3_sample_fe_inventory)
pull_circle_definitions <- function(forest_inventory, .progress = TRUE) {

  stopifnot(is_fe_inventory(forest_inventory))

  plots <- forest_inventory |>
    purrr::pluck("plot")

  circle_def_all <- plots |>
    purrr::map_dfr(
      .f = function(plot) {
        if (ForestElementsR::is_fe_ccircle_spatial(plot)) {
          plot$circle_definition |>
            sf::st_drop_geometry() |>
            dplyr::mutate(plot_id = plot$stand_id) |>
            dplyr::select(
              "plot_id",
              "slope",
              "c_area"
            )
        } else {
          tibble::tibble(
            plot_id = plot$stand_id,
            slope   = NA_real_,
            c_area  = NA_real_
          )
        }
      },
      .progress = .progress
    )

  circle_def_all
}

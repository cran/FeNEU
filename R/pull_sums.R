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


#' Generate Layer-Wise Stand Sum and Mean Values per Plot of an
#' \link{fe_inventory} Object and Hand them Back as a Tibble
#'
#' Basically, this function calls the function
#' \code{\link[ForestElementsR]{stand_sums_static}}. According to the behaviour
#' of that function, If the height values provided with an inventory plot are
#' not complete, all output values that require height for calculation (i.e.
#' meam and dominant heights, tree volumes) will be \code{NA}.
#'
#' @param forest_inventory An \code{\link{fe_inventory}} object
#'
#' @param hd_dom_method Method for calculating the dominant diameter and
#'   dominant height. The default choice is "Weise". See the documentation of
#'   \code{\link[ForestElementsR]{stand_sums_static}} for more options and
#'   details.
#'
#' @param .progress Logical, if TRUE (default) a progress bar will be shown
#'   during execution. Check the documentation of \link[purrr]{map} for more
#'   options.
#'
#' @return A tibble containing the calculated sum and mean values
#'
#' @export
#'
#' @examples
#'   # Very small example
#'   fe_inv <- data_ex3_sample_fe_inventory[1:3, ]
#'
#'   # Example with incomplete heights
#'   fe_inv |> pull_sums(.progress = FALSE)
#'
#'   # Complete all heights with estimates ...
#'   suppressWarnings(
#'   # Warnings come from intentional species code casts - no problem here
#'     all_trees_with_heights <- fe_inv |>
#'       pull_trees() |>
#'       height_complete_inventory()
#'   )
#'
#'   # ... and run pull_sums() again
#'   fe_inv |>
#'     fill_heights_back(all_trees_with_heights, .progress = FALSE) |>
#'     pull_sums(.progress = FALSE)
#'
pull_sums <- function(
    forest_inventory, hd_dom_method = "Weise", .progress = TRUE
) {

  stopifnot(is_fe_inventory(forest_inventory))

  # Apply the sub-function 'layer_sums_per_plot' to each plot, combine the
  # result into a tibble, merge in the representation area of each plot,
  # arrange the columns nicely and hand it back
  forest_inventory |> purrr::pluck("plot") |>
    purrr::map(.f = layer_sums_per_plot, hd_dom_method = hd_dom_method,
               .progress = .progress) |>
    dplyr::bind_rows() |>
    dplyr::inner_join(
      dplyr::select(forest_inventory, "plot_id", "area_rep_ha")
    ) |>
    dplyr::select(
      "plot_id", "area_rep_ha", "layer_key", "species_id",
      tidyselect::everything()
    )
}


#' Sub-function for calculating layer-wise sum and mean values per plot
#' adding plot_id and layer_key and return them as a tibble
#'
#' @param plot An \code{fe_stand} object. This is not checked after calling
#'   this function, because it is guaranteed by the calling function pull_sums()
#'   that \code{plot} is an element of an \code{fe_inventory} object which must
#'   be of class or a child class of \code{fe_stand}.
#'
#' @param hd_dom_method Method for calculationg the dominant diameter and
#'   dominant height. The default choice is "Weise". See the documentation of
#'   \code{\link[ForestElementsR]{stand_sums_static}} for more options and
#'   details.
#'
#' @keywords internal
#'
#' @noRd
#'
layer_sums_per_plot <- function(plot, hd_dom_method) {

  layers <- unique(plot$trees$layer_key)
  purrr::map(
    layers,
    .f = function(layer, plot, hd_dom_method) {
      ForestElementsR::stand_sums_static(
        plot,
        tree_filter = .data$layer_key == layer,
        hd_dom_method = hd_dom_method
      ) |>
        dplyr::mutate(
          layer_key = layer
        )
    },
    plot = plot,
    hd_dom_method = hd_dom_method
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(plot_id = plot$stand_id)
}



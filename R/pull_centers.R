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





#' Pull Center Point Coordinates From an fe_inventory Object
#'
#' @param x An object of class \code{\link{fe_inventory}}
#'
#' @returns An object of class \code{sf} with the column \code{plot_id} as taken
#'   from the input object \code{x}, and a geometry column that represents each
#'   plot's center point. In case the \code{plot} column of \code{x} does not
#'   contain objects that allow to extract meaningful center coordinates the
#'   resulting geometry will be empty.
#'
#' @export
#'
#' @examples
#'   data_ex3_sample_fe_inventory |> pull_centers()
#'
#'
pull_centers <- function(x) {

  stopifnot(is_fe_inventory(x))

  centers <- purrr::map(x$plot, .f = ForestElementsR::get_center)

  # Find a reference CRS from the first valid center
  ref_crs <- NULL
  for (g in centers) {
    g_crs <- sf::st_crs(g)
    if (!is.na(g_crs)) {
      ref_crs <- g_crs
      break
    }
  }

  # Empty point to use as fallback for plots without coordinates
  empty_pt <- sf::st_sfc(sf::st_point(), crs = ref_crs)

  # Normalise each center: ensure CRS matches, replace empty with fallback
  centers <- lapply(centers, function(g) {
    # Handle NULL or zero-length geometries
    if (is.null(g) || length(g) == 0) return(empty_pt)

    g_crs <- sf::st_crs(g)
    if (!is.null(ref_crs)) {
      if (is.na(g_crs)) {
        sf::st_crs(g) <- ref_crs
      } else if (g_crs != ref_crs) {
        g <- sf::st_transform(g, ref_crs)
      }
    }
    g
  })

  geometry <- do.call(c, centers)

  sf::st_sf(plot_id = x$plot_id, geometry = geometry)
}



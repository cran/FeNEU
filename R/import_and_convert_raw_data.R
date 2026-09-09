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
#' Read Preprocessed Inventory Data Into an fe_inventory (expert dispatcher)
#'
#' \strong{Expert-level function -- standard users do not need it.} The normal
#' import path is two calls: the matching \code{raw_to_pre} converter for your
#' raw format, followed by the matching \code{pre_to_fe_inventory} reader, which
#' alone turns the preprocessed on-disk files into a finished
#' \code{\link{fe_inventory}} object (see
#' \code{\link{import_sample_concentric_pre_to_fe_inventory}} and
#' \code{\link{import_standwise_relascope_pre_to_fe_inventory}}).
#'
#' This function is a convenience dispatcher for \strong{expert / programmatic}
#' use -- e.g. when the inventory chain has to be chosen at run time from an
#' argument. It routes to the matching \code{pre_to_fe_inventory} step of the two
#' import chains and derives the correct \code{object_type} from the inventory
#' type, so callers cannot mismatch them:
#' \describe{
#'   \item{\code{"sample_concentric"}}{concentric-circle sample inventory ->
#'     \code{fe_ccircle_spatial} plots, via
#'     \code{\link{import_sample_concentric_pre_to_fe_inventory}}.}
#'   \item{\code{"standwise_relascope"}}{stand-wise relascope
#'     (\dQuote{Winkelzählprobe} / angle-count) inventory -> \code{fe_stand}
#'     plots, via
#'     \code{\link{import_standwise_relascope_pre_to_fe_inventory}}.}
#' }
#'
#' The input must already be in the preprocessed on-disk form of the respective
#' chain (a folder of BaySF-style files for \code{"sample_concentric"}, a
#' \code{WZP_Daten.txt} for \code{"standwise_relascope"}). Raw vendor formats are
#' brought into that form first by the matching \code{raw_to_pre} converter --
#' see \code{\link{import_sample_concentric_format1_raw_to_pre}},
#' \code{\link{import_sample_concentric_format2_raw_to_pre}} and
#' \code{\link{import_standwise_relascope_format1_raw_to_pre}}.
#'
#' @param input_path Path of the folder holding the preprocessed input files
#'   (character). The required filenames depend on \code{inventory_type}; see the
#'   routed \code{pre_to_fe_inventory} functions above.
#'
#' @param inventory_type Character. Which inventory chain to read. One of
#'   \code{"sample_concentric"} or \code{"standwise_relascope"}.
#'
#' @param style Character or \code{NULL}. Format family of the preprocessed data
#'   within the chosen chain. If \code{NULL} (default), the chain's only current
#'   style is used: \code{"baysf"} (Bavarian State Forest style) for
#'   \code{"sample_concentric"}, \code{"silvarith"} (Silvarith-style) for
#'   \code{"standwise_relascope"}. The argument is explicit (not auto-detected):
#'   the right style follows from which raw format was imported.
#'
#' @param coord_sys Character string specifying the coordinate reference system
#'   of the input plot coordinates (used only by \code{"sample_concentric"};
#'   ignored for stand-wise data). Supported values:
#'   \describe{
#'     \item{\code{"lonlat"}}{WGS84 geographic, EPSG:4326 (degrees)}
#'     \item{\code{"etrs89"}}{ETRS89 geographic, EPSG:4258 (degrees)}
#'     \item{\code{"utm32"}, \code{"utm33"}}{ETRS89 / UTM 32N, 33N
#'       (EPSG:25832, 25833)}
#'     \item{\code{"gk2"} … \code{"gk5"}}{DHDN / Gauss-Krueger zones 2-5
#'       (EPSG:31466-31469)}
#'   }
#'   If \code{NULL}, a format-specific default is used (\code{"gk4"} for the
#'   BaySF style).
#'
#'   The \strong{geographic} systems (\code{"lonlat"}, \code{"etrs89"}) are
#'   accepted, but their coordinates are \strong{reprojected to the matching UTM
#'   zone} during import (a message names the zone). This is not cosmetic: tree
#'   positions are built metrically from distance and azimuth around the plot
#'   centre, which is impossible in degrees. The plot coordinates of the returned
#'   object are therefore in UTM, not in the geographic system supplied.
#'
#'   Note that \code{coord_sys} must be stated \strong{explicitly} whenever the
#'   eastings are stored without the Gauss-Krueger zone prefix (the usual BaySF
#'   form): adding the prefix follows from your declaration, but would be a mere
#'   guess if the system had only been defaulted -- so the import stops and asks.
#'   See \code{\link{import_sample_concentric_pre_to_fe_inventory}}.
#'
#' @param check_envelope Logical, default \code{TRUE}; see
#'   \code{\link{import_sample_concentric_pre_to_fe_inventory}}.
#'
#' @return An \code{\link{fe_inventory}} object
#'   (\code{fe_ccircle_spatial} plots for \code{"sample_concentric"},
#'   \code{fe_stand} plots for \code{"standwise_relascope"}).
#'
#' @seealso \code{\link{import_sample_concentric_pre_to_fe_inventory}},
#'   \code{\link{import_standwise_relascope_pre_to_fe_inventory}}
#'
#' @export
#'
#' @examples
#' # Concentric-circle sample inventory in BaySF-style preprocessed form
#' inv_path <- system.file("extdata", "data_ex3_sample_pre",
#'                         package = "FeNEU")
#' read_and_convert_data(inv_path, inventory_type = "sample_concentric",
#'                       coord_sys = "gk4")
#'
read_and_convert_data <- function(input_path,
                                  inventory_type = c("sample_concentric",
                                                     "standwise_relascope"),
                                  style     = NULL,
                                  coord_sys = NULL,
                                  check_envelope = TRUE) {

  inventory_type <- match.arg(inventory_type)

  if (inventory_type == "sample_concentric") {
    if (is.null(style)) style <- "baysf"
    return(
      import_sample_concentric_pre_to_fe_inventory(
        input_path, style = style, coord_sys = coord_sys,
        check_envelope = check_envelope
      )
    )
  }

  # standwise_relascope
  if (is.null(style)) style <- "silvarith"
  import_standwise_relascope_pre_to_fe_inventory(input_path, style = style)
}



#' Convert Raw Inventory in an fe_inventory Compatible Format
#'
#' Converts raw inventory data into a tibble which has the required format for
#' becoming an \code{fe_inventory} object. Typically the input data have been
#' previously imported with \code{\link{import_data}}. This function is usually
#' not used in a standalone way, but typically called by other functions that
#' create \code{fe_inventory} objects.
#'
#'
#' @param inventory_list A list of tibbles with inventory data, typically the
#'   output of \code{\link{make_inventory_list}}. See documentation of
#'   \code{\link{import_data}} for details.
#'
#' @param object_type Character to define what type of objects will be save
#' as inventory points: \code{fe_stand} or children, \code{fe_stand_spatial} or
#' \code{fe_ccircle_spatial}
#'
#'
#' @return A tibble which has the required format for becoming an
#'   \code{fe_inventory} object. It has three columns, \code{plot_id},
#'   \code{plot} (each element a fe_stand or child object), and \code{area_rep}
#'   (the representation area per plot)
#'
#' @noRd
#'
convert_raw_data <- function(inventory_list, object_type = c("fe_stand",
                                                            "fe_stand_spatial",
                                                            "fe_ccircle_spatial")) {

  object_type <- match.arg(object_type)


  # add a dummy plot_id column for grouping so the plot_id can be
  # accessed later by_fe_ccircle_from_list
  inventory_list <- inventory_list |>
    dplyr::mutate(
    plot_id_g = .data$plot_id
  )


  # Progress is reported at this loop level via progressr: FeNEU only *signals*
  # each finished plot, the caller decides how (or whether) to *display* it --
  # a console bar inside progressr::with_progress(), a Shiny bar inside
  # withProgressShiny(), or nothing at all by default (silent). This is what
  # gives large inventories (thousands of plots) a live progress indicator
  # without flooding the console, and it never clashes with a Shiny frontend.
  n_plots <- dplyr::n_distinct(inventory_list$plot_id_g)
  p <- progressr::progressor(steps = n_plots)

  # Throttle redraws: signal progress at most ~200 times over the whole loop,
  # in equal chunks, instead of once per plot. Handlers that redraw on every
  # signal -- notably progressr's default txtprogressbar -- otherwise flicker on
  # inventories with thousands of plots. The chunk amounts still sum to n_plots,
  # so the bar reaches 100 %. tick() is called once per finished plot.
  .step_every <- max(1L, n_plots %/% 200L)
  .done <- 0L
  .last <- 0L
  tick <- function() {
    .done <<- .done + 1L
    if (.done - .last >= .step_every || .done == n_plots) {
      p(amount = .done - .last)
      .last <<- .done
    }
  }

  if(object_type == "fe_ccircle_spatial") {

      inventory_points <- inventory_list |>
        dplyr::group_by(.data$plot_id_g) |>
        dplyr::group_map(
          ~{
            inv_pt <- fe_ccircle_fromlist(.x,
             trees_column_name = "trees",
             circles_column_name = "circles",
             center_coord_name = "center_coordinate",
             small_trees_name = "small_trees",
             height_method = "NFI" # we should put the option already here for the
            )                       # users
            tick()
            inv_pt
          }
        )
  }

  if(object_type == "fe_stand"){

    inventory_points <- inventory_list |>
      dplyr::group_by(.data$plot_id_g) |>
      dplyr::group_map(
        ~{
          inv_pt <- fe_stand_fromlist(.x,
                               trees_column_name = "trees",
                               height_method = "NFI" # we should put the option already here for the
          )                       # users
          tick()
          inv_pt
        }
      )

  }

  # get rep_areas from inventory list (potentially more parameters in the futur)

  inv_rep_areas <- inventory_list |>
    dplyr::select("plot_id", "area_rep_ha") |>
    dplyr::rename(area_rep_ha = "area_rep_ha")



  # Build the tibble to be handed back

  inventory_full <- dplyr::tibble(
    plot_id = purrr::map_chr(
      inventory_points,
      .f = function(x) x$stand_id
    ),
    plot = inventory_points,
  ) |>
    dplyr::left_join(inv_rep_areas) # get the areas

  return(inventory_full)
}



#' Import Raw Inventory Data From a Specified Path
#'
#' Imports raw inventory data from tab-delimited text files.  The required
#' filenames depend on the chosen \code{standard}.
#'
#' Required files per standard:
#' \describe{
#'   \item{\code{"BaySF"}}{
#'     \code{fdinvbhd.txt}, \code{fdinvba.txt}, \code{fdinvkrs.txt},
#'     \code{fdvikrs.txt}, \code{fcbestku.txt}}
#' }
#'
#' Stand-wise relascope (WZP) data is not handled here. Bring the raw form into
#' the preprocessed \code{WZP_Daten.txt} with
#' \code{\link{import_standwise_relascope_format1_raw_to_pre}}, then read it with
#' \code{\link{import_standwise_relascope_pre_to_fe_inventory}} (or
#' \code{\link{read_and_convert_data}} with
#' \code{inventory_type = "standwise_relascope"}).
#'
#' @param input_path Path of the input data folder (character).
#' @param standard Character. One of \code{"BaySF"}, \code{"BW"},
#'   \code{"BW_raw"}.
#'
#' @return A named list of imported data tibbles.
#'
#' @noRd
#'
import_data <- function(input_path,
                       standard = c("BaySF", "BW", "BW_raw"),
                       encoding = "auto") {

  standard <- match.arg(standard)

  if (standard == "BaySF") {
    output_list <- import_data_BaySF(input_path, encoding = encoding)
  }

  if (standard == "BW") {
    output_list <- import_data_BW(input_path)
  }

  if (standard == "BW_raw") {
    output_list <- import_data_BW_raw(input_path)
  }

  output_list
}



#' Convert Raw Data Into an Intermediate Data Structure according to the defined
#' standards. For now "BW" and "BaySF"
#'
#' Generates a tibble of inventory points that contain the keys, the tree lists
#' and the circle Definition. The output of this function is an intermediate
#' data structure on the way from raw input data to an fe_inventory object.
#' Standard users do not need to call this function directly.
#'
#' @param x A list that contains a full raw data set (for now Bavarian State
#'   Forest standard), typically the output of \code{\link{import_data}}
#'
#' @param standard Character. Definition standard of the inventory data to read.
#'   Currently Bavarian State Forest standard and Baden-Wuerttemberg's are
#'   allowed (default setting: "BaySF"). WZP stand-wise formats are not
#'   handled here -- see \code{\link{import_data}}.
#'
#' @param coord_sys Character string specifying the coordinate reference system
#'  of the input plot coordinates. The value is a short CRS key that is resolved
#'   internally to a valid \code{sf} coordinate reference system.
#'
#'   Supported values are:
#'   \describe{
#'     \item{\code{"lonlat"}}{WGS84 geographic coordinates (EPSG:4326)}
#'     \item{\code{"etrs89"}}{ETRS89 geographic coordinates (EPSG:4258)}
#'     \item{\code{"utm32"}}{ETRS89 / UTM zone 32N (EPSG:25832)}
#'     \item{\code{"utm33"}}{ETRS89 / UTM zone 33N (EPSG:25833)}
#'     \item{\code{"gk2"}}{DHDN / Gauss-Krueger zone 2 (EPSG:31466)}
#'     \item{\code{"gk3"}}{DHDN / Gauss-Krueger zone 3 (EPSG:31467)}
#'     \item{\code{"gk4"}}{DHDN / Gauss-Krueger zone 4 (EPSG:31468)}
#'     \item{\code{"gk5"}}{DHDN / Gauss-Krueger zone 5 (EPSG:31469)}
#'   }
#'
#' @return a map with the keys, tree list and circle definition for trees >
#'   6.5 cm
#'
#' @noRd
#'
make_inventory_list <- function(x,
                                standard = c("BaySF", "BW", "BW_raw"),
                                coord_sys = NULL,
                                check_envelope = TRUE) {
  standard <- match.arg(standard)

  if (standard == "BaySF") {
    trees_and_circles_l <- read_rawdata_BaySF(
      x, coord_sys = coord_sys, check_envelope = check_envelope
    )
  }

  if (standard == "BW") {
    trees_and_circles_l <- read_rawdata_BW(x)
  }

  if (standard == "BW_raw") {
    trees_and_circles_l <- read_rawdata_BW_raw(x)
  }

  trees_and_circles_l
}





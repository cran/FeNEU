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

# The pre -> fe_inventory step of the two import chains. Each reads the
# preprocessed on-disk form (produced by the matching *_raw_to_pre converter, or
# supplied from outside in that shape) and returns a finished fe_inventory
# object. The concrete object_type follows from the inventory type, so callers
# cannot mismatch them. These are the functions read_and_convert_data() routes
# to; use them directly when you already know the inventory type.

#' Concentric Sample Inventory: Preprocessed Files to fe_inventory
#'
#' The pre-to-fe_inventory step of the concentric sample import chain. Reads a
#' folder of preprocessed BaySF-style files (\code{fdinvbhd.txt},
#' \code{fdinvba.txt}, \code{fdinvkrs.txt}, \code{fcbestku.txt},
#' \code{fdvikrs.txt}) -- produced by
#' \code{\link{import_sample_concentric_format1_raw_to_pre}} /
#' \code{\link{import_sample_concentric_format2_raw_to_pre}}, or supplied
#' directly in that shape -- and builds an \code{fe_inventory} of
#' \code{fe_ccircle_spatial} plots.
#'
#' Counterpart to \code{\link{import_standwise_relascope_pre_to_fe_inventory}}.
#'
#' @details
#' Building the inventory objects can take a while for large forest units
#' (thousands of plots). Progress is reported through the \pkg{progressr}
#' framework, one step per plot. By default nothing is shown; wrap the call to
#' see a progress bar, and the same code drives a native Shiny progress bar
#' inside \code{shiny::withProgress()} / \code{progressr::withProgressShiny()}:
#' \preformatted{
#' progressr::handlers("cli")
#' progressr::with_progress(
#'   inv <- import_sample_concentric_pre_to_fe_inventory(input_path)
#' )
#' }
#'
#' No height completion is performed here either: the returned inventory carries
#' the measured heights as they are (missing ones stay \code{NA}). Estimating
#' them is a deliberate, separate downstream step --
#' \code{pull_trees() |> height_complete_inventory() |> fill_heights_back()}.
#'
#' @section A note on the BaySF-style column names:
#' The plot-centre coordinate columns are named \code{gauss_rw}/\code{gauss_hw}
#' (Gauss-Krueger Rechtswert / Hochwert) for \strong{historical} reasons, but
#' modern BaySF exports store \strong{UTM32} (ETRS89 / UTM zone 32N) values in
#' them -- the \dQuote{gauss} name is a legacy label, not a statement about the
#' CRS (native files even carry a \code{GAUSS_KZ} zone field that no longer
#' reflects the actual system). FeNEU therefore never infers the CRS from the
#' column name: pre files it writes itself record the real CRS in a
#' \code{coord_sys} column (self-describing, so no argument is needed here),
#' while a genuine BaySF export has no such column and you must pass
#' \code{coord_sys} explicitly -- for current data that is almost always
#' \code{"utm32"}.
#'
#' @param input_path Folder holding the preprocessed BaySF-style files
#'   (character).
#' @param style Format family of the preprocessed data. Currently only
#'   \code{"baysf"} (Bavarian State Forest style); the argument exists so future
#'   concentric styles slot in without changing the call shape.
#' @param coord_sys Coordinate reference system of the plot coordinates; see
#'   \code{\link{read_and_convert_data}}. If \code{NULL}, a format-specific
#'   default is used (\code{"gk4"} for the BaySF style).
#'
#'   Geographic coordinates (\code{"lonlat"}, \code{"etrs89"}) are accepted and
#'   are \strong{reprojected to the matching UTM zone} on import -- tree
#'   positions are built metrically from distance and azimuth around the plot
#'   centre, which degrees do not allow. The plot coordinates of the returned
#'   object are then in UTM, and a message states the zone.
#'
#'   \strong{State \code{coord_sys} explicitly for Gauss-Krueger data.} BaySF
#'   files normally store the easting \emph{without} the zone prefix (a six-digit
#'   Rechtswert), which on its own is not valid Gauss-Krueger. When you declare a
#'   \code{"gk*"} system, the prefix is added to match your declaration and a
#'   message says so. When \code{coord_sys} was left \code{NULL} and merely
#'   defaulted, the same step would be a guess -- UTM eastings occupy exactly the
#'   same six-digit range -- so the import \strong{stops} and asks you to name the
#'   system instead of silently altering your coordinates.
#'
#' @param check_envelope Logical, default \code{TRUE}. Warn when the coordinates,
#'   interpreted with the given \code{coord_sys}, fall outside German-speaking
#'   Europe -- the usual sign of a \code{coord_sys} that does not match the data.
#'   It is a plausibility hint, not a restriction: set \code{FALSE} for
#'   inventories that genuinely lie elsewhere.
#'
#' @param encoding Character encoding of the preprocessed \code{.txt} files.
#'   Default \code{"auto"} detects the encoding per file (UTF-8, incl. BOM, vs.
#'   Latin-1/Windows-1252); pass an explicit encoding only to force one. The
#'   files are read entirely as text and only the consumed numeric columns are
#'   parsed, so a comma decimal is never mis-read as a thousands mark -- the same
#'   decimal-robustness / encoding handling as the raw converters.
#'
#' @return An \code{fe_inventory} of \code{fe_ccircle_spatial} plots.
#'
#' @seealso \code{\link{import_sample_concentric_format1_raw_to_pre}},
#'   \code{\link{read_and_convert_data}}
#'
#' @export
#'
#' @examples
#' # The bundled example pre carries a coord_sys column (utm32), so it is
#' # self-describing and needs no coord_sys argument.
#' inv_path <- system.file("extdata", "data_ex3_sample_pre",
#'                         package = "FeNEU")
#' inv <- import_sample_concentric_pre_to_fe_inventory(inv_path)
#' inv
#'
import_sample_concentric_pre_to_fe_inventory <- function(input_path,
                                                         style     = "baysf",
                                                         coord_sys = NULL,
                                                         check_envelope = TRUE,
                                                         encoding  = "auto") {
  style <- match.arg(style, choices = "baysf")

  # Suppress readr's per-file "Column specification" messages for the whole read
  # chain below, so they neither interleave with the progress bar nor clutter
  # the console. Restored on exit.
  old_opt <- options(readr.show_col_types = FALSE)
  on.exit(options(old_opt), add = TRUE)

  inv_list <- import_data(input_path, standard = "BaySF", encoding = encoding) |>
    make_inventory_list(standard = "BaySF", coord_sys = coord_sys,
                        check_envelope = check_envelope)

  fe_inventory(inv_list, object_type = "fe_ccircle_spatial")
}


#' Stand-Wise Relascope Inventory: Preprocessed WZP_Daten.txt to fe_inventory
#'
#' The pre-to-fe_inventory step of the stand-wise relascope
#' (\dQuote{Winkelzählprobe} / angle-count) import chain. Reads the preprocessed
#' \code{WZP_Daten.txt} -- produced by
#' \code{\link{import_standwise_relascope_format1_raw_to_pre}}, or a
#' Silvarith-style \code{WZP_Daten.txt} supplied directly -- and builds an
#' \code{fe_inventory} of \code{fe_stand} plots.
#'
#' Counterpart to \code{\link{import_sample_concentric_pre_to_fe_inventory}}.
#'
#' @details
#' Building the inventory objects can take a while for large forest units
#' (thousands of plots). Progress is reported through the \pkg{progressr}
#' framework, one step per plot. By default nothing is shown; wrap the call to
#' see a progress bar, and the same code drives a native Shiny progress bar
#' inside \code{shiny::withProgress()} / \code{progressr::withProgressShiny()}:
#' \preformatted{
#' progressr::handlers("cli")
#' progressr::with_progress(
#'   inv <- import_standwise_relascope_pre_to_fe_inventory(input_path)
#' )
#' }
#'
#' No height completion is performed here either, and missing heights are not
#' rejected: the returned inventory carries the measured heights as they are
#' (missing ones stay \code{NA}). Estimating them is a deliberate, separate
#' downstream step --
#' \code{pull_trees() |> height_complete_inventory() |> fill_heights_back()}.
#'
#' @param input_path Folder holding the preprocessed \code{WZP_Daten.txt}
#'   (character).
#' @param style Format family of the preprocessed data. Currently only
#'   \code{"silvarith"} (Silvarith-style); the argument exists so future
#'   stand-wise styles slot in without changing the call shape.
#' @param encoding Character encoding of the preprocessed \code{WZP_Daten.txt}.
#'   Default \code{"auto"} detects it (UTF-8, incl. BOM, vs.
#'   Latin-1/Windows-1252); pass an explicit encoding only to force one. The
#'   file is read entirely as text and only the consumed numeric columns are
#'   parsed, so a comma decimal is never mis-read as a thousands mark -- the same
#'   decimal-robustness / encoding handling as the raw converter.
#'
#' @return An \code{fe_inventory} of \code{fe_stand} plots.
#'
#' @seealso \code{\link{import_standwise_relascope_format1_raw_to_pre}},
#'   \code{\link{read_and_convert_data}}
#'
#' @export
#'
#' @examples
#' # Build a preprocessed WZP_Daten.txt from the bundled Silvarith-style example,
#' # then read it into an fe_inventory.
#' sil_dir <- system.file("extdata", "data_ex6_standwise_raw", package = "FeNEU")
#' out_dir <- tempdir()
#' import_standwise_relascope_format1_raw_to_pre(sil_dir, output_dir = out_dir)
#' inv <- import_standwise_relascope_pre_to_fe_inventory(out_dir)
#' inv
#'
import_standwise_relascope_pre_to_fe_inventory <- function(input_path,
                                                           style    = "silvarith",
                                                           encoding = "auto") {
  style <- match.arg(style, choices = "silvarith")

  # Suppress readr's per-file "Column specification" messages for the whole read
  # chain below, so they neither interleave with the progress bar nor clutter
  # the console. Restored on exit.
  old_opt <- options(readr.show_col_types = FALSE)
  on.exit(options(old_opt), add = TRUE)

  inv_list <- import_data_WZP_Relascope_toFeNEU(input_path, encoding = encoding)

  fe_inventory(inv_list, object_type = "fe_stand")
}

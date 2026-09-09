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
#' Import BaySF-style deadwood inventory data
#'
#' `r lifecycle::badge("experimental")`
#'
#' Wrapper that reads BaySF-style deadwood inventory data from the text file
#' \code{fdinvtot.txt} and converts the raw column names to the internal
#' FeNEU / ForestElementsR naming convention used for deadwood processing.
#' Deadwood import is not part of the first release and may change.
#'
#' @param input_path Path to the input data folder (character). The folder
#'   must contain the file \code{fdinvtot.txt} in BaySF format.
#'
#' @return A tibble with standardized deadwood variables (one row per
#'   deadwood object).
#'
#' @keywords internal
#'
#'
#'
import_deadwood_data_baysf <- function(input_path){

  dead_wood_list <- read_deadwood_data_baysf(input_path)

  rename_dead_wood_fdinvtot(dead_wood_list)
}


#' Read Dead wood Data in BaySF Style
#'
#' `r lifecycle::badge("experimental")`
#'
#' Imports raw inventory data from tab-delimited text files. Currently, these
#' data must follow the standard of the Bavarian State Forest(BaySF)
#'
#' For this version this function is kept independent from the standard
#' importing in functions. It in the future it may be an optional part of
#' import_data_BaySF
#'
#' Currently, the following text files must be present in the folder
#' \code{input_path}:
#' \describe{
#'   \item{fdinvtot.txt}{BaySF-style deadwood inventory file}
#' }
#' @param input_path Path of the input data folder (character)
#'
#' @return A list that contains the imported data files (so far only one)
#'
#' @keywords internal
#'
read_deadwood_data_baysf <- function(input_path) {

  # Read all text tables

  # Check that all the required files are in the folder
  # minimum essential must be
  # "fdinvtot"

  # Do we need to read the rest here?
  # For the time being, we will only read these tables and skip the rest

  all_files <- list.files(
    path = input_path, pattern = ".txt", all.files = TRUE,
    full.names = FALSE
  )

  ## check if the mininmum required files are there
  required_files <- c(
    "fdinvtot.txt"
  )

  i <- which(!(required_files %in% all_files))
  err_message <- paste(
    paste(required_files[i], collapse = ", ")
  )

  check_files <- list(
    rqmt_ok = length(i) == 0,
    err_message = err_message
  )

  if (!check_files$rqmt_ok) {
    stop(
      paste("missing table(s)", check_files$err_message),
      call. = FALSE
    )
  }

  # Read the identifier columns as character (like the tree/plot reader) so the
  # plot keys match and decimal/scientific corruption stays visible.
  fdinvtot <- readr::read_delim(paste0(input_path, "/fdinvtot.txt"),
                                trim_ws = TRUE,
                                col_types = readr::cols(
                                  bestkey = readr::col_character(),
                                  bestlfd = readr::col_character(),
                                  koord   = readr::col_character()
                                )
  ) # Desgleichen

  # Identifiers must be plain integer codes (see .assert_id_cols_integer()).
  .assert_id_cols_integer(fdinvtot, "fdinvtot.txt")



  # Make a list of these data frames (tibbles) and return it
  output_list <- list(
    fdinvtot = fdinvtot)

  output_list
}



#' Standardize BaySF fdinvtot column names
#'
#' Converts raw BaySF Totholz column names to the internal FeNEU /
#' ForestElementsR naming convention used for deadwood processing.
#'
#' @param output_list List returned by
#'   \code{\link{read_deadwood_data_baysf}()} containing \code{fdinvtot}.
#'
#' @return A tibble with standardized column names
#'
#' @keywords internal
#'
rename_dead_wood_fdinvtot <- function(output_list) {

  fdinvtot <- output_list$fdinvtot

  fdinvtot |>
    dplyr::mutate(
      plot_id = paste(
        .id_to_chr(.data$bestkey),
        .id_to_chr(.data$bestlfd),
        .id_to_chr(.data$koord),
        sep = "_"
      )
    ) |>
    dplyr::rename(
      dw_id    = "lfd_tot",
      dw_state = "totkat",
      dw_decay = "totzer",
      species_group  = "totbag",
      d_mean_cm   = "dm",
      height_m = "hoehe") |>
    dplyr::select(-"bestkey",
           -"bestlfd",
           -"koord" )

}











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
#' Import Raw Inventory Data in BaySF Style
#'
#' Imports raw inventory data from tab-delimited text files. Currently, these
#' data must follow the standard of the Bavarian State Forest (BaySF)
#'
#' Currently, the following text files must be present in the folder
#' \code{input_path}:
#' \describe{
#'   \item{lohr_filt.txt}{aggregated and preprocessed data}
#' }
#'
#' @param input_path Path of the input data folder (character)
#'
#'
#' @return A list that contains the five imported data files (see Details) as a
#'   tibble each.
#'
#' @noRd
#'
import_data_BW <- function(input_path) {

  warning(fe_msg("reader_bw_construction"), call. = FALSE)

  alltreedata <- readr::read_delim(
    paste0(input_path, "/lohr_filt.txt"),
    trim_ws = TRUE
  )

  output_list <- list(alltreedata = alltreedata)

  output_list
}

#' Import Raw Inventory Data in BaySF Style
#'
#' Imports raw inventory data from tab-delimited text files. Currently, these
#' data must follow the standard of the Bavarian State Forest (BaySF)
#'
#' Currently, the following text files must be present in the folder
#' \code{input_path}:
#' \describe{
#'   \item{FormBaum.txt: }{Single tree data}
#'  \item{FormSTP.txt: }{Coordinates, representation areas and similar
#'    information about each inventory plot}
#'  \item{KonfigProbekreise.txt: }{Precise definition of the concentric circle samples; can
#'    differ among strata in the same inventory}
#' }
#'
#' @param input_path Path of the input data folder (character)
#'
#'
#' @return A list that contains the five imported data files (see Details) as a
#'   tibble each.
#'
#' @noRd
#'
import_data_BW_raw <- function(input_path) {

  warning(fe_msg("reader_bw_construction"), call. = FALSE)

all_files <- list.files(
  path = input_path, pattern = ".txt", all.files = TRUE,
  full.names = FALSE
)

## check if the mininmum required files are there
required_files <- c(
  "FormBaum.txt", "FormSTP.txt", "KonfigProbekreise.txt"
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

FormBaum <- readr::read_delim(paste0(input_path, "/FormBaum.txt"),
                              trim_ws = TRUE
) #

FormSTP <- readr::read_delim(paste0(input_path, "/FormSTP.txt"),
                             trim_ws = TRUE
) #

KonfigProbekreise <- readr::read_delim(paste0(input_path,
                                              "/KonfigProbekreise.txt"),
                                       trim_ws = TRUE
) #


# Make a list of these data frames (tibbles) and return it
output_list <- list(
  FormBaum = FormBaum,
  FormSTP  = FormSTP,
  KonfigProbekreise = KonfigProbekreise
)

output_list


}


#' Convert Raw Data Into an Intermediate list for Baden-Wuttermberg standard
#'
#' Gererates a tibble of inventory points that contain the keys, the tree lists
#' and the circle Definition.
#'
#' @param x a RData list that contains a full raw data set (for now Bavarian
#'   State Forest standard), typically the output of \code{\link{import_data}}
#'
#' @return a map with the keys, tree list and circle definition for trees >
#'   6.9cm
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' raw <- import_data("path/to/bw_data", standard = "BW_raw")
#' read_rawdata_BW_raw(raw)
#' }
#'
read_rawdata_BW_raw <- function(x) {

  warning(fe_msg("reader_bw_construction"), call. = FALSE)

  trees <- x$FormBaum |>
    dplyr::rename(
      plot_id = "STP",
      tree_id = "OZ40",
      inv_alt = "OZ49",
      ba_fe = "OZ41_ger_nfi", # tree species code (for now manually converted to NFI)
      bhd = "OZ44", # dbh
      alter = "OZ47", # Altersstuffe, Alter = Altersstuffe * 10 (see below)
      bhd_h = "OZ46", # height
      mortal = "OZ16", # Baumstatus, mortal = 4
      pol_wink = "OZ42", # distance
      pol_entf = "OZ43", # angle
    ) |>
    dplyr::mutate(
      plot_id = .id_to_chr(.data$plot_id), # character, non-scientific (see .id_to_chr)
      pol_wink = .data$pol_wink * 360 / 400, # (convert GON to 360°))
      pol_entf = .data$pol_entf * 100,
      bhd_anz = 1, # number of trees per plot (in this case all trees are counted as one)
    ) # convert to cm to match BaySF

  # select the tree information
  trees <- trees |>
    dplyr::select(
      "plot_id", "tree_id", "ba_fe", "inv_alt", "bhd",
      "bhd_h", "pol_entf", "pol_wink", "alter", "bhd_anz",
      "mortal"
    ) |>
    dplyr::mutate(
      inv_alt = as.numeric(.data$alter) * 10, # Altersstuffe * 10
      mortal = ifelse(is.na(.data$mortal), 0, 1), # all values in OZ16 are NA, so assumed is all-alive
      layer_key = dplyr::case_when(.data$alter == "UH" ~ 4,
                                   .data$alter == "US" ~ 2,
                                   .default = 1
      )
    ) # there are no definitions for layers

   # check for duplicated rows (trees)

  if (any(duplicated(trees))) {

    trees_u <- unique(trees)

    nr<- nrow(trees) - nrow(trees_u)

    warning(fe_msg("dup_tree_rows_bw", nr))

    trees <- trees_u
  }

  center_coordinate <- x$FormSTP |>
    dplyr::rename(plot_id = "OZ2",
                  X_coord = "OZ3",   # X
                  Y_coord =  "OZ4",  # Y
                  erfdat = "OZ29") |>
    dplyr::mutate( plot_id = .id_to_chr(.data$plot_id),# character, non-scientific (see .id_to_chr)
      erfdat =  as.numeric(format(as.Date(.data$erfdat, format = "%d.%m.%y"), "%Y")))|> # year
    # center coordinate in UTM
    dplyr::mutate(CRS = c("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

    # Currently, we have no better information about the rep. areas in BW style
    # data
    spacing_grid <- 100 * 200 # m

    center_coordinate <- center_coordinate |>
      dplyr::mutate(area_rep_ha = spacing_grid / 10000)


  # circle definition according to BW

  circle <- x$KonfigProbekreise |>
    dplyr::rename(dbh_lower = "Aufnahmegrenze") |> # dbh lower limit
    dplyr::mutate(c_area= .data$Kreisradius^2 * pi / 10000) |>  # convert to ha
    dplyr::select(-"ID")

  # replicate the circle defintition to all plot_ids to match BaySFs structure

  plot_ids <- center_coordinate |>
    dplyr::select("plot_id", "erfdat")

  circles <- plot_ids |>
    dplyr::distinct(.data$plot_id, .data$erfdat) |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::mutate(data = purrr::map(.data$plot_id, ~circle)) |>
    tidyr::unnest(.data$data)


  trees_l <- trees |>
    dplyr::mutate(ba_fe = .data$ba_fe |> ## tree species code
                    ForestElementsR::fe_species_ger_nfi_2012()) |>
    dplyr::group_by(.data$plot_id) |>
    tidyr::nest() |>
    dplyr::rename(trees = "data") |>
    dplyr::ungroup()

  circles_l <- circles |>
    dplyr::group_by(.data$plot_id) |>
    tidyr::nest() |>
    dplyr::rename(circles = "data") |>
    dplyr::ungroup()

  centers_l <- center_coordinate |>
    dplyr::group_by(.data$plot_id, .data$area_rep_ha) |>
    tidyr::nest() |>
    dplyr::rename(center_coordinate = "data") |>
    dplyr::ungroup()


  trees_and_circles_l <-
    dplyr::left_join(circles_l, trees_l) |>
    dplyr::left_join(centers_l) |>
    dplyr::mutate(
      plot_id = .data$plot_id
    )

  trees_and_circles_l
}



#' Convert Raw Data Into an Intermediate list for Baden-Wuttermberg standard
#'
#' Gererates a tibble of inventory points that contain the keys, the tree lists
#' and the circle Definition.
#'
#' @param x a RData list that contains a full raw data set (for now Bavarian
#'   State Forest standard), typically the output of \code{\link{import_data}}
#'
#' @return a map with the keys, tree list and circle definition for trees >
#'   6.9cm
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' raw <- import_data("path/to/bw_data", standard = "BW")
#' read_rawdata_BW(raw)
#' }
#'
read_rawdata_BW <- function(x) {

  warning(fe_msg("reader_bw_construction"), call. = FALSE)

  x <- x$alltreedata |>
    dplyr::rename(
      plot_id = "OZ2",
      tree_id = "OZ40",
      inv_alt = "OZ49",
      ba_fe = "OZ41_ger_nfi",
      bhd = "OZ44", # dbh
      alter = "OZ47", # Altersstuffe, Alter = Altersstuffe * 10 (see below)
      bhd_h = "OZ46", # height
      mortal = "OZ16", # Baumstatus, mortal = 4
      pol_wink = "OZ42", # distance
      pol_entf = "OZ43", # angle
      X_coord = "X-Wert", # coordinates, in BW it should be Oz3, but with Lorr they area split in two columns
      Y_coord = "Y-Wert"
    ) |>
    dplyr::mutate(
      plot_id = .id_to_chr(.data$plot_id), # character, non-scientific (see .id_to_chr)
      pol_wink = .data$pol_wink * 360 / 400, # (convert GON to 360°))
      pol_entf = .data$pol_entf * 100,
      bhd_anz = 1, # number of trees per plot (in this case all trees are counted as one)
    ) # convert to cm to match BaySF

  # select the tree information
  trees <- x |>
    dplyr::select(
      "plot_id", "tree_id", "ba_fe", "inv_alt", "bhd",
      "bhd_h", "pol_entf", "pol_wink", "alter", "bhd_anz"
    ) |>
    dplyr::mutate(
      inv_alt = as.numeric(.data$alter) * 10, # Altersstuffe * 10
      mortal = 0, # all values in OZ16 are NA, so assumed is all-alive
      layer_key = dplyr::case_when(.data$alter == "UH" ~ 4,
                                   .data$alter == "US" ~ 2,
                                   .default = 1
      )
    ) # there are no definitions for layers



  # circle definition according to BW

  dbh_lower <- c(0, 15, 30)
  radiuses <- c(3, 6, 12)
  c_area <- radiuses^2 * pi / 10000

  circle <- cbind(dbh_lower, c_area)

  # replicate the circle defintition to all plot_ids to match BaySFs structure

  plot_ids <- x |>
    dplyr::select(plot_id)

  circles <- plot_ids |>
    dplyr::distinct(.data$plot_id) |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::mutate(data = purrr::map(.data$plot_id, ~circle)) |>
    tidyr::unnest(.data$data) |>
    dplyr::mutate(
      dbh_lower = .data$data[, 1],
      c_area = .data$data[, 2]
    ) |>
    dplyr::select(-"data") |>
    dplyr::mutate(erfdat = 2022) # there should be a column for BW in OZ29, in Lohr n.a.)

  # center coordinate in UTM

  center_coordinate <- x |>
    dplyr::select("plot_id", "X_coord", "Y_coord") |>
    dplyr::distinct(.data$plot_id, .data$X_coord, .data$Y_coord) |>
    dplyr::mutate(CRS = c("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  # Currently, we have no better information about the rep. areas in BW style
  # data
  spacing_grid <- 100 * 200 # m

  center_coordinate <- center_coordinate |>
    dplyr::mutate(area_rep_ha = spacing_grid / 10000)


  trees_l <- trees |>
    dplyr::mutate(ba_fe = .data$ba_fe |> ## tree species code
                    ForestElementsR::fe_species_ger_nfi_2012()) |>
    dplyr::group_by(.data$plot_id) |>
    tidyr::nest() |>
    dplyr::rename(trees = "data") |>
    dplyr::ungroup()

  circles_l <- circles |>
    dplyr::group_by(.data$plot_id) |>
    tidyr::nest() |>
    dplyr::rename(circles = "data") |>
    dplyr::ungroup()

  centers_l <- center_coordinate |>
    dplyr::group_by(.data$plot_id, .data$area_rep_ha) |>
    tidyr::nest() |>
    dplyr::rename(center_coordinate = "data") |>
    dplyr::ungroup()


  trees_and_circles_l <-
    dplyr::left_join(circles_l, trees_l) |>
    dplyr::left_join(centers_l) |>
    dplyr::mutate(
      plot_id = .data$plot_id
    )

  trees_and_circles_l
}



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
#' Import Inventory Data in BaySF Style
#'
#' Imports raw inventory data from tab-delimited text files. Currently, these
#' data must follow the standard of the Bavarian State Forest (BaySF)
#'
#' Currently, the following text files must be present in the folder
#' \code{input_path}:
#' \describe{
#'   \item{fdinvbhd.txt: }{Tree records. A tree with a position is a single
#'     tree, identified by its position; only a record without position may
#'     stand for several trees of the same dimensions (\code{bhd_anz} > 1)}
#'   \item{fdinvba.txt: }{One row per cohort of a plot (species, stand age and
#'     stand layer), to which the tree records refer through \code{lfd_satz}}
#'  \item{fdinvkrs.txt: }{Coordinates, representation areas and similar
#'    information about each inventory plot}
#'  \item{fdvikrs.txt: }{Precise definition of the concentric circle samples; can
#'    differ among strata in the same inventory}
#'  \item{fcbestku.txt: }{Stand layer coding and definitions}
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
import_data_BaySF <- function(input_path, encoding = "auto") {

  # Read all text tables

  # Check that all the required files are in the folder
  # minimum essential must be
  # "fdinvbhd", "fdinvba", "fdinvkrs", "fdvikrs",
  # and "fcbest_ku" (for the layer definitions)

  # Do we need to read the rest here?
  # For the time being, we will only read these tables and skip the rest

  all_files <- list.files(
    path = input_path, pattern = ".txt", all.files = TRUE,
    full.names = FALSE
  )

  ## check if the mininmum required files are there
  required_files <- c(
    "fdinvbhd.txt", "fdinvba.txt", "fdinvkrs.txt", "fdvikrs.txt",
    "fcbestku.txt"
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
      fe_msg("tables_missing", check_files$err_message),
      call. = FALSE
    )
  }

  # Read every column as text (encoding-aware, per the package-wide decimal-
  # robustness / encoding convention) and parse only the whitelisted numeric
  # columns (import_column_standards.R). This keeps a comma decimal from being
  # eaten as a thousands mark at read time and gives all data imports the same
  # encoding handling as the raw converters.
  #
  # Identifier columns (bestkey, bestlfd, koord) are thus character by
  # construction, never numeric -- identifiers are text by definition, which
  # avoids the scientific-notation corruption that numeric coercion causes (a
  # round stand key 324000000 must not become "3.24e+08"). A consequence is that
  # textually inconsistent keys across files (e.g. "500000.00" vs "500000" for
  # the same stand) no longer silently compare equal -- that is an upstream data
  # error the user must fix, and surfacing it is intended, not papered over.
  fcbestku <- .read_delim_auto(paste0(input_path, "/fcbestku.txt"),
                               encoding = encoding, as_text = TRUE)

  fdinvba <- .read_delim_auto(paste0(input_path, "/fdinvba.txt"),
                              encoding = encoding, as_text = TRUE) |>
    .parse_numeric_cols(.baysf_ba_numeric)

  fdinvbhd <- .read_delim_auto(paste0(input_path, "/fdinvbhd.txt"),
                               encoding = encoding, as_text = TRUE) |>
    .parse_numeric_cols(.baysf_bhd_numeric)

  fdinvkrs <- .read_delim_auto(paste0(input_path, "/fdinvkrs.txt"),
                               encoding = encoding, as_text = TRUE) |>
    .parse_numeric_cols(.baysf_krs_numeric)

  fdvikrs <- .read_delim_auto(paste0(input_path, "/fdvikrs.txt"),
                              encoding = encoding, as_text = TRUE) |>
    .parse_numeric_cols(.baysf_vikrs_numeric)

  # Identifiers must be plain integer codes - reject decimal/scientific/text
  # formatting early and clearly (the alternative is a confusing downstream
  # failure when trees cannot be matched to their plots).
  .assert_id_cols_integer(fdinvba,  "fdinvba.txt")
  .assert_id_cols_integer(fdinvbhd, "fdinvbhd.txt")
  .assert_id_cols_integer(fdinvkrs, "fdinvkrs.txt")

  # Make a list of these data frames (tibbles) and return it
  output_list <- list(
    fcbestku = fcbestku,
    fdinvba  = fdinvba,
    fdinvbhd = fdinvbhd,
    fdinvkrs = fdinvkrs,
    fdvikrs  = fdvikrs
  )

  output_list
}


#' Checks if Input Data Compise all Necessary Information According to the
#' Bavarian State Forest standards
#'
#' @param raw_data Input data to be checked, i.e. a list of data frames, each
#'   representing one of the standard tables of the Bavarian State Forest
#'   inventory data definitions, including the names of the tables. Currently
#'   four tables are required, namely \emph{fdinvbhd}, \emph{fdinvba},
#'   \emph{fdinvkrs}, and \emph{fdvikrs}
#'
#' @return If the check fails, the function terminates with an error. If not, it
#'   returns a list of column name vectors that are required to build an
#'   intermediate inventory list with the function
#'   \code{read_rawdata_BaySF}
#'
#' @keywords internal
#'
check_raw_data_BaySF <- function(raw_data) {
  required_names <- c(
    "fdinvbhd", "fdinvba", "fdinvkrs", "fdvikrs"
  )
  check_rslt <- ForestElementsR::has_required_names(raw_data, required_names)

  if (!check_rslt$rqmt_ok) {
    stop(
      fe_msg("tables_missing", check_rslt$err_message),
      call. = FALSE
    )
  }


  # check trees
  trees <- raw_data$fdinvbhd

  used_columns_trees <- c(
    "fe_period", "bestkey", "bestlfd", "koord", "lfd_satz", "lfd_bhd",
    "bhd", "bhd_h", "pol_wink", "pol_entf", "mortal", "hmb", "bhd_anz"
  ) # it may be that mortal has another name

  check_rslt <- ForestElementsR::has_required_names(
    trees,
    used_columns_trees
  )


  # check species
  species <- raw_data$fdinvba

  used_columns_species <- c(
    "fe_period", "bestkey", "bestlfd", "koord", "ba_fe",
    "lfd_satz"
  )

  check_rslt <- ForestElementsR::has_required_names(
    species,
    used_columns_species
  )

  # check if age is available if not issue a warning
  check_rslt <- ForestElementsR::has_required_names(
    species,
    "inv_alt"
  )

  if (!check_rslt$rqmt_ok) {
    warning(
      fe_msg("cols_missing", check_rslt$err_message),
      call. = FALSE
    )
  } else {
    used_columns_species <- append(used_columns_species, "inv_alt")
  }

  # add layer key (no names for this see fcbestku)
  used_columns_species <- append(used_columns_species, "best_ku")

  # check coordinates of the center
  circles_keys <- raw_data$fdinvkrs

  used_names_center_coord <- c(
    "bestkey", "bestlfd",
    "koord", "gauss_rw", "gauss_hw"
  )

  check_rslt <- ForestElementsR::has_required_names(
    circles_keys,
    used_names_center_coord
  )

  if (!check_rslt$rqmt_ok) {
    stop(
      fe_msg("cols_missing", check_rslt$err_message),
      call. = FALSE
    )
  }


  # check circle keys
  used_names_circ_keys <- c(
    "fe_period", "erfdat", "bestkey",
    "bestlfd", "koord", "fe_ikl", "neigung",
    "repfl"
  )

  check_rslt <- ForestElementsR::has_required_names(
    circles_keys,
    used_names_circ_keys
  )

  if (!check_rslt$rqmt_ok) {
    stop(
      fe_msg("cols_missing", check_rslt$err_message),
      call. = FALSE
    )
  }


  # check circle definition
  circles_def <- raw_data$fdvikrs

  used_columns_circles <- c("fe_ikl", "bhd_von", "kreis_fl")

  check_rslt <- ForestElementsR::has_required_names(
    circles_def,
    used_columns_circles
  )

  if (!check_rslt$rqmt_ok) {
    stop(
      fe_msg("cols_missing", check_rslt$err_message),
      call. = FALSE
    )
  }

  # Assemble output
  cols_names <- list(
    trees        = used_columns_trees,
    species      = used_columns_species,
    center_cords = used_names_center_coord,
    circ_keys    = used_names_circ_keys,
    circ_def     = used_columns_circles
  )

  cols_names
}




#' Convert Raw Data Into an Intermediate list for the Bavarian STate forests
#' standard
#'
#' Gererates a tibble of inventory points that contain the keys, the tree lists
#' and the circle Definition.
#'
#' @param x a RData list that contains a full raw data set (for now Bavarian
#'   State Forest standard), typically the output of \code{\link{import_data}}
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
#'   6.5cm
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' inv_path <- system.file("extdata", "data_ex3_sample_pre", package = "FeNEU")
#' raw <- import_data(inv_path, standard = "BaySF")
#' read_rawdata_BaySF(raw, coord_sys = "gk4")
#' }
#'
read_rawdata_BaySF <- function(x,
                               coord_sys = NULL,
                               check_envelope = TRUE) { # Peter: Function name needs revision, this
                                    # function does not read.
  cols_names <- check_raw_data_BaySF(x)

  # test for Na flags

  x$fdinvbhd <- x$fdinvbhd |>
    dplyr::mutate(bhd_h = ifelse(is.na(.data$hmb), NA, .data$bhd_h))


  used_columns_trees <- cols_names[["trees"]]

  used_columns_species <- cols_names[["species"]]

  used_names_center_coord <- cols_names[["center_cords"]]

  used_names_circ_keys <- cols_names[["circ_keys"]]

  used_columns_circles <- cols_names[["circ_def"]]


  #  full tree list
  trees <- x$fdinvbhd


  # workaround to add a flag for all tree_ids
  if("flag_tree_id" %in% colnames(trees)){

    used_columns_trees <- append(used_columns_trees, "flag_tree_id")

    # also for small trees
    small_trees <- trees |>
      dplyr::filter(.data$bhd == 0) |>
      dplyr::mutate(tree_id = paste(.id_to_chr(.data$lfd_satz),
                                    .id_to_chr(.data$lfd_bhd),
                                    sep = "_"
      )) |>
      dplyr::select(
        "bestkey",
        "bestlfd",
        "koord",
        "tree_id",
        "lfd_satz",
        "bhd",
        "bhd_h",
        "bhd_anz",
        "flag_tree_id"
      )



  } else {

    # small trees

    small_trees <- trees |>
      dplyr::filter(.data$bhd == 0) |>
      dplyr::mutate(tree_id = paste(.id_to_chr(.data$lfd_satz),
                                    .id_to_chr(.data$lfd_bhd),
                                    sep = "_"
      )) |>
      dplyr::select(
        "bestkey",
        "bestlfd",
        "koord",
        "tree_id",
        "lfd_satz",
        "bhd",
        "bhd_h",
        "bhd_anz",
      )


  }


  ## only trees that are larger than 0 cm
  #
  trees <- trees[used_columns_trees] |>
    dplyr::filter(.data$bhd > 0)


  trees$tree_id <- paste(.id_to_chr(trees$lfd_satz),
                         .id_to_chr(trees$lfd_bhd),
                         sep = "_"
  )



  #  species list connected to trees
  species <- x$fdinvba

  species <- species[used_columns_species]

  species <- species |> dplyr::rename(layer_key = "best_ku") |>
    # change the -1 code for dead trees to 1 (with abs)
    # larger than 4 is 4
    dplyr::mutate(layer_key = ifelse(.data$layer_key > 4,
                                     4,
                                     abs(.data$layer_key)
    ))


  # concentric circles keys
  circles_keys <- x$fdinvkrs



  # Define coordinate reference system (here GK zone 4)
  # in the future this should be read from the data (with a function)

  center_coordinate <- circles_keys[used_names_center_coord]
  # center_coordinate$gauss_rw <- center_coordinate$gauss_rw + 4000000 # add zone to X-coord
  # center_coordinate$CRS <- c("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=4500000 + y_0=0 +ellps = bessel +datum=potsdam +units=m +no_defs")


  # The CRS may travel with the data: FeNEU's raw->pre converters resolve it once
  # (from the raw file's own declaration or the user's argument) and write it into
  # fdinvkrs, so a FeNEU-made pre file is self-describing. A genuine BaySF file
  # has no such column, and then the coord_sys argument speaks. An explicit
  # argument always wins over the carried value, but a disagreement is surfaced
  # rather than settled silently.
  coord_sys_carried <- if ("coord_sys" %in% names(circles_keys)) {
    .parse_coord_sys_decl(circles_keys$coord_sys)
  } else {
    NA_character_
  }
  if (!is.null(coord_sys) && !is.na(coord_sys_carried) &&
      !identical(tolower(coord_sys), coord_sys_carried)) {
    warning(fe_msg("coord_sys_conflict", "fdinvkrs.txt/coord_sys",
                   coord_sys_carried, tolower(coord_sys)),
            call. = FALSE)
  }
  if (is.null(coord_sys) && !is.na(coord_sys_carried)) {
    coord_sys <- coord_sys_carried
    message(fe_msg("coord_sys_from_file", "fdinvkrs.txt/coord_sys", coord_sys))
  }

  # Whether the CRS was actually DECLARED (by the user or by the pre file) decides
  # how far we may go when the eastings look un-prefixed (see
  # fe_normalise_coordinates).
  coord_sys_declared <- !is.null(coord_sys)
  if (is.null(coord_sys)) coord_sys <- "gk4"  # Default for BaySF

  center_coordinate <- fe_normalise_coordinates(
    coordinate = center_coordinate,
    coord_sys = coord_sys,
    coord_sys_declared = coord_sys_declared,
    check_envelope = check_envelope
  )

  CRS <- attr(center_coordinate, "fe_crs")

  center_coordinate$epsg <- CRS$epsg

  # concentric circles keys
  circles_keys <- circles_keys[used_names_circ_keys] |>
    dplyr::rename(area_rep_ha = "repfl")

  # concentric circles definition
  circles_def <- x$fdvikrs

  circles_def <- circles_def[used_columns_circles]
  colnames(circles_def) <- c("fe_ikl", "dbh_lower", "c_area")

  # Local geometry of the circle centres, relative to the inventory point.
  # All circles share the point centre (0, 0) except the innermost one
  # (dbh_lower == 0), which sits 5 m NORTH of it. That offset is intentional:
  # it is the BaySF field convention, not a placeholder and not an error.
  # It is visible in plot.fe_ccircle_spatial() as a non-concentric small
  # circle - do not "fix" it.
  circles_def <- circles_def |>
    dplyr::mutate(
      x = 0,
      y = ifelse(.data$dbh_lower == 0, 5, 0)
    )|>
    sf::st_as_sf(coords = c("x", "y"))


  # merge data files
  trees <- merge(trees, species)


  small_trees <- merge(small_trees, species) |>
    dplyr::select(
      -"lfd_satz",
      -"fe_period"
    )


  circles <- merge(circles_keys, circles_def)

  # Detect date format using the first non-NA result

  best_format <- detect_date_format(circles$erfdat)

  if (is.na(best_format)) {
    stop(fe_msg("date_format_undetected"))
  }

  circles <- circles |>
   dplyr::mutate(erfdat = as.numeric(format(as.Date(.data$erfdat,
                                                                 format = best_format), "%Y")))  # convert here

  circles$fe_ikl <- NULL

  # `neigung` is the BaySF plot-level slope in PERCENT; convert it to the
  # internal `slope` in degrees (circle_definition$slope is defined in degrees)
  # used for the horizontal-projection area correction of the concentric
  # circles: slope_deg = atan(neigung / 100) * 180 / pi.
  circles <- circles |>
    dplyr::mutate(slope = atan(.data$neigung / 100) * (180 / pi)) |>
    dplyr::select(-"neigung")


  trees_l <- trees |>
    dplyr::mutate(ba_fe = .data$ba_fe |> ## tree species code
                    ForestElementsR::fe_species_bavrn_state()) |>
    dplyr::group_by(.data$bestkey, .data$bestlfd, .data$koord) |>
    tidyr::nest() |>
    dplyr::rename(trees = "data") |>
    dplyr::ungroup()


  small_trees_l <- small_trees |>
    dplyr::mutate(ba_fe = .data$ba_fe |> ## tree species code
                    ForestElementsR::fe_species_bavrn_state()) |>
    dplyr::group_by(.data$bestkey, .data$bestlfd, .data$koord) |>
    tidyr::nest() |>
    dplyr::rename(small_trees = "data") |>
    dplyr::ungroup()


  circles_l <- circles |>
    dplyr::group_by(.data$bestkey, .data$bestlfd, .data$koord, .data$area_rep_ha) |>
    tidyr::nest() |>
    dplyr::rename(circles = "data") |>
    dplyr::ungroup()

  centers <- center_coordinate |>
    dplyr::rename(X_coord = "gauss_rw", Y_coord = "gauss_hw") |>
    dplyr::group_by(.data$bestkey, .data$bestlfd, .data$koord) |>
    tidyr::nest() |>
    dplyr::rename(center_coordinate = "data") |>
    dplyr::ungroup()


  trees_and_circles_l <-
    dplyr::left_join(circles_l, trees_l) |>
    dplyr::left_join(small_trees_l) |>
    dplyr::left_join(centers) |>
    dplyr::mutate(
      # plot_id = bestkey_bestlfd_koord. The third part, `koord`, is an OPAQUE,
      # plot-unique identifier -- NOT a coordinate carrier. Plot coordinates
      # come exclusively from the geometry (coord_x/coord_y -> gauss_rw/gauss_hw
      # above), never from parsing the plot_id. In genuine BaySF data `koord`
      # happens to be Gauss-coordinate-derived, but no evaluation may rely on
      # that: Format 1 fills `koord` with the user's plot_id (any integer key),
      # so decoding coordinates from it would misread Format-1 inventories. It
      # is used only as an equality key (e.g. baysf-style repeated-inventory
      # matching in match_2_inventories_by_plot_id()).
      plot_id = paste(.id_to_chr(.data$bestkey), .id_to_chr(.data$bestlfd),
                      .id_to_chr(.data$koord), sep = "_"),
      area_rep_ha = .data$area_rep_ha
    )






  trees_and_circles_l
}


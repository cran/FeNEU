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
#' Generate a \link[ForestElementsR]{fe_ccircle_spatial} Object From an Output
#' Row of \code{make_inventory_list}
#'
#' Standard users do not need to call this function directly. Its typical
#' usage is to apply it to each row of the output of
#' \code{make_inventory_list} and to obtain a list of
#' \link[ForestElementsR]{fe_ccircle_spatial} objects this way. Data standard
#' is Bavarian State Forest (BaySF) so far.
#'
#' @param x a data frame or a list with unequivocal key, and two columns with
#'   data frames for trees and circle definition, typically the output of
#'   \code{make_inventory_list}.
#'
#' @param height_method height estimation method for trees with missing heights,
#'   "NFI" height curves are defined as default, alternatively "Bavaria" can be
#'   selected, but in this case tree ages must be provided
#'
#' @param trees_column_name name of the column where the tree lists are stored
#'
#' @param circles_column_name name of the column where the circle definitions
#'   are stored
#'
#' @param center_coord_name name of the column containing the center coordinates
#'   in Gauss-Krueger
#'
#' @param small_trees_name name of the column containing the small trees, i.e.
#'   trees with no dbh because their height is < 1.3 m
#'
#'
#' @return an object of \code{\link[ForestElementsR]{fe_ccircle_spatial}}
#'
#' @keywords internal
#'
#'
fe_ccircle_fromlist <- function(x,
                                trees_column_name,
                                circles_column_name,
                                center_coord_name,
                                small_trees_name,
                                height_method = c("NFI", "Bavaria")) {
  # x <- trees_and_circles_l

  # create list to be read by fe_ccircle_spatial

  x_l <- list()

duplicate_check <- x |>
  dplyr::select(stand_id = plot_id) |>
  dplyr::count(.data$stand_id)

if(duplicate_check$n > 1){
  stop(fe_msg("dup_plot_id_stand",
              paste(unique(duplicate_check$stand_id), collapse = ", ")),
       call. = FALSE)
}


  x_l$stand_id <- x$plot_id

  # read data.frame necessary for trees information

  trees <- x[[trees_column_name]]
  trees <- trees[[1]]

  # Remove non-tree records (e.g. shrubs, bavrn_state code 99) before the object
  # is built. A plot left without any trees becomes a fe_ccircle_spatial_notrees
  # object via the is.null(trees) branch below.
  trees <- .drop_non_tree_records(trees, x$plot_id)
  if (!is.null(trees) && nrow(trees) == 0L) trees <- NULL


  #  read circle information
  #  and build circle definition

  circle_def <- x[[circles_column_name]]
  circle_def <- circle_def[[1]]

  # sort dbh limits in ascending order

  circle_def <- circle_def |>
    dplyr::arrange(.data$dbh_lower)


  # check that circle definition is consistent

  if (is.na(sum(circle_def$dbh_lower))) {
    stop(fe_msg("no_area_in_plot", x$plot_id), call. = FALSE)
  }

  if (length(unique(circle_def$dbh_lower)) != nrow(circle_def)) {
    stop(fe_msg("dbh_lower_not_unique", x$plot_id), call. = FALSE)
  }

  # define the center coordinate

  center_coordinate <- x[[center_coord_name]]
  center_coordinate <- center_coordinate[[1]]


  crs_attr <- center_coordinate$epsg[1]

  # Fall back to a proj4/WKT string in a `CRS` column when no epsg code is
  # present. The BaySF import path attaches an `epsg` column, but the
  # Baden-Wuerttemberg path (read_rawdata_BW / _BW_raw) sets a `CRS` proj4
  # string instead; sf::st_set_crs() accepts either form.
  if (is.null(crs_attr) || is.na(crs_attr)) {
    crs_attr <- center_coordinate$CRS[1]
  }

  if (!is.null(crs_attr)) {
    CRS <- crs_attr
  } else {
    CRS <- NA
  }

  if (is.na(CRS)[1]) {
    stop(fe_msg("no_crs_info"), call. = FALSE)
  }

  center_coordinate <- center_coordinate |>
    dplyr::rename(
      x = "X_coord",
      y = "Y_coord"
    ) |>
    sf::st_as_sf(coords = c("x", "y")) |>
    sf::st_set_crs(CRS)


  # add definitions into the list
  x_l$circle_definition <- circle_def
  x_l$center_coordinate <- center_coordinate

  # read small trees and add them to the list
  small_trees <- x[[small_trees_name]][[1]]
  small_trees <- .drop_non_tree_records(small_trees, x$plot_id)
  if (!is.null(small_trees) && nrow(small_trees) == 0L) small_trees <- NULL

  x_l$small_trees <- dplyr::tibble(
    tree_id = NA,
    dbh_cm = NA,
    height_m = NA,
    species_id = NA,
    age_yr = NA,
    h_m_tree = NA,
    ingrowth = NA,
    removal = NA,
    tree_count = NA,
    layer_key = 1,
    time_yr = NA
  ) # bypass in order to have always a data.frame when there
  # are not small_trees

  # inventory year
  time_yr <- as.numeric(unique(circle_def$erfdat))

  if (!is.null(small_trees)) { # only if small trees are present

    x_l$small_trees <- small_trees |>
      dplyr::rename(
        dbh_cm = "bhd",
        height_m = "bhd_h",
        age_yr = "inv_alt",
        species_id = "ba_fe",
        tree_count = "bhd_anz"
      ) |>
      dplyr::mutate(
        h_m_tree = ifelse(is.na(small_trees$bhd_h), FALSE, TRUE),
        ingrowth = FALSE,
        removal = FALSE,
        time_yr = as.numeric(unique(circle_def$erfdat)),
        n_rep_ha = .data$tree_count /
          (min(circle_def$c_area) * cos(
            unique(circle_def$slope) *
              (pi / 180)
          ))
      ) # Horizontal-projection correction. Forest metrics refer to the
        # horizontal projection of the ground area. On ground sloped by
        # `slope`, the horizontally projected circle area is
        # c_area * cos(slope), so the stem density per projected hectare is
        #   n_rep_ha = tree_count / (c_area * cos(slope)),  with n_rep_ha
        # >= its value on flat ground (cos <= 1). `slope` is converted from
        # degrees to radians (pi / 180) before cos() - a single cosine, first
        # power, division; regular trees are corrected exactly the same way
        # inside ForestElementsR::n_rep_ha(). Small trees are tallied on the
        # smallest (innermost) circle, hence min(circle_def$c_area).
        #
        # circle_def$slope is in degrees; the pre-reader derives it from the
        # BaySF `neigung` column (per-point terrain slope in PERCENT) via
        # slope_deg = atan(neigung / 100) * 180 / pi.
  }

  if("flag_tree_id" %in% names(small_trees)){
    x_l$small_trees$flag_tree_id <- small_trees$flag_tree_id
  }



  # check that there are trees > 6.5 cm, if there are continue to build a
  # normal list for fe_circle_spatial if not create a list with empty trees to
  # pass it to fe_ccircle_spatial_notrees
  if (is.null(trees) == TRUE) {
    warning(fe_msg("no_trees_in_plot", x$plot_id), call. = FALSE)


    x_l$time_yr <- time_yr


    # create fe_ccircle_spatial_notrees object
    # (to be generated in ForestElementsR)
    inventory_point <- x_l |>
      ForestElementsR::fe_ccircle_spatial_notrees(
        stand_id = x_l$stand_id,
        time_yr_name = "time_yr",
      )
  } else {
    # dbh lower limit from second circle
    second_circle_dbh <- circle_def$dbh_lower[2]

    # trees that are larger than second circle lower limit must
    # have coordinates (aprox. dbh >= 12 cm)

    trees_must_coords <- trees |> dplyr::filter(.data$bhd >= second_circle_dbh)

    if (is.na(sum(trees_must_coords$pol_entf))) {
      stop(fe_msg("no_coords_in_plot", x$plot_id), call. = FALSE)
    }

    # build up trees tibble
    trees_df <- dplyr::tibble(
      tree_id = trees$tree_id,
      species_id = trees$ba_fe,
      time_yr = time_yr,
      dbh_cm = trees$bhd,
      height_m = trees$bhd_h,
      removal = as.logical(trees$mortal),
      age = NA_real_,
      layer_key = trees$layer_key,
      tree_count = trees$bhd_anz
    )

    if("flag_tree_id" %in% names(trees)){
      trees_df$flag_tree_id <- trees$flag_tree_id
    }


    # add age if it is present in the inventory data
    if (!is.na(mean(trees$inv_alt, na.rm = TRUE))) {
    trees_df$age <-   as.numeric(trees$inv_alt)
    }


    # add flag for trees with measured height
    trees_df$h_m_tree <- ifelse(is.na(trees$bhd_h), FALSE, TRUE)

    if ("hmb" %in% names(trees)) {
      trees_df$h_m_tree <- ifelse(is.na(trees$hmb), FALSE, TRUE)
    }

    x_l$trees <- trees_df

    # build up tree positions tibble. A usable position needs BOTH a distance
    # and an azimuth. If only one is present (e.g. an innermost-circle tree that
    # carries a distance but no azimuth -- legitimate, as innermost trees need
    # no position), the position is unusable: null BOTH so
    # fe_ccircle_spatial(method = "flexible") treats the tree as unpositioned
    # and skips it, instead of building a half-NA geometry that sf rejects with
    # "missing values in coordinates not allowed". Trees that DO require a
    # position (dbh at/above the second circle) were already dropped upstream by
    # .drop_trees_without_coords() when either component was missing.
    x_l$tree_positions <- trees |>
      dplyr::select(tidyselect::all_of(c(
        "tree_id",
        "pol_entf",
        "pol_wink"
      ))) |>
      dplyr::mutate(
        .incomplete_pos = is.na(.data$pol_entf) | is.na(.data$pol_wink),
        pol_entf = dplyr::if_else(.data$.incomplete_pos, NA_real_,
                                  .data$pol_entf / 100),
        pol_wink = dplyr::if_else(.data$.incomplete_pos, NA_real_,
                                  .data$pol_wink)
      ) |>
      dplyr::select(-".incomplete_pos") |>
      dplyr::distinct() |>
      dplyr::rename(
        R = "pol_entf",
        angle = "pol_wink"
      )


    x_l$time_yr <- time_yr

    inventory_point <- x_l |>
      ForestElementsR::fe_ccircle_spatial(
        method = "flexible",
        tree_id_col = "tree_id",
        species_id_col = "species_id",
        time_yr_col = "time_yr",
        age_yr_col = "age",
        dbh_cm_col = "dbh_cm",
        radius_col = "R",
        angle_col = "angle",
        stand_id = x_l$stand_id,
        layer_key_col = "layer_key",
        height_m_col = "height_m",
        removal_col = "removal",
        time_yr_name = "time_yr"
      )
  }

  # Missing tree heights are the standard case and get filled in later
  # (height_complete_inventory() / fill_heights_back()), not here. No per-plot
  # message is emitted -- announcing this once per plot flooded the console on
  # large inventories; progress is reported at the loop level in
  # convert_raw_data() via progressr instead.
  #
  # The object is left exactly as the ForestElementsR constructor validated it.
  # In particular, no-trees plots keep their canonical `trees = NULL` element:
  # the earlier `inventory_point$trees <- trees` line (removed with the message)
  # incidentally stripped that element AFTER fe_ccircle_spatial_notrees() had
  # validated it, producing objects that would fail validate_fe_ccircle_spatial_
  # notrees() on re-check. Downstream code only ever reads plot$trees via `$` /
  # is.null() / nrow() (NULL either way), so keeping the validated form has no
  # behavioural effect -- it only restores object validity.

  inventory_point
}



# Drop non-tree species records (e.g. bavrn_state code 99 "Strauch") from a raw
# tree / small-tree data frame before an fe_stand(_spatial) / fe_ccircle_spatial
# object is built. ForestElementsR's object constructors reject non-tree codes
# (a code for which fe_species_is_tree() is FALSE), so such records must be
# removed upstream. Records carrying a tree code - or an unknown (NA) code - are
# kept; only codes that are explicitly non-tree are dropped. `plot_id` is used
# for the message only.
.drop_non_tree_records <- function(df, plot_id = NA_character_) {
  if (is.null(df) || nrow(df) == 0L || is.null(df$ba_fe)) {
    return(df)
  }
  is_tree <- ForestElementsR::fe_species_is_tree(df$ba_fe)
  drop    <- is_tree %in% FALSE
  if (any(drop)) {
    message(fe_msg("non_tree_removed", plot_id, sum(drop)))
    df <- df[!drop, , drop = FALSE]
  }
  df
}



#' Generate a \link[ForestElementsR]{fe_ccircle_spatial} Object From an Output
#' Row of \code{make_inventory_list}
#'
#' Standard users do not need to call this function directly. Its typical
#' usage is to apply it to each row of the output of
#' \code{make_inventory_list} and to obtain a list of
#' \link[ForestElementsR]{fe_ccircle_spatial} objects this way. Data standard
#' is Bavarian State Forest (BaySF) so far.
#'
#' @param x a data frame or a list with unequivocal key, and two columns with
#'   data frames for trees and circle definition, typically the output of
#'   \code{make_inventory_list}.
#'
#' @param trees_column_name name of the column where the tree lists are stored
#'
#'
#' @param height_method height estimation method for trees with missing heights,
#'   "NFI" height curves are defined as default, alternatively "Bavaria" can be
#'   selected, but in this case tree ages must be provided
#'
#' @return an object of \code{\link[ForestElementsR]{fe_stand}}
#'
#' @keywords internal
#'
#'
fe_stand_fromlist <- function(x,
                                trees_column_name,
                                height_method = c("NFI", "Bavaria")) {


    # create list to be read by fe_stand


  trees <- x[["trees"]][[1]]
  trees <- .drop_non_tree_records(trees, unique(trees$plot_id))


  # read data.frame necessary for trees information


    # build up trees tibble
    trees_df <- dplyr::tibble(
      tree_id = trees$tree_id,
      species_id = trees$ba_fe,
      time_yr = trees$erfdat,
      dbh_cm = trees$bhd,
      height_m = trees$bhd_h,
      age = NA_real_,
      layer_key = trees$layer_key,
      tree_count = trees$bhd_anz,
      n_rep_ha = trees$n_rep,
      stand_id = trees$plot_id,
      area_ha = trees$area_ha
    )

    # add age if it is present in the inventory data
    if (!is.na(mean(trees$inv_alt, na.rm = TRUE))) {
      trees_df$age <- as.numeric(trees$inv_alt)
    }


    # add flag for trees with measured height
    trees_df$h_m_tree <- ifelse(is.na(trees$bhd_h), FALSE, TRUE)

    if ("hmb" %in% names(trees)) {
      trees_df$h_m_tree <- ifelse(is.na(trees$hmb), FALSE, TRUE)
    }

    area_plot <- unique(trees_df$area_ha)
    stand_id <- unique(trees_df$stand_id)


    inventory_point <- trees_df |>
      ForestElementsR::fe_stand(
        tree_id_col = "tree_id",
        species_id_col = "species_id",
        time_yr_col = "time_yr",
        age_yr_col = "age",
        dbh_cm_col = "dbh_cm",
        stand_id = stand_id,
        layer_key_col = "layer_key",
        height_m_col = "height_m",
        n_rep_ha_col = "n_rep_ha",
        area_ha   = NULL
        # time_yr_name = "time_yr"
      )

  # Missing tree heights are the standard case and get filled in later
  # (height_complete_inventory() / fill_heights_back()), not here. No per-plot
  # message is emitted -- announcing this once per plot flooded the console on
  # large inventories; progress is reported at the loop level in
  # convert_raw_data() via progressr instead.
  #
  # The object is left exactly as the ForestElementsR constructor validated it.
  # In particular, no-trees plots keep their canonical `trees = NULL` element:
  # the earlier `inventory_point$trees <- trees` line (removed with the message)
  # incidentally stripped that element AFTER fe_ccircle_spatial_notrees() had
  # validated it, producing objects that would fail validate_fe_ccircle_spatial_
  # notrees() on re-check. Downstream code only ever reads plot$trees via `$` /
  # is.null() / nrow() (NULL either way), so keeping the validated form has no
  # behavioural effect -- it only restores object validity.

  inventory_point
}






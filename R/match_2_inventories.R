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






#' Match two fe_inventory Objects on Plot Level
#'
#' Wraps the functions \code{\link{match_2_inventories_by_center_coord}}, and
#' \code{\link{match_2_inventories_by_plot_id}} and decides which one to call.
#'
#' @param inv_a An object of class \code{\link{fe_inventory}}
#'
#' @param inv_b An object of class \code{\link{fe_inventory}}
#'
#' @param match_type Character string indicating the method for matching
#'   inventory plots from the two subsequent inventories. The option
#'   "center_coord" matches points whose center coordinates have a distance of
#'   no more than \code{coord_tolerance}, while the option "plot_id" (default)
#'   matches plots with the same id.
#'
#' @param plot_id_style Character string indicating the plot_id style to be
#'   used. Only relevant when \code{match_type == "plot_id"}. The available
#'   options are "baysf" (default) and "generic". For a match, the latter
#'   ("generic") requires the full plot_ids of two plots to be equal in both
#'   inventories. The former ("baysf") only requires the last group of digits
#'   in the plot_id for a match due to the specific BaySF data format.
#'
#' @param coord_tolerance Maximum distance allowed for a match of two inventory
#'   plots in the distance unit of the coordinate reference system of
#'   \code{inv_a} and \code{inv_b}; in virtually all relevant cases it is m.
#'   Default is 10, i.e. inventory plots whose centers have a distance of up to
#'   10 meters will be accepted as matches.
#'
#' @returns A list with five elements: i) \code{matches} -- the matching
#'   inventory plots, ii) \code{in_a_only} -- plots that exist in \code{inv_a}
#'   only, iii) \code{in_b_only} -- plots that exist in \code{inv_b} only,
#'   iv) \code{multiple_matches} -- a logical vector identifying multiple
#'   matches (can only occur with \code{match_type = "center_coord"} and is
#'   undesired), and v) \code{circle_def_mismatch} -- plot pairs that matched
#'   by id or coordinates but were excluded because their circle definitions
#'   (\code{dbh_lower} and/or \code{c_area}) differ between the two inventories.
#'   Such pairs are unsuitable for tree-level increment calculations from
#'   repeated surveys. The exact types of the objects (tibbles or sf) in this
#'   list depend on the \code{match_type} the function has been called with,
#'   but they always contain a column with the relevant plot ids.
#'
#' @family increment
#' @family repeated inventory
#' @family inventory matches
#'
#' @export
#'
#' @examples
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # both options for match_type work for this example
#'   match_2_inventories(inv_a, inv_b, match_type = "plot_id", "baysf")
#'   match_2_inventories(inv_a, inv_b, match_type = "center_coord", "baysf")
#'
match_2_inventories <- function(
    inv_a, inv_b,
    match_type = c("plot_id", "center_coord"),
    plot_id_style = c("baysf", "generic"),
    coord_tolerance = 10
) {

  stopifnot(is_fe_inventory(inv_a))
  stopifnot(is_fe_inventory(inv_b))

  match_type    <- match.arg(match_type)
  plot_id_style <- match.arg(plot_id_style)

  # Apply the selected type of plot matching
  if (match_type == "center_coord") {
    match_rslt <- match_2_inventories_by_center_coord(
      inv_a, inv_b, tolerance = coord_tolerance
    )
    if (!all(!match_rslt$multiple_matches))
      warning(fe_msg("multiple_matches_coord"))

  } else { # i.e. match_type == "plot_id"
    match_rslt <- match_2_inventories_by_plot_id(
      inv_a, inv_b, style = plot_id_style
    )
    # Multiple matches are not possible here, because both inventory objects are
    # checked for unique plot ids when they are created. For compatibility
    # at the calling level, we add that information
    match_rslt$multiple_matches <- rep(FALSE, nrow(match_rslt$matches))
  }

  # Check circle definition compatibility for matched plots.
  # Plots with different circle definitions (dbh_lower and/or c_area) are
  # excluded from the matches and reported separately, because incompatible
  # circle setups make tree-level increment calculations unreliable.
  match_rslt <- check_circle_def_compatibility(match_rslt, inv_a, inv_b)

  if (nrow(match_rslt$matches) == 0)
    warning(fe_msg("no_matching_plots"))

  match_rslt
}



#' Generate Plot Match Overview Statistics from the Output of
#' \code{match_2_inventories}
#'
#' Informs about the numbers of plots that match and do not match in both
#' inventories, and the corresponding representation areas in ha.
#'
#' @param plot_matches list, output of \code{\link{match_2_inventories}}
#'
#' @param inv_a An object of class \code{\link{fe_inventory}} nust be the first
#'   inventory relating to \code{plot_matches}
#'
#' @param inv_b An object of class \code{\link{fe_inventory}} nust be the second
#'   inventory relating to \code{plot_matches}
#'
#' @returns A list containing two named vectors, \code{plot_numbers}, and
#'   \code{rep_areas_ha}. These contain the statistics relating to plot numbers
#'   and represented areas, respectively. Both include a count/area for
#'   plots excluded due to incompatible circle definitions
#'   (\code{n_circle_def_mismatch}, \code{ha_circle_def_mismatch_1st},
#'   \code{ha_circle_def_mismatch_2nd}).
#'
#' @family increment
#' @family repeated inventory
#' @family inventory matches
#'
#' @export
#'
#' @examples
#'
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # both options for match_type work for this example
#'   match_pid <- match_2_inventories(
#'     inv_a, inv_b, match_type = "plot_id", "baysf"
#'   )
#'   match_crd <- match_2_inventories(
#'     inv_a, inv_b, match_type = "center_coord", "baysf"
#'   )
#'
#'   # stats should be the same in both cases
#'   match_plot_statistics(match_pid, inv_a, inv_b)
#'   match_plot_statistics(match_crd, inv_a, inv_b)
#'
match_plot_statistics <- function(plot_matches, inv_a, inv_b) {

  # tree numbers
  n_total_1st <- nrow(inv_a)
  n_only_1st  <- nrow(plot_matches$in_a_only)
  n_total_2nd <- nrow(inv_b)
  n_only_2nd  <- nrow(plot_matches$in_b_only)
  n_in_both   <- nrow(plot_matches$matches)

  n_circle_def_mismatch <- if (!is.null(plot_matches$circle_def_mismatch))
    nrow(plot_matches$circle_def_mismatch) else 0L

  plot_numbers <- c(
    "n_total_1st" = n_total_1st, "n_only_1st" = n_only_1st,
    "n_total_2nd" = n_total_2nd, "n_only_2nd" = n_only_2nd,
    "n_in_both"   = n_in_both,
    "n_circle_def_mismatch" = n_circle_def_mismatch
  )

  # representation areas
  ha_total_1st <- sum(inv_a$area_rep_ha)

  ha_only_1st <- plot_matches$in_a_only |>
    dplyr::inner_join(inv_a, by = c("plot_id" = "plot_id")) |>
    purrr::pluck("area_rep_ha") |>
    sum()

  ha_both_1st <- plot_matches$matches |>
    dplyr::inner_join(inv_a, by = c("plot_id_inv_a" = "plot_id")) |>
    purrr::pluck("area_rep_ha") |>
    sum()

  ha_total_2nd <- sum(inv_b$area_rep_ha)

  ha_only_2nd <- plot_matches$in_b_only |>
    dplyr::inner_join(inv_b, by = c("plot_id" = "plot_id")) |>
    purrr::pluck("area_rep_ha") |>
    sum()

  ha_both_2nd <- plot_matches$matches |>
    dplyr::inner_join(inv_b, by = c("plot_id_inv_b" = "plot_id")) |>
    purrr::pluck("area_rep_ha") |>
    sum()

  if (n_circle_def_mismatch > 0) {
    ha_circle_def_mismatch_1st <- plot_matches$circle_def_mismatch |>
      dplyr::inner_join(inv_a, by = c("plot_id_inv_a" = "plot_id")) |>
      purrr::pluck("area_rep_ha") |>
      sum()
    ha_circle_def_mismatch_2nd <- plot_matches$circle_def_mismatch |>
      dplyr::inner_join(inv_b, by = c("plot_id_inv_b" = "plot_id")) |>
      purrr::pluck("area_rep_ha") |>
      sum()
  } else {
    ha_circle_def_mismatch_1st <- 0
    ha_circle_def_mismatch_2nd <- 0
  }

  rep_areas_ha <- c(
    "ha_total_1st" = ha_total_1st, "ha_only_1st" = ha_only_1st,
    "ha_both_1st"  = ha_both_1st,
    "ha_total_2nd" = ha_total_2nd, "ha_only_2nd" = ha_only_2nd,
    "ha_both_2nd"  = ha_both_2nd,
    "ha_circle_def_mismatch_1st" = ha_circle_def_mismatch_1st,
    "ha_circle_def_mismatch_2nd" = ha_circle_def_mismatch_2nd
  )

  list(plot_numbers = plot_numbers, rep_areas_ha = rep_areas_ha)
}



#' Match Two fe_inventory Objects by the Center Coordinates of the Inventory
#' Plots
#'
#' From two \code{\link{fe_inventory}} objects identify the matching and non-
#' matching inventory plots based on their center coordinates. A warning is
#' issued if the matching coordinates indicate spatial one-to-many or
#' many-to-many relationships.
#'
#' @param inv_a An object of class \code{\link{fe_inventory}}
#'
#' @param inv_b An object of class \code{\link{fe_inventory}}
#'
#' @param tolerance Maximum distance allowed for a match of two inventory plots
#'   in the distance unit of the coordinate reference system of \code{inv_a} and
#'   \code{inv_b}; in virtually all relevant cases it is m. Default is 0, i.e.
#'   exact matches are required.
#'
#' @returns A list containing three \code{sf} objects, \code{matches},
#'   \code{in_a_only}, and \code{in_b_only}, and one logical vector
#'   \code{multiple_matches}. The three \code{sf} objects indicate the
#'   inventory plots that are present in both, \code{inv_a} and \code{inv_b},
#'   only in \code{inv_a} but not in \code{inv_b}, and only in \code{inv_b} but
#'   not in \code{inv_a}, respectively. The \code{sf} object \code{matches} has
#'   the columns \code{plot_id.inv_a}, \code{plot_id.inv_b}, and \code{geometry}
#'   (i.e. the point coordinates). Thereby, The column \code{plot_id_inv_a}
#'   contains the id of a plot it has in \code{inv_a}, and analogously with
#'   \code{plot_id_inv_b}. Note that spatially matching points do not
#'   necessarily need to have the same plot_id in both inventories. The
#'   \code{sf} object \code{in_a_only} has only two columns, \code{plot_id}, and
#'   \code{geometry}. The same applies to the \code{sf} object \code{in_b_only}.
#'   the plot_id entries for the other inventories are \code{NA}. The vector
#'   \code{multiple_matches} relates to the \code{matches} object, and
#'   indicates entries with the same center coordinates. This indicates
#'   one-to-many or many-to-many relations between the two input inventories
#'   which is usually a problem. Therefore, \code{multiple_matches} should
#'   only contain \code{FALSE} entries.
#'
#' @family increment
#' @family repeated inventory
#' @family inventory matches
#'
#' @export
#'
#' @examples
#'   # Construct a dummy example with partly non-matching plots from one
#'   # fe_inventory object
#'   inv_a <- data_ex3_sample_fe_inventory[3:10, ]
#'   inv_b <- data_ex3_sample_fe_inventory[1:7,  ]
#'
#'   # As both dummy inventories were generated from the same set, we do not
#'   # require any tolerance > 0
#'   match_2_inventories_by_center_coord(inv_a, inv_b, tolerance = 0)
#'
#'
#'   # Now a real example from two subsequent inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # Requires a tolerance of 10.0 m in order to obtain the correct match
#'   # (as indicated by the plot ids)
#'   match_2_inventories_by_center_coord(inv_a, inv_b, tolerance = 10.0)
#'
#'   # If tolerance is set too high (here unrealistic 3000 m for demo purposes),
#'   # we obtain duplicate matches and a warning message
#'   match_2_inventories_by_center_coord(inv_a, inv_b, tolerance = 3000)
#'
match_2_inventories_by_center_coord <- function(inv_a, inv_b, tolerance = 0) {

  stopifnot(is_fe_inventory(inv_a))
  stopifnot(is_fe_inventory(inv_b))

  cnt_a <- pull_centers(inv_a)
  cnt_b <- pull_centers(inv_b)

  # Inner Join (i.e. left = FALSE)
  matches <-  sf::st_join(
    cnt_a, cnt_b,
    join = sf::st_is_within_distance,
    dist = tolerance,
    left = FALSE,
    suffix = c("_inv_a", "_inv_b")
  )

  # Left Join from a to b, filter NA's in b => plots that live in a only
  in_a_only <- sf::st_join(
    cnt_a, cnt_b,
    join = sf::st_is_within_distance,
    dist = tolerance,
    left = TRUE,
    suffix = c("_inv_a", "_inv_b")
  ) |>
    dplyr::filter(is.na(.data$plot_id_inv_b)) |>
    dplyr::select(-"plot_id_inv_b") |>
    dplyr::rename(plot_id = "plot_id_inv_a")

  # Left Join from b to a, filter NA's in a => plots that live in b only
  in_b_only <- sf::st_join(
    cnt_b, cnt_a,
    join = sf::st_is_within_distance,
    dist = tolerance,
    left = TRUE,
    suffix = c("_inv_b", "_inv_a")
  ) |>
    dplyr::filter(is.na(.data$plot_id_inv_a)) |>
    dplyr::select(-"plot_id_inv_a") |>
    dplyr::rename(plot_id = "plot_id_inv_b")

  # Are there duplicated points in matches (indicates a problem)
  multiple_matches <- duplicated(matches$geometry)

  # Issue a warning if there are duplicates
  if (any(multiple_matches))
    warning(fe_msg("multiple_matches_matches"))

  # Combine the three result objects into a list and return
  list(
    matches   = matches,
    in_a_only = in_a_only,
    in_b_only = in_b_only,
    multiple_matches = multiple_matches
  )
}




#' Match Two fe_inventory Objects by the IDs of the Inventory Plots
#'
#' From two \code{\link{fe_inventory}} objects identify the matching and non-
#' matching inventory plots based on their center coordinates.
#'
#' @param inv_a An object of class \code{\link{fe_inventory}}
#'
#' @param inv_b An object of class \code{\link{fe_inventory}}
#'
#' @param style Character, two options "generic" and "baysf". The former
#'   requires that the column \code{plot_id} in both inventories, \code{inv_a},
#'   and \code{inv_b} is the same for the same inventory unit. The latter
#'   assumes a BaySF (Bavarian State Forest) style plot_id which is an
#'   underscore ("_") separated string, where only the third substring
#'   (actually the original BaySF field 'koord") has to match for the the same
#'   inventory unit in subsequent inventories.
#'
#' @returns A list that comprises three data frames (tibbles): The first one,
#'   \code{matches}, has two columns, \code{plot_id_inv_a}, and
#'   \code{plot_id_inv_b} which indicate the ids of the matching \code{plot_id}s
#'   in both inventories. The two columns are required because there may be
#'   situations (BaySF style) where the ids of matching points are not entirely
#'   the same. The second data frame, \code{in_a_only}, contains the ids of
#'   those plots that exist in \code{inv_a}, but not in \code{inv_b}, while the
#'   third one, \code{in_b_only}, contains the ids of the plots that are present
#'   in \code{inv_b}, but not in \code{inv_a}.
#'
#' @family increment
#' @family repeated inventory
#' @family inventory matches
#'
#' @export
#'
#' @examples
#'   # Construct a dummy example with partly non-matching plots from one
#'   # fe_inventory object
#'   inv_a <- data_ex3_sample_fe_inventory[3:10, ]
#'   inv_b <- data_ex3_sample_fe_inventory[1:7,  ]
#'
#'   # These are BaySF style inventory data, which means that even matching plot
#'   # ids from subsequent inventories are equal only with regard to their last
#'   # group of digits. However, as this example was constructed from only one
#'   # inventory, both style options, "baysf" and "generic" are equally ok.
#'   match_2_inventories_by_plot_id(inv_a, inv_b, style = "baysf")
#'   match_2_inventories_by_plot_id(inv_a, inv_b, style = "generic")
#'
#'   # Now a real example with two subsequent inventories
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   # These are BaySF style inventory data, from real subsequent inventories.
#'   # Therefore, we must choose that style for matching
#'   match_2_inventories_by_plot_id(inv_a, inv_b, style = "baysf")
#'
#'
#'   # The generic inventory style requires that corresponding plots have the
#'   # same plot_id in both inventories. We can achieve that with a slight
#'   # manipulations of the BaySF style data used above (i.e. reducing the
#'   # original plot_ids to the last group of digits):
#'   inv_a <- inv_a |>
#'     dplyr::mutate(
#'       plot_id = purrr::map_chr(
#'         plot_id, .f = function(.x) strsplit(.x, "_")[[1]][3]
#'       )
#'     )
#'
#'   inv_b <- inv_b |>
#'     dplyr::mutate(
#'       plot_id = purrr::map_chr(
#'         plot_id, .f = function(.x) strsplit(.x, "_")[[1]][3]
#'       )
#'     )
#'
#'   # Now we achieve the same result as above with generic-style matching
#'   match_2_inventories_by_plot_id(inv_a, inv_b, style = "generic")
#'
#'
match_2_inventories_by_plot_id <- function(
    inv_a, inv_b, style = c("generic", "baysf")
) {

  stopifnot(is_fe_inventory(inv_a))
  stopifnot(is_fe_inventory(inv_b))

  inv_a <- inv_a |> dplyr::select(plot_id)
  inv_b <- inv_b |> dplyr::select(plot_id)

  style = match.arg(style)

  if (style == "baysf") {
    join_var <- "koord"

    inv_a <- inv_a |> dplyr::mutate(
      koord = purrr::map_chr(
        .data$plot_id,
        .f = function(.x) strsplit(.x, "_")[[1]][3]
      )
    )
    inv_b <- inv_b |> dplyr::mutate(
      koord = purrr::map_chr(
        .data$plot_id,
        .f = function(.x) strsplit(.x, "_")[[1]][3]
      )
    )
  } else {
    join_var <- "plot_id"

    inv_a <- inv_a |> dplyr::mutate(
      plot_id_inv_a = .data$plot_id
    )
    inv_b <- inv_b |> dplyr::mutate(
      plot_id_inv_b = .data$plot_id
    )
  }

  # inner_join for matches
  matches   <- dplyr::inner_join(
    inv_a, inv_b, by = join_var, suffix = c("_inv_a", "_inv_b")
  ) |>
    dplyr::select(-dplyr::all_of(join_var))
  # Elements in a without a match in b
  in_a_only <- dplyr::anti_join(inv_a, inv_b, by = join_var) |>
    dplyr::select(-dplyr::all_of(join_var))
  # Elements in b without a match in a
  in_b_only <- dplyr::anti_join(inv_b, inv_a, by = join_var) |>
    dplyr::select(-dplyr::all_of(join_var))

  # Return a list with the three join results
  list(matches = matches, in_a_only = in_a_only, in_b_only = in_b_only)
}



#' Check Circle Definition Compatibility of Matched Inventory Plots
#'
#' Internal helper called by \code{\link{match_2_inventories}}. For each matched
#' plot pair, the circle definitions (columns \code{dbh_lower} and \code{c_area})
#' are compared. Pairs with incompatible circle definitions are removed from
#' \code{matches} and stored in a new list element
#' \code{circle_def_mismatch}. The comparison uses tolerances:
#' \code{dbh_lower} is rounded to 1 decimal place (0.1 cm = 1 mm) and
#' \code{c_area} is rounded to 5 decimal places (0.00001 ha = 0.1 m²).
#' This accounts for potential rounding artifacts in the data while still
#' detecting genuine differences in circle setup. Plots with empty circle
#' definitions (notrees plots) are always considered compatible, because
#' ingrowth and mortality are handled separately in the increment
#' calculation.
#'
#' @param match_rslt List as returned by
#'   \code{\link{match_2_inventories_by_plot_id}} or
#'   \code{\link{match_2_inventories_by_center_coord}}.
#'
#' @param inv_a,inv_b \code{\link{fe_inventory}} objects.
#'
#' @returns The input list \code{match_rslt} with an additional element
#'   \code{circle_def_mismatch} containing the incompatible plot pairs. These
#'   pairs are removed from \code{matches}.
#'
#' @keywords internal
#'
check_circle_def_compatibility <- function(match_rslt, inv_a, inv_b) {

  matches <- match_rslt$matches

  # Nothing to check if there are no matches or if the inventories do not
  # contain concentric circle plots with circle definitions
  has_circ_def <- tryCatch(
    !is.null(inv_a$plot[[1]]$circle_definition) &&
      !is.null(inv_b$plot[[1]]$circle_definition),
    error = function(e) FALSE
  )

  if (nrow(matches) == 0 || !has_circ_def) {
    match_rslt$circle_def_mismatch <- matches[0, ]
    return(match_rslt)
  }

  # Look up the position of each matched plot in the inventory objects
  idx_a <- match(matches$plot_id_inv_a, inv_a$plot_id)
  idx_b <- match(matches$plot_id_inv_b, inv_b$plot_id)

  # Compare circle definitions (dbh_lower and c_area, sorted by dbh_lower).
  # Tolerances: dbh_lower rounded to 1 decimal (0.1 cm = 1 mm),
  # c_area rounded to 5 decimals (0.00001 ha = 0.1 m²).
  compatible <- purrr::map2_lgl(
    idx_a, idx_b,
    .progress = "Kreisdefinition pruefen",
    function(ia, ib) {
      cd_a <- inv_a$plot[[ia]]$circle_definition
      cd_b <- inv_b$plot[[ib]]$circle_definition
      # If either circle definition is empty (notrees plot), there is no
      # conflict for increment calculation (ingrowth/mortality handled
      # separately)
      if (nrow(cd_a) == 0 || nrow(cd_b) == 0) return(TRUE)
      # Same number of circles, same thresholds (1 mm), same areas (0.1 m²)
      nrow(cd_a) == nrow(cd_b) &&
        identical(sort(round(cd_a$dbh_lower, 1)), sort(round(cd_b$dbh_lower, 1))) &&
        identical(sort(round(cd_a$c_area, 5)), sort(round(cd_b$c_area, 5)))
    }
  )

  if (any(!compatible)) {
    n_incompat <- sum(!compatible)
    message(fe_msg("incompat_circle_defs", n_incompat))
  }

  match_rslt$circle_def_mismatch <- matches[!compatible, ]
  match_rslt$matches <- matches[compatible, ]

  match_rslt
}



#' Harmonize First Inventory for Repeated Survey Increment Calculation
#'
#' For matched plot pairs from two subsequent inventories, this function ensures
#' that the slope values in the first inventory's circle definitions match
#' those of the second inventory. This is important because differing slope
#' assessments lead to different \code{n_rep_ha} values for the same tree in
#' the same concentric circle, which can distort increment calculations and
#' cause false circle-transition detections.
#'
#' The function modifies the \code{circle_definition$slope} of matched plots in
#' \code{inv_1st} to the value found in the corresponding plot of
#' \code{inv_2nd}. Missing (\code{NA}) slopes are treated as 0 (flat terrain).
#' Plots where neither circle definition contains a \code{slope} column are
#' skipped, as are plots with empty circle definitions (notrees plots).
#' After the slope correction, \code{n_rep_ha} is recalculated for affected
#' trees using \code{\link[ForestElementsR]{n_rep_ha}}, and the modified plot
#' objects are revalidated with
#' \code{\link[ForestElementsR]{validate_fe_ccircle_spatial}}.
#'
#' After harmonization, the user should re-derive the tree data from the
#' returned inventory object (e.g. via \code{pull_trees |>
#' height_complete_inventory |> trees_add_essentials}).
#'
#' @param inv_1st An object of class \code{\link{fe_inventory}} representing
#'   the earlier inventory.
#'
#' @param inv_2nd An object of class \code{\link{fe_inventory}} representing
#'   the later inventory.
#'
#' @param plot_matches Output of \code{\link{match_2_inventories}}.
#'
#' @returns A modified copy of \code{inv_1st} where the slope values in
#'   matched plots have been set to the corresponding values from
#'   \code{inv_2nd} and \code{n_rep_ha} has been recalculated accordingly.
#'   Unmatched plots are returned unchanged.
#'
#' @family increment
#' @family repeated inventory
#' @family inventory matches
#'
#' @export
#'
#' @examples
#'   inv_a <- data_ex3_previous_sample_fe_inventory
#'   inv_b <- data_ex3_sample_fe_inventory
#'
#'   plot_matches <- match_2_inventories(
#'     inv_a, inv_b, match_type = "plot_id", "baysf"
#'   )
#'
#'   inv_a_harmonized <- harmonize_inv_for_repsurv(inv_a, inv_b, plot_matches)
#'
harmonize_inv_for_repsurv <- function(inv_1st, inv_2nd, plot_matches) {

  stopifnot(is_fe_inventory(inv_1st))
  stopifnot(is_fe_inventory(inv_2nd))

  matches <- plot_matches$matches
  if (nrow(matches) == 0) return(inv_1st)

  # Look up the position of each matched plot in the inventory objects
  idx_1st <- match(matches$plot_id_inv_a, inv_1st$plot_id)
  idx_2nd <- match(matches$plot_id_inv_b, inv_2nd$plot_id)

  n_corrected <- 0L

  for (i in seq_along(idx_1st)) {
    i1 <- idx_1st[i]
    i2 <- idx_2nd[i]

    plot_1st <- inv_1st$plot[[i1]]
    plot_2nd <- inv_2nd$plot[[i2]]

    # Skip if either circle definition is empty (notrees plot) -- nothing to
    # harmonize
    if (nrow(plot_1st$circle_definition) == 0 ||
        nrow(plot_2nd$circle_definition) == 0) next

    # Skip if neither circle definition has a slope column -- nothing to
    # harmonize, and no n_rep_ha correction needed (slope is optional)
    has_slope_1st <- "slope" %in% names(plot_1st$circle_definition)
    has_slope_2nd <- "slope" %in% names(plot_2nd$circle_definition)
    if (!has_slope_1st && !has_slope_2nd) next

    # Get slopes; missing column or NA treated as 0 (flat terrain)
    slope_1st <- if (has_slope_1st) unique(plot_1st$circle_definition$slope)
                 else NA_real_
    slope_2nd <- if (has_slope_2nd) unique(plot_2nd$circle_definition$slope)
                 else NA_real_
    if (length(slope_1st) != 1) slope_1st <- NA_real_
    if (length(slope_2nd) != 1) slope_2nd <- NA_real_
    slope_1st <- ifelse(is.na(slope_1st), 0, slope_1st)
    slope_2nd <- ifelse(is.na(slope_2nd), 0, slope_2nd)

    if (slope_1st == slope_2nd) next

    # Correct slope in 1st inventory's circle_definition
    plot_1st$circle_definition$slope <- slope_2nd

    # Recalculate n_rep_ha using ForestElementsR's method
    # (only for plots that actually have trees)
    if (ForestElementsR::is_fe_ccircle_spatial(plot_1st) &&
        nrow(plot_1st$trees) > 0) {
      plot_1st$trees$n_rep_ha <- ForestElementsR::n_rep_ha(plot_1st)
    }

    # Revalidate the modified plot object
    if (ForestElementsR::is_fe_ccircle_spatial(plot_1st)) {
      plot_1st <- ForestElementsR::validate_fe_ccircle_spatial(
        plot_1st, method = "flexible"
      )
    }

    inv_1st$plot[[i1]] <- plot_1st
    n_corrected <- n_corrected + 1L
  }

  if (n_corrected > 0) {
    message(fe_msg("slope_harmonized", n_corrected))
  }

  inv_1st
}







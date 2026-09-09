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

#' Constructor for the **fe_inventory** Class
#'
#' Should be used by expert users only who know exactly what they are doing.
#' Other users, please take the function \code{\link{fe_inventory}} for creating
#' an object of that class.
#'
#' @param x an appropriate \code{tibble} object
#'
#' @param ... additional arguments required for enabling subclasses of
#'   \code{fe_inventory}
#'
#' @param class a character string required for anabling subclasses of
#'   \code{fe_inventory}
#'
#' @return An object of class \code{fe_stand}
#'
#' @keywords internal
#'
new_fe_inventory <- function(x = tibble::tibble(), ..., class = character()) {
  stopifnot(tibble::is_tibble(x))

  parent_classes <- class(tibble::new_tibble(x))

  structure(
    x,
    ...,
    class = c(class, "fe_inventory", parent_classes)
  )
}



#' Check if an Object is an **fe_inventory**
#'
#' @param x an object
#'
#' @return \code{TRUE} if the object inherits from the \code{fe_stand} class
#'
#' @export
#'
#' @examples
#' is_fe_inventory(data_ex4_sample_fe_inventory)
#'
is_fe_inventory <- function(x) {
  inherits(x, "fe_inventory")
}



#' Validate an **fe_inventory** Object
#'
#' Regular users will not require this function. Expert users will want to use
#' it in combination with the constructor \code{\link{new_fe_inventory}}.
#' Regular users, please construct \code{fe_inventory} objects with
#' \code{\link{fe_inventory}}.
#'
#' @param x an object that is expected to be a correct \code{fe_inventory}
#'   object
#'
#' @return Returns \code{x}, but this function is mainly called for its side
#'   effect which is pointing out any violations of the \code{fe_inventory}
#'   object specifications. In case of such violations, the function will
#'   terminate with an error.
#'
#' @export
#'
#' @examples
#' validate_fe_inventory(data_ex4_sample_fe_inventory)
#'
validate_fe_inventory <- function(x) {
  # Check for class attribute
  stopifnot(is_fe_inventory(x))

  # x must essentially be a tibble
  stopifnot(tibble::is_tibble(x))

  # Check for presence of the required columns
  required_names <- c("plot_id", "plot", "area_rep_ha")
  check_rslt <- ForestElementsR::has_required_names(x, required_names)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "column(s)", check_rslt$err_message, "missing in fe_inventory object"
      ),
      call. = FALSE
    )
  }

  # Check for correct types of the columns
  # - normal columns
  required_types_or_classes <- tibble::tribble(
    ~name, ~required,
    "plot_id", "character",
    "area_rep_ha", "numeric"
  )
  check_rslt <- ForestElementsR::has_required_types_or_classes(
    x, required_types_or_classes
  )

  if (!check_rslt$rqmt_ok) {
    stop(
      paste("columns(s)", check_rslt$err_message),
      call. = FALSE
    )
  }

  # - fe_stand object column
  check_rslt <- x$plot |>
    purrr::map_lgl(.f = ForestElementsR::is_fe_stand)

  if (!all(check_rslt)) {
    stop(
      paste(
        "plot(s)",
        paste(x[!check_rslt, ]$plot_id, collapse = ","),
        "is/are not an fe_stand object"
      )
    )
  }

  # plot_id and rep_area no missing values

  required_complete <- c(
    "plot_id", "area_rep_ha"
  )

  check_rslt <- ForestElementsR::has_no_missing_values(x, required_complete)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "columns :",
        check_rslt$err_message
      ),
      call. = FALSE
    )
  }

  # plot_id distinct

  required_unique <- list(
    c("plot_id")
  )

  check_rslt <- ForestElementsR::is_distinct(x, required_unique)

  if (!check_rslt$rqmt_ok) {
    stop(
      paste(
        "plot_id :",
        check_rslt$err_message
      ),
      call. = FALSE
    )
  }

  # plot_id must not contain scientific notation. This arises when a numeric id
  # component is coerced to character by paste()/as.character() for a "round"
  # value (e.g. 324000000 -> "3.24e+08"), or when an upstream export such as
  # Excel already wrote ids like "3.24E+08". Both make ids ambiguous and are
  # treated as errors. The importers force identifier columns to character to
  # prevent the first case (see the BaySF reader's col_types and .id_to_chr()).
  sci_id <- grepl("[0-9](\\.[0-9]+)?[eE][+-][0-9]{2}", x$plot_id)
  if (any(sci_id)) {
    stop(
      paste0(
        "plot_id contains scientific notation, which indicates a numeric ",
        "identifier was mis-formatted or an upstream export corrupted it: ",
        paste(utils::head(x$plot_id[sci_id], 5L), collapse = ", "),
        if (sum(sci_id) > 5L) {
          paste0(" (and ", sum(sci_id) - 5L, " more)")
        } else {
          ""
        }
      ),
      call. = FALSE
    )
  }

  # Only one year in trees (fe_inventory must cover only one survey)
  check_rslt <- x |>
    dplyr::mutate(
      rqmt_ok = purrr::map_lgl(plot,
        .f = function(y) {
          #fe_stand does not have repeated inventories
          if(ForestElementsR::is_fe_stand(y) == FALSE) {
            all.equal(
              max(y$time_yr),
              min(y$time_yr)
          )} else {
            TRUE
          }
        }
      )
    )

  if (!all(check_rslt$rqmt_ok)) {
    stop(fe_msg("inv_year_not_uniform"), call. = FALSE)
  }

  # Mixes of different species coding systems are not allowed. The species
  # coding system is a property of the species_id vector of every fe_stand (or
  # child) object - independent of the plot geometry - so it must be uniform
  # across all plots and across both tree tables (trees and small_trees). This
  # is checked for every fe_stand child, not just fe_ccircle_spatial: plain
  # fe_stand / fe_stand_spatial plots (e.g. from stand-wise imports) carry
  # regular trees that must be checked, and notrees plots carry their only
  # species information in small_trees.
  coding_of <- function(tbl) {
    # Coding-system class of a tree table's species_id, or NULL when the table
    # holds no coded records. Only genuine fe_species vectors count (their class
    # starts with "fe_species_"; they carry no common "fe_species" parent). A
    # logical NA placeholder - which arises when a small_trees slot has rows but
    # no species assignment (see CLAUDE.md: these placeholders should not be
    # created in the first place) - is ignored so it cannot masquerade as a
    # separate coding system and trip the mix check on otherwise valid data.
    if (is.null(tbl) || nrow(tbl) == 0L) {
      return(NULL)
    }
    sp <- tbl$species_id
    if (is.null(sp) || !startsWith(class(sp)[1], "fe_species")) {
      return(NULL)
    }
    class(sp)[1]
  }

  codings <- x$plot |>
    purrr::map(.f = function(y) c(coding_of(y$trees), coding_of(y$small_trees))) |>
    unlist() |>
    unique()

  if (length(codings) > 1) {
    stop(
      paste(
        "species coding systems must not be mixed:",
        paste(codings, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Return the object (will only get there if every hurdle is passed)
  x
}




#' Standard Construction of an **fe_inventory** Object
#'
#'
#' @param inventory_list A tibble with the inventory data containing the
#'   necessary three columns with plot_id, the plot objects, and the
#'   inv_rep_area
#'
#' @param object_type Character to define what type of objects will be save
#'   as inventory points: \code{fe_stand} or children, \code{fe_stand_spatial}
#'   or \code{fe_ccircle_spatial}
#'
#'
#' @return If the information provided allows to construct a valid
#'   \code{fe_inventory} object, this object will be returned. The function will
#'   terminate with an error otherwise
#'
#' @export
#'
#' @examples
#'   # fe_inventory() is the constructor at the end of the import pipeline. It
#'   # is normally reached through the public entry point read_and_convert_data(),
#'   # which builds the intermediate inventory list and calls fe_inventory()
#'   # internally.
#'   in_path <- system.file("extdata", "data_ex3_sample_pre", package = "FeNEU")
#'   read_and_convert_data(in_path, inventory_type = "sample_concentric",
#'                         coord_sys = "gk4")
#'
fe_inventory <- function(
    inventory_list,
    object_type = c(
      "fe_stand", "fe_stand_spatial", "fe_ccircle_spatial")
    ) {

  object_type <- match.arg(object_type)

  # convert_raw_data() read the raw data and create the tibble
  # to become the fe_inventory object, including creation and validation of the
  # single inventory plots as a column of the tibble
  fe_inventory_candidate <- inventory_list |>
                            convert_raw_data(object_type = object_type)

  # Make it formally an fe_inventory object
  fe_inventory_candidate <- new_fe_inventory(fe_inventory_candidate)

  # Validate the object and return it (if validation is successful)
  validate_fe_inventory(fe_inventory_candidate)
}

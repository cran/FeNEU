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


#' Pull All Trees From an fe_inventory Object Into a Tibble
#'
#' While it has many advantages to keep inventory data in an object of class
#' \code{fe_inventory}, having one large data frame comprising all trees
#' is more convenient for many standard evaluation purposes, This function
#' pulls such a data frame (tibble) from an \code{fe_inventory} object.
#'
#' @param forest_inventory Object of class \code{\link{fe_inventory}}
#'
#' @param .progress Logical, if TRUE (default) a progress bar will be shown
#'   during execution. Check the documentation of \link[purrr]{map} for more
#'   options.
#'
#' @param small_trees `r lifecycle::badge("experimental")` logical, default is
#'   FALSE. Only users who absolutely know what they are doing, should use the
#'   setting TRUE. If TRUE also small trees (i.e. trees with heights < 1.3 m)
#'   will be pulled. This is still highly experimental, recommended for
#'   insiders only.
#'
#' @return A tibble with one row per tree, holding the trees of all inventory
#'   plots of \code{forest_inventory} in a single flat table. It is not an
#'   object of a FeNEU class; the plot each tree belongs to is carried in the
#'   column \code{plot_id} instead. The leading columns are \code{plot_id},
#'   \code{area_rep_ha} (the area in ha the plot represents), \code{layer_key},
#'   and \code{species_id} (an \code{fe_species} vector of whatever coding the
#'   inventory uses); they are followed by the tree attributes of the plots'
#'   \code{trees} slots, among them \code{tree_id}, \code{time_yr},
#'   \code{age_yr}, \code{dbh_cm}, \code{height_m}, \code{removal},
#'   \code{ingrowth}, and \code{h_m_tree} (whether the height was measured or
#'   estimated).
#'
#'   Two columns are added by this function and are not tree attributes as
#'   such. \code{n_rep_ha} is the number of trees per hectare the tree
#'   represents; it comes from the plot but is multiplied here by the tree's
#'   \code{tree_count}, so that a record standing for more than one tree is
#'   correctly weighted (\code{tree_count} itself is dropped afterwards).
#'   \code{dbh_trshld} is the lower DBH limit of the concentric circle the tree
#'   was sampled in; it is \code{NA} for plots that are not
#'   \code{fe_ccircle_spatial}, i.e. for stand-wise inventories.
#'
#'   With \code{small_trees = TRUE} the small trees of all plots are appended
#'   as further rows, with \code{crown_base_height_m}, \code{crown_radius_m},
#'   and \code{dbh_trshld} set to \code{NA}.
#'
#' @export
#'
#' @examples
#'   data_ex3_sample_fe_inventory |> pull_trees()
#'
#'
pull_trees <- function(forest_inventory, small_trees = FALSE,
                                    .progress = TRUE) {

  # As long as we do only pull from fe_inventory objects, we don't need a
  # generic pull_trees. Checking for the correct class does the job so far.
  stopifnot(is_fe_inventory(forest_inventory))

  # Subfunction for extracting the trees from a plot (fe_stand object)
  ## if the plot is an fe_ccircle_spatial object, find the threshold value
  ## for each tree to be sampled and give it a column. Otherwise make the
  ## column NA.
  extract_trees_plot_id <- function(plot) {
    if(!is.null(plot$trees)){
      trees <- plot$trees |>
        dplyr::mutate(plot_id = plot$stand_id)

      if (ForestElementsR::is_fe_ccircle_spatial(plot)) {
        thresholds <- sort(plot$circle_definition$dbh_lower)
        trees <- trees |>
          dplyr::mutate(
            dbh_trshld = thresholds[findInterval(.data$dbh_cm, thresholds)]
          )
      } else {
        trees <- trees |> dplyr::mutate(dbh_trshld = NA_real_)
      }
    } else {
      trees <- NULL
    }
    return(trees)
  }


  # Apply the sub-function to each plot, combine into a tibble,
  # merge in the representation area of each plot, arrange the columns nicely
  # and hand it back
  forest_inventory_trees <- forest_inventory |>
    purrr::pluck("plot") |>
    purrr::map(.f = extract_trees_plot_id, .progress = .progress) |>
    dplyr::bind_rows() |>
    dplyr::left_join(
      dplyr::select(forest_inventory, "plot_id", "area_rep_ha")
    ) |>
    # correct rep_ha for trees because some have larger counts than 1
    # remove the tree_count column later
    dplyr::mutate(n_rep_ha = .data$n_rep_ha * .data$tree_count) |>
    dplyr::select(-"tree_count") |>
    dplyr::select(
      "plot_id", "area_rep_ha", "layer_key", "species_id",
      tidyselect::everything()
    )


  if(small_trees == TRUE){

      extract_small_trees_plot_id <- function(plot) {
        if(!is.na(plot$small_trees$dbh_cm[1])){
        plot$small_trees |>
          dplyr::mutate(
            plot_id = plot$stand_id
          )
        }
      }

      forest_inventory_small <- forest_inventory |>
        purrr::pluck("plot") |>
        purrr::map(.f = extract_small_trees_plot_id, .progress = .progress) |>
        dplyr::bind_rows() |>
        dplyr::left_join(
          dplyr::select(forest_inventory, "plot_id", "area_rep_ha")
        ) |>
        dplyr::select(
          "plot_id", "area_rep_ha", "layer_key", "species_id",
          tidyselect::everything()
        ) |>
        # Astor: I put these columns here because they are most
        # probably not measured for small trees
        dplyr::mutate(crown_base_height_m = NA_real_,
                      crown_radius_m = NA_real_,
                      dbh_trshld = NA_real_,
                      time_yr  = unique(forest_inventory_trees$time_yr)) |>
        dplyr::select(-"tree_count")


      forest_inventory_trees <- rbind(forest_inventory_trees,
                                      forest_inventory_small)

  }

  forest_inventory_trees
}



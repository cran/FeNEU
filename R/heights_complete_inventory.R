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


#' Complete Heights for Non-Measured Trees in an Inventory
#'
#' Typically in an inventory, height measurements are done only for a part of
#' the sampled trees. This function provides reasonable estimates for the
#' missing heights.
#'
#' The function first identifies quadratic mean diameter (dq) classes grouped by
#' species group (i.e. \code{\link[ForestElementsR]{fe_species_tum_wwk_short}}),
#' and stand layer level for each plot. These classes are then applied to the
#' whole inventory (i.e. omitting the grouping by plot), to calculate a dq
#' value for each dq class, species group, and stand layer. From the
#' height-measured trees a corresponding hq (quadratic mean height) is
#' calculated. In case there are no height measured trees in a group, so that
#' there is no data-based estimate of hq possible the function
#' \code{\link[ForestElementsR]{h_q_from_d_q}} is used as a fallback. In this
#' case a warning is issued.\cr
#' Both, hq and dq are the entry points for the subsequent individual tree
#' height estimates which are based on the individual trees' dbh. Note that for
#' the height estimation method "Bavaria" also an age must be given for each
#' tree (usually the stand or stand layer age).\cr
#' The estimation methods "NFI", and "Bavaria" use the standard height curve
#' systems \code{\link[ForestElementsR]{h_standard_gnfi3}}, and
#' \code{\link[ForestElementsR]{h_standard_bv}}, respectively.
#'
#' @param pulled_inventory Tibble with tree list for the whole inventory,
#'   typically the output of \code{\link{pull_trees}}
#'
#' @param method Height estimation method for trees with missing heights,
#'   "NFI" height curves are defined as default, alternatively "Bavaria" can be
#'   selected, but in this case an age must be provided for every tree
#'   (see Details)
#'
#' @param d_q_interval Integer indicating the interval breaks for the dq
#'   (quadratic mean diameter) classes to be formed. Default is 5-cm-classes,
#'   where all dq values over 60 cm are all put into the same class.
#'
#' @param diagnostic Logical, if TRUE, the function provides diagnostic output,
#'   which is NOT compatible with the standard output and its usage. Default is
#'   FALSE. Use TRUE only, if you know exactly what you are doing.
#'
#' @return A tibble in the same format as the input \code{pulled_inventory},
#'   but with the additional columns \code{h_est_m}, i.e. height estimates for
#'   every tree, \code{species_group}, \code{d_q_cm}, and \code{h_q_m}.
#'
#' @export
#'
#' @examples
#'  # Pull tree data from an fe_inventory object
#'  pulled_inventory <- data_ex3_sample_fe_inventory |>
#'    pull_trees()
#'
#'  # Use the NFI method. The h_q fallback warning is expected here where a
#'  # species x layer group has no measured height.
#'  suppressWarnings(
#'    height_complete_inventory(pulled_inventory, method = "NFI")
#'  )
#'
#'  # ... and the Bavaria method on the same data
#'  suppressWarnings(
#'    height_complete_inventory(pulled_inventory, method = "Bavaria")
#'  )
#'
height_complete_inventory <- function(pulled_inventory,
                                      d_q_interval = 5,
                                      method = c("NFI", "Bavaria"),
                                      diagnostic = FALSE) {

  # Initial checks
  method <- match.arg(method)

  if(method == "Bavaria" & any(is.na(pulled_inventory$age_yr))) {
    stop(
      fe_msg("age_missing_bavaria"),
      call. = FALSE
    )
  }

  if(diagnostic) {
    warning(
      fe_msg("diagnostic_warning"),
      call. = FALSE
    )
  }

  # Important first step: Introduce the species groups
  work_dat <- pulled_inventory |>
    dplyr::mutate(
      species_group = ForestElementsR::as_fe_species_tum_wwk_short(
        .data$species_id
      )
    )

  # Form the mean diameter (d_q) classes by plot, species group and layer
  # using the function back_table_dclass
  back_dq <- work_dat |>
    dplyr::mutate(
      species_group = ForestElementsR::as_fe_species_tum_wwk_short(
        .data$species_id
      )
    ) |>
    back_table_dclass(d_q_interval) |>
    dplyr::select(-"d_q_cm") # Not required anymore because related to plot level
                           # (see next comment)

  # Add the classes to work_dat
  work_dat <- back_dq |>
    dplyr::inner_join(work_dat)

  # Get the quadratic mean diameters (d_q) and heights (h_q) for each mean
  # diameter class, layer, and species group. Note that the d_q's are calculated
  # here as plot-overarching values for getting the most stable entry points for
  # the standard height curves
  aggr <- work_dat |>
    dplyr::group_by(.data$species_group, .data$layer_key, .data$d_q_class) |>
    dplyr::summarise(
      d_q_cm = ForestElementsR::d_q(
        d     = .data$dbh_cm,
        n_rep = .data$n_rep_ha
      ),
      h_q_m = ForestElementsR::h_q(
        h       = .data$height_m,
        d       = .data$dbh_cm,
        n_rep   = .data$n_rep_ha,
        na_h.rm = TRUE
      )
    ) |>
    dplyr::mutate(
      # Only relevant for diagnostics
      no_heights = ifelse(is.nan(.data$h_q_m), TRUE, FALSE)
    )

  # Check if there are NaN h_q_m (occurs if no measured heights in that group)
  # In this case, issue a warning and do a fallback replacement
  if(any(is.nan(aggr$h_q_m))) {
    warning(
      fe_msg("nan_group_heights"),
      call. = FALSE
    )

    # Perform NaN replacement
    aggr <- aggr |>
      dplyr::mutate(
        h_q_m = ifelse(is.nan(.data$h_q_m),
                       ForestElementsR::h_q_from_d_q(
                         .data$species_group,
                         .data$d_q_cm
                       ),
                       .data$h_q_m)
      )
  }

  # Join the height curve entry point data (aggr) to the single tree data in
  # order to allow height estimates for each tree
  work_dat <- work_dat |>
    dplyr::left_join(aggr, by = c(
      "species_group", "d_q_class",
      "layer_key"
    ))

  # Estimate heights using NFI standard height curve system
  if (method == "NFI") {
    work_dat <- work_dat |>
      dplyr::mutate(h_est_m = ForestElementsR::h_standard_gnfi3(
        .data$species_group,
        .data$dbh_cm,
        .data$d_q_cm,
        .data$h_q_m
      ))
  }

  # Estimate heights using the Bavarian standard height curve system
  if (method == "Bavaria") {
    work_dat <- work_dat |>
      dplyr::mutate(
        h_est_m =
          ForestElementsR::h_standard_bv(
            .data$species_id,
            .data$dbh_cm,
            .data$age_yr,
            .data$d_q_cm,
            .data$h_q_m
          )
      )
  }

  # Prepare final output
  if(diagnostic) {
    # This output is NOT compatible with the standard usage!
    out <- list(
      work_dat = work_dat,
      agg_info = aggr
    )
  }
  else {
    # This is the standard output
    # Remove interim column(s) and hand back the tibble
    out <- work_dat |> dplyr::select(-"d_q_class", -"no_heights")
  }

  out
}




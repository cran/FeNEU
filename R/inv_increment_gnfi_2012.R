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


#' Estimate the Volume Increment With the 3rd German National Forest Inventory
#' Tree Growth Functions by Species and Age Class for a Whole Forest Inventory
#'
#' Uses the single tree growth functions developed for the third German
#' National Forest Inventory \insertCite{bwi3_methods_2017}{FeNEU} as
#' implemented in the package \code{ForestElementsR}, namely the functions
#' \code{\link[ForestElementsR]{d_age_gnfi3}}, and
#' \code{\link[ForestElementsR]{d_age_gnfi3}}
#'
#'
#' @param inv_dat Tree data frame as pulled from an \code{fe_inventory} object
#'   after height completion (\code{\link{height_complete_inventory}}), and
#'   applying \code{\link{trees_add_essentials}}.
#'
#' @param dt Time span to be applied for the increment estimation. Default is
#'   five years.
#'
#' @param plot_area_weight Logical, if TRUE (default), the single plots are
#'   weighted with the area they represent. If FALSE all plots are equally
#'   weighted
#'
#' @return A data frame with ha-wise annual volume increments and virtual areas
#'   per species group and age class, for the \strong{main stand only} and per
#'   \strong{virtual monospecific hectare}
#'
#' @references \insertAllCited{}
#'
# Superseded by inv_increment_gnfi3(), which covers the whole chain and returns
# an object the class tables and reports consume directly. This function does
# the same per-tree estimation but reports one particular aggregation of it:
# main stand only, over virtual pure-stand areas. The name promised more than
# that, which is why it is no longer part of the user-facing API. No @family
# tag: the increment family lists the user-facing functions, and a tag here
# would put a superseded function into their "See also".
#' @keywords internal
#'
# No @examples here: the function is not exported any more, so R CMD check would
# run the example with only the exported objects on the search path and fail on
# the bare call. The code path is covered by the examples of
# inv_increment_gnfi3() and by the test suite.
#'
inv_increment_gnfi_2012 <- function(inv_dat, dt = 5, plot_area_weight = TRUE) {

  # The age requirement covers the whole input, and that is the point rather
  # than a restriction: the gnfi3 increment is estimated per tree for the entire
  # population (step 1), so that any collective -- main stand, all layers, or
  # any other selection -- can be aggregated from the result afterwards. An
  # ageless tree therefore has to be refused wherever it sits, not only in the
  # collective this particular aggregator happens to report. (Only the
  # yield-table strand is inherently bound to the main stand, which is why
  # inv_increment_ytables() may check the narrower collective.)
  #
  # Guarding it here keeps the failure legible. Without the check a missing age
  # yields an NA dbh estimate and surfaces as "No missing species_id or dbh_cm
  # values allowed in arguments to v_gri()" from deep inside ForestElementsR --
  # pointing the user at a dbh problem they do not have. Verified empirically:
  # ages missing outside the main stand break it just the same.
  .assert_age_complete(inv_dat, "inv_increment_gnfi_2012")

  inv_that_plots <- inv_dat |>
  # 1 Add single tree increments
    tree_inc_gnfi_2012(dt) |>
  # 2 Aggregate single tree increments to ha values on plot level
    plot_inc_gnfi_2012()

  # 3 ha-wise increments by species and age class (main stand only)
  #   on plot level.
  #   increments are now per ha virtual monospecific stand
  that_inc_exp <- ha_inc_main_stand_age_class(
    inv_that_plots,
    data_filter = !.data$removal
  )

  # 4 Aggregate over the whole forest estate by species group and age class
  that_inc_ha_agg <- agg_ha_inc_main_stand_age_class(
    that_inc_exp,
    plot_area_weight
  )

  that_inc_ha_agg
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Follows: Four internal functions to be applied in sequence inside
# inv_increment_gnfi_2012() to the pulled trees data frame after height
# completion and added essentials

# They bring us to age class and species group-wise volume increments

# ------------------------------------------------------------------------------


# The internal helpers in this file carry no @family tag on purpose. A family
# tag would list them in the "See Also" of every exported member of that
# family, offering the reader a link to something they cannot call.
#' Single Tree Volume Increment Estimation for Pulled Trees using the Methods of
#' the 3rd German National Forest Inventory
#'
#' Estimates the dbh, height, and volume increment for each tree in a
#' data frame x, that was generated by \code{\link{pull_trees}}, applying
#' \code{\link{height_complete_inventory}} and
#' \code{\link{trees_add_essentials}} to it. For estimating height and dbh
#' increment, the functions \code{\link[ForestElementsR]{d_age_gnfi3}} and
#' \code{\link[ForestElementsR]{h_age_gnfi3}} are used. Volumes for increment
#' are calculated with the function \code{\link[ForestElementsR]{v_gri}}.
#'
#' The estimate can run in either direction in time, encoded in the sign of
#' \code{dt}: a positive value projects the trees that many years into the
#' future, a negative value that many years into the past. The volume
#' increment is averaged on one year (divided by \code{abs(dt)}), so the
#' resulting \code{iv_hub_m3_yr_gnfi3} is positive for a growing tree in
#' either direction. \code{pmax(0, age_yr + dt)} clamps the target age so
#' that backward estimates stay defined for trees younger than \code{|dt|}.
#'
#' The output columns share the suffix \code{_gnfi3} that flags the
#' estimation method. This is consistent with the other gnfi3-derived
#' columns elsewhere in the package (e.g. \code{iv_hub_m3_ha_per_gnfi3} in
#' the fill-gap pipeline) and leaves the direction of estimation as a
#' caller-side concern rather than baking it into the column names.
#'
#' @param x Tree data frame as pulled from an \code{fe_inventory} object after
#'   height completion (\code{\link{height_complete_inventory}}), and applying
#'   \code{\link{trees_add_essentials}}.
#'
#' @param dt Time span for the estimate (years). Positive values project
#'   forward in time, negative values backward. Default 5 — a period that
#'   empirically works well with the gnfi3 functions across age ranges. For
#'   estimating the recent past on plots with only a single observation
#'   (e.g. in \code{\link{inv_inc_big_overview_2nd_only}}) a short backward
#'   window (typically \code{dt = -5}) is preferred over the full
#'   inter-survey distance, because the unknown mortality fraction stays
#'   small. Must be non-zero.
#'
#' @return The input data frame x with five additional columns, all
#'   carrying the \code{_gnfi3} suffix:
#'  \describe{
#'    \item{dbh_cm_gnfi3:}{Estimate for the trees' diameters at the target
#'      age (\code{age_yr + dt}, clamped at 0).}
#'    \item{height_m_gnfi3:}{Estimate for the trees' heights at the target
#'      age.}
#'    \item{v_m3_gnfi3:}{Tree volume at the target age based on the two
#'      preceding columns.}
#'    \item{v_hub_m3_gnfi3:}{The same volume reduced to harvested volume
#'      under bark via \code{\link[ForestElementsR]{v_red_harvest_ubark}}.}
#'    \item{iv_hub_m3_yr_gnfi3:}{Annual tree volume increment, harvested
#'      under bark. For \code{dt > 0}: \code{(v_hub_m3_gnfi3 - v_hub_m3) /
#'      dt}. For \code{dt < 0}: \code{(v_hub_m3 - v_hub_m3_gnfi3) /
#'      abs(dt)}. Both conventions yield a positive value for a growing
#'      tree.}
#'  }
#'
#'
#' @keywords internal
#'
tree_inc_gnfi_2012 <- function(x, dt = 5) {

  stopifnot(is.numeric(dt), length(dt) == 1L, dt != 0)

  abs_dt <- abs(dt)

  suppressWarnings(
    x |> dplyr::mutate(
      dbh_cm_gnfi3       = ForestElementsR::d_age_gnfi3(
                             .data$species_id,
                             pmax(0, .data$age_yr + dt),
                             .data$dbh_cm, .data$age_yr
                           ),
      height_m_gnfi3     = ForestElementsR::h_age_gnfi3(
                             .data$species_id,
                             pmax(0, .data$age_yr + dt),
                             .data$height_m, .data$age_yr
                           ),
      v_m3_gnfi3         = ForestElementsR::v_gri(
                             .data$species_id, .data$dbh_cm_gnfi3,
                             .data$height_m_gnfi3
                           ),
      v_hub_m3_gnfi3     = ForestElementsR::v_red_harvest_ubark(
                             .data$species_id, .data$v_m3_gnfi3
                           ),
      iv_hub_m3_yr_gnfi3 = if (dt > 0) {
        (.data$v_hub_m3_gnfi3 - .data$v_hub_m3) / abs_dt
      } else {
        (.data$v_hub_m3 - .data$v_hub_m3_gnfi3) / abs_dt
      }
    )
  )
}


#' Aggregate 3rd German NFI Tree Increment Estimates on Inventory Plot Level
#'
#' Single tree increments area aggregated to ha values per species group and
#' layer on plot level (but NOT ha values on species group level)
#'
#' @param x A data frame generated with \code{\link{tree_inc_gnfi_2012}}
#'
#' @return A data frame representing a plot level aggregation of the single
#'   tree level input \code{x}
#'
#'
#' @keywords internal
#'
plot_inc_gnfi_2012 <- function(x) {

  x |>
    dplyr::group_by(
      .data$plot_id, .data$area_rep_ha, .data$layer_key, .data$species_group,
      .data$time_yr, .data$removal, .data$ingrowth, .data$age_class
    ) |>
    dplyr::summarise(
      age_yr      = stats::weighted.mean(.data$age_yr, w = .data$n_rep_ha),
      standing_area_m2_ha = sum(.data$standing_area_m2 * .data$n_rep_ha),
      d_q_cm      = ForestElementsR::d_q(.data$dbh_cm, n_rep = .data$n_rep_ha),
      h_q_m       = ForestElementsR::h_q(
                      .data$height_m, .data$dbh_cm, n_rep = .data$n_rep_ha
                    ),
      g_m2_ha     = sum(.data$g_m2 * .data$n_rep_ha),
      v_hub_m3_ha = sum(.data$v_hub_m3 * .data$n_rep_ha),
      iv_hub_m3_ha_yr_gnfi3 = sum(.data$iv_hub_m3_yr_gnfi3 * .data$n_rep_ha)
    )
}


#' Calculate ha-wise Increments per Plot, Species Group, and Age Class
#'
#' The increments added to the output of \code{\link{plot_inc_gnfi_2012}} here
#' relate to virtual species (group) area shares. The calculation is only done
#' for the main stand, because otherwise the species area shares will not make
#' too much sense.
#'
#' @param x A data frame generated with \code{\link{plot_inc_gnfi_2012}}
#'
#' @param data_filter Logical expression relating to the columns of x. Default
#'   is !.data$removal, i.e. the removal stand is not included in the increment
#'   calculation. Independently, there is always an internal filter on
#'   layer_key == 1, which does only take into account main stand trees
#'
#' @return A data frame that is the input \code{x} with additional columns
#'   representing the species groups' area shares, absolute areas and the
#'   related increments per ha
#'
#'
#' @keywords internal
#'
ha_inc_main_stand_age_class <- function(x, data_filter = !.data$removal) {

  x_list <- x |>
    dplyr::filter(.data$layer_key == 1) |>
    dplyr::filter({{data_filter}}) |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::group_split() |>
    purrr::map(
      .f = function(.x) {
        .x <- .x |> dplyr::ungroup() # for safety reasons
        st_area_total <- sum(.x$standing_area_m2_ha)
        # Teilfläche can be max 1 ha
        .x$area_share_ha <- .x$standing_area_m2_ha / st_area_total
        # The rep area of the plot per species_group and age class must be
        # reduced accordingly
        .x$area_rep_ha_share <- .x$area_rep_ha * .x$area_share_ha
        # 'expd' means expanded to one virtual monospecific ha
        .x$iv_hub_m3_expd_ha_yr_gnfi3 <- .x$iv_hub_m3_ha_yr_gnfi3 / .x$area_share_ha
        .x |> dplyr::relocate(
          "area_rep_ha_share", .after = "area_rep_ha"
        )
      }
    )
  do.call(rbind, x_list)
}


#' Calculate ha-wise Increments per Species Group and Age Class on Inventory
#' Level
#'
#' Aggregates the plot level output of \code{\link{ha_inc_main_stand_age_class}}
#' to the level of the whole inventory
#'
#' @param x A data frame with per-plot increment values, typically the output
#'   of \code{\link{ha_inc_main_stand_age_class}}.
#'
#' @param plot_area_weight Logical, if TRUE (default), the single plots are
#'   weighted with the area they represent. If FALSE all plots are equally
#'   weighted
#'
#' @return A data frame which is an aggregation of the input \code{x} to the
#'   inventory level
#'
#'
#' @keywords internal
#'
agg_ha_inc_main_stand_age_class <- function(x, plot_area_weight) {

  if (plot_area_weight) x$weights <- x$area_rep_ha_share
  else                  x$weights <- 1

  x |>
    dplyr::group_by(.data$species_group, .data$age_class) |>
    dplyr::summarise(
      area_ha                  = sum(.data$area_rep_ha_share),
      iv_hub_m3_expd_ha_yr_gnfi3 = stats::weighted.mean(
        .data$iv_hub_m3_expd_ha_yr_gnfi3,
        .data$weights)
    )
}


# ha wise aggregation over species groups
# x: Output of agg_ha_inc_main_stand_age_class()
# agg_ha_inc_main_stand <- function(x) {
#
#   x |>
#     group_by(species_group) |>
#     summarise(
#       iv_hub_m3_expd_ha_yr_gnfi3 = weighted.mean(iv_hub_m3_expd_ha_yr_gnfi3,
#                                                area_ha),
#       area_ha                  = sum(area_ha)
#     ) |>
#     relocate(area_ha, .before = iv_hub_m3_expd_ha_yr_gnfi3)
# }

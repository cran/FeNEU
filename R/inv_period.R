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


#' Survey Year of an Inventory Object
#'
#' Returns the unique survey year (\code{time_yr}) of an inventory object.
#' At present a method is provided for \code{\link{fe_inventory}}, where
#' uniformity of \code{time_yr} across all plots is guaranteed by the
#' \code{fe_inventory} constructor; the method does a defensive check
#' regardless.
#'
#' @param x An inventory object.
#' @param ... Currently unused, reserved for method-specific extensions.
#'
#' @returns A single numeric value: the survey year.
#'
#' @export
#'
#' @examples
#'   get_inv_year(data_ex3_sample_fe_inventory)
#'
get_inv_year <- function(x, ...) UseMethod("get_inv_year")


#' @rdname get_inv_year
#' @export
get_inv_year.fe_inventory <- function(x, ...) {
  yrs <- purrr::map_dbl(x$plot, .f = function(.x) .x$time_yr)
  if (dplyr::n_distinct(yrs) != 1)
    stop(
      "fe_inventory has non-uniform time_yr across plots.",
      call. = FALSE
    )
  yrs[1]
}



#' Survey Years and Period Between Two Inventories
#'
#' Convenience helper that extracts the survey years of two
#' \code{\link{fe_inventory}} objects and returns them together with the
#' resulting inter-survey period, after the same validation that
#' \code{\link{inv_increment_repsurv_ccirc}} applies. Used by functions that
#' combine two inventories so that downstream consumers (e.g.
#' \code{\link{inv_inc_summaric}}) can access the period without having to
#' carry the \code{fe_inventory} objects further.
#'
#' @param inv_1st,inv_2nd Two \code{\link{fe_inventory}} objects in
#'   chronological order.
#'
#' @returns A list with elements \code{time_yr_1st}, \code{time_yr_2nd},
#'   and \code{period_yr} (the difference).
#'
#' @export
#'
#' @examples
#'   inv_period(
#'     data_ex3_previous_sample_fe_inventory,
#'     data_ex3_sample_fe_inventory
#'   )
#'
inv_period <- function(inv_1st, inv_2nd) {
  t1 <- get_inv_year(inv_1st)
  t2 <- get_inv_year(inv_2nd)
  if (t1 == t2)
    stop(fe_msg("inv_year_same"), call. = FALSE)
  if (t2 < t1)
    stop(fe_msg("inv_year_order"), call. = FALSE)
  if ((t2 - t1) < 1)
    stop(fe_msg("inv_period_too_short"), call. = FALSE)
  list(time_yr_1st = t1, time_yr_2nd = t2, period_yr = t2 - t1)
}

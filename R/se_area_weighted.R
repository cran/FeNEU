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


#' Area-Weighted Standard Error and Confidence Interval
#'
#' Computes the standard error and confidence interval for an area-weighted
#' mean and its corresponding total, based on the ratio estimator approach.
#' This is the standard method for forest inventory data where plots have
#' different representation areas (e.g. due to slope correction in concentric
#' circle designs).
#'
#' The area-weighted mean is computed as a ratio estimator:
#' \deqn{\hat{R} = \frac{\sum a_i x_i}{\sum a_i}}{R = sum(a*x) / sum(a)}
#'
#' Its variance is estimated as:
#' \deqn{\mathrm{Var}(\hat{R}) = \frac{n}{(n-1)\,(\sum a_i)^2}
#' \sum a_i^2 (x_i - \hat{R})^2}{Var(R) = n / ((n-1) * (sum(a))^2) *
#' sum(a^2 * (x - R)^2)}
#'
#' The total is \eqn{\hat{Y} = \hat{R} \cdot \sum a_i}{Y = R * sum(a)},
#' with \eqn{\mathrm{SE}(\hat{Y}) = \mathrm{SE}(\hat{R}) \cdot \sum a_i}{
#' SE(Y) = SE(R) * sum(a)}.
#'
#' Confidence intervals use the t-distribution with \code{n - 1} degrees of
#' freedom. No finite population correction (FPC) is applied. For typical
#' forest inventories on systematic grids, the sampling fraction \eqn{n/N} is
#' very small and the FPC is negligible. Omitting it is conservative, i.e. the
#' standard error is slightly overestimated.
#'
#' @param plot_values Numeric vector of per-hectare values, one per plot
#'   (e.g. volume in m3/ha, increment in m3/ha/yr).
#' @param area_weights Numeric vector of representation areas in hectares,
#'   same length as \code{plot_values}. Must be strictly positive.
#' @param conf_level Confidence level for the interval, default 0.95.
#'
#' @returns A tibble (one row) with:
#'   \describe{
#'     \item{n_plots}{Number of plots used}
#'     \item{mean_ha}{Area-weighted mean (per hectare)}
#'     \item{se_mean}{Standard error of the mean}
#'     \item{margin_mean}{Margin of error for the mean (t * SE)}
#'     \item{ci_lo_mean}{Lower confidence bound of the mean}
#'     \item{ci_up_mean}{Upper confidence bound of the mean}
#'     \item{total}{Area-weighted total (mean_ha * total area)}
#'     \item{se_total}{Standard error of the total}
#'     \item{margin_total}{Margin of error for the total (t * SE)}
#'     \item{ci_lo_total}{Lower confidence bound of the total}
#'     \item{ci_up_total}{Upper confidence bound of the total}
#'     \item{ci_pct}{Margin of error as percentage of the mean}
#'   }
#'
#' @export
#'
#' @examples
#'   # Simple example with equal areas
#'   se_area_weighted(c(10, 12, 8, 11), area_weights = c(1, 1, 1, 1))
#'
#'   # Unequal representation areas (e.g. from slope correction)
#'   se_area_weighted(
#'     c(10, 12, 8, 11),
#'     area_weights = c(0.95, 1.00, 0.88, 1.02)
#'   )
#'
se_area_weighted <- function(plot_values,
                             area_weights,
                             conf_level = 0.95) {

  # Input validation
  stopifnot(
    is.numeric(plot_values),
    is.numeric(area_weights),
    length(plot_values) == length(area_weights),
    all(area_weights > 0),
    conf_level > 0, conf_level < 1
  )

  # Remove pairs where plot_values is NA
  keep <- !is.na(plot_values)
  plot_values <- plot_values[keep]
  area_weights <- area_weights[keep]
  n <- length(plot_values)

  # Template for NA results (n < 2)
  na_row <- tibble::tibble(
    n_plots = 0L, mean_ha = NA_real_,
    se_mean = NA_real_, margin_mean = NA_real_,
    ci_lo_mean = NA_real_, ci_up_mean = NA_real_,
    total = NA_real_, se_total = NA_real_, margin_total = NA_real_,
    ci_lo_total = NA_real_, ci_up_total = NA_real_,
    ci_pct = NA_real_
  )

  if (n == 0) return(na_row)

  # Area-weighted mean (ratio estimator)
  sum_a <- sum(area_weights)
  r_hat <- sum(area_weights * plot_values) / sum_a
  total_val <- r_hat * sum_a

  if (n == 1) {
    na_row$n_plots <- 1L
    na_row$mean_ha <- r_hat
    na_row$total   <- total_val
    return(na_row)
  }

  # Ratio estimator variance: Var(R) = n/((n-1)*A^2) * sum(a_i^2*(x_i-R)^2)
  var_r <- n / ((n - 1) * sum_a^2) *
    sum(area_weights^2 * (plot_values - r_hat)^2)
  se_r <- sqrt(var_r)
  se_t <- se_r * sum_a

  # Confidence interval via t-distribution
  t_crit <- stats::qt((1 + conf_level) / 2, df = n - 1)
  margin_m <- t_crit * se_r
  margin_t <- t_crit * se_t

  # Margin of error as percentage of the mean
  ci_pct <- if (abs(r_hat) > .Machine$double.eps) {
    margin_m / abs(r_hat) * 100
  } else {
    NA_real_
  }

  tibble::tibble(
    n_plots      = n,
    mean_ha      = r_hat,
    se_mean      = se_r,
    margin_mean  = margin_m,
    ci_lo_mean   = r_hat - margin_m,
    ci_up_mean   = r_hat + margin_m,
    total        = total_val,
    se_total     = se_t,
    margin_total = margin_t,
    ci_lo_total  = total_val - margin_t,
    ci_up_total  = total_val + margin_t,
    ci_pct       = ci_pct
  )
}



#' Group-Wise Area-Weighted Aggregation
#'
#' Convenience wrapper around \code{\link{se_area_weighted}} that takes a
#' (possibly grouped) data frame with one row per plot per group, a value
#' column, and an area column, and returns one row per group with all the
#' columns of \code{\link{se_area_weighted}}.
#'
#' Used as the shared primitive behind several aggregation pipelines in this
#' package (back tables, big_overview, increment summaries). The data frame
#' is expected to be pre-aggregated to one row per plot per group — the
#' caller decides what \dQuote{per plot} means (e.g. sum over trees in a
#' species group, or a single per-plot quantity).
#'
#' @param data A data frame. If grouped via \code{\link[dplyr]{group_by}},
#'   the aggregation runs per group; otherwise a single row is returned.
#' @param value_col Character. Name of the per-plot per-hectare value
#'   column (e.g. \code{"iv_m3_ha_yr_plot"}).
#' @param area_col Character. Name of the plot's representation area
#'   column, in hectares.
#' @param conf_level Confidence level for the CI, default 0.95.
#'
#' @returns A tibble with one row per group, the group keys preserved, and
#'   all output columns of \code{\link{se_area_weighted}} attached.
#'
#' @export
#'
#' @examples
#'   df <- tibble::tibble(
#'     group = rep(c("a", "b"), each = 4),
#'     value = c(10, 12,  8, 11,  20, 22, 18, 21),
#'     area  = c( 1,  1,  1,  1,   1,  1,  1,  1)
#'   )
#'   df |>
#'     dplyr::group_by(group) |>
#'     se_area_weighted_grouped("value", "area")
#'
se_area_weighted_grouped <- function(data,
                                     value_col,
                                     area_col,
                                     conf_level = 0.95) {
  stopifnot(
    is.data.frame(data),
    is.character(value_col), length(value_col) == 1L,
    is.character(area_col),  length(area_col)  == 1L,
    value_col %in% names(data),
    area_col  %in% names(data)
  )

  data |>
    dplyr::group_modify(~ se_area_weighted(
      plot_values  = .x[[value_col]],
      area_weights = .x[[area_col]],
      conf_level   = conf_level
    )) |>
    dplyr::ungroup()
}

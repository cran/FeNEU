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

#' Prepared Tree Lists of the ex3 Inventory Pair
#'
#' The tree lists of the two ex3 surveys, ready for analysis: heights complete,
#' volumes, standing areas and species groups added. They are shipped so that
#' the increment examples can start from a prepared tree list instead of
#' repeating the five-step preparation in every single \code{@examples} block.
#'
#' These are \emph{not} a fourth processing stage of the inventory taxonomy
#' (raw -> pre -> \code{\link{fe_inventory}}, see
#' \code{\link{data_examples_overview}}).
#' They sit after the import, on the analysis side: an
#' \code{\link{fe_inventory}} object carries the inventory, and pulling and
#' preparing its trees is the first step of evaluating it.
#'
#' @section How they were built:
#' With the canonical chain, spelled out in
#' \code{data-raw/data_ex3_trees_essentials.R}:
#' \preformatted{
#' trees_with_heights <- inv |>
#'   pull_trees() |>
#'   height_complete_inventory()
#'
#' inv |>
#'   fill_heights_back(trees_with_heights) |>
#'   pull_trees() |>
#'   trees_add_essentials(method = "BaySF")
#' }
#' The second \code{\link{pull_trees}} is what makes the chain canonical:
#' \code{\link{fill_heights_back}} writes the estimated heights into the
#' inventory object, so the re-pulled trees carry them in \code{height_m} and
#' none of the estimation helper columns (\code{h_est_m}, \code{d_q_cm},
#' \code{h_q_m}) ride along. Folding \code{h_est_m} into \code{height_m} by hand
#' instead gives the same increments but a tree list of a different shape --
#' a difference that has caused real breakage, see \code{\link{inv_inc_tree_extend}}.
#'
#' @format Each a tibble with one row per tree and 23 columns, among them
#'   \code{plot_id}, \code{tree_id}, \code{species_id}, \code{species_group},
#'   \code{age_yr}, \code{dbh_cm}, \code{height_m}, \code{n_rep_ha},
#'   \code{area_rep_ha}, \code{layer_key}, \code{removal}, \code{g_m2},
#'   \code{v_hub_m3} and \code{standing_area_m2}.
#'   \code{data_ex3_sample_trees_essentials} has 139 rows (second survey),
#'   \code{data_ex3_previous_sample_trees_essentials} 112 (first survey).
#'
#' @seealso \code{\link{data_ex3_sample_fe_inventory}} and
#'   \code{\link{data_ex3_previous_sample_fe_inventory}}, the inventories they
#'   were pulled from; \code{\link{inv_increment_repeated_survey}}, the usual
#'   consumer.
#'
#' @family example data
#' @docType data
#' @keywords data
#'
#' @examples
#'   # What the two objects are
#'   dim(data_ex3_sample_trees_essentials)
#'   dim(data_ex3_previous_sample_trees_essentials)
#'   names(data_ex3_sample_trees_essentials)
#'
#'   # They are ready to use, no preparation needed. The usual consumer is
#'   # inv_increment_repeated_survey(); its help page runs the whole chain on
#'   # exactly this pair, so it is not repeated here.
#'   head(data_ex3_sample_trees_essentials[, c("plot_id", "species_id",
#'                                             "dbh_cm", "height_m", "v_hub_m3")])
#' @name data_ex3_trees_essentials
NULL

#' @rdname data_ex3_trees_essentials
"data_ex3_sample_trees_essentials"

#' @rdname data_ex3_trees_essentials
"data_ex3_previous_sample_trees_essentials"

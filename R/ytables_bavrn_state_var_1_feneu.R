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

#' Complete Bavarian-State-Forest Yield-Table Selection
#'
#' A yield-table selection assigning a ForestElementsR yield table to every
#' \code{bavrn_state} tree species code, for use with
#' \code{\link{inv_increment_ytables}} and its overview. It is FeNEU's completed
#' counterpart to \code{ForestElementsR::ytables_bavrn_state_var_1}: that FER
#' dataset predates the additional \code{bavrn_state} tree species codes
#' introduced in ForestElementsR 3.0.0 and does not cover them, so
#' \code{\link{inv_increment_ytables}} aborts on data containing such species
#' (e.g. recent Bavarian State Forest inventories). This selection covers all
#' current \code{bavrn_state} tree species codes and is therefore the
#' recommended default for the \code{bavrn_state} coding.
#'
#' The 20 species not present in the FER selection are assigned a surrogate
#' yield table by genus / species-group analogy to an already-assigned congener
#' (e.g. the additional oaks use the oak table, the elms the ash table). Three
#' assignments are silvicultural judgement calls: the cedars (Atlaszeder,
#' Libanonzeder) use the larch table, because the Bavarian State Forest counts
#' them in the larch species group; the Baumhasel uses the birch table, as a
#' minor broadleaf (\dQuote{sonstiges Laubholz}). The remaining 45 species keep
#' exactly the assignments of \code{ForestElementsR::ytables_bavrn_state_var_1}.
#'
#' @name ytables_bavrn_state_var_1_feneu
#' @docType data
#' @keywords data
#' @format A tibble with one row per species, and two columns:
#' \describe{
#'   \item{species_id}{An \code{fe_species_bavrn_state} vector (ForestElementsR
#'     species coding \code{"bavrn_state"}) identifying the tree species.}
#'   \item{ytable_name}{(character) The name of the ForestElementsR yield-table
#'     object assigned to that species (e.g.
#'     \code{"fe_ytable_spruce_gehrhardt_moderate_1921"}).}
#' }
#' @seealso \code{\link{inv_increment_ytables}},
#'   \code{ForestElementsR::ytables_bavrn_state_var_1}
#' @source Extends \code{ForestElementsR::ytables_bavrn_state_var_1} with
#'   surrogate assignments for the tree species codes it does not cover.
"ytables_bavrn_state_var_1_feneu"

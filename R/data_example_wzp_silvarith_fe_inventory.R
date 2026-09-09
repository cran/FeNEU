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


#' Stand-Wise Inventory as an \link{fe_inventory} object
#'
#' @name data_ex6_standwise_fe_inventory
#' @family example data
#' @docType data
#' @keywords data
#' @details This \code{\link{fe_inventory}} object (\code{fe_stand} plots) was
#'   built via the two-stage import chain
#'   (\code{\link{import_standwise_relascope_format1_raw_to_pre}} +
#'   \code{\link{import_standwise_relascope_pre_to_fe_inventory}}) from the
#'   raw Silvarith-style example data shipped at
#'   \code{system.file("extdata", "data_ex6_standwise_raw", package = "FeNEU")}.
#'   It has two fictitious stands and carries no plot coordinates, as is
#'   typical for stand-wise inventories with angle-count
#'   (relascopic, \dQuote{Winkelzählprobe}) surveys.
NULL

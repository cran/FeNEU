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

#' Example raw sample inventory tables (Format 1, 10 inventory points)
#'
#' Artificial but realistic raw forest inventory tables (tree, plot and
#' regeneration) in \code{FeNEU}'s raw data \strong{Format 1} for sample
#' inventories with concentric circles. They describe an inventory of only ten
#' inventory points. The Gauss-Krueger coordinates in the point table are random
#' numbers and do not refer to any real location. Only the columns the Format-1
#' converter consumes are kept (see
#' \code{\link{import_sample_concentric_format1_raw_to_pre}}).
#'
#' @name data_ex1_sample_raw
#' @family example data
#' @docType data
#' @keywords data
#' @aliases data_ex1_sample_raw_points data_ex1_sample_raw_trees data_ex1_sample_raw_smalltrees
#'
#' @format
#' \describe{
#'   \item{data_ex1_sample_raw_points}{Plot table linked by \code{plot_id}.}
#'   \item{data_ex1_sample_raw_trees}{Tree table linked by \code{plot_id}.}
#'   \item{data_ex1_sample_raw_smalltrees}{Regeneration table linked by \code{plot_id}.}
#' }
#'
#' @source Internal example data (artificial, Format 1)
NULL


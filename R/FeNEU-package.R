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
#' @import sf
# ForestElementsR is in Depends, not Imports: FeNEU is an evaluation layer on
# top of its objects, so users need its functions on their search path as well.
# The import below is kept in addition, so that the namespace is loaded even
# when FeNEU is only loaded and not attached (FeNEU::foo()). Either way the
# point is that ForestElementsR's S3 methods get registered. FeNEU calls that
# package through :: throughout, and :: alone does not load the namespace, so
# without this, generics dispatching on ForestElementsR classes fell through to
# the default method - plot() on an inventory plot failed in exactly that way,
# and species codes printed as bare numbers.
#' @importFrom ForestElementsR fe_stand
#' @importFrom ggplot2 ggplot geom_point aes
# lifecycle is used for the badges in the roxygen blocks. Those are expanded at
# documentation time, so nothing in the package code calls the package any more
# since the defunct functions were removed. The import below keeps the declared
# dependency an actually used one.
#' @importFrom lifecycle deprecated
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom Rdpack reprompt
#' @importFrom tidyselect vars_select_helpers
utils::globalVariables("i")
# Bare column names consumed by tidyselect contexts (group_by(),
# tidyr::nesting(), tidyr::complete()), which do not accept .data$
# pronouns. Listed here so R CMD check stops flagging them as undefined
# globals.
utils::globalVariables(c(
  "fill_option", "species_group",
  "id_able_tree", "tree_status", "plot_id",
  "age_class", "d_q_class", "layer_key", "removal"
))
NULL

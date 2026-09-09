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

#' Inventory-Wide Meta Information for the Table Headers
#'
#' Derive the enterprise-level figures that the meta block of the static table
#' PDFs (\code{\link{output_base_table_pdf}},
#' \code{\link{output_structure_table_pdf}}) displays: the inventory year, the
#' total (represented) area, and the number of inventory points. The result is
#' returned in exactly the \code{list(year, area_ha, n_plots)} shape those
#' functions expect for their \code{meta} argument (and their \code{inventory}
#' argument uses this function internally).
#'
#' The figures are read off the whole \code{\link{fe_inventory}} object, which
#' is the authoritative plot list - so treeless plots (and their area) are
#' counted too, unlike a derivation from an already-filtered tree table. This
#' mirrors how the increment tables derive their header meta from the second
#' inventory.
#'
#' @param x A \code{\link{fe_inventory}} object.
#'
#' @return A named list with three elements: \code{year} (the survey year; the
#'   most recent one if the plots carry more than one, with a warning),
#'   \code{area_ha} (sum of the per-plot represented areas), and \code{n_plots}
#'   (number of inventory points).
#'
#' @export
#'
#' @examples
#'   inventory_meta(data_ex3_sample_fe_inventory)
#'
inventory_meta <- function(x) {
  stopifnot(is_fe_inventory(x))

  # Pull the survey year per plot. fe_ccircle_spatial carries a validated
  # top-level time_yr element; a plain fe_stand does NOT (ForestElementsR keeps
  # time_yr there only as a per-tree column of the trees slot) - a known
  # inconsistency between the child and its parent (see the FER follow-up note
  # in CLAUDE.md). Read the top-level element when present, else fall back to
  # the trees slot. NB fe_stand is a general stand representation, not a
  # WZP-specific one.
  yrs <- unlist(lapply(x$plot, function(p) {
    ty <- tryCatch(p[["time_yr"]], error = function(e) NULL)
    if (is.null(ty) || length(ty) == 0) {
      ty <- tryCatch(p[["trees"]][["time_yr"]], error = function(e) NULL)
    }
    if (is.null(ty) || length(ty) == 0) NA_real_ else as.numeric(ty)
  }))
  yrs <- yrs[!is.na(yrs)]

  if (length(unique(yrs)) > 1L) {
    warning(fe_msg("inventory_meta_multiyear",
                   paste(sort(unique(yrs)), collapse = ", ")),
            call. = FALSE)
  }

  list(
    year    = if (length(yrs) > 0) max(yrs) else NA_real_,
    area_ha = sum(x$area_rep_ha),
    n_plots = nrow(x)
  )
}

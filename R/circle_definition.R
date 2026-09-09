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

# Circle-definition file for concentric sample inventories.
#
# The circle definitions live in a small table keyed by fe_ikl (the inventory
# circle-class, "Inventurkreisklasse"), one row per concentric circle, with
# clean, unit-bearing column names:
#
#   fe_ikl   dbh_cm_from   radiuses_m
#   ALL      0             2.82
#   ALL      12            5.64
#   ALL      30            12.62
#
# fe_ikl = "ALL" (case-insensitive) is the common case: ONE uniform definition
# for every plot, and it must then be the only definition in the file.
# Otherwise each fe_ikl is a class; the inventory-point file links each plot to
# its class via its own fe_ikl column. (Within one fe_ikl there are many
# individual sample circles across the inventory -- fe_ikl is the class, not an
# individual circle; that is what the name kreis_id would denote.)

# Parse an already-read circle-definition table (keyed by fe_ikl).
#
# @param raw A data frame with columns fe_ikl, dbh_cm_from (or bhd_cm_von),
#   radiuses_m (or radius_m).
# @return list(circle_definitions, uniform):
#   circle_definitions  data.frame(fe_ikl, bhd_von, radius) for build_fdvikrs()
#   uniform             TRUE for a single "ALL" definition, FALSE per-class.
# @keywords internal
.parse_circle_definition_df <- function(raw) {
  names(raw) <- tolower(names(raw))
  # Canonical file columns are dbh_cm_from / radiuses_m (matching the importer
  # parameter names); accept the older bhd_cm_von / radius_m as aliases.
  if ("bhd_cm_von" %in% names(raw) && !"dbh_cm_from" %in% names(raw)) {
    names(raw)[names(raw) == "bhd_cm_von"] <- "dbh_cm_from"
  }
  if ("radius_m" %in% names(raw) && !"radiuses_m" %in% names(raw)) {
    names(raw)[names(raw) == "radius_m"] <- "radiuses_m"
  }

  req     <- c("fe_ikl", "dbh_cm_from", "radiuses_m")
  missing <- setdiff(req, names(raw))
  if (length(missing) > 0) {
    stop(fe_msg("circle_def_file_cols_missing", paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  # Parse the numeric geometry columns tolerant of the decimal mark, via the same
  # whitelist mechanism the inventory-point reader uses: .read_delim_auto reads
  # with decimal_mark = ".", so a German comma radius ("2,82") arrives as text; a
  # bare as.numeric() would turn it into NA and silently corrupt the (result-
  # relevant) circle geometry. .parse_numeric_cols accepts comma OR point and
  # warns on genuinely bad tokens. fe_ikl is a class key and stays untouched.
  raw <- .parse_numeric_cols(raw, .ccirc_cdef_numeric)

  raw <- data.frame(
    fe_ikl      = trimws(as.character(raw$fe_ikl)),
    dbh_cm_from = raw$dbh_cm_from,
    radiuses_m  = raw$radiuses_m,
    stringsAsFactors = FALSE
  )

  # --- Uniform case: a single "ALL" definition for every plot -----------------
  is_all <- toupper(raw$fe_ikl) == "ALL"
  if (any(is_all)) {
    if (!all(is_all)) {
      stop(fe_msg("circle_def_all_mixed"), call. = FALSE)
    }
    defs <- raw[order(raw$dbh_cm_from), , drop = FALSE]
    circle_definitions <- data.frame(
      fe_ikl  = 1L,
      bhd_von = defs$dbh_cm_from,
      radius  = defs$radiuses_m
    )
    return(list(circle_definitions = circle_definitions, uniform = TRUE))
  }

  # --- Per-class case: fe_ikl are integer class keys --------------------------
  ikl_int <- suppressWarnings(as.integer(raw$fe_ikl))
  if (any(is.na(ikl_int))) {
    stop(fe_msg("circle_def_ikl_noninteger",
                paste(unique(raw$fe_ikl[is.na(ikl_int)]), collapse = ", ")),
         call. = FALSE)
  }
  circle_definitions <- data.frame(
    fe_ikl  = ikl_int,
    bhd_von = raw$dbh_cm_from,
    radius  = raw$radiuses_m
  )
  circle_definitions <- circle_definitions[
    order(circle_definitions$fe_ikl, circle_definitions$bhd_von), , drop = FALSE]
  list(circle_definitions = circle_definitions, uniform = FALSE)
}


# Resolve the circle-definition source for a concentric raw importer.
#
# Exactly one of {circle_def_filename, (bhds_from + radiuses)} must be supplied;
# the caller checks mutual exclusion / presence and reports it. bhds_from +
# radiuses is the uniform shortcut (one class for all plots); a
# circle_def_filename is the per-class table keyed by fe_ikl (or a single "ALL"
# definition).
#
# @return list(fdvikrs, uniform).
# @keywords internal
.resolve_circle_source <- function(input_path, circle_def_filename,
                                   bhds_from, radiuses, encoding = "auto") {
  if (is.null(circle_def_filename)) {
    return(list(fdvikrs = build_fdvikrs(bhds_from, radiuses), uniform = TRUE))
  }

  raw    <- .read_delim_auto(
    paste0(input_path, "/", .ensure_txt(circle_def_filename)),
    encoding = encoding, as_text = TRUE)
  parsed <- .parse_circle_definition_df(raw)
  list(
    fdvikrs = build_fdvikrs(circle_definitions = parsed$circle_definitions),
    uniform = parsed$uniform
  )
}

# Assign each inventory point its circle class (fe_ikl).
#
# Uniform definition (bhds_from/radiuses or an "ALL" file): every plot gets the
# single class 1; if the inventory points carry an fe_ikl column with more than
# one distinct value, that contradicts a uniform definition -> warning.
# Per-class definition: the inventory points must carry an fe_ikl column (the
# caller checks its presence); each value's coverage against the definitions is
# checked by the caller via fdinvkrs vs. fdvikrs.
#
# @param inv_points Inventory-point data frame.
# @param uniform Logical; TRUE when a single definition applies to all plots.
# @keywords internal
.assign_circle_class <- function(inv_points, uniform) {
  has_ikl <- "fe_ikl" %in% names(inv_points)

  if (uniform) {
    if (has_ikl &&
        dplyr::n_distinct(inv_points$fe_ikl[!is.na(inv_points$fe_ikl)]) > 1) {
      warning(fe_msg("circle_uniform_multi_ikl"), call. = FALSE)
    }
    inv_points$fe_ikl <- 1L
    return(inv_points)
  }

  # Per-class: keep the point's own fe_ikl (presence is checked by the caller).
  inv_points$fe_ikl <- as.integer(inv_points$fe_ikl)
  inv_points
}


#' Generate a Circle-Definition File for a Concentric Sample Inventory
#'
#' Writes an \code{fe_ikl}-keyed circle-definition file (default
#' \code{Probekreisdefinition.txt}) for the concentric sample import chain
#' (\code{\link{import_sample_concentric_format1_raw_to_pre}},
#' \code{\link{import_sample_concentric_format2_raw_to_pre}}). The concentric
#' importers require a circle definition; when a ready-made one is not at hand,
#' this helper produces a \strong{uniform} definition -- the same set of
#' concentric circles for \emph{every} plot -- written as a single
#' \code{fe_ikl = "ALL"} group. (For multiple circle classes, write the file
#' yourself with one integer \code{fe_ikl} per class and link each inventory
#' point to its class via its own \code{fe_ikl} column.)
#'
#' There is deliberately no default geometry: the caller must supply
#' \code{dbh_cm_from} and \code{radiuses_m}, because the evaluation results
#' depend directly on the circle sizes. These parameter names match those of the
#' concentric raw importers.
#'
#' @param output_dir Folder the file is written to (character). Created if it
#'   does not exist.
#' @param dbh_cm_from Numeric vector of lower DBH limits in \strong{cm}, one per
#'   concentric circle (e.g. \code{c(0, 12, 30)}).
#' @param radiuses_m Numeric vector of circle radii in \strong{m}, the same
#'   length as \code{dbh_cm_from} (e.g. \code{c(2.82, 5.64, 12.62)}).
#' @param filename Base file name with or without the \code{.txt} extension. Default
#'   \code{"Probekreisdefinition"}.
#'
#' @return The path of the written file (invisibly).
#'
#' @seealso \code{\link{import_sample_concentric_format1_raw_to_pre}},
#'   \code{\link{import_sample_concentric_format2_raw_to_pre}}
#'
#' @export
#'
#' @examples
#' dir <- tempdir()
#' generate_circle_definition(
#'   dir,
#'   dbh_cm_from = c(0, 12, 30),
#'   radiuses_m  = c(2.82, 5.64, 12.62)
#' )
#'
generate_circle_definition <- function(output_dir,
                                       dbh_cm_from,
                                       radiuses_m,
                                       filename = "Probekreisdefinition") {
  if (!is.numeric(dbh_cm_from) || !is.numeric(radiuses_m) ||
      length(dbh_cm_from) != length(radiuses_m) || length(dbh_cm_from) < 1L) {
    stop(fe_msg("circle_def_gen_args"), call. = FALSE)
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  ord <- order(dbh_cm_from)
  out <- data.frame(
    fe_ikl      = "ALL",
    dbh_cm_from = dbh_cm_from[ord],
    radiuses_m  = radiuses_m[ord]
  )

  path <- file.path(output_dir, paste0(filename, ".txt"))
  utils::write.table(out, path, sep = "\t", row.names = FALSE, quote = FALSE)
  invisible(path)
}

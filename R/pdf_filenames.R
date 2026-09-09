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

#' Sanitise a String for Use in a File Name
#'
#' Turns an arbitrary string - a user-supplied table title, a plot identifier, or
#' a captured \code{tree_filter} expression - into a compact, file-name-safe
#' token: runs of characters outside \code{[A-Za-z0-9.]} collapse to single
#' underscores, and leading or trailing dots and underscores are trimmed.
#'
#' Dots are kept \emph{inside} the token on purpose, because plot identifiers are
#' commonly dotted (\code{900000002.1.99900002}) and should stay recognisable in
#' the file name. Only a leading dot is removed - it would make the rendered PDF
#' a hidden file on unix-alikes - along with a trailing one, which Windows does
#' not accept in a file name.
#'
#' This is the single sanitiser used by all \code{*_pdf()} functions, so the
#' rendered file names follow one convention across the table families.
#'
#' @param s A length-one character.
#'
#' @param empty A length-one character used when \code{s} contains nothing that
#'   survives sanitising and would yield an empty string.
#'
#' @return A length-one character, safe to embed in a file name.
#'
#' @keywords internal
#'
.safe_filename_part <- function(s, empty = "x") {
  x <- gsub("[^A-Za-z0-9.]+", "_", as.character(s))
  x <- gsub("_+", "_", x)
  x <- gsub("^[._]+|[._]+$", "", x)
  if (nchar(x) == 0L) empty else x
}


#' File-Name Tag for an Increment Table's Tree Collective
#'
#' Derives the file-name fragment identifying which tree collective an increment
#' table describes, from the \code{collective} descriptor the increment pipeline
#' carries along (see \code{.collective_from_filter}). The standard kinds
#' (\code{"main_stand"}, \code{"all_layers"}) are already file-safe and are used
#' as they are; a \code{"custom"} collective is tagged with its sanitised
#' \emph{definition} (the tree-filter expression) rather than the bare word
#' \code{"custom"}, so that two PDFs built from different user-defined filters do
#' not overwrite each other. This mirrors what \code{.table_pdf_meta} does for
#' the base and structure tables.
#'
#' @param collective A collective descriptor list with \code{kind} and
#'   \code{definition} elements.
#'
#' @return A length-one character, safe to embed in a file name.
#'
#' @keywords internal
#'
.collective_file_tag <- function(collective) {
  kind <- collective$kind
  if (is.null(kind) || is.na(kind)) return("kollektiv")
  if (!identical(kind, "custom")) return(.safe_filename_part(kind, "kollektiv"))

  def <- collective$definition
  if (is.null(def) || is.na(def)) return("custom")
  .safe_filename_part(def, "custom")
}


#' File-Name Tag for an Increment Table's Method
#'
#' Derives the file-name fragment identifying \emph{which of the three increment
#' strands} a base table came from. Without it the three strands collide: the
#' file name is built from the table title, the tree collective and the class
#' axis, all three of which a repeated-survey, a gnfi3 and a yield-table table of
#' the same enterprise share. The rendered PDFs are distinguishable (their
#' subtitle names the method) -- only the file names were not, so the last one
#' rendered silently replaced the others.
#'
#' The tag is read from the \code{source} element of the header meta the
#' pipeline already carries: \code{\link{inv_inc_tree_extend_single}} stamps
#' \code{"gnfi3"} and \code{\link{increment_ytables_base_table}} stamps
#' \code{"ytable"}. The repeated survey sets no \code{source} -- it is the
#' reference case and keeps its file name unchanged, so PDFs and scripts that
#' already exist do not shift.
#'
#' @param meta The header meta list carried on an increment output table (its
#'   \code{"meta"} attribute), or \code{NULL}.
#'
#' @return A length-one character: the sanitised method tag, or \code{""} for the
#'   repeated survey (and whenever no source is recorded). Callers append it with
#'   a separator only when it is non-empty.
#'
#' @keywords internal
#'
.increment_method_tag <- function(meta) {
  src <- meta$source
  if (is.null(src) || length(src) != 1L || is.na(src)) return("")
  .safe_filename_part(src, "")
}

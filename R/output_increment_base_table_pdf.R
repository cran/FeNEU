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

#' Render an Increment Base Table in PDF Format
#'
#' Increment counterpart of \code{\link{output_base_table_pdf}}. Turns the
#' restructured increment base table from \code{\link{output_increment_base_table}}
#' into a formatted PDF, using the unified header (Forstbetrieb name, collective
#' line, meta line) modelled on the increment overview PDFs.
#'
#' The header is built in four parts: the table title, the prominent
#' Forstbetrieb name (\code{tab_title}) right below it, and a flushleft meta
#' block modelled on the \dQuote{Zuwachs auf Betriebsebene} overview PDF -- a
#' collective line (\dQuote{Hauptbestand} / \dQuote{Alle Schichten} /
#' \dQuote{Benutzerdefiniertes Kollektiv: ...}) taken from the table's
#' \code{collective} descriptor, followed by the inventory meta (year, area,
#' point count and, for repeated inventories, the previous inventory year,
#' period, Berechnungsmethode and Ergänzungsmethode). A footnote states
#' whether the table is restricted to the main stand (a partial collective that
#' allows virtual per-species hectare areas) or not.
#'
#' The user must make sure that \code{x} was produced by
#' \code{\link{output_increment_base_table}}; the function does not check that.
#'
#' @param x Input object, the output of \code{\link{output_increment_base_table}}.
#'   Its \code{"collective"} attribute is used unless \code{collective} is given.
#'
#' @param tab_title Character; the Forstbetrieb (or collective) name shown
#'   prominently in the header and used for the file name.
#'
#' @param type Character, \code{"age"} (default) or \code{"dq"}, must match how
#'   the table was built. Used for the subtitle, the class-column header and the
#'   file name.
#'
#' @param output_dir Path to the directory the PDF is written to. Default
#'   \code{NA}: the R session's temporary directory (gone when the session ends).
#'
#' @param meta Optional named list with inventory meta-information for the header
#'   meta block. Recognised names: \code{year} (current inventory year),
#'   \code{area_ha} (total area in ha), \code{n_plots} (number of inventory
#'   points). For repeated-inventory increments the block additionally shows, if
#'   present, \code{inv_year_prev} (previous inventory year), \code{period_yr}
#'   (period length in years), \code{method} (Berechnungsmethode) and
#'   \code{fill_option} (Ergänzungsmethode) -- the same block as the
#'   \dQuote{Zuwachs auf Betriebsebene} overview PDF. Any name may be omitted.
#'   \code{NULL} (default) uses the \code{"meta"} attribute that the increment
#'   pipeline carries on \code{x} (assembled in \code{\link{inv_inc_tree_extend}}
#'   for a repeated inventory); pass a list to override it, or when the source
#'   carries none.
#'
#' @param collective Optional collective descriptor list
#'   (\code{kind}/\code{label}/\code{definition}) overriding the one carried on
#'   \code{x}. \code{NULL} (default) uses \code{attr(x, "collective")}; if that is
#'   also absent, an \dQuote{Alle Schichten} collective is assumed.
#'
#' @param ytables_used Optional data frame of the yield tables used (columns
#'   \code{species_group}, \code{name_orig}), printed as a \dQuote{Verwendete
#'   Ertragstafeln} list below the table (one row per table). \code{NULL}
#'   (default) uses \code{attr(x, "ytables_used")}, which the yield-table class
#'   tables carry; for the repeated-survey and gnfi3 tables it is absent and
#'   nothing is printed.
#'
#' @param language Language of the rendered output. Currently only \code{"ger"}
#'   is implemented and accepted; other values raise an error. Future languages
#'   are added as separate template files.
#'
#' @returns The path to the rendered PDF file.
#'
#' @family increment
#' @family pdf_output
#'
#' @export
#'
#' @examples
#' \donttest{
#' # A gnfi3 increment of the ex3 inventory; the ready-made tree list is
#' # shipped with the package, so the height chain does not have to be run
#' inc <- inv_increment_gnfi3(
#'   data_ex3_sample_fe_inventory, data_ex3_sample_trees_essentials
#' )
#'
#' bt <- increment_base_table_main_stand(inc, by_class = "age") |>
#'   output_increment_base_table()
#'
#' # output_dir defaults to tempdir()
#' pdf_path <- output_increment_base_table_pdf(
#'   bt,
#'   tab_title = "Forstbetrieb Beispiel",
#'   meta      = list(year = 2024, area_ha = 1234, n_plots = 42)
#' )
#' basename(pdf_path)
#' }
output_increment_base_table_pdf <- function(x,
                                            tab_title,
                                            type         = c("age", "dq"),
                                            output_dir   = NA,
                                            meta         = NULL,
                                            collective   = NULL,
                                            ytables_used = NULL,
                                            language     = c("ger")) {

  language <- match.arg(language)
  check_pdf_dependencies("output_increment_base_table_pdf")
  type      <- match.arg(type)
  tab_title <- as.character(tab_title)

  # Meta and collective: an explicit argument wins, otherwise the descriptor
  # carried by output_increment_base_table() as an attribute (assembled by the
  # increment pipeline). Collective falls back to all-layers.
  if (is.null(meta))       meta       <- attr(x, "meta")
  if (is.null(collective)) collective <- attr(x, "collective")
  if (is.null(collective)) {
    collective <- list(kind = "all_layers", label = "Alle Schichten",
                       definition = NA_character_)
  }
  # Yield-table class tables carry a list of used yield tables (as an attribute
  # from output_increment_base_table()); for the other sources it is NULL and
  # the template simply prints nothing.
  if (is.null(ytables_used)) ytables_used <- attr(x, "ytables_used")

  if (is.na(output_dir)) output_dir <- tempdir()
  # rmarkdown::render() resolves output paths relative to the template (in the
  # installed package) unless output_dir is absolute -- normalise it so the PDF
  # lands where the caller expects.
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

  # Which of the three increment strands the table came from. Without it the
  # strands collide: collective and class type are identical for a
  # repeated-survey, a gnfi3 and a yield-table table of the same enterprise, so
  # the last one rendered replaced the others. Empty for the repeated survey,
  # which keeps the file name it always had (see .increment_method_tag).
  method_tag <- .increment_method_tag(meta)

  rmarkdown::render(
    input = paste0(system.file(package = "FeNEU"),
                   "/rmd/output_increment_base_table_template_ger.Rmd"),
    params = list(x            = x,
                  tab_title    = tab_title,
                  type         = type,
                  meta         = meta,
                  collective   = collective,
                  ytables_used = ytables_used),
    output_format = "pdf_document",
    # The file name carries every axis the table can vary along - the tree
    # collective, the class type and the increment method - so that the variants
    # of one enterprise do not overwrite each other. A user-defined collective is
    # tagged with its filter expression rather than the bare word "custom" (see
    # .collective_file_tag).
    output_file   = paste0("output_increment_base_table_ger_",
                           .safe_filename_part(tab_title), "_",
                           .collective_file_tag(collective), "_", type,
                           if (nzchar(method_tag)) paste0("_", method_tag)
                           else "",
                           ".pdf"),
    output_dir    = output_dir
  )
}

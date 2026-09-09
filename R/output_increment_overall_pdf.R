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

#' Render an Increment Overview Table in PDF Format
#'
#' Thin wrapper around \code{rmarkdown::render()} that turns the
#' restructured intermediate from \code{\link{output_increment_overall}} into a
#' nicely formatted PDF. The intermediate's \code{meta} block drives the
#' title, survey-year line, plot-match summary, and the fill_option
#' subtitle; the three tibbles (\code{table_matched},
#' \code{table_2nd_only}, \code{table_combined}) are rendered stacked
#' below with shared column shape and decimal-point-bound numbers.
#'
#' The user must make sure that the input object \code{x} has been
#' generated with \code{\link{output_increment_overall}}; the function does
#' not check that. Other objects will either produce an error or
#' strange results.
#'
#' @param x Input object, must be the output of
#'   \code{\link{output_increment_overall}}.
#' @param output_dir Path to the directory where the PDF will be
#'   written. Default \code{NA}; in this case the file goes into the
#'   R session's temporary directory and is gone when the session
#'   ends.
#' @param language Language of the rendered output. Currently only
#'   \code{"ger"} (German) is implemented and accepted; passing any
#'   other value raises an error. An English variant is planned and
#'   will be added as a separate template
#'   (\code{output_increment_overall_template_eng.Rmd}), keeping each
#'   template linguistically consistent rather than scattering
#'   if-switches across one template.
#'
#' @returns The path to the rendered PDF file.
#'
#' @family pdf_output
#'
#' @export
#'
#' @examples
#' \donttest{
#' # See output_increment_overall() for what the intermediate is built from;
#' # matching and gap filling of the ex3 pair are shipped ready-made.
#' inv_b       <- data_ex3_sample_fe_inventory
#' inv_a_trees <- data_ex3_previous_sample_trees_essentials
#' inv_b_trees <- data_ex3_sample_trees_essentials
#' inc_rep     <- data_ex3_increment_matched
#' inc_fl      <- data_ex3_increment_fills
#'
#' inc_sum <- inv_inc_summaric(
#'   inc_rep$tree_increments, inc_fl$fills_forward, inv_a_trees, inv_b_trees
#' )
#' bo <- inv_inc_big_overview(
#'   inc_fl, inv_a_trees, inv_b_trees, inv_b,
#'   style = "generic", inc_summaric = inc_sum
#' )
#' out <- output_increment_overall(
#'   bo,
#'   fill_option   = "standard",
#'   user_subtitle = "Forstbetrieb Neustadt"
#' )
#'
#' # output_dir defaults to tempdir()
#' pdf_path <- output_increment_overall_pdf(out)
#' basename(pdf_path)
#' }
#' @family increment
output_increment_overall_pdf <- function(x,
                                   output_dir = NA,
                                   language   = c("ger")) {

  language <- match.arg(language)
  check_pdf_dependencies("output_increment_overall_pdf")

  if (is.na(output_dir)) output_dir <- tempdir()
  # rmarkdown::render() resolves the output path relative to the
  # *input* template by default, not to getwd() -- so an output_dir
  # like "." ends up next to the template inside the installed
  # FeNEU/rmd/ directory. Normalise to an absolute path here so the
  # PDF really lands where the caller expects it.
  output_dir <- normalizePath(output_dir, winslash = "/",
                              mustWork = FALSE)

  # Template selection by language (currently only ger is implemented).
  template <- paste0(
    system.file(package = "FeNEU"),
    "/rmd/output_increment_overall_template_ger.Rmd"
  )

  # File name tag: use user_subtitle when present, else just the fill_option.
  tag <- if (!is.null(x$meta$user_subtitle)) {
    paste0(.safe_filename_part(x$meta$user_subtitle), "_", x$meta$fill_option)
  } else {
    x$meta$fill_option
  }

  output_file <- paste0(
    "output_increment_overall_", language, "_", tag, ".pdf"
  )

  rmarkdown::render(
    input         = template,
    params        = list(x = x),
    output_format = "pdf_document",
    output_file   = output_file,
    output_dir    = output_dir
  )
}

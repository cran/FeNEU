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


#' Restructure a gnfi3 Single-Inventory Increment Overview for Output
#'
#' Builds the species-level increment overview for the increment estimated from
#' a single inventory with the tree growth functions of the Third German
#' National Forest Inventory (gnfi3). The layout corresponds to table B of
#' \code{\link{output_increment_overall}}: one row per species group plus a total row,
#' each carrying the annual volume increment as an absolute total
#' (\code{zv_m3_yr}) and per hectare (\code{zv_m3_ha_yr}). Confidence intervals
#' are not reported here -- consistent with the package-wide convention that the
#' gnfi3 estimate carries no empirical confidence interval (unlike the repeated
#' survey).
#'
#' Like the increment base tables, this is the language-neutral intermediate;
#' \code{species_group} stays an \code{fe_species_*} vector so the render step
#' decides the language. The table's \code{collective} descriptor is carried
#' through as a \code{"collective"} attribute.
#'
#' @param increment_base_table A gnfi3 increment base table, the output of
#'   \code{\link{increment_base_table}} (or
#'   \code{\link{increment_base_table_main_stand}}) built from a single-inventory
#'   source (\code{\link{inv_increment_gnfi3}}). Its \code{total} and
#'   \code{all_total} elements drive the overview.
#'
#' @returns A tibble with one row per species group plus a total row, the
#'   columns \code{species_group}, \code{is_total_row}, \code{zv_m3_yr} and
#'   \code{zv_m3_ha_yr}, and a \code{"collective"} attribute carried over from
#'   the input.
#'
#' @family increment
#'
#' @export
#'
#' @examples
#'   inv_b <- data_ex3_sample_fe_inventory
#'   inv_b_trees <- data_ex3_sample_trees_essentials
#'
#'   inc <- inv_increment_gnfi3(inv_b, inv_b_trees)
#'   increment_base_table(inc, by_class = "age") |>
#'     output_increment_overview_gnfi3()
#'
output_increment_overview_gnfi3 <- function(increment_base_table) {

  bt <- increment_base_table

  species_part <- bt$total |>
    dplyr::arrange(.data$species_group) |>
    dplyr::transmute(
      species_group = .data$species_group,
      is_total_row  = FALSE,
      zv_m3_yr      = .data$iv_m3_yr_total,
      zv_m3_ha_yr   = .data$iv_m3_ha_yr
    )

  total_part <- bt$all_total |>
    dplyr::transmute(
      species_group = bt$total$species_group[NA_integer_],
      is_total_row  = TRUE,
      zv_m3_yr      = .data$iv_m3_yr_total,
      zv_m3_ha_yr   = .data$iv_m3_ha_yr
    )

  out <- dplyr::bind_rows(species_part, total_part)
  attr(out, "collective") <- bt$collective
  out
}



#' Render a gnfi3 Single-Inventory Increment Overview in PDF Format
#'
#' Renders the overview from \code{\link{output_increment_overview_gnfi3}} as a
#' PDF, using the unified header (Forstbetrieb name, collective line, meta line)
#' of the increment table PDFs. The German heading names the method in full
#' (\dQuote{Zuwachsfunktionen der 3. Bundeswaldinventur}); a footnote states
#' that all volume figures are harvested volume under bark (Erntefestmeter ohne
#' Rinde).
#'
#' @param x The output of \code{\link{output_increment_overview_gnfi3}}. Its
#'   \code{"collective"} attribute is used unless \code{collective} is given.
#'
#' @param tab_title Character; the Forstbetrieb name shown prominently in the
#'   header and used for the file name.
#'
#' @param output_dir Path to the directory the PDF is written to. Default
#'   \code{NA}: the R session's temporary directory.
#'
#' @param meta Optional named list with header meta-information (\code{year},
#'   \code{area_ha}, \code{n_plots}); see
#'   \code{\link{output_increment_base_table_pdf}}.
#'
#' @param collective Optional collective descriptor overriding the one carried on
#'   \code{x}.
#'
#' @param language Language of the rendered output. Currently only \code{"ger"}.
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
#' ov <- inv_increment_gnfi3(
#'   data_ex3_sample_fe_inventory, data_ex3_sample_trees_essentials
#' ) |>
#'   increment_base_table(by_class = "age") |>
#'   output_increment_overview_gnfi3()
#'
#' # output_dir defaults to tempdir()
#' pdf_path <- output_increment_overview_gnfi3_pdf(
#'   ov, tab_title = "Forstbetrieb Beispiel"
#' )
#' basename(pdf_path)
#' }
output_increment_overview_gnfi3_pdf <- function(x,
                                                tab_title,
                                                output_dir = NA,
                                                meta       = NULL,
                                                collective = NULL,
                                                language   = c("ger")) {

  language <- match.arg(language)
  check_pdf_dependencies("output_increment_overview_gnfi3_pdf")
  tab_title <- as.character(tab_title)

  if (is.null(collective)) collective <- attr(x, "collective")
  if (is.null(collective)) {
    collective <- list(kind = "all_layers", label = "Alle Schichten",
                       definition = NA_character_)
  }

  if (is.na(output_dir)) output_dir <- tempdir()
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

  rmarkdown::render(
    input = paste0(system.file(package = "FeNEU"),
                   "/rmd/output_increment_overview_gnfi3_template_ger.Rmd"),
    params = list(x          = x,
                  tab_title  = tab_title,
                  meta       = meta,
                  collective = collective),
    output_format = "pdf_document",
    # No class axis in the file name: the overview aggregates over the age /
    # dq classes, so it has only one variant per collective.
    output_file   = paste0("output_increment_overview_gnfi3_ger_",
                           .safe_filename_part(tab_title), "_",
                           .collective_file_tag(collective),
                           ".pdf"),
    output_dir    = output_dir
  )
}



#' Render a Yield-Table Increment Overview in PDF Format
#'
#' Renders the overview of an \code{\link{inv_increment_ytables}} result as a
#' PDF with the unified header (Forstbetrieb name, collective line, meta line).
#' Two tables are shown: the species-group overview (virtual area, mean site
#' index, increment per ha and total) and a listing of the yield tables used per
#' species group. A footnote states that all volumes are harvested volume under
#' bark (Erntefestmeter ohne Rinde) and that the table is restricted to the main
#' stand.
#'
#' @param x The result of \code{\link{inv_increment_ytables}}, or any list
#'   carrying \code{overview}, \code{ytables_used} and \code{collective}. A
#'   bundle also supplies the header figures, so \code{meta} can be omitted.
#'
#' @param tab_title Character; the Forstbetrieb name shown prominently in the
#'   header and used for the file name.
#'
#' @param output_dir Path to the directory the PDF is written to. Default
#'   \code{NA}: the R session's temporary directory.
#'
#' @param meta Optional named list with header meta-information (\code{year},
#'   \code{area_ha}, \code{n_plots}).
#'
#' @param language Language of the rendered output. Currently only \code{"ger"}.
#'   The German table uses only the German yield-table names (\code{name_orig}).
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
#' # FeNEU's own yield-table collection covers all current bavrn_state
#' # species codes; the ForestElementsR collection does not.
#' inc <- inv_increment_ytables(
#'   data_ex3_sample_fe_inventory,
#'   data_ex3_sample_trees_essentials,
#'   ytables_bavrn_state_var_1_feneu
#' )
#'
#' # output_dir defaults to tempdir()
#' pdf_path <- output_increment_overview_ytables_pdf(
#'   inc, tab_title = "Forstbetrieb Beispiel"
#' )
#' basename(pdf_path)
#' }
output_increment_overview_ytables_pdf <- function(x,
                                                  tab_title,
                                                  output_dir = NA,
                                                  meta       = NULL,
                                                  language   = c("ger")) {

  language <- match.arg(language)
  check_pdf_dependencies("output_increment_overview_ytables_pdf")
  tab_title <- as.character(tab_title)

  collective <- x$collective
  if (is.null(collective)) {
    collective <- list(kind = "main_stand", label = "Hauptbestand",
                       definition = NA_character_)
  }

  # The bundle carries its own header figures, so meta only has to be passed
  # when one wants to override them.
  if (is.null(meta) && is_fe_increment_ytables(x)) meta <- x$meta

  if (is.na(output_dir)) output_dir <- tempdir()
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

  rmarkdown::render(
    input = paste0(system.file(package = "FeNEU"),
                   "/rmd/output_increment_overview_ytables_template_ger.Rmd"),
    params = list(x          = x,
                  tab_title  = tab_title,
                  meta       = meta,
                  collective = collective),
    output_format = "pdf_document",
    # The collective tag is constant here in practice - a yield-table increment
    # is only methodically sound on the main stand - but it is written out
    # anyway, so that all increment PDFs share one file-name convention.
    output_file   = paste0("output_increment_overview_ytables_ger_",
                           .safe_filename_part(tab_title), "_",
                           .collective_file_tag(collective),
                           ".pdf"),
    output_dir    = output_dir
  )
}

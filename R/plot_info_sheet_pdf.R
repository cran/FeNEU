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

#' Render an Information Sheet for an Inventory Plot as a PDF File
#'
#' @param x An object of class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}
#'
#' @param title id or name for the title of the plot and file. Default is NA,
#'   which means that the entry \code{stand_id} of the input object \code{x}
#'   will be used. The actual file name will be "plot_info_ger_" followed by
#'   \code{title} and the appendix ".pdf".
#'
#' @param output_dir Path to the directory where the information sheet will
#'   be stored in. Default is NA; in this case, the file will be written into
#'   the temporary directory of the current R session. This directory will,
#'   however, only available as long as the session is going on.
#'
#' @param dbh_scale Factor for oversizing the trees' dbh in the plot
#'
#' @param language Language of the rendered output. Currently only
#'   \code{"ger"} (German) is implemented and accepted; passing any
#'   other value raises an error. Future languages will be added as
#'   separate template files (one template per language, keeping each
#'   template linguistically consistent).
#'
#' @family pdf_output
#'
#' @return The path to the rendered output file
#'
#' @export
#'
#' @examples
#' \donttest{
#' # One info sheet for the first inventory point of ex3.
#' # output_dir defaults to tempdir().
#' file_path <- data_ex3_sample_fe_inventory$plot[[1]] |>
#'   plot_info_sheet_pdf()
#' basename(file_path)
#'
#' # Several sheets in one go (not run here -- one render per plot)
#' # data_ex3_sample_fe_inventory$plot[1:10] |> lapply(plot_info_sheet_pdf)
#' }
plot_info_sheet_pdf <- function(x,
                                title = NA,
                                output_dir = NA,
                                dbh_scale = 4,
                                language = c("ger")) {

  language <- match.arg(language)
  check_pdf_dependencies("plot_info_sheet_pdf")

  stopifnot(ForestElementsR::is_fe_ccircle_spatial(x))

  if(is.na(title)) title           <- x$stand_id
  if(is.na(output_dir)) output_dir <- tempdir()
  # Absolute path so the PDF lands where the caller expects.
  output_dir <- normalizePath(output_dir, winslash = "/",
                              mustWork = FALSE)

  title <- as.character(title)

  path <- rmarkdown::render(
    input = paste0(system.file(package = "FeNEU"),
      "/rmd/plot_info_template_ger_fe_ccircle_spatial.Rmd"
    ),
    params        = list(x = x, title = title,  dbh_scale = dbh_scale),
    output_format = rmarkdown::pdf_document(fig_caption = FALSE),
    # Only the file name is sanitised; the title shown on the sheet keeps its
    # original spelling. This matters here because the default title is the
    # plot's stand_id, which can be an identifier with spaces and dashes
    # (e.g. the standwise "7- 2- 0- - 0" style codes).
    output_file   = paste0("plot_info_ger_", .safe_filename_part(title, "plot")),
    output_dir    = output_dir
  )

  path
}



















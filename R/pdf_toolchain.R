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

#' Diagnose the PDF Rendering Toolchain
#'
#' Checks whether the tools required for rendering PDF reports (pandoc and a
#' LaTeX distribution) are available on the current system, and prints a
#' message for each missing component explaining how to install it.
#'
#' @return Invisibly, a named logical list with elements \code{pandoc} and
#'   \code{latex}.
#'
#' @seealso \code{\link{setup_pdf_toolchain}}
#'
#' @export
#'
#' @examples
#' diagnose_pdf_toolchain()
#'
diagnose_pdf_toolchain <- function() {

  has_pandoc <- rmarkdown::pandoc_available()
  has_latex  <- tinytex::is_tinytex() || nzchar(Sys.which("pdflatex"))

  if (has_pandoc && has_latex) {
    message(fe_msg("pdf_toolchain_ok"))
  } else {
    if (!has_pandoc)
      message(fe_msg("pdf_pandoc_missing"))
    if (!has_latex)
      message(fe_msg("pdf_latex_missing"))
  }

  invisible(list(pandoc = has_pandoc, latex = has_latex))
}


#' Install Missing PDF Rendering Tools
#'
#' Installs TinyTeX if no LaTeX distribution is found on the current system.
#' For pandoc, installation must be done manually (see
#' \code{\link{diagnose_pdf_toolchain}} for instructions).
#'
#' @return Invisibly, the result of \code{\link{diagnose_pdf_toolchain}} after
#'   the setup attempt.
#'
#' @seealso \code{\link{diagnose_pdf_toolchain}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Deliberately not run, and deliberately not merely skipped on the check
#' # farm either: this installs TinyTeX into the user's own filespace. That
#' # must never happen unasked, least of all during a package check.
#' setup_pdf_toolchain()
#' }
setup_pdf_toolchain <- function() {

  if (!tinytex::is_tinytex() && !nzchar(Sys.which("pdflatex"))) {
    message(fe_msg("pdf_tinytex_installing"))
    tinytex::install_tinytex()
  } else {
    message(fe_msg("pdf_latex_available"))
  }

  invisible(diagnose_pdf_toolchain())
}

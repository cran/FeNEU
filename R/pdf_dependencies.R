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

#' Setup for PDF Output Functions
#'
#' The \verb{*_pdf()} functions in FeNEU render their tables through
#' \pkg{rmarkdown} + \pkg{kableExtra} on top of pandoc and a LaTeX
#' distribution. Pandoc and LaTeX are not R packages; they are external
#' tools that must be installed once per machine.
#'
#' @section R packages:
#'   The R-side dependencies are listed as \emph{Suggests} in the
#'   FeNEU \verb{DESCRIPTION}. They are not pulled in by
#'   \code{install.packages("FeNEU")}; users who want PDF output must
#'   install them explicitly:
#'   \preformatted{install.packages(c("rmarkdown", "kableExtra", "tinytex"))}
#'   \code{\link{plot_info_sheet_pdf}} additionally needs \pkg{ggrepel} for
#'   the tree labels on its map:
#'   \preformatted{install.packages("ggrepel")}
#'
#' @section pandoc:
#'   \itemize{
#'     \item Check via \code{rmarkdown::pandoc_available()} (returns
#'       \code{TRUE} when a usable pandoc is on the path).
#'     \item RStudio bundles pandoc and rmarkdown picks it up
#'       automatically.
#'     \item A standalone installer for every platform is available at
#'       \url{https://pandoc.org/installing.html}. On macOS, also
#'       \code{brew install pandoc}; on Debian/Ubuntu,
#'       \code{apt install pandoc}.
#'   }
#'
#' @section LaTeX:
#'   \itemize{
#'     \item The easiest path for R users is \pkg{tinytex}:
#'       \preformatted{install.packages("tinytex")
#' tinytex::install_tinytex()}
#'       The second call downloads a small LaTeX distribution (~ 300 MB)
#'       to the user's home directory. From then on, missing LaTeX
#'       packages required by FeNEU's templates (siunitx, booktabs,
#'       makecell, longtable, ...) are fetched automatically when a
#'       PDF is rendered.
#'     \item Alternatively, any TeX Live or MiKTeX installation works
#'       as long as those LaTeX packages are available.
#'   }
#'
#' @section Diagnostic:
#'   To verify the environment before rendering a first PDF:
#'   \preformatted{rmarkdown::pandoc_available()    # pandoc on PATH?
#' rmarkdown::pandoc_version()      # which version?
#' tinytex::is_tinytex()            # tinytex installed?
#' Sys.which("pdflatex")            # latex on PATH?}
#'
#' @name pdf_dependencies
#'
#' @family pdf_output
#'
#' @examples
#' # See whether everything is in place to render a FeNEU PDF
#' rmarkdown::pandoc_available()
#' requireNamespace("kableExtra", quietly = TRUE)
NULL


#' Check R Package and System Dependencies for PDF Rendering
#'
#' Gate function called at the top of every \verb{*_pdf()} function. Throws an
#' informative error when \pkg{rmarkdown}, \pkg{kableExtra}, or pandoc is
#' missing, so users get a clear install message rather than a cryptic
#' backtrace from inside \code{rmarkdown::render()}.
#'
#' @param fun_name Character string — the name of the calling \verb{*_pdf()}
#'   function, used in the error message.
#'
#' @param extra_pkgs Character vector of further R packages that only the
#'   calling function needs on top of \pkg{rmarkdown} and \pkg{kableExtra}
#'   (e.g. \pkg{ggrepel} for \code{\link{plot_info_sheet_pdf}}). Default: none.
#'
#' @return \code{TRUE} invisibly when all dependencies are present.
#'
#' @seealso \code{\link{pdf_dependencies}}, \code{\link{diagnose_pdf_toolchain}}
#'
#' @family pdf_output
#'
#' @export
#'
#' @examples
#' # The gate every *_pdf() function calls first: silent when everything is in
#' # place, an informative error otherwise. Guarded here so that the example
#' # itself stays harmless on a machine without the toolchain.
#' if (requireNamespace("kableExtra", quietly = TRUE) &&
#'     rmarkdown::pandoc_available()) {
#'   check_pdf_dependencies("output_base_table_pdf")
#' }
check_pdf_dependencies <- function(fun_name, extra_pkgs = character()) {

  needed <- unique(c("rmarkdown", "kableExtra", extra_pkgs))
  missing_pkgs <- needed[
    !vapply(needed, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0L) {
    stop(
      sprintf(
        paste0(
          "%s() needs the following R package(s) which are listed as ",
          "Suggests in FeNEU and are not pulled in by default: %s.\n",
          "Install with:\n  install.packages(c(%s))\n",
          "See ?pdf_dependencies for the full setup."
        ),
        fun_name,
        paste(missing_pkgs, collapse = ", "),
        paste(sprintf("\"%s\"", missing_pkgs), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!rmarkdown::pandoc_available()) {
    stop(
      sprintf(
        paste0(
          "%s() needs pandoc to render the PDF, but pandoc is not ",
          "available.\nIf you are using RStudio its bundled pandoc ",
          "should normally be picked up; otherwise install pandoc ",
          "from https://pandoc.org/installing.html.\n",
          "See ?pdf_dependencies for the full setup."
        ),
        fun_name
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

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
#' The sheet serves two purposes: finding the trees again in the field, and
#' checking a survey. It shows a table of the plot's trees and a map of their
#' positions, both built from the same selection of trees.
#'
#' @details
#' \strong{Which trees are shown.} By default all trees of the plot, removed
#' ones (\code{removal == TRUE}) included -- the same as
#' \code{plot()} on the plot object. Removed trees are marked: in the table by
#' the column \emph{Ausg.}, on the map by a cross over the tree. Pass
#' \code{tree_filter = !removal} for the remaining stand only. Any other
#' selection works the same way; a selection other than the default is printed
#' on the sheet and becomes part of the file name, so that sheets of the same
#' plot with different selections do not overwrite each other.
#'
#' \strong{Trees without a position} (typically those in the innermost circle,
#' whose position is not recorded) appear in the table with an empty distance
#' and azimuth. The map cannot show them; a line on the sheet says how many
#' there are.
#'
#' \strong{Small trees} (the \code{small_trees} of the plot, DBH 0) are not
#' listed individually. A line on the sheet gives their number, counted as
#' trees, not records; \code{tree_filter} does not apply to them.
#'
#' The table is sorted by tree number. Tree numbers of the form
#' \code{"4_13"} are sorted by their numeric parts, so \code{"2_1"} comes
#' before \code{"10_1"}.
#'
#' @param x An object of class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}
#'
#' @param title Heading of the sheet, printed as given. Default is NA, which
#'   gives "Stichprobenpunkt" followed by the entry \code{stand_id} of the
#'   input object \code{x}. The file name is "plot_info_ger_" followed by
#'   \code{title} and \code{stand_id} (or \code{stand_id} alone, by default),
#'   both made file-name safe, and the appendix ".pdf". It always carries the
#'   \code{stand_id}, so a title shared by all plots of an enterprise does not
#'   make the sheets overwrite each other.
#'
#' @param subtitle Line printed below the heading, as given. Default is NA,
#'   which prints none. Meant for whatever the heading leaves out, e.g. the
#'   plot number when the heading names the enterprise and the year.
#'
#' @param output_dir Path to the directory where the information sheet will
#'   be stored in. Default is NA; in this case, the file will be written into
#'   the temporary directory of the current R session. This directory will,
#'   however, only available as long as the session is going on.
#'
#' @param dbh_scale Factor for oversizing the trees' dbh in the plot
#'
#' @param tree_filter A \code{data-masking} expression that applies to the
#'   trees of the plot (\code{x$trees}) and selects the ones shown on the
#'   sheet, in the table and on the map alike. The default \code{TRUE} shows
#'   all trees, removed ones included (and marked); \code{!removal} shows the
#'   remaining stand only. Works like the argument of the same name of
#'   \code{plot()} on the plot object.
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
#' # The same plot without the removed trees; the selection becomes part of
#' # the file name, so both sheets can exist side by side
#' file_path <- data_ex3_sample_fe_inventory$plot[[1]] |>
#'   plot_info_sheet_pdf(tree_filter = !removal)
#' basename(file_path)
#'
#' # Several sheets in one go (not run here -- one render per plot)
#' # data_ex3_sample_fe_inventory$plot[1:10] |> lapply(plot_info_sheet_pdf)
#' }
plot_info_sheet_pdf <- function(x,
                                title = NA,
                                subtitle = NA,
                                output_dir = NA,
                                dbh_scale = 4,
                                tree_filter = TRUE,
                                language = c("ger")) {

  language <- match.arg(language)
  check_pdf_dependencies("plot_info_sheet_pdf", extra_pkgs = "ggrepel")

  stopifnot(ForestElementsR::is_fe_ccircle_spatial(x))

  # Captured here and evaluated once on the tree list, so that table and map
  # are built from one and the same selection (.plot_info_sheet_data()).
  tree_filter <- rlang::enquo(tree_filter)
  filter_text <- if (isTRUE(rlang::quo_get_expr(tree_filter))) {
    NA_character_
  } else {
    rlang::quo_text(tree_filter)
  }

  # A given title is printed as it is; only the default names the plot itself.
  # The file name always carries the stand_id: a title shared by all plots of
  # an enterprise (its name and year) would otherwise give every sheet the
  # same file name, and each would overwrite the one before.
  stand_part <- .safe_filename_part(x$stand_id, "plot")
  if (is.na(title)) {
    heading <- paste("Stichprobenpunkt", x$stand_id)
    file_id <- stand_part
  } else {
    heading    <- as.character(title)
    title_part <- .safe_filename_part(heading, "")
    file_id <- if (title_part %in% c("", stand_part)) {
      stand_part
    } else {
      paste0(title_part, "_", stand_part)
    }
  }
  subtitle <- if (is.na(subtitle)) "" else as.character(subtitle)

  if(is.na(output_dir)) output_dir <- tempdir()
  # Absolute path so the PDF lands where the caller expects.
  output_dir <- normalizePath(output_dir, winslash = "/",
                              mustWork = FALSE)

  # Only the file name is sanitised; the title shown on the sheet keeps its
  # original spelling. This matters here because the default title is the
  # plot's stand_id, which can be an identifier with spaces and dashes
  # (e.g. the standwise "7- 2- 0- - 0" style codes). A selection other than
  # the default is appended, so that sheets of one plot with different
  # selections do not overwrite each other; the default keeps the file name
  # it always had.
  file_stem <- paste0("plot_info_ger_", file_id)
  if (!is.na(filter_text)) {
    file_stem <- paste0(file_stem, "_",
                        .safe_filename_part(filter_text, "filter"))
  }

  path <- rmarkdown::render(
    input = paste0(system.file(package = "FeNEU"),
      "/rmd/plot_info_template_ger_fe_ccircle_spatial.Rmd"
    ),
    params        = list(sheet       = .plot_info_sheet_data(x, tree_filter),
                         heading     = heading,
                         subtitle    = subtitle,
                         dbh_scale   = dbh_scale,
                         filter_text = filter_text),
    output_format = rmarkdown::pdf_document(fig_caption = FALSE),
    output_file   = file_stem,
    output_dir    = output_dir
  )

  path
}


#' Prepare the Content of a Plot Information Sheet
#'
#' Everything \code{\link{plot_info_sheet_pdf}} shows, computed without
#' rendering: the selection of trees, the table, and the counts behind the
#' lines on the sheet. Table and map are built from the same selection, so they
#' cannot disagree. Column names stay English; the template translates them.
#'
#' @param x An object of class
#'   \code{\link[ForestElementsR]{fe_ccircle_spatial}}.
#'
#' @param tree_filter A quosure, as captured by \code{plot_info_sheet_pdf}.
#'
#' @return A list with
#'   \describe{
#'     \item{\code{x}}{\code{x} reduced to the selected trees and their
#'       positions, for the map}
#'     \item{\code{table}}{one row per selected tree record, sorted by tree
#'       number: \code{tree_id}, \code{R}, \code{angle}, \code{species_id},
#'       \code{dbh_cm}, \code{age_yr}, \code{height_m}, \code{tree_count},
#'       \code{removal}, \code{positioned}}
#'     \item{\code{removed_ids}}{tree ids of the selected removed trees}
#'     \item{\code{n_unpositioned_records}, \code{n_unpositioned_trees}}{how
#'       many selected records, and trees, have no position}
#'     \item{\code{n_small_trees}}{number of small trees (not records)}
#'   }
#'
#' @keywords internal
#'
.plot_info_sheet_data <- function(x, tree_filter = rlang::quo(TRUE)) {

  trees <- x$trees
  if (!"tree_count" %in% names(trees)) trees$tree_count <- 1
  # dplyr::filter() drops rows where the expression is NA, where a logical
  # index would have produced rows of NA
  trees <- dplyr::filter(trees, !!tree_filter)

  positions <- x$tree_positions[
    x$tree_positions$tree_id %in% trees$tree_id, , drop = FALSE
  ]
  x$trees          <- trees
  x$tree_positions <- positions

  pos_df <- sf::st_drop_geometry(positions)[, c("tree_id", "R", "angle")]

  table <- trees |>
    dplyr::select("tree_id", "species_id", "dbh_cm", "age_yr", "height_m",
                  "tree_count", "removal") |>
    dplyr::left_join(pos_df, by = "tree_id") |>
    dplyr::mutate(
      removal    = .data$removal %in% TRUE,
      positioned = .data$tree_id %in% pos_df$tree_id
    ) |>
    dplyr::relocate("R", "angle", .after = "tree_id")
  table <- table[.tree_id_order(table$tree_id), , drop = FALSE]

  # small_trees can be NULL, empty, or a placeholder row of NAs
  st <- x$small_trees
  n_small_trees <- 0
  if (!is.null(st) && nrow(st) > 0L) {
    st <- st[!is.na(st$tree_id), , drop = FALSE]
    # as.numeric: the placeholder carries logical columns, whose sum would
    # be an integer
    n_small_trees <- as.numeric(if ("tree_count" %in% names(st)) {
      sum(st$tree_count, na.rm = TRUE)
    } else {
      nrow(st)
    })
  }

  list(
    x                      = x,
    table                  = table,
    removed_ids            = table$tree_id[table$removal],
    n_unpositioned_records = sum(!table$positioned),
    n_unpositioned_trees   = sum(table$tree_count[!table$positioned]),
    n_small_trees          = n_small_trees
  )
}


#' Order Tree Identifiers by Their Numeric Parts
#'
#' Returns the permutation that sorts tree identifiers the way a reader counts
#' them. Identifiers made of numbers joined by underscores -- the
#' \code{"lfd_satz_lfd_bhd"} form of the BaySF import, e.g. \code{"4_13"} --
#' are ordered by their numeric parts from left to right, so \code{"2_1"}
#' precedes \code{"10_1"}. As soon as one identifier does not have this form,
#' all are ordered as text: mixing the two orders would be harder to follow
#' than either.
#'
#' @param ids A character vector of tree identifiers.
#'
#' @return An integer vector, the ordering permutation of \code{ids}.
#'
#' @keywords internal
#'
.tree_id_order <- function(ids) {
  ids <- as.character(ids)
  if (length(ids) == 0L) return(integer())
  if (!all(grepl("^[0-9]+(_[0-9]+)*$", ids))) {
    return(order(ids, method = "radix"))
  }
  parts <- strsplit(ids, "_", fixed = TRUE)
  n_max <- max(lengths(parts))
  keys <- lapply(seq_len(n_max), function(k) {
    vapply(parts, function(p) {
      if (length(p) >= k) as.numeric(p[[k]]) else -1
    }, numeric(1))
  })
  do.call(order, keys)
}



















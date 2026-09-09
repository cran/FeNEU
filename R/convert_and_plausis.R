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

#' Import BaySF-style named flat files (experimental preview of Format 3)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reads a folder that already contains BaySF-style named flat files
#' (\code{BHD.txt}, \code{Inventurpunkte.txt}, \code{SCH.txt}) and re-shapes
#' them into the standard BaySF intermediate output files
#' (\code{fdinvbhd.txt}, \code{fdinvba.txt}, \code{fdinvkrs.txt},
#' \code{fcbestku.txt}, \code{fdvikrs.txt}). Unlike
#' \code{import_sample_concentric_format1_raw_to_pre()} /
#' \code{import_sample_concentric_format2_raw_to_pre()}, which convert
#' field-collected raw data, this reader expects files that are already close
#' to the BaySF target format.
#'
#' @details
#' This is an early, experimental reader for a third, \dQuote{BaySF-named} raw
#' input format. It is not part of the public import API, carries no correctness
#' or stability guarantee, and its interface may change. It will probably
#' converge into the future \code{import_sample_concentric_format_3_raw_to_pre}.
#'
#' @param input_path Path of the input data folder (character)
#'
#' @param bhds_from A vector of the lower diameter limits of the concentric
#'  circles (numeric)
#'
#' @param radiuses A vector of the radiuses of the concentric circles (numeric)
#'
#' @param year The year of the inventory (numeric)
#'
#' @keywords internal
import_data_BaySF_inraw <- function(input_path,
                                    bhds_from = bhds_from,
                                    radiuses = radiuses,
                                    year = year){

  warning(fe_msg("reader_experimental_f3"), call. = FALSE)

  # if bhds_from and radiuses provided they must have the same length
  if (!is.null(bhds_from) & !is.null(radiuses)) {
    if (length(bhds_from) != length(radiuses)) {
      stop(fe_msg("circle_args_len_mismatch"))
    }
  }

  # Read all text tables

  all_files <- list.files(
    path = input_path, pattern = ".txt", all.files = TRUE,
    full.names = FALSE
  )

  required_files <- c(
    "BHD.txt", "Inventurpunkte.txt", "SCH.txt"
  )

  i <- which(!(required_files %in% all_files))
  err_message <- paste(
    paste(required_files[i], collapse = ", ")
  )


  BHD <- readr::read_delim(paste0(input_path, "/BHD.txt"),
                           trim_ws = TRUE)

  Inventurpunkte <- readr::read_delim(paste0(input_path, "/Inventurpunkte.txt"),
                                      trim_ws = TRUE)

  SCH <- readr::read_delim(paste0(input_path, "/SCH.txt"),
                           trim_ws = TRUE)


  # convert data into the required tables for next step with minimum
  # amount of columns: fdinvbhd, fdinvba, fdinvkrs, fdvikrs, fcbestku

  fdinvbhd <- BHD |> dplyr::rename(
    koord = "OSWIKRS",
    pol_wink = "POL_WINK",
    pol_entf = "POL_ENTF",
    mortal = "MORTAL",
    bhd_anz = "BHD_ANZ",
    bhd = "BHD",
    bhd_h = "HOEHE") |>
    dplyr::mutate(
      hmb = ifelse(.data$HMB == "J", "J", NA_real_),
      lfd_satz = as.integer(as.factor(.data$BA_FE)),
      lfd_bhd = stats::ave(.data$LFD_MDE, .data$BA_FE, FUN = seq_along),
      bestkey = 1,
      bestlfd = 1,
      fe_period = 1
    )

  fdinvba <- BHD |>
    dplyr::group_by(.data$BA_FE, .data$lfd_satz) |>
    dplyr::summarise(
      koord = .data$OSWIKRS,
      ba_fe = .data$BA_FE,
      bestkey = 1,
      bestlfd = 1,
      fe_period = 1,
      best_ku = unique(.data$BEST_KU)
    )


  fdinvkrs <- Inventurpunkte |> dplyr::rename(
    koord = "OSWIKRS",
    gauss_rw = "GAUSS_RW",
    gauss_hw = "GAUSS_HW",
    repfl = "REPFL",
    erfdat = "ERFDAT"
  ) |>
    dplyr::mutate(
      bestkey = 1,
      bestlfd = 1,
      fe_period = 1
    )

  # check circle keys
  used_names_circ_keys <- c(
    "fe_period", "erfdat", "bestkey",
    "bestlfd", "koord", "fe_ikl", "neig_grad",
    "repfl"
  )



  if(!("fcbesku" %in% all_files)) {
    # generate file or call it (in the future) from shinny
    # Create the fcbestku data frame
    fcbestku <- data.frame(
      best_ku = c("ungeschichtet", "Oberschicht", "Unterstand",
                  "Vorausverjuengung","Nachhiebsrest", "Ueberhaelter", "Altbaum",
                  "Abgang Mortal"),
      best_ku_l = c("UG", "OS", "US", "VVJ", "NHR", "UEH", "ALT", "+++"),
      best_ku_k = c(1, 1, 2, 3, 4, 4, 4, -1),
      best_ku_g = c(-1, 0, 0, 0, 0, 0, 0, -1),
      gueltig_id = c(0, 1, 2, 3, 4, 5, 6, -1),
      stringsAsFactors = FALSE
    )

  }


  fdivkrs <- data.frame()

  if (!("fdivkrs" %in% all_files)) {
    # generate file or call it (in the future) from shinny

    # Create the fdvikrs data frame
    fdvikrs <- data.frame(
      bhd_von = bhds_from,
      kreis_fl = radiuses * pi^2,
      fe_ikl = seq(1, length(bhds_from)),
    )

    fe_ikl_krs <- unique(fdvikrs$fe_ikl)

    #check that fe_ikl matches the one in the circles keys

    if (unique(fdivkrs$fe_ikl %in% fe_ikl_krs) != TRUE) {
      stop(fe_msg("fe_ikl_mismatch"))
    }

  }

  check_files <- list(
    rqmt_ok = length(i) == 0,
    err_message = err_message
  )

  readr::write_delim(fdinvbhd,
                     file = paste0(input_path, "/fdinvbhd.txt"),
                     delim = "\t")

  readr::write_delim(fdinvba,
                     file = paste0(input_path, "/fdinvba.txt"),
                     delim = "\t")

  readr::write_delim(fdinvkrs,
                     file = paste0(input_path, "/fdinvkrs.txt"),
                     delim = "\t")

  readr::write_delim(fcbestku,
                     file = paste0(input_path, "/fcbestku.txt"),
                     delim = "\t")

  readr::write_delim(fdvikrs,
                     file = paste0(input_path, "/fdvikrs.txt"),
                     delim = "\t")


}



#' Check for missing values in important columns of the tree list
#'
#' @param tree_list variable containing the the tree list
#'
#' @return The input \code{tree_list} unchanged when all checks pass.  Stops
#'   with an informative error message if any checked column contains
#'   \code{NA} values, listing the affected \code{plot_id}s.
#'
#' @keywords internal
#'
missing_checks <- function(tree_list) {

  # Check that the required columns contain no unexpected missing values.
  # Collects every problem instead of stopping at the first, so the calling
  # converter can report them all at once. Column names follow the English
  # canonical standard (German aliases already normalised by the orchestrator).
  # Returns list(tree_list, errors); errors is empty when all checks pass.

  errors <- character()

  missing_plot_id <- tree_list |>
    dplyr::select("plot_id") |>
    dplyr::filter(is.na(.data$plot_id)) |>
    dplyr::distinct()

  if (nrow(missing_plot_id) > 0) {
    errors <- c(errors, fe_msg("plot_id_missing"))
  }

  # tree_nr (baumnummer) is optional and never consumed (identification is by
  # coordinates). Only when the column is actually supplied is a gap in it worth
  # flagging as a data-quality problem; an absent column is fine.
  if ("tree_nr" %in% names(tree_list)) {
    missing_tree_nr <- tree_list |>
      dplyr::filter(is.na(.data$tree_nr)) |>
      dplyr::select("plot_id") |>
      dplyr::distinct()

    if (nrow(missing_tree_nr) > 0) {
      errors <- c(errors, fe_msg("tree_nr_missing",
                                 paste(missing_tree_nr$plot_id, collapse = ", ")))
    }
  }

  # Size requirements differ by source file (distinguished by the `regeneration`
  # flag: FALSE = main tree list, TRUE = small-tree tally):
  #   * main tree list: every tree MUST carry a real diameter (dbh > 0); height
  #     is optional. A missing or zero dbh is an unusable record --
  #     fe_ccircle_spatial() would reject it too, and a height is no substitute
  #     for the diameter here.
  #   * small-tree tally: a record needs at least one size measure -- either a
  #     real diameter (dbh > 0) or a height (both is fine, too).
  # dbh/height are parsed decimal-mark-tolerantly here because this runs before
  # the numeric normalisation step.
  n_rows     <- nrow(tree_list)
  dbh_num    <- if ("dbh" %in% names(tree_list))
    suppressWarnings(.parse_numeric_vec(tree_list$dbh)) else rep(NA_real_, n_rows)
  height_num <- if ("height" %in% names(tree_list))
    suppressWarnings(.parse_numeric_vec(tree_list$height)) else rep(NA_real_, n_rows)

  is_regen  <- tree_list$regeneration %in% TRUE
  no_dbh    <- is.na(dbh_num) | dbh_num == 0
  no_height <- is.na(height_num)

  # Main tree list: dbh is mandatory.
  dbh_required <- unique(tree_list$plot_id[!is_regen & no_dbh])
  if (length(dbh_required) > 0) {
    errors <- c(errors, fe_msg("tree_dbh_required",
                               paste(dbh_required, collapse = ", ")))
  }

  # Small-tree tally: needs a dbh > 0 or a height.
  no_size <- unique(tree_list$plot_id[is_regen & no_dbh & no_height])
  if (length(no_size) > 0) {
    errors <- c(errors, fe_msg("tree_no_size",
                               paste(no_size, collapse = ", ")))
  }

  # NOTE: a missing angle / distance is deliberately NOT an error here. The
  # BaySF-style pre format needs no coordinates for trees of the innermost
  # circle -- those are tallied as a group, not located individually -- and the
  # threshold depends on the inventory's circle definition, which this check
  # does not know. Trees that DO need a position but lack one are excluded (with
  # a prominent warning) by .drop_trees_without_coords() in the converter, after
  # normalisation, where the circle definition is available. Erroring out here
  # wrongly rejected perfectly importable data.

  missing_mortality <- tree_list |>
    dplyr::filter(is.na(.data$mortality)) |>
    dplyr::select("plot_id") |>
    dplyr::distinct()

  if (nrow(missing_mortality) > 0) {
    errors <- c(errors, fe_msg("mortality_missing",
                               paste(missing_mortality$plot_id, collapse = ", ")))
  }

  list(tree_list = tree_list, errors = errors)

}



#' Cross-check plot IDs between tree list and inventory-point list
#'
#' Compares the set of plot IDs (\code{plot_id}) present in the tree list
#' against those in the inventory-point list.  If the two sets differ, the
#' function emits a diagnostic \code{message()} and writes two tab-delimited
#' diagnostic files into \code{output_dir}:
#' \itemize{
#'   \item \code{tree_not_in_inv.txt} — plots with trees but no inventory point
#'   \item \code{inv_not_in_tree.txt} — inventory points with no trees
#' }
#' Writing the diagnostic files alongside the check is intentional: it gives
#' the user ready-to-inspect artefacts without requiring the orchestrating
#' function to manage intermediate data frames.
#'
#' They go to the \strong{output} folder, not next to the input data: the output
#' folder is the one the user picked to be written to, so write permission there
#' is assured (and their business), whereas the input folder may be read-only.
#'
#' @param tree_list   Data frame containing the tree list; must have a
#'   \code{plot_id} column.
#' @param Iventurpunkte Data frame containing the inventory-point list; must
#'   have a \code{plot_id} column.
#' @param output_dir Path of the output folder (character). Diagnostic files are
#'   written here when the two ID sets do not match; created if it does not exist.
#'
#' @return Invisibly, a named list with the mismatch counts
#'   \code{n_tree_not_in_inv} (plots with trees but no inventory point) and
#'   \code{n_inv_not_in_tree} (inventory points with no trees); both \code{0}
#'   when the two ID sets match. The orchestrator uses \code{n_tree_not_in_inv}
#'   to raise a report warning, because trees on those plots have no plot record
#'   and are dropped further down the build.
#'
#' @keywords internal
#'
inv_point_checks <- function(tree_list, Iventurpunkte, output_dir) {

  points_from_treelist <- tree_list |>
    dplyr::select(plot_id) |>
    dplyr::distinct()


  points_from_invlist <- Iventurpunkte |>
    dplyr::select(plot_id) |>
    dplyr::distinct()


  if (nrow(Iventurpunkte) != nrow(points_from_invlist)) {
    warning(fe_msg("dup_plot_ids"))
  }

  # plot_ids from the tree list that do NOT exist in the inventory-point list
  tree_not_in_inv <- dplyr::anti_join(
    points_from_treelist, points_from_invlist, by = "plot_id"
  )
  # plot_ids from the inventory-point list that do NOT exist in the tree list
  inv_not_in_tree <- dplyr::anti_join(
    points_from_invlist, points_from_treelist, by = "plot_id"
  )

  n_tree_not_in_inv <- nrow(tree_not_in_inv)
  n_inv_not_in_tree <- nrow(inv_not_in_tree)

  # Compare the ID *sets*, not just their sizes: equal counts can still hide a
  # symmetric mismatch (e.g. 5 tree-only and 5 inv-only plots).
  if (n_tree_not_in_inv > 0 || n_inv_not_in_tree > 0) {
    message(fe_msg("points_mismatch"))
    message(fe_msg("points_count",
                   nrow(points_from_treelist), nrow(points_from_invlist)))
    # The diagnostic files go to the OUTPUT folder, never to the input folder:
    # that is the one the user chose to be written to, so write permission there
    # is assured (and their business). The input folder may well be read-only.
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    message(fe_msg("missing_points_saved", output_dir))

    readr::write_delim(tree_not_in_inv,
                       file = file.path(output_dir, "tree_not_in_inv.txt"),
                       delim = "\t")
    readr::write_delim(inv_not_in_tree,
                       file = file.path(output_dir, "inv_not_in_tree.txt"),
                       delim = "\t")
  } else {
    message(fe_msg("points_match"))
  }

  invisible(list(n_tree_not_in_inv = n_tree_not_in_inv,
                 n_inv_not_in_tree = n_inv_not_in_tree))

}




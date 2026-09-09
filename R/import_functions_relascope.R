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

# Stand-wise (WZP / angle-count) tree data enters FeNEU through the two-stage
# relascope chain: import_standwise_relascope_format1_raw_to_pre() validates and
# normalises a Silvarith-style raw export into the canonical WZP_Daten.txt, and
# import_standwise_relascope_pre_to_fe_inventory() reads that into an
# fe_inventory. import_data_WZP_Relascope_toFeNEU() below is the internal pre ->
# list reader the second stage delegates to (the WZP counterpart of the BaySF
# import_data_BaySF()).
#
# The Silvarith-specific helpers that remain here -- import_standwise_relascope_format1_metadata()
# and import_standwise_relascope_format1_stand_register_bundle() -- serve the Revierbuch (stand_register_pdf) path:
# they load the per-stand metadata a stand register needs, which the generic
# tree chain does not carry.

#' Strict Importer: Relascope WZP Export → FeNEU Intermediate List
#'
#' Reads the \code{WZP_Daten.txt} file -- Rainer Dürr's original export
#' format and, more generally, the vendor-agnostic canonical shape FeNEU
#' expects for angle-count (relascopic, \dQuote{Winkelzählprobe}) stand-wise
#' inventories -- and returns the intermediate inventory list expected by
#' \code{\link{convert_raw_data}} for stand-wise (\code{fe_stand})
#' inventories.
#'
#' This is the internal pre -> list reader of the stand-wise chain, the WZP
#' counterpart of the concentric \code{import_data_BaySF()}: it does only
#' \strong{minor} checks (required file present, required columns present), not
#' the full value-level sanity checks. Those live once, upstream, in
#' \code{\link{import_standwise_relascope_format1_raw_to_pre}} -- the
#' heavy-validation step that turns a Silvarith-style raw export into the clean
#' \code{WZP_Daten.txt} this reader consumes. Reading a \code{WZP_Daten.txt}
#' that never went through that step skips the checks; bad values (e.g. a
#' negative \code{BHD} or an unparseable \code{Stichtag}) then surface later as
#' confusing errors deeper in the pipeline instead of a clear message here.
#' Normally reached through
#' \code{\link{import_standwise_relascope_pre_to_fe_inventory}}, which wraps it.
#'
#' The preprocessed \code{WZP_Daten.txt} shape: default filename
#' \code{WZP_Daten.txt}, ASCII column headers (no umlauts: \code{Hoehe},
#' \code{Best.-Flaeche}), date format \code{DD-MM-YY} in \code{Stichtag}.
#'
#' \strong{Required columns} (after \code{tolower()} of the header):
#' \describe{
#'   \item{stpnr}{Plot number within the stand (integer).}
#'   \item{lfdnrstp}{Sequential record number within the plot (integer);
#'     internally renamed to \code{lfd_nr}.}
#'   \item{bestand}{Stand identifier (character).}
#'   \item{best.-flaeche}{Stand area in ha (numeric); internally renamed to
#'     \code{bestflaeche}.}
#'   \item{zf}{Angle-count factor / Zählfaktor (numeric); internally renamed to
#'     \code{zaehlfaktor}.}
#'   \item{ba}{Species code in BaySF coding (integer); internally renamed to
#'     \code{baumart}.}
#'   \item{alter}{Age in years (integer).}
#'   \item{schicht}{Silvarith layer code, mapped to \code{fe_stand}
#'     \code{layer_key}: 0 = Hauptschicht (main stand) -> 1;
#'     1 = Nachhiebsreste (harvest remnants) -> 4; 2 = Ueberhaelter
#'     (veterans / hold-over trees) -> 4; 3 = Unter-/Zwischenschicht
#'     (understorey / interstorey) -> 2. Silvarith has no code for advance
#'     regeneration (Vorausverjuengung), so \code{layer_key} 3 is never
#'     produced by this importer.}
#'   \item{anz.}{Tree count for this record (integer); internally renamed to
#'     \code{anzahl}.}
#'   \item{bhd}{DBH in cm (numeric).}
#'   \item{hoehe}{Height in m (numeric).}
#'   \item{stichtag}{Survey date in \code{DD-MM-YY} format (character).}
#' }
#' Additional columns in the source file are silently passed through.
#'
#' @param input_path Path of the folder containing the source file (character).
#' @param treelist_filename File name (without \code{.txt} extension).
#'   Default \code{"WZP_Daten"} matches the standard Relascope export name.
#' @param encoding Character encoding of the source file. Default \code{"auto"}
#'   detects it (UTF-8, incl. BOM, vs. Latin-1/Windows-1252); pass an explicit
#'   encoding only to force one.
#'
#' @return A tibble with columns \code{plot_id}, \code{trees} (list-column of
#'   per-plot tree data frames), and \code{area_rep_ha} (ha), ready to be
#'   passed to \code{\link{convert_raw_data}}.
#'
#' @seealso \code{\link{import_standwise_relascope_pre_to_fe_inventory}} (the
#'   public pre -> fe_inventory step that wraps this reader),
#'   \code{\link{import_standwise_relascope_format1_raw_to_pre}} (the
#'   heavy-validation step this reader expects to run after),
#'   \code{\link{convert_raw_data}}
#'
#' @references Rainer Dürr, author of the WZP export format and the
#'   forest management software built around it.
#'
#' @noRd
#'
import_data_WZP_Relascope_toFeNEU <- function(input_path,
                                              treelist_filename = "WZP_Daten",
                                              encoding          = "auto") {

  # --- 1. Minor check: required file present ----------------------------------
  # Heavy validation is import_standwise_relascope_format1_raw_to_pre()'s job (see @description above);
  # this importer only checks the file exists and has the required columns.

  file_path <- paste0(input_path, "/", treelist_filename, ".txt")
  if (!file.exists(file_path)) {
    stop(fe_msg("file_missing", file_path), call. = FALSE)
  }

  # Read every column as text and defer numeric parsing to the whitelist below,
  # so a comma decimal in a Silvarith-normalised WZP_Daten.txt is never eaten as
  # a thousands mark at read time (package-wide decimal-robustness convention).
  raw <- .read_delim_auto(file_path, encoding = encoding, as_text = TRUE)
  names(raw) <- tolower(names(raw))

  # --- 2. Relascope-specific rename → canonical FeNEU WZP names ---------------
  # No umlauts in Relascope headers, so the rename map is shorter than Silvarith's.
  rename_map <- c(
    lfd_nr      = "lfdnrstp",
    zaehlfaktor = "zf",
    bestflaeche = "best.-flaeche",
    baumart     = "ba",
    anzahl      = "anz."
    # `hoehe` and `bhd` already match canonical after tolower().
  )
  for (target in names(rename_map)) {
    src <- rename_map[[target]]
    if (src %in% names(raw)) {
      names(raw)[names(raw) == src] <- target
    }
  }

  # --- 3. Minor check: required canonical columns present ---------------------
  # (.wzp_required_names is shared with import_standwise_relascope_format1_raw_to_pre(), R/import_standwise_relascope_format1_raw_to_pre.R)

  check_rslt <- ForestElementsR::has_required_names(raw, .wzp_required_names)
  if (!check_rslt$rqmt_ok) {
    stop(fe_msg("cols_missing_in_file", check_rslt$err_message, treelist_filename),
         call. = FALSE)
  }

  # --- 4. Parse the consumed numeric columns, tolerant of the decimal mark -----
  # Whitelist parse (.wzp_numeric, R/import_column_standards.R): only the columns
  # this reader consumes are turned numeric; identifiers (stpnr, lfd_nr, bestand)
  # and free-text pass-through columns keep their commas untouched.

  raw <- .parse_numeric_cols(raw, .wzp_numeric)

  # --- 5. Parse Stichtag (needed for erfdat below; no validation here -- that
  #        is import_standwise_relascope_format1_raw_to_pre()'s job) -----------------------------------

  parsed_dates <- as.Date(raw$stichtag, format = "%d-%m-%y")

  # --- 6. Compute WZP-derived values + canonical intermediate columns ---------

  trees <- raw |>
    dplyr::mutate(
      ba_fe            = .data$baumart,
      bhd_h            = .data$hoehe,
      inv_alt          = .data$alter,
      bhd_anz_rech     = .data$anzahl,
      stand_id         = .data$bestand,
      area_ha          = .data$bestflaeche,
      plot_inStand     = .data$stpnr,
      tree_id          = paste(.data$stpnr, .data$lfd_nr, sep = "_"),
      bhd_anz          = 1.0,
      g                = .data$zaehlfaktor * .data$anzahl,
      n_rep            = .data$g / ((.data$bhd / 100)^2 * pi / 4),
      erfdat           = as.numeric(format(parsed_dates, "%Y")),
      plot_id          = paste(.data$stpnr, .data$bestand, sep = "_"),
      grouping_plot_id = .data$plot_id,
      layer_key        = dplyr::case_when(
                           .data$schicht == 0 ~ 1L,  # Hauptschicht -> main stand
                           .data$schicht == 1 ~ 4L,  # Nachhiebsreste -> remnant / veteran layer
                           .data$schicht == 2 ~ 4L,  # Ueberhaelter -> remnant / veteran layer
                           .data$schicht == 3 ~ 2L,  # Unter-/Zwischenschicht -> understorey
                           TRUE               ~ 1L
                         )
    )

  # --- 7. Per-plot representation area ----------------------------------------

  stands <- trees |>
    dplyr::group_by(.data$stand_id) |>
    dplyr::mutate(
      area_stand = unique(.data$area_ha),
      n_points   = dplyr::n_distinct(.data$plot_inStand)
    ) |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::summarise(
      area_rep_ha = unique(.data$area_stand / .data$n_points),
      .groups     = "drop"
    )

  # --- 8. Apply species coding and nest by plot -------------------------------

  trees_l <- trees |>
    dplyr::mutate(
      ba_fe = .data$ba_fe |>
        ForestElementsR::fe_species_bavrn_state()
    ) |>
    dplyr::group_by(.data$grouping_plot_id) |>
    tidyr::nest() |>
    dplyr::rename(trees = "data") |>
    dplyr::ungroup() |>
    dplyr::rename(plot_id = "grouping_plot_id")

  dplyr::left_join(trees_l, stands, by = "plot_id")
}


#' Import Per-Stand Metadata Companion to Format 1 Standwise Inventory Data
#'
#' Reads a per-stand metadata file (default name \code{Inventuraufnahmen.txt})
#' coming with Format 1 standwise inventory data (Silvarith-style) and returns
#' one tibble row per stand with canonical lower-case ASCII column names. This
#' is the metadata companion used by
#' \code{\link{import_standwise_relascope_format1_stand_register_bundle}} and
#' produces a tibble suitable as the \code{stand_data} argument of
#' \code{\link{stand_register_pdf}}.
#'
#' The source file's headers are very long and can contain special characters
#' (\code{Ø}, slashes, parentheses, German umlauts), so columns are matched
#' by \strong{position} rather than by name — the file must have exactly 13
#' columns in the order shown below.
#'
#' \strong{Canonical columns} (in order):
#' \describe{
#'   \item{bestand}{Stand identifier (character, matches the WZP
#'     \code{bestand} column).}
#'   \item{bestandesform}{Stand-form classification (e.g. \code{"Kie-Lä-Bu"}).}
#'   \item{alter_mittel}{Mean stand age in years.}
#'   \item{nutzungsart}{Management type code (e.g. \code{"VJT"}, \code{"LB"}).}
#'   \item{bemerkung}{Free-text remark (may be empty).}
#'   \item{bestflaeche}{Stand area in ha.}
#'   \item{bestockungsgrad}{Stocking degree (BG, dimensionless).}
#'   \item{vorrat_efm_ha}{Standing volume in efm D.o.R. per ha.}
#'   \item{aufnahmeart}{Survey type: 1 = VK, 2 = WZP, 4 = fest. PK,
#'     5 = konz. PK.}
#'   \item{zaehlbreite}{Angle-count factor or max. circle area, as a string
#'     (e.g. \code{"ZB 4,0"}).}
#'   \item{n_stichproben}{Number of sample plots in the stand.}
#'   \item{stp_pro_ha}{Plot density (STP / ha).}
#'   \item{stdfehler_pct}{Sampling error of standing volume in percent
#'     (numeric, parsed from strings like \code{"24,4\%"}).}
#' }
#'
#' @param input_path Path of the folder containing the source file (character).
#' @param filename File name (without \code{.txt} extension).  Default
#'   \code{"Inventuraufnahmen"}.
#' @param encoding Character encoding.  Default \code{"auto"} (detected).
#'
#' @return A tibble with one row per stand and the 13 canonical columns
#'   described above.
#'
#' @seealso \code{\link{import_standwise_relascope_format1_stand_register_bundle}},
#'   \code{\link{stand_register_pdf}}
#'
#' @keywords internal
#'
#'
import_standwise_relascope_format1_metadata <- function(input_path,
                                             filename = "Inventuraufnahmen",
                                             encoding = "auto") {

  # Coarse progress phases (progressr); shown only inside
  # progressr::with_progress(). The readr read below is already quiet
  # (show_col_types = FALSE).
  p <- progressr::progressor(steps = 2L)

  raw <- .read_delim_auto(
    paste0(input_path, "/", filename, ".txt"),
    encoding     = encoding,
    decimal_mark = ","
  )

  # Strict column-count check (rename by position because the original
  # headers are too messy to match by name).
  if (ncol(raw) != 13) {
    stop(
      "Inventuraufnahmen file must have exactly 13 columns, got ", ncol(raw),
      ". Expected the standard Silvarith Beta export format.",
      call. = FALSE
    )
  }

  names(raw) <- c(
    "bestand", "bestandesform", "alter_mittel", "nutzungsart", "bemerkung",
    "bestflaeche", "bestockungsgrad", "vorrat_efm_ha", "aufnahmeart",
    "zaehlbreite", "n_stichproben", "stp_pro_ha", "stdfehler_pct"
  )
  p(message = fe_msg("progress_read"))

  # Type coercion: Silvarith's "24,4%" → numeric 24.4
  raw <- raw |>
    dplyr::mutate(
      bestand         = as.character(.data$bestand),
      bestandesform   = as.character(.data$bestandesform),
      nutzungsart     = as.character(.data$nutzungsart),
      bemerkung       = as.character(.data$bemerkung),
      zaehlbreite     = as.character(.data$zaehlbreite),
      alter_mittel    = suppressWarnings(as.numeric(.data$alter_mittel)),
      bestflaeche     = suppressWarnings(as.numeric(.data$bestflaeche)),
      bestockungsgrad = suppressWarnings(as.numeric(.data$bestockungsgrad)),
      vorrat_efm_ha   = suppressWarnings(as.numeric(.data$vorrat_efm_ha)),
      aufnahmeart     = suppressWarnings(as.integer(.data$aufnahmeart)),
      n_stichproben   = suppressWarnings(as.integer(.data$n_stichproben)),
      stp_pro_ha      = suppressWarnings(as.numeric(.data$stp_pro_ha)),
      stdfehler_pct   = suppressWarnings(as.numeric(
        sub(",", ".", sub("%$", "", .data$stdfehler_pct))
      ))
    )

  # Sanity checks
  if (any(duplicated(raw$bestand))) {
    stop(fe_msg("stand_id_duplicate"), call. = FALSE)
  }
  if (any(!is.na(raw$bestflaeche) & raw$bestflaeche <= 0)) {
    stop(fe_msg("bestflaeche_not_positive"), call. = FALSE)
  }
  if (!all(raw$aufnahmeart %in% c(1L, 2L, 4L, 5L, NA))) {
    stop(fe_msg("aufnahmeart_invalid"), call. = FALSE)
  }
  p(message = fe_msg("progress_process"))

  raw
}


#' Import a Standwise Format 1 Data Bundle (Trees + Stand Metadata) Required
#' For a Stand Register (Revierbuch)
#'
#' Convenience orchestrator that reads both, a raw standwise Format 1 input file,
#' and its metadata companion (both Silvarith-style) for one forest management
#' unit from a single folder and returns the two artefacts needed for a
#' \code{\link{stand_register_pdf}} call:
#' \itemize{
#'   \item \code{fe_inv} — an \code{fe_inventory} of \code{fe_stand} objects,
#'     built from the Silvarith-style trees export via the two-stage relascope
#'     import chain (\code{\link{import_standwise_relascope_format1_raw_to_pre}}
#'     + \code{\link{import_standwise_relascope_pre_to_fe_inventory}}).
#'   \item \code{stand_data} — per-stand metadata read from
#'     \code{Inventuraufnahmen.txt} via
#'     \code{\link{import_standwise_relascope_format1_metadata}}.
#' }
#'
#' Cross-checks that every stand referenced in the inventory has a
#' corresponding row in \code{stand_data} and emits a warning if any are
#' missing (the inventory data is still returned in that case).
#'
#' @param input_path Path of the folder containing both source files
#'   (character).
#' @param treelist_filename File name of the trees export (without
#'   \code{.txt}).  Default \code{"EingabedatenGesamt"}.
#' @param inventuraufnahmen_filename File name of the metadata export
#'   (without \code{.txt}).  Default \code{"Inventuraufnahmen"}.
#' @param encoding Character encoding for both files. Default \code{"auto"}
#'   detects it per file (UTF-8, incl. BOM, vs. Latin-1/Windows-1252); pass an
#'   explicit encoding only to force one.
#'
#' @return A named list with elements \code{fe_inv} (an
#'   \code{fe_inventory}) and \code{stand_data} (a tibble).
#'
#' @seealso \code{\link{import_standwise_relascope_format1_metadata}},
#'   \code{\link{import_standwise_relascope_format1_raw_to_pre}},
#'   \code{\link{stand_register_pdf}}
#'
#'
#' @keywords internal
#'
import_standwise_relascope_format1_stand_register_bundle <- function(input_path,
                                  treelist_filename          = "EingabedatenGesamt",
                                  inventuraufnahmen_filename = "Inventuraufnahmen",
                                  encoding                   = "auto") {

  # Progress reporting and quiet readr reads are handled by the delegated
  # functions this orchestrator calls (the two-stage relascope chain and the
  # metadata reader), so no progressor is created here -- that would compete
  # with the children's own progress.

  # Trees run through the two-stage relascope chain: validate + normalise the
  # Silvarith-style raw export into a preprocessed WZP_Daten.txt, then read that
  # into an fe_inventory of fe_stand plots.
  pre_dir <- tempfile("wzp_pre_")
  dir.create(pre_dir)
  conv <- import_standwise_relascope_format1_raw_to_pre(
    input_path        = input_path,
    output_dir        = pre_dir,
    treelist_filename = treelist_filename,
    encoding          = encoding
  )
  if (!conv$ok) {
    stop(
      "Silvarith-style raw import failed:\n",
      paste0("- ", conv$errors, collapse = "\n"),
      call. = FALSE
    )
  }
  if (length(conv$warnings) > 0) {
    warning(paste(conv$warnings, collapse = "\n"), call. = FALSE)
  }

  fe_inv <- import_standwise_relascope_pre_to_fe_inventory(pre_dir)

  stand_data <- import_standwise_relascope_format1_metadata(
    input_path = input_path,
    filename   = inventuraufnahmen_filename,
    encoding   = encoding
  )

  # Cross-check: every stand in the inventory should have a metadata row.
  # plot_id format from the WZP import is "<stpnr>_<bestand>".
  stand_ids_inv <- unique(sub("^[^_]+_", "", fe_inv$plot_id))
  missing_meta  <- setdiff(stand_ids_inv, stand_data$bestand)
  if (length(missing_meta) > 0) {
    warning(
      "Stand(s) with inventory data but no metadata in Inventuraufnahmen: ",
      paste(missing_meta, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    fe_inv     = fe_inv,
    stand_data = stand_data
  )
}

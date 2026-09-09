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

# Required (ASCII, lower-case) column names of the preprocessed WZP_Daten.txt.
# Shared with the pre -> fe_inventory reader
# (import_standwise_relascope_pre_to_fe_inventory, R/import_functions_relascope.R),
# so both ends of the stand-wise chain agree on the column contract.
.wzp_required_names <- c(
  "stpnr", "lfd_nr", "bestand", "bestflaeche", "zaehlfaktor",
  "baumart", "alter", "schicht", "anzahl", "bhd", "hoehe", "stichtag"
)

#' Import a Stand-Wise Angle-Count Inventory: Silvarith-style Raw to Preprocessed
#'
#' The raw-to-pre step of the stand-wise relascope
#' (\dQuote{Winkelzählprobe} / angle-count) import chain. Reads a
#' Silvarith-style raw tree file (\code{EingabedatenGesamt.txt}: German locale
#' -- decimal comma, umlaut headers, \code{DD.MM.YYYY} dates), validates it
#' (collecting \emph{every} problem rather than stopping at the first), and --
#' if valid -- writes the preprocessed \code{WZP_Daten.txt} (the canonical
#' ASCII / decimal-point form) that
#' \code{\link{import_standwise_relascope_pre_to_fe_inventory}} reads.
#'
#' This is the direct counterpart to
#' \code{\link{import_sample_concentric_format1_raw_to_pre}} for concentric
#' sample inventories: a single top-level converter that performs its own
#' validation (\dQuote{plausi}) internally and returns a collected report. There
#' is no separately exported plausi function.
#'
#' \dQuote{Silvarith-style} describes the \emph{shape} of the data. Silvarith is
#' a stand-wise angle-count software; this importer targets its export layout
#' without claiming exact or official compatibility. A user who already holds a
#' \code{WZP_Daten.txt} -- Silvarith also produces one directly, or it was made
#' earlier by this function -- skips this step and goes straight to
#' \code{\link{import_standwise_relascope_pre_to_fe_inventory}}.
#'
#' \strong{Required columns} (after \code{tolower()} of the header):
#' \describe{
#'   \item{stpnr}{Plot number within the stand (integer).}
#'   \item{lfdnrstp}{Sequential record number within the plot; renamed to
#'     \code{lfd_nr}.}
#'   \item{bestand}{Stand identifier (character).}
#'   \item{best.-fläche}{Stand area in ha (decimal comma); renamed to
#'     \code{bestflaeche}.}
#'   \item{zf}{Angle-count factor; renamed to \code{zaehlfaktor}.}
#'   \item{ba}{Species code in BaySF coding; renamed to \code{baumart}.}
#'   \item{alter}{Age in years (integer).}
#'   \item{schicht}{Silvarith layer code in \{0, 1, 2, 3\}: 0 = main stand
#'     (Hauptschicht), 1 = harvest remnants (Nachhiebsreste), 2 = veterans
#'     (Ueberhaelter) -- 1 and 2 both fall into the remnant/veteran layer --
#'     3 = understorey (Unter-/Zwischenschicht). Mapped to \code{fe_stand}
#'     \code{layer_key} in
#'     \code{\link{import_standwise_relascope_pre_to_fe_inventory}}; Silvarith
#'     has no code for advance regeneration.}
#'   \item{anz.}{Tree count for this record; renamed to \code{anzahl}.}
#'   \item{bhd}{DBH in cm (decimal comma).}
#'   \item{höhe}{Height in m (decimal comma); renamed to \code{hoehe}.}
#'   \item{stichtag}{Survey date in \code{DD.MM.YYYY} format.}
#' }
#'
#' \strong{Collected checks:} \code{bhd} non-negative; \code{schicht} in
#' \{0, 1, 2, 3\}; \code{stichtag} parseable as \code{\%d.\%m.\%Y};
#' \code{hoehe > 50 m} raises a (non-blocking) warning.
#'
#' @details
#' No height completion is performed here, and missing heights are not rejected:
#' measured tree heights are carried through as given (missing ones stay
#' \code{NA}). Estimating the missing heights is a deliberate, separate
#' downstream step --
#' \code{pull_trees() |> height_complete_inventory() |> fill_heights_back()} --
#' so that measured and estimated heights are never silently mixed. (Angle-count
#' surveys usually measure a height for every tallied tree, so missing heights
#' are uncommon here, but they are tolerated.)
#'
#' @param input_path Folder holding the raw file (character).
#' @param output_dir Folder the preprocessed \code{WZP_Daten.txt} is written to
#'   (character, required). There is deliberately no default: the function
#'   writes files, and a default would write into the folder the raw data came
#'   from, i.e. into the user's own filespace. Created if it does not exist.
#'   Untouched if validation fails.
#' @param treelist_filename Raw file name, with or without the \code{.txt}. Default
#'   \code{"EingabedatenGesamt"}, the standard Silvarith export name.
#' @param encoding Character encoding of the source file. Default \code{"auto"}
#'   detects it (UTF-8, incl. BOM, vs. Latin-1/Windows-1252); pass an explicit
#'   encoding only to force one.
#'
#' @return A named list (the common raw-to-pre report):
#' \describe{
#'   \item{ok}{Logical. \code{TRUE} if the file passed validation and was
#'     written.}
#'   \item{errors}{Character vector of validation problems; empty if \code{ok}.}
#'   \item{warnings}{Character vector of non-blocking notices; may be non-empty
#'     even when \code{ok}.}
#'   \item{paths}{Character vector of written files, or \code{NULL} if
#'     \code{!ok}.}
#' }
#'
#' @seealso \code{\link{import_standwise_relascope_pre_to_fe_inventory}},
#'   \code{\link{import_sample_concentric_format1_raw_to_pre}}
#'
#' @export
#'
#' @examples
#' # The bundled example uses the canonical Silvarith export name
#' # "EingabedatenGesamt.txt" (the default of `treelist_filename`).
#' sil_dir <- system.file("extdata", "data_ex6_standwise_raw", package = "FeNEU")
#' out_dir <- tempdir()
#' result  <- import_standwise_relascope_format1_raw_to_pre(
#'   sil_dir, output_dir = out_dir
#' )
#' result$ok
#' result$paths
#'
import_standwise_relascope_format1_raw_to_pre <- function(input_path,
                                                          output_dir,
                                                          treelist_filename = "EingabedatenGesamt.txt",
                                                          encoding = "auto") {

  # output_dir has no default on purpose: this function writes files, and a
  # default would write into the user's own filespace (CRAN policy; and the
  # package never guesses silently). missing() cannot be seen from inside the
  # helper, so it is folded into NULL here and reported there.
  if (missing(output_dir)) output_dir <- NULL
  .assert_output_dir(output_dir, "import_standwise_relascope_format1_raw_to_pre")

  file_path <- paste0(input_path, "/", .ensure_txt(treelist_filename))

  errors   <- character()
  warnings <- character()

  # Coarse progress phases (progressr); shown only inside
  # progressr::with_progress(). The single readr read below is already quiet
  # (show_col_types = FALSE), so no readr option needs toggling here.
  p <- progressr::progressor(steps = 3L)

  # --- 1. Read the Silvarith-style raw as text --------------------------------
  # Read every column as text and parse only the consumed numeric columns (via
  # the .wzp_numeric whitelist, step 3b), so a comma decimal ("39,5") from this
  # German-locale export is never eaten as a thousands grouping mark at read
  # time (package-wide decimal-robustness convention). Encoding is auto-detected.

  raw <- tryCatch(
    .read_delim_auto(file_path, encoding = encoding, as_text = TRUE),
    error = function(e) e
  )
  if (inherits(raw, "error")) {
    return(list(ok = FALSE,
                errors   = paste0("Could not read file: ", conditionMessage(raw)),
                warnings = character(), paths = NULL))
  }

  # --- 2. Silvarith umlaut headers -> canonical FeNEU WZP names ---------------

  names(raw) <- tolower(names(raw))
  rename_map <- c(
    lfd_nr      = "lfdnrstp",
    zaehlfaktor = "zf",
    bestflaeche = "best.-fl\u00e4che", # best.-flaeche
    baumart     = "ba",
    anzahl      = "anz.",
    hoehe       = "h\u00f6he"           # hoehe
  )
  for (target in names(rename_map)) {
    src <- rename_map[[target]]
    if (src %in% names(raw)) names(raw)[names(raw) == src] <- target
  }
  p(message = fe_msg("progress_read"))

  # --- 3. Validate, collecting every problem instead of stopping first --------

  missing_cols <- setdiff(.wzp_required_names, names(raw))
  if (length(missing_cols) > 0) {
    return(list(ok = FALSE,
                errors   = paste0("Missing required column(s): ",
                                  paste(missing_cols, collapse = ", ")),
                warnings = character(), paths = NULL))
  }

  # 3b. Parse the consumed numeric columns tolerant of the decimal mark before
  # validating and writing (.wzp_numeric, R/import_column_standards.R). Both the
  # value checks below and the canonical output rely on numeric, point-decimal
  # values; identifiers (stpnr, lfd_nr, bestand) and stichtag stay as read.
  raw <- .parse_numeric_cols(raw, .wzp_numeric)

  bhd <- suppressWarnings(as.numeric(raw$bhd))
  if (any(!is.na(bhd) & bhd < 0)) {
    errors <- c(errors, sprintf("bhd column contains %d negative value(s).",
                                sum(!is.na(bhd) & bhd < 0)))
  }

  schicht     <- suppressWarnings(as.integer(raw$schicht))
  bad_schicht <- !is.na(schicht) & !schicht %in% c(0L, 1L, 2L, 3L)
  if (any(bad_schicht)) {
    errors <- c(errors, sprintf(
      "schicht column contains %d value(s) outside {0, 1, 2, 3}.",
      sum(bad_schicht)))
  }

  parsed_dates <- suppressWarnings(
    as.Date(as.character(raw$stichtag), format = "%d.%m.%Y"))
  bad_dates <- is.na(parsed_dates) & !is.na(raw$stichtag) &
    nzchar(as.character(raw$stichtag))
  if (any(bad_dates)) {
    errors <- c(errors, sprintf(
      "stichtag column contains %d unparseable date(s) for format '%s'.",
      sum(bad_dates), "%d.%m.%Y"))
  }

  hoehe <- suppressWarnings(as.numeric(raw$hoehe))
  if (any(!is.na(hoehe) & hoehe > 50)) {
    warnings <- c(warnings, sprintf(
      "%d tree(s) have hoehe > 50 m. Please verify they are correct.",
      sum(!is.na(hoehe) & hoehe > 50)))
  }

  if (length(errors) > 0) {
    return(list(ok = FALSE, errors = errors, warnings = warnings, paths = NULL))
  }
  p(message = fe_msg("progress_validate"))

  # --- 4. Write the preprocessed WZP_Daten.txt (ASCII, decimal point) ---------
  # Numeric columns were already parsed to point-decimal numeric in step 3b.

  canonical <- data.frame(
    STPNr           = raw$stpnr,
    lfdNrSTP        = raw$lfd_nr,
    Bestand         = raw$bestand,
    `Best.-Flaeche` = raw$bestflaeche,
    ZF              = raw$zaehlfaktor,
    BA              = raw$baumart,
    Alter           = raw$alter,
    Schicht         = raw$schicht,
    `ANZ.`          = raw$anzahl,
    BHD             = raw$bhd,
    Hoehe           = raw$hoehe,
    Stichtag        = format(parsed_dates, "%d-%m-%y"),
    check.names     = FALSE
  )

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  path <- file.path(output_dir, "WZP_Daten.txt")
  utils::write.table(canonical, path, sep = "\t", row.names = FALSE,
                     quote = FALSE, na = "", dec = ".")
  p(message = fe_msg("progress_write"))

  list(ok = TRUE, errors = character(), warnings = warnings, paths = path)
}

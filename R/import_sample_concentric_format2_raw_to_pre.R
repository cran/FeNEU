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

#' Convert raw sample inventory data (format 2) to preprocessed BaySF style data
#'
#' Reads format 2 raw sample inventory data collected with the concentric circle
#' method and writes the standard preprocessed BaySF-style intermediate files
#' (\code{fdinvbhd.txt}, \code{fdinvba.txt}, \code{fdinvkrs.txt},
#' \code{fcbestku.txt}, \code{fdvikrs.txt}) into \code{output_dir}.
#'
#' Format 2 defines \strong{nine} source tables in total. This converter
#' currently supports only the narrow core -- two tables: the main tree list
#' (02) and the inventory-point / coordinate table (01). Importers for the
#' remaining tables are \strong{under construction}: the tree-property data
#' (03), regeneration incl. height classes (04-06, 08), and dead wood (07, 09)
#' are not read yet. In particular, Format 2 has \strong{its own dedicated
#' small-tree / regeneration format}, for which a reader will be added in the
#' future; until then no regeneration / small-tree records are produced, so this
#' converter never yields the \code{bhd == 0} tally rows that the BaySF-style
#' pre-reader would otherwise split off into small trees.
#'
#' @section Format 1 and Format 2:
#' FeNEU currently supports two raw data formats for sample inventories with
#' concentric circles. The two grew up in parallel with FeNEU itself and have no
#' fundamental differences in content -- they simply reflect different user
#' preferences for how the data are exported. We call them \dQuote{Format 1} and
#' \dQuote{Format 2}. This function reads \strong{Format 2}; Format 1 is read by
#' \code{\link{import_sample_concentric_format1_raw_to_pre}}. Both produce the
#' same BaySF-style preprocessed files, so everything downstream is identical.
#'
#' @section Stand-layer coding (\code{bestku}):
#' Format 2 codes the stand layer in the tree-list column \code{bestku}
#' \strong{differently} from the BaySF-style pre format and from Format 1. The
#' converter therefore \strong{remaps} the Format-2 codes
#' onto the BaySF layer codes that the rest of the pipeline (the \code{fcbestku}
#' table and the internal \code{layer_key}) expects:
#' \tabular{lll}{
#'   \strong{Format 2 (\code{bestku})} \tab \strong{->} \tab \strong{BaySF code} \cr
#'   1 Hauptschicht                 \tab -> \tab 1 Oberschicht (main stand) \cr
#'   2 Zwischenschicht              \tab -> \tab 2 Unterstand \cr
#'   3 Unterschicht                 \tab -> \tab 2 Unterstand \cr
#'   4 Vorausverjuengung            \tab -> \tab 3 Vorausverjuengung \cr
#'   5 Ueberhaelter/Nachhiebsrest   \tab -> \tab 5 Ueberhaelter \cr
#' }
#' Note in particular that the same number means different things in the two
#' codings (e.g. Format-2 \code{bestku = 4} is advance regeneration, whereas
#' BaySF/Format-1 code 4 is residual stand). The main stand (code 1) is
#' identical in both. Format 2's two sub-canopy layers (Zwischenschicht,
#' Unterschicht) are both mapped to the single BaySF Unterstand, and Format 2's
#' combined Ueberhaelter/Nachhiebsrest maps to Ueberhaelter.
#'
#' @details
#' In its raw encoding, Format 2 records DBH in mm and heights in dm, the tree
#' position as a back azimuth given in gon or degrees (disambiguated by a unit
#' flag) plus a distance in cm, its own stand-layer code (remapped to BaySF
#' codes, see above), and living trees only. Format 1 encodes the same
#' information differently (cm and m, a single degree azimuth, BaySF layer codes
#' directly, living and dead trees); see
#' \code{\link{import_sample_concentric_format1_raw_to_pre}}.
#'
#' \strong{Stand age.} Age in forest inventories always refers to the stand or
#' stand layer, never to the individual tree, even though it is recorded
#' redundantly in every tree row. It is \strong{optional}: in structure-rich,
#' uneven-aged stands -- increasingly important in practice -- a meaningful stand
#' age often cannot be given. When the age is missing or incomplete, the analyses
#' that group by \strong{age class} are unavailable: base tables and structure
#' tables by age class, and the age-class breakdown of increment from a repeated
#' inventory -- all of these can be run by \strong{mean-diameter (dq) class}
#' instead. Increment from yield tables and from the BWI3 (gnfi3) functions
#' requires the age and is not possible without it. A warning is issued on import
#' whenever the age is absent or has gaps, so an accidental omission can be
#' noticed and corrected; users whose forest structure genuinely precludes a
#' stand age can ignore it.
#'
#' No height completion is performed here. Measured tree heights are carried
#' through as given (missing heights stay \code{NA}); the converter only records
#' which heights were measured (internal \code{hmb} flag) and normalises their
#' unit where the raw format requires it (Format 2 stores heights in dm).
#' Estimating the missing heights is a deliberate, separate downstream step --
#' \code{pull_trees() |> height_complete_inventory() |> fill_heights_back()} --
#' so that measured and estimated heights are never silently mixed.
#'
#' @param input_path Path to the folder holding the source files (character).
#' @param output_dir Folder the preprocessed BaySF-style files are written to
#'   (character, required). There is deliberately no default: the function
#'   writes files, and a default would write into the folder the raw data came
#'   from, i.e. into the user's own filespace. Created if it does not exist.
#'   Untouched if validation fails.
#' @param treelist_filename Name of the tree-list file (Format 2 source table 02,
#'   with or without the \code{.txt} extension). Default \code{"02_probekreis.txt"}
#'   (the format's standard file name).
#'
#'   \strong{Required columns:}
#'   \describe{
#'     \item{\code{koord}}{Plot identifier (integer). Must match \code{koord} in
#'       the inventory-point file exactly.}
#'     \item{\code{ba_fe}}{Species code (BaySF \code{bavrn_state} integer coding).}
#'     \item{\code{bhd}}{Diameter at breast height, in \strong{mm} (cm × 10);
#'       converted to cm internally.}
#'     \item{\code{hoehe_1}}{Total tree height, in \strong{dm}; converted to m
#'       internally (\code{NA} if not measured).}
#'     \item{\code{pol_entf}}{Horizontal distance from plot centre, in
#'       \strong{cm}.}
#'     \item{\code{pol_wink}, \code{rueckazimut_gon}, \code{rueckazimut_grad},
#'       \code{einheit_azimut}}{Azimuth of the tree. \code{pol_wink} (degrees) is
#'       used if present; otherwise the back azimuth is read from the column that
#'       \code{einheit_azimut} declares -- \code{1} = \code{rueckazimut_gon}
#'       (gon, converted × 0.9 to degrees), \code{2} = \code{rueckazimut_grad}
#'       (degrees). \code{einheit_azimut} is authoritative: if it does not match
#'       the populated column, the tree is treated as having \emph{no} usable
#'       azimuth rather than reading a value in the wrong unit. Trees outside the
#'       innermost circle that end up without a usable position are excluded from
#'       the output, with a prominent warning in the report. An innermost-circle
#'       tree may legitimately carry no azimuth.}
#'     \item{\code{bestku}}{Stand-layer code in the \strong{Format-2}
#'       coding -- \emph{not} the BaySF coding; it is remapped internally. See
#'       the \dQuote{Stand-layer coding} section below.}
#'   }
#'
#'   \strong{Optional columns:}
#'   \describe{
#'     \item{\code{alter_ba}}{\strong{Stand} (or stand-layer) age in years --
#'       never an individual-tree age, even though it is stored redundantly per
#'       tree row. Used if present. Missing or incomplete age limits which
#'       analyses are possible and triggers a warning on import -- see the
#'       \dQuote{Stand age} section in Details.}
#'     \item{\code{baumnummer}}{A tree number within the plot. It may be supplied
#'       but is \strong{not processed further}: tree identification -- in
#'       particular when linking two successive inventories -- rests solely on the
#'       coordinates, and trees without coordinates (the innermost circle) are
#'       treated as unmatchable, so no tree number is ever consumed.}
#'   }
#'
#'   Any further columns are ignored -- extra columns do not disturb the import.
#'   Format 2 records only living trees (no mortality column); mortality is set
#'   to 0 for every record.
#' @param inv_punkt_filename Name of the inventory-point file (Format 2 source
#'   table 01, with or without the \code{.txt} extension). Default
#'   \code{"01_root_entity.txt"} (the format's standard file name).
#'
#'   \strong{Required columns:}
#'   \describe{
#'     \item{\code{koord}}{Plot key linking the point to its trees.}
#'     \item{\code{lage_probekreismittelpunkt_x} / \code{lage_probekreismittelpunkt_y}}{
#'       Plot-centre coordinates. Their coordinate reference system is declared
#'       either by the file's own \strong{optional}
#'       \code{lage_probekreismittelpunkt_srs} column (an EPSG declaration such as
#'       \code{"EPSG:4326"}, as written by the native export) or by the
#'       \code{coord_sys} argument -- see there.}
#'     \item{\code{repfl}}{Represented area (ha).}
#'   }
#'
#'   \strong{Optional columns:}
#'   \describe{
#'     \item{\code{date_created}}{Survey date, in any format parseable by
#'       \code{\link{detect_date_format}}. Only its \emph{year} is used (see
#'       \code{inventory_year}); the day and month are discarded. If
#'       \code{inventory_year} is given it wins and this column is not read; if
#'       it is absent, either \code{date_created} or \code{inventory_year} must
#'       be present.}
#'     \item{\code{lage_probekreismittelpunkt_srs}}{CRS declaration; see
#'       \code{coord_sys}.}
#'     \item{\code{neigung} (or \code{slope})}{Terrain slope, in \strong{percent}.
#'       Format 2's multi-table source usually carries no slope; when the column
#'       is absent the slope defaults to 0 (level ground, no horizontal-projection
#'       area correction of the concentric circles) and a warning is issued.
#'       Supply \code{neigung} only if the plots actually lie on a slope.}
#'   }
#'   Any further columns are ignored -- extra columns do not disturb the import.
#' @param circle_def_filename Name of the circle-definition file (with or
#'   without the \code{.txt} extension), or \code{NULL} (default). An
#'   \code{fe_ikl}-keyed \strong{Probekreisdefinition} file with columns
#'   \code{fe_ikl}, \code{dbh_cm_from} (lower DBH limit, cm) and
#'   \code{radiuses_m} (circle radius, m); use \code{fe_ikl = "ALL"} for one
#'   uniform definition. Create a uniform one with
#'   \code{\link{generate_circle_definition}}. Supply \strong{either} this file
#'   \strong{or} \code{dbh_cm_from}/\code{radiuses_m}, never both.
#'
#'   In practice Format 2 runs \strong{uniform}: its source tables carry no
#'   circle-class key (the inventory-point table has no \code{fe_ikl} column), so
#'   one circle definition applies to every plot -- give it via
#'   \code{dbh_cm_from}/\code{radiuses_m} or an \code{fe_ikl = "ALL"} file. The
#'   per-class (multi-\code{fe_ikl}) path exists only for the shared machinery
#'   and is reached only if a user deliberately adds an \code{fe_ikl} column to
#'   the inventory-point file.
#' @param species_guess Logical. Currently unused; reserved for future
#'   species-guessing support. Default is \code{FALSE}.
#' @param dbh_cm_from Numeric vector of lower DBH limits (cm) for each concentric
#'   circle, or \code{NULL}. Alternative to \code{circle_def_filename}; must have
#'   the same length as \code{radiuses_m}. A circle definition (file or these
#'   arguments) is required.
#' @param radiuses_m Numeric vector of circle radii (m), or \code{NULL}. Same
#'   length as \code{dbh_cm_from}.
#'
#' @param coord_sys Coordinate reference system of the plot-centre coordinates,
#'   or \code{NULL} (default) to take it from the point file's own optional
#'   \code{lage_probekreismittelpunkt_srs} column. One of \code{"lonlat"}
#'   (EPSG:4326), \code{"etrs89"} (4258), \code{"utm32"}/\code{"utm33"} (25832,
#'   25833), \code{"gk2"}-\code{"gk5"} (31466-31469).
#'
#'   The native Format-2 export stores \strong{geographic} coordinates and
#'   declares them (\code{srs = "EPSG:4326"}), so it normally needs no argument
#'   here. Nothing is guessed, though: if neither the column nor this argument
#'   states the system, the import fails and says so; if both do and they
#'   disagree, it fails and names both; and if the declaration contradicts the
#'   magnitude of the coordinates, it fails with a concrete suggestion. (Real
#'   exports do contain unusable \code{srs} values such as \code{"GUESS!!"};
#'   those count as no declaration.)
#'
#'   The preprocessed \code{fdinvkrs.txt} stores the plot centres in a
#'   \strong{projected metric} CRS -- the \code{gauss_rw}/\code{gauss_hw} columns
#'   carry UTM/Gauss-Krueger by convention, as real BaySF exports do. Geographic
#'   input (\code{lonlat}/\code{etrs89}) is therefore reprojected to UTM32
#'   (EPSG:25832) here, when the pre is written; already-projected input
#'   (\code{gk*}, \code{utm*}) is kept as is. The CRS of the written coordinates
#'   is recorded in the \code{coord_sys} column, so the pre reader picks it up
#'   without a second declaration.
#' @param inventory_year Optional single survey year (e.g. \code{2025}). It
#'   overrides any date in the point file and is then the year for every plot;
#'   the file's \code{date_created} is not parsed. If omitted, the year is taken
#'   as the plain calendar year of each plot's \code{date_created}. If neither is
#'   available the import stops and asks for \code{inventory_year}.
#'
#'   Downstream only the year is kept (each plot object carries a single
#'   \code{time_yr}). Set \code{inventory_year} deliberately by the growing season
#'   the survey belongs to: a survey done in, e.g., April counts for the previous
#'   season, since almost no growth has happened yet. This choice matters most
#'   when the increment between two successive inventories is computed later --
#'   the period length is the difference of the two inventory years, so an
#'   off-by-one year biases the whole increment rate. The native Format-2
#'   \code{date_created} is the ambiguous \code{DD-MM-YY}; for such data passing
#'   \code{inventory_year} explicitly is the recommended path.
#' @param encoding Character encoding of the source \code{.txt} files.
#'   Default \code{"auto"} detects the encoding per file (UTF-8, incl. BOM, vs.
#'   Latin-1/Windows-1252); pass an explicit encoding only to force one.
#'
#' @return A named list (the common raw-to-pre report): \code{ok} (logical),
#'   \code{errors} (character), \code{warnings} (character), and \code{paths}
#'   (the written files, or \code{NULL} if \code{!ok}). On success it writes
#'   \code{fdinvbhd.txt}, \code{fdinvba.txt}, \code{fdinvkrs.txt},
#'   \code{fcbestku.txt}, and \code{fdvikrs.txt} into \code{output_dir}.
#'
#' @export
#'
#' @examples
#' # Use example raw input data shipped with the package
#' # and write them to a temporary folder under the default
#' # file names, so the converter can be called with its defaults.
#'
#' td <- tempdir()
#'
#' readr::write_delim(
#'   data_ex2_sample_raw_trees,
#'   file = file.path(td, "02_probekreis.txt"),
#'   delim = "\t"
#' )
#'
#' readr::write_delim(
#'   data_ex2_sample_raw_points,
#'   file = file.path(td, "01_root_entity.txt"),
#'   delim = "\t"
#' )
#'
#' # Definition of concentric sample circles
#' dbh_cm_from <- c(0, 12, 30)
#' radiuses_m  <- c(2., 6.31, 12.62)
#'
#' # Create BaySF-style intermediate files in a separate temporary folder
#' # (function is called for its side effects; the file names default to the
#' # format's standard names "02_probekreis.txt" / "01_root_entity.txt").
#' # output_dir is mandatory -- the converter never picks a target itself.
#' # The example points carry their own srs (EPSG:4326), so no coord_sys is
#' # needed; inventory_year is passed because the native date_created is the
#' # ambiguous DD-MM-YY.
#' out_dir <- file.path(tempdir(), "pre_ex2")
#' import_sample_concentric_format2_raw_to_pre(
#'   input_path = td,
#'   output_dir = out_dir,
#'   dbh_cm_from = dbh_cm_from,
#'   radiuses_m = radiuses_m,
#'   inventory_year = 2025
#' )
#'
#' # Check that one of the output files was created
#' file.exists(file.path(out_dir, "fdinvbhd.txt"))
#'
import_sample_concentric_format2_raw_to_pre <- function(input_path,
                                           output_dir,
                                           treelist_filename   = "02_probekreis.txt",
                                           inv_punkt_filename  = "01_root_entity.txt",
                                           circle_def_filename = NULL,
                                           species_guess       = FALSE,
                                           dbh_cm_from         = NULL,
                                           radiuses_m          = NULL,
                                           coord_sys           = NULL,
                                           inventory_year      = NULL,
                                           encoding            = "auto") {

  # output_dir has no default on purpose: this function writes files, and a
  # default would write into the user's own filespace (CRAN policy; and the
  # package never guesses silently). missing() cannot be seen from inside the
  # helper, so it is folded into NULL here and reported there.
  if (missing(output_dir)) output_dir <- NULL
  .assert_output_dir(output_dir, "import_sample_concentric_format2_raw_to_pre")

  warnings <- character()

  # Coarse progress phases (progressr) + quiet readr column-spec messages,
  # restored on exit. Progress is shown only inside progressr::with_progress().
  old_opt <- options(readr.show_col_types = FALSE)
  on.exit(options(old_opt), add = TRUE)
  p <- progressr::progressor(steps = 4L)

  # --- 1. Validate circle arguments -----------------------------------------

  if (!is.null(dbh_cm_from) && !is.null(radiuses_m)) {
    if (length(dbh_cm_from) != length(radiuses_m)) {
      return(.raw_to_pre_fail(fe_msg("circle_args_len_mismatch"), warnings))
    }
  }

  # --- 2. Read tree list -----------------------------------------------------

  # Read as text and defer numeric parsing to the whitelist
  # (.parse_numeric_cols further down); this keeps comma decimals intact (see
  # .read_delim_auto's `as_text`) and keeps the all-numeric koord from being
  # coerced to a double (scientific-notation risk for large round keys).
  tree_list <- .read_delim_auto(
    paste0(input_path, "/", .ensure_txt(treelist_filename)),
    encoding = encoding, as_text = TRUE
  )
  names(tree_list) <- tolower(names(tree_list))

  # Only the columns consumed to build the pre files are required (a clean fail
  # report instead of a cryptic dplyr error further down).
  # alter_ba (stand age) is NOT required: in structure-rich, uneven-aged stands a
  # meaningful stand age often does not exist. It is used if present; see the age
  # warning and the @details "Stand age" note.
  required_names_tree <- c("koord", "ba_fe", "bhd", "hoehe_1", "bestku",
                           "pol_entf", "pol_wink", "rueckazimut_gon",
                           "rueckazimut_grad", "einheit_azimut")
  check_rslt <- ForestElementsR::has_required_names(tree_list,
                                                     required_names_tree)
  if (!check_rslt$rqmt_ok) {
    return(.raw_to_pre_fail(
      fe_msg("cols_missing_in_file", check_rslt$err_message, treelist_filename),
      warnings
    ))
  }

  # Warn on missing / incomplete stand age so an accidental omission is noticed;
  # analyses that group by age class then fall back to dq classes (see docs).
  if (!("alter_ba" %in% names(tree_list)) ||
      any(is.na(.parse_numeric_vec(tree_list$alter_ba)))) {
    warnings <- c(warnings, fe_msg("age_missing_incomplete"))
  }
  # Ensure the column exists so the normalisation below can map it unconditionally.
  if (!("alter_ba" %in% names(tree_list))) tree_list$alter_ba <- NA_real_

  tree_list <- tree_list |>
    dplyr::mutate(
      koord_org = .data$koord,
      mortal    = 0L,   # Maske only records living trees
      anzahl    = 1L    # Hauptschicht: always 1 per record
    )

  # --- 3. Read inventory points ----------------------------------------------

  inv_points <- .read_delim_auto(
    paste0(input_path, "/", .ensure_txt(inv_punkt_filename)),
    encoding = encoding, as_text = TRUE
  )
  names(inv_points) <- tolower(names(inv_points))

  # Only the columns consumed to build the pre files are required. repfl
  # (represented area) is mandatory; the survey year comes from date_created OR
  # from the inventory_year argument (checked below), so date_created is not
  # required here.
  required_names_pt <- c("koord", "lage_probekreismittelpunkt_x",
                         "lage_probekreismittelpunkt_y", "repfl")
  check_rslt <- ForestElementsR::has_required_names(inv_points,
                                                     required_names_pt)
  if (!check_rslt$rqmt_ok) {
    return(.raw_to_pre_fail(
      fe_msg("cols_missing_in_file", check_rslt$err_message, inv_punkt_filename),
      warnings
    ))
  }

  # The plot identifier (koord) must be unique across the point file: a
  # duplicate would otherwise collapse two distinct plots into one at the pre
  # reader and surface as a cryptic "dbh_lower is not unique" error. Reject it
  # here, clearly. Shared logic with Format 1 (see .plot_id_duplicate_msg).
  dup_msg <- .plot_id_duplicate_msg(inv_points, "koord", inv_punkt_filename)
  if (!is.null(dup_msg)) return(.raw_to_pre_fail(dup_msg, warnings))

  inv_points <- .parse_numeric_cols(inv_points, .mask_point_numeric)

  # Every plot must carry its centre coordinate; an empty one cannot be placed
  # and would otherwise surface later as a cryptic sf "missing values in
  # coordinates" error at the pre reader. Reject it here, clearly. Coordinates
  # are numeric now, so an empty cell is already NA. Shared with Format 1 (see
  # .missing_coords_msg).
  coord_msg <- .missing_coords_msg(inv_points, "lage_probekreismittelpunkt_x",
                                   "lage_probekreismittelpunkt_y", "koord",
                                   inv_punkt_filename)
  if (!is.null(coord_msg)) return(.raw_to_pre_fail(coord_msg, warnings))

  # Resolve the coordinate reference system: from the file's own optional SRS
  # column, or from coord_sys -- checked against each other and against the
  # magnitude of the coordinates, never guessed.
  .crs <- tryCatch(
    .resolve_raw_coord_sys(
      inv_points, coord_sys = coord_sys,
      x_col   = "lage_probekreismittelpunkt_x",
      y_col   = "lage_probekreismittelpunkt_y",
      decl_col = "lage_probekreismittelpunkt_srs"
    ),
    error = function(e) list(err = conditionMessage(e))
  )
  if (!is.null(.crs$err)) return(.raw_to_pre_fail(.crs$err, warnings))
  if (!is.null(.crs$msg)) message(.crs$msg)

  # Store the plot centres metrically in the pre: the gauss_rw/gauss_hw columns
  # carry a projected CRS by convention (real BaySF exports store UTM32 there),
  # so geographic input is reprojected to UTM32 while projected input is kept as
  # is. The effective CRS travels with the pre data (fdinvkrs column
  # `coord_sys`), so the pre reader is self-describing.
  .pre_crs <- .coords_to_pre_metric(inv_points$lage_probekreismittelpunkt_x,
                                    inv_points$lage_probekreismittelpunkt_y,
                                    .crs$coord_sys)
  inv_points$lage_probekreismittelpunkt_x <- .pre_crs$x
  inv_points$lage_probekreismittelpunkt_y <- .pre_crs$y

  # Circle geometry from exactly one source (file XOR dbh_cm_from+radiuses_m).
  has_cdef_file <- !is.null(circle_def_filename)
  any_cdef_vec  <- !is.null(dbh_cm_from) || !is.null(radiuses_m)
  both_cdef_vec <- !is.null(dbh_cm_from) && !is.null(radiuses_m)
  if (has_cdef_file && any_cdef_vec) {
    return(.raw_to_pre_fail(fe_msg("circle_def_conflict"), warnings))
  }
  if (!has_cdef_file && !both_cdef_vec) {
    return(.raw_to_pre_fail(fe_msg("circle_def_missing"), warnings))
  }

  .cdef <- .resolve_circle_source(input_path, circle_def_filename,
                                  dbh_cm_from, radiuses_m, encoding)
  if (!.cdef$uniform && !"fe_ikl" %in% names(inv_points)) {
    return(.raw_to_pre_fail(fe_msg("circle_ikl_col_missing"), warnings))
  }
  inv_points <- .assign_circle_class(inv_points, .cdef$uniform)
  p(message = fe_msg("progress_read"))

  # --- 4. Normalise tree list ------------------------------------------------

  # Parse the numeric tree-list columns tolerant of the decimal mark (comma vs.
  # point) via the whitelist mechanism. Only the named measurement/code columns
  # are touched; koord rides through as text. Replaces the older df-global
  # convert_numbers_with_commas() (see the decimal-robustness rollout note).
  tree_list <- .parse_numeric_cols(tree_list, .mask_tree_numeric)

  # Remap the Format-2 stand-layer codes to the BaySF layer codes that
  # the rest of the pipeline (fcbestku, layer_key) expects. The two codings are
  # NOT identical (see the @section on stand layers at the top of this function):
  #   1 Hauptschicht              -> 1 Oberschicht (main stand)
  #   2 Zwischenschicht           -> 2 Unterstand
  #   3 Unterschicht              -> 2 Unterstand
  #   4 Vorausverjuengung         -> 3 Vorausverjuengung
  #   5 Ueberhaelter/Nachhiebsrest-> 5 Ueberhaelter
  # Only living trees carry a layer code; dead trees (mortal > 0) get -1 as
  # before. Unknown codes are passed through unchanged.
  tree_list <- tree_list |>
    dplyr::mutate(
      bestku = dplyr::case_when(
        .data$mortal > 0     ~ -1,
        .data$bestku == 1    ~  1,
        .data$bestku == 2    ~  2,
        .data$bestku == 3    ~  2,
        .data$bestku == 4    ~  3,
        .data$bestku == 5    ~  5,
        TRUE                 ~ as.numeric(.data$bestku)
      )
    ) |>
    dplyr::group_by(.data$koord) |>
    dplyr::mutate(
      lfd_satz = dplyr::dense_rank(paste(.data$ba_fe, .data$bestku))
    ) |>
    dplyr::ungroup()

  # Lower DBH bound (cm) of the second circle, taken from the resolved circle
  # definition (never hard-coded): at or above it a tree needs a position, below
  # it the tree belongs to the innermost circle where coordinates are optional.
  # NA when the definition has only one circle -> the helper then drops nothing.
  second_circle_dbh_cm <- sort(unique(.cdef$fdvikrs$bhd_von))[2]

  normalised_trees <- tree_list |>
    dplyr::rename(
      bhd_anz = "anzahl",
      bhd_h   = "hoehe_1"   # Maske stores height as hoehe_1
    ) |>
    dplyr::mutate(
      bhd      = .data$bhd      / 10,    # mm -> cm
      bhd_h    = .data$bhd_h   / 10,    # dm -> m
      # pol_entf stays in cm: the BaySF pre format stores plot distances in cm
      # (make_object_lists divides by 100 -> m). The raw Format-2 value is
      # already cm, so it is written through unchanged. (Previously divided by
      # 100 here, which -- on top of the /100 in make_object_lists -- shrank all
      # tree positions 100-fold. Nothing noticed: every tree then sat well
      # inside its circle, and FeNEU does not compare a tree's distance with
      # the radius of the circle its DBH belongs to.)
      pol_entf = .data$pol_entf,
      # pol_wink in fdinvbhd is always in degrees.
      # Priority: 1) use pol_wink if set; 2) if pol_entf is NA there is no
      # distance so leave pol_wink NA too (method = "flexible" skips these);
      # 3) otherwise take the back azimuth from the column that `einheit_azimut`
      #    declares: 1 = gon (-> deg: * 0.9), 2 = degrees.
      # 4) anything else -> NA (no usable azimuth).
      # `einheit_azimut` is deliberately AUTHORITATIVE here: if it does not match
      # the populated column, the tree counts as having no usable azimuth rather
      # than silently reading a value in the wrong unit (an 11 % error). Such
      # trees are then excluded -- loudly -- by .drop_trees_without_coords() when
      # they are outside the innermost circle.
      pol_wink = dplyr::case_when(
        !is.na(.data$pol_wink)        ~ .data$pol_wink,
        is.na(.data$pol_entf)         ~ NA_real_,
        .data$einheit_azimut == 1 &
          !is.na(.data$rueckazimut_gon)  ~ .data$rueckazimut_gon * 0.9,
        .data$einheit_azimut == 2 &
          !is.na(.data$rueckazimut_grad) ~ .data$rueckazimut_grad,
        TRUE                          ~ NA_real_
      ),
      hmb       = ifelse(!is.na(.data$bhd_h), "J", NA_character_),
      bestkey   = 1L,
      bestlfd   = 1L,
      fe_period = 1L,
      ba_fe     = .data$ba_fe,
      best_ku   = .data$bestku,
      inv_alt   = .data$alter_ba
    ) |>
    dplyr::group_by(.data$koord, .data$lfd_satz) |>
    dplyr::mutate(lfd_bhd = dplyr::row_number()) |>
    dplyr::ungroup()

  # basic sanity checks
  if (is.na(mean(as.numeric(normalised_trees$mortal)))) {
    return(.raw_to_pre_fail(fe_msg("mortal_col_empty", treelist_filename),
                            warnings))
  }
  if (any(as.numeric(normalised_trees$bhd_h) > 50, na.rm = TRUE)) {
    warnings <- c(warnings, fe_msg("height_gt_50"))
  }
  if (is.na(mean(as.numeric(
    normalised_trees$bhd[normalised_trees$mortal == 0]
  )))) {
    return(.raw_to_pre_fail(fe_msg("dbh_col_nonnumeric", treelist_filename),
                            warnings))
  }

  # Trees outside the innermost circle need a position; without one they are
  # excluded (never a fatal error, so incomplete data stays workable) and the
  # user is told prominently -- report AND console. Shared with Format 1 so both
  # formats behave symmetrically.
  .pos <- .drop_trees_without_coords(normalised_trees, second_circle_dbh_cm)
  normalised_trees <- .pos$trees
  if (!is.null(.pos$msg)) {
    warnings <- c(warnings, .pos$msg)
    warning(.pos$msg, call. = FALSE)
  }

  # Innermost-circle trees keep their (optional) position, but a PARTIAL one --
  # a distance but no azimuth, or vice versa -- is unusable and would otherwise
  # crash the geometry build at the pre reader. Null both components and report
  # the affected trees identifiably. Shared with Format 1.
  .par <- .null_partial_positions(normalised_trees, second_circle_dbh_cm)
  normalised_trees <- .par$trees
  if (!is.null(.par$msg)) {
    warnings <- c(warnings, .par$msg)
    warning(.par$msg, call. = FALSE)
  }

  # A living tree whose stand-layer code is missing (empty bestku -> best_ku NA)
  # cannot be assigned to a layer; exclude it (identifiable report) rather than
  # let it abort the whole import later with FER's "layer_key must be complete".
  # Shared with Format 1.
  .lay <- .drop_trees_without_layer(normalised_trees)
  normalised_trees <- .lay$trees
  if (!is.null(.lay$msg)) {
    warnings <- c(warnings, .lay$msg)
    warning(.lay$msg, call. = FALSE)
  }

  # Slope (neigung, percent). Format 2's multi-table source carries no terrain
  # slope of its own (the former `neig_grad` was derived from the positional
  # accuracy `lage_probekreismittelpunkt_accuracy`, which is a GPS accuracy in
  # metres, NOT a slope -- so it is dropped). Honour a real `neigung`/`slope`
  # column (percent) if the point file happens to provide one; otherwise default
  # to 0 (level ground: no horizontal-projection area correction of the circles)
  # and warn, so the absent correction is visible rather than silent. The
  # pre-reader converts this percent value to the internal slope in degrees.
  slope_col <- intersect(c("neigung", "slope"), names(inv_points))[1]
  if (is.na(slope_col)) {
    inv_points$neigung <- 0
    warnings <- c(warnings, fe_msg("format2_no_slope"))
  } else {
    inv_points$neigung <- inv_points[[slope_col]]
  }

  # Downstream only the survey YEAR is used. An explicit inventory_year wins and
  # the file date is not parsed at all (the native export writes date_created as
  # the ambiguous DD-MM-YY, so this is the recommended path); otherwise the plain
  # calendar year of date_created is taken per plot. erfdat is then written as a
  # normalised <year>-01-01 that the pre-reader reduces back to the year.
  .yr <- .derive_inventory_year(inv_points$date_created, inventory_year)
  if (!isTRUE(.yr$ok)) {
    msg <- if (identical(.yr$key, "inventory_year_invalid")) {
      fe_msg(.yr$key, .yr$arg)
    } else {
      fe_msg(.yr$key)
    }
    return(.raw_to_pre_fail(msg, warnings))
  }
  inv_points$erfdat <- paste0(.yr$years, "-01-01")

  normalised_points <- inv_points |>
    dplyr::rename(
      gauss_rw = "lage_probekreismittelpunkt_x",
      gauss_hw = "lage_probekreismittelpunkt_y"
    ) |>
    dplyr::mutate(
      bestkey   = 1L,
      bestlfd   = 1L,
      fe_period = 1L,
      coord_sys = .pre_crs$coord_sys  # CRS of the written coords; read back by the pre reader
    )

  p(message = fe_msg("progress_validate"))

  # --- 5. Build goal data frames ---------------------------------------------

  fdinvbhd <- build_fdinvbhd(normalised_trees)

  fdinvba <- normalised_trees |>
    dplyr::group_by(.data$koord, .data$lfd_satz) |>
    dplyr::summarise(
      ba_fe     = as.numeric(unique(.data$ba_fe)),
      best_ku   = dplyr::first(.data$best_ku),
      bestkey   = 1L,
      bestlfd   = 1L,
      fe_period = 1L,
      inv_alt   = dplyr::first(.data$inv_alt),
      .groups   = "drop"
    )

  fdinvkrs <- build_fdinvkrs(normalised_points)

  # BaySF-compatible stand-layer table is generated; Format 2 does not read a
  # user-supplied layer file (see the deferred-features note above the function).
  fcbestku <- build_fcbestku()

  fdvikrs <- .cdef$fdvikrs

  # Validate: all fe_ikl values used by points must be defined in fdvikrs
  missing_ikl <- setdiff(unique(fdinvkrs$fe_ikl), unique(fdvikrs$fe_ikl))
  if (length(missing_ikl) > 0) {
    return(.raw_to_pre_fail(
      fe_msg("circle_class_undefined", paste(missing_ikl, collapse = ", ")),
      warnings
    ))
  }
  if (0 %in% fdvikrs$kreis_fl) {
    return(.raw_to_pre_fail(fe_msg("radiuses_gt_zero"), warnings))
  }
  p(message = fe_msg("progress_build"))

  # --- 6. Write live-tree goal files ----------------------------------------

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  readr::write_delim(fdinvbhd, paste0(output_dir, "/fdinvbhd.txt"), delim = "\t")
  readr::write_delim(fdinvba,  paste0(output_dir, "/fdinvba.txt"),  delim = "\t")
  readr::write_delim(fdinvkrs, paste0(output_dir, "/fdinvkrs.txt"), delim = "\t")
  readr::write_delim(fcbestku, paste0(output_dir, "/fcbestku.txt"), delim = "\t")
  readr::write_delim(fdvikrs,  paste0(output_dir, "/fdvikrs.txt"),  delim = "\t")

  paths <- file.path(output_dir, c("fdinvbhd.txt", "fdinvba.txt", "fdinvkrs.txt",
                                   "fcbestku.txt", "fdvikrs.txt"))

  # --- 7. Dead wood (DEACTIVATED -- deferred to a later release) -------------
  # Dead-wood import is postponed post-CRAN (the EVALUATION of dead wood is not
  # part of the first release). The working implementation is kept here,
  # commented out rather than deleted, so it can be reinstated as-is: re-add the
  # `deadwood_filename` and `fe_nr` parameters to the signature (their @param
  # docs live in the deferred block below) and uncomment. It reads the raw
  # dead-wood file as text with the .mask_dw_numeric whitelist and builds
  # fdinvtot via build_fdinvtot(source = "inputmask").
  #
  # if (!is.null(deadwood_filename)) {
  #
  #   dead_raw <- .read_delim_auto(
  #     paste0(input_path, "/", .ensure_txt(deadwood_filename)),
  #     encoding = encoding, as_text = TRUE
  #   )
  #   names(dead_raw) <- tolower(names(dead_raw))
  #   dead_raw <- .parse_numeric_cols(dead_raw, .mask_dw_numeric)
  #
  #   fe_nr_val <- fe_nr   # capture parameter before mutate masks it with column name
  #   normalised_deadwood <- dead_raw |>
  #     dplyr::mutate(
  #       fe_period = 1L,
  #       bestkey   = 1L,
  #       bestlfd   = 1L,
  #       fe_nr     = fe_nr_val
  #     )
  #
  #   fdinvtot <- build_fdinvtot(normalised_deadwood)
  #
  #   readr::write_delim(fdinvtot, paste0(output_dir, "/fdinvtot.txt"),
  #                      delim = "\t")
  #   paths <- c(paths, file.path(output_dir, "fdinvtot.txt"))
  # }

  p(message = fe_msg("progress_write"))

  .raw_to_pre_ok(paths, warnings)
}


# ---------------------------------------------------------------------------
# Deferred to a later release -- parameters removed from the public interface
# for the first release, kept here so their documentation is not lost when the
# feature is re-added. Do NOT delete. (Mirrors the deferred-features block of
# import_sample_concentric_format1_raw_to_pre().)
#
# @param layers_filename Name of the layer-definition file (with or without the
#   .txt extension), or NULL to use the standard BaySF layer table generated by
#   build_fcbestku(). (Format 2 currently always builds the default; a
#   user-supplied layer file is not offered.)
#
# @param deadwood_filename Name of the deadwood tab-delimited text file (with or
#   without the extension) in input_path, corresponding to the deadwood sheet
#   (07_totholzerfassung_ or equivalent). NULL -> no fdinvtot.txt is written. The
#   EVALUATION of deadwood is not part of the first release. Required columns:
#   koord, lfd_tot, totholz_lage, totzer, totbag, dm (mm), hoehe (dm). Deadwood is
#   always recorded on a fixed 500 m2 circle per BaySF protocol (section 6.2.8,
#   Erstanlage 2024), independent of the live-tree concentric circles. The
#   working implementation is kept, deactivated, in section 7 of the converter.
#
# @param fe_nr Forest district number, written into the fe_nr column of
#   fdinvtot.txt (a BaySF administrative key of the native fdinvtot layout; not
#   consumed by any FeNEU reader). Required when deadwood_filename was supplied.
# ---------------------------------------------------------------------------

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

#' Convert raw sample inventory data (format 1) to preprocessed BaySF style data
#'
#' Reads format 1 raw _sample_ inventory data collected with the concentric-circle
#' method and writes the five standard BaySF-style intermediate files
#' (\code{fdinvbhd.txt}, \code{fdinvba.txt}, \code{fdinvkrs.txt},
#' \code{fcbestku.txt}, \code{fdvikrs.txt}) into \code{output_dir}.
#'
#' Column names in the source files are case-insensitive (converted to lower
#' case internally). Both English canonical names and their German equivalents
#' are accepted; the German alias is silently renamed to the English canonical
#' before validation.
#'
#' @section Format 1 and Format 2:
#' FeNEU currently supports two raw data formats for sample inventories with
#' concentric circles. The two grew up in parallel with FeNEU itself and have no
#' fundamental differences in content -- they simply reflect different user
#' preferences for how the data are exported. We call them \dQuote{Format 1} and
#' \dQuote{Format 2}. This function reads \strong{Format 1}; Format 2 is read by
#' \code{\link{import_sample_concentric_format2_raw_to_pre}}. Both produce the
#' same BaySF-style preprocessed files, so everything downstream is identical.
#'
#' @details
#' In its raw encoding, Format 1 records measurements in familiar units (DBH in
#' cm, height in m), the tree position as a single azimuth \code{angle} (degrees)
#' plus a \code{distance} (m), the stand layer directly as BaySF layer codes, and
#' both living and dead trees. Format 2 encodes the same information differently
#' (DBH in mm, a gon-or-degree back azimuth with a unit flag, its own layer code,
#' living trees only); see
#' \code{\link{import_sample_concentric_format2_raw_to_pre}}.
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
#' unit where the raw format requires it. Estimating the missing heights is a
#' deliberate, separate downstream step --
#' \code{pull_trees() |> height_complete_inventory() |> fill_heights_back()} --
#' so that measured and estimated heights are never silently mixed.
#'
#' @param input_path Path to the folder holding the source files (character).
#'
#' @param output_dir Folder the preprocessed BaySF-style files are written to
#'   (character, required). There is deliberately no default: the function
#'   writes files, and a default would write into the folder the raw data came
#'   from, i.e. into the user's own filespace. Created if it does not exist.
#'   Untouched if validation fails.
#'
#'   Should the plot ids of the tree list and the inventory-point file not match,
#'   two diagnostic files listing the offending plots are written here as well:
#'   \code{tree_not_in_inv.txt} (plots with trees but no inventory point -- their
#'   trees are dropped further down the build) and \code{inv_not_in_tree.txt}
#'   (inventory points without trees). They go to the output folder, never next to
#'   the input data, because this is the folder you chose to be written to.
#'
#' @param treelist_filename Name of the tree-list file (with or without the
#'   \code{.txt} extension).
#'
#'   \strong{Required columns} (German alias in parentheses):
#'   \describe{
#'     \item{\code{plot_id}}{Plot identifier (character or integer). Must
#'       match the \code{plot_id} values in \code{inv_punkt_filename} exactly.}
#'     \item{\code{dbh} (\code{bhd})}{Diameter at breast height, in
#'       \strong{cm} (numeric, >= 0; use 0 for small-tree / ingrowth
#'       records).}
#'     \item{\code{height} (\code{hoehe})}{Total tree height, in \strong{m}
#'       (numeric, 0-50; \code{NA} if not measured).}
#'     \item{\code{species} (\code{baumart})}{Species code in BaySF integer
#'       coding, or a species abbreviation when \code{species_guess = TRUE}
#'       (see the \code{species_guess} argument).}
#'     \item{\code{angle} (\code{winkel})}{Azimuth from plot centre to the
#'       tree stem, in decimal \strong{degrees} (numeric, 0-360). May be
#'       \code{NA} for trees on the innermost circle, which need no position;
#'       a larger tree without a position is excluded (with a report warning).}
#'     \item{\code{distance} (\code{entfernung})}{Horizontal distance from
#'       plot centre to the tree stem, in \strong{m} (numeric, > 0). Same
#'       \code{NA} rule as \code{angle}.}
#'     \item{\code{layer} (\code{schicht})}{Stand-layer code (integer):
#'       1 = main stand (OS), 2 = understorey (US),
#'       3 = advance regeneration (VVJ), 4 = residual stand (NHR),
#'       5 = overstorey (UEH), 6 = veteran tree (ALT).}
#'     \item{\code{mortality} (\code{mortal})}{Mortality status (integer):
#'       0 = alive, 1 = recently dead (Frischmortalitaet),
#'       2 = older snag (Altmortalitaet).}
#'     \item{\code{count} (\code{anzahl})}{Number of trees this record
#'       represents (integer, usually 1; values > 1 are used for small-tree
#'       tally records where individual trees are not located).}
#'   }
#'
#'   \strong{Optional columns:}
#'   \describe{
#'     \item{\code{age} (\code{alter})}{\strong{Stand} (or stand-layer) age in
#'       years -- never an individual-tree age, even though it is stored
#'       redundantly per tree row (numeric). Optional; used if present. Missing or
#'       incomplete age limits which analyses are possible and triggers a warning
#'       on import -- see the \dQuote{Stand age} section in Details.}
#'     \item{\code{tree_nr} (\code{baumnummer})}{Tree number within the plot.
#'       It may be supplied but is \strong{not processed further}: tree
#'       identification -- in particular when linking two successive inventories
#'       -- rests solely on the coordinates, and trees without coordinates (the
#'       innermost circle) are treated as unmatchable. So no tree number is ever
#'       consumed.}
#'   }
#'   Any further columns are ignored -- extra columns do not disturb the import,
#'   they are simply not used. As FeNEU develops, more columns may become
#'   supported (this applies to all raw import formats).
#'
#' @param inv_punkt_filename Name of the inventory-point file (with or without the
#'   \code{.txt} extension).
#'
#'   \strong{Required columns} (German alias in parentheses):
#'   \describe{
#'     \item{\code{plot_id}}{Plot identifier (character or integer). Must
#'       match the values in \code{treelist_filename} exactly.}
#'     \item{\code{coord_x}}{X coordinate (easting / longitude) of the plot
#'       centre, in the coordinate reference system declared either by the
#'       optional \code{coord_sys} column below or by the \code{coord_sys}
#'       argument (numeric).}
#'     \item{\code{coord_y}}{Y coordinate (northing / latitude) of the plot
#'       centre (numeric).}
#'     \item{\code{rep_area} (\code{repfl})}{Area represented by this
#'       inventory plot, in \strong{ha} (numeric, > 0).}
#'     \item{\code{slope} (\code{neigung})}{Terrain slope at the plot centre, in
#'       \strong{percent} (numeric, >= 0; e.g. \code{27} means a 27 percent
#'       slope). Used for the horizontal-projection (area) correction of the
#'       concentric circles; use 0 for level ground. The BaySF-native name
#'       \code{neigung} is accepted as an alias; the pre-reader converts the
#'       percent value to the internal slope in degrees.}
#'   }
#'
#'   \strong{Optional columns:}
#'   \describe{
#'     \item{\code{coord_sys}}{Coordinate reference system of
#'       \code{coord_x}/\code{coord_y}, one value for the whole file. Either a
#'       FeNEU key (\code{"gk4"}, \code{"lonlat"}, … -- see the \code{coord_sys}
#'       argument) or an EPSG declaration (\code{"EPSG:31468"}, \code{"31468"}).
#'       When absent, the \code{coord_sys} argument must supply it; when both are
#'       given they must agree (otherwise the import stops).}
#'     \item{\code{survey_date} (\code{erfdat})}{Survey date in any format
#'       parseable by \code{\link{detect_date_format}} (character or Date). Only
#'       its \emph{year} is used (see \code{inventory_year}); the day and month
#'       are discarded. If \code{inventory_year} is given it wins and this column
#'       is not read; if it is absent, either \code{survey_date} or
#'       \code{inventory_year} must be present.}
#'     \item{\code{fe_ikl}}{Circle-class key (integer). Only needed when the
#'       inventory uses more than one circle definition: it links each plot to
#'       its class in the per-class circle-definition file
#'       (\code{circle_def_filename}). Omit it when one circle definition applies
#'       to all plots (\code{dbh_cm_from}/\code{radiuses_m} or an
#'       \code{fe_ikl = "ALL"} file).}
#'   }
#'   Further columns are ignored (they do not disturb the import).
#'
#' @param small_trees_filename Name of the small-tree file (with or without the
#'   \code{.txt} extension), or \code{NULL} (default) if none exists. Small
#'   trees are trees below the inventory's usual caliper threshold
#'   (\dQuote{Kluppschwelle}) -- often even below breast height (1.3 m) -- and
#'   are therefore recorded with somewhat different methods: un-located tally
#'   records (no azimuth/distance). They are carried into the \code{small_trees}
#'   slot of each inventory plot. A record needs at least one size measure: a
#'   diameter (\code{dbh > 0}) or a height. Records with \code{dbh > 0} are
#'   sorted into the regular trees slot; those with \code{dbh = 0} stay small
#'   trees and must then carry a height.
#'   \strong{Required columns} (German alias in parentheses):
#'   \describe{
#'     \item{\code{plot_id}}{Plot identifier matching the tree-list.}
#'     \item{\code{dbh} (\code{bhd})}{DBH in \strong{cm} (0 for small trees
#'       below the caliper threshold).}
#'     \item{\code{height} (\code{hoehe})}{Height in \strong{m} (numeric,
#'       \code{NA} if not measured).}
#'     \item{\code{species} (\code{baumart})}{Species code (same coding as
#'       main tree list).}
#'     \item{\code{count} (\code{anzahl})}{Count of trees this record
#'       represents.}
#'   }
#'
#'   \strong{Optional columns:}
#'   \describe{
#'     \item{\code{layer} (\code{schicht})}{Stand-layer code (integer), same
#'       coding as the tree list; used if present to place the small-tree records
#'       in a layer.}
#'     \item{\code{age} (\code{alter})}{Stand (stand-layer) age in years; used if
#'       present.}
#'     \item{\code{tree_nr} (\code{baumnummer})}{Tree / tally number -- may be
#'       supplied but is not processed further (see the tree-list note above).}
#'   }
#'   Any further columns are ignored (they do not disturb the import).
#'
#' @param circle_def_filename Name of the circle-definition file (with or
#'   without the \code{.txt} extension), or \code{NULL} (default). An
#'   \code{fe_ikl}-keyed \strong{Probekreisdefinition} file with columns
#'   \code{fe_ikl}, \code{dbh_cm_from} (lower DBH limit, cm) and \code{radiuses_m}
#'   (circle radius, m), one row per concentric circle (the older
#'   \code{bhd_cm_von} / \code{radius_m} names are still accepted as aliases).
#'   Use \code{fe_ikl =
#'   "ALL"} for a single definition applied to every plot (it must then be the
#'   only definition); otherwise give one integer \code{fe_ikl} per circle class
#'   and link each inventory point to its class via the point file's own
#'   \code{fe_ikl} column. Create a uniform file with
#'   \code{\link{generate_circle_definition}}. Supply \strong{either} this file
#'   \strong{or} \code{dbh_cm_from}/\code{radiuses_m}, never both.
#'
#' @param species_guess Logical. If \code{TRUE} the function tries to map
#'   text species abbreviations to numeric BaySF codes using
#'   \code{species_abbreviations_bavrn_state}. Default is \code{FALSE}.
#'
#'   This is a \strong{stopgap for real-world data} whose species column holds
#'   text abbreviations instead of codes. We \strong{strongly recommend
#'   supplying the proper numeric BaySF species codes} (\code{species_guess =
#'   FALSE}) instead: guessing relies on exact, case-insensitive, and -- as a
#'   last resort -- fuzzy matching, and the fuzzy tier can silently mis-assign
#'   an ambiguous abbreviation. When guessing runs, always check the returned
#'   \code{species_mapping} (especially the \code{tier == "review"} rows, which
#'   also set \code{needs_review = TRUE}) before trusting the result.
#'
#' @param dbh_cm_from Numeric vector of lower DBH limits in \strong{cm} for each
#'   concentric circle, or \code{NULL}. Alternative to \code{circle_def_filename};
#'   must have the same length as \code{radiuses_m}. A circle definition (file or
#'   these arguments) is required.
#'
#' @param radiuses_m Numeric vector of circle radii in \strong{m}, or \code{NULL}.
#'   Same length as \code{dbh_cm_from}.
#'
#' @param coord_sys Coordinate reference system of \code{coord_x}/\code{coord_y}
#'   in the inventory-point file, or \code{NULL} (default) to take it from that
#'   file's own optional \code{coord_sys} column. One of \code{"lonlat"}
#'   (EPSG:4326), \code{"etrs89"} (4258), \code{"utm32"}/\code{"utm33"} (25832,
#'   25833), \code{"gk2"}-\code{"gk5"} (31466-31469).
#'
#'   The system is never guessed: if neither the column nor this argument states
#'   it, the import fails and says so; if both do and they disagree, it fails and
#'   names both; and if the declaration contradicts the magnitude of the
#'   coordinates (geographic values declared as projected or vice versa), it fails
#'   with a concrete suggestion.
#'
#'   The preprocessed \code{fdinvkrs.txt} stores the plot centres in a
#'   \strong{projected metric} CRS -- the \code{gauss_rw}/\code{gauss_hw} columns
#'   carry UTM/Gauss-Krueger by convention, as real BaySF exports do. Geographic
#'   input (\code{lonlat}/\code{etrs89}) is therefore reprojected to UTM32
#'   (EPSG:25832) here, when the pre is written; already-projected input
#'   (\code{gk*}, \code{utm*}) is kept as is. The CRS of the written coordinates
#'   is recorded in the \code{coord_sys} column, so the pre reader picks it up
#'   without a second declaration.
#'
#' @param inventory_year Optional single survey year (e.g. \code{2025}). It
#'   overrides any \code{survey_date} in the point file and is then the year for
#'   every plot; \code{survey_date} is not parsed. If omitted, the year is taken
#'   as the plain calendar year of each plot's \code{survey_date}. If neither is
#'   available the import stops and asks for \code{inventory_year}.
#'
#'   Downstream only the year is kept (each plot object carries a single
#'   \code{time_yr}). Set \code{inventory_year} deliberately by the growing season
#'   the survey belongs to: a survey done in, e.g., April counts for the previous
#'   season, since almost no growth has happened yet. This choice matters most
#'   when the increment between two successive inventories is computed later --
#'   the period length is the difference of the two inventory years, so an
#'   off-by-one year biases the whole increment rate.
#'
#' @param encoding Character encoding of the source \code{.txt} files.
#'   Default \code{"auto"} detects the encoding per file (UTF-8, including a
#'   byte-order mark, vs. Latin-1/Windows-1252), so users normally never set
#'   this. Pass an explicit encoding string only to force a particular one in a
#'   pathological case.
#'
#' @return A named list (the common raw-to-pre report): \code{ok} (logical),
#'   \code{errors} (character), \code{warnings} (character), \code{paths}
#'   (the written files, or \code{NULL} if \code{!ok}), \code{species_mapping}
#'   and \code{needs_review}. On success it writes \code{fdinvbhd.txt},
#'   \code{fdinvba.txt}, \code{fdinvkrs.txt}, \code{fcbestku.txt}, and
#'   \code{fdvikrs.txt} into \code{output_dir}.
#'
#'   When \code{species_guess = TRUE}, \code{species_mapping} is a tibble with
#'   one row per raw species abbreviation (\code{raw}, \code{n_trees},
#'   \code{canonical}, \code{species_id}, \code{name_ger}, \code{tier}). A
#'   \code{tier} of \code{"confirmed"} means an exact or case-insensitive match;
#'   \code{"review"} means the code was resolved only by substring / fuzzy /
#'   mojibake matching and is worth a manual check. \code{needs_review} is
#'   \code{TRUE} whenever any row is \code{"review"} (with a matching warning);
#'   both are \code{NULL}/\code{FALSE} when \code{species_guess = FALSE}.
#'
#' @export
#'
#' @examples
#' # Use example raw input data shipped with the package
#' # and write them to a temporary folder.
#'
#' td <- tempdir()
#'
#' # Write example tables under the recommended Format-1 file names
#' readr::write_delim(
#'   data_ex1_sample_raw_trees,
#'   file = file.path(td, "Baumschicht.txt"),
#'   delim = "\t"
#' )
#'
#' readr::write_delim(
#'   data_ex1_sample_raw_points,
#'   file = file.path(td, "Inv_punkt.txt"),
#'   delim = "\t"
#' )
#'
#' readr::write_delim(
#'   data_ex1_sample_raw_smalltrees,
#'   file = file.path(td, "Verjuengung.txt"),
#'   delim = "\t"
#' )
#'
#' # Definition of concentric sample circles
#' dbh_cm_from <- c(0, 12, 30, 48)
#' radiuses_m  <- c(2.82, 5.64, 11.28, 17.84)
#'
#' # Create BaySF-style intermediate files in a separate temporary folder
#' # (function is called for its side effects). output_dir is mandatory --
#' # the converter never picks a target folder itself.
#' out_dir <- file.path(tempdir(), "pre_ex1")
#' import_sample_concentric_format1_raw_to_pre(
#'   input_path = td,
#'   output_dir = out_dir,
#'   treelist_filename = "Baumschicht",
#'   inv_punkt_filename = "Inv_punkt",
#'   small_trees_filename = "Verjuengung",
#'   species_guess = TRUE,
#'   dbh_cm_from = dbh_cm_from,
#'   radiuses_m = radiuses_m,
#'   coord_sys = "gk4"
#' )
#'
#' # Check that one of the output files was created
#' file.exists(file.path(out_dir, "fdinvbhd.txt"))
#'
import_sample_concentric_format1_raw_to_pre <- function(input_path,
                                            output_dir,
                                            treelist_filename    = "Baumschicht.txt",
                                            inv_punkt_filename   = "Inv_punkt.txt",
                                            small_trees_filename = NULL,
                                            circle_def_filename  = NULL,
                                            species_guess        = FALSE,
                                            dbh_cm_from            = NULL,
                                            radiuses_m             = NULL,
                                            coord_sys            = NULL,
                                            inventory_year       = NULL,
                                            encoding             = "auto") {

  # output_dir has no default on purpose: this function writes files, and a
  # default would write into the user's own filespace (CRAN policy; and the
  # package never guesses silently). missing() cannot be seen from inside the
  # helper, so it is folded into NULL here and reported there.
  if (missing(output_dir)) output_dir <- NULL
  .assert_output_dir(output_dir, "import_sample_concentric_format1_raw_to_pre")

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

  # --- 2. Read and validate tree list ----------------------------------------

  # Read as text and defer numeric parsing to the whitelist (.parse_numeric_cols
  # further down); this keeps comma decimals intact (see .read_delim_auto's
  # `as_text`) and keeps an all-numeric plot_id from being coerced to a double.
  tree_list <- .read_delim_auto(
    paste0(input_path, "/", .ensure_txt(treelist_filename)),
    encoding = encoding, as_text = TRUE
  )
  names(tree_list) <- .normalise_header(names(tree_list))
  tree_list <- .normalise_colnames(tree_list, .ccirc_tree_aliases)

  # tree_nr (baumnummer) is deliberately NOT required: tree identification --
  # in particular when linking two successive inventories -- rests solely on the
  # coordinates, so a tree number is never consumed. It may be supplied and then
  # rides through untouched, like any extra column.
  required_names <- c("plot_id", "angle", "distance",
                      "layer", "mortality", "dbh", "height", "species", "count")

  check_rslt <- ForestElementsR::has_required_names(tree_list, required_names)
  if (!check_rslt$rqmt_ok) {
    return(.raw_to_pre_fail(
      fe_msg("cols_missing_in_file", check_rslt$err_message, treelist_filename),
      warnings
    ))
  }

  tree_list$regeneration <- FALSE

  # --- 3. Optionally merge small trees ---------------------------------------

  if (!is.null(small_trees_filename)) {

    small_trees <- .read_delim_auto(
      paste0(input_path, "/", .ensure_txt(small_trees_filename)),
      encoding = encoding, as_text = TRUE
    )
    names(small_trees) <- .normalise_header(names(small_trees))
    small_trees <- .normalise_colnames(small_trees, .ccirc_tree_aliases)

    required_names_sm <- c("plot_id", "dbh", "height",
                           "species", "count")
    check_rslt <- ForestElementsR::has_required_names(small_trees,
                                                       required_names_sm)
    if (!check_rslt$rqmt_ok) {
      return(.raw_to_pre_fail(
        fe_msg("cols_missing_in_file", check_rslt$err_message,
               small_trees_filename),
        warnings
      ))
    }

    # The tree list is kept entirely as text until the single numeric-parsing
    # step (.parse_numeric_cols) further down, so the injected `mortality` is a
    # text "0" to bind cleanly with the (still character) main tree list; it is
    # parsed to numeric together with the other measurement columns.
    small_trees <- small_trees |>
      dplyr::mutate(
        mortality    = "0",
        regeneration = TRUE,
        best_ku      = 2L
      )

    tree_list <- dplyr::bind_rows(tree_list, small_trees)
  }

  # --- 4. Read and validate inventory points ---------------------------------

  inv_points <- .read_delim_auto(
    paste0(input_path, "/", .ensure_txt(inv_punkt_filename)),
    encoding = encoding, as_text = TRUE
  )
  names(inv_points) <- .normalise_header(names(inv_points))
  inv_points <- .normalise_colnames(inv_points, .ccirc_point_aliases)

  # survey_date is not required: the survey year comes from it OR from the
  # inventory_year argument (resolved below).
  required_names_pt <- c("plot_id", "coord_x", "coord_y",
                         "rep_area", "slope")
  check_rslt <- ForestElementsR::has_required_names(inv_points,
                                                     required_names_pt)
  if (!check_rslt$rqmt_ok) {
    return(.raw_to_pre_fail(
      fe_msg("cols_missing_in_file", check_rslt$err_message, inv_punkt_filename),
      warnings
    ))
  }

  # The plot identifier (plot_id) must be unique across the point file: a
  # duplicate would otherwise collapse two distinct plots into one at the pre
  # reader and surface as a cryptic "dbh_lower is not unique" error. Reject it
  # here, clearly. Shared logic with Format 2 (see .plot_id_duplicate_msg).
  dup_msg <- .plot_id_duplicate_msg(inv_points, "plot_id", inv_punkt_filename)
  if (!is.null(dup_msg)) return(.raw_to_pre_fail(dup_msg, warnings))

  # Parse the numeric inventory-point columns tolerant of the decimal mark, so
  # comma-decimal exports (rep_area = "1,69") from German-locale systems are read
  # the same as point-decimal ones. The tree list is handled separately by
  # convert_numbers_with_commas() further down; the point reader had no such
  # step and silently kept comma values as text (real break with Steinfeld data).
  inv_points <- .parse_numeric_cols(inv_points, .ccirc_point_numeric)

  # Every plot must carry its centre coordinate; an empty one cannot be placed
  # and would otherwise surface later as a cryptic sf "missing values in
  # coordinates" error at the pre reader. Reject it here, clearly. Coordinates
  # are numeric now, so an empty cell is already NA. Shared with Format 2 (see
  # .missing_coords_msg).
  coord_msg <- .missing_coords_msg(inv_points, "coord_x", "coord_y", "plot_id",
                                   inv_punkt_filename)
  if (!is.null(coord_msg)) return(.raw_to_pre_fail(coord_msg, warnings))

  # Resolve the coordinate reference system: from the point file's own optional
  # `srs` column, or from coord_sys -- checked against each other and against the
  # magnitude of the coordinates, never guessed. Same helper as Format 2, so
  # both behave identically.
  .crs <- tryCatch(
    .resolve_raw_coord_sys(
      inv_points, coord_sys = coord_sys,
      x_col = "coord_x", y_col = "coord_y", decl_col = "coord_sys"
    ),
    error = function(e) list(err = conditionMessage(e))
  )
  if (!is.null(.crs$err)) return(.raw_to_pre_fail(.crs$err, warnings))
  if (!is.null(.crs$msg)) message(.crs$msg)

  # Store the plot centres metrically in the pre (gauss_rw/gauss_hw carry a
  # projected CRS by convention): geographic input is reprojected to UTM32,
  # projected input kept as is. The effective CRS travels in the pre's
  # `coord_sys` column. Same handling as Format 2.
  .pre_crs <- .coords_to_pre_metric(inv_points$coord_x, inv_points$coord_y,
                                    .crs$coord_sys)
  inv_points$coord_x <- .pre_crs$x
  inv_points$coord_y <- .pre_crs$y

  # --- 4b. Resolve circle definition + class assignment ----------------------

  # The circle geometry must come from exactly one source: either a
  # circle-definition file (fe_ikl-keyed, or a single "ALL" definition) or the
  # dbh_cm_from + radiuses_m vectors (uniform). Both, or neither, is an error --
  # circle geometry drives the results, so it is never guessed.
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

  # Per-class definitions need the fe_ikl link column in the inventory points.
  if (!.cdef$uniform && !"fe_ikl" %in% names(inv_points)) {
    return(.raw_to_pre_fail(fe_msg("circle_ikl_col_missing"), warnings))
  }

  inv_points <- .assign_circle_class(inv_points, .cdef$uniform)

  # --- 5. Cross-check plots --------------------------------------------------

  pt_check <- inv_point_checks(tree_list, inv_points, output_dir)
  if (pt_check$n_tree_not_in_inv > 0) {
    # Trees on these plots have no inventory point (no coordinates / rep_area /
    # slope) and are dropped further down the build -- surface it in the report,
    # not just as a console message (see tree_not_in_inv.txt).
    warnings <- c(warnings,
                  fe_msg("plots_trees_no_point", pt_check$n_tree_not_in_inv))
  }
  p(message = fe_msg("progress_read"))

  # --- 6. Validate tree-list values ------------------------------------------

  mc <- missing_checks(tree_list)
  if (length(mc$errors) > 0) return(.raw_to_pre_fail(mc$errors, warnings))
  tree_list <- mc$tree_list

  # Warn on missing / incomplete stand age so an accidental omission is noticed;
  # analyses that group by age class then fall back to dq classes (see docs).
  # Regeneration / small-tree rows carry no stand age by construction and are
  # excluded from the check.
  main_rows <- !(tree_list$regeneration %in% TRUE)
  if (!("age" %in% names(tree_list)) ||
      any(is.na(.parse_numeric_vec(tree_list$age[main_rows])))) {
    warnings <- c(warnings, fe_msg("age_missing_incomplete"))
  }
  p(message = fe_msg("progress_validate"))

  # --- 7. Species coding -----------------------------------------------------

  species_mapping <- NULL   # populated only when species_guess = TRUE

  if (!species_guess) {
    tree_list <- tree_list |> dplyr::rename(species_id = "species")
    # With species_guess = FALSE the species column must already hold numeric
    # BaySF codes. Catch a text column (e.g. species abbreviations) here with a
    # clear message instead of letting it slip through to build_fdinvba(), where
    # as.numeric() would coerce every code to NA with cryptic warnings.
    sp_raw <- as.character(tree_list$species_id)
    # Missing codes: caught here (early, clear) rather than silently written as
    # NA and only surfacing later as "species_id must be complete".
    sp_missing <- is.na(sp_raw) | !nzchar(trimws(sp_raw))
    if (any(sp_missing)) {
      bad_plots <- unique(tree_list$plot_id[sp_missing])
      examples <- paste(bad_plots[seq_len(min(5, length(bad_plots)))],
                        collapse = ", ")
      return(.raw_to_pre_fail(
        fe_msg("species_col_missing", treelist_filename, examples),
        warnings
      ))
    }
    sp_bad <- !is.na(sp_raw) & nzchar(trimws(sp_raw)) &
      is.na(suppressWarnings(as.numeric(sp_raw)))
    if (any(sp_bad)) {
      bad_vals <- unique(sp_raw[sp_bad])
      examples <- paste(bad_vals[seq_len(min(5, length(bad_vals)))],
                        collapse = ", ")
      return(.raw_to_pre_fail(
        fe_msg("species_col_nonnumeric", treelist_filename, examples),
        warnings
      ))
    }
  } else {
    species_table <- species_abbreviations_bavrn_state
    # check_and_guess_species uses 'baumart' internally; rename around the call
    tree_list <- tree_list |>
      dplyr::rename(baumart = "species") |>
      check_and_guess_species(species_table)
    # output has baumart + species_id from join; species_id is used downstream.
    # Grab the mapping report now: the dplyr verbs in step 8 drop attributes.
    species_mapping <- attr(tree_list, "species_mapping")
  }

  # --- 8. Normalise to common column names / units ---------------------------

  # Parse the numeric tree-list columns tolerant of the decimal mark (comma vs.
  # point), via the same whitelist mechanism the inventory-point reader uses.
  # Only the named measurement/code columns are touched; identifiers (plot_id,
  # tree_nr) and any extra columns ride through untouched -- in particular an
  # all-numeric plot_id is kept as text rather than coerced to a double (which
  # risked scientific notation for large round IDs). Replaces the older df-global
  # convert_numbers_with_commas(); see the decimal-robustness rollout note.
  tree_list <- .parse_numeric_cols(tree_list, .ccirc_tree_numeric)

  tree_list <- tree_list |>
    dplyr::mutate(
      best_ku = ifelse(.data$mortality > 0, -1L, as.integer(.data$layer))
    ) |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::mutate(
      lfd_satz = dplyr::dense_rank(paste(.data$species_id, .data$best_ku))
    ) |>
    dplyr::select(-"layer") |>
    dplyr::ungroup()

  # Format 1 has no BaySF administrative keys, so bestkey/bestlfd/fe_period are
  # set to 1 (below) and the user's plot_id becomes `koord` -- the third part of
  # the assembled plot_id (bestkey_bestlfd_koord). `koord` is an OPAQUE,
  # plot-unique identifier here, NOT a coordinate carrier: coordinates come from
  # coord_x/coord_y (-> gauss_rw/gauss_hw) and go into the geometry. Nothing may
  # decode coordinates from `koord`/plot_id (see the note at the plot_id
  # construction in import_functions_BaySF.R).
  normalised_trees <- tree_list |>
    dplyr::rename(
      koord    = "plot_id",
      pol_wink = "angle",
      pol_entf = "distance",
      bhd_anz  = "count",
      bhd_h    = "height",
      bhd      = "dbh",
      ba_fe    = "species_id",
      mortal   = "mortality"
    ) |>
    dplyr::mutate(
      hmb       = ifelse(!is.na(.data$bhd_h), "J", NA_character_),
      bestkey   = 1L,
      bestlfd   = 1L,
      fe_period = 1L,
      inv_alt   = if ("age" %in% names(tree_list)) .data$age else NA_real_
    )

  # basic sanity checks on the normalised data
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
  # user is told prominently -- report AND console. The threshold is the second
  # circle's lower DBH bound from the resolved circle definition, never
  # hard-coded. Shared with Format 2 so both formats behave symmetrically.
  second_circle_dbh_cm <- sort(unique(.cdef$fdvikrs$bhd_von))[2]
  .pos <- .drop_trees_without_coords(normalised_trees, second_circle_dbh_cm)
  normalised_trees <- .pos$trees
  if (!is.null(.pos$msg)) {
    warnings <- c(warnings, .pos$msg)
    warning(.pos$msg, call. = FALSE)
  }

  # Innermost-circle trees keep their (optional) position, but a PARTIAL one --
  # a distance but no azimuth, or vice versa -- is unusable and would otherwise
  # crash the geometry build at the pre reader. Null both components and report
  # the affected trees identifiably. Shared with Format 2.
  .par <- .null_partial_positions(normalised_trees, second_circle_dbh_cm)
  normalised_trees <- .par$trees
  if (!is.null(.par$msg)) {
    warnings <- c(warnings, .par$msg)
    warning(.par$msg, call. = FALSE)
  }

  # A living tree whose stand-layer code is missing (empty layer -> best_ku NA)
  # cannot be assigned to a layer; exclude it (identifiable report) rather than
  # let it abort the whole import later with FER's "layer_key must be complete".
  # Shared with Format 2.
  .lay <- .drop_trees_without_layer(normalised_trees)
  normalised_trees <- .lay$trees
  if (!is.null(.lay$msg)) {
    warnings <- c(warnings, .lay$msg)
    warning(.lay$msg, call. = FALSE)
  }

  # Downstream only the survey YEAR is used. An explicit inventory_year wins and
  # survey_date is not parsed; otherwise the plain calendar year of survey_date
  # is taken per plot. erfdat is written as a normalised <year>-01-01 that the
  # pre-reader reduces back to the year. Same helper as Format 2.
  .yr <- .derive_inventory_year(inv_points$survey_date, inventory_year)
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
      koord     = "plot_id",
      gauss_rw  = "coord_x",
      gauss_hw  = "coord_y",
      repfl     = "rep_area",
      # `slope` is the canonical raw name (alias `neigung`); rename it back to
      # the BaySF-native `neigung` (percent) for the pre file, where the
      # pre-reader converts it to the internal slope in degrees.
      neigung   = "slope"
    ) |>
    dplyr::mutate(
      bestkey   = 1L,
      bestlfd   = 1L,
      fe_period = 1L,
      coord_sys = .pre_crs$coord_sys  # CRS of the written coords; read back by the pre reader
    )

  # --- 9. Build goal data frames ---------------------------------------------

  fdinvbhd <- build_fdinvbhd(normalised_trees)
  fdinvba  <- build_fdinvba(normalised_trees)
  fdinvkrs <- build_fdinvkrs(normalised_points)

  # BaySF-compatible stand-layer table is generated; Format 1 does not read a
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

  # --- 10. Write live-tree goal files ----------------------------------------

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  paths <- character()

  readr::write_delim(fdinvbhd, paste0(output_dir, "/fdinvbhd.txt"), delim = "\t")
  readr::write_delim(fdinvba,  paste0(output_dir, "/fdinvba.txt"),  delim = "\t")
  readr::write_delim(fdinvkrs, paste0(output_dir, "/fdinvkrs.txt"), delim = "\t")
  readr::write_delim(fcbestku, paste0(output_dir, "/fcbestku.txt"), delim = "\t")
  readr::write_delim(fdvikrs,  paste0(output_dir, "/fdvikrs.txt"),  delim = "\t")

  paths <- c(paths, file.path(output_dir, c("fdinvbhd.txt", "fdinvba.txt",
                                            "fdinvkrs.txt", "fcbestku.txt",
                                            "fdvikrs.txt")))
  p(message = fe_msg("progress_write"))

  # Flag fuzzy-matched species for manual inspection (only when guessing ran).
  needs_review <- !is.null(species_mapping) &&
    any(species_mapping$tier == "review")
  if (needs_review) {
    warnings <- c(warnings,
                  fe_msg("species_needs_review",
                         sum(species_mapping$tier == "review")))
  }

  .raw_to_pre_ok(paths, warnings,
                 species_mapping = species_mapping,
                 needs_review    = needs_review)
}


# ---------------------------------------------------------------------------
# Deferred to a later release -- parameters removed from the public interface
# for the first release, kept here so their documentation is not lost when the
# feature is re-added. Do NOT delete.
#
# @param small_trees vs. deadwood/regeneration: the EVALUATION of deadwood and
#   regeneration is postponed; only the small-tree IMPORT stays supported.
#
# @param layers_filename Name of the layer-definition file (with or without the
#   .txt extension), or NULL to use the standard BaySF layer table generated by
#   build_fcbestku(). (Format 1 currently always builds the default; a
#   user-supplied layer file is not offered.)
#
# @param circle_map Optional list for filter-based circle-class assignment, with
#   `column` (a column in the inventory-point file) and `mapping` (named integer
#   vector of column value -> circle-class id). Superseded by the `fe_ikl`
#   class-key column linking inventory points to the circle-definition file.
#
# @param circle_definitions Optional data frame (kreis_id/bhd_von/radius)
#   defining multiple circle classes programmatically. Superseded by the
#   circle-definition file keyed on `fe_ikl`.
#
# @param deadwood_filename Name of the deadwood file (with or without the .txt
#   extension), or NULL. Deadwood is validated and written to fdinvtot.txt; the
#   evaluation of deadwood is not part of this release. Tab-delimited; German
#   umlaut headers (e.g. "Laenge in m") handled automatically. Required columns
#   (German alias in parentheses): piece_id (karten_nr), coord_y (y),
#   coord_x (x), length_m (laenge_in_m), mid_diam (mittendurchmesser_in_cm),
#   dbh (bhd), species_group (baumart), decay (grad), category (kategorie),
#   plot_id. Deadwood is recorded on a fixed 500 m2 circle per BaySF protocol
#   (Erstanlage 2024, section 6.2.8), independent of the live-tree circles.
#   Implementation lived in section 10 of the converter and used
#   .ccirc_dw_aliases + build_fdinvtot(source = "concentric").
#
# @param fe_nr Forest district number (integer), required when deadwood_filename
#   was supplied.
# ---------------------------------------------------------------------------

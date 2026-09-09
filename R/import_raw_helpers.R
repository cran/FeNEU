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

# Ensure a table file name carries its .txt extension. The raw->pre importers
# take file names relative to `input_path`; users naturally think in real file
# names ("trees.txt"), so accept the name with OR without the extension (and
# never produce "trees.txt.txt"). NULL passes through (optional-file arguments).
.ensure_txt <- function(x) {
  if (is.null(x)) return(x)
  ifelse(grepl("\\.txt$", x, ignore.case = TRUE), x, paste0(x, ".txt"))
}

#' Convert numbers with commas to point format
#'
#' Helper function for handling input data generated on German systems.
#'
#' Remark by Peter: Operates on a data frame. Future versions could be more
#' simple and generic by just taking one vector as the input; the current
#' version would also replace correct commas in text fields. The vector-based
#' version would be called from outside only for those columns of a data frame,
#' that are intended to be numeric.
#'
#' @param df A data frame containing numbers with commas
#'
#' @return A data frame similar to the input, however with all commas replaced
#'   by points and all colums converted to numeric if possible
#'
#' @keywords internal
#'
convert_numbers_with_commas <- function(df) {

  # Check if there are any numbers with commas
  has_commas <- any(grepl(",", unlist(df)))

  # Warn the user if any are found
  if (has_commas) {
    warning(fe_msg("commas_in_numbers"))
  }

  # Replace commas with dots and convert to numeric where possible
  df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), ~ gsub(",", ".", .))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(~ all(grepl("^\\d*\\.?\\d+$", .), na.rm = TRUE)),
        as.numeric
      )
    )
}


#' Parse one vector as numbers, tolerant of the decimal mark
#'
#' Locale-agnostic numeric parsing for a single column. Accepts both point
#' decimals (\code{"1.69"}) and German comma decimals (\code{"1,69"}) as well as
#' plain integers, so a file exported on a German-locale system and one exported
#' on an English-locale system yield the same numbers. A value that already
#' parses as a plain point-decimal number is taken as is; a pure comma-decimal
#' value has its comma turned into a point and is then parsed. Empty strings and
#' \code{NA} become \code{NA}.
#'
#' Anything else -- free text, a thousands separator, scientific notation, mixed
#' punctuation -- is returned as \code{NA} and reported with a warning: a
#' measurement column should not contain such tokens, and turning them into
#' \code{NA} silently would hide a data problem. A vector that is already
#' numeric is returned unchanged.
#'
#' @param x A vector (character or already numeric) from a column that is
#'   expected to hold numbers.
#' @param col_label Column name used in the warning message (character or
#'   \code{NULL}).
#'
#' @return A numeric vector the same length as \code{x}.
#'
#' @keywords internal
#'
.parse_numeric_vec <- function(x, col_label = NULL) {
  if (is.numeric(x)) return(x)

  chr <- trimws(as.character(x))
  chr[chr == ""] <- NA_character_

  point_ok <- grepl("^[+-]?[0-9]+(\\.[0-9]+)?$", chr)
  comma_ok <- grepl("^[+-]?[0-9]+(,[0-9]+)?$", chr)

  out <- rep(NA_real_, length(chr))
  out[point_ok] <- as.numeric(chr[point_ok])
  comma_only <- comma_ok & !point_ok
  out[comma_only] <- as.numeric(gsub(",", ".", chr[comma_only], fixed = TRUE))

  bad <- !is.na(chr) & !point_ok & !comma_ok
  if (any(bad)) {
    warning(
      fe_msg("numeric_parse_failed",
             if (is.null(col_label)) "?" else col_label,
             paste(utils::head(unique(chr[bad]), 3L), collapse = ", ")),
      call. = FALSE
    )
  }
  out
}


#' Parse selected columns of a raw/pre import table as numbers
#'
#' Decimal-mark-tolerant numeric parsing for exactly the columns a reader
#' actually consumes. Only the columns named in \code{cols} are touched (via
#' \code{\link{.parse_numeric_vec}}); every other column is passed through
#' unchanged. This is deliberately a whitelist: a raw or pre file that comes
#' from outside may carry extra, optional, or free-text columns FeNEU does not
#' use -- those ride through untouched, and legitimate commas in text fields are
#' never harmed (unlike the older df-global \code{\link{convert_numbers_with_commas}}).
#'
#' Column names in \code{cols} that are not present in \code{df} are silently
#' skipped, so an optional numeric column that a given file happens to omit is
#' no problem. Identifier columns (plot / stand / tree keys) must NOT be listed
#' here: they are kept as character and guarded separately (see
#' \code{.assert_id_cols_integer()}).
#'
#' \code{cols} is expected to hold canonical column names, so the call belongs
#' \emph{after} any header normalisation / alias renaming has run. The canonical
#' numeric-column sets are defined per format next to the alias maps in
#' \code{R/import_column_standards.R}, so a format change is a single-file edit.
#'
#' @param df A data frame just read from a raw or pre import file.
#' @param cols Character vector of canonical column names expected to hold
#'   numbers.
#'
#' @return \code{df} with the targeted character columns converted to numeric.
#'
#' @keywords internal
#'
.parse_numeric_cols <- function(df, cols) {
  for (cn in intersect(cols, names(df))) {
    df[[cn]] <- .parse_numeric_vec(df[[cn]], cn)
  }
  df
}


#' Drop trees that need a position but do not have one
#'
#' The BaySF-style pre format needs no tree coordinates for trees of the
#' \strong{innermost} concentric circle: those are tallied as a group rather
#' than located individually, so a missing azimuth / distance is legitimate and
#' they are kept (with \code{NA} coordinates). Every \emph{larger} tree does
#' need a position -- without one it cannot be placed on its circle and would be
#' meaningless downstream.
#'
#' Such trees are \strong{excluded} rather than treated as a fatal error, so a
#' user with not-yet-complete data can carry on working; the caller is expected
#' to surface the returned message prominently (report \emph{and} console), as
#' the excluded trees take part in no evaluation.
#'
#' The DBH limit is \strong{not} hard-coded: it is the lower DBH bound of the
#' second circle, i.e. it follows the circle definition of the actual inventory.
#' With only one circle every tree is an innermost-circle tree, so nothing is
#' dropped.
#'
#' Both concentric raw formats normalise to the same column names before this
#' runs (\code{koord}, \code{bhd} in cm, \code{pol_wink}, \code{pol_entf}),
#' which is what lets Format 1 and Format 2 share this helper and stay
#' symmetric.
#'
#' @param normalised_trees Tree data after normalisation: \code{koord},
#'   \code{bhd} (cm), \code{pol_wink}, \code{pol_entf}.
#' @param second_circle_dbh_cm Lower DBH bound (cm) of the second circle, i.e.
#'   the threshold at or above which a position is required. \code{Inf} or
#'   \code{NA} means there is no second circle -- nothing is dropped.
#'
#' @return \code{list(trees, msg)}: the (possibly reduced) tree data, and either
#'   \code{NULL} or a ready-to-emit warning message naming the count, the
#'   threshold and the affected inventory points.
#'
#' @keywords internal
#'
.drop_trees_without_coords <- function(normalised_trees, second_circle_dbh_cm) {

  if (length(second_circle_dbh_cm) != 1L || is.na(second_circle_dbh_cm) ||
      is.infinite(second_circle_dbh_cm)) {
    return(list(trees = normalised_trees, msg = NULL))
  }

  needs_pos <- !is.na(normalised_trees$bhd) &
    normalised_trees$bhd >= second_circle_dbh_cm
  no_pos    <- is.na(normalised_trees$pol_wink) | is.na(normalised_trees$pol_entf)
  drop      <- needs_pos & no_pos

  if (!any(drop)) return(list(trees = normalised_trees, msg = NULL))

  msg <- fe_msg("trees_no_position",
                sum(drop),
                format(second_circle_dbh_cm),
                .tree_ident_label(normalised_trees[drop, , drop = FALSE]))

  list(trees = normalised_trees[!drop, , drop = FALSE], msg = msg)
}


# ---------------------------------------------------------------------------
#' Compact per-tree identifier list for report messages
#'
#' When FeNEU excludes or alters individual trees (missing position, partial
#' position, ...), the report must let the user find those trees in their OWN
#' raw data. A per-plot identifier is not enough -- a plot has many trees. This
#' builds a compact, per-plot listing that names each affected tree by its tree
#' number when the raw file supplied one (\code{baumnummer} / \code{tree_nr},
#' both optional under the grouping taxonomy) and \strong{otherwise by its dbh
#' (cm)} -- so the trees stay identifiable even without tree numbers. The format
#' is symbolic and language-neutral (\code{"<plot>: <dbh>, <dbh>; <plot>:
#' #<nr>(<dbh>)"}); the surrounding message text explains it.
#'
#' @param trees Affected-tree subset with at least \code{koord} and \code{bhd}
#'   (cm); \code{baumnummer}/\code{tree_nr} used when present.
#' @param plot_col Name of the plot-identifier column (default \code{"koord"}).
#'
#' @return A single character string.
#'
#' @keywords internal
#'
.tree_ident_label <- function(trees, plot_col = "koord") {
  nr_col  <- intersect(c("baumnummer", "tree_nr"), names(trees))[1]
  has_nr  <- !is.na(nr_col) && any(!is.na(trees[[nr_col]]))
  fmt_dbh <- function(b) formatC(as.numeric(b), format = "f", digits = 1)

  plots <- unique(trees[[plot_col]])
  parts <- vapply(plots, function(k) {
    sub <- trees[trees[[plot_col]] == k, , drop = FALSE]
    items <- if (has_nr) {
      ifelse(!is.na(sub[[nr_col]]),
             sprintf("#%s(%s)", sub[[nr_col]], fmt_dbh(sub$bhd)),
             fmt_dbh(sub$bhd))
    } else {
      fmt_dbh(sub$bhd)
    }
    sprintf("%s: %s", k, paste(items, collapse = ", "))
  }, character(1))

  paste(parts, collapse = "; ")
}


# ---------------------------------------------------------------------------
#' Null incomplete positions of innermost-circle trees (with an identifiable report)
#'
#' Innermost-circle trees (dbh below the second circle) need no position. A raw
#' file may nonetheless give such a tree only PART of a position -- a distance
#' but no azimuth, or vice versa. A half-position is unusable and, left in
#' place, makes \code{ForestElementsR::fe_ccircle_spatial()} build an \code{NA}
#' geometry that \pkg{sf} rejects with \dQuote{missing values in coordinates not
#' allowed}, aborting the import. This nulls \strong{both} components so the
#' tree is treated as unpositioned (it stays in the evaluation -- count / volume
#' -- but is not placed spatially), and returns an identifiable report of the
#' affected trees. Trees that DO require a position were already handled by
#' \code{\link{.drop_trees_without_coords}}, which is why only innermost trees
#' remain here. Shared by both concentric raw->pre converters so they stay
#' symmetric; \code{make_object_lists()} nulls the same case again as a safety
#' net for pre files supplied directly (not produced by these converters).
#'
#' @param normalised_trees Tree data after normalisation: \code{koord},
#'   \code{bhd} (cm), \code{pol_wink}, \code{pol_entf}.
#' @param second_circle_dbh_cm Lower DBH bound (cm) of the second circle.
#'   \code{Inf}/\code{NA} means a single circle -- then every tree is innermost.
#'
#' @return \code{list(trees, msg)}: the tree data with incomplete positions
#'   nulled, and either \code{NULL} or a ready-to-emit warning message naming
#'   the affected trees.
#'
#' @keywords internal
#'
.null_partial_positions <- function(normalised_trees, second_circle_dbh_cm) {
  innermost <- !is.finite(second_circle_dbh_cm) |
    is.na(normalised_trees$bhd) |
    normalised_trees$bhd < second_circle_dbh_cm
  partial <- xor(is.na(normalised_trees$pol_wink),
                 is.na(normalised_trees$pol_entf))
  hit <- innermost & partial

  if (!any(hit)) return(list(trees = normalised_trees, msg = NULL))

  msg <- fe_msg("trees_partial_position",
                sum(hit),
                .tree_ident_label(normalised_trees[hit, , drop = FALSE]))

  normalised_trees$pol_wink[hit] <- NA_real_
  normalised_trees$pol_entf[hit] <- NA_real_

  list(trees = normalised_trees, msg = msg)
}


# ---------------------------------------------------------------------------
#' Drop trees that carry no stand-layer code (with an identifiable report)
#'
#' Every living tree must be assigned to a stand layer -- it is what places the
#' tree in the main stand, the understorey, etc. A raw file may leave the layer
#' blank for an individual tree (empty \code{layer} / \code{bestku}); such a
#' tree ends up with \code{best_ku = NA} after normalisation, which
#' \code{ForestElementsR::fe_ccircle_spatial()} later rejects with the cryptic
#' \dQuote{column(s) layer_key must be complete}, aborting the whole import over
#' a single defective tree. FeNEU cannot guess the layer, so -- exactly like a
#' tree without a required position -- the tree is \strong{excluded} (not a
#' fatal error, so the rest of the intact plot and inventory still build), and
#' reported identifiably so the user can add the missing code and re-import.
#' Dead trees always carry \code{best_ku = -1} (set during normalisation), so
#' only living trees with a genuinely missing layer are affected. Both
#' concentric raw formats normalise the layer to the same \code{best_ku} column,
#' which lets Format 1 and Format 2 share this helper and stay symmetric.
#'
#' @param normalised_trees Tree data after normalisation: \code{koord},
#'   \code{bhd} (cm), \code{best_ku} (integer layer code, \code{NA} when the raw
#'   layer was missing).
#'
#' @return \code{list(trees, msg)}: the (possibly reduced) tree data, and either
#'   \code{NULL} or a ready-to-emit warning message naming the affected trees.
#'
#' @keywords internal
#'
.drop_trees_without_layer <- function(normalised_trees) {
  drop <- is.na(normalised_trees$best_ku)

  if (!any(drop)) return(list(trees = normalised_trees, msg = NULL))

  dropped <- normalised_trees[drop, , drop = FALSE]

  # Distinguish regular trees from small-tree (regeneration) records so the
  # report names which kind of record is affected. The regeneration flag exists
  # only on the Format-1 path (Format 2 produces no small-tree records); if the
  # column is absent, every dropped record is treated as a regular tree.
  is_reg <- if ("regeneration" %in% names(dropped)) {
    dropped$regeneration %in% TRUE
  } else {
    rep(FALSE, nrow(dropped))
  }

  msgs <- character(0)
  if (any(!is_reg)) {
    msgs <- c(msgs, fe_msg("trees_no_layer",
                           sum(!is_reg),
                           .tree_ident_label(dropped[!is_reg, , drop = FALSE])))
  }
  if (any(is_reg)) {
    msgs <- c(msgs, fe_msg("small_trees_no_layer",
                           sum(is_reg),
                           .tree_ident_label(dropped[is_reg, , drop = FALSE])))
  }

  list(trees = normalised_trees[!drop, , drop = FALSE],
       msg = paste(msgs, collapse = "\n\n"))
}


# ---------------------------------------------------------------------------
#' Reject a duplicated plot identifier in the inventory-point file
#'
#' The plot-identifier column of the inventory-point file (\code{koord} in
#' Format 2, \code{plot_id} in Format 1) must be unique -- it is THE key that
#' links a plot to its trees, its coordinates and its circle definition. A
#' duplicate is never harmless: the pre reader nests each plot's circle
#' definition by that key, so two rows sharing an id silently collapse into one
#' plot carrying both circle sets, which only surfaces much later as the cryptic
#' \dQuote{dbh_lower is not unique} error. Catching it here, at the raw->pre
#' stage, turns that into a clear, early failure. Shared by both concentric
#' raw->pre converters so they stay symmetric.
#'
#' @param points Inventory-point data frame.
#' @param id_col Name of the plot-identifier column (\code{"koord"} /
#'   \code{"plot_id"}).
#' @param filename The point file name, for the error message.
#'
#' @return \code{NULL} when the identifier is unique, otherwise a ready-to-emit
#'   error message naming the duplicated ids.
#'
#' @keywords internal
#'
.plot_id_duplicate_msg <- function(points, id_col, filename) {
  ids <- points[[id_col]]
  dup <- unique(ids[duplicated(ids)])
  if (length(dup) == 0L) return(NULL)
  fe_msg("plot_id_duplicate", filename, paste(dup, collapse = ", "))
}


# ---------------------------------------------------------------------------
#' Reject inventory points without plot-centre coordinates
#'
#' Every concentric sample plot is built around its centre coordinate -- the
#' whole circle geometry (and every tree position derived from it) hangs off
#' that one point. A plot whose centre coordinate is empty cannot be placed and
#' must not travel into the pre files, where it would otherwise surface much
#' later as a cryptic \dQuote{missing values in coordinates not allowed} error
#' from \pkg{sf} when the pre reader turns the centre into a geometry. Catching
#' it here, at the raw->pre stage, turns that into a clear, early failure.
#' Called after the coordinate columns have been parsed to numeric (so an empty
#' cell is already \code{NA}). Shared by both concentric raw->pre converters so
#' they stay symmetric.
#'
#' @param points Inventory-point data frame with numeric coordinate columns.
#' @param x_col,y_col Names of the plot-centre coordinate columns.
#' @param id_col Name of the plot-identifier column (\code{"koord"} /
#'   \code{"plot_id"}).
#' @param filename The point file name, for the error message.
#'
#' @return \code{NULL} when every plot has both coordinates, otherwise a
#'   ready-to-emit error message naming the affected plots.
#'
#' @keywords internal
#'
.missing_coords_msg <- function(points, x_col, y_col, id_col, filename) {
  bad <- is.na(points[[x_col]]) | is.na(points[[y_col]])
  if (!any(bad)) return(NULL)
  fe_msg("plot_coords_missing", filename,
         paste(unique(points[[id_col]][bad]), collapse = ", "))
}


# ---------------------------------------------------------------------------
#' Detect the character encoding of a text file
#'
#' Returns the encoding of a source text file without the user having to know
#' it. The detection is deterministic for the only distinction that occurs in
#' practice for this package's German-language forestry exports -- UTF-8 vs.
#' Latin-1/Windows-1252 -- because valid UTF-8 is a self-identifying byte
#' pattern: a Latin-1 file with umlauts (e.g. a lone \code{0xE4} for "ä") is
#' almost never a valid UTF-8 byte sequence. A byte-order mark (BOM) is honoured
#' when present.
#'
#' Logic: (1) an UTF-8 BOM -> \code{"UTF-8"}; a UTF-16 BOM -> \code{"UTF-16"};
#' (2) otherwise, if the raw bytes form valid UTF-8 -> \code{"UTF-8"};
#' (3) otherwise \code{"windows-1252"} (a superset of ISO-8859-1 that also
#' covers the few CP1252 punctuation bytes, harmless for umlaut data).
#'
#' Not covered: UTF-16 without a BOM (extremely rare for these exports); such a
#' file would be mis-read and is treated as an input error elsewhere.
#'
#' @param path Path to the file (character).
#'
#' @return A single encoding string usable in \code{readr::locale(encoding=)}.
#'
#' @keywords internal
.detect_file_encoding <- function(path) {
  sz <- file.info(path)$size
  if (is.na(sz) || sz == 0) return("UTF-8")
  raw <- readBin(path, what = "raw", n = sz)

  if (length(raw) >= 3 &&
      identical(raw[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) return("UTF-8")
  if (length(raw) >= 2 &&
      (identical(raw[1:2], as.raw(c(0xFF, 0xFE))) ||
       identical(raw[1:2], as.raw(c(0xFE, 0xFF))))) return("UTF-16")

  # iconv() from UTF-8 to UTF-8 yields NA when the bytes are not valid UTF-8.
  if (isTRUE(!is.na(iconv(list(raw), from = "UTF-8", to = "UTF-8")))) {
    "UTF-8"
  } else {
    "windows-1252"
  }
}


# ---------------------------------------------------------------------------
#' Read a delimited text file with automatic encoding detection
#'
#' The single entry point every raw/pre reader in this package uses to read a
#' delimited text file, so that character encoding is handled uniformly and
#' users never have to choose one. With \code{encoding = "auto"} (the default)
#' the encoding is detected via \code{\link{.detect_file_encoding}}; any other
#' value is passed through as an explicit override for pathological cases.
#'
#' \strong{Project convention:} do not call \code{readr::read_delim()} (or set a
#' fixed \code{locale(encoding = )}) directly in a reader. Route every text-file
#' read through this helper and expose an \code{encoding = "auto"} argument on
#' the reader so the override is available. This keeps encoding handling in one
#' place and consistent across all import functions.
#'
#' @param file Path to the file (character).
#' @param delim Field delimiter. Default tab.
#' @param encoding \code{"auto"} (default) to detect, or an explicit encoding
#'   string to force.
#' @param decimal_mark Decimal mark for numeric columns. Default \code{"."};
#'   pass \code{","} for comma-decimal exports. (Numeric robustness itself is
#'   handled separately by \code{\link{.parse_numeric_cols}}; this only sets the
#'   readr locale.)
#' @param as_text If \code{TRUE}, read every column as character and defer all
#'   numeric parsing to an explicit \code{\link{.parse_numeric_cols}} step. This
#'   is the robust choice for raw user files: readr's type guessing would
#'   otherwise treat a comma decimal in a mixed column (e.g. a height column of
#'   mostly integers plus a few \code{"23,4"}) as a thousands \emph{grouping}
#'   mark and silently turn \code{"23,4"} into \code{234} at read time, before
#'   any whitelist can recover it. Reading as text sidesteps that entirely.
#'   Default \code{FALSE} (readr guesses types) for backward compatibility.
#' @param ... Passed to \code{readr::read_delim()} (e.g. \code{col_types}).
#'
#' @return A tibble, as returned by \code{readr::read_delim()}.
#'
#' @keywords internal
.read_delim_auto <- function(file, delim = "\t", encoding = "auto",
                             decimal_mark = ".", as_text = FALSE, ...) {
  enc <- if (identical(encoding, "auto")) .detect_file_encoding(file) else encoding
  grouping_mark <- if (identical(decimal_mark, ",")) "." else ","
  col_types <- if (as_text) readr::cols(.default = readr::col_character()) else NULL
  readr::read_delim(
    file,
    delim          = delim,
    trim_ws        = TRUE,
    show_col_types = FALSE,
    col_types      = col_types,
    locale         = readr::locale(encoding = enc,
                                   decimal_mark  = decimal_mark,
                                   grouping_mark = grouping_mark),
    ...
  )
}


#' Check and guess species names
#'
#' This is a convenience function that helps in the context of inconsistently
#' coded German species name abbreviations. It tries to guess correct species
#' names and connects them to species codes that are supported by the package
#' *ForestElementsR*. Despite the function applies clever techniques, a manual
#' check ot the outcome is absolutely recommended.
#'
#' Remark by Peter: The current version operates on data frames and requires
#' column naming conventions. A future streamlined version can be more generic
#' and based on named vectors.
#'
#' @param tree_list A data frame containing a column "baumart" which contains
#'   the species name abbreviations to be checked.
#'
#' @param species_table A data frame containing with the columns "name_ger",
#'   "species_id", and "abbreviation". The first two columns have to come from
#'   a species coding table provided by the package *ForestElementsR*. The
#'   third column contains the standard abbrevations that the function will try
#'   to guess from the entries in \code{tree_list$baumart}. In *FeNEU* we
#'   provide one such data frame called
#'   \code{species_abbreviations_bavrn_state}. It supports the species coding
#'   "bavrn_state" (Bavarian State Forest) supported by *ForestElementsR*.
#'
#' @return A duplicate-free data frame containing all successful matches
#'
#' @keywords internal
#'
check_and_guess_species <- function(tree_list, species_table) {

  # tree_list <- tree_list |>
  #   dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("ä", "ae", .))) |>
  #   dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("ö", "oe", .))) |>
  #   dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("ü", "ue", .)))

  # The code above generated a warning due to non-ASCII characters
  # Replaced with unicode-escapes in the following block. Upper-case umlauts
  # are normalised as well: an entry like "LAE" written for larch in all caps
  # otherwise survives this step, stays unmatched, and reaches the mojibake
  # repair below, where iconv() on an already-valid umlaut produces invalid
  # bytes that make the following gsub() abort (post-CRAN: harden that stage
  # itself, see the note in fix_mojibake()).
  tree_list <- tree_list |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\u00E4", "ae", .))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\u00F6", "oe", .))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\u00FC", "ue", .))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\u00C4", "AE", .))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\u00D6", "OE", .))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\u00DC", "UE", .)))


  # Canonicalise case up front: map any abbreviation that matches a standard
  # entry case-insensitively to the standard's canonical spelling (e.g. "KIe",
  # "fi", "SAH" -> "Kie", "Fi", "SAh"). The uppercase-exact tier below did this
  # anyway, but only after the "not in standard table" warning had already been
  # built from a case-sensitive comparison -- which drowned the warning in
  # harmless case variants. Doing it here keeps that warning focused on
  # genuinely unknown codes, and keeps the final case-sensitive check valid.
  std_abbr  <- iconv(species_table$abbreviation, from = "", to = "UTF-8")
  std_upper <- toupper(std_abbr)
  ba_utf8   <- iconv(tree_list$baumart, from = "", to = "UTF-8")
  canon_idx <- match(toupper(ba_utf8), std_upper)
  tree_list$baumart <- ifelse(is.na(canon_idx), ba_utf8, std_abbr[canon_idx])

  # Per-row normalised raw abbreviation (before canonicalisation) -- the basis
  # for the species-mapping report attached to the return value below.
  work_raw <- ba_utf8


  # check how many abbreviations are in the species_table (standard)
  check_species <- tree_list |>
    dplyr::mutate(baumart = iconv(.data$baumart, from = "", to = "UTF-8"),
                  id_standard = .data$baumart %in% species_table$abbreviation,
    ) |>
    dplyr::filter(.data$id_standard == FALSE) |>
    dplyr::select("baumart") |>
    dplyr::distinct()


  # error if check_species contains NA values
  # Peter: Code between ## and ## comes from merging in issue_38
  #        Is it required?
  ##
  if(any(is.na(check_species$baumart))){
    stop(fe_msg("species_col_has_na"))
  }
  ##

  # warning if some species do not match the standard
  if(nrow(check_species) > 0){
    warning(fe_msg("species_not_in_table_warn",
                   paste(check_species$baumart, collapse = ", ")))
  }

  # The standard table needs a UTF-8, uppercase key for both the fuzzy tiers
  # below and the final join; compute it before the early exit.
  species_table <- species_table |>
    dplyr::mutate(abbreviation = iconv(.data$abbreviation, from = "", to = "UTF-8"),
                  abbreviation_upper = toupper(.data$abbreviation))

  # After the up-front case canonicalisation, a clean data set leaves
  # `check_species` empty. Nothing is unresolved, so attach the species codes
  # and return -- skipping the rowwise substring/fuzzy matchers, which error on
  # zero rows and would have nothing to do anyway.
  if (nrow(check_species) == 0) {
    message(fe_msg("species_all_matched"))
    out <- tree_list |>
      dplyr::left_join(species_table, by = c("baumart" = "abbreviation")) |>
      dplyr::select(-"abbreviation_upper")
    # Everything resolved by exact / case-insensitive match -> all "confirmed".
    attr(out, "species_mapping") <-
      .species_map_report(work_raw, stats::setNames(character(), character()),
                          species_table)
    return(out)
  }

  # Convert to uppercase for better matching (handles case differences)
  check_species <- check_species |>
    dplyr::mutate(baumart_upper = toupper(.data$baumart))

  # Three levels of trying to find matches
  check_species_first <- check_species |>
    dplyr::mutate(baumart_repl =
                    species_table$abbreviation[
                      match(.data$baumart_upper,
                            species_table$abbreviation_upper)])



  check_species_second <- check_species |>
    dplyr::rowwise() |>
    dplyr::mutate(
      baumart_repl = ifelse(
        any(stringr::str_detect(
          species_table$abbreviation,
          .data$baumart)),  # Check if any match exists
        species_table$abbreviation[
          stringr::str_detect(species_table$abbreviation,
                              .data$baumart)][1],  # Take first match
        NA_character_  # Assign NA if no match found
      )
    ) |>
    dplyr::ungroup()


  check_species_third <- check_species |>
    dplyr::mutate(
      baumart_repl = species_table$abbreviation[
        stringdist::amatch(.data$baumart_upper,
                           species_table$abbreviation_upper,
                           method = "jw", maxDist = 0.3)]

    ) |>
    dplyr::rename(baumart_repl_fuzzy = "baumart_repl") |>  # Rename before join
    dplyr::select("baumart", "baumart_repl_fuzzy")  # Keep only necessary columns


  check_species_final <- check_species_first |>
    dplyr::left_join(check_species_second, by = "baumart",
                     suffix = c("_exact",
                                "_approx")) |>
    dplyr::left_join(check_species_third, by = "baumart") |>
    dplyr::mutate(baumart_repl = dplyr::coalesce(.data$baumart_repl_exact,
                                                 .data$baumart_repl_approx,
                                                 .data$baumart_repl_fuzzy)) |>
    dplyr::select("baumart", "baumart_repl")

  # Fourth level: recover species abbreviations that were UTF-8 in the source
  # file but got misread as ISO-8859-1/Latin-1 upstream (e.g. "Lä" ->
  # "LÃ¤"). Such strings still contain the original bytes, just
  # mis-tagged: re-encoding to raw Latin-1 recovers those bytes, and
  # re-declaring them as UTF-8 decodes the intended character. Only entries
  # still unmatched after the three tiers above are attempted here, so this is
  # a safety net for encoding mishaps rather than a first-choice match path.
  still_unmatched <- check_species_final |>
    dplyr::filter(is.na(.data$baumart_repl))

  if (nrow(still_unmatched) > 0) {

    fix_mojibake <- function(x) {
      fixed <- suppressWarnings(iconv(x, to = "latin1"))
      ok <- !is.na(fixed)
      Encoding(fixed[ok]) <- "UTF-8"
      fixed
    }

    mojibake_repl <- still_unmatched |>
      dplyr::transmute(
        baumart      = .data$baumart,
        baumart_fix  = fix_mojibake(.data$baumart)
      ) |>
      dplyr::filter(!is.na(.data$baumart_fix)) |>
      dplyr::mutate(
        baumart_fix = gsub("\u00e4", "ae", .data$baumart_fix),
        baumart_fix = gsub("\u00f6", "oe", .data$baumart_fix),
        baumart_fix = gsub("\u00fc", "ue", .data$baumart_fix),
        baumart_fix_upper = toupper(.data$baumart_fix)
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        baumart_repl_mojibake = dplyr::coalesce(
          species_table$abbreviation[
            match(.data$baumart_fix_upper, species_table$abbreviation_upper)],
          species_table$abbreviation[
            stringdist::amatch(.data$baumart_fix_upper,
                               species_table$abbreviation_upper,
                               method = "jw", maxDist = 0.3)]
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select("baumart", "baumart_repl_mojibake")

    check_species_final <- check_species_final |>
      dplyr::left_join(mojibake_repl, by = "baumart") |>
      dplyr::mutate(baumart_repl = dplyr::coalesce(.data$baumart_repl,
                                                    .data$baumart_repl_mojibake)) |>
      dplyr::select("baumart", "baumart_repl")
  }


  # SUbstitute the guesses into the tree_list
  tree_list <- tree_list |>
    dplyr::left_join(check_species_final, by = "baumart") |>
    dplyr::mutate(
      baumart_orig = .data$baumart,
      baumart = dplyr::coalesce(.data$baumart_repl, .data$baumart)) |>  # Replace if matched, keep original if NA
    dplyr::select(-"baumart_repl")  # Remove helper column


  check_species <- tree_list |>
    dplyr::mutate(id_standard = .data$baumart %in% species_table$abbreviation) |>
    dplyr::filter(.data$id_standard == FALSE) |>
    dplyr::select("baumart") |>
    dplyr::distinct()

  # Peter: Why does this error not stop the function? Better make that a
  # warning if the function's output is still useful despite the problem.
  # Errors should always break execution if not specifically taken care of
  # e.g. with tryCatch().
  if(nrow(check_species) > 0){
    stop(fe_msg("species_not_in_table",
                paste(check_species$baumart, collapse = ", ")))
  }

  message(fe_msg("species_all_matched"))

  # Print an error message if there are species in the data that are not in the
  # standard and print the list.
  # Peter: An error that does not break the execution must be a warning at most.
  #        An error means "I cannot proceed"
  out <- tree_list |>
    dplyr::left_join(species_table, by = c("baumart" = "abbreviation")) |>
    dplyr::select(-"abbreviation_upper")

  # Attach the species-mapping report: entries resolved by the substring /
  # fuzzy / mojibake tiers (i.e. everything in check_species_final) are flagged
  # tier == "review"; exact / case-insensitive matches are "confirmed".
  rev_map <- stats::setNames(check_species_final$baumart_repl,
                             check_species_final$baumart)
  attr(out, "species_mapping") <-
    .species_map_report(work_raw, rev_map, species_table)
  out

}


# ---------------------------------------------------------------------------
#' Build the species-guessing mapping report
#'
#' Summarises how each raw species abbreviation was resolved during
#' \code{\link{check_and_guess_species}}: one row per distinct (normalised) raw
#' abbreviation with its tree count, the canonical abbreviation it maps to, the
#' resulting \code{species_id}/\code{name_ger}, and a \code{tier} of either
#' \code{"confirmed"} (exact or case-insensitive match) or \code{"review"}
#' (resolved only by substring / fuzzy / mojibake matching, so worth a manual
#' check). Rows are ordered review-first, then by descending tree count.
#'
#' @param work_raw Character vector of per-row normalised raw abbreviations.
#' @param rev_map Named character vector mapping review-tier raw abbreviations
#'   to their guessed canonical abbreviation (empty when nothing needed the
#'   fuzzy tiers).
#' @param species_table The standard species table (must carry
#'   \code{abbreviation}, \code{species_id}, \code{name_ger}).
#'
#' @return A tibble with columns \code{raw}, \code{n_trees}, \code{canonical},
#'   \code{species_id}, \code{name_ger}, \code{tier}.
#'
#' @keywords internal
.species_map_report <- function(work_raw, rev_map, species_table) {
  std_abbr  <- iconv(species_table$abbreviation, from = "", to = "UTF-8")
  std_upper <- toupper(std_abbr)

  tab <- as.data.frame(table(work_raw), stringsAsFactors = FALSE)
  names(tab) <- c("raw", "n_trees")

  ci        <- match(toupper(tab$raw), std_upper)
  confirmed <- !is.na(ci)
  canonical <- ifelse(confirmed, std_abbr[ci], unname(rev_map[tab$raw]))
  sp_idx    <- match(canonical, species_table$abbreviation)

  tibble::tibble(
    raw        = tab$raw,
    n_trees    = as.integer(tab$n_trees),
    canonical  = canonical,
    species_id = species_table$species_id[sp_idx],
    name_ger   = species_table$name_ger[sp_idx],
    tier       = ifelse(confirmed, "confirmed", "review")
  ) |>
    dplyr::arrange(factor(.data$tier, levels = c("review", "confirmed")),
                   dplyr::desc(.data$n_trees))
}

#' Resolve the inventory year for a raw-to-pre conversion
#'
#' Downstream only the survey \emph{year} is used: each plot object carries a
#' single \code{time_yr}, and \code{fe_inventory()} validates that a plot covers
#' one survey year. The converters therefore need a year, not a full date.
#'
#' An explicit \code{inventory_year} always wins, and the file dates are then not
#' parsed at all. Otherwise the year is taken as the plain calendar year of the
#' per-plot file date. If neither an \code{inventory_year} nor a usable file date
#' is available, the caller must stop and ask for \code{inventory_year}.
#'
#' The vegetation-period convention (a survey in, e.g., April counts for the
#' previous growing season) is deliberately \strong{not} applied automatically --
#' that is a silvicultural judgement the user expresses through
#' \code{inventory_year}, not a hidden cut-off inside the importer.
#'
#' @param date_vec Character vector of per-plot dates from the file, or
#'   \code{NULL} when the file carries no date column.
#' @param inventory_year Optional single year overriding the file dates.
#'
#' @return A list \code{list(ok, years, key, arg)}. On success \code{ok = TRUE}
#'   and \code{years} is either a single year (override) or one year per element
#'   of \code{date_vec}. On failure \code{ok = FALSE} and \code{key} names the
#'   i18n message to report (with optional \code{arg}).
#'
#' @keywords internal
.derive_inventory_year <- function(date_vec, inventory_year = NULL) {

  if (!is.null(inventory_year)) {
    yr <- suppressWarnings(as.integer(inventory_year))
    if (length(inventory_year) != 1L || is.na(yr) ||
        yr < 1500L || yr > 2100L) {
      return(list(ok = FALSE, key = "inventory_year_invalid",
                  arg = paste(inventory_year, collapse = ", ")))
    }
    return(list(ok = TRUE, years = yr))
  }

  dv     <- as.character(date_vec)
  usable <- !is.na(dv) & nzchar(trimws(dv))
  if (!any(usable)) {
    return(list(ok = FALSE, key = "inventory_year_required"))
  }

  # detect_date_format() stop()s on an unrecognised or ambiguous format; catch
  # that and turn it into a clean fail pointing the user to inventory_year.
  fmt <- tryCatch(detect_date_format(dv), error = function(e) NA_character_)
  if (is.na(fmt)) {
    return(list(ok = FALSE, key = "inventory_year_undetectable"))
  }

  years <- as.integer(format(as.Date(dv, format = fmt), "%Y"))
  list(ok = TRUE, years = years)
}


#' Detect and validate a uniform date format in a character vector
#'
#' Attempts to determine the date format used in a character vector containing
#' date strings. A small set of expected formats is tested on a representative
#' non-empty value, and strict round-trip validation is applied. Exactly one
#' format must match; otherwise the function raises an error. This ensures that
#' all imported date fields follow a single, unambiguous, and internally
#' convertible format.
#'
#' @param date_string A character vector containing date values to be tested.
#'
#' @return A single character string representing the detected date format.
#'   If no format or more than one format matches, an informative error is
#'   thrown.
#'
#' @keywords internal
detect_date_format <- function(date_string) {

  # Candidate formats (adapt as needed)
  fmts <- c(
    "%Y-%m-%d",          # standard ISO format (most of your data)
    "%d-%m-%Y",          # Nordhalben edited format
    "%d.%m.%Y %H:%M:%S", # original BaySF with time
    "%d.%m.%Y"           # plain German date
  )

  # 1) pick one representative, non-empty value
  date_string <- as.character(date_string)
  sample <- date_string[!is.na(date_string) & nzchar(date_string)][1]

  if (length(sample) == 0 || is.na(sample)) {
    stop(fe_msg("date_no_valid_string"), call. = FALSE)
  }

  matched <- character(0)

  for (fmt in fmts) {

    parsed_one <- strptime(sample, format = fmt)

    # if parsing fails for this sample, skip this format
    if (is.na(parsed_one[1])) next

    # roundtrip: format back and compare the leading part of the original
    roundtrip <- format(parsed_one, fmt)

    if (substr(sample, 1, nchar(roundtrip)) == roundtrip) {
      matched <- c(matched, fmt)
    }
  }

  # no match at all
  if (length(matched) == 0) {
    stop(fe_msg("date_format_undetectable", sample), call. = FALSE)
  }

  # more than one possible match → ambiguous
  if (length(matched) > 1) {
    stop(fe_msg("date_format_ambiguous", sample,
                paste(matched, collapse = ", ")), call. = FALSE)
  }

  # exactly one format
  matched[1]
}


#' Require an explicit output directory
#'
#' The raw->pre converters write files, so they must be told where. Earlier
#' versions defaulted to the raw-data folder, which writes into the user's own
#' filespace -- not permitted by CRAN policy, and against the package's
#' principle of never guessing silently. \code{output_dir} is therefore a
#' mandatory argument, and this helper turns the bare "argument missing" into a
#' message that says what to pass. Shared by all three raw->pre converters so
#' they stay symmetric.
#'
#' @param output_dir The caller's \code{output_dir} argument, unevaluated-safe
#'   (checked with \code{missing()} in the caller and passed on).
#' @param fn Name of the calling function, for the error message.
#'
#' @return Invisibly \code{TRUE}; called for the error it raises.
#'
#' @keywords internal
#'
.assert_output_dir <- function(output_dir, fn) {
  if (is.null(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop(fe_msg("output_dir_missing", fn), call. = FALSE)
  }
  invisible(TRUE)
}

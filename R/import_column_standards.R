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

# Column-name alias maps for the concentric-circle raw inventory format.
#
# This is FeNEU's own user-prepared format for concentric-circle inventories
# (not a third-party software export). Column names are therefore controlled
# by FeNEU and are defined in English. For backward compatibility, the German
# equivalents are accepted as aliases: after .normalise_header() is applied to
# the user's file headers, .normalise_colnames() renames any German alias to the
# English canonical before the required-column check runs.
#
# .normalise_header() lowercases, expands German umlauts (ae/oe/ue/ss), and
# replaces spaces and punctuation with underscores. This allows column names
# such as "Laenge in m" or "Karten Nr." to be matched reliably by name instead
# of by position.
#
# The Eingabemaske (Input Mask) format, the WZP Silvarith format, the WZP
# Relascope format, and the BaySF raw format use column names fixed by
# their respective software or data standard and are NOT affected by these
# alias maps.


# Internal: normalise a vector of column names for robust alias matching.
# Steps: tolower -> German umlaut expansion -> non-alphanumeric to "_" ->
#   collapse repeated "_" -> strip leading/trailing "_".
#
# @param nms Character vector of column names.
# @return Normalised character vector.
# @keywords internal
.normalise_header <- function(nms) {
  # Recover headers that were UTF-8 in the source file but got misread as
  # ISO-8859-1/Latin-1 upstream (e.g. "Laenge in m" alias "L\u00e4nge in m" ->
  # "L\u00c3\u00a4nge in m"). The tell-tale signature is "\u00c3" (U+00C3, the lead byte
  # of a 2-byte UTF-8 sequence) immediately followed by a byte in the
  # U+0080-U+00BF range; correctly-encoded text essentially never produces
  # this pattern by coincidence, so the fix is only attempted where it's
  # detected. Re-encoding to raw Latin-1 recovers the original UTF-8 bytes;
  # re-declaring them as UTF-8 then decodes the intended character.
  mojibake <- grepl("\u00c3[\u0080-\u00bf]", nms)
  if (any(mojibake)) {
    fixed <- suppressWarnings(iconv(nms[mojibake], to = "latin1"))
    ok <- !is.na(fixed)
    Encoding(fixed[ok]) <- "UTF-8"
    nms[mojibake][ok] <- fixed[ok]
  }

  nms <- tolower(nms)
  nms <- gsub("\u00e4", "ae", nms, fixed = TRUE)   # ae (umlaut a)
  nms <- gsub("\u00f6", "oe", nms, fixed = TRUE)   # oe (umlaut o)
  nms <- gsub("\u00fc", "ue", nms, fixed = TRUE)   # ue (umlaut u)
  nms <- gsub("\u00df", "ss", nms, fixed = TRUE)   # ss (sharp s)
  nms <- gsub("[^a-z0-9_]", "_", nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  nms
}


# Internal: convert an identifier component to a plain character string.
#
# Identifier components (stand keys, plot keys, running tree numbers) must be
# character so that composite ids built with paste() are stable. When such a
# component is read as numeric, R's default as.character() switches to
# scientific notation for "round" values (e.g. 324000000 -> "3.24e+08"),
# silently corrupting the id. This helper forces a plain, non-scientific
# representation without the column padding that format() adds by default.
#
# @param x A vector used as (part of) an identifier.
# @return A character vector, free of scientific notation and padding.
# @keywords internal
.id_to_chr <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  format(x, scientific = FALSE, trim = TRUE)
}


# Internal: assert that BaySF identifier columns hold plain integer codes.
#
# In the BaySF format the plot key (bestkey, bestlfd, koord) and the tree
# numbering (lfd_satz, lfd_bhd) are integer codes. They are read as character
# (so decimal/scientific corruption stays visible instead of being silently
# normalised by numeric coercion). A value that is not a pure run of digits -
# e.g. "500000.00", "799351961.0", "5e+05" or text - means the same key is
# formatted inconsistently across the files; the (bestkey, bestlfd, koord)
# joins then fail silently and trees can no longer be matched to their plots.
# This is an upstream data error, so we stop at import with an actionable
# message rather than letting it surface later as missing trees. Whitespace is
# already stripped upstream by read_delim(trim_ws = TRUE), so no trimming here.
#
# @param df A data frame just read from a BaySF-format file.
# @param file_label Character, file name used in the error message.
# @param id_cols Identifier columns to check (only those present are checked).
# @return Invisibly \code{df}; called for its side effect (error on violation).
# @keywords internal
.assert_id_cols_integer <- function(df, file_label,
                                    id_cols = c("bestkey", "bestlfd", "koord",
                                                "lfd_satz", "lfd_bhd")) {
  for (cn in intersect(id_cols, names(df))) {
    vals <- as.character(df[[cn]])
    bad <- !is.na(vals) & !grepl("^[0-9]+$", vals)
    if (any(bad)) {
      stop(
        sprintf(
          paste0(
            "Column '%s' in %s contains non-integer identifier(s) (e.g. %s). ",
            "BaySF identifier columns (bestkey, bestlfd, koord, lfd_satz, ",
            "lfd_bhd) must be plain integer codes - no decimal point, ",
            "scientific notation, or text. Such values mean the same key is ",
            "formatted inconsistently across files, so trees can no longer be ",
            "matched to their plots. Please fix the source data."
          ),
          cn, file_label,
          paste(utils::head(unique(vals[bad]), 3L), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(df)
}


# Tree-list aliases: German (after .normalise_header) -> English canonical
.ccirc_tree_aliases <- c(
  "baumnummer"  = "tree_nr",
  "baumart"     = "species",
  "bhd"         = "dbh",
  "entfernung"  = "distance",
  "winkel"      = "angle",
  "alter"       = "age",
  "mortal"      = "mortality",
  "schicht"     = "layer",
  "hoehe"       = "height",
  "anzahl"      = "count"
)

# Tree-list numeric columns (canonical names, i.e. after the alias map above and
# after the species column has been coded to `species_id`). These hold
# measurements or numeric codes and must be parsed tolerant of the decimal mark
# (comma vs. point) via .parse_numeric_cols(). Identifiers (plot_id, tree_nr) and
# the logical `regeneration` flag are deliberately NOT listed -- keeping an
# all-numeric plot_id as text avoids coercing it to a double (scientific-notation
# risk for large round IDs). `age` is optional and simply skipped when absent.
.ccirc_tree_numeric <- c("angle", "distance", "dbh", "height", "count",
                         "mortality", "layer", "age", "species_id")

# Inventory-point aliases: German (after .normalise_header) -> English canonical.
# The plot slope is canonically `slope` (English, consistent with the other
# canonical names); the BaySF-native `neigung` is accepted as an alias. It is
# the terrain slope in PERCENT. (The former `neig_grad` column is a coarse slope
# *class*, not a measurement, and is no longer part of the format. When Format 1
# writes the BaySF pre file it renames `slope` back to `neigung`, and the
# pre-reader converts that percent value to the internal slope in degrees.)
.ccirc_point_aliases <- c(
  "repfl"   = "rep_area",
  "erfdat"  = "survey_date",
  "neigung" = "slope"
)

# Inventory-point numeric columns (canonical names, i.e. after the alias map
# above has been applied). These hold measurements and must be parsed tolerant
# of the decimal mark (comma vs. point) via .parse_numeric_cols(). plot_id is an
# identifier and survey_date is a date, so neither is listed here.
.ccirc_point_numeric <- c("coord_x", "coord_y", "rep_area", "slope")

# Circle-definition numeric columns (canonical names, i.e. after the bhd_cm_von
# -> dbh_cm_from / radius_m -> radiuses_m alias rename in
# .parse_circle_definition_df). These drive the circle geometry (radii, DBH
# thresholds) and are result-relevant, so they must be parsed tolerant of the
# decimal mark via .parse_numeric_cols(); fe_ikl is a class key, not listed here.
.ccirc_cdef_numeric <- c("dbh_cm_from", "radiuses_m")

# --- Format 2 (multi-table export) numeric column whitelists -------
#
# Format 2 keeps the source (raw, lower-cased) column names -- it does not use
# the alias machinery above. As for Format 1, the readers read every column as
# text (.read_delim_auto(as_text = TRUE)) and defer all numeric parsing to these
# whitelists, so a comma decimal in a mixed column is never silently eaten as a
# thousands grouping mark at read time. Only the columns the converter actually
# consumes are listed; identifiers (koord) stay character, and `totholz_lage`
# is deliberately kept character (it is compared to "1"/"2"). `fe_ikl` is a
# class key, not parsed here (as in Format 1).

# Format-2 tree list (02): measurements, angles, and numeric codes.
.mask_tree_numeric  <- c("bhd", "hoehe_1", "pol_entf", "pol_wink",
                         "rueckazimut_gon", "rueckazimut_grad", "alter_ba",
                         "einheit_azimut", "ba_fe", "bestku")

# Format-2 inventory points (01): coordinates, represented area, azimuth unit,
# and the optional real slope column (neigung/slope, percent).
.mask_point_numeric <- c("lage_probekreismittelpunkt_x",
                         "lage_probekreismittelpunkt_y", "repfl",
                         "einheit_azimut", "neigung", "slope")

# Format-2 basic dead wood (07): the numeric measurement / code columns consumed
# by build_fdinvtot(source = "inputmask"). `totholz_lage` stays character.
.mask_dw_numeric    <- c("dm", "hoehe", "totzer", "totbag")


# --- BaySF-style preprocessed files (pre_to_fe_inventory reader) --------------
#
# import_data_BaySF() reads the five preprocessed tables as text
# (.read_delim_auto(as_text = TRUE)) and parses only the consumed numeric columns
# listed here, so a comma decimal is never eaten as a thousands mark at read time
# and the file encoding is handled the same way as for the raw converters (the
# package-wide decimal-robustness / encoding convention). The identifier columns
# (bestkey, bestlfd, koord, lfd_satz, lfd_bhd) stay character -- they are keys,
# not numbers (used via .id_to_chr() and equality merges), and the
# .assert_id_cols_integer() guard checks their integer formatting on the
# character values. erfdat stays character (parsed by detect_date_format
# downstream); hmb is a character flag.
#
# Unlike the raw Formats 1/2 -- where fe_ikl may be the "ALL" shorthand and is
# therefore kept character -- fe_ikl is ALWAYS an integer in BaySF-style
# preprocessed data (build_fdvikrs() writes it as.integer). Parsing it as a
# number here preserves the previous behaviour and makes the fe_ikl merge between
# fdinvkrs and fdvikrs robust to formatting differences (e.g. "1" vs "1.0").
.baysf_bhd_numeric   <- c("fe_period", "bhd", "bhd_h",
                         "pol_wink", "pol_entf", "mortal", "bhd_anz")
.baysf_ba_numeric    <- c("fe_period", "ba_fe", "inv_alt", "best_ku")
.baysf_krs_numeric   <- c("fe_period", "fe_ikl", "neigung", "repfl",
                         "gauss_rw", "gauss_hw")
.baysf_vikrs_numeric <- c("fe_ikl", "bhd_von", "kreis_fl")


# --- Stand-wise relascope WZP files (Silvarith raw + WZP_Daten.txt pre) --------
#
# Both ends of the stand-wise relascope chain -- the raw converter
# (import_standwise_relascope_format1_raw_to_pre) and the pre reader
# (import_data_WZP_Relascope_toFeNEU) -- read the file as text
# (.read_delim_auto(as_text = TRUE)) and parse only the consumed numeric columns
# listed here via .parse_numeric_cols(), so a comma decimal (Silvarith is a
# German-locale export: "39,5") is never eaten as a thousands mark at read time.
# Column names are the canonical WZP names, i.e. after the umlaut/ASCII header
# has been lower-cased and the alias rename (zf -> zaehlfaktor, ba -> baumart,
# anz. -> anzahl, best.-flaeche -> bestflaeche, hoehe) has run.
#
# The identifier columns (stpnr, lfd_nr, bestand) stay character -- they are
# keys used only in paste()/n_distinct(), not numbers. `stichtag` is a date,
# parsed separately. `baumart` IS listed: it is a numeric BaySF species code fed
# to fe_species_bavrn_state() (no "ALL" shorthand exists in WZP data), and
# `schicht` IS listed: it is the numeric layer code compared to {0,1,2,3}.
.wzp_numeric <- c("bestflaeche", "zaehlfaktor", "baumart", "alter",
                  "schicht", "anzahl", "bhd", "hoehe")

# Deadwood aliases: German (after .normalise_header) -> English canonical.
# All ten columns are covered, including the five positional ones whose
# original BaySF headers contain umlauts or punctuation.
.ccirc_dw_aliases <- c(
  # columns 1-5 (matched by name after header normalisation)
  "karten_nr"               = "piece_id",    # Karten Nr.
  "y"                       = "coord_y",     # Y
  "x"                       = "coord_x",     # X
  "laenge_in_m"             = "length_m",    # Laenge in m (Laenge = length)
  "mittendurchmesser_in_cm" = "mid_diam",    # Mittendurchmesser in cm
  # columns 6-10 (by name, no umlaut issues)
  "bhd"                     = "dbh",
  "baumart"                 = "species_group",
  "grad"                    = "decay",
  "kategorie"               = "category"
)


# Internal: apply an alias map to a data frame's column names.
# Renames an alias column to its canonical equivalent only when the canonical
# name is not already present (canonical always wins over alias).
#
# @param df     A data frame whose column names are to be normalised.
# @param alias_map Named character vector: names are aliases (post-normalise),
#   values are canonical names.
# @return df with alias columns renamed to canonical names.
#
# @keywords internal
.normalise_colnames <- function(df, alias_map) {
  for (alias in names(alias_map)) {
    canonical <- alias_map[[alias]]
    if (alias %in% names(df) && !canonical %in% names(df)) {
      names(df)[names(df) == alias] <- canonical
    }
  }
  df
}

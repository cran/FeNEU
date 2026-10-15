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

# Common report constructors for the raw->pre converters. Every raw->pre
# converter (concentric format1/format2, standwise relascope) returns the same
# shape: ok / errors / warnings / paths. `_fail` short-circuits on a
# blocking problem; `_ok` reports the written pre files.
.raw_to_pre_fail <- function(errors, warnings = character()) {
  # Clear closing message so the user always sees the outcome, not just the
  # returned report object.
  message(fe_msg("pre_write_fail"))
  list(ok = FALSE, errors = errors, warnings = warnings, paths = NULL,
       species_mapping = NULL, needs_review = FALSE)
}
.raw_to_pre_ok <- function(paths, warnings = character(),
                           species_mapping = NULL, needs_review = FALSE) {
  out_dir <- if (length(paths) > 0) dirname(paths[[1]]) else "?"
  message(fe_msg("pre_write_ok", length(paths), out_dir))
  list(ok = TRUE, errors = character(), warnings = warnings, paths = paths,
       species_mapping = species_mapping, needs_review = needs_review)
}

# Shared build helpers for import_sample_concentric_format1_raw_to_pre() and
# import_sample_concentric_format2_raw_to_pre(). Each function takes already-normalised data
# (common column names, correct units) and returns the corresponding goal data
# frame. Writing to disk is left to the calling orchestrator.
#
# Common pre-conditions for the normalised_trees input:
#   koord       plot identifier
#   lfd_satz    species × layer rank within plot
#   mortal      mortality flag (0 = alive)
#   bhd         DBH in cm
#   bhd_h       height in m (NA allowed)
#   pol_wink    azimuth in degrees
#   pol_entf    distance in CENTIMETRES -- the BaySF pre convention. The raw
#               converters bring their own unit into it: Format 1 and the BW
#               reader multiply their metre values by 100, Format 2's source is
#               already in cm. On the way out, make_object_lists() divides by
#               100 again. (This line said "in m" up to and including 3.0.2,
#               and Format 1 did what the line said.)
#   bhd_anz     count; 1 for every tree with a position (it is identified by
#               it), > 1 only for records without position
#   hmb         height-measured flag ("J" / NA)
#   ba_fe       numeric species code
#   best_ku     layer code (integer)
#   bestkey, bestlfd, fe_period  administrative keys (all 1)


# ---------------------------------------------------------------------------
#' Build fdinvbhd from normalised tree data
#'
#' @param normalised_trees Tibble with common normalised columns (see file
#'   header for required column names).
#'
#' @return Tibble ready to be written as \code{fdinvbhd.txt}.
#'
#' @keywords internal
build_fdinvbhd <- function(normalised_trees) {

  normalised_trees |>
    dplyr::group_by(.data$koord, .data$lfd_satz) |>
    dplyr::mutate(lfd_bhd = dplyr::row_number()) |>
    dplyr::ungroup()

}


# ---------------------------------------------------------------------------
#' Build fdinvba from normalised tree data
#'
#' @param normalised_trees Tibble with common normalised columns.
#'
#' @return Tibble ready to be written as \code{fdinvba.txt}.
#'
#' @keywords internal
build_fdinvba <- function(normalised_trees) {

  normalised_trees |>
    dplyr::group_by(.data$koord, .data$lfd_satz) |>
    dplyr::summarise(
      ba_fe     = as.numeric(dplyr::first(.data$ba_fe)),
      best_ku   = dplyr::first(.data$best_ku),
      bestkey   = dplyr::first(.data$bestkey),
      bestlfd   = dplyr::first(.data$bestlfd),
      fe_period = dplyr::first(.data$fe_period),
      inv_alt   = dplyr::first(.data$inv_alt),
      .groups   = "drop"
    )

}


# ---------------------------------------------------------------------------
#' Build fdinvkrs from normalised inventory-point data
#'
#' @param normalised_points Tibble with common normalised columns:
#'   \code{koord}, \code{gauss_rw}, \code{gauss_hw}, \code{erfdat},
#'   \code{bestkey}, \code{bestlfd}, \code{fe_period}.
#'   Any additional columns are passed through unchanged.
#'
#' @return Tibble ready to be written as \code{fdinvkrs.txt}.
#'
#' @keywords internal
build_fdinvkrs <- function(normalised_points) {
  normalised_points
}


# ---------------------------------------------------------------------------
#' Build fdvikrs (circle definitions)
#'
#' Returns the standard fdvikrs data frame.  When \code{circle_definitions}
#' is \code{NULL} (the default), a single circle class (\code{fe_ikl = 1})
#' is created from \code{bhds_from} and \code{radiuses}.  When
#' \code{circle_definitions} is supplied, it must be a data frame with
#' columns \code{fe_ikl}, \code{bhd_von}, and \code{radius}, and multiple
#' circle classes are created.
#'
#' @param bhds_from  Numeric vector of lower DBH limits per circle.
#'   Ignored when \code{circle_definitions} is not \code{NULL}.
#' @param radiuses   Numeric vector of circle radii in metres.
#'   Ignored when \code{circle_definitions} is not \code{NULL}.
#' @param circle_definitions  Optional data frame with columns
#'   \code{fe_ikl} (integer circle-class key), \code{bhd_von} (numeric, cm),
#'   and \code{radius} (numeric, m).  Default \code{NULL}.
#'
#' @return Data frame ready to be written as \code{fdvikrs.txt}.
#'
#' @keywords internal
build_fdvikrs <- function(bhds_from = NULL,
                          radiuses = NULL,
                          circle_definitions = NULL) {

  if (!is.null(circle_definitions)) {
    req <- c("fe_ikl", "bhd_von", "radius")
    missing <- setdiff(req, names(circle_definitions))
    if (length(missing) > 0) {
      stop(fe_msg("circle_def_cols_missing",
                  paste(missing, collapse = ", ")), call. = FALSE)
    }
    fdvikrs <- data.frame(
      bhd_von  = circle_definitions$bhd_von,
      kreis_fl = (circle_definitions$radius^2 * pi) / 10000,
      radius   = circle_definitions$radius,
      fe_ikl   = as.integer(circle_definitions$fe_ikl)
    )
  } else {
    fdvikrs <- data.frame(
      bhd_von  = bhds_from,
      kreis_fl = (radiuses^2 * pi) / 10000,
      radius   = radiuses,
      fe_ikl   = 1L
    )
  }
  fdvikrs
}


# ---------------------------------------------------------------------------
#' Build fcbestku (standard BaySF layer definition)
#'
#' Returns the standard BaySF layer code table. This default is used whenever
#' no external \code{layers_filename} is supplied to the orchestrator.
#'
#' @return Data frame ready to be written as \code{fcbestku.txt}.
#'
#' @keywords internal
build_fcbestku <- function() {

  data.frame(
    best_ku    = c("ungeschichtet", "Oberschicht", "Unterstand",
                   "Vorausverjuengung", "Nachhiebsrest", "Ueberhaelter",
                   "Altbaum", "Abgang Mortal"),
    best_ku_l  = c("UG", "OS", "US", "VVJ", "NHR", "UEH", "ALT", "+++"),
    best_ku_k  = c(1, 1, 2, 3, 4, 4, 4, 1),
    best_ku_g  = c(-1, 0, 0, 0, 0, 0, 0, -1),
    gueltig_id = c(0, 1, 2, 3, 4, 5, 6, -1),
    stringsAsFactors = FALSE
  )

}


# ---------------------------------------------------------------------------
#' Build fdinvtot from normalised deadwood data
#'
#' Converts normalised deadwood measurements into the \code{fdinvtot} format
#' used by the BaySF processing chain. Per BaySF instructions (Erstanlage
#' 2024, §6.2.8), deadwood is always recorded on a fixed 500 m² circle
#' (radius 12.62 m), independent of the live-tree concentric circles.
#'
#' Volume per piece is estimated with a half-cylinder approximation
#' (form factor 0.5):
#' \deqn{v_{ha} = 0.5 \cdot \pi \cdot \left(\frac{d_{cm}}{200}\right)^2
#'       \cdot h_m \cdot n_{ha}}
#'
#' Two input sources are supported via the \code{source} argument:
#'
#' \strong{\code{"inputmask"}} (Eingabemaske / input mask app export) — normalised input must
#' contain:
#'   \describe{
#'     \item{koord}{Plot identifier.}
#'     \item{lfd_tot}{Sequential deadwood piece number within plot.}
#'     \item{totholz_lage}{Position code: \code{"1"} = standing,
#'       \code{"2"} = lying.  Standing pieces cannot be distinguished as whole
#'       vs. partial stem, so all are mapped to \code{totkat = 91}.}
#'     \item{totzer}{Decomposition grade (integer 1–5).}
#'     \item{totbag}{Species group code; \code{4} (Nicht bestimmbar) is
#'       recoded to \code{0}.}
#'     \item{dm}{Mid-diameter in \strong{mm} (converted to cm internally).}
#'     \item{hoehe}{Length / height in \strong{dm} (converted to m internally).}
#'     \item{fe_nr, fe_period, bestkey, bestlfd}{Administrative keys.}
#'   }
#'
#' \strong{\code{"concentric"}} (Kraus / concentric-circle survey) — normalised
#' input must contain:
#'   \describe{
#'     \item{koord}{Plot identifier.}
#'     \item{lfd_tot}{Sequential deadwood piece number within plot.}
#'     \item{kategorie}{Position code: \code{1} = stehend ganzer Stamm
#'       (\code{totkat = 90}), \code{2} = stehend Stammteil
#'       (\code{totkat = 91}), \code{3} = liegend (\code{totkat = 92}).
#'       Any other value is mapped to \code{NA} and filtered out.}
#'     \item{grad}{Decomposition grade (integer 1–3), mapped directly to
#'       \code{totzer}.}
#'     \item{totbag}{Species group code: 1 = Eiche, 2 = Nadelholz,
#'       3 = Laubholz, 4 = nicht zuzuordnen (recoded to 0).}
#'     \item{dm}{Mid-diameter in \strong{cm} (no conversion needed).}
#'     \item{hoehe}{Length / height in \strong{m} (no conversion needed).}
#'     \item{fe_nr, fe_period, bestkey, bestlfd}{Administrative keys.}
#'   }
#'
#' @param normalised_deadwood Tibble with source-specific columns as described
#'   above.
#' @param source Character scalar: \code{"inputmask"} (default) or
#'   \code{"concentric"}.
#'
#' @return Tibble ready to be written as \code{fdinvtot.txt} with columns
#'   \code{fe_period}, \code{bestkey}, \code{bestlfd}, \code{koord},
#'   \code{lfd_tot}, \code{totkat}, \code{totzer}, \code{totbag}, \code{dm}
#'   (cm), \code{hoehe} (m), \code{fe_nr}, \code{v_ha}, \code{n_ha}.
#'
#' @keywords internal
build_fdinvtot <- function(normalised_deadwood, source = "inputmask") {

  # Fixed deadwood circle: 500 m² per BaySF protocol (§6.2.8, Erstanlage 2024)
  deadwood_circle_ha <- 500 / 10000   # 0.05 ha
  deadwood_n_ha      <- 1 / deadwood_circle_ha  # 20

  if (source == "inputmask") {

    normalised_deadwood |>
      dplyr::filter(!is.na(.data$totholz_lage)) |>
      dplyr::mutate(
        # totkat: position code per BaySF key
        # Input mask cannot distinguish whole stem (90) from partial stem (91);
        # all standing pieces are mapped to 91 as a conservative default
        totkat = dplyr::case_when(
          .data$totholz_lage == "1" ~ 91L,
          .data$totholz_lage == "2" ~ 92L,
          TRUE                      ~ NA_integer_
        ),
        totzer = as.integer(.data$totzer),
        # totbag: "4" (Nicht bestimmbar) -> 0
        totbag = dplyr::case_when(
          as.integer(.data$totbag) == 4L ~ 0L,
          !is.na(.data$totbag)           ~ as.integer(.data$totbag),
          TRUE                           ~ NA_integer_
        ),
        dm_cm   = .data$dm    / 10,   # mm -> cm
        hoehe_m = .data$hoehe / 10,   # dm -> m
        v_ha    = 0.5 * pi * (.data$dm_cm / 200)^2 * .data$hoehe_m *
                  deadwood_n_ha,
        n_ha    = deadwood_n_ha
      ) |>
      dplyr::select(
        "fe_period", "bestkey", "bestlfd", "koord",
        "lfd_tot", "totkat", "totzer", "totbag",
        dm = "dm_cm", hoehe = "hoehe_m",
        "fe_nr", "v_ha", "n_ha"
      )

  } else if (source == "concentric") {

    normalised_deadwood |>
      dplyr::mutate(
        # kategorie 1 = stehend ganzer Stamm -> 90
        # kategorie 2 = stehend Stammteil   -> 91
        # kategorie 3 = liegend             -> 92
        totkat = dplyr::case_when(
          as.integer(.data$kategorie) == 1L ~ 90L,
          as.integer(.data$kategorie) == 2L ~ 91L,
          as.integer(.data$kategorie) == 3L ~ 92L,
          TRUE                              ~ NA_integer_
        ),
        totzer = as.integer(.data$grad),
        # totbag: 4 (nicht zuzuordnen) -> 0
        totbag = dplyr::case_when(
          as.integer(.data$totbag) == 4L ~ 0L,
          !is.na(.data$totbag)           ~ as.integer(.data$totbag),
          TRUE                           ~ NA_integer_
        ),
        # units already in cm / m — no conversion needed
        dm_cm   = as.numeric(.data$dm),
        hoehe_m = as.numeric(.data$hoehe),
        v_ha    = 0.5 * pi * (.data$dm_cm / 200)^2 * .data$hoehe_m *
                  deadwood_n_ha,
        n_ha    = deadwood_n_ha
      ) |>
      dplyr::filter(!is.na(.data$totkat)) |>
      dplyr::select(
        "fe_period", "bestkey", "bestlfd", "koord",
        "lfd_tot", "totkat", "totzer", "totbag",
        dm = "dm_cm", hoehe = "hoehe_m",
        "fe_nr", "v_ha", "n_ha"
      )

  } else {
    stop(fe_msg("source_invalid"))
  }

}

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
#' Normalise plot center coordinates and attach CRS metadata
#'
#' Internal helper to standardise plot center coordinates and associate them
#' with a coordinate reference system (CRS). The function resolves the CRS from
#' a short key, applies Gauss-Krueger zone-prefix correction if required, and
#' stores CRS information as attributes.
#'
#' Supported CRS keys (Germany-focused):
#' \describe{
#'   \item{lonlat}{WGS84 geographic coordinates (EPSG:4326)}
#'   \item{utm32}{ETRS89 / UTM zone 32N (EPSG:25832)}
#'   \item{utm33}{ETRS89 / UTM zone 33N (EPSG:25833)}
#'   \item{gk2}{DHDN / Gauss-Krueger zone 2 (EPSG:31466)}
#'   \item{gk3}{DHDN / Gauss-Krueger zone 3 (EPSG:31467)}
#'   \item{gk4}{DHDN / Gauss-Krueger zone 4 (EPSG:31468)}
#'   \item{gk5}{DHDN / Gauss-Krueger zone 5 (EPSG:31469)}
#'   \item{epsg}{User-supplied EPSG code}
#' }
#'
#' For Gauss-Krueger systems, the function automatically checks whether the
#' Rechtswert appears to be missing the zone prefix and adds it if necessary.
#'
#' @param coordinate A data frame containing plot center coordinates.
#' @param coord_sys Character CRS key (see Details). Default is "gk4".
#' @param epsg Optional integer EPSG code, used only if coord_sys = "epsg".
#' @param x_col Name of the x coordinate column. Default "gauss_rw".
#' @param y_col Name of the y coordinate column. Default "gauss_hw".
#'
#' @return The input data frame with unchanged structure, but with CRS metadata
#'   attached as attributes:
#'   \describe{
#'     \item{fe_crs}{An \code{sf::crs} object}
#'     \item{fe_coord_sys}{Resolved CRS key}
#'     \item{fe_coord_cols}{Coordinate column names}
#'   }
#'
#' @keywords internal
fe_normalise_coordinates <- function(coordinate,
                                            coord_sys = coord_sys,
                                            epsg = NULL,
                                            x_col = "gauss_rw",
                                            y_col = "gauss_hw",
                                            coord_sys_declared = TRUE,
                                            check_envelope = TRUE) {

  if (!is.data.frame(coordinate)) {
    stop(fe_msg("coord_not_dataframe"), call. = FALSE)
  }

  if (!(x_col %in% names(coordinate)) ||
      !(y_col %in% names(coordinate))) {
    stop(fe_msg("coord_cols_missing", x_col, y_col), call. = FALSE)
  }

  coord_sys <- tolower(as.character(coord_sys))

  # Resolve CRS
  if (coord_sys == "epsg") {
    if (is.null(epsg) || length(epsg) != 1L || !is.numeric(epsg)) {
      stop(fe_msg("coord_epsg_invalid"), call. = FALSE)
    }
    crs <- sf::st_crs(as.integer(epsg))
  } else if (coord_sys %in% names(fe_crs_registry)) {
    crs <- sf::st_crs(as.integer(fe_crs_registry[[coord_sys]]))
  } else {
    stop(fe_msg("coord_sys_unknown", coord_sys,
                paste(names(fe_crs_registry), collapse = ", ")),
         call. = FALSE)
  }

  # Gauss-Krueger zone prefix. Storing the easting WITHOUT the zone prefix is the
  # normal BaySF form (the gold-standard files do it), and a six-digit easting is
  # not valid Gauss-Krueger on its own -- so adding the prefix is not a guess but
  # a direct consequence of the user DECLARING a gk* system. It is applied, with
  # a message.
  # It only becomes a guess when nobody declared anything and the gk* default
  # merely kicked in: the very same six-digit range is also what UTM eastings
  # look like. Silently mangling undeclared data is exactly the failure we refuse
  # to risk, so that case stops and asks for an explicit coord_sys.
  if (coord_sys %in% c("gk2", "gk3", "gk4", "gk5")) {

    zone <- as.integer(sub("^gk", "", coord_sys))
    x_vals <- coordinate[[x_col]]
    x_med  <- stats::median(x_vals, na.rm = TRUE)

    #  missing prefix if values are clearly too small
    if (is.finite(x_med) && x_med < 2000000) {
      if (!isTRUE(coord_sys_declared)) {
        stop(
          fe_msg("coord_sys_undeclared",
                 format(x_med, scientific = FALSE),
                 coord_sys,
                 format(zone * 1000000, scientific = FALSE)),
          call. = FALSE
        )
      }
      coordinate[[x_col]] <- x_vals + zone * 1000000
      message(
        fe_msg("coord_gk_prefix_missing",
               format(x_med, scientific = FALSE),
               coord_sys,
               format(zone * 1000000, scientific = FALSE))
      )
    }
  }

  # WGS84 bbox sanity check: catches mismatched coord_sys (e.g. UTM data
  # passed as gk4, or lonlat passed as something projected). Envelope is
  # German-speaking Europe with margin (lat 47-56, lon 5-16). Skipped if
  # all coordinates are non-finite.
  x_vals_chk <- coordinate[[x_col]]
  y_vals_chk <- coordinate[[y_col]]
  finite_idx <- is.finite(x_vals_chk) & is.finite(y_vals_chk)

  if (isTRUE(check_envelope) && any(finite_idx)) {
    pts <- sf::st_as_sf(
      data.frame(x = x_vals_chk[finite_idx], y = y_vals_chk[finite_idx]),
      coords = c("x", "y"), crs = crs
    )
    pts_wgs <- sf::st_transform(pts, 4326)
    bbox    <- sf::st_bbox(pts_wgs)

    envelope_lon <- c(5, 16)
    envelope_lat <- c(47, 56)

    if (bbox[["xmin"]] < envelope_lon[1] ||
        bbox[["xmax"]] > envelope_lon[2] ||
        bbox[["ymin"]] < envelope_lat[1] ||
        bbox[["ymax"]] > envelope_lat[2]) {
      # A soft plausibility hint, not a hard invariant: FeNEU must not be
      # Germany-only, so a legitimate inventory elsewhere may not be blocked.
      # It stays a warning (not a message) because the failure mode it guards
      # against -- a mismatched coord_sys -- silently invalidates every area,
      # per-hectare and increment result.
      warning(
        fe_msg("coord_outside_envelope",
               envelope_lon[1], envelope_lon[2],
               envelope_lat[1], envelope_lat[2],
               coord_sys,
               sprintf("lon %.2f-%.2f, lat %.2f-%.2f.",
                       bbox[["xmin"]], bbox[["xmax"]],
                       bbox[["ymin"]], bbox[["ymax"]])),
        call. = FALSE
      )
    }
  }

  # Geographic (lon/lat) input MUST be reprojected to a metric CRS before any
  # geometry is built: ForestElementsR::fe_ccircle_spatial() places trees at
  # centre + (R*sin(angle), R*cos(angle)) with R in METRES and then only stamps
  # the centre's CRS -- a planar, metric assumption. In degrees a 10 m offset
  # would silently become 10 degrees (~1000 km), and the envelope check would
  # NOT catch it, because the plot centres themselves are perfectly plausible.
  # The UTM zone follows the data's own median longitude.
  if (sf::st_is_longlat(crs) && any(finite_idx)) {
    lon_med  <- stats::median(x_vals_chk[finite_idx])
    utm_zone <- as.integer(floor((lon_med + 180) / 6) + 1)
    target   <- sf::st_crs(32600L + utm_zone)   # WGS84 / UTM north

    pts <- sf::st_as_sf(
      data.frame(x = x_vals_chk[finite_idx], y = y_vals_chk[finite_idx]),
      coords = c("x", "y"), crs = crs
    )
    xy <- sf::st_coordinates(sf::st_transform(pts, target))
    coordinate[[x_col]][finite_idx] <- xy[, 1]
    coordinate[[y_col]][finite_idx] <- xy[, 2]

    message(fe_msg("coord_reprojected", coord_sys, utm_zone, 32600L + utm_zone))
    crs       <- target
    coord_sys <- paste0("utm", utm_zone)
  }

  # Attach metadata
  attr(coordinate, "fe_crs") <- crs
  attr(coordinate, "fe_coord_sys") <- coord_sys
  attr(coordinate, "fe_coord_cols") <- c(x_col, y_col)

  coordinate
}


# Internal: turn a coordinate-system declaration found in raw data into a FeNEU
# CRS key.
#
# Two spellings are accepted, because the two raw formats differ:
#   * a FeNEU key, exactly as in the coord_sys argument -- "gk4", "lonlat", ...
#     (Format 1's own optional `coord_sys` column)
#   * an EPSG declaration -- "EPSG:4326", "epsg: 4326", "4326", or the code
#     (Format 2's `lage_probekreismittelpunkt_srs`, which is Format 2's own format and
#     not ours to rename)
#
# Anything else -- empty, "GUESS!!" (a real value in some exports), an EPSG the
# registry does not support, or two genuinely DIFFERENT systems in one file --
# yields NA, i.e. "no usable declaration", so the caller falls back to the
# coord_sys argument rather than acting on nonsense.
#
# Note that the same system spelled in more than one way is NOT such a case.
# Real exports do that: an Amberg point file carries 65 rows saying "4326" and
# 59 saying "EPSG:4326". Deciding on the raw strings made that look ambiguous
# and threw away a declaration the file states perfectly clearly, so the
# resolution happens first and the comparison afterwards.
#
# @param x The declaration column (or a single value).
# @return A single CRS key from fe_crs_registry, or NA_character_.
# @keywords internal
.parse_coord_sys_decl <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x) & !toupper(x) %in% c("NA", "NULL")]
  if (length(x) == 0L) return(NA_character_)

  # One declaration -> one key, or NA when it cannot be read at all.
  resolve_one <- function(u) {
    # A FeNEU key, spelled as in the coord_sys argument
    if (tolower(u) %in% names(fe_crs_registry)) return(tolower(u))

    # ... or an EPSG declaration
    code <- suppressWarnings(
      as.integer(sub("^epsg[[:space:]:]*", "", u, ignore.case = TRUE))
    )
    if (is.na(code)) return(NA_character_)     # e.g. "GUESS!!"

    hit <- names(fe_crs_registry)[match(code, fe_crs_registry)]
    if (length(hit) != 1L || is.na(hit)) return(NA_character_)
    hit
  }

  keys <- vapply(unique(x), resolve_one, character(1), USE.NAMES = FALSE)

  # A value we cannot read still devalues the whole column: a typo is not
  # distinguishable from a system we do not support, and neither is something
  # to build on.
  if (anyNA(keys)) return(NA_character_)

  keys <- unique(keys)
  if (length(keys) != 1L) return(NA_character_)  # two different systems
  keys
}


# Internal: does the declared CRS fit the magnitude of the coordinates?
#
# The one discriminator that always holds: geographic coordinates are small
# (|lon| <= 180, |lat| <= 90), projected ones are not. That is enough to catch
# the two mix-ups that actually occur -- lon/lat declared as a projected system,
# and projected values declared as lon/lat.
#
# @return TRUE/FALSE, or NA when there is nothing finite to judge.
# @keywords internal
.coord_sys_plausible <- function(coord_sys, x, y) {
  fin <- is.finite(x) & is.finite(y)
  if (!any(fin)) return(NA)
  is_geo    <- isTRUE(sf::st_is_longlat(sf::st_crs(as.integer(
    fe_crs_registry[[coord_sys]]))))
  looks_geo <- max(abs(x[fin])) <= 180 && max(abs(y[fin])) <= 90
  identical(is_geo, looks_geo)
}


# Internal: resolve the CRS for a raw->pre converter.
#
# The CRS may come from the raw data itself (an optional declaration column --
# Format 1's own `coord_sys`, Format 2's `lage_probekreismittelpunkt_srs`) or
# from the caller's coord_sys argument. Both are checked for presence AND
# plausibility; nothing is guessed:
#   * declaration in the file and in the argument disagree -> error, name both
#   * neither -> error, ask for coord_sys
#   * the declaration contradicts the magnitude of the coordinates -> error with
#     a concrete suggestion the user may then declare (at their own risk)
#
# Shared by Format 1 and Format 2 so both behave identically.
#
# @param points Inventory-point data frame (raw, columns already lower-cased).
# @param coord_sys The caller's coord_sys argument, or NULL.
# @param x_col,y_col Coordinate column names in `points`.
# @param decl_col Name of the optional declaration column, or NULL.
# @return list(coord_sys = <key>, msg = <message or NULL>)
# @keywords internal
.resolve_raw_coord_sys <- function(points, coord_sys = NULL,
                                   x_col, y_col, decl_col = NULL) {

  decl_key <- if (!is.null(decl_col) && decl_col %in% names(points)) {
    .parse_coord_sys_decl(points[[decl_col]])
  } else {
    NA_character_
  }

  if (!is.null(coord_sys)) {
    coord_sys <- tolower(as.character(coord_sys))
    if (!coord_sys %in% names(fe_crs_registry)) {
      stop(fe_msg("coord_sys_unknown", coord_sys,
                  paste(names(fe_crs_registry), collapse = ", ")),
           call. = FALSE)
    }
  }

  # The file says one thing, the caller another: never pick a winner silently.
  if (!is.null(coord_sys) && !is.na(decl_key) && !identical(coord_sys, decl_key)) {
    stop(fe_msg("coord_sys_conflict", decl_col, decl_key, coord_sys), call. = FALSE)
  }

  resolved <- if (!is.null(coord_sys)) coord_sys else decl_key
  if (is.na(resolved)) {
    stop(fe_msg("coord_sys_needed", paste(names(fe_crs_registry), collapse = ", ")),
         call. = FALSE)
  }

  ok <- .coord_sys_plausible(resolved, points[[x_col]], points[[y_col]])
  if (isFALSE(ok)) {
    looks_geo <- max(abs(points[[x_col]]), na.rm = TRUE) <= 180
    stop(
      fe_msg("coord_sys_implausible", resolved,
             if (looks_geo) fe_msg("coord_looks_geographic")
             else fe_msg("coord_looks_projected"),
             if (looks_geo) "'lonlat', 'etrs89'" else "'gk2'-'gk5', 'utm32', 'utm33'"),
      call. = FALSE
    )
  }

  msg <- if (is.null(coord_sys)) fe_msg("coord_sys_from_file", decl_col, resolved)
         else NULL
  list(coord_sys = resolved, msg = msg)
}


# ---------------------------------------------------------------------------
#' Convert plot-centre coordinates to a metric CRS for the pre files
#'
#' The BaySF-style pre columns are named \code{gauss_rw}/\code{gauss_hw} but, by
#' convention, carry a \strong{projected metric} CRS -- real BaySF exports store
#' UTM32 there (the \dQuote{gauss} name is a legacy misnomer; verified on native
#' FDINVKRS files, whose \code{GAUSS_RW}/\code{GAUSS_HW} hold UTM32 and whose
#' \code{GAUSS_KZ} zone field is unreliable). FeNEU follows that convention when
#' it writes a pre: \strong{geographic} input (\code{lonlat} / ETRS89-geographic)
#' is reprojected to UTM32 (EPSG:25832, ETRS89 / UTM zone 32N) so a pre file
#' never holds degrees in a \code{gauss_*} column, while already-projected input
#' (\code{gk*}, \code{utm*}) is passed through unchanged. The returned
#' \code{coord_sys} describes the returned coordinates and is what the converter
#' writes into the pre's \code{coord_sys} column, keeping the pre
#' self-describing.
#'
#' @param x,y Numeric plot-centre coordinates in the CRS named by
#'   \code{coord_sys}.
#' @param coord_sys FeNEU CRS key of the input coordinates.
#'
#' @return \code{list(x, y, coord_sys)}: the (possibly reprojected) coordinates
#'   and the key of their CRS.
#'
#' @keywords internal
#'
.coords_to_pre_metric <- function(x, y, coord_sys) {
  if (!coord_sys %in% c("lonlat", "etrs89")) {
    return(list(x = x, y = y, coord_sys = coord_sys))
  }
  src <- sf::st_crs(as.integer(fe_crs_registry[[coord_sys]]))
  fin <- is.finite(x) & is.finite(y)
  if (any(fin)) {
    pts <- sf::st_as_sf(data.frame(x = x[fin], y = y[fin]),
                        coords = c("x", "y"), crs = src)
    xy  <- sf::st_coordinates(sf::st_transform(pts, 25832L))
    x[fin] <- xy[, 1]
    y[fin] <- xy[, 2]
  }
  list(x = x, y = y, coord_sys = "utm32")
}


#' Internal CRS registry
#'
#' A named vector mapping short CRS keys used internally in FeNEU to EPSG codes.
#' The registry defines the coordinate reference systems supported by inventory
#' import and conversion functions.
#'
#' Supported CRS keys:
#' \describe{
#'   \item{lonlat}{WGS84 geographic coordinates (EPSG:4326)}
#'   \item{etrs89}{ETRS89 geographic coordinates (EPSG:4258)}
#'   \item{utm32}{ETRS89 / UTM zone 32N (EPSG:25832)}
#'   \item{utm33}{ETRS89 / UTM zone 33N (EPSG:25833)}
#'   \item{gk2}{DHDN / Gauss-Krueger zone 2 (EPSG:31466)}
#'   \item{gk3}{DHDN / Gauss-Krueger zone 3 (EPSG:31467)}
#'   \item{gk4}{DHDN / Gauss-Krueger zone 4 (EPSG:31468)}
#'   \item{gk5}{DHDN / Gauss-Krueger zone 5 (EPSG:31469)}
#' }
#'
#' @keywords internal
fe_crs_registry <- c(
  lonlat = 4326,
  etrs89 = 4258,
  utm32  = 25832,
  utm33  = 25833,
  gk2    = 31466,
  gk3    = 31467,
  gk4    = 31468,
  gk5    = 31469
)













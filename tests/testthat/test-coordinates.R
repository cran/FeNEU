# Coordinate handling: how a declared vs. defaulted CRS is treated, the metric
# reprojection of geographic input, and the envelope plausibility hint.

# A copy of the ex3 pre with the coord_sys column stripped, mimicking a genuine
# BaySF export (which has no such column). The bundled ex3 pre IS self-describing
# (coord_sys = utm32), so these tests -- about the undeclared-stop and the
# declared-gk4 behaviour -- need the column-less variant to exercise them.
.ex3_pre_no_coordsys <- function() {
  src <- system.file("extdata", "data_ex3_sample_pre", package = "FeNEU")
  skip_if(identical(src, ""), "data_ex3_sample_pre not available")
  d <- tempfile(); dir.create(d)
  file.copy(list.files(src, full.names = TRUE), d)
  krs <- readr::read_delim(file.path(d, "fdinvkrs.txt"), delim = "\t",
                           show_col_types = FALSE)
  krs$coord_sys <- NULL
  readr::write_delim(krs, file.path(d, "fdinvkrs.txt"), delim = "\t")
  d
}

test_that("an undeclared coord_sys stops when the eastings look un-prefixed", {
  # BaySF stores six-digit eastings; reading them as gk4 needs the zone prefix.
  # With coord_sys defaulted (and no coord_sys column), adding it would be a
  # guess -> stop and ask.
  expect_error(
    suppressMessages(suppressWarnings(
      import_sample_concentric_pre_to_fe_inventory(.ex3_pre_no_coordsys())
    )),
    "coord_sys"
  )
})

test_that("a declared Gauss-Krueger coord_sys adds the zone prefix and imports", {
  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(.ex3_pre_no_coordsys(),
                                                 coord_sys = "gk4")
  ))
  expect_true(is_fe_inventory(inv))
})

test_that("the zone prefix is added with a message, never silently", {
  crd <- data.frame(gauss_rw = c(675300, 675461), gauss_hw = c(5570000, 5570100))
  expect_message(
    out <- suppressWarnings(
      fe_normalise_coordinates(crd, coord_sys = "gk4", coord_sys_declared = TRUE)
    ),
    "prefix"
  )
  expect_equal(out$gauss_rw, c(4675300, 4675461))   # zone 4 prefix applied
})

test_that("geographic coordinates are reprojected to a metric UTM CRS", {
  # Tree positions are built as centre + metric offset, so degrees must never
  # reach the geometry step.
  crd <- data.frame(gauss_rw = c(11.8718505, 11.7846398),
                    gauss_hw = c(49.4540415, 49.3779063))
  expect_message(
    out <- fe_normalise_coordinates(crd, coord_sys = "lonlat"),
    "UTM"
  )
  expect_identical(attr(out, "fe_coord_sys"), "utm32")        # 11.9 deg E
  expect_equal(sf::st_crs(attr(out, "fe_crs"))$epsg, 32632L)
  expect_false(sf::st_is_longlat(attr(out, "fe_crs")))
  # Degrees are gone; metric eastings/northings are in place.
  expect_gt(min(out$gauss_rw), 100000)
  expect_gt(min(out$gauss_hw), 1000000)
})

test_that(".parse_coord_sys_decl accepts real declarations and rejects junk", {
  expect_identical(.parse_coord_sys_decl("EPSG:4326"), "lonlat")
  expect_identical(.parse_coord_sys_decl("epsg: 4326"), "lonlat")
  expect_identical(.parse_coord_sys_decl("4326"), "lonlat")
  expect_identical(.parse_coord_sys_decl(rep("EPSG:31468", 3)), "gk4")
  # Junk / absent / genuinely ambiguous / unsupported -> "no usable declaration"
  expect_true(is.na(.parse_coord_sys_decl("GUESS!!")))          # real Winter value
  expect_true(is.na(.parse_coord_sys_decl(c("", NA))))
  expect_true(is.na(.parse_coord_sys_decl(c("4326", "31468")))) # two systems
  expect_true(is.na(.parse_coord_sys_decl("EPSG:3857")))        # not in the registry
})

test_that("one system spelled two ways is not an ambiguity", {
  # A real Amberg point file states the same system in two spellings: 65 rows
  # "4326" and 59 rows "EPSG:4326". Comparing the raw strings made that look
  # ambiguous, so the declaration was discarded and the import asked for a
  # coord_sys argument the file did not need.
  expect_identical(
    .parse_coord_sys_decl(c(rep("4326", 65), rep("EPSG:4326", 59))),
    "lonlat"
  )
  # The FeNEU key and its EPSG code are the same system too, in any case and
  # with the whitespace real files carry.
  expect_identical(.parse_coord_sys_decl(c("lonlat", "EPSG:4326", "4326")),
                   "lonlat")
  expect_identical(.parse_coord_sys_decl(c("gk4", "GK4", " epsg: 31468 ")),
                   "gk4")

  # What must NOT become permissive: an unreadable value still devalues the
  # whole column, because a typo cannot be told from a system we do not carry.
  expect_true(is.na(.parse_coord_sys_decl(c("4326", "GUESS!!"))))
  expect_true(is.na(.parse_coord_sys_decl(c("4326", "EPSG:3857"))))
  # ... and two spellings of two different systems are still two systems.
  expect_true(is.na(.parse_coord_sys_decl(c("EPSG:4326", "gk4"))))
})

test_that("a point file declaring one system in two spellings imports", {
  # The end-to-end consequence: without the coord_sys argument, and with the
  # declaration column split over two spellings, the CRS is still read off the
  # file.
  pts <- data.frame(
    x = c(11.87, 11.78, 11.91),
    y = c(49.45, 49.38, 49.41),
    coord_sys = c("4326", "EPSG:4326", "4326")
  )
  expect_identical(.parse_coord_sys_decl(pts$coord_sys), "lonlat")
})

test_that(".resolve_raw_coord_sys takes the CRS from the file, argument overrides", {
  pts <- data.frame(x = c(11.87, 11.78), y = c(49.45, 49.38),
                    coord_sys = "EPSG:4326")
  res <- .resolve_raw_coord_sys(pts, coord_sys = NULL,
                                x_col = "x", y_col = "y", decl_col = "coord_sys")
  expect_identical(res$coord_sys, "lonlat")
  expect_match(res$msg, "coord_sys")      # says where it came from

  # A FeNEU key spelled as in the argument is accepted just as well.
  pts_key <- pts; pts_key$coord_sys <- "lonlat"
  expect_identical(
    .resolve_raw_coord_sys(pts_key, coord_sys = NULL, x_col = "x", y_col = "y",
                           decl_col = "coord_sys")$coord_sys,
    "lonlat"
  )

  # Same declaration passed explicitly: no conflict, no message needed.
  res2 <- .resolve_raw_coord_sys(pts, coord_sys = "lonlat",
                                 x_col = "x", y_col = "y", decl_col = "coord_sys")
  expect_identical(res2$coord_sys, "lonlat")
  expect_null(res2$msg)
})

test_that(".resolve_raw_coord_sys refuses to settle a conflict or to guess", {
  pts <- data.frame(x = c(11.87, 11.78), y = c(49.45, 49.38), coord_sys = "EPSG:4326")
  # File says lonlat, caller says gk4 -> never silently pick one.
  expect_error(
    .resolve_raw_coord_sys(pts, coord_sys = "gk4",
                           x_col = "x", y_col = "y", decl_col = "coord_sys"),
    "onflict|iderspruch"
  )
  # No usable declaration anywhere -> ask, do not default.
  bare <- data.frame(x = c(11.87, 11.78), y = c(49.45, 49.38))
  expect_error(
    .resolve_raw_coord_sys(bare, coord_sys = NULL, x_col = "x", y_col = "y"),
    "coord_sys"
  )
})

test_that(".resolve_raw_coord_sys catches a declaration that contradicts the values", {
  # The derived Winter files: srs says EPSG:4326, but x/y are Gauss-Krueger.
  derived <- data.frame(x = c(4490815, 4490701), y = c(5479689, 5479847),
                        coord_sys = "EPSG:4326")
  expect_error(
    .resolve_raw_coord_sys(derived, coord_sys = NULL,
                           x_col = "x", y_col = "y", decl_col = "coord_sys"),
    "projected|projiziert"
  )
  # And the other way round: lon/lat declared as Gauss-Krueger.
  native <- data.frame(x = c(11.87, 11.78), y = c(49.45, 49.38))
  expect_error(
    .resolve_raw_coord_sys(native, coord_sys = "gk4", x_col = "x", y_col = "y"),
    "geographic|geographisch"
  )
})

test_that("the CRS travels from the raw data through the pre files to the object", {
  # Way (b): the raw file declares its CRS (Format 2's native srs column), the
  # converter resolves it once and writes it into fdinvkrs, and the pre reader
  # picks it up -- no second declaration anywhere.
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  pts <- data_ex2_sample_raw_points
  n   <- nrow(pts)
  # Geographic coordinates around Amberg, as the native Format-2 export has them.
  pts$lage_probekreismittelpunkt_x   <- 11.78 + seq_len(n) / 1000
  pts$lage_probekreismittelpunkt_y   <- 49.37 + seq_len(n) / 1000
  pts$lage_probekreismittelpunkt_srs <- "EPSG:4326"
  readr::write_delim(pts, file.path(in_dir, "01_root_entity.txt"), delim = "\t")
  readr::write_delim(data_ex2_sample_raw_trees,
                     file.path(in_dir, "02_probekreis.txt"), delim = "\t")

  # No coord_sys argument: the file's own declaration must be enough.
  # (inventory_year is supplied because ex2's native date_created is DD-MM-YY.)
  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      input_path = in_dir, output_dir = out_dir,
      dbh_cm_from = c(0, 12, 30), radiuses_m = c(2., 6.31, 12.62),
      inventory_year = 2025)))
  expect_true(res$ok)

  krs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_true("coord_sys" %in% names(krs))
  # Geographic input is reprojected to UTM32 when the pre is written (the
  # gauss_rw/gauss_hw columns hold a metric CRS by convention), so the carried
  # key is "utm32" and the eastings are metric, not degrees.
  expect_true(all(krs$coord_sys == "utm32"))
  expect_true(all(krs$gauss_rw > 1e5))

  # The pre reader needs no declaration either -- it reads the carried "utm32"
  # and uses the already-metric coordinates directly (no further reprojection).
  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(out_dir)))
  expect_true(is_fe_inventory(inv))
})

test_that("the envelope is a warning, not an error, and can be switched off", {
  # Real GK4 eastings read as UTM32 land far outside German-speaking Europe.
  crd <- data.frame(gauss_rw = c(4490815, 4490701), gauss_hw = c(5479689, 5479847))

  expect_warning(
    fe_normalise_coordinates(crd, coord_sys = "utm32"),
    "envelope"
  )
  expect_silent(
    fe_normalise_coordinates(crd, coord_sys = "utm32", check_envelope = FALSE)
  )
})

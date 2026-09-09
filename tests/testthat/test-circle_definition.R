mkdir_tmp <- function() {
  d <- tempfile("cdef")
  dir.create(d)
  d
}

test_that("generate_circle_definition writes a uniform fe_ikl = ALL file", {
  d <- mkdir_tmp()
  p <- generate_circle_definition(d, dbh_cm_from = c(30, 0, 12),
                                  radiuses_m = c(12.62, 2.82, 5.64))
  expect_true(file.exists(p))
  df <- readr::read_delim(p, delim = "\t", show_col_types = FALSE)
  expect_setequal(names(df), c("fe_ikl", "dbh_cm_from", "radiuses_m"))
  expect_true(all(df$fe_ikl == "ALL"))
  # rows sorted by lower DBH limit
  expect_equal(df$dbh_cm_from, c(0, 12, 30))
  expect_equal(df$radiuses_m, c(2.82, 5.64, 12.62))
})

test_that("generate_circle_definition rejects bad arguments", {
  d <- mkdir_tmp()
  expect_error(generate_circle_definition(d, c(0, 12), c(2.82)))
  expect_error(generate_circle_definition(d, numeric(0), numeric(0)))
})

test_that(".parse_circle_definition_df handles the ALL (uniform) case", {
  raw <- data.frame(fe_ikl = "ALL", bhd_cm_von = c(0, 12, 30),
                    radius_m = c(2.82, 5.64, 12.62))
  cd <- .parse_circle_definition_df(raw)
  expect_true(cd$uniform)
  expect_setequal(names(cd$circle_definitions), c("fe_ikl", "bhd_von", "radius"))
  expect_true(all(cd$circle_definitions$fe_ikl == 1L))
  expect_equal(cd$circle_definitions$radius, c(2.82, 5.64, 12.62))
})

test_that(".parse_circle_definition_df parses comma-decimal radii (German locale)", {
  # .read_delim_auto reads with decimal_mark = "."; a comma-decimal file therefore
  # arrives with radius/dbh as character ("2,82"). The parser must accept it, not
  # turn it into NA (which would silently corrupt the circle geometry).
  raw <- data.frame(
    fe_ikl      = "ALL",
    dbh_cm_from = c("0", "12", "30"),
    radiuses_m  = c("2,82", "5,64", "12,62"),
    stringsAsFactors = FALSE
  )
  cd <- .parse_circle_definition_df(raw)
  expect_true(cd$uniform)
  expect_equal(cd$circle_definitions$bhd_von, c(0, 12, 30))
  expect_equal(cd$circle_definitions$radius, c(2.82, 5.64, 12.62))

  # a genuinely unparseable token warns (not silent NA) and yields NA for it
  raw_bad <- data.frame(
    fe_ikl      = "ALL",
    dbh_cm_from = c("0", "12"),
    radiuses_m  = c("2.82", "not_a_number"),
    stringsAsFactors = FALSE
  )
  expect_warning(cd_bad <- .parse_circle_definition_df(raw_bad))
  expect_true(is.na(cd_bad$circle_definitions$radius[2]))
})

test_that(".parse_circle_definition_df keeps per-class fe_ikl definitions", {
  raw <- data.frame(
    fe_ikl     = c(1, 1, 2, 2),
    bhd_cm_von = c(0, 12, 0, 13),
    radius_m   = c(2.82, 5.64, 3.09, 6.0)
  )
  cd <- .parse_circle_definition_df(raw)
  expect_false(cd$uniform)
  expect_equal(sort(unique(cd$circle_definitions$fe_ikl)), c(1L, 2L))
})

test_that(".parse_circle_definition_df errors on ALL mixed with classes", {
  raw <- data.frame(fe_ikl = c("ALL", "2"), bhd_cm_von = c(0, 0),
                    radius_m = c(2.82, 3.09))
  expect_error(.parse_circle_definition_df(raw), "ALL")
})

test_that(".parse_circle_definition_df errors on missing columns", {
  raw <- data.frame(fe_ikl = "ALL", radius_m = 2.82)
  expect_error(.parse_circle_definition_df(raw))
})

test_that(".assign_circle_class: uniform sets class 1 and warns on multi fe_ikl", {
  # no fe_ikl column -> silent, all class 1
  ip <- data.frame(plot_id = c("a", "b"))
  out <- .assign_circle_class(ip, uniform = TRUE)
  expect_true(all(out$fe_ikl == 1L))

  # multiple distinct fe_ikl but a uniform definition -> warning, forced to 1
  ip2 <- data.frame(plot_id = c("a", "b"), fe_ikl = c(1L, 2L))
  out2 <- expect_warning(.assign_circle_class(ip2, uniform = TRUE))
  expect_true(all(out2$fe_ikl == 1L))
})

test_that("Probekreisdefinition file == dbh_cm_from/radiuses_m (Format 2, ex2)", {
  skip_below_depth("full")
  bh <- c(0, 12, 30); rd <- c(2.0, 6.31, 12.62)

  # Format 2's slope handling is still deferred, so its converter does not emit
  # the `neigung` column that the pre-reader now requires. Inject a flat
  # neigung = 0 into the generated pre as a stopgap so this circle-definition
  # test (unrelated to slope) can run. Remove once Format 2 writes `neigung`.
  inject_neigung <- function(dir) {
    f   <- file.path(dir, "fdinvkrs.txt")
    krs <- readr::read_delim(f, delim = "\t", show_col_types = FALSE,
                             col_types = readr::cols(.default = readr::col_character()))
    krs$neigung <- 0
    readr::write_delim(krs, f, delim = "\t")
  }

  nrep <- function(inv) {
    v <- unlist(lapply(seq_len(nrow(inv)), function(i) {
      p <- inv$plot[[i]]
      if (inherits(p, "fe_ccircle_spatial")) round(p$trees$n_rep_ha, 4) else NULL
    }))
    sort(unique(v))
  }

  d1 <- mkdir_tmp()
  readr::write_delim(data_ex2_sample_raw_trees, file.path(d1, "tree.txt"), delim = "\t")
  readr::write_delim(data_ex2_sample_raw_points, file.path(d1, "points.txt"), delim = "\t")
  import_sample_concentric_format2_raw_to_pre(
    d1, output_dir = d1,
    treelist_filename = "tree", inv_punkt_filename = "points",
    dbh_cm_from = bh, radiuses_m = rd, inventory_year = 2025)
  inject_neigung(d1)
  inv_args <- suppressWarnings(import_sample_concentric_pre_to_fe_inventory(d1))

  d2 <- mkdir_tmp()
  readr::write_delim(data_ex2_sample_raw_trees, file.path(d2, "tree.txt"), delim = "\t")
  readr::write_delim(data_ex2_sample_raw_points, file.path(d2, "points.txt"), delim = "\t")
  generate_circle_definition(d2, dbh_cm_from = bh, radiuses_m = rd)
  import_sample_concentric_format2_raw_to_pre(
    d2, output_dir = d2,
    treelist_filename = "tree", inv_punkt_filename = "points",
    circle_def_filename = "Probekreisdefinition", inventory_year = 2025)
  inject_neigung(d2)
  inv_file <- suppressWarnings(import_sample_concentric_pre_to_fe_inventory(d2))

  expect_equal(nrep(inv_args), nrep(inv_file))
})

test_that("a concentric importer fails (report) without a circle definition", {
  d <- mkdir_tmp()
  readr::write_delim(data_ex2_sample_raw_trees, file.path(d, "tree.txt"), delim = "\t")
  readr::write_delim(data_ex2_sample_raw_points, file.path(d, "points.txt"), delim = "\t")
  res <- suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      d, output_dir = d,
      treelist_filename = "tree", inv_punkt_filename = "points"))
  expect_false(res$ok)
})

test_that("a concentric importer fails when both file and vectors are given", {
  d <- mkdir_tmp()
  readr::write_delim(data_ex2_sample_raw_trees, file.path(d, "tree.txt"), delim = "\t")
  readr::write_delim(data_ex2_sample_raw_points, file.path(d, "points.txt"), delim = "\t")
  generate_circle_definition(d, dbh_cm_from = c(0, 12, 30),
                             radiuses_m = c(2.0, 6.31, 12.62))
  res <- suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      d, output_dir = d,
      treelist_filename = "tree", inv_punkt_filename = "points",
      circle_def_filename = "Probekreisdefinition",
      dbh_cm_from = c(0, 12, 30), radiuses_m = c(2.0, 6.31, 12.62)))
  expect_false(res$ok)
})

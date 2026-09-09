# Tests for the decimal-mark-tolerant numeric parsing primitive
# (.parse_numeric_vec / .parse_numeric_cols) and its first use site, the
# concentric Format-1 inventory-point reader (the "Steinfeld" comma-decimal
# hardening).

# --- vector helper ----------------------------------------------------------

test_that(".parse_numeric_vec reads comma and point decimals alike", {
  expect_equal(.parse_numeric_vec(c("1,69", "2.5", "3")), c(1.69, 2.5, 3))
})

test_that(".parse_numeric_vec passes an already-numeric vector through unchanged", {
  x <- c(1.5, 2.5, NA)
  expect_identical(.parse_numeric_vec(x), x)
})

test_that(".parse_numeric_vec maps blanks and NA to NA", {
  expect_equal(.parse_numeric_vec(c("", NA, "1,0")), c(NA, NA, 1))
})

test_that(".parse_numeric_vec handles negative and point values", {
  expect_equal(.parse_numeric_vec(c("-1,5", "-2.5", "+3")), c(-1.5, -2.5, 3))
})

test_that(".parse_numeric_vec warns and NA-fills on unparseable tokens", {
  expect_warning(
    res <- .parse_numeric_vec(c("1,5", "abc"), "rep_area"),
    "rep_area"
  )
  expect_equal(res, c(1.5, NA))
})

# --- column-targeted wrapper ------------------------------------------------

test_that(".parse_numeric_cols only touches the listed columns", {
  df <- data.frame(
    a    = c("1,5", "2,5"),
    note = c("gut, aber schief", "ok"),   # legitimate comma in a text field
    stringsAsFactors = FALSE
  )
  out <- .parse_numeric_cols(df, "a")
  expect_equal(out$a, c(1.5, 2.5))
  expect_identical(out$note, df$note)     # text comma left intact
})

test_that(".parse_numeric_cols skips absent columns without error", {
  df <- data.frame(a = "1,0", stringsAsFactors = FALSE)
  out <- .parse_numeric_cols(df, c("a", "does_not_exist"))
  expect_equal(out$a, 1)
})

test_that(".parse_numeric_cols handles mixed decimal marks within one column", {
  df <- data.frame(x = c("1.5", "2,5", "3"), stringsAsFactors = FALSE)
  out <- .parse_numeric_cols(df, "x")
  expect_equal(out$x, c(1.5, 2.5, 3))
})

# --- integration: Format-1 inventory-point reader ---------------------------

test_that("format1 point reader accepts comma-decimal rep_area (Steinfeld)", {
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  readr::write_delim(data_ex1_sample_raw_trees,
                     file.path(in_dir, "tree_layer.txt"), delim = "\t")
  readr::write_delim(data_ex1_sample_raw_smalltrees,
                     file.path(in_dir, "ingrowth.txt"), delim = "\t")

  # Emulate a German-locale export: comma decimals in the numeric point columns.
  pts <- data_ex1_sample_raw_points
  to_comma <- function(x) gsub("\\.", ",", format(x, trim = TRUE, scientific = FALSE))
  pts$rep_area <- to_comma(pts$rep_area)
  pts$coord_x  <- to_comma(pts$coord_x)
  pts$coord_y  <- to_comma(pts$coord_y)
  readr::write_delim(pts, file.path(in_dir, "inventory_points.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_true(res$ok)

  krs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_type(krs$repfl, "double")
  expect_false(anyNA(krs$repfl))
  expect_true(all(krs$repfl > 0))
})

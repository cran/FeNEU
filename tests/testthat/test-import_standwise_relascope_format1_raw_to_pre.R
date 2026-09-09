# Tests for import_standwise_relascope_format1_raw_to_pre() -- the stand-wise
# relascope raw->pre converter (Silvarith-style EingabedatenGesamt.txt ->
# preprocessed WZP_Daten.txt). Replaces the former test-relascope_wzp_plausi.R;
# the two-vendor concept and wzp_sniff_vendor() are gone.

sil_dir <- system.file("extdata", "data_ex6_standwise_raw", package = "FeNEU")

test_that("it converts a Silvarith-style raw file and writes WZP_Daten.txt", {
  out_dir <- tempfile(); dir.create(out_dir)
  res <- import_standwise_relascope_format1_raw_to_pre(
    sil_dir, output_dir = out_dir
  )
  expect_true(res$ok)
  expect_length(res$errors, 0)
  expect_true(file.exists(res$paths))
  expect_identical(basename(res$paths), "WZP_Daten.txt")
})

test_that("its output feeds the pre->fe_inventory reader", {
  out_dir <- tempfile(); dir.create(out_dir)
  import_standwise_relascope_format1_raw_to_pre(
    sil_dir, output_dir = out_dir
  )
  # Internal pre -> list reader still produces the canonical shape ...
  inv_list <- import_data_WZP_Relascope_toFeNEU(out_dir)
  expect_s3_class(inv_list, "tbl_df")
  expect_named(inv_list, c("plot_id", "trees", "area_rep_ha"))

  # ... and the public pre -> fe_inventory wrapper builds the object.
  inv <- import_standwise_relascope_pre_to_fe_inventory(out_dir)
  expect_true(is_fe_inventory(inv))
})

test_that("read_and_convert_data reads the standwise chain end to end", {
  # Coverage gap closed: the standwise route THROUGH the read_and_convert_data
  # dispatcher (raw->pre, then dispatcher -> fe_stand object) is otherwise
  # untested package-wide -- every other dispatcher caller is concentric BaySF.
  out_dir <- tempfile(); dir.create(out_dir)
  import_standwise_relascope_format1_raw_to_pre(
    sil_dir, output_dir = out_dir
  )

  inv <- read_and_convert_data(
    out_dir,
    inventory_type = "standwise_relascope",
    style          = "silvarith"
  )
  expect_true(is_fe_inventory(inv))
  expect_s3_class(inv$plot[[1]], "fe_stand")

  # style = NULL defaults to "silvarith" and yields the same object
  inv_default <- read_and_convert_data(
    out_dir,
    inventory_type = "standwise_relascope"
  )
  expect_equal(inv_default, inv)
})

test_that("it collects a missing-required-column error without throwing", {
  in_dir <- tempfile(); dir.create(in_dir)
  # ASCII header lacking Best.-Flaeche / Hoehe -> those canonical columns are
  # missing after the umlaut rename map fails to match.
  writeLines(c(
    "STPNr\tlfdNrSTP\tBestand\tZF\tBA\tAlter\tSchicht\tANZ.\tBHD\tStichtag",
    "1\t1\t7- 2- 0- - 0\t2\t51\t110\t0\t11\t39,5\t01.01.2023"
  ), file.path(in_dir, "trees.txt"))

  res <- import_standwise_relascope_format1_raw_to_pre(
    in_dir, output_dir = in_dir, treelist_filename = "trees")
  expect_false(res$ok)
  expect_match(res$errors, "Missing required column", all = FALSE)
  expect_null(res$paths)
})

test_that("it collects multiple independent value errors in one pass", {
  in_dir <- tempfile(); dir.create(in_dir)
  # Valid Silvarith-style header (umlaut columns built ASCII-safe via intToUtf8)
  flaeche <- intToUtf8(c(66L, 101L, 115L, 116L, 46L, 45L, 70L, 108L, 228L,
                         99L, 104L, 101L))            # "Best.-Fläche"
  hoehe_h <- intToUtf8(c(72L, 246L, 104L, 101L))       # "Höhe"
  header  <- paste("STPNr", "lfdNrSTP", "Bestand", flaeche, "ZF", "BA",
                   "Alter", "Schicht", "ANZ.", "BHD", hoehe_h, "Stichtag",
                   sep = "\t")
  # One row with three independent problems: schicht 9, negative BHD, bad date
  row_bad <- paste("1", "1", "7- 2- 0- - 0", "3,33", "2", "51", "110", "9",
                   "11", "-39,5", "31,0", "notadate", sep = "\t")

  con <- file(file.path(in_dir, "trees.txt"), encoding = "UTF-8")
  writeLines(c(header, row_bad), con)
  close(con)

  res <- import_standwise_relascope_format1_raw_to_pre(
    in_dir, output_dir = in_dir, treelist_filename = "trees")
  expect_false(res$ok)
  expect_length(res$errors, 3)
  expect_match(res$errors, "bhd",      all = FALSE)
  expect_match(res$errors, "schicht",  all = FALSE)
  expect_match(res$errors, "stichtag", all = FALSE)
  expect_null(res$paths)
})

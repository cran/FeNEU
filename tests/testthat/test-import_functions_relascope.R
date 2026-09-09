# Tests for the strict WZP importers.
#
# Focus: the Silvarith/Relascope `Schicht` -> ForestElementsR `layer_key`
# mapping. This mapping was silently wrong before (Ueberhaelter/Nachhiebsreste
# were not routed to the veteran layer) and had no test, which is how the bug
# slipped through. Both strict importers share the identical case_when, so the
# Relascope importer is exercised here as the regression guard.

test_that("import_data_WZP_Relascope_toFeNEU maps Schicht to fe_stand layer_key", {
  # Silvarith/Relascope Schicht  ->  ForestElementsR layer_key
  #   0  Hauptschicht            ->  1  (main stand)
  #   1  Nachhiebsreste          ->  4  (remnant / veteran layer)
  #   2  Ueberhaelter            ->  4  (remnant / veteran layer)
  #   3  Unter-/Zwischenschicht  ->  2  (understorey)
  dir <- tempfile(); dir.create(dir)
  writeLines(c(
    "STPNr\tlfdNrSTP\tBestand\tBest.-Flaeche\tZF\tBA\tAlter\tSchicht\tANZ.\tBHD\tHoehe\tStichtag",
    "1\t1\t1- 1- 0- - 0\t5.0\t4\t10\t50\t0\t2\t30\t20\t01-01-23",
    "1\t2\t1- 1- 0- - 0\t5.0\t4\t10\t50\t1\t2\t30\t20\t01-01-23",
    "1\t3\t1- 1- 0- - 0\t5.0\t4\t10\t50\t2\t2\t30\t20\t01-01-23",
    "1\t4\t1- 1- 0- - 0\t5.0\t4\t10\t50\t3\t2\t30\t20\t01-01-23"
  ), file.path(dir, "WZP_Daten.txt"))

  inv <- import_data_WZP_Relascope_toFeNEU(dir)
  expect_equal(nrow(inv), 1L)              # all four rows share one plot
  trees <- inv$trees[[1]]

  mapping <- trees[order(trees$schicht), c("schicht", "layer_key")]
  expect_equal(mapping$schicht,     c(0, 1, 2, 3))
  expect_identical(mapping$layer_key, c(1L, 4L, 4L, 2L))

  # layer_key must never leave the range fe_stand() accepts
  expect_true(all(trees$layer_key %in% 1:4))
})


test_that("import_standwise_relascope_format1_stand_register_bundle builds fe_inv + stand_data via the two-stage chain", {
  # Etappe 3c: import_standwise_relascope_format1_stand_register_bundle no longer uses a direct Silvarith importer;
  # it routes trees through import_standwise_relascope_format1_raw_to_pre() +
  # import_standwise_relascope_pre_to_fe_inventory() and pairs them with the
  # stand metadata reader.
  sil <- system.file("extdata", "data_ex6_standwise_raw", package = "FeNEU")
  skip_if(identical(sil, ""), "data_ex6_standwise_raw not available")

  bundle <- suppressWarnings(
    import_standwise_relascope_format1_stand_register_bundle(sil))

  expect_true(is_fe_inventory(bundle$fe_inv))
  expect_s3_class(bundle$fe_inv$plot[[1]], "fe_stand")
  expect_s3_class(bundle$stand_data, "tbl_df")
  expect_true("bestand" %in% names(bundle$stand_data))
})

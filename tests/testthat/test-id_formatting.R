

# Identifier components must never reach a composite id (plot_id, tree_id) in
# scientific notation. A round numeric stand key like 324000000 was being
# coerced to "3.24e+08" by paste()/as.character(), silently corrupting plot_id.

test_that(".id_to_chr formats identifiers without scientific notation", {
  # the original bug value: round numeric -> plain integer string
  expect_identical(.id_to_chr(324000000), "324000000")
  expect_identical(.id_to_chr(320101000), "320101000")
  # vectors must not be space-padded to a common width
  expect_identical(.id_to_chr(c(5e5, 7)), c("500000", "7"))
  # character and factor inputs pass through unchanged (as character)
  expect_identical(.id_to_chr("3.24E+08"), "3.24E+08")
  expect_identical(.id_to_chr(factor(c("a", "b"))), c("a", "b"))
})


test_that("BaySF import keeps plot_id character and free of scientific notation", {
  inv_path <- system.file("extdata", package = "FeNEU") |>
    paste0("/data_ex3_sample_pre")

  inv <- read_and_convert_data(inv_path, inventory_type = "sample_concentric",
                               coord_sys = "gk4") |>
    suppressWarnings() |>
    suppressMessages()

  expect_type(inv$plot_id, "character")
  expect_false(
    any(grepl("[0-9](\\.[0-9]+)?[eE][+-][0-9]{2}", inv$plot_id))
  )
})


test_that(".assert_id_cols_integer rejects non-integer BaySF identifiers", {
  ok <- data.frame(bestkey = c("799351961", "500000"),
                   bestlfd = c("1", "0"),
                   koord   = c("113701221", "3010001"),
                   stringsAsFactors = FALSE)
  expect_invisible(.assert_id_cols_integer(ok, "fdinvbhd.txt"))

  # decimal-formatted key (the Traunstein "500000.00" / Pegnitz "799351961.0")
  dec <- ok; dec$bestkey[1] <- "500000.00"
  expect_error(.assert_id_cols_integer(dec, "fdinvbhd.txt"),
               "non-integer identifier")
  dec2 <- ok; dec2$bestkey[2] <- "799351961.0"
  expect_error(.assert_id_cols_integer(dec2, "fdinvba.txt"), "fdinvba.txt")

  # scientific notation and text are rejected too
  sci <- ok; sci$bestkey[1] <- "5e+05"
  expect_error(.assert_id_cols_integer(sci, "fdinvkrs.txt"), "non-integer")
  txt <- ok; txt$koord[1] <- "ABC"
  expect_error(.assert_id_cols_integer(txt, "fdinvkrs.txt"), "non-integer")

  # only columns that are present are checked; NA values are ignored
  partial <- data.frame(bestkey = c("500000", NA), stringsAsFactors = FALSE)
  expect_invisible(.assert_id_cols_integer(partial, "fdinvkrs.txt"))
})


test_that("validate_fe_inventory rejects scientific-notation plot_id", {
  inv <- data_ex3_sample_fe_inventory

  # R-style mis-formatting of a numeric id
  bad_r <- inv
  bad_r$plot_id[1] <- "3.24e+08_0_113701221"
  expect_error(validate_fe_inventory(bad_r), "scientific notation")

  # Excel-style upper-case export of a numeric id
  bad_excel <- inv
  bad_excel$plot_id[1] <- "3.24E+08_0_113701221"
  expect_error(validate_fe_inventory(bad_excel), "scientific notation")

  # the unmodified object must still validate cleanly
  expect_silent(validate_fe_inventory(inv))
})

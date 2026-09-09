# Tests for the pre->fe_inventory wrappers (Weg a: they return the object).

test_that("concentric pre->fe_inventory builds an fe_inventory", {
  inv_path <- system.file("extdata", "data_ex3_sample_pre",
                          package = "FeNEU")
  skip_if(inv_path == "", "bundled BaySF example data not available")
  # The bundled ex3 pre carries a coord_sys column (utm32), so no argument is
  # needed -- the pre reader is self-describing.
  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(inv_path)
  ))
  expect_true(is_fe_inventory(inv))
})

test_that("standwise pre->fe_inventory builds an fe_inventory", {
  sil_dir <- system.file("extdata", "data_ex6_standwise_raw", package = "FeNEU")
  out_dir <- tempfile(); dir.create(out_dir)
  import_standwise_relascope_format1_raw_to_pre(sil_dir, output_dir = out_dir)
  inv <- suppressMessages(suppressWarnings(
    import_standwise_relascope_pre_to_fe_inventory(out_dir)
  ))
  expect_true(is_fe_inventory(inv))
})

test_that("pre->fe_inventory rejects an unsupported style", {
  inv_path <- system.file("extdata", "data_ex3_sample_pre",
                          package = "FeNEU")
  skip_if(inv_path == "", "bundled BaySF example data not available")
  expect_error(
    import_sample_concentric_pre_to_fe_inventory(inv_path, style = "bawue")
  )
})

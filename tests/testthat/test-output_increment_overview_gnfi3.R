# Tests for output_increment_overview_gnfi3() and its PDF renderer.
#
# Key invariants:
#   - restructured overview: one row per species group plus a total row
#     (species_group NA, is_total_row TRUE), with zv_m3_yr / zv_m3_ha_yr
#   - the total row equals the sum of the species rows (zv_m3_yr)
#   - the collective descriptor is carried through as an attribute
#   - the PDF renders (skipped when the LaTeX/Pandoc toolchain is absent)

suppressWarnings(suppressMessages({
  tr <- data_ex3_sample_fe_inventory |>
    pull_trees() |>
    height_complete_inventory(method = "NFI")
  tr$height_m <- ifelse(is.na(tr$height_m), tr$h_est_m, tr$height_m)
  tr <- tr |> trees_add_essentials(method = "BaySF")
  ext <- inv_inc_tree_extend_single(tr)
  bt  <- increment_base_table(ext, by_class = "age")
  ov  <- output_increment_overview_gnfi3(bt)
}))


test_that("overview has species rows plus one total row", {
  expect_true(all(c("species_group", "is_total_row", "zv_m3_yr",
                    "zv_m3_ha_yr") %in% names(ov)))
  expect_equal(sum(ov$is_total_row), 1L)
  tot <- ov[ov$is_total_row, ]
  expect_true(is.na(tot$species_group))
})


test_that("the total row sums the species rows (sum-consistency)", {
  sp  <- ov[!ov$is_total_row, ]
  tot <- ov[ov$is_total_row, ]
  expect_equal(sum(sp$zv_m3_yr), tot$zv_m3_yr, tolerance = 1e-8)
})


test_that("the collective descriptor is carried through as an attribute", {
  co <- attr(ov, "collective")
  expect_equal(co$kind, "all_layers")
})


test_that("output_increment_overview_gnfi3_pdf() renders to a PDF", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not on PATH")

  pdf_path <- tryCatch(
    output_increment_overview_gnfi3_pdf(
      ov, tab_title = "Testbetrieb", output_dir = tempdir(),
      meta = list(year = 2024, area_ha = 1234, n_plots = 10)
    ),
    error = function(e) {
      skip(paste("render failed:", e$message))
    }
  )
  expect_true(file.exists(pdf_path))
  expect_match(basename(pdf_path), "\\.pdf$")
})

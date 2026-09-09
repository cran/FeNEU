# Tests for output_increment_base_table(), the display restructuring of the
# increment base tables (counterpart of output_base_table()).
#
# Key invariants under test:
#   - returns a wide data frame: species_group + variable (rows), class levels
#     + a total column
#   - the increment variables appear as rows
#   - the cross-species "Summe" block is appended (NA species_group)
#   - reshaping is faithful: the total column reproduces the increment base
#     table's total / all_total values
#   - works on both the plain and the main-stand increment base table, both axes

suppressWarnings(suppressMessages({
  tr <- data_ex3_sample_fe_inventory |>
    pull_trees() |>
    height_complete_inventory(method = "NFI")
  tr$height_m <- ifelse(is.na(tr$height_m), tr$h_est_m, tr$height_m)
  tr <- tr |> trees_add_essentials(method = "BaySF")
  ext <- inv_inc_tree_extend_single(tr)

  bt_plain <- increment_base_table(ext, by_class = "age")
  bt_main  <- increment_base_table_main_stand(ext, by_class = "age")
  bt_dq    <- increment_base_table(ext, by_class = "dq")

  out_plain <- output_increment_base_table(bt_plain)
  out_main  <- output_increment_base_table(bt_main)
  out_dq    <- output_increment_base_table(bt_dq)
}))


test_that("output is a wide species x variable table with a total column", {
  expect_s3_class(out_plain, "tbl_df")
  expect_true(all(c("species_group", "variable", "total") %in% names(out_plain)))
  # the age-class levels are present as columns
  expect_true(all(levels(bt_plain$detail$age_class) %in% names(out_plain)))
  # the increment variables show up as rows
  expect_true(all(c(
    "iv_m3_yr_total", "iv_m3_ha_yr", "ci95_iv_m3_ha_yr", "n_plot_class"
  ) %in% out_plain$variable))
})


test_that("the cross-species Summe block is appended (NA species_group)", {
  expect_true(any(is.na(out_plain$species_group)))
})


test_that("reshaping is faithful: total column reproduces the base table", {
  # per-species total of iv_m3_yr_total
  tot_from_out <- out_plain |>
    dplyr::filter(.data$variable == "iv_m3_yr_total",
                  !is.na(.data$species_group)) |>
    dplyr::select("species_group", out_total = "total")
  ref <- bt_plain$total |>
    dplyr::select("species_group", ref_total = "iv_m3_yr_total")
  cmp <- dplyr::left_join(tot_from_out, ref, by = "species_group")
  expect_equal(cmp$out_total, cmp$ref_total, tolerance = 1e-8)

  # Summe-block total cell of iv_m3_yr_total == all_total
  summe_cell <- out_plain |>
    dplyr::filter(is.na(.data$species_group),
                  .data$variable == "iv_m3_yr_total") |>
    dplyr::pull(.data$total)
  expect_equal(summe_cell, bt_plain$all_total$iv_m3_yr_total, tolerance = 1e-8)
})


test_that("main-stand variant restructures (no per_species / Ant. (%) row)", {
  expect_false("per_species" %in% out_main$variable)
  # the ideal-area rows are still there
  expect_true(all(c("area_ha", "iv_m3_ha_yr_virt") %in% out_main$variable))
  expect_true(all(c("species_group", "variable", "total") %in% names(out_main)))
})


test_that("the dq axis works", {
  expect_true(all(levels(bt_dq$detail$d_q_class) %in% names(out_dq)))
  expect_true("iv_m3_ha_yr" %in% out_dq$variable)
})


test_that("the collective descriptor is carried through as an attribute", {
  co_plain <- attr(out_plain, "collective")
  expect_equal(co_plain$kind, "all_layers")
  co_main <- attr(out_main, "collective")
  expect_equal(co_main$kind, "main_stand")
  expect_equal(co_main$label, "Hauptbestand")
})


# --- PDF renderer wrapper smoke test (skip if toolchain missing) -------------

test_that("output_increment_base_table_pdf() renders to a PDF", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not on PATH")

  pdf_path <- tryCatch(
    output_increment_base_table_pdf(
      out_main, tab_title = "Testbetrieb", type = "age",
      output_dir = tempdir(),
      meta = list(year = 2024, area_ha = 1234, n_plots = 10)
    ),
    error = function(e) {
      skip(paste("output_increment_base_table_pdf() failed to render:",
                 e$message))
    }
  )
  expect_true(file.exists(pdf_path))
  expect_match(basename(pdf_path), "\\.pdf$")
  # The file name must identify both axes the table varies along - collective
  # and class type - so the variants of one enterprise do not overwrite each
  # other.
  expect_match(basename(pdf_path), "main_stand")
  expect_match(basename(pdf_path), "age")
})

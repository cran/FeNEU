test_that("plot_info_sheet_pdf() renders to a PDF with a safe file name", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not on PATH")

  plot_1 <- data_ex3_sample_fe_inventory$plot[[1]]

  pdf_path <- plot_info_sheet_pdf(plot_1, output_dir = tempdir())
  expect_true(file.exists(pdf_path))
  expect_match(basename(pdf_path), "\\.pdf$")
  # The default title is the plot's (dotted) stand_id and must survive
  # sanitising unchanged, so the file name still names the plot.
  expect_identical(
    basename(pdf_path),
    paste0("plot_info_ger_", plot_1$stand_id, ".pdf")
  )

  # An identifier-style title with spaces and dashes (the default title is the
  # plot's stand_id, which may look like this) must not leak into the file
  # name, while the sheet itself keeps the original spelling.
  pdf_path_id <- plot_info_sheet_pdf(plot_1, title = "7- 2- 0- - 0",
                                     output_dir = tempdir())
  expect_true(file.exists(pdf_path_id))
  expect_identical(basename(pdf_path_id), "plot_info_ger_7_2_0_0.pdf")
})

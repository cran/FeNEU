test_that("plot_info_sheet_pdf() renders to a PDF with a safe file name", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("ggrepel")
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

  # An identifier-style stand_id with spaces and dashes (the stand-wise
  # "7- 2- 0- - 0" codes look like this) must not leak into the file name,
  # while the sheet itself keeps the original spelling.
  plot_id_style <- plot_1
  plot_id_style$stand_id <- "7- 2- 0- - 0"
  pdf_path_id <- plot_info_sheet_pdf(plot_id_style, output_dir = tempdir())
  expect_true(file.exists(pdf_path_id))
  expect_identical(basename(pdf_path_id), "plot_info_ger_7_2_0_0.pdf")

  # A selection other than the default goes into the file name, so the sheet
  # with and the sheet without removed trees can exist side by side.
  pdf_path_live <- plot_info_sheet_pdf(plot_1, tree_filter = !removal,
                                       output_dir = tempdir())
  expect_true(file.exists(pdf_path_live))
  expect_identical(
    basename(pdf_path_live),
    paste0("plot_info_ger_", plot_1$stand_id, "_removal.pdf")
  )

  # A free heading with a subtitle, both carrying characters that YAML and
  # LaTeX treat specially, renders. The file name joins title and stand_id,
  # so that sheets sharing one title (enterprise and year) stay apart.
  pdf_path_sub <- plot_info_sheet_pdf(
    plot_1,
    title    = "Forstbetrieb \"Test\" 2025",
    subtitle = "Punkt 1, ohne Ausscheider: 'x' & 50 % _a_",
    output_dir = tempdir()
  )
  expect_true(file.exists(pdf_path_sub))
  expect_identical(
    basename(pdf_path_sub),
    paste0("plot_info_ger_Forstbetrieb_Test_2025_", plot_1$stand_id, ".pdf")
  )
  pdf_path_sub_2 <- plot_info_sheet_pdf(
    data_ex3_sample_fe_inventory$plot[[2]],
    title = "Forstbetrieb \"Test\" 2025", output_dir = tempdir()
  )
  expect_false(basename(pdf_path_sub_2) == basename(pdf_path_sub))

  # A plot with trees without position and records standing for several
  # trees renders as well
  pdf_path_8 <- plot_info_sheet_pdf(data_ex3_sample_fe_inventory$plot[[8]],
                                    output_dir = tempdir())
  expect_true(file.exists(pdf_path_8))
})


test_that("the info sheet shows the same trees as plot(), removed ones marked", {
  # ex3 plot 1: 14 trees, 13 of them removed. Up to 3.0.2 the sheet showed
  # only the one remaining tree, while plot() on the same object showed 14.
  p <- data_ex3_sample_fe_inventory$plot[[1]]
  sheet <- .plot_info_sheet_data(p)

  expect_identical(nrow(sheet$table), nrow(p$trees))
  expect_setequal(sheet$table$tree_id, p$trees$tree_id)
  expect_identical(length(sheet$removed_ids), sum(p$trees$removal))
  expect_setequal(sheet$removed_ids, p$trees$tree_id[p$trees$removal])

  # Table and map come from the same selection
  expect_setequal(sheet$x$tree_positions$tree_id,
                  sheet$table$tree_id[sheet$table$positioned])
  expect_setequal(sheet$x$trees$tree_id, sheet$table$tree_id)
})


test_that("tree_filter selects the trees of table and map alike", {
  p <- data_ex3_sample_fe_inventory$plot[[1]]

  live <- .plot_info_sheet_data(p, rlang::quo(!removal))
  expect_identical(nrow(live$table), sum(!p$trees$removal))
  expect_length(live$removed_ids, 0)
  expect_setequal(live$x$tree_positions$tree_id, live$table$tree_id)

  # The expression is evaluated where it was written, so local values work
  thr <- 30
  big <- .plot_info_sheet_data(p, rlang::quo(dbh_cm > thr))
  expect_true(all(big$table$dbh_cm > thr))
  expect_identical(nrow(big$table), sum(p$trees$dbh_cm > thr))
})


test_that("a removal flag of NA does not produce an empty table row", {
  p <- data_ex3_sample_fe_inventory$plot[[1]]
  p$trees$removal[2] <- NA
  sheet <- .plot_info_sheet_data(p, rlang::quo(!removal))
  expect_false(anyNA(sheet$table$tree_id))
  expect_false(p$trees$tree_id[2] %in% sheet$table$tree_id)
})


test_that("trees without position are listed and counted", {
  # ex3 plot 8: 18 trees, only 8 with a position
  p <- data_ex3_sample_fe_inventory$plot[[8]]
  sheet <- .plot_info_sheet_data(p)

  expect_identical(nrow(sheet$table), nrow(p$trees))
  n_unpos <- sum(!p$trees$tree_id %in% p$tree_positions$tree_id)
  expect_gt(n_unpos, 0)
  expect_identical(sheet$n_unpositioned_records, n_unpos)
  unpos <- sheet$table[!sheet$table$positioned, ]
  expect_true(all(is.na(unpos$R) & is.na(unpos$angle)))
})


test_that("the table is sorted by the numeric parts of the tree number", {
  p <- data_ex3_sample_fe_inventory$plot[[8]]
  ids <- .plot_info_sheet_data(p)$table$tree_id
  expect_identical(ids[1:3], c("1_1", "2_1", "2_2"))
  expect_true(match("9_1", ids) < match("10_1", ids))

  expect_identical(.tree_id_order(c("10_1", "2_2", "2_1", "2_10")),
                   c(3L, 2L, 4L, 1L))
  # Identifiers of another form are sorted as text
  expect_identical(.tree_id_order(c("b", "a", "10_1")), c(3L, 2L, 1L))
  expect_identical(.tree_id_order(character()), integer())
})


test_that("small trees are counted as trees, not records", {
  p1 <- data_ex3_sample_fe_inventory$plot[[1]]
  expect_identical(.plot_info_sheet_data(p1)$n_small_trees,
                   sum(p1$small_trees$tree_count))
  expect_gt(nrow(p1$small_trees), 0)

  # Plot 2 carries only a placeholder row of NAs
  p2 <- data_ex3_sample_fe_inventory$plot[[2]]
  expect_identical(.plot_info_sheet_data(p2)$n_small_trees, 0)
})


test_that("check_pdf_dependencies() names a missing extra package", {
  expect_error(
    check_pdf_dependencies("plot_info_sheet_pdf",
                           extra_pkgs = "aPackageThatDoesNotExist"),
    "aPackageThatDoesNotExist"
  )
})

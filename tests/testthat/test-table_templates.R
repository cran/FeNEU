# Both table templates take their table from params$x. They used to read a bare
# `x`, which only worked because rmarkdown::render() evaluates in the caller's
# frame and the argument of output_*_table_pdf() happened to be called `x` as
# well - renaming it there would have broken rendering. These renders come from
# a frame that deliberately has no object called `x`, which is the case the old
# templates could not survive.

test_that("the table templates do not depend on the caller's variable name", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not on PATH")

  out <- tempdir()

  suppressWarnings(suppressMessages({
    twh <- data_ex3_sample_fe_inventory |> pull_trees() |> height_complete_inventory()
    tw  <- data_ex3_sample_fe_inventory |>
      fill_heights_back(twh) |> pull_trees() |> trees_add_essentials()
  }))

  # A quarter of the ages removed, so the templates also meet a class whose
  # level is NA - the "k. A." column of the base table and the "k. A." row group
  # of the structure table.
  tw_na <- tw
  set.seed(1)
  idx <- sample(nrow(tw_na), floor(nrow(tw_na) / 4))
  tw_na$age_yr[idx]    <- NA_real_
  tw_na$age_class[idx] <- NA

  render_here <- function(tbl, title, template) {
    deliberately_not_x <- tbl
    rmarkdown::render(
      input = paste0(system.file(package = "FeNEU"), "/rmd/", template),
      params = list(x = deliberately_not_x, tab_title = title, type = "age",
                    meta = NULL, cohort = "mainstand"),
      output_format = "pdf_document",
      output_file = paste0(title, ".pdf"), output_dir = out, quiet = TRUE
    )
  }

  suppressWarnings(suppressMessages({
    ob    <- tw    |> base_table_age_class_main_stand()      |> output_base_table()
    ob_na <- tw_na |> base_table_age_class_main_stand()      |> output_base_table()
    os_na <- tw_na |> structure_table_age_class_main_stand() |> output_structure_table()

    p1 <- render_here(ob,    "chk_base",      "output_base_table_template_ger.Rmd")
    p2 <- render_here(ob_na, "chk_base_na",   "output_base_table_template_ger.Rmd")
    p3 <- render_here(os_na, "chk_struct_na", "output_structure_table_template_ger.Rmd")
  }))

  expect_true(all(file.exists(p1, p2, p3)))
})

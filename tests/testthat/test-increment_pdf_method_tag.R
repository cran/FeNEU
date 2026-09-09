# The increment base-table PDF name is built from the table title, the tree
# collective and the class axis. All three are identical for a repeated-survey,
# a gnfi3 and a yield-table table of the same enterprise, so without a method
# tag the three strands overwrite each other's file -- silently, because the
# rendered PDFs are correctly distinguishable inside (their subtitle names the
# method) and only the file names collided.
#
# The tag comes from the `source` element of the header meta the pipeline
# already carries: "gnfi3" from inv_inc_tree_extend_single(), "ytable" from
# increment_ytables_base_table(). The repeated survey sets no source and keeps
# its file name unchanged.

test_that(".increment_method_tag() reads the source from the meta", {
  expect_identical(.increment_method_tag(list(source = "gnfi3")), "gnfi3")
  expect_identical(.increment_method_tag(list(source = "ytable")), "ytable")

  # Repeated survey and metaless tables: no tag
  expect_identical(.increment_method_tag(list(year = 2025)), "")
  expect_identical(.increment_method_tag(NULL), "")
  expect_identical(.increment_method_tag(list(source = NA_character_)), "")
})


test_that("the three strands carry the source their tag is derived from", {
  # Runs all three increment strands to read one meta field from each. The field
  # is ours and changes only when we change it, so this belongs where our edits
  # happen rather than on the check farm.
  skip_below_depth("full")

  trees <- data_ex3_sample_trees_essentials
  inv_b <- data_ex3_sample_fe_inventory

  # Strand 2 -- gnfi3
  ext_g <- suppressWarnings(suppressMessages(
    inv_inc_tree_extend_single(trees, inv = inv_b)
  ))
  bt_g <- suppressWarnings(suppressMessages(
    increment_base_table_main_stand(ext_g, by_class = "age")
  ))
  expect_identical(bt_g$meta$source, "gnfi3")

  # Strand 3 -- ytable
  bt_y <- suppressWarnings(suppressMessages(
    increment_ytables_base_table(trees, ytables_bavrn_state_var_1_feneu,
                                 by_class = "age", fe_inv = inv_b)
  ))
  expect_identical(bt_y$meta$source, "ytable")

  # Strand 1 -- repeated survey, no source
  inc <- suppressWarnings(suppressMessages(
    inv_increment_repeated_survey(
      data_ex3_previous_sample_fe_inventory, inv_b,
      data_ex3_previous_sample_trees_essentials, trees,
      progress_bar = FALSE
    )
  ))
  bt_r <- suppressWarnings(suppressMessages(
    increment_base_table_main_stand(inc, by_class = "age")
  ))
  expect_null(bt_r$meta$source)
})


test_that("the three strands do not overwrite each other's PDF", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  trees <- data_ex3_sample_trees_essentials
  inv_b <- data_ex3_sample_fe_inventory
  outdir <- file.path(tempdir(), "inc_method_tag")
  unlink(outdir, recursive = TRUE)
  dir.create(outdir, recursive = TRUE)

  # Deliberately the SAME title, collective and class axis for all three -- that
  # is exactly the case that used to collapse into one file.
  ttl <- "Forstbetrieb Beispiel"

  suppressWarnings(suppressMessages({
    inc <- inv_increment_repeated_survey(
      data_ex3_previous_sample_fe_inventory, inv_b,
      data_ex3_previous_sample_trees_essentials, trees,
      progress_bar = FALSE
    )
    increment_base_table_main_stand(inc, by_class = "age") |>
      output_increment_base_table() |>
      output_increment_base_table_pdf(ttl, type = "age", output_dir = outdir)

    inv_inc_tree_extend_single(trees, inv = inv_b) |>
      increment_base_table_main_stand(by_class = "age") |>
      output_increment_base_table() |>
      output_increment_base_table_pdf(ttl, type = "age", output_dir = outdir)

    increment_ytables_base_table(trees, ytables_bavrn_state_var_1_feneu,
                                 by_class = "age", fe_inv = inv_b) |>
      output_increment_base_table() |>
      output_increment_base_table_pdf(ttl, type = "age", output_dir = outdir)
  }))

  pdfs <- sort(list.files(outdir, pattern = "[.]pdf$"))
  expect_length(pdfs, 3L)

  # The repeated survey keeps the untagged name; the other two are tagged
  expect_true(any(grepl("_main_stand_age[.]pdf$", pdfs)))
  expect_true(any(grepl("_main_stand_age_gnfi3[.]pdf$", pdfs)))
  expect_true(any(grepl("_main_stand_age_ytable[.]pdf$", pdfs)))
})

test_that("tree_selection / class_type tags flow through base/structure tables", {
  # Builds a dozen tables to read their attributes. What it guards is our own
  # bookkeeping -- which label a table carries and how it becomes a file name --
  # and that can only break through an edit in this package, never through an R
  # or dependency update. So it runs for us, where such an edit happens.
  skip_below_depth("full")

  # This file checks attributes and file-name tokens only -- never a figure or a
  # row count -- so the species coding of the tree list does not matter here.
  # The shipped list therefore does the job and saves building the chain.
  tw <- data_ex3_sample_trees_essentials

  back_d <- back_table_dclass(tw)

  # --- base tables: tree_selection stored as the RAW label ------------------
  bt_main  <- base_table_age_class_main_stand(tw)
  bt_all   <- base_table_age_class(tw)
  bt_cust  <- base_table_age_class(tw, tree_filter = layer_key == 2)

  expect_identical(attr(bt_main, "tree_selection"), "mainstand")
  expect_identical(attr(bt_all,  "tree_selection"), "alllayers")
  expect_identical(attr(bt_cust, "tree_selection"), "layer_key == 2")

  # sanitiser turns a raw expression into a file-name-safe token
  expect_identical(.safe_filename_part("layer_key == 2"), "layer_key_2")
  expect_identical(.safe_filename_part("!removal"),        "removal")

  bt_dq_main <- base_table_d_q_class_main_stand(tw, back_d)
  expect_identical(attr(bt_dq_main, "tree_selection"), "mainstand")

  # --- propagation + class_type through output_base_table -------------------
  ob_main <- output_base_table(bt_main)
  ob_all  <- output_base_table(bt_all)
  ob_cust <- output_base_table(bt_cust)
  expect_identical(attr(ob_main, "tree_selection"), "mainstand")
  expect_identical(attr(ob_all,  "tree_selection"), "alllayers")
  expect_identical(attr(ob_cust, "tree_selection"), "layer_key == 2")
  expect_identical(attr(ob_main, "class_type"), "age")
  expect_identical(attr(output_base_table(bt_dq_main), "class_type"), "dq")

  # --- .table_pdf_meta(): type derived, cohort raw, file_tag sanitised ------
  m_main <- .table_pdf_meta(ob_main, "test")
  m_cust <- .table_pdf_meta(ob_cust, "test")
  expect_identical(m_main$type, "age")
  expect_identical(m_main$cohort, "mainstand")
  expect_identical(m_main$file_tag, "mainstand")
  expect_identical(m_cust$cohort, "layer_key == 2")
  expect_identical(m_cust$file_tag, "layer_key_2")

  # missing class_type -> clear error (nothing silently guessed)
  bare <- ob_main
  attr(bare, "class_type") <- NULL
  expect_error(.table_pdf_meta(bare, "test"), "class_type")

  # --- structure tables -----------------------------------------------------
  st_main <- structure_table_age_class_main_stand(tw)
  st_all  <- structure_table_age_class(tw)
  expect_identical(attr(st_main, "tree_selection"), "mainstand")
  expect_identical(attr(st_all,  "tree_selection"), "alllayers")
  os_main <- output_structure_table(st_main)
  expect_identical(attr(os_main, "tree_selection"), "mainstand")
  expect_identical(attr(os_main, "class_type"), "age")

  # bare-data-frame path leaves both attributes NULL (no crash)
  expect_null(attr(output_structure_table(st_all$detail), "tree_selection"))
})


test_that("increment PDF file names distinguish the tree collective", {
  # Standard kinds pass through unchanged ...
  expect_identical(
    .collective_file_tag(list(kind = "main_stand", definition = NA_character_)),
    "main_stand"
  )
  expect_identical(
    .collective_file_tag(list(kind = "all_layers", definition = "alle Baeume")),
    "all_layers"
  )

  # ... while two DIFFERENT user-defined collectives must not both collapse to
  # "custom" - that would make the second PDF overwrite the first.
  tag_1 <- .collective_file_tag(
    list(kind = "custom", definition = "layer_key == 2")
  )
  tag_2 <- .collective_file_tag(
    list(kind = "custom", definition = "layer_key == 3")
  )
  expect_identical(tag_1, "layer_key_2")
  expect_identical(tag_2, "layer_key_3")
  expect_false(identical(tag_1, tag_2))

  # A custom collective without a usable definition still yields a valid tag.
  expect_identical(
    .collective_file_tag(list(kind = "custom", definition = NA_character_)),
    "custom"
  )
  expect_identical(.collective_file_tag(list(kind = "custom")), "custom")

  # The sanitiser never returns an empty fragment.
  expect_identical(.safe_filename_part("+++"), "x")
  expect_identical(.safe_filename_part("+++", "filter"), "filter")
  expect_identical(.safe_filename_part("Nordhalben-1223"), "Nordhalben_1223")
  # Dots survive INSIDE the token (dotted plot identifiers must stay
  # recognisable) but not at either end: a leading dot would hide the file on
  # unix-alikes, a trailing one is invalid on Windows.
  expect_identical(.safe_filename_part("900000002.1.99900002"),
                   "900000002.1.99900002")
  expect_identical(.safe_filename_part(".data$removal"), "data_removal")
  expect_identical(.safe_filename_part("Betrieb."), "Betrieb")
})

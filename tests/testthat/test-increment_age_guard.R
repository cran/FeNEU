# Stand age is optional in a sample_concentric inventory, but all three
# increment strands depend on it somewhere: the yield-table and gnfi3 (BWI 3)
# strands entirely, the repeated-survey strand in its age-class breakdown. A
# tree without an age silently produces an NA increment that the aggregation
# then drops, so the table would look complete while covering only part of the
# collective. Every affected entry point must refuse instead.
#
# How wide the requirement reaches follows from the method, not from taste:
#   - The gnfi3 strand estimates per tree over the WHOLE population, precisely
#     so that main stand, all layers or any other collective can be aggregated
#     from the result afterwards. Ages are therefore required everywhere.
#   - The yield-table strand is inherently bound to the main stand, so it checks
#     that narrower collective and an ageless understorey does not block it.
#   - The repeated-survey strand needs ages only for its age-class breakdown,
#     checked after the table's own filter; the dq breakdown is unaffected.

inv_g   <- data_ex3_sample_fe_inventory
trees_g <- data_ex3_sample_trees_essentials

test_that("complete ages still pass through both strands", {
  expect_no_error(suppressWarnings(suppressMessages(
    inv_inc_tree_extend_single(trees_g)
  )))
  expect_no_error(suppressWarnings(suppressMessages(
    inv_increment_ytables(inv_g, trees_g, ytables_bavrn_state_var_1_feneu)
  )))
})

test_that("gnfi3 increment refuses partially missing ages", {
  tr <- trees_g
  tr$age_yr[which(!tr$removal)[1]] <- NA_real_
  expect_error(
    suppressWarnings(suppressMessages(inv_inc_tree_extend_single(tr))),
    "inv_inc_tree_extend_single"
  )
})

test_that("yield-table increment refuses partially missing ages", {
  tr <- trees_g
  tr$age_yr[which(tr$layer_key == 1 & !tr$removal)[1]] <- NA_real_
  expect_error(
    suppressWarnings(suppressMessages(
      inv_increment_ytables(inv_g, tr, ytables_bavrn_state_var_1_feneu)
    )),
    "inv_increment_ytables"
  )
})

test_that("a missing age outside the evaluated collective does not block", {
  # Yield tables evaluate the main stand only, so an ageless understorey tree
  # must not stop the evaluation.
  tr <- trees_g
  other <- which(tr$layer_key != 1 & !tr$removal)
  skip_if(length(other) == 0, "no non-main-stand trees in the example")
  tr$age_yr[other[1]] <- NA_real_
  expect_no_error(suppressWarnings(suppressMessages(
    inv_increment_ytables(inv_g, tr, ytables_bavrn_state_var_1_feneu)
  )))
})

test_that("a missing age_yr column is reported as such", {
  tr <- trees_g
  tr$age_yr <- NULL
  expect_error(
    suppressWarnings(suppressMessages(inv_inc_tree_extend_single(tr))),
    "age_yr"
  )
})


test_that("inv_increment_gnfi_2012() requires ages for the whole population", {
  # It estimates per tree over everything so that any collective can be
  # aggregated from the result; an ageless tree outside the collective this
  # aggregator reports must be refused just the same. Without the guard the
  # failure arrives as a ForestElementsR v_gri() complaint about dbh values.
  tr <- trees_g
  tr$age_yr[which(tr$layer_key == 1 & !tr$removal)[1]] <- NA_real_
  expect_error(
    suppressWarnings(suppressMessages(inv_increment_gnfi_2012(tr))),
    "inv_increment_gnfi_2012"
  )

  outside <- which(!(trees_g$layer_key == 1 & !trees_g$removal))
  skip_if(length(outside) == 0, "no trees outside the main stand in the example")
  tr2 <- trees_g
  tr2$age_yr[outside[1]] <- NA_real_
  expect_error(
    suppressWarnings(suppressMessages(inv_increment_gnfi_2012(tr2))),
    "inv_increment_gnfi_2012"
  )
})


test_that("repeated-survey age-class tables refuse incomplete ages", {
  # Same reason as below: the bundle has to be built first. That the age guard
  # itself bites is covered by the cheap tests above.
  skip_below_depth("full")

  inc <- suppressWarnings(suppressMessages(
    inv_increment_repeated_survey(
      data_ex3_previous_sample_fe_inventory,
      data_ex3_sample_fe_inventory,
      data_ex3_previous_sample_trees_essentials,
      trees_g,
      progress_bar = FALSE
    )
  ))

  inc_hurt <- inc
  set.seed(42)
  idx <- sample(nrow(inc_hurt$trees), floor(nrow(inc_hurt$trees) / 4))
  inc_hurt$trees$age_yr[idx] <- NA_real_

  expect_error(
    suppressWarnings(suppressMessages(
      increment_base_table(inc_hurt, by_class = "age")
    )),
    "increment_base_table"
  )
  expect_error(
    suppressWarnings(suppressMessages(
      increment_base_table_main_stand(inc_hurt, by_class = "age")
    )),
    "increment_base_table_main_stand"
  )
})


test_that("the dq breakdown needs no age and keeps working", {
  # Needs a whole repeated-survey bundle before it can empty the ages in it.
  # The guards themselves -- the tests above that refuse incomplete ages -- are
  # cheap and stay on CRAN; this one checks how the bundle and the class tables
  # play together, which changes only when we change it.
  skip_below_depth("full")

  inc <- suppressWarnings(suppressMessages(
    inv_increment_repeated_survey(
      data_ex3_previous_sample_fe_inventory,
      data_ex3_sample_fe_inventory,
      data_ex3_previous_sample_trees_essentials,
      trees_g,
      progress_bar = FALSE
    )
  ))
  inc$trees$age_yr <- NA_real_

  expect_no_error(suppressWarnings(suppressMessages(
    increment_base_table(inc, by_class = "dq")
  )))
  expect_no_error(suppressWarnings(suppressMessages(
    increment_base_table_main_stand(inc, by_class = "dq")
  )))
})

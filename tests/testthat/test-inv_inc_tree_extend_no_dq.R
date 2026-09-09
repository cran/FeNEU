# inv_inc_tree_extend() used to rely on the join suffixes .x/.y to tell an
# incoming d_q_cm column apart from the one back_table_dclass() computes. That
# tied it to how the tree data had been prepared: the chain that folds the
# heights into the tibble directly carries d_q_cm along, the canonical
# write-back chain (fill_heights_back -> pull_trees -> trees_add_essentials)
# does not - and without it no suffixes are generated and the rename failed
# with "Column `d_q_cm.x` doesn't exist".
#
# The rest of the increment test suite happens to use the folding chain, which
# is why this went unnoticed. This file deliberately uses the write-back chain.

suppressWarnings(suppressMessages({

  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory

  prep_writeback <- function(inv) {
    est <- inv |> pull_trees() |> height_complete_inventory(method = "NFI")
    inv |> fill_heights_back(est) |> pull_trees() |>
      trees_add_essentials(method = "BaySF")
  }
  trees_a <- prep_writeback(inv_a)
  trees_b <- prep_writeback(inv_b)

}))

test_that("the write-back chain really produces no d_q_cm column", {
  # If this ever changes, the regression below stops testing what it means to.
  expect_false("d_q_cm" %in% names(trees_a))
  expect_false("d_q_cm" %in% names(trees_b))
})

test_that("inv_inc_tree_extend() works without an incoming d_q_cm column", {
  suppressWarnings(suppressMessages({
    inc_rep <- inv_increment_repsurv_ccirc(
      inv_a, inv_b, trees_a, trees_b,
      match_type = "plot_id", plot_id_style = "baysf", progress_bar = FALSE
    )
    inc_fl   <- inv_inc_fill_gaps_gnfi3(inc_rep, trees_a, trees_b,
                                        include_reptrees = TRUE)
    inc_cons <- inv_inc_tree_consolidate(inc_fl, fill_option = "standard")
    extd     <- inv_inc_tree_extend(inc_cons, trees_a, trees_b, inv_b)
  }))

  # The class axis is what the aggregation actually consumes, and it must be
  # complete - the d_q class comes from the freshly computed cohort d_q.
  expect_true("d_q_class" %in% names(extd))
  expect_true("d_q_cm_for_class" %in% names(extd))
  expect_false(any(is.na(extd$d_q_class)))
  expect_gt(nrow(extd), 0)

  # And the increment base tables built on top of it still work.
  suppressWarnings(suppressMessages({
    bt <- increment_base_table(extd, by_class = "dq")
  }))
  expect_true(is.list(bt))
})

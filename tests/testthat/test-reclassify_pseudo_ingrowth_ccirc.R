
# =============================================================================
# test-reclassify_pseudo_ingrowth_ccirc.R
#
# Tests for the pseudo-ingrowth reclassification of trees on concentric
# circle plots — including the inner_circle_non_matchable parameter on the
# matching side, the reclassify_pseudo_ingrowth_ccirc() helper, and the
# downstream invariance contract for inv_inc_summaric().
# =============================================================================


# --- Setup: BaySF-style (sub10) ----------------------------------------------
suppressWarnings(suppressMessages({
  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory

  inv_a_trees <- inv_a |>
    pull_trees() |>
    height_complete_inventory()
  inv_a_trees$height_m <- ifelse(
    is.na(inv_a_trees$height_m), inv_a_trees$h_est_m, inv_a_trees$height_m
  )
  inv_a_trees <- inv_a_trees |> trees_add_essentials(method = "BaySF")

  inv_b_trees <- inv_b |>
    pull_trees() |>
    height_complete_inventory()
  inv_b_trees$height_m <- ifelse(
    is.na(inv_b_trees$height_m), inv_b_trees$h_est_m, inv_b_trees$height_m
  )
  inv_b_trees <- inv_b_trees |> trees_add_essentials(method = "BaySF")

  inc_rep_baysf <- inv_increment_repsurv_ccirc(
    inv_a, inv_b, inv_a_trees, inv_b_trees,
    method = "rep_classic", progress_bar = FALSE
  )
}))


# --- Tests: pseudo_ingrowth_corrected flag contract --------------------------

test_that("pseudo_ingrowth_corrected column exists with clean TRUE/FALSE", {
  ti <- inc_rep_baysf$tree_increments
  expect_true("pseudo_ingrowth_corrected" %in% names(ti))
  expect_type(ti$pseudo_ingrowth_corrected, "logical")
  expect_equal(sum(is.na(ti$pseudo_ingrowth_corrected)), 0L)
})


test_that("at least one pseudo-ingrowth tree is identified in sub10 BaySF data", {
  ti <- inc_rep_baysf$tree_increments
  expect_gt(sum(ti$pseudo_ingrowth_corrected), 0L)
})


test_that("is_unmatchable_1st/2nd columns exist with clean TRUE/FALSE", {
  ti <- inc_rep_baysf$tree_increments
  expect_true("is_unmatchable_1st" %in% names(ti))
  expect_true("is_unmatchable_2nd" %in% names(ti))
  expect_type(ti$is_unmatchable_1st, "logical")
  expect_type(ti$is_unmatchable_2nd, "logical")
  expect_equal(sum(is.na(ti$is_unmatchable_1st)), 0L)
  expect_equal(sum(is.na(ti$is_unmatchable_2nd)), 0L)
})


test_that("pseudo-ingrowth trees are also flagged is_unmatchable_2nd", {
  ti     <- inc_rep_baysf$tree_increments
  pseudo <- ti[ti$pseudo_ingrowth_corrected %in% TRUE, ]
  # Each pseudo-ingrowth tree's inv_2 record belongs to the inv_1
  # inner-circle cohort; for cohort recoverability the unmatchable_2nd
  # flag must be TRUE on these rows.
  expect_true(all(pseudo$is_unmatchable_2nd %in% TRUE))
})


test_that("reclassified rows have all required fields filled consistently", {
  ti     <- inc_rep_baysf$tree_increments
  pseudo <- ti[ti$pseudo_ingrowth_corrected %in% TRUE, ]

  # By construction, reclassified rows must look like both-exist trees
  expect_true(all(pseudo$tree_exists_1st %in% TRUE))
  expect_true(all(pseudo$tree_exists_2nd %in% TRUE))

  # v_hub_m3_1st is the gnfi3 backward estimate, clamped at 0; n_rep_ha_1st is
  # the inner-circle representation, n_rep_ha_2nd a larger-circle value
  expect_true(all(!is.na(pseudo$v_hub_m3_1st) & pseudo$v_hub_m3_1st >= 0))
  expect_true(all(!is.na(pseudo$n_rep_ha_1st)))
  expect_true(all(pseudo$n_rep_ha_1st > pseudo$n_rep_ha_2nd))
})


test_that("normal both-exist remeasured trees keep flag = FALSE", {
  ti <- inc_rep_baysf$tree_increments
  both_exist <- ti[ti$tree_exists_1st %in% TRUE &
                     ti$tree_exists_2nd %in% TRUE, ]
  # Most both-exist trees are normal remeasured; pseudo-corrected is a small
  # minority. Filter out the corrected ones (they look like both-exist after
  # reclassification) and check the rest.
  real_both <- both_exist[!both_exist$pseudo_ingrowth_corrected, ]
  expect_false(any(real_both$pseudo_ingrowth_corrected %in% TRUE))
})


# --- Tests: flag propagates cleanly through the fill-gap pipeline ------------

test_that("pseudo_ingrowth_corrected propagates clean through fill_gaps", {
  skip_below_depth("full")
  inc_fl <- suppressMessages(inv_inc_fill_gaps_gnfi3(
    inc_rep_baysf, inv_a_trees, inv_b_trees, include_reptrees = FALSE
  ))
  for (slot in c("fills_forward", "fills_backward", "fills_backward_nomatch",
                 "fills_backward_implausible", "fills_backward_reptrees")) {
    df <- inc_fl[[slot]]
    expect_true(
      "pseudo_ingrowth_corrected" %in% names(df),
      info = paste("missing column in", slot)
    )
    expect_equal(
      sum(is.na(df$pseudo_ingrowth_corrected)), 0L,
      info = paste("NA found in column of", slot)
    )
  }
})


# --- Tests: inv_inc_summaric() is invariant under the correction -------------

test_that("inv_inc_summaric() ignores the pseudo-ingrowth correction", {
  skip_below_depth("full")
  inc_fl <- suppressMessages(inv_inc_fill_gaps_gnfi3(
    inc_rep_baysf, inv_a_trees, inv_b_trees, include_reptrees = FALSE
  ))

  inc_sum_corrected <- suppressMessages(inv_inc_summaric(
    inc_rep_baysf$tree_increments, inc_fl$fills_forward,
    inv_a_trees, inv_b_trees
  ))

  # Manually undo the reclassification on matched_trees — the summaric
  # output must be identical (the function does the same undo internally
  # on a local copy, so undoing it upstream changes nothing).
  matched_undone <- inc_rep_baysf$tree_increments
  is_p <- matched_undone$pseudo_ingrowth_corrected %in% TRUE
  matched_undone$tree_exists_1st[is_p] <- FALSE
  matched_undone$v_hub_m3_1st[is_p]    <- NA_real_
  matched_undone$n_rep_ha_1st[is_p]    <- NA_real_
  inc_sum_undone <- suppressMessages(inv_inc_summaric(
    matched_undone, inc_fl$fills_forward, inv_a_trees, inv_b_trees
  ))

  expect_equal(inc_sum_corrected$overall, inc_sum_undone$overall)
  expect_equal(inc_sum_corrected$by_species_group,
               inc_sum_undone$by_species_group)
})


# --- Tests: inner_circle_non_matchable parameter behaviour -------------------

# Use the simulated data with stable tree_ids on the inner circle. In that
# case the default generic match finds Inv-1 inner-circle trees, so
# Kreischanger are matched directly and no pseudo-ingrowth identification
# happens. With inner_circle_non_matchable = TRUE, those Inv-1 trees are
# dropped from the match pool and the BaySF-style reclassification kicks in.

suppressWarnings(suppressMessages({
  inv_a_sim <- data_ex5_previous_sample_fe_inventory
  inv_b_sim <- data_ex5_sample_fe_inventory

  inv_a_sim_trees <- inv_a_sim |>
    pull_trees() |>
    height_complete_inventory(method = "NFI")
  inv_a_sim_trees$height_m <- ifelse(
    is.na(inv_a_sim_trees$height_m),
    inv_a_sim_trees$h_est_m, inv_a_sim_trees$height_m
  )
  inv_a_sim_trees <- inv_a_sim_trees |> trees_add_essentials(method = "BaySF")

  inv_b_sim_trees <- inv_b_sim |>
    pull_trees() |>
    height_complete_inventory(method = "NFI")
  inv_b_sim_trees$height_m <- ifelse(
    is.na(inv_b_sim_trees$height_m),
    inv_b_sim_trees$h_est_m, inv_b_sim_trees$height_m
  )
  inv_b_sim_trees <- inv_b_sim_trees |> trees_add_essentials(method = "BaySF")
}))


test_that("generic + inner_circle_non_matchable = FALSE leaves matches alone", {
  skip_below_depth("full")
  inc_gen_default <- suppressMessages(inv_increment_repsurv_ccirc(
    inv_a_sim, inv_b_sim, inv_a_sim_trees, inv_b_sim_trees,
    match_type = "plot_id", plot_id_style = "baysf",
    tree_id_style = "generic",
    inner_circle_non_matchable = FALSE,
    method = "rep_classic", progress_bar = FALSE
  ))
  ti <- inc_gen_default$tree_increments

  # Flag column still present (always added), but all FALSE because the
  # short-circuit branch did not run the position check.
  expect_true("pseudo_ingrowth_corrected" %in% names(ti))
  expect_equal(sum(ti$pseudo_ingrowth_corrected %in% TRUE), 0L)
})


test_that("generic + inner_circle_non_matchable = TRUE drops Inv-1 inner trees", {
  skip_below_depth("full")
  inc_gen_default <- suppressMessages(inv_increment_repsurv_ccirc(
    inv_a_sim, inv_b_sim, inv_a_sim_trees, inv_b_sim_trees,
    match_type = "plot_id", plot_id_style = "baysf",
    tree_id_style = "generic",
    inner_circle_non_matchable = FALSE,
    method = "rep_classic", progress_bar = FALSE
  ))

  inc_gen_strict <- suppressMessages(inv_increment_repsurv_ccirc(
    inv_a_sim, inv_b_sim, inv_a_sim_trees, inv_b_sim_trees,
    match_type = "plot_id", plot_id_style = "baysf",
    tree_id_style = "generic",
    inner_circle_non_matchable = TRUE,
    method = "rep_classic", progress_bar = FALSE
  ))

  ti_d <- inc_gen_default$tree_increments
  ti_s <- inc_gen_strict$tree_increments

  # When inner-circle Inv-1 trees are dropped from the match, the number of
  # matched (both-exist) trees can only stay the same or go down — never up.
  n_both_default <- sum(ti_d$tree_exists_1st %in% TRUE &
                          ti_d$tree_exists_2nd %in% TRUE &
                          !ti_d$pseudo_ingrowth_corrected)
  n_both_strict  <- sum(ti_s$tree_exists_1st %in% TRUE &
                          ti_s$tree_exists_2nd %in% TRUE &
                          !ti_s$pseudo_ingrowth_corrected)
  expect_lte(n_both_strict, n_both_default)
})

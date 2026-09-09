
# =============================================================================
# test-inv_increment_repsurv_generic.R
#
# Tests that tree_id_style = "generic" (ID-based matching) produces identical
# results to tree_id_style = "baysf" (coordinate-based matching) when the
# BaySF tree_ids are permanent (stable across inventories).
#
# Test dataset: 10 plots from Peter Biber's simulated forest enterprise,
# where the original simulation tree_ids are used as lfd_bhd, making the
# BaySF tree_id (= paste(lfd_satz, lfd_bhd)) a permanent identifier.
#
# Data generation: data-raw/simulated_inventory_generic_matching.R
# =============================================================================


# --- Setup: prepare tree data and run matching with both styles ---------------
suppressWarnings(suppressMessages({

  inv_a <- data_ex5_previous_sample_fe_inventory
  inv_b <- data_ex5_sample_fe_inventory

  inv_a_trees <- inv_a |>
    pull_trees() |>
    height_complete_inventory(method = "NFI")
  inv_a_trees$height_m <- ifelse(
    is.na(inv_a_trees$height_m), inv_a_trees$h_est_m, inv_a_trees$height_m
  )
  inv_a_trees <- inv_a_trees |> trees_add_essentials(method = "BaySF")

  inv_b_trees <- inv_b |>
    pull_trees() |>
    height_complete_inventory(method = "NFI")
  inv_b_trees$height_m <- ifelse(
    is.na(inv_b_trees$height_m), inv_b_trees$h_est_m, inv_b_trees$height_m
  )
  inv_b_trees <- inv_b_trees |> trees_add_essentials(method = "BaySF")

  # --- Run all four methods with BOTH tree_id_styles ---

  methods <- c("rep_classic", "rep_mean", "rep_end", "rep_trans")

  inc_baysf   <- list()
  inc_generic <- list()

  for (m in methods) {
    inc_baysf[[m]] <- inv_increment_repsurv_ccirc(
      inv_a, inv_b, inv_a_trees, inv_b_trees,
      match_type = "plot_id", plot_id_style = "baysf",
      tree_id_style = "baysf",
      method = m, progress_bar = FALSE
    )

    inc_generic[[m]] <- inv_increment_repsurv_ccirc(
      inv_a, inv_b, inv_a_trees, inv_b_trees,
      match_type = "plot_id", plot_id_style = "baysf",
      tree_id_style = "generic",
      method = m, progress_bar = FALSE
    )
  }
}))


# --- Helper: sort tree_increments for comparison ------------------------------
sort_ti <- function(ti) {
  ti |>
    dplyr::arrange(
      .data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd
    )
}


# --- Test: identical tree matches ---------------------------------------------

test_that("generic matching finds the same number of trees as baysf matching", {

  for (m in methods) {
    ti_b <- inc_baysf[[m]]$tree_increments
    ti_g <- inc_generic[[m]]$tree_increments

    expect_equal(nrow(ti_b), nrow(ti_g),
                 info = paste("Method:", m))
  }
})


test_that("tree existence flags are identical between baysf and generic", {

  for (m in methods) {
    ti_b <- sort_ti(inc_baysf[[m]]$tree_increments)
    ti_g <- sort_ti(inc_generic[[m]]$tree_increments)

    expect_equal(ti_b$tree_exists_1st, ti_g$tree_exists_1st,
                 info = paste("tree_exists_1st, method:", m))
    expect_equal(ti_b$tree_exists_2nd, ti_g$tree_exists_2nd,
                 info = paste("tree_exists_2nd, method:", m))
  }
})


test_that("matched tree IDs correspond between baysf and generic", {

  # For each method, the set of (plot_id, tree_id_1st, tree_id_2nd)
  # combinations must be the same
  for (m in methods) {
    ti_b <- sort_ti(inc_baysf[[m]]$tree_increments)
    ti_g <- sort_ti(inc_generic[[m]]$tree_increments)

    expect_equal(ti_b$plot_id_1st, ti_g$plot_id_1st,
                 info = paste("plot_id_1st, method:", m))
    expect_equal(ti_b$tree_id_1st, ti_g$tree_id_1st,
                 info = paste("tree_id_1st, method:", m))
    expect_equal(ti_b$tree_id_2nd, ti_g$tree_id_2nd,
                 info = paste("tree_id_2nd, method:", m))
  }
})


# --- Test: identical increment calculations -----------------------------------

test_that("periodic increments are identical between baysf and generic", {

  for (m in methods) {
    ti_b <- sort_ti(inc_baysf[[m]]$tree_increments)
    ti_g <- sort_ti(inc_generic[[m]]$tree_increments)

    expect_equal(ti_b$iv_hub_m3_ha_per_rep, ti_g$iv_hub_m3_ha_per_rep,
                 info = paste("iv_hub_m3_ha_per_rep, method:", m))
  }
})


test_that("annual increments are identical between baysf and generic", {

  for (m in methods) {
    ti_b <- sort_ti(inc_baysf[[m]]$tree_increments)
    ti_g <- sort_ti(inc_generic[[m]]$tree_increments)

    expect_equal(ti_b$iv_hub_m3_ha_yr_rep, ti_g$iv_hub_m3_ha_yr_rep,
                 info = paste("iv_hub_m3_ha_yr_rep, method:", m))
  }
})


# --- Test: identical volumes and representation factors -----------------------

test_that("volumes and n_rep_ha are identical between baysf and generic", {

  for (m in methods) {
    ti_b <- sort_ti(inc_baysf[[m]]$tree_increments)
    ti_g <- sort_ti(inc_generic[[m]]$tree_increments)

    expect_equal(ti_b$v_hub_m3_1st, ti_g$v_hub_m3_1st,
                 info = paste("v_hub_m3_1st, method:", m))
    expect_equal(ti_b$v_hub_m3_2nd, ti_g$v_hub_m3_2nd,
                 info = paste("v_hub_m3_2nd, method:", m))
    expect_equal(ti_b$n_rep_ha_1st, ti_g$n_rep_ha_1st,
                 info = paste("n_rep_ha_1st, method:", m))
    expect_equal(ti_b$n_rep_ha_2nd, ti_g$n_rep_ha_2nd,
                 info = paste("n_rep_ha_2nd, method:", m))
  }
})


# --- Test: plausibility flags identical ---------------------------------------

test_that("plausibility flags are identical between baysf and generic", {

  for (m in methods) {
    ti_b <- sort_ti(inc_baysf[[m]]$tree_increments)
    ti_g <- sort_ti(inc_generic[[m]]$tree_increments)

    expect_equal(ti_b$volume_change_implausible, ti_g$volume_change_implausible,
                 info = paste("volume_change_implausible, method:", m))
    expect_equal(ti_b$species_mismatch, ti_g$species_mismatch,
                 info = paste("species_mismatch, method:", m))
  }
})


# --- Test: match statistics identical -----------------------------------------

test_that("match_tree_stats are identical between baysf and generic", {

  for (m in methods) {
    stats_b <- inc_baysf[[m]]$match_tree_stats
    stats_g <- inc_generic[[m]]$match_tree_stats

    expect_equal(stats_b, stats_g,
                 info = paste("match_tree_stats, method:", m))
  }
})


# --- Test: generic matching runs without errors for all methods ---------------

test_that("generic matching produces valid output structure", {

  expected_names <- c(
    "tree_increments", "plot_matches", "match_plot_stats", "match_tree_stats",
    "inv_period", "method"
  )

  for (m in methods) {
    expect_named(inc_generic[[m]], expected_names,
                 info = paste("Method:", m))
    expect_s3_class(inc_generic[[m]]$tree_increments, "tbl_df")
    expect_equal(inc_generic[[m]]$method, m, info = paste("Method:", m))
  }
})


# --- Test: dataset has expected tree dynamics ---------------------------------

test_that("test dataset contains ingrowth, vanished, and re-measured trees", {

  # Use rep_classic as representative; all methods should have the same matches
  ti <- inc_baysf[["rep_classic"]]$tree_increments

  n_both    <- sum(ti$tree_exists_1st & ti$tree_exists_2nd)
  n_only_00 <- sum(ti$tree_exists_1st & !ti$tree_exists_2nd)
  n_only_10 <- sum(!ti$tree_exists_1st & ti$tree_exists_2nd)

  expect_true(n_both > 0, info = "No re-measured trees found")
  expect_true(n_only_00 > 0, info = "No vanished trees found")
  expect_true(n_only_10 > 0, info = "No ingrowth trees found")
})

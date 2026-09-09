# Tests for inv_inc_tree_extend_combined(), which folds the second-only plots
# (backward gnfi3) into the matched repeated-survey per-tree increment frame so
# the increment base tables cover the whole enterprise (matched + second-only).
#
# Key invariants under test:
#   - the combined frame keeps the per-tree contract consumed by
#     inv_inc_tree_2_plot() and adds rows for the second-only plots' living trees
#   - the whole-enterprise "inc_meta" attribute of the matched frame is carried
#     over unchanged
#   - the combined base table equals the enterprise-level combined_standard from
#     inv_inc_big_overview() (matches_only + second_only), and stays
#     sum-consistent across species
#   - the main-stand variant runs without a zero-area edge case
#   - dt > 0 warns; a run without second-only plots returns the input unchanged

contract_cols <- c(
  "plot_id", "species_group", "age_class", "d_q_class", "layer_key",
  "removal", "standing_area_m2", "n_rep_ha", "iv_hub_m3_ha_per",
  "iv_hub_m3_ha_yr", "iv_filled", "area_rep_ha"
)

suppressWarnings(suppressMessages({

  # Tree lists and the first two increment steps are shipped ready-made for this
  # pair, see ?data_ex3_trees_essentials and ?data_ex3_increment_interim.
  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory

  inv_a_trees <- data_ex3_previous_sample_trees_essentials
  inv_b_trees <- data_ex3_sample_trees_essentials

  inc_rep <- data_ex3_increment_matched
  inc_fl <- data_ex3_increment_fills
  inc_extd <- inv_inc_tree_consolidate(inc_fl, fill_option = "standard") |>
    inv_inc_tree_extend(inv_a_trees, inv_b_trees, inv_b)

  inc_extd_all <- inv_inc_tree_extend_combined(inc_extd, inc_fl, inv_b_trees)

  summaric <- inv_inc_summaric(
    inc_rep$tree_increments, inc_fl$fills_forward, inv_a_trees, inv_b_trees
  )
  bo <- inv_inc_big_overview(
    inc_fl, inv_a_trees, inv_b_trees, inv_b,
    style = "generic", inc_summaric = summaric
  )

  # sub10 has second-only plots; guard the whole suite on that assumption
  n_b_only <- length(inc_fl$plot_matches$in_b_only$plot_id)
}))


# --- contract + row addition ------------------------------------------------

test_that("the sub10 fixture actually has second-only plots", {
  expect_gt(n_b_only, 0)
})

test_that("combined frame keeps the inv_inc_tree_2_plot contract", {
  expect_true(all(contract_cols %in% names(inc_extd_all)))
})

test_that("combined frame adds rows for the second-only plots", {
  expect_gt(nrow(inc_extd_all), nrow(inc_extd))
})

test_that("added rows are all gnfi3-estimated living second-only trees", {
  b_only_ids <- inc_fl$plot_matches$in_b_only$plot_id
  added <- inc_extd_all[!inc_extd_all$plot_id %in% inc_extd$plot_id, ]
  # every added row sits on a second-only plot ...
  expect_true(all(added$plot_id %in% b_only_ids))
  # ... is a gnfi3 estimate ...
  expect_true(all(added$iv_filled))
  # ... and is a living tree (removals filtered out)
  expect_false(any(added$removal))
})


# --- meta -------------------------------------------------------------------

test_that("the whole-enterprise inc_meta is carried over (base fields intact)", {
  m0 <- attr(inc_extd,     "inc_meta")
  m1 <- attr(inc_extd_all, "inc_meta")
  # base fields unchanged (area and total point count already whole-enterprise)
  expect_identical(m1[names(m0)], m0)
})

test_that("inc_meta gains the repeated / backward point-count split", {
  m0 <- attr(inc_extd,     "inc_meta")
  m1 <- attr(inc_extd_all, "inc_meta")
  n_b_only <- length(inc_fl$plot_matches$in_b_only$plot_id)
  expect_equal(m1$n_plots_backward, n_b_only)
  expect_equal(m1$n_plots_repeated, m0$n_plots - n_b_only)
  # the split adds up to the whole-enterprise point count
  expect_equal(m1$n_plots_repeated + m1$n_plots_backward, m0$n_plots)
})


# --- cross-consistency with the enterprise overview -------------------------

test_that("combined base table equals big_overview combined_standard", {
  bt  <- increment_base_table(inc_extd_all, by_class = "age")
  cb  <- bo$combined$overall$iv_total
  ref <- cb$iv_m3_yr_total[cb$fill_option == "combined_standard"]

  expect_equal(bt$all_total$iv_m3_yr_total, ref, tolerance = 1e-6)
  # per-ha over the full operation area
  expect_equal(
    bt$all_total$iv_m3_ha_yr,
    ref / attr(inc_extd_all, "inc_meta")$area_ha,
    tolerance = 1e-6
  )
})

test_that("combined base table stays sum-consistent across species", {
  bt <- increment_base_table(inc_extd_all, by_class = "age")
  expect_equal(
    sum(bt$total$iv_m3_yr_total),
    bt$all_total$iv_m3_yr_total,
    tolerance = 1e-6
  )
})


# --- main-stand variant -----------------------------------------------------

test_that("main-stand variant runs and has no zero ideal-area edge case", {
  btm <- increment_base_table_main_stand(inc_extd_all, by_class = "age")
  expect_true(all(btm$detail$area_ha > 0))
  # operation-wide per-ha increment sum-consistent with all_total
  expect_equal(
    sum(btm$total$iv_m3_yr_total),
    btm$all_total$iv_m3_yr_total,
    tolerance = 1e-6
  )
})


# --- edge cases -------------------------------------------------------------

test_that("dt > 0 warns (backward estimate expected)", {
  expect_warning(
    inv_inc_tree_extend_combined(inc_extd, inc_fl, inv_b_trees, dt = 5),
    "backward"
  )
})

test_that("no second-only plots returns the input unchanged", {
  # Build a plot_matches with an empty in_b_only
  fl_no_b <- inc_fl
  fl_no_b$plot_matches$in_b_only <-
    inc_fl$plot_matches$in_b_only[0, , drop = FALSE]

  out <- inv_inc_tree_extend_combined(inc_extd, fl_no_b, inv_b_trees)
  expect_equal(nrow(out), nrow(inc_extd))
  expect_identical(attr(out, "inc_meta"), attr(inc_extd, "inc_meta"))
})

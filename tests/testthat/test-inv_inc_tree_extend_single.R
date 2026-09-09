# Tests for inv_inc_tree_extend_single(), the single-inventory (gnfi3)
# counterpart to inv_inc_tree_extend().
#
# Key invariants under test:
#   - extend_single produces the per-tree contract consumed by
#     inv_inc_tree_2_plot() (same columns the repeated-survey extend exposes)
#   - one row per input tree (no row inflation/loss)
#   - iv_filled is uniformly TRUE (every increment is a gnfi3 estimate)
#   - the resulting frame flows through inv_inc_tree_2_plot() for both the
#     "age" and "dq" class axes, regardless of style (BaySF / generic)
#   - dt must be non-zero; the annual increment is non-negative in either
#     time direction

# The columns inv_inc_tree_2_plot() relies on -- the shared contract that both
# the repeated-survey and the single-inventory source must satisfy.
contract_cols <- c(
  "plot_id", "species_group", "age_class", "d_q_class", "layer_key",
  "removal", "standing_area_m2", "n_rep_ha", "iv_hub_m3_ha_per",
  "iv_hub_m3_ha_yr", "iv_filled", "area_rep_ha"
)

suppressWarnings(suppressMessages({

  prep_baysf <- function(inv) {
    t <- inv |> pull_trees() |> height_complete_inventory(method = "NFI")
    t$height_m <- ifelse(is.na(t$height_m), t$h_est_m, t$height_m)
    t |> trees_add_essentials(method = "BaySF")
  }
  prep_generic <- function(inv) {
    t <- inv |> pull_trees() |> height_complete_inventory()
    t$height_m <- ifelse(is.na(t$height_m), t$h_est_m, t$height_m)
    t |> trees_add_essentials(method = "WWK")
  }

  trees_baysf   <- prep_baysf(data_ex3_sample_fe_inventory)
  trees_generic <- prep_generic(data_ex5_sample_fe_inventory)

  # Default dt = 5 (forward); the forward-looking planning perspective
  ext_baysf   <- inv_inc_tree_extend_single(trees_baysf)
  ext_generic <- inv_inc_tree_extend_single(trees_generic)
}))


# --- contract --------------------------------------------------------------

test_that("extend_single exposes the inv_inc_tree_2_plot contract (BaySF)", {
  expect_true(all(contract_cols %in% names(ext_baysf)))
})

test_that("extend_single exposes the inv_inc_tree_2_plot contract (generic)", {
  expect_true(all(contract_cols %in% names(ext_generic)))
})

test_that("extend_single keeps one row per living tree (removals dropped)", {
  # A single-inventory estimate concerns the living stand: trees flagged for
  # removal are dropped, so the row count matches the non-removal trees.
  expect_equal(nrow(ext_baysf),   sum(!trees_baysf$removal))
  expect_equal(nrow(ext_generic), sum(!trees_generic$removal))
  # no removal-flagged tree survives into the output
  expect_false(any(ext_baysf$removal))
  expect_false(any(ext_generic$removal))
})

test_that("extend_single marks every increment as gnfi3-estimated", {
  expect_true(all(ext_baysf$iv_filled))
  expect_true(all(ext_generic$iv_filled))
  expect_type(ext_baysf$iv_filled, "logical")
})

test_that("the single-inventory contract is the same one the repeated extend exposes", {
  skip_below_depth("full")
  # Both sources must satisfy the identical column contract so that the
  # downstream aggregation can be written only once.
  suppressWarnings(suppressMessages({
    inv_a <- data_ex3_previous_sample_fe_inventory
    inv_b <- data_ex3_sample_fe_inventory
    inv_a_trees <- prep_baysf(inv_a)
    inv_b_trees <- prep_baysf(inv_b)
    inc_rep  <- inv_increment_repsurv_ccirc(
      inv_a, inv_b, inv_a_trees, inv_b_trees,
      match_type = "plot_id", plot_id_style = "baysf", progress_bar = FALSE
    )
    inc_fl   <- inv_inc_fill_gaps_gnfi3(
      inc_rep, inv_a_trees, inv_b_trees, include_reptrees = TRUE
    )
    inc_cons <- inv_inc_tree_consolidate(inc_fl, fill_option = "standard")
    inc_extd <- inv_inc_tree_extend(inc_cons, inv_a_trees, inv_b_trees, inv_b)
  }))
  expect_true(all(contract_cols %in% names(inc_extd)))
  expect_true(all(contract_cols %in% names(ext_baysf)))
})


# --- flows through the shared plot-level aggregator ------------------------

test_that("extend_single output aggregates via inv_inc_tree_2_plot (age + dq)", {
  for (ext in list(ext_baysf, ext_generic)) {
    p_age <- inv_inc_tree_2_plot(ext, "age")
    p_dq  <- inv_inc_tree_2_plot(ext, "dq")

    expect_s3_class(p_age, "tbl_df")
    expect_true(all(c(
      "plot_id", "species_group", "age_class", "layer_key", "removal",
      "iv_hub_m3_ha_yr", "n_iv_filled", "n_iv_total", "area_rep_ha"
    ) %in% names(p_age)))
    expect_true("d_q_class" %in% names(p_dq))

    # iv_filled is all TRUE, so every cohort is fully estimated
    expect_equal(p_age$n_iv_filled, p_age$n_iv_total)
  }
})


# --- dt handling -----------------------------------------------------------

test_that("extend_single rejects dt = 0", {
  expect_error(inv_inc_tree_extend_single(trees_baysf, dt = 0))
})

test_that("annual increment is non-negative in either time direction", {
  suppressWarnings(suppressMessages({
    ext_back <- inv_inc_tree_extend_single(trees_baysf, dt = -5)
  }))
  expect_true(all(ext_baysf$iv_hub_m3_ha_yr >= 0))  # default, forward
  expect_true(all(ext_back$iv_hub_m3_ha_yr  >= 0))   # backward analogy
})

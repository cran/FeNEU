
# Setup: prepare tree data and run all four methods once for all tests below
# Warnings/messages come from intended species casts - suppressed throughout
suppressWarnings(suppressMessages({

  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory

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

  inc_classic <- inv_increment_repsurv_ccirc(
    inv_a, inv_b, inv_a_trees, inv_b_trees,
    match_type = "plot_id", plot_id_style = "baysf",
    method = "rep_classic", progress_bar = FALSE
  )
  inc_mean <- inv_increment_repsurv_ccirc(
    inv_a, inv_b, inv_a_trees, inv_b_trees,
    match_type = "plot_id", plot_id_style = "baysf",
    method = "rep_mean", progress_bar = FALSE
  )
  inc_end <- inv_increment_repsurv_ccirc(
    inv_a, inv_b, inv_a_trees, inv_b_trees,
    match_type = "plot_id", plot_id_style = "baysf",
    method = "rep_end", progress_bar = FALSE
  )
  inc_trans <- inv_increment_repsurv_ccirc(
    inv_a, inv_b, inv_a_trees, inv_b_trees,
    match_type = "plot_id", plot_id_style = "baysf",
    method = "rep_trans", progress_bar = FALSE
  )
}))


# --- Output structure ---------------------------------------------------------

test_that("inv_increment_repsurv_ccirc() returns expected output structure", {

  expected_names <- c(
    "tree_increments", "plot_matches", "match_plot_stats", "match_tree_stats",
    "inv_period", "method"
  )
  expect_named(inc_classic, expected_names)
  expect_equal(inc_classic$method, "rep_classic")

  # inv_period: list with the two survey years and the period length
  expect_named(inc_classic$inv_period, c("time_yr_1st", "time_yr_2nd", "period_yr"))
  expect_equal(inc_classic$inv_period$time_yr_1st, 2009)
  expect_equal(inc_classic$inv_period$time_yr_2nd, 2020)
  expect_equal(inc_classic$inv_period$period_yr, 11)

  expected_cols <- c(
    "plot_id_1st", "plot_id_2nd", "tree_id_1st", "tree_id_2nd",
    "tree_exists_1st", "tree_exists_2nd", "tree_removal_2nd",
    "time_yr_1st", "time_yr_2nd",
    "v_hub_m3_1st", "n_rep_ha_1st", "v_hub_m3_2nd", "n_rep_ha_2nd",
    "v_hub_m3_at_thrsh",
    "volume_change_rate_per_decade", "volume_change_implausible",
    "species_mismatch",
    "iv_hub_m3_ha_per_rep", "iv_hub_m3_ha_yr_rep"
  )
  expect_true(all(expected_cols %in% names(inc_classic$tree_increments)))
  expect_s3_class(inc_classic$tree_increments, "tbl_df")
})


# --- v_hub_m3_at_thrsh --------------------------------------------------------

test_that("v_hub_m3_at_thrsh is NA/non-NA for the correct tree categories", {

  ti <- inc_classic$tree_increments

  # Ingrowth trees (only in 2nd survey): non-NA
  ingrowth <- ti |> dplyr::filter(!.data$tree_exists_1st & .data$tree_exists_2nd)
  expect_true(nrow(ingrowth) > 0)
  expect_true(all(!is.na(ingrowth$v_hub_m3_at_thrsh)))

  # Circle-transition trees (in both surveys, n1 > n2): non-NA
  circle_tr <- ti |> dplyr::filter(
    .data$tree_exists_1st & .data$tree_exists_2nd &
      .data$n_rep_ha_1st > .data$n_rep_ha_2nd
  )
  if (nrow(circle_tr) > 0)
    expect_true(all(!is.na(circle_tr$v_hub_m3_at_thrsh)))

  # 1st-survey-only trees: NA
  first_only <- ti |> dplyr::filter(.data$tree_exists_1st & !.data$tree_exists_2nd)
  if (nrow(first_only) > 0)
    expect_true(all(is.na(first_only$v_hub_m3_at_thrsh)))

  # Regular re-measured trees without circle transition (n1 == n2): NA
  regular <- ti |> dplyr::filter(
    .data$tree_exists_1st & .data$tree_exists_2nd &
      .data$n_rep_ha_1st == .data$n_rep_ha_2nd
  )
  if (nrow(regular) > 0)
    expect_true(all(is.na(regular$v_hub_m3_at_thrsh)))
})


# --- Plausibility flags -------------------------------------------------------

test_that("plausibility columns are present and correctly typed", {

  ti <- inc_classic$tree_increments

  expect_true("volume_change_implausible" %in% names(ti))
  expect_true("species_mismatch" %in% names(ti))
  expect_true("volume_change_rate_per_decade" %in% names(ti))

  expect_type(ti$volume_change_implausible, "logical")
  expect_type(ti$species_mismatch, "logical")
  expect_type(ti$volume_change_rate_per_decade, "double")
})

test_that("non-remeasured trees are not flagged as implausible", {

  ti <- inc_classic$tree_increments

  # Trees only in one survey should not be flagged as implausible
  not_both <- ti |> dplyr::filter(
    !(.data$tree_exists_1st & .data$tree_exists_2nd)
  )
  if (nrow(not_both) > 0) {
    expect_true(all(!not_both$volume_change_implausible))
    expect_true(all(!not_both$species_mismatch))
  }
})

test_that("implausible trees get zero increment in all methods", {

  for (inc in list(inc_classic, inc_mean, inc_end, inc_trans)) {
    ti <- inc$tree_increments
    impl <- ti |> dplyr::filter(
      .data$volume_change_implausible | .data$species_mismatch
    )
    if (nrow(impl) > 0)
      expect_true(all(impl$iv_hub_m3_ha_per_rep == 0))
  }
})


# --- rep_classic formula correctness -----------------------------------------

test_that("rep_classic applies correct increment formulas", {

  ti <- inc_classic$tree_increments

  # Re-measured plausible trees (in both surveys) with no shrinkage:
  # iv = n2*v2 - n1*v1 (when v2 >= v1)
  remeasured <- ti |> dplyr::filter(
    .data$tree_exists_1st & .data$tree_exists_2nd &
      !.data$volume_change_implausible & !.data$species_mismatch &
      .data$v_hub_m3_2nd >= .data$v_hub_m3_1st
  )
  expect_equal(
    remeasured$iv_hub_m3_ha_per_rep,
    remeasured$n_rep_ha_2nd * remeasured$v_hub_m3_2nd -
      remeasured$n_rep_ha_1st * remeasured$v_hub_m3_1st
  )

  # Re-measured plausible trees with shrinkage: iv = 0 (clamped by default)
  shrunk <- ti |> dplyr::filter(
    .data$tree_exists_1st & .data$tree_exists_2nd &
      !.data$volume_change_implausible & !.data$species_mismatch &
      .data$v_hub_m3_2nd < .data$v_hub_m3_1st
  )
  if (nrow(shrunk) > 0)
    expect_true(all(shrunk$iv_hub_m3_ha_per_rep == 0))

  # Ingrowth trees (only in 2nd survey): iv = n2*v2
  ingrowth <- ti |> dplyr::filter(!.data$tree_exists_1st & .data$tree_exists_2nd)
  expect_equal(
    ingrowth$iv_hub_m3_ha_per_rep,
    ingrowth$n_rep_ha_2nd * ingrowth$v_hub_m3_2nd
  )

  # 1st-survey-only trees: iv = 0
  first_only <- ti |> dplyr::filter(.data$tree_exists_1st & !.data$tree_exists_2nd)
  if (nrow(first_only) > 0)
    expect_true(all(first_only$iv_hub_m3_ha_per_rep == 0))
})


# --- Annual increment derived from periodic -----------------------------------

test_that("annual increment equals periodic increment divided by time span", {

  delta_t <- inc_classic$tree_increments$time_yr_2nd[[1]] -
    inc_classic$tree_increments$time_yr_1st[[1]]

  for (inc in list(inc_classic, inc_mean, inc_end, inc_trans)) {
    ti <- inc$tree_increments
    expect_equal(
      ti$iv_hub_m3_ha_yr_rep,
      ti$iv_hub_m3_ha_per_rep / delta_t
    )
  }
})


# --- Non-negative methods -----------------------------------------------------

test_that("rep_mean, rep_end, rep_trans produce non-negative tree-level increments", {

  # rep_classic is excluded: n_rep_ha weighting can produce negative ha-level
  # increments even for growing trees, which is statistically correct
  for (inc in list(inc_mean, inc_end, inc_trans)) {
    ti <- inc$tree_increments
    expect_true(
      all(ti$iv_hub_m3_ha_per_rep >= 0, na.rm = TRUE)
    )
  }
})


# --- No NA increments ---------------------------------------------------------

test_that("NA increments appear only on unmatchable rows", {

  # Unmatchable rows (rows where the tree was measured on one side but
  # could not be matched to the other inventory — typically inner-circle
  # trees without polar coordinates) carry NA in tree_exists_* on the
  # missing side. Per the NA-semantics design, tree_inc_repsurv()'s
  # case_when branches all fail on NA and the per-tree increment falls
  # through to NA_real_. For every other row the increment must be
  # non-NA. The test below asserts that one-to-one correspondence.
  for (inc in list(inc_classic, inc_mean, inc_end, inc_trans)) {
    ti <- inc$tree_increments
    is_unmatch <- is.na(ti$tree_exists_1st) | is.na(ti$tree_exists_2nd)
    expect_true(all(is.na(ti$iv_hub_m3_ha_per_rep) == is_unmatch))
    expect_true(all(is.na(ti$iv_hub_m3_ha_yr_rep) == is_unmatch))
  }
})


# --- rep_end and rep_trans agree without circle transition --------------------

test_that("rep_mean, rep_end, rep_trans agree for trees without circle transition", {

  sort_cols <- c("plot_id_1st", "tree_id_1st", "tree_id_2nd")
  ti_mean  <- inc_mean$tree_increments  |> dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  ti_end   <- inc_end$tree_increments   |> dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  ti_trans <- inc_trans$tree_increments |> dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))

  # Trees without circle transition: ingrowth, 1st-only, or n1 == n2
  no_ct <- with(ti_end,
    (!tree_exists_1st &  tree_exists_2nd) |   # ingrowth
    ( tree_exists_1st & !tree_exists_2nd) |   # 1st only
    ( tree_exists_1st &  tree_exists_2nd & n_rep_ha_1st == n_rep_ha_2nd)  # no change
  )

  expect_equal(
    ti_mean$iv_hub_m3_ha_per_rep[no_ct],
    ti_end$iv_hub_m3_ha_per_rep[no_ct]
  )
  expect_equal(
    ti_end$iv_hub_m3_ha_per_rep[no_ct],
    ti_trans$iv_hub_m3_ha_per_rep[no_ct]
  )
})

test_that("all four methods agree for re-measured trees without circle transition", {

  sort_cols <- c("plot_id_1st", "tree_id_1st", "tree_id_2nd")
  ti_classic <- inc_classic$tree_increments |> dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  ti_end     <- inc_end$tree_increments     |> dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))

  # Re-measured trees only, no circle transition (n1 == n2)
  remeas_no_ct <- with(ti_classic,
    tree_exists_1st & tree_exists_2nd & n_rep_ha_1st == n_rep_ha_2nd
  )

  expect_equal(
    ti_classic$iv_hub_m3_ha_per_rep[remeas_no_ct],
    ti_end$iv_hub_m3_ha_per_rep[remeas_no_ct]
  )
})


# --- Regression tests ---------------------------------------------------------
# Reference objects must be regenerated after code changes by running
# data-raw/inc_repsurv_sub10_reference_data.R with the updated package installed.

test_that("rep_classic tree_increments match reference object", {
  expect_equal(
    inc_classic$tree_increments |>
      dplyr::arrange(.data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd),
    inc_sub10_rep_classic |>
      dplyr::arrange(.data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd)
  )
})

test_that("rep_mean tree_increments match reference object", {
  expect_equal(
    inc_mean$tree_increments |>
      dplyr::arrange(.data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd),
    inc_sub10_rep_mean |>
      dplyr::arrange(.data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd)
  )
})

test_that("rep_end tree_increments match reference object", {
  expect_equal(
    inc_end$tree_increments |>
      dplyr::arrange(.data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd),
    inc_sub10_rep_end |>
      dplyr::arrange(.data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd)
  )
})

test_that("rep_trans tree_increments match reference object", {
  expect_equal(
    inc_trans$tree_increments |>
      dplyr::arrange(.data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd),
    inc_sub10_rep_trans |>
      dplyr::arrange(.data$plot_id_1st, .data$tree_id_1st, .data$tree_id_2nd)
  )
})


# --- clamp_plausible_shrinkage = FALSE -----------------------------------------

# Prepare matched trees for direct calls to tree_inc_repsurv
suppressWarnings(suppressMessages({
  inv_pt_mtch <- match_2_inventories(
    inv_a, inv_b, "plot_id", "baysf"
  )
  mtch_trs <- match_trees_on_inventory_repsurv_ccirc(
    inv_pt_mtch, inv_a, inv_b, inv_a_trees, inv_b_trees,
    tree_id_style = "baysf", progress_bar = FALSE
  )
}))

test_that("clamp_plausible_shrinkage = FALSE allows negative increments", {

  # Identify plausible shrinkage cases (v2 < v1, not flagged as implausible)
  shrunk <- mtch_trs |> dplyr::filter(
    .data$tree_exists_1st & .data$tree_exists_2nd &
      !.data$volume_change_implausible & !.data$species_mismatch &
      .data$v_hub_m3_2nd < .data$v_hub_m3_1st
  )

  # Only run if there are any plausible shrinkage cases in the test data
  skip_if(nrow(shrunk) == 0, "No plausible shrinkage cases in test data")

  # rep_classic: should have negative increments
  inc_nc <- tree_inc_repsurv(mtch_trs, "rep_classic",
                             clamp_plausible_shrinkage = FALSE)
  shrunk_nc <- inc_nc |> dplyr::filter(
    .data$tree_exists_1st & .data$tree_exists_2nd &
      !.data$volume_change_implausible & !.data$species_mismatch &
      .data$v_hub_m3_2nd < .data$v_hub_m3_1st
  )
  expect_true(any(shrunk_nc$iv_hub_m3_ha_per_rep < 0))

  # rep_mean: should have negative increments
  inc_nm <- tree_inc_repsurv(mtch_trs, "rep_mean",
                             clamp_plausible_shrinkage = FALSE)
  shrunk_nm <- inc_nm |> dplyr::filter(
    .data$tree_exists_1st & .data$tree_exists_2nd &
      !.data$volume_change_implausible & !.data$species_mismatch &
      .data$v_hub_m3_2nd < .data$v_hub_m3_1st
  )
  expect_true(any(shrunk_nm$iv_hub_m3_ha_per_rep < 0))

  # rep_end: should have negative increments
  inc_ne <- tree_inc_repsurv(mtch_trs, "rep_end",
                             clamp_plausible_shrinkage = FALSE)
  shrunk_ne <- inc_ne |> dplyr::filter(
    .data$tree_exists_1st & .data$tree_exists_2nd &
      !.data$volume_change_implausible & !.data$species_mismatch &
      .data$v_hub_m3_2nd < .data$v_hub_m3_1st
  )
  expect_true(any(shrunk_ne$iv_hub_m3_ha_per_rep < 0))
})

test_that("rep_trans warns when clamp_plausible_shrinkage = FALSE", {
  expect_warning(
    tree_inc_repsurv(mtch_trs, "rep_trans",
                     clamp_plausible_shrinkage = FALSE),
    "rep_trans.*always clamps"
  )
})

test_that("rep_trans still produces non-negative increments with clamp = FALSE", {
  inc_nt <- suppressWarnings(
    tree_inc_repsurv(mtch_trs, "rep_trans",
                     clamp_plausible_shrinkage = FALSE)
  )
  expect_true(all(inc_nt$iv_hub_m3_ha_per_rep >= 0, na.rm = TRUE))
})


# --- match_tree_stats: the two "in both" counts are NOT mirror images ---------
# reclassify_pseudo_ingrowth_ccirc() sets tree_exists_1st = TRUE on 2nd-only
# rows but leaves tree_id_1st at NA (no individual inv-1 record was ever
# identified - that is why the tree was unmatchable). Such rows can only be
# counted from the inv-2 side, so the two counts differ by exactly the number
# of pseudo-ingrowth trees. Documented as an invariant, asserted here.

test_that("n_in_both_2nd - n_in_both_1st equals n_pseudo_ingrowth", {
  st <- inc_classic$match_tree_stats

  expect_true(all(c("n_in_both_1st", "n_in_both_2nd", "n_pseudo_ingrowth") %in%
                    names(st)))
  expect_equal(
    unname(st[["n_in_both_2nd"]] - st[["n_in_both_1st"]]),
    unname(st[["n_pseudo_ingrowth"]])
  )

  # And the mechanism behind it: every both-TRUE row lacking an inv-1 tree id
  # is a reclassified pseudo-ingrowth row, and vice versa.
  ti <- inc_classic$tree_increments
  both <- ti[ti$tree_exists_1st %in% TRUE & ti$tree_exists_2nd %in% TRUE, ]
  expect_identical(
    is.na(both$tree_id_1st),
    both$pseudo_ingrowth_corrected %in% TRUE
  )
})

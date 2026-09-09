# Tests for the increment aggregation pipeline:
#   inv_inc_tree_consolidate, inv_inc_tree_extend, inv_inc_summaric,
#   inv_inc_big_overview_matches_only, inv_inc_big_overview_2nd_only,
#   tree_inc_gnfi_2012
#
# Key invariants under test:
#   - inv_period is propagated through the list outputs of the pipeline
#   - tree_inc_extd self-describes its inventory period via time_yr_prev and
#     time_yr_2nd (uniform across all rows)
#   - inv_inc_summaric returns a list with $overall and $by_species_group
#   - Per-plot inc_summaric values sum across species groups to the overall
#     per-plot value
#   - In inv_inc_big_overview_matches_only, the by_species_group
#     aggregations sum across species groups to the corresponding overall
#     values (the central sum-consistency invariant): denominators stay at
#     the full area across all plots so plots without a given species
#     group contribute their area but a zero numerator

suppressWarnings(suppressMessages({

  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory

  # Tree lists and the first two increment steps are shipped ready-made for this
  # pair, see ?data_ex3_trees_essentials and ?data_ex3_increment_interim.
  inv_a_trees <- data_ex3_previous_sample_trees_essentials
  inv_b_trees <- data_ex3_sample_trees_essentials

  inc_rep <- data_ex3_increment_matched
  inc_fl <- data_ex3_increment_fills
  inc_cons <- inv_inc_tree_consolidate(inc_fl, fill_option = "standard")
  inc_extd <- inv_inc_tree_extend(inc_cons, inv_a_trees, inv_b_trees, inv_b)
  inc_sum  <- inv_inc_summaric(
    inc_rep$tree_increments, inc_fl$fills_forward,
    inv_a_trees, inv_b_trees
  )
  bo <- inv_inc_big_overview_matches_only(
    inc_fl, inv_a_trees, inv_b_trees, inv_b,
    style = "generic", inc_summaric = inc_sum
  )
}))


# --- inv_period propagation through list outputs ------------------------------

test_that("inv_period is attached to repsurv_ccirc and fill_gaps_gnfi3 outputs", {
  expect_true("inv_period" %in% names(inc_rep))
  expect_equal(inc_rep$inv_period$period_yr, 11)

  expect_true("inv_period" %in% names(inc_fl))
  expect_identical(inc_fl$inv_period, inc_rep$inv_period)
})


test_that("inv_inc_tree_consolidate handles the non-data-frame inv_period element", {
  # Adding the inv_period list element to fill_gaps' output must not break
  # consolidate's internal bind_rows.
  expect_s3_class(inc_cons, "tbl_df")
  expect_true(nrow(inc_cons) > 0)
})


# --- tree_inc_extd carries inventory period metadata as columns ---------------

test_that("inv_inc_tree_extend keeps time_yr_2nd and time_yr_prev uniform", {
  expect_true(all(c("time_yr_prev", "time_yr_2nd") %in% names(inc_extd)))
  expect_equal(unique(inc_extd$time_yr_prev), 2009)
  expect_equal(unique(inc_extd$time_yr_2nd), 2020)
})


# --- inv_inc_summaric output structure ----------------------------------------

test_that("inv_inc_summaric returns list(overall, by_species_group)", {
  expect_named(inc_sum, c("overall", "by_species_group"))
  expect_s3_class(inc_sum$overall, "tbl_df")
  expect_s3_class(inc_sum$by_species_group, "tbl_df")

  expect_true(all(c(
    "plot_id", "v_hub_m3_ha_1st", "v_hub_m3_ha_2nd",
    "v_hub_m3_ha_aus", "inc_summaric_m3_ha_yr"
  ) %in% names(inc_sum$overall)))
  expect_true(all(c(
    "plot_id", "species_group", "v_hub_m3_ha_1st", "v_hub_m3_ha_2nd",
    "v_hub_m3_ha_aus", "inc_summaric_m3_ha_yr"
  ) %in% names(inc_sum$by_species_group)))
})


test_that("Per-plot inc_summaric: species sums match overall", {
  cmp <- inc_sum$by_species_group |>
    dplyr::group_by(.data$plot_id) |>
    dplyr::summarise(
      sum_sg_v_1st = sum(.data$v_hub_m3_ha_1st),
      sum_sg_v_2nd = sum(.data$v_hub_m3_ha_2nd),
      sum_sg_v_aus = sum(.data$v_hub_m3_ha_aus),
      sum_sg_inc   = sum(.data$inc_summaric_m3_ha_yr),
      .groups = "drop"
    ) |>
    dplyr::left_join(inc_sum$overall, by = "plot_id")

  expect_equal(cmp$sum_sg_v_1st, cmp$v_hub_m3_ha_1st)
  expect_equal(cmp$sum_sg_v_2nd, cmp$v_hub_m3_ha_2nd)
  expect_equal(cmp$sum_sg_v_aus, cmp$v_hub_m3_ha_aus)
  expect_equal(cmp$sum_sg_inc,   cmp$inc_summaric_m3_ha_yr)
})


test_that("inv_inc_summaric refuses empty input", {
  expect_error(
    inv_inc_summaric(
      inc_rep$tree_increments[0, ], inc_fl$fills_forward,
      inv_a_trees, inv_b_trees
    ),
    "empty"
  )
})


# --- inv_inc_big_overview_matches_only top-level structure --------------------

test_that("inv_inc_big_overview_matches_only top level is list(overall, by_species_group, method)", {
  expect_named(bo, c("overall", "by_species_group", "method"))
  expect_named(
    bo$overall,
    c("iv_by_groups", "iv_total", "v_total", "n_plots")
  )
  expect_named(
    bo$by_species_group,
    c("iv_by_groups", "iv_total", "v_total")
  )
})


# --- Sum-consistency in big_overview ------------------------------------------

test_that("by_species_group$iv_total sums equal overall$iv_total per fill_option", {
  sg_sums <- bo$by_species_group$iv_total |>
    dplyr::group_by(.data$fill_option) |>
    dplyr::summarise(
      n              = sum(.data$n),
      iv_m3_yr_total = sum(.data$iv_m3_yr_total),
      iv_m3_ha_yr    = sum(.data$iv_m3_ha_yr),
      .groups        = "drop"
    )

  cmp <- bo$overall$iv_total |>
    dplyr::select("fill_option", "n", "iv_m3_yr_total", "iv_m3_ha_yr") |>
    dplyr::left_join(sg_sums, by = "fill_option", suffix = c("_ovr", "_sg"))

  expect_equal(cmp$n_ovr, cmp$n_sg)
  expect_equal(cmp$iv_m3_yr_total_ovr, cmp$iv_m3_yr_total_sg)
  expect_equal(cmp$iv_m3_ha_yr_ovr, cmp$iv_m3_ha_yr_sg)
})


test_that("by_species_group$v_total sums equal overall$v_total", {
  expect_equal(sum(bo$by_species_group$v_total$v_hub_m3),
               bo$overall$v_total$v_hub_m3)
  expect_equal(sum(bo$by_species_group$v_total$v_hub_m3_ha),
               bo$overall$v_total$v_hub_m3_ha)
})


test_that("by_species_group$iv_by_groups sums equal overall$iv_by_groups", {
  ovr_g <- bo$overall$iv_by_groups |>
    dplyr::arrange(.data$fill_option, .data$id_able_tree, .data$tree_status)
  sg_g <- bo$by_species_group$iv_by_groups |>
    dplyr::group_by(
      .data$fill_option, .data$id_able_tree, .data$tree_status
    ) |>
    dplyr::summarise(
      n              = sum(.data$n),
      iv_m3_yr_total = sum(.data$iv_m3_yr_total),
      iv_m3_ha_yr    = sum(.data$iv_m3_ha_yr),
      .groups        = "drop"
    ) |>
    dplyr::arrange(.data$fill_option, .data$id_able_tree, .data$tree_status)

  expect_equal(sg_g$n, ovr_g$n)
  expect_equal(sg_g$iv_m3_yr_total, ovr_g$iv_m3_yr_total)
  expect_equal(sg_g$iv_m3_ha_yr, ovr_g$iv_m3_ha_yr)
})


# --- generic style: pseudo-ingrowth is its own tree_status category -----------

test_that("style = 'generic' carries a separate 'pseudo_ingrown' tree_status", {
  n_pseudo <- sum(inc_rep$tree_increments$pseudo_ingrowth_corrected)
  # The sub10 pipeline must actually contain pseudo-ingrowth for this test
  # to be meaningful.
  expect_gt(n_pseudo, 0L)

  g <- bo$overall$iv_by_groups
  expect_true("pseudo_ingrown" %in% g$tree_status)

  # Per fill_option exactly the reclassified trees show up in the category,
  # and they are all id-able (both surveys TRUE after reclassification).
  pseudo_rows <- g[g$tree_status == "pseudo_ingrown", ]
  expect_true(all(pseudo_rows$n == n_pseudo))
  expect_true(all(pseudo_rows$id_able_tree))

  # No "other" fall-through was provoked by the new branch.
  expect_false("other" %in% g$tree_status)
})


test_that("style = 'baysf' keeps pseudo-ingrowth inside 'living' (unchanged)", {
  bo_bay <- inv_inc_big_overview_matches_only(
    inc_fl, inv_a_trees, inv_b_trees, inv_b,
    style = "baysf", inc_summaric = inc_sum
  )
  levels_bay <- bo_bay$overall$iv_by_groups$tree_status
  expect_false("pseudo_ingrown" %in% levels_bay)
  # The baysf category set is the established one (table-63 style).
  expect_true("living" %in% levels_bay)
})


test_that("inc_summaric row in by_species_group$iv_total sums to overall inc_summaric", {
  ovr_inc <- bo$overall$iv_total |>
    dplyr::filter(.data$fill_option == "inc_summaric")
  sg_inc  <- bo$by_species_group$iv_total |>
    dplyr::filter(.data$fill_option == "inc_summaric")

  expect_equal(sum(sg_inc$iv_m3_yr_total), ovr_inc$iv_m3_yr_total)
  expect_equal(sum(sg_inc$iv_m3_ha_yr),    ovr_inc$iv_m3_ha_yr)
  expect_equal(sum(sg_inc$n), ovr_inc$n)
})


# --- Denominator invariant: all species rows share the same total area --------

test_that("area_total_ha is the same for all rows in by_species_group$iv_total", {
  # This is the formal expression of the sum-consistency requirement: plots
  # without a given species group still contribute their area to the
  # denominator.
  expect_equal(length(unique(bo$by_species_group$iv_total$area_total_ha)), 1)
  expect_equal(
    bo$by_species_group$iv_total$area_total_ha[1],
    bo$overall$iv_total$area_total_ha[1]
  )
})


# === tree_inc_gnfi_2012 sign convention =====================================

test_that("tree_inc_gnfi_2012 produces positive increments in both directions", {
  # Use the existing inv_b_trees fixture; restrict to a handful of trees to
  # keep the test fast and the assertions easy to read.
  trees_smp <- inv_b_trees |>
    dplyr::filter(!.data$removal, .data$age_yr > 10, .data$dbh_cm > 7) |>
    head(20)

  # Forward (dt > 0)
  fwd <- tree_inc_gnfi_2012(trees_smp, dt = 5)
  expect_true(all(c(
    "dbh_cm_gnfi3", "height_m_gnfi3", "v_m3_gnfi3",
    "v_hub_m3_gnfi3", "iv_hub_m3_yr_gnfi3"
  ) %in% names(fwd)))
  expect_true(all(fwd$iv_hub_m3_yr_gnfi3 >= 0))
  # Forward: future volume >= current volume
  expect_true(all(fwd$v_hub_m3_gnfi3 >= fwd$v_hub_m3 - 1e-9))

  # Backward (dt < 0)
  bwd <- tree_inc_gnfi_2012(trees_smp, dt = -5)
  expect_true(all(bwd$iv_hub_m3_yr_gnfi3 >= 0))
  # Backward: past volume <= current volume
  expect_true(all(bwd$v_hub_m3_gnfi3 <= bwd$v_hub_m3 + 1e-9))
})


test_that("tree_inc_gnfi_2012 rejects dt = 0", {
  trees_smp <- inv_b_trees |> head(5)
  expect_error(tree_inc_gnfi_2012(trees_smp, dt = 0))
})


# === inv_inc_big_overview_2nd_only ===========================================

suppressWarnings(suppressMessages({
  bo_2nd <- inv_inc_big_overview_2nd_only(inc_fl, inv_b_trees, inv_b)
}))


test_that("sub10 fixture has the expected number of second-only plots", {
  # Sanity check on the fixture so the assertions below have a known shape.
  expect_equal(length(inc_fl$plot_matches$in_b_only$plot_id), 1L)
})


test_that("inv_inc_big_overview_2nd_only top level is list(overall, by_species_group)", {
  expect_named(bo_2nd, c("overall", "by_species_group"))
  expect_named(
    bo_2nd$overall,
    c("iv_by_groups", "iv_total", "v_total", "n_plots")
  )
  expect_named(
    bo_2nd$by_species_group,
    c("iv_by_groups", "iv_total", "v_total")
  )
})


test_that("inv_inc_big_overview_2nd_only iv_total uses the gnfi3_backward label", {
  expect_equal(bo_2nd$overall$iv_total$fill_option, "gnfi3_backward")
  expect_true(all(bo_2nd$by_species_group$iv_total$fill_option == "gnfi3_backward"))
})


test_that("inv_inc_big_overview_2nd_only n_plots matches the in_b_only count", {
  expect_equal(
    bo_2nd$overall$n_plots$n_plots,
    length(inc_fl$plot_matches$in_b_only$plot_id)
  )
})


test_that("by_species_group$iv_total sums equal overall$iv_total", {
  expect_equal(
    sum(bo_2nd$by_species_group$iv_total$iv_m3_yr_total),
    bo_2nd$overall$iv_total$iv_m3_yr_total
  )
  expect_equal(
    sum(bo_2nd$by_species_group$iv_total$iv_m3_ha_yr),
    bo_2nd$overall$iv_total$iv_m3_ha_yr
  )
  expect_equal(
    sum(bo_2nd$by_species_group$iv_total$n),
    bo_2nd$overall$iv_total$n
  )
})


test_that("by_species_group$v_total sums equal overall$v_total", {
  expect_equal(
    sum(bo_2nd$by_species_group$v_total$v_hub_m3),
    bo_2nd$overall$v_total$v_hub_m3
  )
  expect_equal(
    sum(bo_2nd$by_species_group$v_total$v_hub_m3_ha),
    bo_2nd$overall$v_total$v_hub_m3_ha
  )
})


test_that("inv_inc_big_overview_2nd_only returns shell when no second-only plots", {
  inc_fl_empty <- inc_fl
  inc_fl_empty$plot_matches$in_b_only <- inc_fl_empty$plot_matches$in_b_only[0, ]
  bo_empty <- inv_inc_big_overview_2nd_only(
    inc_fl_empty, inv_b_trees, inv_b
  )

  expect_equal(bo_empty$overall$n_plots$n_plots, 0L)
  expect_equal(bo_empty$overall$n_plots$area_total_ha, 0)
  expect_equal(nrow(bo_empty$overall$iv_total), 0L)
  expect_equal(nrow(bo_empty$overall$iv_by_groups), 0L)
  expect_equal(nrow(bo_empty$overall$v_total), 0L)
  expect_equal(nrow(bo_empty$by_species_group$iv_total), 0L)
})


test_that("inv_inc_big_overview_2nd_only warns on dt > 0", {
  expect_warning(
    inv_inc_big_overview_2nd_only(inc_fl, inv_b_trees, inv_b, dt = 5),
    "only backward estimates make sense"
  )
})


# === Combined inv_inc_big_overview() =========================================

suppressWarnings(suppressMessages({
  bo_comb <- inv_inc_big_overview(
    inc_fl, inv_a_trees, inv_b_trees, inv_b,
    style = "generic", inc_summaric = inc_sum
  )
}))


test_that("inv_inc_big_overview top level has matches_only, second_only, combined", {
  expect_named(
    bo_comb,
    c("matches_only", "second_only", "combined", "inv_period", "method")
  )
  # inv_period and method are carried through from the input
  expect_identical(bo_comb$inv_period, inc_fl$inv_period)
  expect_identical(bo_comb$method, inc_fl$method)
})


test_that("matches_only and second_only are passed through verbatim", {
  # Compare with direct calls
  ref_m <- inv_inc_big_overview_matches_only(
    inc_fl, inv_a_trees, inv_b_trees, inv_b,
    style = "generic", inc_summaric = inc_sum
  )
  ref_2 <- inv_inc_big_overview_2nd_only(inc_fl, inv_b_trees, inv_b, dt = -5)
  expect_equal(bo_comb$matches_only, ref_m)
  expect_equal(bo_comb$second_only, ref_2)
})


test_that("combined fill_options are prefixed with 'combined_'", {
  expect_true(all(grepl(
    "^combined_", bo_comb$combined$overall$iv_total$fill_option
  )))
  expect_true(all(grepl(
    "^combined_", bo_comb$combined$by_species_group$iv_total$fill_option
  )))
})


test_that("Sum-consistency: each combined row equals matches_only + second_only", {
  ovr_m <- bo_comb$matches_only$overall$iv_total
  ovr_s <- bo_comb$second_only$overall$iv_total
  ovr_c <- bo_comb$combined$overall$iv_total

  # iv_m3_yr_total: per-row sum of matched + (single second_only row)
  expect_equal(
    ovr_c$iv_m3_yr_total,
    ovr_m$iv_m3_yr_total + ovr_s$iv_m3_yr_total[1L]
  )
  # area_total_ha
  expect_equal(
    ovr_c$area_total_ha,
    ovr_m$area_total_ha + ovr_s$area_total_ha[1L]
  )
  # n
  expect_equal(ovr_c$n, ovr_m$n + ovr_s$n[1L])
  # iv_m3_ha_yr is the resulting ratio
  expect_equal(ovr_c$iv_m3_ha_yr, ovr_c$iv_m3_yr_total / ovr_c$area_total_ha)
})


test_that("combined CI95 columns are NA throughout", {
  expect_true(all(is.na(bo_comb$combined$overall$iv_total$ci95_iv_m3_yr_total)))
  expect_true(all(is.na(bo_comb$combined$overall$iv_total$ci95_iv_m3_ha_yr)))
  expect_true(all(is.na(
    bo_comb$combined$by_species_group$iv_total$ci95_iv_m3_yr_total
  )))
  expect_true(all(is.na(
    bo_comb$combined$by_species_group$iv_total$ci95_iv_m3_ha_yr
  )))
})


test_that("combined v_total is additive", {
  expect_equal(
    bo_comb$combined$overall$v_total$v_hub_m3,
    bo_comb$matches_only$overall$v_total$v_hub_m3 +
      bo_comb$second_only$overall$v_total$v_hub_m3
  )
  expect_equal(
    bo_comb$combined$overall$v_total$area_total_ha,
    bo_comb$matches_only$overall$v_total$area_total_ha +
      bo_comb$second_only$overall$v_total$area_total_ha
  )
})


test_that("combined n_plots is additive (now correct after the length->sum fix)", {
  expect_equal(
    bo_comb$combined$overall$n_plots$n_plots,
    bo_comb$matches_only$overall$n_plots$n_plots +
      bo_comb$second_only$overall$n_plots$n_plots
  )
})


test_that("by_species_group sum across species equals combined overall per fill_option", {
  sg <- bo_comb$combined$by_species_group$iv_total
  ovr <- bo_comb$combined$overall$iv_total

  per_fo <- sg |>
    dplyr::group_by(.data$fill_option) |>
    dplyr::summarise(
      iv_m3_yr_total = sum(.data$iv_m3_yr_total),
      n              = sum(.data$n),
      .groups        = "drop"
    )

  cmp <- dplyr::left_join(
    ovr |> dplyr::select("fill_option",
                         iv_yr_ovr = "iv_m3_yr_total",
                         n_ovr     = "n"),
    per_fo |> dplyr::rename(iv_yr_sg = "iv_m3_yr_total", n_sg = "n"),
    by = "fill_option"
  )
  expect_equal(cmp$iv_yr_ovr, cmp$iv_yr_sg)
  expect_equal(cmp$n_ovr, cmp$n_sg)
})


# === Regression: combined view with no second-only plots =====================

test_that("inv_inc_big_overview() merges cleanly when all plots are matched", {
  # When `inc_fl$plot_matches$in_b_only` is empty, the second_only sub-
  # overview comes from the zero-row shell in empty_big_overview_2nd_only().
  # Its species_group columns are bare character(); the matched side
  # carries a typed fe_species_* vector. inv_inc_big_overview() now
  # casts bo_2$by_species_group$*$species_group to the matched
  # prototype before further processing, so the dplyr::full_join() and
  # dplyr::anti_join() in combine_matches_and_2nd_only_overviews() and
  # the dplyr::left_join() in output_increment_overall() succeed instead of
  # erroring out on "incompatible types".
  inc_fl_no_2nd <- inc_fl
  inc_fl_no_2nd$plot_matches$in_b_only <-
    inc_fl_no_2nd$plot_matches$in_b_only[0, ]

  suppressWarnings(suppressMessages({
    bo_no_2nd <- inv_inc_big_overview(
      inc_fl_no_2nd, inv_a_trees, inv_b_trees, inv_b,
      style = "generic", inc_summaric = inc_sum
    )
  }))

  # second_only side is empty but its species_group columns now share
  # the matched side's class -- a regression guard against the
  # character() shell leaking out.
  expect_equal(bo_no_2nd$second_only$overall$n_plots$n_plots, 0L)
  expect_equal(nrow(bo_no_2nd$second_only$by_species_group$iv_total), 0L)
  expect_identical(
    class(bo_no_2nd$second_only$by_species_group$iv_total$species_group),
    class(bo_no_2nd$matches_only$by_species_group$iv_total$species_group)
  )
  expect_identical(
    class(bo_no_2nd$second_only$by_species_group$v_total$species_group),
    class(bo_no_2nd$matches_only$by_species_group$v_total$species_group)
  )

  # combined side equals the matched side (numerically, modulo CI95 NAs
  # and the "combined_" prefix on fill_option). At minimum: a non-zero
  # number of rows and the same number of distinct species as on the
  # matched side. format() is the proper S3 method for the fe_species_*
  # vectors used as species_group.
  expect_gt(nrow(bo_no_2nd$combined$by_species_group$iv_total), 0L)
  expect_setequal(
    unique(format(bo_no_2nd$combined$by_species_group$iv_total$species_group)),
    unique(format(bo_no_2nd$matches_only$by_species_group$iv_total$species_group))
  )

  # output_increment_overall() builds tables via dplyr::left_join() on
  # species_group; that join also fails on type mismatch if bo_2's
  # species_group is bare character(). Verify it runs end-to-end.
  out_no_2nd <- output_increment_overall(bo_no_2nd, fill_option = "standard")
  expect_named(
    out_no_2nd,
    c("meta", "table_matched", "table_2nd_only", "table_combined")
  )
  # table_2nd_only has the canonical species rows + a total row,
  # everything in the single-tree block is 0 (no 2nd-only contribution).
  expect_gt(nrow(out_no_2nd$table_2nd_only), 1L)
  expect_true(all(out_no_2nd$table_2nd_only$iv_m3_yr_total_st == 0 |
                  is.na(out_no_2nd$table_2nd_only$iv_m3_yr_total_st)))
})


# === Older test: inv_inc_big_overview_2nd_only ci95 contract =================

test_that("inv_inc_big_overview_2nd_only ci95 columns are NA by contract", {
  # CI columns exist for structural compatibility with the matched-plots
  # overview, but values are intentionally NA: the increments come from a
  # model, not from empirical re-measurement, so a confidence interval
  # would suggest spurious precision. See the function head.
  expect_true(all(is.na(bo_2nd$overall$iv_total$ci95_iv_m3_yr_total)))
  expect_true(all(is.na(bo_2nd$overall$iv_total$ci95_iv_m3_ha_yr)))
  expect_true(all(is.na(bo_2nd$by_species_group$iv_total$ci95_iv_m3_yr_total)))
  expect_true(all(is.na(bo_2nd$by_species_group$iv_total$ci95_iv_m3_ha_yr)))
})

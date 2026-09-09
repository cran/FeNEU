# Tests for inv_increment_repeated_survey(), the bundled repeated-survey
# increment chain, and for the whole-enterprise coverage it guarantees.
#
# Depth (helper-depth.R): two costly tests stay on CRAN on purpose. The
# structure test is the only full run of the repeated-survey chain, and the
# progressr test hangs on a foreign package, which is exactly what CRAN's
# checks are worth to us. The convergence test stays as well, with its loop
# shortened to one fill option -- it guards the fault that once made every
# per-ha figure 1.4 percent too low.
#
# The tree preparation deliberately uses the CANONICAL write-back chain
# (fill_heights_back() -> pull_trees() -> trees_add_essentials()), not the flat
# fold that most other increment tests use. That chain does not carry the helper
# columns d_q_cm / h_q_m / h_est_m, and relying on them has broken the increment
# chain before (see inv_inc_tree_extend()). Testing on the chain the user
# scripts actually run keeps that class of defect visible.

prep_canonical <- function(inv) {
  suppressWarnings(suppressMessages({
    twh <- inv |> pull_trees() |> height_complete_inventory()
    inv |> fill_heights_back(twh) |> pull_trees() |>
      trees_add_essentials(method = "BaySF")
  }))
}

inv_a <- data_ex3_previous_sample_fe_inventory
inv_b <- data_ex3_sample_fe_inventory

# The shipped tree lists ARE the canonical chain: data-raw/ builds them with the
# very same steps as prep_canonical() above. Using them here rather than running
# the chain twice keeps the file's premise (see header) while saving two cluster
# starts; the first test below runs the chain for real and anchors that identity.
trees_a <- data_ex3_previous_sample_trees_essentials
trees_b <- data_ex3_sample_trees_essentials

# Many tests below examine different aspects of the SAME bundle, and one run of
# the chain costs a few seconds. Results are therefore cached under a key built
# from the EFFECTIVE arguments: the function's own defaults are filled in, so
# that e.g. run_bundle(dt_second_only = -5) and run_bundle() share one entry.
# The defaults are read from the function rather than repeated here -- were they
# repeated and one of them changed, two different bundles would collide on one
# key and a test would silently check the wrong object.
.bundle_defaults <- local({
  fmls <- formals(inv_increment_repeated_survey)
  fmls <- fmls[setdiff(
    names(fmls), c("inv_1st", "inv_2nd", "inv_1st_trees", "inv_2nd_trees")
  )]
  out <- lapply(fmls, function(d) {
    v <- eval(d)
    if (length(v) > 1L) v[[1L]] else v   # match.arg() takes the first
  })
  # The tests' own default, see run_bundle(): keep the console quiet.
  out$progress_bar <- FALSE
  out
})

.bundle_cache <- new.env(parent = emptyenv())

run_bundle <- function(...) {
  # Default the console bar off so the test output stays readable; the two
  # progress tests below set it explicitly.
  args <- utils::modifyList(.bundle_defaults, list(...))
  args <- args[order(names(args))]
  key <- paste(
    names(args),
    vapply(args, function(v) paste(format(v), collapse = "/"), character(1)),
    sep = "=", collapse = ","
  )
  if (is.null(.bundle_cache[[key]])) {
    .bundle_cache[[key]] <- suppressWarnings(suppressMessages(do.call(
      inv_increment_repeated_survey,
      c(list(inv_a, inv_b, trees_a, trees_b), args)
    )))
  }
  .bundle_cache[[key]]
}


test_that("the canonical write-back chain carries no d_q_cm helper column", {
  skip_below_depth("full")
  # Guards the premise of this file: if this ever changes, the tests below stop
  # exercising the chain they are meant to exercise. The chain is run for real
  # here -- once -- and the shipped list is checked against it, so that the
  # other tests may use the shipped list without weakening what they cover.
  chain_b <- prep_canonical(inv_b)

  expect_false("d_q_cm" %in% names(chain_b))
  expect_false("h_est_m" %in% names(chain_b))
  expect_equal(trees_b, chain_b)
})


test_that("inv_increment_repeated_survey() returns the documented structure", {
  inc <- run_bundle()

  expect_s3_class(inc, "fe_increment_repsurv")
  expect_true(is_fe_increment_repsurv(inc))
  expect_named(
    inc, c("trees", "overview", "summaric", "meta", "steps"), ignore.order = TRUE
  )
  expect_s3_class(inc$trees, "data.frame")

  # The meta describes the whole enterprise and the chosen options
  expect_true(all(
    c("year", "inv_year_prev", "period_yr", "area_ha", "n_plots",
      "method", "fill_option") %in% names(inc$meta)
  ))
  expect_identical(inc$meta$method, "rep_classic")
  expect_identical(inc$meta$fill_option, "standard")
  expect_identical(inc$meta$n_plots, nrow(inv_b))

  # The intermediate steps hold several per-tree frames and grow with the
  # inventory, so they are not returned unless asked for.
  expect_null(inc$steps)

  inc_steps <- run_bundle(keep_steps = TRUE)
  expect_named(
    inc_steps$steps,
    c("tree_level", "filled", "consolidated", "extended_matched"),
    ignore.order = TRUE
  )

  # Asking for them changes nothing but their presence
  expect_equal(inc_steps$trees,    inc$trees)
  expect_equal(inc_steps$overview, inc$overview)
})


test_that("the bundle reproduces the manual six-step chain exactly", {
  # Runs the chain twice over, once bundled and once by hand. What it guards is
  # that the bundle stays a faithful shorthand, which cannot break through an
  # R or dependency update alone -- only through an edit in this package, and
  # then it fails here for us before it reaches anyone else.
  skip_below_depth("full")

  inc <- run_bundle(method = "rep_mean", fill_option = "min_estimates")

  manual <- suppressWarnings(suppressMessages({
    tl <- inv_increment_repsurv_ccirc(
      inv_a, inv_b, trees_a, trees_b,
      match_type = "plot_id", plot_id_style = "baysf", tree_id_style = "baysf",
      method = "rep_mean", progress_bar = FALSE
    )
    fl <- inv_inc_fill_gaps_gnfi3(tl, trees_a, trees_b, include_reptrees = TRUE)
    ex <- inv_inc_tree_consolidate(fl, fill_option = "min_estimates") |>
      inv_inc_tree_extend(trees_a, trees_b, inv_b)
    inv_inc_tree_extend_combined(ex, fl, trees_b)
  }))

  expect_equal(nrow(inc$trees), nrow(manual))
  expect_equal(
    sum(inc$trees$iv_hub_m3_ha_yr), sum(manual$iv_hub_m3_ha_yr)
  )
})


test_that("base tables and enterprise level converge on the same increment", {
  # The point of folding in the second-only plots: the base tables express
  # their per-ha increment over the FULL operation area, so their total must
  # equal the enterprise-level `combined` row. Without the fold the numerator
  # would cover only the matched plots while the denominator covers all of
  # them, and every per-ha figure would come out too low.
  # The convergence itself is a property of the fold, not of the fill option, so
  # checking the default one exercises the mechanism. The other two are run
  # where there is time for them.
  fill_options <- if (fe_test_depth() == "full")
    c("standard", "min_estimates", "all_estimates") else "standard"

  for (fo in fill_options) {
    inc <- run_bundle(fill_option = fo)

    bt <- suppressWarnings(suppressMessages(
      increment_base_table(inc, by_class = "age")
    ))
    ref <- inc$overview$combined$overall$iv_total |>
      dplyr::filter(.data$fill_option == paste0("combined_", fo))

    expect_equal(
      bt$all_total$iv_m3_yr_total, ref$iv_m3_yr_total,
      info = paste("total, fill_option =", fo)
    )
    expect_equal(
      bt$all_total$iv_m3_ha_yr, ref$iv_m3_ha_yr,
      info = paste("per ha, fill_option =", fo)
    )
  }
})


test_that("dt_second_only reaches both sides that estimate second-only plots", {
  # The second-only plots are estimated twice over: once for the per-tree frame
  # (inv_inc_tree_extend_combined) and once for the enterprise-level overview
  # (inv_inc_big_overview's dt_2nd_only). If the argument reaches only one of
  # them, base tables and enterprise level drift apart silently as soon as the
  # default is left -- which is exactly the convergence the fold establishes.
  # Guards a defect that was here once: the argument reached only one of the two
  # places. That is an edit in this package, not something an R update brings,
  # so it is checked where there is time for three more chain runs.
  skip_below_depth("full")

  b_only <- run_bundle(keep_steps = TRUE)$steps$filled$plot_matches$in_b_only$plot_id
  skip_if(length(b_only) == 0, "example data have no second-only plots")

  inc <- run_bundle(dt_second_only = -10)

  bt <- suppressWarnings(suppressMessages(
    increment_base_table(inc, by_class = "age")
  ))
  ref <- inc$overview$combined$overall$iv_total |>
    dplyr::filter(.data$fill_option == "combined_standard")

  expect_equal(bt$all_total$iv_m3_yr_total, ref$iv_m3_yr_total)
  expect_equal(bt$all_total$iv_m3_ha_yr,    ref$iv_m3_ha_yr)

  # And the value must actually make a difference, otherwise the test above
  # would pass even if dt_second_only were ignored on both sides.
  inc_5 <- run_bundle(dt_second_only = -5)
  so_10 <- inc$trees   |> dplyr::filter(.data$plot_id %in% b_only)
  so_5  <- inc_5$trees |> dplyr::filter(.data$plot_id %in% b_only)
  expect_false(isTRUE(all.equal(
    sum(so_10$iv_hub_m3_ha_yr), sum(so_5$iv_hub_m3_ha_yr)
  )))
})


test_that("include_second_only = FALSE yields the matched-plots-only view", {
  # A second view of the same chain rather than a mechanism of its own.
  skip_below_depth("full")

  inc_all <- run_bundle(include_second_only = TRUE, keep_steps = TRUE)
  inc_mat <- run_bundle(include_second_only = FALSE)

  # The matched-only frame can never have more rows than the combined one.
  expect_lte(nrow(inc_mat$trees), nrow(inc_all$trees))

  b_only <- inc_all$steps$filled$plot_matches$in_b_only$plot_id
  if (length(b_only) > 0) {
    # Second-only plots appear in the combined frame and not in the matched one
    expect_true(any(inc_all$trees$plot_id %in% b_only))
    expect_false(any(inc_mat$trees$plot_id %in% b_only))
  }
})


test_that("second-only trees are estimated under every fill_option", {
  # min_estimates avoids estimates wherever a measured repeated-survey
  # increment exists. On second-only plots none can exist by construction, so
  # those trees are estimated there too -- zeroing them would put their area
  # into the denominator with an empty numerator.
  # Whether a fill option zeroes those trees is a property of the options, so
  # this one walks all three -- where there is time for it.
  skip_below_depth("full")

  b_only <- run_bundle(keep_steps = TRUE)$steps$filled$plot_matches$in_b_only$plot_id
  skip_if(length(b_only) == 0, "example data have no second-only plots")

  for (fo in c("standard", "min_estimates", "all_estimates")) {
    inc <- run_bundle(fill_option = fo)
    so <- inc$trees |> dplyr::filter(.data$plot_id %in% b_only)
    expect_true(all(so$iv_filled), info = paste("fill_option =", fo))
  }
})


test_that("the aggregation and output steps accept the bundle directly", {
  skip_below_depth("full")
  inc <- run_bundle()

  bt_bundle <- suppressWarnings(suppressMessages(
    increment_base_table(inc, by_class = "age")
  ))
  bt_frame <- suppressWarnings(suppressMessages(
    increment_base_table(inc$trees, by_class = "age")
  ))
  expect_equal(bt_bundle$all_total, bt_frame$all_total)

  ms_bundle <- suppressWarnings(suppressMessages(
    increment_base_table_main_stand(inc, by_class = "dq")
  ))
  ms_frame <- suppressWarnings(suppressMessages(
    increment_base_table_main_stand(inc$trees, by_class = "dq")
  ))
  expect_equal(ms_bundle$all_total, ms_frame$all_total)

  # output_increment_overall() takes the bundle and defaults fill_option from it
  oo_bundle <- suppressWarnings(suppressMessages(output_increment_overall(inc)))
  oo_frame  <- suppressWarnings(suppressMessages(
    output_increment_overall(inc$overview, fill_option = "standard")
  ))
  expect_equal(oo_bundle$table_combined, oo_frame$table_combined)
})


test_that("a stand-wise inventory is rejected before any work is done", {
  # Repeated-survey increment is a sample-inventory method: it needs
  # fe_ccircle_spatial plots with tree positions.
  wzp <- data_ex6_standwise_fe_inventory
  expect_error(
    suppressWarnings(suppressMessages(
      inv_increment_repeated_survey(wzp, wzp, trees_a, trees_b)
    ))
  )
})


test_that("progress is signalled through progressr for an external handler", {
  # progress_bar = FALSE is the Shiny case: FeNEU only signals, the caller's
  # handler (withProgressShiny() in the app) displays it. The signal count must
  # match the number of phases the progressor was set up for -- a mismatch
  # would mean the progressor over- or under-steps.
  n_signals <- 0L
  # Deliberately NOT run_bundle(): this test counts the signals of an actual
  # run, and a cached result would emit none.
  suppressWarnings(suppressMessages(
    withCallingHandlers(
      progressr::with_progress(
        inv_increment_repeated_survey(inv_a, inv_b, trees_a, trees_b,
                                      progress_bar = FALSE),
        handlers = progressr::handler_void(enable = TRUE)
      ),
      progression = function(c) n_signals <<- n_signals + 1L
    )
  ))
  expect_gt(n_signals, 0L)
})


test_that("progress_bar = TRUE displays a bar without a caller-side handler", {
  # The console bar is ours, not progressr's, and showing it costs a full chain
  # run. The signalling test above stays on CRAN because that one does depend on
  # progressr keeping its behaviour.
  skip_below_depth("full")

  # The default: an interactive user gets a bar with no ceremony. Must not
  # error, and must return the same result as the signalling-only mode.
  inc_bar <- suppressWarnings(suppressMessages(
    inv_increment_repeated_survey(inv_a, inv_b, trees_a, trees_b,
                                  progress_bar = TRUE)
  ))
  inc_off <- run_bundle(progress_bar = FALSE)

  expect_s3_class(inc_bar, "fe_increment_repsurv")
  expect_equal(nrow(inc_bar$trees), nrow(inc_off$trees))
  expect_equal(
    sum(inc_bar$trees$iv_hub_m3_ha_yr), sum(inc_off$trees$iv_hub_m3_ha_yr)
  )
})

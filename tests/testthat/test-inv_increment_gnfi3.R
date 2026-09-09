# Tests for inv_increment_gnfi3(), the bundled single-inventory BWI 3 increment,
# and for the fe_increment class the increment bundles share.
#
# Depth (helper-depth.R): only the structure test stays on CRAN. It is the one
# that reaches ForestElementsR's gnfi3 machinery, so it is the one that can
# break without us touching anything. Everything else here is arithmetic of
# ours about our own bundle -- sums, subsets, guards, messages.
#
# Tree preparation uses the canonical write-back chain, for the same reason as
# in test-inv_increment_repeated_survey.R.

inv_g   <- data_ex3_sample_fe_inventory
# The shipped list IS that canonical chain: data-raw/ builds it with exactly
# those steps, and test-inv_increment_repeated_survey.R anchors the identity by
# running the chain for real. Using it here avoids a second cluster start.
trees_g <- data_ex3_sample_trees_essentials

run_gnfi3 <- function(...) {
  suppressWarnings(suppressMessages(
    inv_increment_gnfi3(inv = inv_g, inv_trees = trees_g, ...)
  ))
}


test_that("inv_increment_gnfi3() returns the documented structure", {
  inc <- run_gnfi3()

  expect_s3_class(inc, "fe_increment_gnfi3")
  expect_s3_class(inc, "fe_increment")
  expect_true(is_fe_increment_gnfi3(inc))
  expect_named(
    inc, c("trees", "overview", "meta", "steps"), ignore.order = TRUE
  )
  expect_s3_class(inc$trees, "data.frame")

  # The meta describes the inventory and names the estimation source, so the
  # report titles can distinguish it from a repeated survey.
  expect_identical(inc$meta$source, "gnfi3")
  expect_identical(inc$meta$n_plots, nrow(inv_g))
  expect_equal(inc$meta$area_ha, sum(inv_g$area_rep_ha))

  # Steps are opt-in, as in the repeated-survey bundle
  expect_null(inc$steps)
  expect_false(is.null(run_gnfi3(keep_steps = TRUE)$steps))
})


test_that("the estimate covers all stand layers, not just the main stand", {
  skip_below_depth("full")
  # The point of estimating per tree over the whole population: any collective
  # can be aggregated from the result afterwards. A bundle restricted to the
  # main stand would silently make that impossible.
  inc <- run_gnfi3()
  expect_gt(dplyr::n_distinct(inc$trees$layer_key), 1)
  expect_true(all(inc$trees$layer_key %in% unique(trees_g$layer_key)))
})


test_that("the overview and the base table agree on the total increment", {
  skip_below_depth("full")
  # The overview is aggregated straight from the per-tree frame, the base table
  # by species group and class. They must meet, and the meeting point must not
  # depend on which class axis the base table uses.
  inc <- run_gnfi3()

  ovr <- inc$overview$overall$iv_total
  bt_age <- suppressWarnings(suppressMessages(
    increment_base_table(inc, by_class = "age")
  ))
  bt_dq <- suppressWarnings(suppressMessages(
    increment_base_table(inc, by_class = "dq")
  ))

  expect_equal(bt_age$all_total$iv_m3_yr_total, ovr$iv_m3_yr_total)
  expect_equal(bt_age$all_total$iv_m3_ha_yr,    ovr$iv_m3_ha_yr)
  expect_equal(bt_dq$all_total$iv_m3_yr_total,  ovr$iv_m3_yr_total)
})


test_that("the species groups of the overview sum to its overall value", {
  skip_below_depth("full")
  inc <- run_gnfi3()
  sg  <- inc$overview$by_species_group$iv_total
  ovr <- inc$overview$overall$iv_total

  expect_equal(sum(sg$iv_m3_yr_total), ovr$iv_m3_yr_total)
  expect_equal(sum(sg$iv_m3_ha_yr),    ovr$iv_m3_ha_yr)
})


test_that("a gnfi3 estimate carries no confidence interval", {
  skip_below_depth("full")
  # There is no measured increment to vary between plots, so the columns exist
  # for structural symmetry with the repeated survey but stay empty.
  inc <- run_gnfi3()
  expect_true(all(is.na(inc$overview$overall$iv_total$ci95_iv_m3_yr_total)))
  expect_true(all(is.na(
    inc$overview$by_species_group$iv_total$ci95_iv_m3_ha_yr)))
})


test_that("dt looks forward or backward, but never nowhere", {
  skip_below_depth("full")
  fwd <- run_gnfi3(dt = 5)
  bwd <- run_gnfi3(dt = -5)

  expect_gt(fwd$overview$overall$iv_total$iv_m3_ha_yr, 0)
  expect_gt(bwd$overview$overall$iv_total$iv_m3_ha_yr, 0)
  expect_false(isTRUE(all.equal(
    fwd$overview$overall$iv_total$iv_m3_ha_yr,
    bwd$overview$overall$iv_total$iv_m3_ha_yr
  )))

  expect_error(run_gnfi3(dt = 0))
})


test_that("incomplete ages stop the estimate instead of thinning it out", {
  # Ageing a tree by dt years needs its age. Estimating only those trees that
  # happen to carry one would return a plausible-looking figure for part of the
  # population, with nothing in the result to say so.
  trees_na <- trees_g
  trees_na$age_yr[1] <- NA_real_

  expect_error(
    suppressWarnings(suppressMessages(
      inv_increment_gnfi3(inv = inv_g, inv_trees = trees_na)
    ))
  )
})


test_that("the class tables accept the bundle and the bare frame alike", {
  skip_below_depth("full")
  inc <- run_gnfi3()

  from_bundle <- suppressWarnings(suppressMessages(
    increment_base_table(inc, by_class = "age")
  ))
  from_frame <- suppressWarnings(suppressMessages(
    increment_base_table(inc$trees, by_class = "age")
  ))
  expect_equal(from_bundle, from_frame)

  ms_bundle <- suppressWarnings(suppressMessages(
    increment_base_table_main_stand(inc, by_class = "age")
  ))
  ms_frame <- suppressWarnings(suppressMessages(
    increment_base_table_main_stand(inc$trees, by_class = "age")
  ))
  expect_equal(ms_bundle, ms_frame)
})


test_that("the main stand is a subset of the whole collective", {
  skip_below_depth("full")
  inc <- run_gnfi3()
  all_layers <- suppressWarnings(suppressMessages(
    increment_base_table(inc, by_class = "age")
  ))
  main_only <- suppressWarnings(suppressMessages(
    increment_base_table_main_stand(inc, by_class = "age")
  ))
  expect_lt(
    main_only$all_total$iv_m3_yr_total, all_layers$all_total$iv_m3_yr_total
  )
})


test_that("output_increment_overall() refuses a single-inventory bundle", {
  skip_below_depth("full")
  # That report contrasts matched plots with second-only ones, which exists in
  # a repeated survey alone. Refusing clearly beats failing on a missing slot.
  inc <- run_gnfi3()
  expect_error(output_increment_overall(inc), "single inventory|Einzelinventur")
})


test_that("the gnfi3 overview table runs off the bundle", {
  skip_below_depth("full")
  # The user-facing route to the species-group overview and its PDF.
  inc <- run_gnfi3()
  ovw <- suppressWarnings(suppressMessages(
    output_increment_overview_gnfi3(increment_base_table(inc, by_class = "age"))
  ))
  expect_s3_class(ovw, "data.frame")
  expect_true(any(ovw$is_total_row))

  total <- ovw$zv_m3_ha_yr[ovw$is_total_row]
  expect_equal(total, inc$overview$overall$iv_total$iv_m3_ha_yr)
})


test_that("the age message names the caller and shows the way out", {
  skip_below_depth("full")
  # Peter's case: ages recorded for the main stand only. The estimate is
  # refused -- it is computed for the whole population -- but the message has
  # to say what to do about it, and it must name the function the user called,
  # not the internal step that raises it.
  t_part <- trees_g
  t_part$age_yr[t_part$layer_key != 1] <- NA_real_

  msg <- tryCatch(
    suppressWarnings(suppressMessages(
      inv_increment_gnfi3(inv = inv_g, inv_trees = t_part))),
    error = function(e) conditionMessage(e)
  )

  expect_match(msg, "^inv_increment_gnfi3")
  expect_match(msg, "layer_key == 1", fixed = TRUE)

  # And that way out has to work, with the full inventory area as reference
  inc <- suppressWarnings(suppressMessages(inv_increment_gnfi3(
    inv = inv_g, inv_trees = dplyr::filter(t_part, .data$layer_key == 1)
  )))
  expect_s3_class(inc, "fe_increment_gnfi3")
  expect_equal(
    inc$overview$overall$iv_total$area_total_ha, sum(inv_g$area_rep_ha)
  )

  # It must give the same main-stand figure as the ordinary route does on data
  # whose ages are complete throughout
  full <- suppressWarnings(suppressMessages(
    inv_increment_gnfi3(inv = inv_g, inv_trees = trees_g)))
  pre <- suppressWarnings(suppressMessages(inv_increment_gnfi3(
    inv = inv_g, inv_trees = dplyr::filter(trees_g, .data$layer_key == 1)
  )))
  expect_equal(
    suppressWarnings(suppressMessages(
      increment_base_table_main_stand(pre, by_class = "age")))$all_total,
    suppressWarnings(suppressMessages(
      increment_base_table_main_stand(full, by_class = "age")))$all_total
  )
})

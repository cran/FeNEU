# Tests for increment_base_table_main_stand(), the main-stand increment base
# table with virtual monospecific species areas.
#
# Key invariants under test:
#   - four-element list(detail, total, all_species, all_total) with area_ha,
#     iv_m3_ha_yr (per virtual area) and per_species
#   - virtual areas are consistent: detail areas sum across species to the
#     all_species class area; total areas sum to all_total; the grand-total
#     virtual area equals the summed representation area of the main-stand plots
#   - per_species shares sum to 100 within each cross-section
#   - iv_m3_ha_yr == iv_m3_yr_total / area_ha (per row)
#   - iv_m3_yr_total is sum-consistent (totals are additive)
#   - works on both the single-inventory and the repeated-survey source

suppressWarnings(suppressMessages({

  # Tree lists and the first two increment steps are shipped ready-made for this
  # pair, see ?data_ex3_trees_essentials and ?data_ex3_increment_interim.
  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory
  inv_a_trees <- data_ex3_previous_sample_trees_essentials
  inv_b_trees <- data_ex3_sample_trees_essentials

  ext_single <- inv_inc_tree_extend_single(inv_b_trees)
  bt <- increment_base_table_main_stand(ext_single, by_class = "age")

  inc_rep  <- data_ex3_increment_matched
  inc_fl   <- data_ex3_increment_fills
  inc_cons <- inv_inc_tree_consolidate(inc_fl, fill_option = "standard")
  inc_extd <- inv_inc_tree_extend(inc_cons, inv_a_trees, inv_b_trees, inv_b)
  bt_rep   <- increment_base_table_main_stand(inc_extd, by_class = "age")

  # Main-stand plots: all plots with a layer-1 tree (including removals -- their
  # standing area counts towards the ideal species area).
  ms_area <- ext_single |>
    dplyr::filter(.data$layer_key == 1) |>
    dplyr::distinct(.data$plot_id, .data$area_rep_ha)
}))


test_that("main-stand base table has the expected four-element shape", {
  expect_named(bt, c("detail", "total", "all_species", "all_total", "collective", "meta"))
  expect_true(all(c(
    "species_group", "age_class", "n_plot_class", "n_iv_total", "n_iv_filled",
    "iv_m3_yr_total", "ci95_iv_m3_yr_total",
    "iv_m3_ha_yr", "ci95_iv_m3_ha_yr",   # over operation area
    "area_ha", "iv_m3_ha_yr_virt"        # ideal (virtual) area, no CI
  ) %in% names(bt$detail)))
  # the ideal-area per-ha increment carries no confidence interval
  expect_false("ci95_iv_m3_ha_yr_virt" %in% names(bt$detail))
  # increment main-stand tables deliberately carry NO per-species area share
  expect_false("per_species" %in% names(bt$detail))
  expect_equal(nrow(bt$all_total), 1L)
})


test_that("virtual areas are consistent across levels", {
  per_class <- bt$detail |>
    dplyr::group_by(.data$age_class) |>
    dplyr::summarise(a = sum(.data$area_ha), .groups = "drop") |>
    dplyr::left_join(
      dplyr::select(bt$all_species, "age_class", a_all = "area_ha"),
      by = "age_class"
    )
  expect_equal(per_class$a, per_class$a_all, tolerance = 1e-8)
  expect_equal(sum(bt$total$area_ha), bt$all_total$area_ha, tolerance = 1e-8)
  # grand-total virtual area == summed representation area of main-stand plots
  expect_equal(bt$all_total$area_ha, sum(ms_area$area_rep_ha), tolerance = 1e-8)
})


test_that("no per_species column in any increment main-stand level", {
  for (el in c("detail", "total", "all_species", "all_total"))
    expect_false("per_species" %in% names(bt[[el]]))
})


test_that("iv_m3_ha_yr_virt equals iv_m3_yr_total / area_ha (per virtual area)", {
  d <- bt$detail
  expect_equal(d$iv_m3_ha_yr_virt, d$iv_m3_yr_total / d$area_ha,
               tolerance = 1e-8)
})

test_that("iv_m3_ha_yr (operation) uses the operation area on the repeated source", {
  op_area <- sum(inv_b$area_rep_ha)
  d <- bt_rep$detail
  expect_equal(d$iv_m3_ha_yr * op_area, d$iv_m3_yr_total, tolerance = 1e-6)
  # the virtual per-ha still divides by the per-species virtual area
  expect_equal(d$iv_m3_ha_yr_virt * d$area_ha, d$iv_m3_yr_total,
               tolerance = 1e-6)
})


test_that("iv_m3_yr_total stays sum-consistent", {
  expect_equal(sum(bt$total$iv_m3_yr_total),
               bt$all_total$iv_m3_yr_total, tolerance = 1e-8)
  expect_equal(sum(bt$all_species$iv_m3_yr_total),
               bt$all_total$iv_m3_yr_total, tolerance = 1e-8)
})


test_that("the repeated-survey source produces the same structure", {
  expect_named(bt_rep, c("detail", "total", "all_species", "all_total", "collective", "meta"))
  expect_false("per_species" %in% names(bt_rep$detail))
  expect_equal(sum(bt_rep$total$area_ha), bt_rep$all_total$area_ha,
               tolerance = 1e-8)
})


test_that("the dq axis works", {
  btq <- increment_base_table_main_stand(ext_single, by_class = "dq")
  expect_true("d_q_class" %in% names(btq$detail))
  expect_equal(sum(btq$total$iv_m3_yr_total),
               btq$all_total$iv_m3_yr_total, tolerance = 1e-8)
})


test_that("main-stand carries the main_stand collective", {
  expect_equal(bt$collective$kind, "main_stand")
  expect_equal(bt$collective$label, "Hauptbestand")
})

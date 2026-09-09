# Tests for increment_ytables_base_table(): the main-stand yield-table increment
# base table (age + dq axes) that flows through the shared increment-base-table
# output/PDF pipeline.

suppressWarnings(suppressMessages({
  inv <- data_ex3_sample_fe_inventory
  trees <- inv |>
    pull_trees() |>
    height_complete_inventory(method = "NFI")
  trees$height_m <- ifelse(is.na(trees$height_m), trees$h_est_m, trees$height_m)
  trees <- trees |> trees_add_essentials(method = "BaySF")

  sel  <- ytables_bavrn_state_var_1_feneu
  bt_a <- increment_ytables_base_table(trees, sel, by_class = "age",
                                       fe_inv = inv)
  bt_d <- increment_ytables_base_table(trees, sel, by_class = "dq",
                                       fe_inv = inv)
  inc_yt <- inv_increment_ytables(inv, trees, sel)
  ov   <- list(overview = inc_yt$overview)
}))

eq <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-6))

test_that("shape: four data levels + collective + meta + ytables_used", {
  expect_named(bt_a, c("detail", "total", "all_species", "all_total",
                       "collective", "meta", "ytables_used"))
  expect_true(all(c("species_group", "ytable_name", "name_orig") %in%
                    names(bt_a$ytables_used)))
  expect_gt(nrow(bt_a$ytables_used), 0)
  expect_identical(bt_a$collective$kind, "main_stand")
  expect_identical(bt_a$meta$source, "ytable")
  expect_true(all(c("species_group", "age_class", "n_plot_class",
                    "iv_m3_yr_total", "iv_m3_ha_yr", "area_ha", "site_index",
                    "iv_m3_ha_yr_virt") %in% names(bt_a$detail)))
  expect_true("d_q_class" %in% names(bt_d$detail))
})

test_that("no confidence interval (columns present but all NA)", {
  for (el in c("detail", "total", "all_species", "all_total"))
    for (cc in c("ci95_iv_m3_yr_total", "ci95_iv_m3_ha_yr"))
      expect_true(all(is.na(bt_a[[el]][[cc]])))
})

test_that("sum-consistent within each axis", {
  expect_true(eq(sum(bt_a$total$iv_m3_yr_total), bt_a$all_total$iv_m3_yr_total))
  expect_true(eq(sum(bt_d$total$iv_m3_yr_total), bt_d$all_total$iv_m3_yr_total))
  expect_true(eq(sum(bt_a$all_species$area_ha),  bt_a$all_total$area_ha))
})

test_that("age detail rests on the cohorts of the bundle", {
  # The base table is an aggregation of inc_yt$cohorts, so the ideal areas and
  # the per-ideal-hectare increments must be reproducible from them.
  ref <- inc_yt$cohorts |>
    dplyr::group_by(species_group, age_class) |>
    dplyr::summarise(
      area_ha = sum(area_rep_ha_share),
      iv      = stats::weighted.mean(iv_hub_m3_ha_yr_yt, area_rep_ha_share),
      .groups = "drop"
    ) |>
    dplyr::arrange(species_group, age_class)

  det <- bt_a$detail |> dplyr::arrange(species_group, age_class)
  expect_true(eq(det$area_ha,          ref$area_ha))
  expect_true(eq(det$iv_m3_ha_yr_virt, ref$iv))
})

test_that("age totals match the yield-table overview", {
  tot <- bt_a$total |> dplyr::arrange(species_group)
  ovs <- ov$overview |>
    dplyr::filter(!is_total_row, !is_no_main_stand_row) |>
    dplyr::arrange(species_group)
  expect_true(eq(tot$iv_m3_yr_total, ovs$zv_m3_yr))
  expect_true(eq(bt_a$all_total$iv_m3_yr_total,
                 ov$overview$zv_m3_yr[ov$overview$is_total_row]))
})

test_that("operation-area per-ha equals total / operation area", {
  op <- sum(inv$area_rep_ha)
  expect_true(eq(bt_a$all_total$iv_m3_ha_yr,
                 bt_a$all_total$iv_m3_yr_total / op))
})

test_that("site index is per-species only (absent from the Summe block)", {
  # present in the per-species levels ...
  expect_true("site_index" %in% names(bt_a$detail))
  expect_true("site_index" %in% names(bt_a$total))
  # ... and dropped from the cross-species Summe block (averaging across
  # species groups is meaningless)
  expect_false("site_index" %in% names(bt_a$all_species))
  expect_false("site_index" %in% names(bt_a$all_total))
})

# --- consistency of the whole increment system: gnfi3 and yield-table
#     main-stand ideal areas must agree (both use the same living main-stand
#     collective, removals excluded). This guards against the two single-
#     inventory paths drifting apart in their tree collective. -------------
test_that("gnfi3 and yield-table main-stand ideal areas are equal", {
  suppressWarnings(suppressMessages({
    ext <- inv_inc_tree_extend_single(trees, dt = 5, inv = inv)
    g3_a <- increment_base_table_main_stand(ext, by_class = "age")
    g3_d <- increment_base_table_main_stand(ext, by_class = "dq")
  }))

  # grand-total ideal area, both axes
  expect_equal(g3_a$all_total$area_ha, bt_a$all_total$area_ha, tolerance = 1e-6)
  expect_equal(g3_d$all_total$area_ha, bt_d$all_total$area_ha, tolerance = 1e-6)

  # per-species ideal area (age axis), same species order
  g3_tot <- g3_a$total |> dplyr::arrange(species_group)
  yt_tot <- bt_a$total |> dplyr::arrange(species_group)
  expect_equal(g3_tot$species_group, yt_tot$species_group)
  expect_equal(g3_tot$area_ha, yt_tot$area_ha, tolerance = 1e-6)

  # Per (species, age class) the two need NOT agree, and that is not a drift of
  # the collective but a difference in what an age class means to each strand.
  # gnfi3 estimates per tree, so a tree carries its own age and its own class.
  # A yield-table increment is estimated for a cohort -- one species in the main
  # stand of one plot -- which has a single age, hence a single class. Where the
  # data give one cohort several ages, the gnfi3 table spreads it over several
  # classes while the yield-table one keeps it together. What must still hold is
  # that nothing is lost: the class rows sum to the same area per species.
  g3_by_sp <- g3_a$detail |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(area = sum(area_ha), .groups = "drop") |>
    dplyr::arrange(species_group)
  yt_by_sp <- bt_a$detail |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(area = sum(area_ha), .groups = "drop") |>
    dplyr::arrange(species_group)
  expect_equal(g3_by_sp$area, yt_by_sp$area, tolerance = 1e-6)
})


test_that("the class axis labels the estimate but does not change it", {
  # A yield-table increment belongs to a cohort (one species, one plot, main
  # stand), and a cohort has one age and one quadratic mean diameter. Which
  # class a table groups by is therefore a label on the result, not an input to
  # it -- so both axes must land on the same enterprise figure. Before the
  # cohort was formed per class, which split it wherever the data carried
  # several ages, and the two axes disagreed.
  expect_equal(bt_a$all_total$iv_m3_yr_total, bt_d$all_total$iv_m3_yr_total,
               tolerance = 1e-9)
  expect_equal(bt_a$all_total$iv_m3_ha_yr, bt_d$all_total$iv_m3_ha_yr,
               tolerance = 1e-9)
  expect_equal(bt_a$all_total$area_ha, bt_d$all_total$area_ha,
               tolerance = 1e-9)

  # and per species, too
  a <- bt_a$total |> dplyr::arrange(species_group)
  d <- bt_d$total |> dplyr::arrange(species_group)
  expect_equal(a$species_group, d$species_group)
  expect_equal(a$iv_m3_yr_total, d$iv_m3_yr_total, tolerance = 1e-9)
  expect_equal(a$site_index, d$site_index, tolerance = 1e-9)
})

test_that("uncovered species raise the informative coverage error", {
  # The old FER selection lacks some current bavrn_state codes present in the
  # sub10 data only if any -- use a deliberately empty-ish selection instead:
  # drop spruce (10) which is present in the data.
  sel_bad <- sel[unclass(sel$species_id) != "10", ]
  expect_error(
    increment_ytables_base_table(trees, sel_bad, by_class = "age"),
    "does not cover all species"
  )
})

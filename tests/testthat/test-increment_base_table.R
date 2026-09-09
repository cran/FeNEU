# Tests for increment_base_table(), the inventory-level increment base table
# (plain aggregation) built on inv_inc_tree_2_plot().
#
# Key invariants under test:
#   - four-element list(detail, total, all_species, all_total) like the static
#     base_table_* family
#   - sum-consistency on iv_m3_yr_total: species rows of detail sum to the
#     matching all_species row; total sums to all_total
#   - iv_m3_yr_total == iv_m3_ha_yr * area_ha (per row)
#   - full-area denominator: n_plot equals the number of plots for every group
#   - works identically on the single-inventory and the repeated-survey source
#   - both class axes ("age", "dq")

suppressWarnings(suppressMessages({

  # Tree lists and the first two increment steps are shipped ready-made for this
  # pair, see ?data_ex3_trees_essentials and ?data_ex3_increment_interim.
  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory
  inv_a_trees <- data_ex3_previous_sample_trees_essentials
  inv_b_trees <- data_ex3_sample_trees_essentials

  # Single-inventory source
  ext_single <- inv_inc_tree_extend_single(inv_b_trees)
  bt_single  <- increment_base_table(ext_single, by_class = "age")

  # Repeated-survey source (same contract -> same base table machinery)
  inc_rep  <- data_ex3_increment_matched
  inc_fl   <- data_ex3_increment_fills
  inc_cons <- inv_inc_tree_consolidate(inc_fl, fill_option = "standard")
  inc_extd <- inv_inc_tree_extend(inc_cons, inv_a_trees, inv_b_trees, inv_b)
  bt_rep   <- increment_base_table(inc_extd, by_class = "age")

  n_plots_single <- dplyr::n_distinct(ext_single$plot_id)
}))


# --- structure -------------------------------------------------------------

test_that("increment_base_table returns the four-element base-table list", {
  expect_named(bt_single, c("detail", "total", "all_species", "all_total", "collective", "meta"))
  for (el in bt_single[c("detail", "total", "all_species", "all_total")]) {
    expect_s3_class(el, "tbl_df")
  }
  expect_true(is.list(bt_single$collective))

  expect_true(all(c(
    "species_group", "age_class", "n_plot_class", "n_iv_total", "n_iv_filled",
    "iv_m3_yr_total", "ci95_iv_m3_yr_total", "iv_m3_ha_yr", "ci95_iv_m3_ha_yr"
  ) %in% names(bt_single$detail)))
  # the plain (all-layers) table carries no per-species virtual area
  expect_false("area_ha" %in% names(bt_single$detail))
  expect_equal(nrow(bt_single$all_total), 1L)
})


# --- sum-consistency on the (additive) totals ------------------------------

test_that("detail species rows sum to all_species per class", {
  per_class <- bt_single$detail |>
    dplyr::group_by(.data$age_class) |>
    dplyr::summarise(iv = sum(.data$iv_m3_yr_total), .groups = "drop") |>
    dplyr::left_join(
      dplyr::select(bt_single$all_species, "age_class",
                    iv_all = "iv_m3_yr_total"),
      by = "age_class"
    )
  expect_equal(per_class$iv, per_class$iv_all, tolerance = 1e-8)
})

test_that("total sums to all_total, and all_species sums to all_total", {
  expect_equal(sum(bt_single$total$iv_m3_yr_total),
               bt_single$all_total$iv_m3_yr_total, tolerance = 1e-8)
  expect_equal(sum(bt_single$all_species$iv_m3_yr_total),
               bt_single$all_total$iv_m3_yr_total, tolerance = 1e-8)
})


# --- internal arithmetic and denominator -----------------------------------

test_that("per-ha uses a single reference area (total / per-ha constant)", {
  # Without an fe_inventory the per-ha refers to the stocked collective area;
  # whatever it is, the same constant applies to every cell.
  d  <- bt_single$detail
  nz <- d$iv_m3_yr_total != 0 & !is.na(d$iv_m3_ha_yr) & d$iv_m3_ha_yr != 0
  ref <- d$iv_m3_yr_total[nz] / d$iv_m3_ha_yr[nz]
  expect_equal(length(unique(round(ref, 6))), 1L)
})

test_that("the repeated-inventory per-ha denominator is the operation area", {
  op_area <- sum(inv_b$area_rep_ha)
  d <- bt_rep$detail
  expect_equal(d$iv_m3_ha_yr * op_area, d$iv_m3_yr_total, tolerance = 1e-6)
  expect_equal(bt_rep$all_total$iv_m3_ha_yr * op_area,
               bt_rep$all_total$iv_m3_yr_total, tolerance = 1e-6)
})

test_that("n_plot_class is the per-class inventory point count", {
  expect_true(all(bt_single$detail$n_plot_class >= 1))
  expect_true(all(bt_single$detail$n_plot_class <= n_plots_single))
  expect_true(bt_single$all_total$n_plot_class <= n_plots_single)
})


# --- repeated-survey source gives the same shape ---------------------------

test_that("the repeated-survey source produces the same base-table structure", {
  expect_named(bt_rep, c("detail", "total", "all_species", "all_total", "collective", "meta"))
  expect_true(all(c(
    "species_group", "age_class", "iv_m3_yr_total", "iv_m3_ha_yr"
  ) %in% names(bt_rep$detail)))
  expect_equal(sum(bt_rep$total$iv_m3_yr_total),
               bt_rep$all_total$iv_m3_yr_total, tolerance = 1e-8)
})


# --- header meta carried through the repeated-inventory pipeline ------------

test_that("the repeated-survey base table carries the pipeline header meta", {
  m <- bt_rep$meta
  expect_type(m, "list")
  expect_named(m, c("year", "inv_year_prev", "period_yr", "area_ha",
                    "n_plots", "method", "fill_option"))
  expect_equal(m$n_plots, nrow(inv_b))
  expect_equal(m$area_ha, sum(inv_b$area_rep_ha), tolerance = 1e-8)
  expect_equal(m$period_yr, m$year - m$inv_year_prev)
  expect_equal(m$fill_option, "standard")
  expect_true(is.character(m$method) && nzchar(m$method))
})

test_that("the single-inventory source carries the gnfi3 source marker only", {
  # inv_inc_tree_extend_single() without an inventory attaches a minimal meta
  # that only marks the source as gnfi3 (so the base-table title reads
  # "Schaetzung nach BWI 3"); it carries no repeated-inventory fields
  # (year/area/n_plots/inv_year_prev/method), and the PDF header then falls
  # back to an explicit meta argument.
  expect_identical(bt_single$meta, list(source = "gnfi3"))
  expect_null(bt_single$meta$inv_year_prev)
  expect_null(bt_single$meta$area_ha)
})

test_that("output_increment_base_table promotes the meta to an attribute", {
  out <- output_increment_base_table(bt_rep)
  expect_equal(attr(out, "meta"), bt_rep$meta)
})

test_that("gnfi3 source carries no CI; repeated survey does", {
  # Only the repeated-inventory method carries an empirical CI. A gnfi3
  # single-inventory base table has its CI columns blanked (NA).
  ci_cols <- c("ci95_iv_m3_yr_total", "ci95_iv_m3_ha_yr")
  # gnfi3: all CI values NA across levels
  for (el in c("detail", "total", "all_species", "all_total"))
    for (cc in ci_cols)
      expect_true(all(is.na(bt_single[[el]][[cc]])))
  # repeated survey: the grand-total CI is a real (non-NA) number
  expect_false(is.na(bt_rep$all_total$ci95_iv_m3_yr_total))
})

test_that("gnfi3 main-stand source also carries no CI", {
  bt_ms <- increment_base_table_main_stand(ext_single, by_class = "age")
  for (el in c("detail", "total", "all_species", "all_total"))
    for (cc in c("ci95_iv_m3_yr_total", "ci95_iv_m3_ha_yr"))
      expect_true(all(is.na(bt_ms[[el]][[cc]])))
})


# --- removals are included by default --------------------------------------

test_that("the default collective includes trees removed between inventories", {
  suppressWarnings(suppressMessages({
    bt_standing <- increment_base_table(inc_extd, by_class = "age",
                                        tree_filter = !.data$removal)
  }))
  # sub10 carries removed main-stand trees, so including them (default) yields a
  # larger total increment than restricting to standing trees.
  expect_gt(bt_rep$all_total$iv_m3_yr_total,
            bt_standing$all_total$iv_m3_yr_total)
  # both remain the "all_layers" collective
  expect_equal(bt_rep$collective$kind, "all_layers")
  expect_equal(bt_standing$collective$kind, "all_layers")
})


# --- both class axes -------------------------------------------------------

test_that("the dq axis works and carries d_q_class", {
  btq <- increment_base_table(ext_single, by_class = "dq")
  expect_true("d_q_class" %in% names(btq$detail))
  expect_equal(sum(btq$total$iv_m3_yr_total),
               btq$all_total$iv_m3_yr_total, tolerance = 1e-8)
})


# --- collective descriptor -------------------------------------------------

test_that("default tree_filter yields the all-layers collective", {
  co <- bt_single$collective
  expect_equal(co$kind, "all_layers")
  expect_equal(co$label, "Alle Schichten")
})

test_that("a non-default tree_filter is reported as a custom collective", {
  suppressWarnings(suppressMessages({
    bt_c <- increment_base_table(ext_single, by_class = "age",
                                 tree_filter = layer_key == 1)
  }))
  expect_equal(bt_c$collective$kind, "custom")
  expect_match(bt_c$collective$label, "^Benutzerdefiniertes Kollektiv: ")
  expect_equal(bt_c$collective$definition, "layer_key == 1")
})

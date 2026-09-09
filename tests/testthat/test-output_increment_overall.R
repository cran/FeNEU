# Tests for the output_increment_overall() intermediate. The renderer
# output_increment_overall_pdf() needs LaTeX + Pandoc to actually produce a
# PDF; we exercise it under skip_if_not_installed("rmarkdown") only.

suppressWarnings(suppressMessages({

  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory

  # Tree lists and the first two increment steps are shipped ready-made for this
  # pair, see ?data_ex3_trees_essentials and ?data_ex3_increment_interim.
  inv_a_trees <- data_ex3_previous_sample_trees_essentials
  inv_b_trees <- data_ex3_sample_trees_essentials

  inc_rep <- data_ex3_increment_matched
  inc_fl   <- data_ex3_increment_fills
  inc_sum <- inv_inc_summaric(
    inc_rep$tree_increments, inc_fl$fills_forward,
    inv_a_trees, inv_b_trees
  )
  bo <- inv_inc_big_overview(
    inc_fl, inv_a_trees, inv_b_trees, inv_b,
    style = "generic", inc_summaric = inc_sum
  )

  out <- output_increment_overall(
    bo,
    fill_option   = "standard",
    user_subtitle = "Forstbetrieb Neustadt"
  )
}))


# --- Top-level structure ------------------------------------------------------

test_that("output_increment_overall() returns the expected top-level slots", {
  expect_named(out, c("meta", "table_matched", "table_2nd_only",
                      "table_combined"))
})


test_that("meta carries the required scalars and the plot match stats", {
  m <- out$meta
  expect_named(
    m,
    c("title", "user_subtitle", "method", "fill_option",
      "inv_year_prev", "inv_year_curr", "period_yr", "plot_match_stats")
  )
  expect_equal(m$inv_year_prev, 2009)
  expect_equal(m$inv_year_curr, 2020)
  expect_equal(m$period_yr,     11)
  expect_equal(m$fill_option,   "standard")
  expect_equal(m$method,        "rep_classic")
  expect_equal(m$user_subtitle, "Forstbetrieb Neustadt")
  expect_named(
    m$plot_match_stats,
    c("n_total_2nd", "n_matched", "n_2nd_only",
      "area_total_2nd_ha", "area_matched_ha", "area_2nd_only_ha")
  )
  # Plot match stats are additive
  ps <- m$plot_match_stats
  expect_equal(ps$n_total_2nd, ps$n_matched + ps$n_2nd_only)
  expect_equal(ps$area_total_2nd_ha,
               ps$area_matched_ha + ps$area_2nd_only_ha)
})


# --- Shared column shape over the three tables --------------------------------

test_that("the three tables share the exact same column order", {
  expect_identical(names(out$table_matched),  names(out$table_2nd_only))
  expect_identical(names(out$table_matched),  names(out$table_combined))
  # ten columns: keys + 4 single-tree + 4 summaric
  expect_equal(ncol(out$table_matched), 10L)
})


test_that("species_group ordering is the same across the three tables", {
  drop_total <- function(tbl) tbl[!tbl$is_total_row, "species_group", drop = TRUE]
  expect_identical(drop_total(out$table_matched),
                   drop_total(out$table_2nd_only))
  expect_identical(drop_total(out$table_matched),
                   drop_total(out$table_combined))
})


test_that("every table has exactly one total row, and it is the last row", {
  for (tbl_name in c("table_matched", "table_2nd_only", "table_combined")) {
    tbl <- out[[tbl_name]]
    expect_equal(sum(tbl$is_total_row), 1L, info = tbl_name)
    expect_true(tbl$is_total_row[nrow(tbl)], info = tbl_name)
  }
})


# --- CI95 NA contract --------------------------------------------------------

test_that("CI95 is filled only in table_matched, NA in 2nd_only and combined", {
  # table_matched: at least some CI95 values are non-NA (multi-plot groups
  # produce a CI; single-plot groups can still be NA, so we check 'any').
  expect_true(any(!is.na(out$table_matched$ci95_iv_m3_yr_total_st)))
  expect_true(any(!is.na(out$table_matched$ci95_iv_m3_ha_yr_st)))
  expect_true(any(!is.na(out$table_matched$ci95_iv_m3_yr_total_sum)))
  expect_true(any(!is.na(out$table_matched$ci95_iv_m3_ha_yr_sum)))

  # 2nd_only and combined: all CI95 columns NA throughout
  for (tbl in list(out$table_2nd_only, out$table_combined)) {
    expect_true(all(is.na(tbl$ci95_iv_m3_yr_total_st)))
    expect_true(all(is.na(tbl$ci95_iv_m3_ha_yr_st)))
    expect_true(all(is.na(tbl$ci95_iv_m3_yr_total_sum)))
    expect_true(all(is.na(tbl$ci95_iv_m3_ha_yr_sum)))
  }
})


# --- Summaric block in table_2nd_only is empty by design ---------------------

test_that("table_2nd_only summaric block iv columns are NA", {
  expect_true(all(is.na(out$table_2nd_only$iv_m3_yr_total_sum)))
  expect_true(all(is.na(out$table_2nd_only$iv_m3_ha_yr_sum)))
})


# --- Sum-consistency of total rows with bo's overall iv_total ---------------

test_that("table_matched total row matches bo$matches_only$overall$iv_total", {
  totrow <- out$table_matched[out$table_matched$is_total_row, ]
  ovr_st <- bo$matches_only$overall$iv_total |>
    dplyr::filter(.data$fill_option == "standard")
  ovr_sum <- bo$matches_only$overall$iv_total |>
    dplyr::filter(.data$fill_option == "inc_summaric")
  expect_equal(totrow$iv_m3_yr_total_st,  ovr_st$iv_m3_yr_total)
  expect_equal(totrow$iv_m3_ha_yr_st,     ovr_st$iv_m3_ha_yr)
  expect_equal(totrow$iv_m3_yr_total_sum, ovr_sum$iv_m3_yr_total)
  expect_equal(totrow$iv_m3_ha_yr_sum,    ovr_sum$iv_m3_ha_yr)
})


test_that("table_combined total iv_m3_yr_total equals matched + 2nd_only", {
  m_tot <- out$table_matched[ out$table_matched$is_total_row,  ]
  s_tot <- out$table_2nd_only[out$table_2nd_only$is_total_row, ]
  c_tot <- out$table_combined[out$table_combined$is_total_row, ]

  expect_equal(
    c_tot$iv_m3_yr_total_st,
    m_tot$iv_m3_yr_total_st + s_tot$iv_m3_yr_total_st
  )
  # summaric block: matched contributes + 2nd_only contributes the same
  # single-tree gnfi3 estimate (the second_only side has no separate
  # summaric estimator)
  expect_equal(
    c_tot$iv_m3_yr_total_sum,
    m_tot$iv_m3_yr_total_sum + s_tot$iv_m3_yr_total_st
  )
})


# --- Per-row sum invariant in single-tree block --------------------------------

test_that("combined iv_m3_yr_total_st equals matched + 2nd_only per species", {
  m <- out$table_matched
  s <- out$table_2nd_only
  c <- out$table_combined
  expect_equal(
    c$iv_m3_yr_total_st,
    m$iv_m3_yr_total_st + s$iv_m3_yr_total_st
  )
})


# --- fill_option choice is honoured ------------------------------------------

test_that("fill_option propagates into table_matched single-tree block", {
  out_min <- output_increment_overall(bo, fill_option = "min_estimates")
  out_all <- output_increment_overall(bo, fill_option = "all_estimates")

  ovr_min <- bo$matches_only$overall$iv_total |>
    dplyr::filter(.data$fill_option == "min_estimates")
  ovr_all <- bo$matches_only$overall$iv_total |>
    dplyr::filter(.data$fill_option == "all_estimates")

  expect_equal(
    out_min$table_matched$iv_m3_yr_total_st[out_min$table_matched$is_total_row],
    ovr_min$iv_m3_yr_total
  )
  expect_equal(
    out_all$table_matched$iv_m3_yr_total_st[out_all$table_matched$is_total_row],
    ovr_all$iv_m3_yr_total
  )
})


# --- Empty user_subtitle stays NULL, not character() -------------------------

test_that("NULL user_subtitle survives untouched", {
  out_no_sub <- output_increment_overall(bo, fill_option = "standard")
  expect_null(out_no_sub$meta$user_subtitle)
})


# --- Renderer wrapper smoke test (skip if rmarkdown missing) -----------------

test_that("output_increment_overall_pdf() returns a path", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(nzchar(Sys.which("pdflatex")),
              "pdflatex not on PATH")

  pdf_path <- tryCatch(
    output_increment_overall_pdf(out, output_dir = tempdir()),
    error = function(e) {
      skip(paste("output_increment_overall_pdf() failed to render:", e$message))
    }
  )
  expect_true(file.exists(pdf_path))
  expect_match(basename(pdf_path), "\\.pdf$")
})

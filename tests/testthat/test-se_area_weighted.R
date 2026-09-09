# Tests for se_area_weighted() and the grouped wrapper
# se_area_weighted_grouped(). The grouped wrapper is the shared primitive
# behind several aggregation pipelines (back tables, big_overview,
# increment summaries), so its correctness matters across the package.

# --- Vector primitive: regression-level correctness ---------------------------

test_that("se_area_weighted() matches a hand calculation with equal weights", {
  # With equal weights the ratio estimator collapses to the arithmetic mean.
  vals  <- c(10, 12, 8, 11)
  areas <- c( 1,  1, 1,  1)
  res <- se_area_weighted(vals, areas)

  expect_equal(res$mean_ha, mean(vals))
  expect_equal(res$total,   mean(vals) * sum(areas))
  expect_equal(res$n_plots, 4L)
})


test_that("se_area_weighted() returns NA columns for single-plot groups", {
  res <- se_area_weighted(plot_values = 7, area_weights = 1)
  expect_equal(res$n_plots, 1L)
  expect_equal(res$mean_ha, 7)
  expect_true(is.na(res$se_mean))
  expect_true(is.na(res$ci_pct))
})


# --- Grouped wrapper: equivalence to the vector primitive ---------------------

test_that("se_area_weighted_grouped() agrees with se_area_weighted() per group", {
  df <- tibble::tibble(
    group = rep(c("a", "b"), each = 4),
    value = c(10, 12,  8, 11,   20, 22, 18, 21),
    area  = c( 1,  1,  1,  1,    1,  1,  1,  1)
  )

  grouped <- df |>
    dplyr::group_by(group) |>
    se_area_weighted_grouped("value", "area")

  ref_a <- se_area_weighted(df$value[df$group == "a"], df$area[df$group == "a"])
  ref_b <- se_area_weighted(df$value[df$group == "b"], df$area[df$group == "b"])

  # Group keys preserved
  expect_equal(grouped$group, c("a", "b"))
  # Numeric columns match per-group reference
  expect_equal(grouped$mean_ha,      c(ref_a$mean_ha,      ref_b$mean_ha))
  expect_equal(grouped$total,        c(ref_a$total,        ref_b$total))
  expect_equal(grouped$margin_mean,  c(ref_a$margin_mean,  ref_b$margin_mean))
  expect_equal(grouped$margin_total, c(ref_a$margin_total, ref_b$margin_total))
})


test_that("se_area_weighted_grouped() respects area weighting (ratio estimator)", {
  # Two plots with different areas: the area-weighted mean must not equal
  # the simple arithmetic mean.
  df <- tibble::tibble(
    group = "g",
    value = c(10, 20),
    area  = c( 1,  3)   # second plot represents 3x more area
  )
  res <- df |>
    dplyr::group_by(group) |>
    se_area_weighted_grouped("value", "area")

  # Hand calculation: (1*10 + 3*20) / (1+3) = 70/4 = 17.5
  expect_equal(res$mean_ha, 17.5)
  expect_equal(res$total,   17.5 * 4)
})


test_that("se_area_weighted_grouped() handles single-plot groups gracefully", {
  df <- tibble::tibble(
    group = c("solo", "pair", "pair"),
    value = c(   7,    10,    12),
    area  = c(   1,     1,     1)
  )
  res <- df |>
    dplyr::group_by(group) |>
    se_area_weighted_grouped("value", "area")

  solo <- res[res$group == "solo", ]
  pair <- res[res$group == "pair", ]

  expect_equal(solo$n_plots, 1L)
  expect_true(is.na(solo$ci_pct))
  expect_equal(solo$mean_ha, 7)

  expect_equal(pair$n_plots, 2L)
  expect_equal(pair$mean_ha, 11)
})


test_that("se_area_weighted_grouped() returns ungrouped tibble", {
  res <- tibble::tibble(g = c("a", "b"), v = c(1, 2), a = c(1, 1)) |>
    dplyr::group_by(g) |>
    se_area_weighted_grouped("v", "a")
  expect_false(dplyr::is_grouped_df(res))
})


test_that("se_area_weighted_grouped() validates column names", {
  df <- tibble::tibble(group = "a", value = 1, area = 1)
  expect_error(
    df |> dplyr::group_by(group) |>
      se_area_weighted_grouped("nope", "area")
  )
  expect_error(
    df |> dplyr::group_by(group) |>
      se_area_weighted_grouped("value", "nope")
  )
})

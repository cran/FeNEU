

# Setup: use the sub10 sample data available in the package
suppressWarnings(suppressMessages({
  inv_a <- data_ex3_previous_sample_fe_inventory
  inv_b <- data_ex3_sample_fe_inventory

  plot_matches <- match_2_inventories(
    inv_a, inv_b, match_type = "plot_id", plot_id_style = "baysf"
  )
}))


# --- check_circle_def_compatibility ------------------------------------------

test_that("compatible circle definitions are kept in matches", {
  # The sub10 data should have matching circle definitions for all matched plots
  expect_true(nrow(plot_matches$matches) > 0)
})

test_that("circle_def_mismatch element is always present in output", {
  expect_true("circle_def_mismatch" %in% names(plot_matches))
  # Should be a data frame (or tibble) with the same column structure as matches
  expect_true(is.data.frame(plot_matches$circle_def_mismatch))
})

test_that("identical circle definitions pass compatibility check", {
  # For the sub10 data, all matched plots should be compatible
  # (none moved to circle_def_mismatch)
  expect_equal(nrow(plot_matches$circle_def_mismatch), 0)
})

test_that("notrees plots (empty circle_definition) are considered compatible", {
  # Create a modified inventory where one plot has an empty circle_definition
  inv_a_mod <- inv_a
  # Empty the circle_definition of the first matched plot
  idx <- match(plot_matches$matches$plot_id_inv_a[1], inv_a_mod$plot_id)
  inv_a_mod$plot[[idx]]$circle_definition <-
    inv_a_mod$plot[[idx]]$circle_definition[0, ]

  suppressWarnings(suppressMessages({
    pm_mod <- match_2_inventories(
      inv_a_mod, inv_b, match_type = "plot_id", plot_id_style = "baysf"
    )
  }))

  # The plot with the empty circle_definition should still be in matches
  expect_true(
    plot_matches$matches$plot_id_inv_a[1] %in% pm_mod$matches$plot_id_inv_a
  )
  expect_equal(nrow(pm_mod$circle_def_mismatch), 0)
})

test_that("different dbh_lower values cause incompatibility", {
  inv_a_mod <- inv_a
  idx <- match(plot_matches$matches$plot_id_inv_a[1], inv_a_mod$plot_id)
  # Alter a dbh_lower threshold so it no longer matches
  inv_a_mod$plot[[idx]]$circle_definition$dbh_lower[2] <- 15.0

  suppressWarnings(suppressMessages({
    pm_mod <- match_2_inventories(
      inv_a_mod, inv_b, match_type = "plot_id", plot_id_style = "baysf"
    )
  }))

  # That plot pair should now be in circle_def_mismatch
  expect_true(
    plot_matches$matches$plot_id_inv_a[1] %in%
      pm_mod$circle_def_mismatch$plot_id_inv_a
  )
  expect_false(
    plot_matches$matches$plot_id_inv_a[1] %in%
      pm_mod$matches$plot_id_inv_a
  )
})

test_that("different c_area values cause incompatibility", {
  inv_a_mod <- inv_a
  idx <- match(plot_matches$matches$plot_id_inv_a[1], inv_a_mod$plot_id)
  # Alter a c_area so it no longer matches (change by more than tolerance)
  inv_a_mod$plot[[idx]]$circle_definition$c_area[2] <- 0.01

  suppressWarnings(suppressMessages({
    pm_mod <- match_2_inventories(
      inv_a_mod, inv_b, match_type = "plot_id", plot_id_style = "baysf"
    )
  }))

  expect_true(
    plot_matches$matches$plot_id_inv_a[1] %in%
      pm_mod$circle_def_mismatch$plot_id_inv_a
  )
})

test_that("rounding differences within tolerance are accepted", {
  inv_a_mod <- inv_a
  idx <- match(plot_matches$matches$plot_id_inv_a[1], inv_a_mod$plot_id)
  # Add a tiny rounding artifact to dbh_lower (well within 0.1 cm tolerance)
  inv_a_mod$plot[[idx]]$circle_definition$dbh_lower[2] <-
    inv_a_mod$plot[[idx]]$circle_definition$dbh_lower[2] + 0.001
  # Add a tiny rounding artifact to c_area (well within 0.00001 ha tolerance)
  inv_a_mod$plot[[idx]]$circle_definition$c_area[2] <-
    inv_a_mod$plot[[idx]]$circle_definition$c_area[2] + 0.000001

  suppressWarnings(suppressMessages({
    pm_mod <- match_2_inventories(
      inv_a_mod, inv_b, match_type = "plot_id", plot_id_style = "baysf"
    )
  }))

  # Should still be compatible despite tiny differences
  expect_true(
    plot_matches$matches$plot_id_inv_a[1] %in% pm_mod$matches$plot_id_inv_a
  )
  expect_equal(nrow(pm_mod$circle_def_mismatch), 0)
})

test_that("match_plot_statistics reports circle_def_mismatch counts", {
  stats <- match_plot_statistics(plot_matches, inv_a, inv_b)
  expect_true("n_circle_def_mismatch" %in% names(stats$plot_numbers))
  expect_true("ha_circle_def_mismatch_1st" %in% names(stats$rep_areas_ha))
  expect_true("ha_circle_def_mismatch_2nd" %in% names(stats$rep_areas_ha))
})


# --- harmonize_inv_for_repsurv ------------------------------------------------

test_that("harmonize_inv_for_repsurv returns unchanged inventory when slopes match", {
  suppressWarnings(suppressMessages({
    inv_a_harmonized <- harmonize_inv_for_repsurv(inv_a, inv_b, plot_matches)
  }))

  # Check that n_rep_ha values are unchanged for all matched plots
  for (i in seq_len(nrow(plot_matches$matches))) {
    pid_a <- plot_matches$matches$plot_id_inv_a[i]
    idx <- match(pid_a, inv_a$plot_id)
    plot_orig <- inv_a$plot[[idx]]
    plot_harm <- inv_a_harmonized$plot[[idx]]
    if (nrow(plot_orig$trees) > 0) {
      expect_equal(plot_harm$trees$n_rep_ha, plot_orig$trees$n_rep_ha)
    }
  }
})

test_that("harmonize_inv_for_repsurv corrects differing slopes", {
  # Artificially change a slope in inv_a to create a difference
  inv_a_mod <- inv_a
  idx_a <- match(plot_matches$matches$plot_id_inv_a[1], inv_a_mod$plot_id)
  idx_b <- match(plot_matches$matches$plot_id_inv_b[1], inv_b$plot_id)

  slope_2nd <- unique(inv_b$plot[[idx_b]]$circle_definition$slope)
  if (length(slope_2nd) == 1 && !is.na(slope_2nd)) {
    new_slope <- slope_2nd + 10
  } else {
    new_slope <- 15
  }
  inv_a_mod$plot[[idx_a]]$circle_definition$slope <- new_slope

  suppressWarnings(suppressMessages({
    inv_a_harmonized <- harmonize_inv_for_repsurv(
      inv_a_mod, inv_b, plot_matches
    )
  }))

  # After harmonization, slope should match inv_b's slope
  slope_harmonized <- unique(
    inv_a_harmonized$plot[[idx_a]]$circle_definition$slope
  )
  expect_equal(slope_harmonized, unique(inv_b$plot[[idx_b]]$circle_definition$slope))
})

test_that("harmonize_inv_for_repsurv recalculates n_rep_ha after slope change", {
  inv_a_mod <- inv_a
  idx_a <- match(plot_matches$matches$plot_id_inv_a[1], inv_a_mod$plot_id)

  skip_if(nrow(inv_a_mod$plot[[idx_a]]$trees) == 0, "No trees in first matched plot")

  # Set a clearly different slope and recalculate n_rep_ha manually to get
  # the "wrong" values that would result from slope = 30
  inv_a_mod$plot[[idx_a]]$circle_definition$slope <- 30
  inv_a_mod$plot[[idx_a]]$trees$n_rep_ha <-
    ForestElementsR::n_rep_ha(inv_a_mod$plot[[idx_a]])
  wrong_n_rep <- inv_a_mod$plot[[idx_a]]$trees$n_rep_ha

  suppressWarnings(suppressMessages({
    inv_a_harmonized <- harmonize_inv_for_repsurv(
      inv_a_mod, inv_b, plot_matches
    )
  }))

  # After harmonization (slope corrected to inv_b's value), n_rep_ha should
  # differ from the "wrong" slope=30 values
  expect_false(
    all(inv_a_harmonized$plot[[idx_a]]$trees$n_rep_ha == wrong_n_rep)
  )
})

test_that("harmonize_inv_for_repsurv handles empty matches gracefully", {
  empty_matches <- plot_matches
  empty_matches$matches <- plot_matches$matches[0, ]

  suppressWarnings(suppressMessages({
    result <- harmonize_inv_for_repsurv(inv_a, inv_b, empty_matches)
  }))

  # Should return inv_a unchanged
  expect_identical(result, inv_a)
})

test_that("harmonize_inv_for_repsurv skips notrees plots", {
  inv_a_mod <- inv_a
  idx_a <- match(plot_matches$matches$plot_id_inv_a[1], inv_a_mod$plot_id)

  # Empty the circle_definition to simulate a notrees plot
  inv_a_mod$plot[[idx_a]]$circle_definition <-
    inv_a_mod$plot[[idx_a]]$circle_definition[0, ]

  # This should not error
  suppressWarnings(suppressMessages({
    result <- harmonize_inv_for_repsurv(inv_a_mod, inv_b, plot_matches)
  }))

  # The emptied circle_definition should still be empty (not modified)
  expect_equal(nrow(result$plot[[idx_a]]$circle_definition), 0)
})

test_that("harmonize_inv_for_repsurv skips when no slope column exists", {
  inv_a_mod <- inv_a
  inv_b_mod <- inv_b

  # Remove slope column from all plots in both inventories
  for (i in seq_along(inv_a_mod$plot)) {
    if ("slope" %in% names(inv_a_mod$plot[[i]]$circle_definition)) {
      inv_a_mod$plot[[i]]$circle_definition$slope <- NULL
    }
  }
  for (i in seq_along(inv_b_mod$plot)) {
    if ("slope" %in% names(inv_b_mod$plot[[i]]$circle_definition)) {
      inv_b_mod$plot[[i]]$circle_definition$slope <- NULL
    }
  }

  # Should not error and should return inventory unchanged
  suppressWarnings(suppressMessages({
    pm <- match_2_inventories(
      inv_a_mod, inv_b_mod, match_type = "plot_id", plot_id_style = "baysf"
    )
    result <- harmonize_inv_for_repsurv(inv_a_mod, inv_b_mod, pm)
  }))

  # n_rep_ha should be unchanged for all plots
  for (i in seq_along(inv_a_mod$plot)) {
    if (nrow(inv_a_mod$plot[[i]]$trees) > 0) {
      expect_equal(
        result$plot[[i]]$trees$n_rep_ha,
        inv_a_mod$plot[[i]]$trees$n_rep_ha
      )
    }
  }
})

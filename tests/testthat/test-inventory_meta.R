test_that("inventory_meta() derives year / area_ha / n_plots from an fe_inventory", {
  m <- inventory_meta(data_ex3_sample_fe_inventory)

  expect_named(m, c("year", "area_ha", "n_plots"))
  expect_identical(m$n_plots, nrow(data_ex3_sample_fe_inventory))
  expect_equal(m$area_ha, sum(data_ex3_sample_fe_inventory$area_rep_ha))
  expect_false(is.na(m$year))
  expect_true(is.numeric(m$year) && m$year > 1900)

  # works on the standwise/relascope (fe_stand) example too, not just concentric
  m2 <- inventory_meta(data_ex6_standwise_fe_inventory)
  expect_identical(m2$n_plots, nrow(data_ex6_standwise_fe_inventory))
  expect_false(is.na(m2$year))

  # non-fe_inventory input is rejected
  expect_error(inventory_meta(iris))
})

test_that("inventory_meta() warns and takes the most recent year for mixed years", {
  fe <- data_ex3_sample_fe_inventory
  # tamper one plot's survey year so the plots span two years
  fe$plot[[1]]$time_yr <- fe$plot[[1]]$time_yr + 1L
  expect_warning(m <- inventory_meta(fe), "year")
  expect_equal(m$year, max(vapply(fe$plot, function(p) as.numeric(p[["time_yr"]])[1],
                                  numeric(1))))
})

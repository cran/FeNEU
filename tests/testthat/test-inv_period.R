# Tests for the get_inv_year() S3 generic and the inv_period() helper.

test_that("get_inv_year() returns the uniform survey year of an fe_inventory", {
  expect_equal(get_inv_year(data_ex3_previous_sample_fe_inventory), 2009)
  expect_equal(get_inv_year(data_ex3_sample_fe_inventory), 2020)
})


test_that("get_inv_year() rejects fe_inventory with non-uniform time_yr", {
  bad_inv <- data_ex3_sample_fe_inventory
  # Tamper with a single plot's time_yr to break uniformity
  bad_inv$plot[[1]]$time_yr <- bad_inv$plot[[1]]$time_yr + 1L
  expect_error(get_inv_year(bad_inv), "non-uniform time_yr")
})


test_that("inv_period() returns the two years and the period length", {
  ip <- inv_period(
    data_ex3_previous_sample_fe_inventory,
    data_ex3_sample_fe_inventory
  )
  expect_named(ip, c("time_yr_1st", "time_yr_2nd", "period_yr"))
  expect_equal(ip$time_yr_1st, 2009)
  expect_equal(ip$time_yr_2nd, 2020)
  expect_equal(ip$period_yr, 11)
})


test_that("inv_period() validates chronological order and minimum interval", {
  # Inverted order
  expect_error(
    inv_period(
      data_ex3_sample_fe_inventory,
      data_ex3_previous_sample_fe_inventory
    ),
    "cannot be later"
  )

  # Same inventory twice
  expect_error(
    inv_period(
      data_ex3_sample_fe_inventory,
      data_ex3_sample_fe_inventory
    ),
    "same year"
  )
})

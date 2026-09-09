# Regression guard for the win-builder failure of 2026-07-24: fill_heights_back()
# requested detectCores() - free_cores = 92 cores on a 96-core CRAN check
# machine, and parallel:::.check_ncores() aborted the examples, tests, and the
# main vignette. The cause was a case-sensitive read of _R_CHECK_LIMIT_CORES_
# ("TRUE" only) that missed win-builder's lower-case value.
#
# The core count is therefore checked through .fe_worker_cores() with a plot
# count large enough to ask for a cluster at all -- an inventory the size of the
# example data runs in this process, and would never reach the limit.

# Set _R_CHECK_LIMIT_CORES_ for one expression and restore it afterwards. A tiny
# base-R stand-in for withr::local_envvar, so that the tests need no extra
# dependency; on.exit works here because this is a real function frame.
with_limit_cores <- function(value, code) {
  old <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = value)
  on.exit(
    if (is.na(old)) Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
    else Sys.setenv("_R_CHECK_LIMIT_CORES_" = old),
    add = TRUE
  )
  force(code)
}

# An inventory big enough for the parallel branch, without building one.
many <- .fe_parallel_min_plots * 10L

test_that(".fe_worker_cores never exceeds the CRAN core limit", {
  # R's own .check_ncores() aborts above two cores whenever the variable is set
  # to anything but "false", regardless of the truthy spelling.
  for (val in c("TRUE", "true", "True", "warn", "error", "1")) {
    with_limit_cores(
      val, expect_lte(.fe_worker_cores(free_cores = 0, n_tasks = many), 2L)
    )
  }
  # Disabled or unset: the free_cores logic applies, never below one core.
  for (val in c("false", "")) {
    with_limit_cores(
      val, expect_gte(.fe_worker_cores(free_cores = 0, n_tasks = many), 1L)
    )
  }
})

test_that(".fe_worker_cores caps the count outside a check", {
  with_limit_cores("false", {
    # absolute cap of four, whatever the core count of the test machine
    expect_lte(.fe_worker_cores(free_cores = 0, n_tasks = many), 4L)
    # never more workers than there are tasks
    expect_equal(.fe_worker_cores(free_cores = 0, n_tasks = 1L), 1L)
  })
})

test_that("a small inventory is not sent through a cluster at all", {
  # Every worker receives a serialised copy of the whole inventory, which costs
  # far more than the work itself unless there are many plots. Below the
  # threshold the answer is one worker, and fill_heights_back() then runs
  # sequentially instead of starting a cluster of one.
  with_limit_cores("false", {
    expect_equal(
      .fe_worker_cores(free_cores = 0, n_tasks = .fe_parallel_min_plots - 1L), 1L
    )
    expect_gt(.fe_worker_cores(free_cores = 0, n_tasks = many), 1L)
  })
})

test_that("fill_heights_back runs under a CRAN-style core limit", {
  # The example inventory stays in this process, so this exercises the whole
  # function rather than the cluster branch; the core arithmetic above covers
  # the many-plot case.
  est <- suppressWarnings(suppressMessages(
    pull_trees(data_ex3_sample_fe_inventory) |> height_complete_inventory()
  ))
  res <- with_limit_cores("true", suppressWarnings(suppressMessages(
    fill_heights_back(data_ex3_sample_fe_inventory, est, free_cores = 0)
  )))
  expect_true(is_fe_inventory(res))
})

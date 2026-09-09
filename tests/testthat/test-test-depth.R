# The depth switch itself. It decides what CRAN sees, so a wrong answer here
# would silently change the shape of the check rather than fail visibly.

with_depth <- function(value, code) {
  old <- Sys.getenv("FENEU_TEST_DEPTH", unset = NA)
  if (is.na(old)) on.exit(Sys.unsetenv("FENEU_TEST_DEPTH"), add = TRUE)
  else on.exit(Sys.setenv(FENEU_TEST_DEPTH = old), add = TRUE)
  if (is.na(value)) Sys.unsetenv("FENEU_TEST_DEPTH")
  else Sys.setenv(FENEU_TEST_DEPTH = value)
  force(code)
}

test_that("an explicit depth wins over the CRAN guess, in any spelling", {
  with_depth("smoke", expect_equal(fe_test_depth(), "smoke"))
  with_depth("CRAN",  expect_equal(fe_test_depth(), "cran"))
  with_depth("Full",  expect_equal(fe_test_depth(), "full"))
})

test_that("an unknown depth is refused rather than guessed", {
  with_depth("deep", expect_error(fe_test_depth(), "FENEU_TEST_DEPTH"))
  with_depth("",     expect_equal(fe_test_depth(), if (on_cran()) "cran" else "full"))
})

test_that("skip_below_depth passes and skips at the right levels", {
  with_depth("full", {
    expect_true(skip_below_depth("cran"))
    expect_true(skip_below_depth("full"))
  })
  with_depth("smoke", {
    expect_true(skip_below_depth("smoke"))
    expect_condition(skip_below_depth("cran"), class = "skip")
    expect_condition(skip_below_depth("full"), class = "skip")
  })
  expect_error(skip_below_depth("deep"))
})

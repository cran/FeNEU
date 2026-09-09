# How deep does this run go?
#
# The prose version of all this -- how to run each depth, how to decide where a
# test belongs, and the time budget -- is in CONTRIBUTING.md at the repo root.
#
# The suite serves two audiences with different needs. CRAN's check farm is a
# shared resource, and the policy asks that long-running tests "can be made
# optional for checking, but do ensure that the checks that are left do
# exercise all the features of the package" -- breadth, not depth. Our own runs
# want everything.
#
# The criterion for where a test belongs is a single question:
#
#     Can this test break through something that does NOT happen in our repo?
#
# If yes -- a new R version, an update of ForestElementsR, dplyr, sf, readr or
# progressr, a different platform, another encoding -- it belongs on CRAN. That
# is what CRAN's checks are worth to us: an early warning for changes we do not
# see coming. If no -- a regression test against a fault of ours, attribute
# plumbing, file-name building, an option matrix in which every cell exercises
# the same mechanism -- it runs for us, where we are sitting at the machine
# anyway. A test that is the only cover for a feature stays on CRAN regardless,
# because of the policy sentence above.
#
# The level is read from an ENVIRONMENT VARIABLE, not from an R option:
# R CMD check starts the tests in a process of its own, where options of the
# calling session never arrive, while environment variables are inherited.
# That is why testthat's own NOT_CRAN works the same way.
#
#   smoke  every module once           -- a quick pass while working
#   cran   + everything that hangs on foreign code
#   full   + variants, regressions, renders   (the default for us)
#
# The three levels exist; only two of them are populated today. Nothing has
# been classified below "cran" yet, so a "smoke" run currently sees exactly
# what CRAN sees. The level is there for the modules still to come -- deadwood,
# regeneration, the stand register -- each of which gets one test at "smoke"
# saying the module runs at all, with its variants at "full".
#
# Unset, the level follows on_cran() as before, so nothing changes for anyone
# who does not set it. FENEU_TEST_DEPTH=cran reproduces the CRAN view locally,
# which previously needed the NOT_CRAN=false trick.
fe_test_depth <- function() {
  known <- c("smoke", "cran", "full")
  d <- tolower(Sys.getenv("FENEU_TEST_DEPTH", ""))
  if (nzchar(d)) {
    if (!d %in% known) {
      stop("FENEU_TEST_DEPTH must be one of: ", paste(known, collapse = ", "),
           call. = FALSE)
    }
    return(d)
  }
  if (on_cran()) "cran" else "full"
}

# Skip unless the run is at least as deep as `level`.
#
# skip_below_depth("full") is what skip_on_cran() used to say; both may appear
# in the suite, they mean the same thing.
skip_below_depth <- function(level) {
  rank <- c(smoke = 1L, cran = 2L, full = 3L)
  stopifnot(level %in% names(rank))
  if (rank[[fe_test_depth()]] < rank[[level]]) {
    testthat::skip(paste0("test depth '", fe_test_depth(), "' is below '",
                          level, "'"))
  }
  invisible(TRUE)
}

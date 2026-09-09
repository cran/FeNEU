# Is this run happening on CRAN's machines?
#
# Some tests walk a whole matrix of options where one of them exercises the
# mechanism just as well. Those run in full for us and in short on CRAN, whose
# check farm is a shared resource -- the CRAN policy asks for exactly that:
# "Long-running tests [...] can be made optional for checking, but do ensure
# that the checks that are left do exercise all the features of the package."
#
# testthat carries the same predicate internally for skip_on_cran(), but it is
# not exported and reaching in with ::: would draw a check note. The rule is
# theirs: NOT_CRAN is set to "true" by devtools::test() and by testthat when
# run locally, and is unset on CRAN.
on_cran <- function() {
  !identical(tolower(Sys.getenv("NOT_CRAN")), "true")
}

# Tests for check_and_guess_species(): the up-front case canonicalisation must
# resolve upper/lower-case variants of canonical abbreviations silently (no
# "not in standard table" warning) while still mapping them to the right code.

test_that("case variants of canonical abbreviations resolve without a warning", {
  st <- species_abbreviations_bavrn_state
  tl <- tibble::tibble(baumart = c("FI", "kie", "Bu", "bu", "KIe"))

  res <- expect_no_warning(check_and_guess_species(tl, st))

  # canonical spellings are Fi (10), Kie (20), Bu (60)
  expect_setequal(res$species_id, c("10", "20", "60"))
  expect_identical(res$species_id, c("10", "20", "60", "60", "20"))
})

test_that("already-canonical abbreviations pass through unchanged and silently", {
  st <- species_abbreviations_bavrn_state
  tl <- tibble::tibble(baumart = c("Fi", "Kie", "Bu"))

  res <- expect_no_warning(check_and_guess_species(tl, st))
  expect_identical(res$species_id, c("10", "20", "60"))
})

test_that("species_mapping attribute records confirmed and review tiers", {
  st <- species_abbreviations_bavrn_state
  # FI / Fi -> confirmed (canonical Fi); Bi -> resolved only via fuzzy -> review
  tl <- tibble::tibble(baumart = c("FI", "Fi", "Bi"))

  res <- suppressWarnings(suppressMessages(check_and_guess_species(tl, st)))
  m   <- attr(res, "species_mapping")

  expect_s3_class(m, "tbl_df")
  expect_setequal(names(m),
                  c("raw", "n_trees", "canonical", "species_id", "name_ger", "tier"))
  expect_true(all(c("confirmed", "review") %in% m$tier))

  bi <- m[m$raw == "Bi", ]
  expect_identical(bi$tier, "review")
  expect_identical(bi$canonical, "Bir")

  # review rows are ordered first
  expect_identical(m$tier[1], "review")
})

test_that("species_mapping is all-confirmed when everything matches exactly", {
  st <- species_abbreviations_bavrn_state
  tl <- tibble::tibble(baumart = c("Fi", "kie", "BU"))

  res <- expect_no_warning(check_and_guess_species(tl, st))
  m   <- attr(res, "species_mapping")
  expect_true(all(m$tier == "confirmed"))
  expect_setequal(m$canonical, c("Fi", "Kie", "Bu"))
})

test_that("alias abbreviations resolve as confirmed matches, not fuzzy", {
  st <- species_abbreviations_bavrn_state
  # These used to fall to the fuzzy tier (and "Aspe" was mis-mapped to
  # Speierling); alias rows in the standard table now make them exact.
  tl <- tibble::tibble(baumart = c("Aspe", "As", "SAh", "Sah", "SAH"))

  res <- expect_no_warning(check_and_guess_species(tl, st))
  m   <- attr(res, "species_mapping")

  expect_true(all(m$tier == "confirmed"))
  # Aspe (82) for the Aspe aliases, Spitzahorn (65) for the SAh family
  expect_identical(m$species_id[m$raw == "Aspe"], "82")
  expect_identical(m$species_id[m$raw == "As"],   "82")
  expect_true(all(m$species_id[m$raw %in% c("SAh", "Sah", "SAH")] == "65"))
})

test_that("Grauerle and Gruenerle have distinct abbreviations", {
  st <- species_abbreviations_bavrn_state
  # Previously both carried "GrErl", so Gruenerle (85) had no usable code and
  # would be misread as Grauerle (81). They must now be distinct.
  expect_identical(sum(st$abbreviation == "GrErl"), 1L)
  expect_identical(st$species_id[st$abbreviation == "GrErl"], "81")
  expect_identical(st$species_id[st$abbreviation == "GnErl"], "85")
  expect_false(any(duplicated(toupper(st$abbreviation))))

  res <- expect_no_warning(
    check_and_guess_species(tibble::tibble(baumart = c("GrErl", "GnErl")), st))
  m <- attr(res, "species_mapping")
  expect_true(all(m$tier == "confirmed"))
  expect_identical(m$species_id[m$raw == "GrErl"], "81")
  expect_identical(m$species_id[m$raw == "GnErl"], "85")
})

test_that("a genuinely unknown code still surfaces in the warning", {
  st <- species_abbreviations_bavrn_state
  # 'Fi' is a clean case variant; 'Zzz9' is neither a case variant nor close
  # enough for the fuzzy tiers, so it must remain reported.
  tl <- tibble::tibble(baumart = c("fi", "Zzz9"))

  expect_warning(
    try(check_and_guess_species(tl, st), silent = TRUE),
    "Zzz9"
  )
})

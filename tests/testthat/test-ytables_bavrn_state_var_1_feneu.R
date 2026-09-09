# Tests for the FeNEU-shipped complete Bavarian-State-Forest yield-table
# selection and the informative "species not covered" error message.

test_that("ytables_bavrn_state_var_1_feneu has the expected shape", {
  sel <- ytables_bavrn_state_var_1_feneu
  expect_true(all(c("species_id", "ytable_name") %in% names(sel)))
  expect_identical(
    ForestElementsR::fe_species_get_coding(sel$species_id), "bavrn_state"
  )
  expect_false(any(duplicated(unclass(sel$species_id))))
  expect_type(sel$ytable_name, "character")
})

test_that("it covers all current bavrn_state tree species codes", {
  sc <- ForestElementsR::species_codings
  bv <- sc$code_table[[which(sc$species_coding == "bavrn_state")]]
  all_codes  <- sort(unique(bv$species_id))
  sp_all     <- ForestElementsR::as_fe_species_bavrn_state(all_codes)
  tree_codes <- all_codes[ForestElementsR::fe_species_is_tree(sp_all)]

  covered <- unclass(ytables_bavrn_state_var_1_feneu$species_id)
  expect_length(setdiff(tree_codes, covered), 0)
})

test_that("it is a superset of the ForestElementsR selection", {
  fer <- unclass(ForestElementsR::ytables_bavrn_state_var_1$species_id)
  feneu <- unclass(ytables_bavrn_state_var_1_feneu$species_id)
  expect_length(setdiff(fer, feneu), 0)
})

test_that("every referenced yield table is accessible in ForestElementsR", {
  # Use the same access mechanism as apply_ytables() (ForestElementsR::<name>),
  # which resolves exported objects and lazy-loaded package data alike.
  nms <- unique(ytables_bavrn_state_var_1_feneu$ytable_name)
  ok <- vapply(nms, function(nm) {
    !is.null(tryCatch(
      eval(parse(text = paste0("ForestElementsR::", nm))),
      error = function(e) NULL
    ))
  }, logical(1))
  expect_true(all(ok))
})

test_that("the confirmed surrogate assignments are as agreed", {
  sel <- ytables_bavrn_state_var_1_feneu
  tab <- stats::setNames(sel$ytable_name, as.character(unclass(sel$species_id)))
  # cedars -> larch (BaySF larch group)
  expect_equal(unname(tab["43"]), "fe_ytable_larch_schober_moderate_1946")
  expect_equal(unname(tab["44"]), "fe_ytable_larch_schober_moderate_1946")
  # Baumhasel -> birch (minor broadleaf)
  expect_equal(unname(tab["96"]), "fe_ytable_birch_schwappach_1903_29")
  # sessile oak -> oak
  expect_equal(unname(tab["55"]), "fe_ytable_oak_juettner_moderate_1955")
})

test_that("check_ytable_selection_vs_data names uncovered species", {
  # Build a tiny inventory-like frame carrying a species the selection lacks.
  # 55 (Traubeneiche) is not in the FER selection.
  inv <- tibble::tibble(
    species_id = ForestElementsR::as_fe_species_bavrn_state(
      c("10", "10", "55")
    )
  )
  res <- check_ytable_selection_vs_data(
    inv, ForestElementsR::ytables_bavrn_state_var_1
  )
  expect_false(res$success)
  expect_match(res$message, "does not cover all species")
  expect_match(res$message, "55")
  expect_match(res$message, "1 trees")

  # The FeNEU selection covers it -> success.
  res_ok <- check_ytable_selection_vs_data(inv, ytables_bavrn_state_var_1_feneu)
  expect_true(res_ok$success)
})

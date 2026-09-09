# Non-tree species codes (e.g. BaySF / bavrn_state code 99 "Strauch") must not
# reach the fe_stand children that fe_inventory() builds. ForestElementsR's
# object constructors reject non-tree codes, so FeNEU has to drop such records
# in make_object_lists.R *before* building the object, and re-check whether the
# plot becomes treeless. These tests assert that desired behaviour by deriving a
# fixture from the real example data and injecting a shrub record.
#
# NOTE (TDD): until the non-tree filter is implemented in make_object_lists.R,
# both tests are expected to FAIL with the constructor's
# ".assert_tree_species()" abort - that failure is the documented finding.

# Build the intermediate inventory list (make_inventory_list output) from the
# small packaged BaySF example.
base_inv_list <- function() {
  path <- system.file("extdata/data_ex3_sample_pre", package = "FeNEU")
  skip_if(identical(path, ""), "data_ex3_sample_pre not available")
  suppressWarnings(suppressMessages(
    import_data(path, standard = "BaySF") |>
      make_inventory_list(standard = "BaySF", coord_sys = "gk4")
  ))
}

# Turn an existing tree row into a shrub (non-tree bavrn_state code 99). A small
# dbh keeps it on the inner circle, so it carries no coordinate requirement.
make_shrub_row <- function(tr) {
  shrub          <- tr[1, , drop = FALSE]
  shrub$ba_fe    <- ForestElementsR::fe_species_bavrn_state("99")
  shrub$tree_id  <- "99_1"
  shrub$bhd      <- 5
  shrub$bhd_h    <- NA_real_
  shrub$mortal   <- 0
  shrub$bhd_anz  <- 1
  shrub$layer_key <- 1
  shrub
}


test_that("a shrub (code 99) next to real trees is dropped and the object still builds", {
  il  <- base_inv_list()
  pid <- il$plot_id[[1]]
  il$trees[[1]] <- dplyr::bind_rows(il$trees[[1]], make_shrub_row(il$trees[[1]]))

  inv <- suppressWarnings(suppressMessages(
    fe_inventory(il, object_type = "fe_ccircle_spatial")
  ))

  idx <- which(inv$plot_id == pid)
  expect_length(idx, 1L)

  obj <- inv$plot[[idx]]
  # The plot keeps real trees, so it is a regular fe_ccircle_spatial ...
  expect_true(ForestElementsR::is_fe_ccircle_spatial(obj))
  # ... and the shrub is gone: the injected record was dropped and no non-tree
  # code survived into the object.
  expect_false("99_1" %in% obj$trees$tree_id)
  expect_true(all(ForestElementsR::fe_species_is_tree(obj$trees$species_id)))
})


test_that("a plot containing only a shrub (code 99) becomes fe_ccircle_spatial_notrees", {
  il  <- base_inv_list()
  pid <- il$plot_id[[2]]
  # Replace the whole tree list of this plot with a single shrub record.
  il$trees[[2]] <- make_shrub_row(il$trees[[2]])

  inv <- suppressWarnings(suppressMessages(
    fe_inventory(il, object_type = "fe_ccircle_spatial")
  ))

  idx <- which(inv$plot_id == pid)
  expect_length(idx, 1L)
  expect_s3_class(inv$plot[[idx]], "fe_ccircle_spatial_notrees")
})

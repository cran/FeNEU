


test_that("fe_inventory behaves predictably", {

  inv_path <- system.file("extdata", package = "FeNEU") |>
    paste0("/data_ex3_sample_pre")

  # The ex3 pre is self-describing (coord_sys = utm32), so no argument is
  # needed; the rebuilt object matches the packaged one.
  rebuilt <- read_and_convert_data(inv_path,
                                   inventory_type = "sample_concentric") |>
    suppressWarnings() |>
    suppressMessages()

  # The WKT text of a CRS depends on the PROJ installed (see helper-crs.R), so
  # both sides are compared with their CRSs re-derived in this session. The
  # comparison still requires both to name the same system: a different EPSG
  # code would yield a different CRS here.
  expect_equal(
    restamp_crs(rebuilt),
    restamp_crs(data_ex3_sample_fe_inventory)
  )

})


# Recode a bavrn_state species_id vector into a different coding system
# (tum_wwk_short) so it can be used to fake a mixed-coding inventory. The cast
# emits override messages/warnings which are irrelevant here.
.recode_species <- function(v) {
  suppressWarnings(suppressMessages(
    ForestElementsR::as_fe_species_tum_wwk_short(v)
  ))
}

test_that("validate_fe_inventory accepts uniform species coding", {
  # The shipped example passes even though several plots carry a logical NA
  # placeholder in small_trees$species_id (empty small-tree species slot) - such
  # placeholders must be tolerated and not be mistaken for a coding system.
  expect_identical(
    validate_fe_inventory(data_ex3_sample_fe_inventory),
    data_ex3_sample_fe_inventory
  )
})


test_that("validate_fe_inventory rejects mixed species coding in trees", {
  inv <- data_ex3_sample_fe_inventory
  inv$plot[[1]]$trees$species_id <-
    .recode_species(inv$plot[[1]]$trees$species_id)

  expect_error(
    validate_fe_inventory(inv),
    "species coding systems must not be mixed"
  )
})


test_that("validate_fe_inventory rejects mixed species coding in small_trees", {
  # Regression: the coding-mix check now inspects small_trees$species_id, not
  # only trees. Plot 1 of the example carries genuine fe_species small trees.
  inv <- data_ex3_sample_fe_inventory
  stopifnot(startsWith(class(inv$plot[[1]]$small_trees$species_id)[1],
                       "fe_species"))
  inv$plot[[1]]$small_trees$species_id <-
    .recode_species(inv$plot[[1]]$small_trees$species_id)

  expect_error(
    validate_fe_inventory(inv),
    "species coding systems must not be mixed"
  )
})


test_that("validate_fe_inventory rejects trees and small_trees using different codings", {
  # Even a *consistent* split - every trees table in one coding, every
  # small_trees table in another - is rejected: a single fe_inventory must use
  # one coding system throughout, across both tree tables and all plots.
  inv <- data_ex3_sample_fe_inventory
  for (i in seq_along(inv$plot)) {
    sp <- inv$plot[[i]]$small_trees$species_id
    if (!is.null(sp) && startsWith(class(sp)[1], "fe_species")) {
      inv$plot[[i]]$small_trees$species_id <- .recode_species(sp)
    }
  }

  expect_error(
    validate_fe_inventory(inv),
    "species coding systems must not be mixed"
  )
})



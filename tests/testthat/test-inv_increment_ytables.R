# Tests for inv_increment_ytables(), the bundled yield-table increment, and its
# PDF renderer.
#
# Depth (helper-depth.R): the PDF render runs at "full". Rendering does hang on
# foreign code (rmarkdown, pandoc, LaTeX, kableExtra), but since 2da8703 the
# help pages render seven PDFs under \donttest, so CRAN exercises that path
# anyway and does not need to pay for it twice.
#
# Key invariants:
#   - the estimate is per cohort: one species in the main stand of one plot,
#     which has one age and one quadratic mean diameter. The class an
#     evaluation groups by is a label on that estimate, so both axes reach the
#     same inventory total.
#   - overview: one row per species group plus a total row (species_group NA),
#     total site_index NA (yield-table index scales differ), total zv_m3_yr =
#     sum of the species rows
#   - ytables_used carries the German and international yield-table names
#   - collective is "main_stand"
#   - the PDF renders (skipped when the LaTeX/Pandoc toolchain is absent)

suppressWarnings(suppressMessages({
  inv <- data_ex3_sample_fe_inventory
  tr <- inv |>
    pull_trees() |>
    height_complete_inventory(method = "NFI")
  tr$height_m <- ifelse(is.na(tr$height_m), tr$h_est_m, tr$height_m)
  tr <- tr |> trees_add_essentials(method = "BaySF")

  sel <- ForestElementsR::ytables_bavrn_state_var_1
  inc <- inv_increment_ytables(inv, tr, sel)
}))


test_that("inv_increment_ytables() returns the documented structure", {
  expect_s3_class(inc, "fe_increment_ytables")
  expect_s3_class(inc, "fe_increment")
  expect_true(is_fe_increment_ytables(inc))
  expect_named(inc, c("cohorts", "overview", "meta", "ytables_used",
                      "collective"), ignore.order = TRUE)

  expect_identical(inc$meta$source, "ytable")
  expect_identical(inc$meta$n_plots, nrow(inv))
  expect_equal(inc$meta$area_ha, sum(inv$area_rep_ha))
})


test_that("a cohort is one species on one plot, carrying both class labels", {
  co <- inc$cohorts
  expect_true(all(c("plot_id", "species_id", "age_yr", "age_class",
                    "d_q_class", "site_index", "iv_hub_m3_ha_yr_yt") %in%
                    names(co)))
  # one row per plot and species, nothing split any further
  key <- co[, c("plot_id", "layer_key", "species_id")]
  expect_equal(nrow(co), nrow(unique(key)))
  # main stand only
  expect_true(all(co$layer_key == 1))
})


test_that("the class axis labels the estimate but does not change it", {
  bt_a <- suppressWarnings(suppressMessages(
    increment_ytables_base_table(inc, by_class = "age")))
  bt_d <- suppressWarnings(suppressMessages(
    increment_ytables_base_table(inc, by_class = "dq")))

  expect_equal(bt_a$all_total$iv_m3_yr_total, bt_d$all_total$iv_m3_yr_total,
               tolerance = 1e-9)
  expect_equal(bt_a$all_total$iv_m3_ha_yr, bt_d$all_total$iv_m3_ha_yr,
               tolerance = 1e-9)

  # and both agree with the overview of the same bundle
  tot <- inc$overview[inc$overview$is_total_row, ]
  expect_equal(bt_a$all_total$iv_m3_yr_total, tot$zv_m3_yr, tolerance = 1e-8)
  expect_equal(bt_a$all_total$iv_m3_ha_yr,    tot$zv_m3_ha_yr, tolerance = 1e-8)
})


test_that("the bundle and the bare tree list give the same tables", {
  from_bundle <- suppressWarnings(suppressMessages(
    increment_ytables_base_table(inc, by_class = "age")))
  from_trees <- suppressWarnings(suppressMessages(
    increment_ytables_base_table(tr, sel, by_class = "age", fe_inv = inv)))

  expect_equal(from_bundle$detail,    from_trees$detail)
  expect_equal(from_bundle$all_total, from_trees$all_total)
})


test_that("without a bundle the yield-table selection is required", {
  expect_error(
    suppressWarnings(suppressMessages(
      increment_ytables_base_table(tr, by_class = "age", fe_inv = inv))),
    "ytable_selection"
  )
})


test_that("the overview has species rows plus one total row", {
  o <- inc$overview
  expect_true(all(c("species_group", "area_ha", "site_index", "zv_m3_ha_yr",
                    "zv_m3_yr", "is_total_row") %in% names(o)))
  expect_equal(sum(o$is_total_row), 1L)
  tot <- o[o$is_total_row, ]
  expect_true(is.na(tot$species_group))
  expect_true(is.na(tot$site_index))   # index scales differ between tables
})


test_that("the total row sums the species rows (zv_m3_yr)", {
  o   <- inc$overview
  sp  <- o[!o$is_total_row & !o$is_no_main_stand_row, ]
  tot <- o[o$is_total_row, ]
  expect_equal(sum(sp$zv_m3_yr), tot$zv_m3_yr, tolerance = 1e-8)
})


test_that("ytables_used carries German and international names", {
  yu <- inc$ytables_used
  expect_true(all(c("species_group", "ytable_name", "name_orig",
                    "name_international") %in% names(yu)))
  expect_gt(nrow(yu), 0)
  expect_false(any(is.na(yu$name_orig)))
})


test_that("the collective is main_stand", {
  expect_equal(inc$collective$kind, "main_stand")
})


test_that("the areas add up to the whole operation area", {
  # The inventory is part of the bundle, so per-hectare figures always relate
  # to the whole operation. Points carrying no living main stand appear as a
  # residual row, so that the areas still add up.
  o   <- inc$overview
  tot <- o[o$is_total_row, ]
  res <- o[o$is_no_main_stand_row, ]
  sp  <- o[!o$is_total_row & !o$is_no_main_stand_row, ]

  expect_equal(tot$area_ha, sum(inv$area_rep_ha), tolerance = 1e-8)
  expect_equal(sum(sp$area_ha) + sum(res$area_ha), tot$area_ha,
               tolerance = 1e-8)
  expect_equal(tot$zv_m3_ha_yr, tot$zv_m3_yr / tot$area_ha, tolerance = 1e-8)

  if (nrow(res) > 0) {
    expect_true(res$area_ha > 0)
    expect_true(is.na(res$zv_m3_yr))
  }
})


test_that("the estimate refuses an inventory whose main stand lacks ages", {
  tr_na <- tr
  i <- which(tr_na$layer_key == 1 & !tr_na$removal)[1]
  tr_na$age_yr[i] <- NA_real_
  expect_error(suppressWarnings(suppressMessages(
    inv_increment_ytables(inv, tr_na, sel))))
})


test_that("data with several ages per cohort raise a warning", {
  tr_multi <- tr
  i <- which(tr_multi$layer_key == 1 & !tr_multi$removal)
  # give one tree of an existing cohort a different age
  sp1 <- tr_multi$species_id[i[1]]
  pl1 <- tr_multi$plot_id[i[1]]
  same <- which(tr_multi$layer_key == 1 & !tr_multi$removal &
                  tr_multi$plot_id == pl1 &
                  unclass(tr_multi$species_id) == unclass(sp1))
  skip_if(length(same) < 2, "no cohort with more than one tree")
  tr_multi$age_yr[same[1]] <- tr_multi$age_yr[same[1]] + 20

  expect_warning(
    suppressMessages(inv_increment_ytables(inv, tr_multi, sel)),
    "age|Alter"
  )
})


test_that("the overview PDF renders from the bundle", {
  skip_below_depth("full")
  skip_if_not(
    isTRUE(tryCatch(check_pdf_dependencies("test"), error = function(e) FALSE)),
    "PDF toolchain not available"
  )
  out <- tempfile("ytpdf"); dir.create(out)
  p <- suppressWarnings(suppressMessages(
    output_increment_overview_ytables_pdf(inc, tab_title = "Testbetrieb",
                                          output_dir = out)))
  expect_true(file.exists(p))
})

# Tests for base_table_*() and output_base_table()
# Verifies horizontal sums, vertical sums, and per_species percentages.

suppressWarnings(suppressMessages({

  # The shipped list is exactly this inventory put through the canonical chain
  # (see data-raw/), so building it here again would only cost a cluster start.
  trees <- data_ex3_sample_trees_essentials

  bt_all <- trees |> base_table_age_class()
  bt_ms  <- trees |> base_table_age_class_main_stand()

  out_all <- output_base_table(bt_all)
  out_ms  <- output_base_table(bt_ms)

}))


# ---------- list structure ----------------------------------------------------

test_that("base_table_age_class() returns a 4-element list of data frames", {
  expect_named(bt_all, c("detail", "total", "all_species", "all_total"))
  expect_true(all(vapply(bt_all, is.data.frame, logical(1L))))
})

test_that("base_table_age_class_main_stand() returns a 4-element list of data frames", {
  expect_named(bt_ms, c("detail", "total", "all_species", "all_total"))
  expect_true(all(vapply(bt_ms, is.data.frame, logical(1L))))
})


# ---------- all-layers: horizontal sums (detail -> total per species) ---------
# For absolute variables, sum across age classes must equal species total.

test_that("base_table (all layers): n/g_m2/v_hub_m3 sum across age classes equals total per species", {
  add_vars <- c("n", "g_m2", "v_hub_m3")

  for (v in add_vars) {
    detail_sums <- bt_all$detail |>
      dplyr::group_by(species_group) |>
      dplyr::summarise(summed = sum(.data[[v]], na.rm = TRUE), .groups = "drop")

    total_vals <- bt_all$total |>
      dplyr::select(species_group, total_val = dplyr::all_of(v))

    joined <- dplyr::inner_join(detail_sums, total_vals, by = "species_group")

    expect_equal(joined$summed, joined$total_val,
                 tolerance = 1e-6, label = paste("horizontal sum:", v))
  }
})


# ---------- all-layers: vertical sums (detail -> all_species per class) ------
# For absolute variables, sum across species in a class must equal all_species.

test_that("base_table (all layers): n/g_m2/v_hub_m3 sum across species equals all_species per class", {
  add_vars <- c("n", "g_m2", "v_hub_m3")

  for (v in add_vars) {
    detail_sums <- bt_all$detail |>
      dplyr::group_by(age_class) |>
      dplyr::summarise(summed = sum(.data[[v]], na.rm = TRUE), .groups = "drop")

    all_sp_vals <- bt_all$all_species |>
      dplyr::select(age_class, all_val = dplyr::all_of(v))

    joined <- dplyr::inner_join(detail_sums, all_sp_vals, by = "age_class")

    expect_equal(joined$summed, joined$all_val,
                 tolerance = 1e-6, label = paste("vertical sum:", v))
  }
})


# ---------- all-layers: all_species -> all_total across classes ---------------

test_that("base_table (all layers): n/g_m2/v_hub_m3 sum across classes equals all_total", {
  add_vars <- c("n", "g_m2", "v_hub_m3")

  for (v in add_vars) {
    expect_equal(
      sum(bt_all$all_species[[v]], na.rm = TRUE),
      bt_all$all_total[[v]],
      tolerance = 1e-6,
      label = paste("all_species -> all_total:", v)
    )
  }
})


# ---------- main-stand: per_species sums to 100 per class --------------------

test_that("base_table (main stand): per_species sums to 100 within each age class", {
  class_sums <- bt_ms$detail |>
    dplyr::group_by(age_class) |>
    dplyr::summarise(pct_sum = sum(per_species, na.rm = TRUE), .groups = "drop")

  expect_equal(class_sums$pct_sum, rep(100, nrow(class_sums)), tolerance = 1e-6)
})

test_that("base_table (main stand): all_species$per_species sums to 100 across classes", {
  expect_equal(sum(bt_ms$all_species$per_species, na.rm = TRUE), 100, tolerance = 1e-6)
})

test_that("base_table (main stand): all_total$per_species equals 100", {
  expect_equal(bt_ms$all_total$per_species, 100)
})


# ---------- main-stand: area_ha additive in both directions ------------------

test_that("base_table (main stand): area_ha sums across age classes equals total per species", {
  detail_sums <- bt_ms$detail |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(area_sum = sum(area_ha, na.rm = TRUE), .groups = "drop")

  total_areas <- bt_ms$total |>
    dplyr::select(species_group, area_ha)

  joined <- dplyr::inner_join(detail_sums, total_areas, by = "species_group")
  expect_equal(joined$area_sum, joined$area_ha, tolerance = 1e-6)
})

test_that("base_table (main stand): area_ha sums across species equals all_species per class", {
  detail_sums <- bt_ms$detail |>
    dplyr::group_by(age_class) |>
    dplyr::summarise(area_sum = sum(area_ha, na.rm = TRUE), .groups = "drop")

  all_sp_areas <- bt_ms$all_species |>
    dplyr::select(age_class, area_ha)

  joined <- dplyr::inner_join(detail_sums, all_sp_areas, by = "age_class")
  expect_equal(joined$area_sum, joined$area_ha, tolerance = 1e-6)
})


# ---------- output_base_table(): Summe block = colSums of species rows --------
# Helper: colSums that returns NA (not 0) for all-NA columns, matching the
# behaviour of output_base_table() which leaves empty classes as NA.

colsums_na_aware <- function(mat) {
  result <- colSums(mat, na.rm = TRUE)
  result[colSums(!is.na(mat)) == 0L] <- NA_real_
  result
}

test_that("output_base_table (all layers): Summe block equals colSums of species rows for n/g/v", {
  class_cols <- grep("^\\(", names(out_all), value = TRUE)
  add_vars   <- c("v_hub_m3", "g_m2", "n")
  summe_rows <- out_all[is.na(out_all$species_group), ]
  sp_rows    <- out_all[!is.na(out_all$species_group), ]

  for (v in add_vars) {
    summe_v <- summe_rows[summe_rows$variable == v, ]
    sp_v    <- sp_rows[sp_rows$variable == v, ]
    if (nrow(summe_v) == 0 || nrow(sp_v) == 0) next

    expected <- colsums_na_aware(sp_v[, class_cols, drop = FALSE])
    actual   <- unlist(summe_v[, class_cols])
    names(actual) <- class_cols

    expect_equal(actual, expected, tolerance = 1e-4, label = paste("Summe colSums:", v))
  }
})

test_that("output_base_table (main stand): Summe block area_ha equals colSums of species rows", {
  class_cols <- grep("^\\(", names(out_ms), value = TRUE)
  summe_rows <- out_ms[is.na(out_ms$species_group), ]
  sp_rows    <- out_ms[!is.na(out_ms$species_group), ]

  summe_area <- summe_rows[summe_rows$variable == "area_ha", ]
  sp_area    <- sp_rows[sp_rows$variable == "area_ha", ]

  expected <- colsums_na_aware(sp_area[, class_cols, drop = FALSE])
  actual   <- unlist(summe_area[, class_cols])
  names(actual) <- class_cols

  expect_equal(actual, expected, tolerance = 1e-4)
})

test_that("output_base_table (main stand): per_species Summe block sums to 100 across classes", {
  class_cols <- grep("^\\(", names(out_ms), value = TRUE)
  summe_rows <- out_ms[is.na(out_ms$species_group), ]
  pct_row    <- summe_rows[summe_rows$variable == "per_species", ]

  expect_equal(sum(unlist(pct_row[, class_cols]), na.rm = TRUE), 100, tolerance = 1e-6)
})


# ---------- %Anteil: BaySF's dual Basistabelle scheme --------------------------
# BaySF's Basistabelle shows two different "%Anteil" rows that meet at 100 %
# in the Summe/Summe corner:
#  - per species: "%Anteil der Baumartengruppe" (pct_anteil_bag) = species' G
#    as a share of the cross-species G total OF THAT CLASS COLUMN
#    (column-normalized). Down each column - incl. the Summe column - the
#    species sum to 100 %; each species' Summe cell is its G share of the whole.
#  - the cross-species Summe row: "%Anteil der Klasse" (pct_anteil_class) =
#    that class' G as a share of the grand total (row-normalized). Sums to
#    100 % across classes; its Summe cell is 100 %.

test_that("output_base_table() (all layers): pct_anteil_bag is column-normalized (species share per class)", {
  class_cols <- grep("^\\(", names(out_all), value = TRUE)

  bag     <- out_all[out_all$variable == "pct_anteil_bag" & !is.na(out_all$species_group), ]
  summe_g <- out_all[out_all$variable == "g_m2" & is.na(out_all$species_group), ]
  sp_g    <- out_all[out_all$variable == "g_m2" & !is.na(out_all$species_group), ]

  # cell == 100 * species g / cross-species g of that column
  joined <- dplyr::inner_join(
    sp_g[, c("species_group", class_cols)],
    bag[, c("species_group", class_cols)],
    by = "species_group", suffix = c("_g", "_pct")
  )
  for (cn in class_cols) {
    denom    <- summe_g[[cn]]
    expected <- if (!is.na(denom) && denom > 0) 100 * joined[[paste0(cn, "_g")]] / denom else NA_real_
    got      <- joined[[paste0(cn, "_pct")]]
    keep     <- !is.na(expected)
    expect_equal(got[keep], expected[keep], tolerance = 1e-6)
  }

  # each species' Summe (total) cell = its G share of the grand total; species
  # sum to 100 % (NOT 100 per species).
  grand_g <- summe_g$total
  expect_equal(bag$total, 100 * sp_g$total / grand_g, tolerance = 1e-6)
  expect_equal(sum(bag$total), 100, tolerance = 1e-6)

  # down each column the species shares sum to 100 % (where the column has G)
  col_sums <- colSums(bag[, class_cols, drop = FALSE], na.rm = TRUE)
  has_g    <- vapply(class_cols, function(cn) isTRUE(summe_g[[cn]] > 0), logical(1))
  expect_equal(unname(col_sums[has_g]), rep(100, sum(has_g)), tolerance = 1e-1)
})

test_that("output_base_table() (all layers): Summe pct_anteil_class is row-normalized and sums to 100 %", {
  class_cols <- grep("^\\(", names(out_all), value = TRUE)
  cls <- out_all[out_all$variable == "pct_anteil_class", ]
  expect_equal(nrow(cls), 1)
  expect_true(is.na(cls$species_group))
  expect_equal(sum(cls[, class_cols], na.rm = TRUE), 100, tolerance = 1e-1)
  expect_equal(cls$total, 100, tolerance = 1e-6)
})


# ---------- output_base_table(): rows stay contiguous per species -----------
# The Rmd template's pack_rows() assumes every species' rows form one
# unbroken block (in species_group order); appending pct_anteil_bag as a
# separate bind_rows() step previously broke this by clumping every species'
# bag row together at the very end instead of next to its own species,
# scrambling the rendered table. Guard against that regressing.

is_contiguous_by_group <- function(g) {
  # base R's rle() never merges consecutive NAs (NA == NA is NA, not TRUE),
  # so a run of NA rows would wrongly look like many separate 1-row groups.
  # The Rmd template groups via dplyr::group_by(), which correctly treats
  # all NAs as a single group - replace NA with a sentinel to match that.
  g_chr <- format(g)
  g_chr[is.na(g)] <- "..NA.."
  rl <- rle(g_chr)
  length(rl$values) == length(unique(g_chr))
}

test_that("output_base_table() (all layers): each species' rows are contiguous", {
  expect_true(is_contiguous_by_group(out_all$species_group))
})

test_that("output_base_table() (main stand): each species' rows are contiguous", {
  expect_true(is_contiguous_by_group(out_ms$species_group))
})


# ---------- PDF renderer wrapper smoke test (skip only if toolchain missing) --
# Per project convention, every exported *_pdf() needs a render test, and a
# render *failure* must surface as a test failure - only a missing toolchain
# (rmarkdown / pandoc / pdflatex) may skip.

test_that("output_base_table_pdf() renders to a PDF", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not on PATH")

  # exercise the `inventory =` shortcut: the meta block is derived from the
  # fe_inventory via inventory_meta() (no hand-built meta list needed).
  pdf_path <- output_base_table_pdf(
    out_ms, tab_title = "Testbetrieb",
    output_dir = tempdir(),
    inventory = data_ex3_sample_fe_inventory
  )
  expect_true(file.exists(pdf_path))
  expect_match(basename(pdf_path), "\\.pdf$")
  # type (age) and cohort (mainstand) are derived from the object and appear
  # in the self-describing file name.
  expect_match(basename(pdf_path), "_age_mainstand\\.pdf$")
})


# ---------- class column with an NA level (incomplete ages) -------------------
# Trees without an age fall into an age_class of NA, which pivots to a column
# literally named "NA". The share rows used to be normalised over the columns
# matched by "^\(", so that column was skipped and its raw basal area was
# relabelled as a percentage.

test_that("share rows are normalised in the NA age class column too", {
  trees_na <- trees
  set.seed(1)
  idx <- sample(nrow(trees_na), floor(nrow(trees_na) / 4))
  trees_na$age_yr[idx]    <- NA_real_
  trees_na$age_class[idx] <- NA

  suppressWarnings(suppressMessages({
    out_na <- trees_na |> base_table_age_class() |> output_base_table()
  }))

  expect_true("NA" %in% names(out_na))

  g_rows   <- out_na[out_na$variable == "g_m2", ]
  bag_rows <- out_na[out_na$variable == "pct_anteil_bag", ]
  expect_gt(nrow(bag_rows), 0)

  # A share must be a share: never the raw basal area, and never above 100 %.
  expect_true(all(bag_rows[["NA"]] <= 100 + 1e-8, na.rm = TRUE))
  expect_false(isTRUE(all.equal(
    bag_rows[["NA"]][!is.na(bag_rows[["NA"]])],
    g_rows[!is.na(g_rows$species_group), ][["NA"]][!is.na(bag_rows[["NA"]])]
  )))

  # Column-normalised: the species shares of the NA column sum to 100 %.
  expect_equal(sum(bag_rows[["NA"]], na.rm = TRUE), 100, tolerance = 1e-6)

  # And the cross-species row-normalised share includes the NA column, so the
  # class shares still add up to 100 % across all columns.
  cls_row <- out_na[out_na$variable == "pct_anteil_class", ]
  cls_cols <- setdiff(names(out_na), c("species_group", "variable", "total"))
  expect_equal(sum(unlist(cls_row[cls_cols]), na.rm = TRUE), 100,
               tolerance = 1e-6)
})

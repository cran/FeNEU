# Tests for structure_table_*() and output_structure_table()
# Verifies horizontal sums (across dbh_class), vertical sums (across species),
# and per_species percentages for main-stand variant.

suppressWarnings(suppressMessages({

  # The shipped list is exactly this inventory put through the canonical chain
  # (pull_trees -> height_complete_inventory -> fill_heights_back -> pull_trees
  # -> trees_add_essentials(method = "BaySF"), see data-raw/). Using it saves
  # building the chain -- and starting a parallel cluster -- for every test file.
  trees <- data_ex3_sample_trees_essentials

  st_all <- trees |> structure_table_age_class()
  st_ms  <- trees |> structure_table_age_class_main_stand()

  out_all <- output_structure_table(st_all)
  out_ms  <- output_structure_table(st_ms)

}))


# ---------- list structure ----------------------------------------------------

test_that("structure_table_age_class() returns an 8-element list of data frames", {
  expect_named(st_all, c("detail", "total", "all_species", "all_total",
                          "species_total", "species_dbh_total",
                          "grand_total", "grand_dbh_total"))
  expect_true(all(vapply(st_all, is.data.frame, logical(1L))))
})

test_that("structure_table_age_class_main_stand() returns an 8-element list of data frames", {
  expect_named(st_ms, c("detail", "total", "all_species", "all_total",
                         "species_total", "species_dbh_total",
                         "grand_total", "grand_dbh_total"))
  expect_true(all(vapply(st_ms, is.data.frame, logical(1L))))
})


# ---------- all-layers: horizontal sums (detail -> total per species × class) -
# For absolute variables, sum across dbh_class must equal total.

test_that("structure_table (all layers): n/g_m2/v_hub_m3 sum across dbh_class equals total", {
  add_vars <- c("n", "g_m2", "v_hub_m3")

  for (v in add_vars) {
    detail_sums <- st_all$detail |>
      dplyr::group_by(species_group, age_class) |>
      dplyr::summarise(summed = sum(.data[[v]], na.rm = TRUE), .groups = "drop")

    total_vals <- st_all$total |>
      dplyr::select(species_group, age_class, total_val = dplyr::all_of(v))

    joined <- dplyr::inner_join(detail_sums, total_vals, by = c("species_group", "age_class"))

    expect_equal(joined$summed, joined$total_val,
                 tolerance = 1e-6, label = paste("dbh horizontal sum:", v))
  }
})


# ---------- all-layers: vertical sums (detail -> all_species per class × dbh) -
# For absolute variables, sum across species must equal all_species.

test_that("structure_table (all layers): n/g_m2/v_hub_m3 sum across species equals all_species", {
  add_vars <- c("n", "g_m2", "v_hub_m3")

  for (v in add_vars) {
    detail_sums <- st_all$detail |>
      dplyr::group_by(age_class, dbh_class) |>
      dplyr::summarise(summed = sum(.data[[v]], na.rm = TRUE), .groups = "drop")

    all_sp_vals <- st_all$all_species |>
      dplyr::select(age_class, dbh_class, all_val = dplyr::all_of(v))

    joined <- dplyr::inner_join(detail_sums, all_sp_vals, by = c("age_class", "dbh_class"))

    expect_equal(joined$summed, joined$all_val,
                 tolerance = 1e-6, label = paste("cross-species sum:", v))
  }
})


# ---------- all-layers: all_species -> all_total across dbh_class -------------

test_that("structure_table (all layers): n/g_m2/v_hub_m3 sum across dbh_class equals all_total", {
  add_vars <- c("n", "g_m2", "v_hub_m3")

  for (v in add_vars) {
    all_sp_sums <- st_all$all_species |>
      dplyr::group_by(age_class) |>
      dplyr::summarise(summed = sum(.data[[v]], na.rm = TRUE), .groups = "drop")

    all_tot_vals <- st_all$all_total |>
      dplyr::select(age_class, tot_val = dplyr::all_of(v))

    joined <- dplyr::inner_join(all_sp_sums, all_tot_vals, by = "age_class")

    expect_equal(joined$summed, joined$tot_val,
                 tolerance = 1e-6, label = paste("all_species -> all_total:", v))
  }
})


# ---------- main-stand: per_species sums to 100 per age class ----------------
# per_species is constant within a (species × age_class) block; distinct()
# de-duplicates across dbh_class before summing across species.

test_that("structure_table (main stand): per_species sums to 100 within each age class", {
  class_sums <- st_ms$detail |>
    dplyr::distinct(species_group, age_class, per_species) |>
    dplyr::group_by(age_class) |>
    dplyr::summarise(pct_sum = sum(per_species, na.rm = TRUE), .groups = "drop")

  expect_equal(class_sums$pct_sum, rep(100, nrow(class_sums)), tolerance = 1e-6)
})

test_that("structure_table (main stand): all_species$per_species sums to 100 per age class", {
  class_sums <- st_ms$all_species |>
    dplyr::distinct(age_class, per_species) |>
    dplyr::group_by(age_class) |>
    dplyr::summarise(pct_sum = sum(per_species, na.rm = TRUE), .groups = "drop")

  expect_equal(class_sums$pct_sum, rep(100, nrow(class_sums)), tolerance = 1e-6)
})


# ---------- output_structure_table(): total column for additive vars ----------
# output_structure_table() sets total = round(rowSums(dbh_cols), 2) for most
# variables. Exceptions: d_q_cm, h_q_m (weighted means), conf_95_per,
# v_hub_m3_ha (both NA), and n_plot (a distinct-plot COUNT, not a tree sum - a
# plot with the species spread across several dbh classes is counted once per
# class it appears in, so per-dbh_class values can legitimately over-sum
# relative to the single distinct-plot total; this is most visible in the
# per-species "Summe" row, which collapses across ALL age/dq classes and so
# maximises the chance a plot contributes to multiple dbh_class buckets).
# pct_anteil_v is excluded here: it is a % row (its dbh cells sum to 100 by
# construction), and its Total is left NA on individual class rows (only the
# "Summe" rows carry 100) - so it does not follow the additive rowSums rule.
# We verify the contract for the remaining additive rows, which holds for both
# the per-class rows and the per-species "Summe" rows, since
# species_dbh_total's per-dbh_class values are aggregated directly from the
# trees (not derived from species_total).

test_that("output_structure_table (all layers): total = rowSums(dbh_cols) for additive variables", {
  dbh_cols <- grep("^\\(", names(out_all), value = TRUE)
  non_add  <- c("conf_95_per", "v_hub_m3_ha", "d_q_cm", "h_q_m", "n_plot", "pct_anteil_v")
  add_rows <- out_all[!out_all$variable %in% non_add & !is.na(out_all$total), ]

  computed <- round(rowSums(add_rows[, dbh_cols, drop = FALSE], na.rm = TRUE), 2)
  expect_equal(computed, round(add_rows$total, 2), tolerance = 1e-2)
})


# ---------- output_structure_table(): Summe block = colSums of species rows ---
# Helper: sum that returns NA (not 0) when all values are NA, matching the
# behaviour of output_structure_table() which leaves empty cells as NA.

sum_na_aware <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

test_that("output_structure_table (all layers): Summe block equals colSums of species rows for n/g/v", {
  dbh_cols   <- grep("^\\(", names(out_all), value = TRUE)
  add_vars   <- c("v_hub_m3", "g_m2", "n")
  summe_rows <- out_all[is.na(out_all$species_group), ]
  sp_rows    <- out_all[!is.na(out_all$species_group), ]

  summe_long <- summe_rows |>
    dplyr::filter(.data$variable %in% add_vars) |>
    tidyr::pivot_longer(dplyr::all_of(dbh_cols),
                        names_to = "dbh_class", values_to = "summe_val")

  sp_sums_long <- sp_rows |>
    dplyr::filter(.data$variable %in% add_vars) |>
    tidyr::pivot_longer(dplyr::all_of(dbh_cols),
                        names_to = "dbh_class", values_to = "sp_val") |>
    dplyr::group_by(age_class, variable, dbh_class) |>
    dplyr::summarise(sp_sum = sum_na_aware(sp_val), .groups = "drop")

  joined <- dplyr::inner_join(summe_long, sp_sums_long,
                               by = c("age_class", "variable", "dbh_class"))

  expect_equal(joined$summe_val, joined$sp_sum, tolerance = 1e-4)
})


# ---------- output_structure_table(): cross-species grand "Summe" row -------
# The cross-species "Summe" block should itself get a trailing "Summe" row
# (grand total across every species, class, and dbh class), whose Total must
# equal the sum of every individual species' own "Summe" row Total.

test_that("output_structure_table (all layers): cross-species grand Summe row equals sum of species Summe rows", {
  add_vars <- c("v_hub_m3", "g_m2", "n")

  grand_row <- out_all[is.na(out_all$species_group) & out_all$age_class == "Summe" &
                          out_all$variable %in% add_vars, c("variable", "total")]
  expect_equal(nrow(grand_row), length(add_vars))

  species_summe_sum <- out_all[!is.na(out_all$species_group) & out_all$age_class == "Summe" &
                                  out_all$variable %in% add_vars, ] |>
    dplyr::group_by(variable) |>
    dplyr::summarise(sp_sum = sum(total, na.rm = TRUE), .groups = "drop")

  joined <- dplyr::inner_join(grand_row, species_summe_sum, by = "variable")
  expect_equal(nrow(joined), length(add_vars))
  expect_equal(joined$total, joined$sp_sum, tolerance = 1e-4)
})


# ---------- output_structure_table(): pct_anteil_v ("%Anteil") ---------------
# BaySFs Strukturtabelle "%Anteil" is the standing-volume (Vorrat)
# distribution across the dbh classes: each dbh_class cell of a "v_hub_m3" row
# as a % of THAT SAME ROW's own total (verified: BaySF Fichte Akl I
# 13.328/21.801 = 61.1 %). Row-normalized, so every v_hub_m3 row - a single
# age/dq class, a species "Summe" row, or the cross-species grand Summe row -
# gets a pct_anteil_v row whose dbh cells sum to 100 % of THAT row. The Total
# column is NA on individual class rows and 100 only on "Summe" rows (matching
# BaySF, which leaves it blank except on the Summe line).

test_that("output_structure_table (all layers): pct_anteil_v is the row-normalized Vorrat distribution", {
  dbh_cols <- grep("^\\(", names(out_all), value = TRUE)

  v_rows   <- out_all[out_all$variable == "v_hub_m3", ]
  pct_rows <- out_all[out_all$variable == "pct_anteil_v", ]
  expect_equal(nrow(v_rows), nrow(pct_rows))

  # spot-check: pct cell == 100 * v cell / that row's own v total
  joined <- dplyr::inner_join(
    v_rows[, c("species_group", "age_class", dbh_cols, "total")],
    pct_rows[, c("species_group", "age_class", dbh_cols)],
    by = c("species_group", "age_class"), suffix = c("_v", "_pct")
  )
  for (cn in dbh_cols) {
    expected <- ifelse(!is.na(joined$total) & joined$total > 0,
                        100 * joined[[paste0(cn, "_v")]] / joined$total, NA_real_)
    expect_equal(joined[[paste0(cn, "_pct")]], expected, tolerance = 1e-6)
  }

  # each pct_anteil_v row's own dbh cells sum to 100 % of ITS OWN row
  row_sums <- rowSums(pct_rows[, dbh_cols, drop = FALSE], na.rm = TRUE)
  has_any  <- rowSums(!is.na(pct_rows[, dbh_cols, drop = FALSE])) > 0
  expect_equal(row_sums[has_any], rep(100, sum(has_any)), tolerance = 1e-1)
})

test_that("output_structure_table (all layers): pct_anteil_v Total is NA on class rows, 100 on Summe rows", {
  pct_rows <- out_all[out_all$variable == "pct_anteil_v", ]
  is_summe <- as.character(pct_rows$age_class) == "Summe"

  # Summe rows carry 100 in the Total column
  expect_true(all(abs(pct_rows$total[is_summe] - 100) < 1e-6))
  # individual class rows leave the Total column blank (NA)
  expect_true(all(is.na(pct_rows$total[!is_summe])))
})


# ---------- output_structure_table(): dbh_class columns are in ascending order
# Regression guard: the Bhd-Klassen columns must come out sorted by their lower
# bound, not in order of first appearance. output_structure_table() pivots
# dbh_class to columns with names_sort = TRUE; without it the columns render
# scrambled (e.g. (0,5] (5,10] (10,15] (40,45] (30,35] ...) in the PDF, because
# pivot_wider defaults to first-appearance order for the new columns.

test_that("output_structure_table: dbh_class columns are sorted ascending", {
  for (out in list(out_all, out_ms)) {
    dbh_cols   <- grep("^\\(", names(out), value = TRUE)
    low_bound  <- as.numeric(sub("^\\(([-0-9.]+),.*$", "\\1", dbh_cols))
    expect_false(is.unsorted(low_bound))
  }
})


# ---------- PDF renderer wrapper smoke test (skip only if toolchain missing) --
# Per project convention, every exported *_pdf() needs a render test, and a
# render *failure* must surface as a test failure - only a missing toolchain
# (rmarkdown / pandoc / pdflatex) may skip.

test_that("output_structure_table_pdf() renders to a PDF", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not on PATH")

  # exercise the `inventory =` shortcut: meta is derived via inventory_meta(),
  # and the same argument guards against standwise inventories (see below).
  pdf_path <- output_structure_table_pdf(
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

test_that("output_structure_table_pdf() rejects a standwise inventory via inventory=", {
  # structure tables are only meaningful for sample (concentric-circle)
  # inventories; passing a standwise inventory must stop with a clear error.
  # No PDF toolchain needed - the guard fires before any rendering.
  expect_error(
    output_structure_table_pdf(
      out_ms, tab_title = "Testbetrieb",
      output_dir = tempdir(),
      inventory = data_ex6_standwise_fe_inventory
    ),
    "sample|Stichprobeninventur"
  )
})


# ---------- incomplete ages ---------------------------------------------------
# Trees without an age form an age class of NA. pack_rows() cannot take an NA
# group name, so the age-class structure table used to fail to render entirely
# for any inventory with incomplete ages.

test_that("age-class structure table renders with incomplete ages", {
  skip_if_not_installed("rmarkdown")
  skip_below_depth("full")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not on PATH")

  trees_na <- trees
  set.seed(1)
  idx <- sample(nrow(trees_na), floor(nrow(trees_na) / 4))
  trees_na$age_yr[idx]    <- NA_real_
  trees_na$age_class[idx] <- NA

  suppressWarnings(suppressMessages({
    out_na <- trees_na |> structure_table_age_class_main_stand() |>
      output_structure_table()
  }))
  expect_true(any(is.na(out_na$age_class)))

  pdf_path <- output_structure_table_pdf(out_na, tab_title = "Testbetrieb",
                                         output_dir = tempdir())
  expect_true(file.exists(pdf_path))
})


# ---------- the "total" column ------------------------------------------------
# The total column must come from the properly aggregated total / all_total
# elements, not from a row sum across the dbh classes. Summing is valid only for
# the additive quantities: a mean tree cannot be summed, n_plot would count an
# inventory point once per dbh class it occupies, each per-hectare figure has
# its own reference area, and a confidence interval is not additive either.
# Before this was wired up, n_plot and the per-hectare rows silently carried
# inflated sums (ex3: n_plot up to 6 too high, n_ha up to 21565 too high), while
# v_hub_m3_ha and conf_95_per were blanked although the correct values existed.

.chr_keys <- function(d) {
  if ("species_group" %in% names(d))
    d <- dplyr::mutate(d, species_group = trimws(format(species_group)))
  for (cl in intersect(c("age_class", "d_q_class"), names(d)))
    d[[cl]] <- as.character(d[[cl]])
  d
}

test_that("output_structure_table(): total column equals the class-wise aggregation", {
  skip_below_depth("full")
  for (nm in c("all", "mainstand")) {
    st <- if (nm == "all") st_all else st_ms
    ot <- if (nm == "all") out_all else out_ms

    # species blocks against `total`
    proper_vars <- setdiff(names(st$total), c("species_group", "age_class"))
    expect_true(all(c("n_plot", "conf_95_per", "d_q_cm") %in% proper_vars))

    proper <- st$total |>
      tidyr::pivot_longer(dplyr::all_of(proper_vars),
                          names_to = "variable", values_to = "proper") |>
      .chr_keys()

    cmp <- ot |>
      dplyr::filter(!is.na(species_group), as.character(age_class) != "Summe") |>
      .chr_keys() |>
      dplyr::select(dplyr::all_of(c("species_group", "age_class", "variable", "total"))) |>
      dplyr::inner_join(proper, by = c("species_group", "age_class", "variable"))

    expect_gt(nrow(cmp), 0)
    expect_equal(cmp$total, cmp$proper, tolerance = 1e-6,
                 label = paste("species-block total column:", nm))

    # cross-species block against `all_total`
    proper_vars_all <- setdiff(names(st$all_total), "age_class")
    proper_all <- st$all_total |>
      tidyr::pivot_longer(dplyr::all_of(proper_vars_all),
                          names_to = "variable", values_to = "proper") |>
      .chr_keys()

    cmp_all <- ot |>
      dplyr::filter(is.na(species_group), as.character(age_class) != "Summe") |>
      .chr_keys() |>
      dplyr::select(dplyr::all_of(c("age_class", "variable", "total"))) |>
      dplyr::inner_join(proper_all, by = c("age_class", "variable"))

    expect_gt(nrow(cmp_all), 0)
    expect_equal(cmp_all$total, cmp_all$proper, tolerance = 1e-6,
                 label = paste("cross-species total column:", nm))
  }
})

test_that("output_structure_table(): n_plot total is a plot count, not a sum", {
  # The sharpest case: an inventory point holding trees in several dbh classes
  # must be counted once, so the total can never exceed the row sum and is
  # strictly below it wherever a group spreads over more than one dbh class.
  dbh_cols <- setdiff(names(out_ms),
                      c("species_group", "age_class", "variable", "total"))

  np <- out_ms |>
    dplyr::filter(variable == "n_plot", !is.na(species_group),
                  as.character(age_class) != "Summe")

  row_sums <- rowSums(np[, dbh_cols], na.rm = TRUE)
  n_occupied <- rowSums(!is.na(np[, dbh_cols]))

  expect_true(all(np$total <= row_sums))
  expect_true(any(np$total < row_sums))
  # and it is never smaller than the widest single dbh class it occupies
  expect_true(all(np$total >= apply(np[, dbh_cols], 1, max, na.rm = TRUE)))
  expect_gt(sum(n_occupied > 1), 0)
})

test_that("output_structure_table(): area shares match the base table", {
  skip_below_depth("full")
  # per_species is not carried by `total` and keeps the row sum - which is
  # correct for it, because the shares of one class all refer to that class'
  # own total ideal area. Checked against the base table, which computes the
  # same share directly.
  suppressWarnings(suppressMessages({
    bt <- trees |> base_table_age_class_main_stand()
  }))

  base_ps <- bt$detail |>
    dplyr::select(dplyr::all_of(c("species_group", "age_class", "per_species"))) |>
    .chr_keys()

  struct_ps <- out_ms |>
    dplyr::filter(variable == "per_species", !is.na(species_group),
                  as.character(age_class) != "Summe") |>
    .chr_keys() |>
    dplyr::select(dplyr::all_of(c("species_group", "age_class", "total")))

  ps <- dplyr::inner_join(struct_ps, base_ps, by = c("species_group", "age_class"))
  expect_gt(nrow(ps), 0)
  expect_equal(ps$total, ps$per_species, tolerance = 1e-2)
})


# ---------- dbh-class axis ----------------------------------------------------

test_that("output_structure_table(): the dbh-class axis is complete and ascending", {
  dbh_levels <- levels(st_ms$detail$dbh_class)
  expect_gt(length(dbh_levels), 0)

  cols <- setdiff(names(out_ms),
                  c("species_group", "age_class", "variable", "total"))
  # every level is present, none extra, and in factor-level (ascending) order
  expect_equal(cols, dbh_levels)

  # ex3 does not populate every class, so this really does add empty columns
  observed <- unique(as.character(st_ms$detail$dbh_class))
  expect_true(length(setdiff(dbh_levels, observed)) > 0)
})


# ---------- trees without an age ----------------------------------------------

test_that("the k. A. class is carried by the pooling row group", {
  skip_below_depth("full")
  # Trees without an age form a class of their own (level NA). It must be a
  # class like any other: the pooling "Summe" row group has to contain it, or
  # the block totals would quietly omit those trees.
  trees_na <- trees
  set.seed(1)
  idx <- sample(nrow(trees_na), floor(nrow(trees_na) / 4))
  trees_na$age_yr[idx]    <- NA_real_
  trees_na$age_class[idx] <- NA

  suppressWarnings(suppressMessages({
    out_na <- trees_na |> structure_table_age_class_main_stand() |>
      output_structure_table()
  }))

  chk <- out_na |>
    dplyr::filter(variable == "v_hub_m3", !is.na(species_group)) |>
    dplyr::mutate(
      sg = trimws(format(species_group)),
      ac = dplyr::if_else(is.na(age_class), "kA", as.character(age_class))
    ) |>
    dplyr::group_by(sg) |>
    dplyr::summarise(
      classes = sum(total[ac != "Summe"], na.rm = TRUE),
      only_ka = sum(total[ac == "kA"], na.rm = TRUE),
      pooled  = sum(total[ac == "Summe"], na.rm = TRUE),
      .groups = "drop")

  # the injected NAs must actually have produced a k. A. class somewhere
  expect_gt(sum(chk$only_ka), 0)
  expect_equal(chk$classes, chk$pooled, tolerance = 1e-6)
})


# ---------- standwise data ----------------------------------------------------

test_that("structure_table_*() warns for data without concentric-circle structure", {
  skip_below_depth("full")
  suppressWarnings(suppressMessages({
    est_w <- data_ex6_standwise_fe_inventory |> pull_trees() |>
      height_complete_inventory()
    trees_w <- data_ex6_standwise_fe_inventory |>
      fill_heights_back(est_w) |> pull_trees() |> trees_add_essentials()
  }))

  # the marker: pull_trees() only fills dbh_trshld for fe_ccircle_spatial plots
  expect_true(all(is.na(trees_w$dbh_trshld)))
  expect_false(all(is.na(trees$dbh_trshld)))

  expect_warning(
    suppressMessages(structure_table_age_class_main_stand(trees_w)),
    "dbh_trshld"
  )
  # ... and a sample inventory does not trigger it
  expect_no_warning(
    suppressMessages(FeNEU:::.warn_if_not_sample_inventory(trees, "zzz()"))
  )
  # the table is still produced - this warns, it does not refuse
  suppressWarnings(suppressMessages({
    st_w <- structure_table_age_class_main_stand(trees_w)
  }))
  expect_named(st_w, c("detail", "total", "all_species", "all_total",
                       "species_total", "species_dbh_total",
                       "grand_total", "grand_dbh_total"))
})


# ---------- area share of the pooling rows ------------------------------------

test_that("output_structure_table(): the pooling rows carry an area share too", {
  dbh_cols <- setdiff(names(out_ms),
                      c("species_group", "age_class", "variable", "total"))

  pooled <- out_ms |>
    dplyr::filter(variable == "per_species", as.character(age_class) == "Summe")
  sp <- pooled |> dplyr::filter(!is.na(species_group))
  gr <- pooled |> dplyr::filter(is.na(species_group))

  # the row exists at all - it used to be missing from the pooling block
  expect_gt(nrow(sp), 0)
  expect_equal(nrow(gr), 1L)

  # a pooling row is a share of the whole table's ideal area, so the species
  # shares add up to 100 % and the cross-species row is 100 % by construction
  expect_equal(sum(sp$total), 100, tolerance = 1e-6)
  expect_equal(gr$total, 100, tolerance = 1e-6)

  # and the dbh-class cells of each pooling row still add up to its own total
  expect_equal(rowSums(sp[, dbh_cols], na.rm = TRUE), sp$total, tolerance = 1e-6)
  expect_equal(rowSums(gr[, dbh_cols], na.rm = TRUE), gr$total, tolerance = 1e-6)

  # the all-layers variant computes no areas, so it gets no share row either
  expect_false("per_species" %in% out_all$variable)
})

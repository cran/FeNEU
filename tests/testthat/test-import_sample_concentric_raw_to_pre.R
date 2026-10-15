# Tests for the concentric raw->pre converters (format 1 = FeNEU-native,
# format 2 = the multi-table style): the mandatory output_dir, the report
# return (ok/errors/warnings/paths), and collect-all validation. Input is built
# from the bundled raw example tables, as the functions' own @examples do.
#
# Depth (see helper-depth.R): what stays on CRAN is what can break through
# something outside this repo -- one full round trip per format, the
# decimal-mark tolerance that rides on readr, the species work that rides on
# ForestElementsR and stringdist, and the partial-position case that FER's
# fe_ccircle_spatial once refused. The rest are rules of ours about our own
# input formats; they break only when we change them, and then we are here.

.write_concentric_raw <- function() {
  td <- tempfile(); dir.create(td)
  readr::write_delim(data_ex1_sample_raw_trees,
                     file.path(td, "tree_layer.txt"),       delim = "\t")
  readr::write_delim(data_ex1_sample_raw_points,
                     file.path(td, "inventory_points.txt"), delim = "\t")
  readr::write_delim(data_ex1_sample_raw_smalltrees,
                     file.path(td, "ingrowth.txt"),         delim = "\t")
  td
}

.baysf_pre_files <- c("fdinvbhd.txt", "fdinvba.txt", "fdinvkrs.txt",
                      "fcbestku.txt", "fdvikrs.txt")

test_that("format1 converter writes the pre files and returns an ok report", {
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
    coord_sys            = "gk4"
    )
  ))

  expect_true(res$ok)
  expect_length(res$errors, 0)
  expect_true(all(file.exists(file.path(out_dir, .baysf_pre_files))))
  expect_setequal(basename(res$paths), .baysf_pre_files)
})

# Up to and including 3.0.2 this was wrong, and nothing here noticed: Format 1
# records the distance from the plot centre in metres, the BaySF pre format
# stores centimetres, and the converter only renamed the column. Every tree
# position came out at a hundredth of its true distance -- on ex1, whose
# outermost circle has a radius of 17.84 m, no tree sat further than 17 cm from
# the centre. The aggregates were unaffected, which is why it stayed hidden:
# the circle a tree belongs to is decided by its DBH, not by its position.
test_that("format1 writes pol_entf in centimetres, not in the metres it reads", {
  skip_below_depth("full")

  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path           = in_dir,
      output_dir           = out_dir,
      treelist_filename    = "tree_layer",
      inv_punkt_filename   = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess        = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))
  expect_true(res$ok)

  raw <- round(data_ex1_sample_raw_trees$distance, 6)
  raw <- raw[!is.na(raw)]
  pre <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"), delim = "\t",
                           show_col_types = FALSE)$pol_entf
  pre <- pre[!is.na(pre)]

  # Every distance written must be a hundred times one that was read. Stated
  # this way rather than row by row, because the converter legitimately drops
  # trees on the way (those beyond the innermost circle without a position).
  expect_true(length(pre) > 0)
  expect_true(all(round(pre / 100, 6) %in% raw))

  # ... and the consequence a user would see: make_object_lists() divides by
  # 100 again, so the positions must land back on the metres that were read,
  # inside the plot rather than in a heap on its centre.
  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(out_dir, coord_sys = "gk4")
  ))
  radii <- unlist(lapply(inv$plot, function(p) {
    if (is.null(p$tree_positions)) NULL else p$tree_positions$R
  }))
  radii <- radii[is.finite(radii)]

  expect_true(length(radii) > 0)
  expect_true(all(round(radii, 6) %in% raw))
  expect_lte(max(radii), 17.84)          # the outermost circle of ex1
  expect_gt(max(radii), 5)               # not shrunk by a factor of a hundred
})

test_that("format1 tree list is decimal-mark tolerant (comma vs. point)", {
  common <- list(
    inv_punkt_filename   = "inventory_points",
    small_trees_filename = "ingrowth",
    species_guess        = TRUE,
    dbh_cm_from          = c(0, 12, 30, 48),
    radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
  )

  # Reference run with the bundled (point-decimal) tree data.
  in_dot  <- .write_concentric_raw()
  out_dot <- tempfile(); dir.create(out_dot)
  suppressMessages(suppressWarnings(do.call(
    import_sample_concentric_format1_raw_to_pre,
    c(list(input_path = in_dot, output_dir = out_dot,
           treelist_filename = "tree_layer"), common))))

  # Same tree data, but every numeric measurement written German-style with a
  # comma decimal ("12.5" -> "12,5"); points/ingrowth copied unchanged.
  in_comma <- tempfile(); dir.create(in_comma)
  file.copy(file.path(in_dot, "inventory_points.txt"),
            file.path(in_comma, "inventory_points.txt"))
  file.copy(file.path(in_dot, "ingrowth.txt"),
            file.path(in_comma, "ingrowth.txt"))
  trees <- data_ex1_sample_raw_trees
  num   <- names(trees)[vapply(trees, is.numeric, logical(1))]
  trees[num] <- lapply(trees[num],
                       function(v) gsub(".", ",", as.character(v), fixed = TRUE))
  readr::write_delim(trees, file.path(in_comma, "tree_layer.txt"), delim = "\t")

  out_comma <- tempfile(); dir.create(out_comma)
  res <- suppressMessages(suppressWarnings(do.call(
    import_sample_concentric_format1_raw_to_pre,
    c(list(input_path = in_comma, output_dir = out_comma,
           treelist_filename = "tree_layer"), common))))

  expect_true(res$ok)
  # Comma and point input must yield the same numeric pre file (not NA).
  bhd_dot   <- readr::read_delim(file.path(out_dot,   "fdinvbhd.txt"),
                                 delim = "\t", show_col_types = FALSE)
  bhd_comma <- readr::read_delim(file.path(out_comma, "fdinvbhd.txt"),
                                 delim = "\t", show_col_types = FALSE)
  expect_equal(bhd_comma$bhd,   bhd_dot$bhd)
  expect_equal(bhd_comma$bhd_h, bhd_dot$bhd_h)
  expect_false(anyNA(bhd_comma$bhd))
})

test_that("format1 optional inputs are decimal-mark tolerant (points, small trees, circle def)", {
  skip_below_depth("full")
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  # Tree list unchanged (its comma tolerance is covered above).
  readr::write_delim(data_ex1_sample_raw_trees,
                     file.path(in_dir, "tree_layer.txt"), delim = "\t")

  # Point file (mandatory numerics + slope) written German-style.
  pts    <- data_ex1_sample_raw_points
  orig_x <- pts$coord_x
  pts$coord_x  <- paste0(as.character(orig_x), ",5")
  pts$rep_area <- "1,69"
  pts$neigung  <- "27,5"
  readr::write_delim(pts, file.path(in_dir, "inventory_points.txt"), delim = "\t")

  # Optional small-tree file: comma decimals in dbh (0), height and count.
  sm <- data_ex1_sample_raw_smalltrees
  sm$dbh    <- "0"
  sm$height <- "4,56"
  sm$count  <- "3"
  readr::write_delim(sm, file.path(in_dir, "ingrowth.txt"), delim = "\t")

  # Optional circle-definition file with comma-decimal radii.
  cdef <- data.frame(
    fe_ikl      = "ALL",
    dbh_cm_from = c(0, 12, 30, 48),
    radiuses_m  = c("2,82", "5,64", "11,28", "17,84"),
    stringsAsFactors = FALSE
  )
  readr::write_delim(cdef, file.path(in_dir, "circle_def.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path           = in_dir,
      output_dir           = out_dir,
      treelist_filename    = "tree_layer",
      inv_punkt_filename   = "inventory_points",
      small_trees_filename = "ingrowth",
      circle_def_filename  = "circle_def",
      species_guess        = TRUE,
      coord_sys            = "gk4"
    )
  ))
  expect_true(res$ok)

  krs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_false(anyNA(krs$repfl))
  expect_equal(krs$repfl,   rep(1.69, nrow(krs)))       # "1,69" -> 1.69
  expect_equal(krs$neigung, rep(27.5, nrow(krs)))       # "27,5" -> 27.5
  expect_equal(sort(krs$gauss_rw), sort(orig_x + 0.5))  # comma coordinate

  vik <- readr::read_delim(file.path(out_dir, "fdvikrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_equal(sort(round(vik$radius, 2)), c(2.82, 5.64, 11.28, 17.84))

  # Small-tree rows (dbh == 0) carried their comma height and count.
  bhd    <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"),
                              delim = "\t", show_col_types = FALSE)
  sm_out <- bhd[bhd$bhd == 0, ]
  expect_gt(nrow(sm_out), 0)
  expect_equal(sm_out$bhd_h,   rep(4.56, nrow(sm_out)))  # "4,56" -> 4.56, not 456
  expect_equal(sm_out$bhd_anz, rep(3,    nrow(sm_out)))
})

test_that("plot-mismatch diagnostics are written to the output folder, not the input", {
  skip_below_depth("full")
  # The input folder may be read-only; the output folder is the one the user
  # chose to be written to.
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # Give one tree a plot_id that has no inventory point.
  trees <- data_ex1_sample_raw_trees
  trees$plot_id[1] <- "no_such_plot"
  readr::write_delim(trees, file.path(in_dir, "tree_layer.txt"), delim = "\t")

  suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = TRUE,
      dbh_cm_from        = c(0, 12, 30, 48),
      radiuses_m         = c(2.82, 5.64, 11.28, 17.84),
      coord_sys          = "gk4"
    )
  ))

  diag_files <- c("tree_not_in_inv.txt", "inv_not_in_tree.txt")
  expect_true(all(file.exists(file.path(out_dir, diag_files))))
  expect_false(any(file.exists(file.path(in_dir, diag_files))))
})

.write_f1_numeric_species <- function(mutate_trees = identity) {
  in_dir <- tempfile(); dir.create(in_dir)
  st     <- species_abbreviations_bavrn_state
  trees  <- data_ex1_sample_raw_trees
  trees$species <- st$species_id[match(trees$species, st$abbreviation)]
  trees  <- trees[!is.na(trees$species), ]
  trees  <- mutate_trees(trees)
  readr::write_delim(trees, file.path(in_dir, "tree_layer.txt"), delim = "\t")
  readr::write_delim(data_ex1_sample_raw_points,
                     file.path(in_dir, "inventory_points.txt"), delim = "\t")
  in_dir
}

.f1_call <- function(in_dir, out_dir, dbh_cm_from = c(0, 12, 30, 48),
                     radiuses_m = c(2.82, 5.64, 11.28, 17.84)) {
  import_sample_concentric_format1_raw_to_pre(
    input_path = in_dir, output_dir = out_dir,
    treelist_filename = "tree_layer", inv_punkt_filename = "inventory_points",
    species_guess = FALSE, dbh_cm_from = dbh_cm_from, radiuses_m = radiuses_m,
    coord_sys = "gk4")
}

test_that("format1 imports without a tree_nr column (baumnummer is optional)", {
  skip_below_depth("full")
  # tree_nr is optional and never consumed (identification is by coordinates),
  # so a tree list without it must still import cleanly.
  in_dir <- .write_f1_numeric_species(function(trees) {
    trees$tree_nr    <- NULL
    trees$baumnummer <- NULL
    trees
  })
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(.f1_call(in_dir, out_dir)))
  expect_true(res$ok)
  expect_length(res$errors, 0)
  expect_true(all(file.exists(file.path(out_dir, .baysf_pre_files))))
})

test_that("format1 keeps innermost-circle trees that have no coordinates", {
  skip_below_depth("full")
  # The BaySF-style pre format needs no position for trees of the innermost
  # circle (they are tallied, not located). Stripping their coordinates must NOT
  # abort the import -- it used to, which wrongly rejected importable data.
  in_dir <- .write_f1_numeric_species(function(trees) {
    # The bundled main tree list holds only calipered trees (dbh >= 12), so make
    # one an innermost-circle tree and strip its position.
    trees$dbh[1]      <- 8
    trees$angle[1]    <- NA
    trees$distance[1] <- NA
    trees
  })
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(.f1_call(in_dir, out_dir)))
  expect_true(res$ok)                       # no longer a fatal error
  expect_length(res$errors, 0)

  bhd <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"),
                           delim = "\t", show_col_types = FALSE)
  # They are kept, with NA coordinates, and the pre stays readable.
  expect_gt(sum(bhd$bhd < 12 & is.na(bhd$pol_wink)), 0)
  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(out_dir)))
  expect_true(is_fe_inventory(inv))
})

test_that("format1 excludes larger trees without coordinates and says so loudly", {
  skip_below_depth("full")
  bad_plot <- NULL
  bad_dbh  <- NULL
  in_dir <- .write_f1_numeric_species(function(trees) {
    big <- which(as.numeric(trees$dbh) >= 12)[1]
    bad_plot <<- as.character(trees$plot_id[big])
    bad_dbh  <<- as.numeric(trees$dbh[big])
    trees$angle[big]    <- NA
    trees$distance[big] <- NA
    trees
  })
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(.f1_call(in_dir, out_dir)))
  expect_true(res$ok)                       # excluded, not fatal
  expect_true(any(grepl("no usable position", res$warnings)))
  expect_true(any(grepl("EXCLUDED", res$warnings)))
  expect_true(any(grepl(bad_plot, res$warnings)))
  # identifiable down to the tree: the excluded tree's dbh is named
  expect_true(any(grepl(sprintf("%.1f", bad_dbh), res$warnings, fixed = TRUE)))

  bhd <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_equal(sum(bhd$bhd >= 12 & is.na(bhd$pol_wink)), 0L)
})

test_that("the position threshold follows the circle definition, not a fixed 12 cm", {
  skip_below_depth("full")
  # Second circle starts at 20 cm here, so a 15 cm tree is an innermost-circle
  # tree and may legitimately have no coordinates.
  in_dir <- .write_f1_numeric_species(function(trees) {
    trees$dbh      <- 15
    trees$angle    <- NA
    trees$distance <- NA
    trees
  })
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(
    .f1_call(in_dir, out_dir, dbh_cm_from = c(0, 20), radiuses_m = c(5, 12))))
  expect_true(res$ok)
  expect_false(any(grepl("no usable position", res$warnings)))  # kept, not dropped

  bhd <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_gt(nrow(bhd), 0)

  # With the standard definition (second circle at 12 cm) the same trees DO need
  # a position and are excluded instead.
  out2 <- tempfile(); dir.create(out2)
  res2 <- suppressMessages(suppressWarnings(.f1_call(in_dir, out2)))
  expect_true(any(grepl("no usable position", res2$warnings)))
})

# output_dir used to default to input_path, i.e. the converters wrote into the
# folder the user's raw data came from. That is a CRAN policy violation and it
# guesses a target the caller never named, so the argument is mandatory now.
# The check fires before anything is read, so a bad call costs nothing.
test_that("the raw->pre converters demand an explicit output_dir", {
  in_dir <- .write_concentric_raw()

  expect_error(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      dbh_cm_from        = c(0, 12, 30, 48),
      radiuses_m         = c(2.82, 5.64, 11.28, 17.84),
      coord_sys          = "gk4"
    ),
    "output_dir"
  )
  expect_error(
    import_sample_concentric_format2_raw_to_pre(input_path = in_dir),
    "output_dir"
  )
  expect_error(
    import_standwise_relascope_format1_raw_to_pre(input_path = in_dir),
    "output_dir"
  )

  # Nothing was written into the raw folder on the way.
  expect_false(file.exists(file.path(in_dir, "fdinvbhd.txt")))
  expect_false(file.exists(file.path(in_dir, "WZP_Daten.txt")))

  # An empty or NA path is refused just as clearly as an absent one.
  expect_error(
    import_sample_concentric_format2_raw_to_pre(in_dir, output_dir = NA),
    "output_dir"
  )
  expect_error(
    import_sample_concentric_format2_raw_to_pre(in_dir, output_dir = ""),
    "output_dir"
  )
})

test_that("format1 returns a fail report (no throw) on missing required columns", {
  skip_below_depth("full")
  in_dir <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)
  writeLines(c("foo\tbar", "1\t2"), file.path(in_dir, "tree_layer.txt"))

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_false(res$ok)
  expect_gt(length(res$errors), 0)
  expect_null(res$paths)
})

test_that("format1 requires the slope point column (canonical or `neigung` alias)", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # Remove the slope column entirely (neither the canonical `slope` nor its
  # alias `neigung`) -> must fail in the plausi, not later in the BaySF build.
  pts <- data_ex1_sample_raw_points
  pts$neigung   <- NULL
  pts$neig_grad <- NULL
  readr::write_delim(pts, file.path(in_dir, "inventory_points.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_false(res$ok)
  expect_match(paste(res$errors, collapse = " "), "slope")
  expect_null(res$paths)
})

test_that("format1 accepts the canonical `slope` column name", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # Rename the BaySF-native `neigung` to the canonical English `slope`.
  pts <- data_ex1_sample_raw_points
  pts$slope   <- pts$neigung
  pts$neigung <- NULL
  readr::write_delim(pts, file.path(in_dir, "inventory_points.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_true(res$ok)
  # the pre file keeps the BaySF-native `neigung` column
  krs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_true("neigung" %in% names(krs))
})

test_that("format1 carries neigung into fdinvkrs", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()   # example points carry `neigung`
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_true(res$ok)
  krs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_true("neigung" %in% names(krs))
  expect_false(anyNA(krs$neigung))
})

test_that("format1 fails clearly on a non-numeric species column when species_guess = FALSE", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # Overwrite the species column with text abbreviations, then read WITHOUT
  # guessing: the converter must fail early with a clear message rather than
  # coercing every code to NA deep inside build_fdinvba().
  trees <- data_ex1_sample_raw_trees
  trees$species <- rep(c("Kie", "Bu"), length.out = nrow(trees))
  readr::write_delim(trees, file.path(in_dir, "tree_layer.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = FALSE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_false(res$ok)
  expect_match(paste(res$errors, collapse = " "), "species_guess")
  expect_null(res$paths)
})

test_that("format1 works end-to-end with real numeric species codes (guess = FALSE)", {
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  st <- species_abbreviations_bavrn_state
  trees <- data_ex1_sample_raw_trees
  trees$species <- st$species_id[match(trees$species, st$abbreviation)]
  trees <- trees[!is.na(trees$species), ]   # genuine numeric codes only
  sm <- data_ex1_sample_raw_smalltrees
  sm$species <- st$species_id[match(sm$species, st$abbreviation)]
  sm <- sm[!is.na(sm$species), ]
  readr::write_delim(trees, file.path(in_dir, "tree_layer.txt"), delim = "\t")
  readr::write_delim(sm,    file.path(in_dir, "ingrowth.txt"),   delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = FALSE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_true(res$ok)
  expect_null(res$species_mapping)     # no guessing happened
  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(out_dir)))
  expect_true(is_fe_inventory(inv))
})

test_that("format1 fails clearly on a missing species code when guess = FALSE", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  st <- species_abbreviations_bavrn_state
  trees <- data_ex1_sample_raw_trees
  trees$species <- st$species_id[match(trees$species, st$abbreviation)]
  trees <- trees[!is.na(trees$species), ]
  trees$species[1] <- NA                # one missing code
  readr::write_delim(trees, file.path(in_dir, "tree_layer.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      species_guess      = FALSE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_false(res$ok)
  expect_match(paste(res$errors, collapse = " "), "missing")
})

test_that("format1 reports species_mapping and flags fuzzy matches for review", {
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # "Bi" is only resolvable via the fuzzy tier (-> Bir), so it must land in the
  # species_mapping with tier == "review" and set needs_review = TRUE.
  trees <- data_ex1_sample_raw_trees
  trees$species[1] <- "Bi"
  readr::write_delim(trees, file.path(in_dir, "tree_layer.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_true(res$ok)
  expect_true(res$needs_review)
  expect_s3_class(res$species_mapping, "tbl_df")
  expect_true("Bi" %in% res$species_mapping$raw)
  expect_identical(res$species_mapping$tier[res$species_mapping$raw == "Bi"],
                   "review")
})

test_that("format1 warns in the report about plots with trees but no inventory point", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # Add a tree on a plot that has no inventory point -> non-fatal, but the
  # report must carry a warning (not just a console message).
  trees <- data_ex1_sample_raw_trees
  extra <- trees[1, ]
  extra$plot_id <- "9999999"
  readr::write_delim(rbind(trees, extra),
                     file.path(in_dir, "tree_layer.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_true(res$ok)  # non-fatal
  expect_match(paste(res$warnings, collapse = " "), "no inventory point")
})

test_that("format1 links inventory points to a per-class fe_ikl circle file", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # Give the inventory points a fe_ikl class-key column (all class 2 here).
  pts <- data_ex1_sample_raw_points
  pts$fe_ikl <- 2L
  readr::write_delim(pts, file.path(in_dir, "inventory_points.txt"), delim = "\t")

  # Per-class circle-definition file: class 1 and class 2.
  cdef <- data.frame(
    fe_ikl     = c(1, 1, 2, 2),
    bhd_cm_von = c(0, 12, 0, 12),
    radius_m   = c(2.82, 5.64, 3.09, 6.00)
  )
  readr::write_delim(cdef, file.path(in_dir, "Probekreisdefinition.txt"),
                     delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path          = in_dir,
      output_dir          = out_dir,
      treelist_filename   = "tree_layer",
      inv_punkt_filename  = "inventory_points",
      small_trees_filename = "ingrowth",
      circle_def_filename = "Probekreisdefinition",
      coord_sys           = "gk4",
      species_guess       = TRUE
    )
  ))

  expect_true(res$ok)
  fdvikrs <- readr::read_delim(file.path(out_dir, "fdvikrs.txt"),
                               delim = "\t", show_col_types = FALSE)
  expect_setequal(unique(fdvikrs$fe_ikl), c(1L, 2L))
  fdinvkrs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                                delim = "\t", show_col_types = FALSE)
  expect_true(all(fdinvkrs$fe_ikl == 2L))   # points linked to their class
})

test_that("format1 fails when a per-class file has no fe_ikl column in points", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # Strip the fe_ikl class-key column from the points so a per-class circle
  # definition has nothing to link to.
  pts <- data_ex1_sample_raw_points
  pts$fe_ikl <- NULL
  readr::write_delim(pts, file.path(in_dir, "inventory_points.txt"), delim = "\t")

  cdef <- data.frame(fe_ikl = c(1, 2), bhd_cm_von = c(0, 0),
                     radius_m = c(2.82, 3.09))
  readr::write_delim(cdef, file.path(in_dir, "Probekreisdefinition.txt"),
                     delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path          = in_dir,
      output_dir          = out_dir,
      treelist_filename   = "tree_layer",
      inv_punkt_filename  = "inventory_points",
      small_trees_filename = "ingrowth",
      circle_def_filename = "Probekreisdefinition",
      coord_sys           = "gk4",
      species_guess       = TRUE
    )
  ))
  expect_false(res$ok)
  expect_match(paste(res$errors, collapse = " "), "fe_ikl")
})

test_that("missing_checks collects multiple missing-value errors in one pass", {
  skip_below_depth("full")
  tl <- data.frame(
    plot_id      = c("a", NA),
    tree_nr      = c(NA, "2"),
    dbh          = c(10, 12),
    mortality    = c(0, 0),
    angle        = c(1, 1),
    distance     = c(1, 1),
    regeneration = c(FALSE, FALSE)
  )
  res <- missing_checks(tl)
  expect_length(res$errors, 2)          # plot_id AND tree_nr, both collected
  expect_identical(res$tree_list, tl)
})

test_that("main-tree records require a dbh; height is no substitute", {
  skip_below_depth("full")
  # regeneration == FALSE: a dbh of 0/NA is an error even when a height exists.
  tl <- data.frame(
    plot_id      = c("a", "b", "c"),
    tree_nr      = c("1", "2", "3"),
    dbh          = c(10, NA, 0),      # b: missing, c: zero
    height       = c(NA, 15, 20),     # heights present for b and c
    mortality    = c(0, 0, 0),
    angle        = c(1, 1, 1),
    distance     = c(1, 1, 1),
    regeneration = c(FALSE, FALSE, FALSE)
  )
  res <- missing_checks(tl)
  expect_length(res$errors, 1)
  err <- paste(res$errors, collapse = " ")
  expect_match(err, "valid dbh")
  expect_match(err, "b")              # the NA-dbh plot
  expect_match(err, "c")              # the zero-dbh plot
})

test_that("small-tree records accept dbh OR height, but need at least one", {
  skip_below_depth("full")
  # regeneration == TRUE: dbh 0 + a height is fine; neither is an error.
  tl <- data.frame(
    plot_id      = c("ok_dbh", "ok_height", "bad"),
    tree_nr      = c("1", "2", "3"),
    dbh          = c(5, 0, NA),       # ok_height has no dbh, bad has neither
    height       = c(NA, 3, NA),      # ok_dbh has no height, bad has neither
    mortality    = c(0, 0, 0),
    angle        = c(NA, NA, NA),     # small trees carry no azimuth/distance
    distance     = c(NA, NA, NA),
    regeneration = c(TRUE, TRUE, TRUE)
  )
  res <- missing_checks(tl)
  expect_length(res$errors, 1)
  err <- paste(res$errors, collapse = " ")
  expect_match(err, "no size")
  expect_match(err, "bad")
  expect_false(grepl("ok_dbh", err))
  expect_false(grepl("ok_height", err))
})

test_that("format1 accepts file names WITH the .txt extension (tolerant, no .txt.txt)", {
  skip_below_depth("full")
  in_dir  <- .write_concentric_raw()   # writes tree_layer.txt / inventory_points.txt / ingrowth.txt
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer.txt",        # passed WITH extension
      inv_punkt_filename = "inventory_points.txt",  # passed WITH extension
      small_trees_filename = "ingrowth.txt",
      species_guess      = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_true(res$ok)
  expect_length(res$errors, 0)
})


# --- Format 2 (multi-table / "Winter" export) --------------------------------

.write_format2_raw <- function() {
  td <- tempfile(); dir.create(td)
  readr::write_delim(data_ex2_sample_raw_trees,
                     file.path(td, "tree_layer.txt"),       delim = "\t")
  readr::write_delim(data_ex2_sample_raw_points,
                     file.path(td, "inventory_points.txt"), delim = "\t")
  td
}

.format2_call <- function(in_dir, out_dir) {
  import_sample_concentric_format2_raw_to_pre(
    input_path         = in_dir,
    output_dir         = out_dir,
    treelist_filename  = "tree_layer",
    inv_punkt_filename = "inventory_points",
    dbh_cm_from        = c(0, 12, 30),
    radiuses_m         = c(2., 6.31, 12.62),
    # ex2's native date_created is the ambiguous DD-MM-YY; supply the year.
    inventory_year     = 2025
  )
}

test_that("format2 converter writes the pre files and runs end-to-end", {
  in_dir  <- .write_format2_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))

  expect_true(res$ok)
  expect_length(res$errors, 0)
  expect_true(all(file.exists(file.path(out_dir, .baysf_pre_files))))

  # The pre files are readable by the pre-reader all the way to an fe_inventory.
  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(out_dir)))
  expect_true(is_fe_inventory(inv))
})

test_that("format2 emits neigung (not neig_grad) and defaults slope to 0 when absent", {
  skip_below_depth("full")
  in_dir  <- .write_format2_raw()   # ex2 points carry no slope column
  out_dir <- tempfile(); dir.create(out_dir)

  # The missing-slope default is surfaced as a report warning.
  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))
  expect_true(res$ok)
  expect_true(any(grepl("neigung|slope", res$warnings)))

  krs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_true("neigung" %in% names(krs))
  expect_false("neig_grad" %in% names(krs))   # bogus placeholder is gone
  expect_true(all(krs$neigung == 0))          # level-ground default
})

test_that("format2 honours a real neigung column (percent) when present", {
  skip_below_depth("full")
  in_dir  <- .write_format2_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  pts <- data_ex2_sample_raw_points
  pts$neigung <- 25            # 25 percent slope on every plot
  readr::write_delim(pts, file.path(in_dir, "inventory_points.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))
  expect_true(res$ok)

  krs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_true(all(krs$neigung == 25))   # carried through as percent
})

test_that("format2 reads a comma-decimal circle-definition file (shared machinery)", {
  skip_below_depth("full")
  in_dir  <- .write_format2_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # German comma decimals in the (result-relevant) circle geometry must survive.
  cdef <- data.frame(
    fe_ikl      = c("ALL", "ALL", "ALL"),
    dbh_cm_from = c("0", "12", "30"),
    radiuses_m  = c("2,0", "6,31", "12,62"),
    stringsAsFactors = FALSE
  )
  readr::write_delim(cdef, file.path(in_dir, "circle_def.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      input_path          = in_dir,
      output_dir          = out_dir,
      treelist_filename   = "tree_layer",
      inv_punkt_filename  = "inventory_points",
      circle_def_filename = "circle_def",
      inventory_year      = 2025
    )
  ))
  expect_true(res$ok)

  vik <- readr::read_delim(file.path(out_dir, "fdvikrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  # 6.31 / 12.62 parsed from the comma form, not corrupted to 631 / 1262.
  expect_equal(sort(round(vik$radius, 2)), c(2.00, 6.31, 12.62))
})

test_that("format2 remaps the Winter stand-layer codes to BaySF codes", {
  skip_below_depth("full")
  # Winter bestku -> BaySF best_ku:  1->1, 2->2, 3->2, 4->3, 5->5.
  # Deterministic check on the two codes that actually move (4->3, 3->2).
  remap <- c(`1` = 1, `2` = 2, `3` = 2, `4` = 3, `5` = 5)

  for (code in names(remap)) {
    in_dir  <- .write_format2_raw()
    out_dir <- tempfile(); dir.create(out_dir)

    trees <- data_ex2_sample_raw_trees
    trees$bestku <- as.integer(code)                       # all rows this code
    readr::write_delim(trees, file.path(in_dir, "tree_layer.txt"), delim = "\t")

    res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))
    expect_true(res$ok)

    ba <- readr::read_delim(file.path(out_dir, "fdinvba.txt"),
                            delim = "\t", show_col_types = FALSE)
    expect_true(all(ba$best_ku == remap[[code]]))          # remapped code only
  }
})

test_that("format2 uses the native Winter default file names", {
  skip_below_depth("full")
  td      <- tempfile(); dir.create(td)
  out_dir <- tempfile(); dir.create(out_dir)
  # Write under the DEFAULT names, then call WITHOUT the filename arguments.
  readr::write_delim(data_ex2_sample_raw_trees,
                     file.path(td, "02_probekreis.txt"),  delim = "\t")
  readr::write_delim(data_ex2_sample_raw_points,
                     file.path(td, "01_root_entity.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      input_path  = td,
      output_dir  = out_dir,
      dbh_cm_from = c(0, 12, 30),
      radiuses_m  = c(2., 6.31, 12.62),
      inventory_year = 2025
    )
  ))

  expect_true(res$ok)
  expect_true(all(file.exists(file.path(out_dir, .baysf_pre_files))))
})

test_that("format2 is decimal-mark tolerant across mandatory and optional inputs", {
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  # Tree list (mandatory): numeric measurements written German-style (comma
  # decimal). bhd < 12 cm keeps every tree on the innermost circle, so none is
  # dropped for missing coordinates -- the test stays about parsing.
  trees <- data_ex2_sample_raw_trees
  trees$bhd      <- "56,7"     # mm -> 5.67 cm
  trees$hoehe_1  <- "45,6"     # dm -> 4.56 m
  trees$pol_entf <- "123,4"    # cm (written through unchanged)
  readr::write_delim(trees, file.path(in_dir, "02_probekreis.txt"), delim = "\t")

  # Points: mandatory repfl + coordinate, AND the OPTIONAL neigung column, all
  # as comma decimals. Each coordinate keeps its distinct value plus 0.5, written
  # German-style (decimal comma) -- ex2 coordinates are lon/lat decimals.
  pts    <- data_ex2_sample_raw_points
  orig_x <- pts$lage_probekreismittelpunkt_x
  pts$lage_probekreismittelpunkt_x <-
    gsub(".", ",", as.character(orig_x + 0.5), fixed = TRUE)
  pts$repfl   <- "1,69"
  pts$neigung <- "27,5"        # optional slope column (percent)
  readr::write_delim(pts, file.path(in_dir, "01_root_entity.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      input_path  = in_dir, output_dir = out_dir,
      dbh_cm_from = c(0, 12, 30), radiuses_m = c(2., 6.31, 12.62),
      inventory_year = 2025)))
  expect_true(res$ok)

  bhd <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_gt(nrow(bhd), 0)
  expect_false(anyNA(bhd$bhd))
  expect_equal(bhd$bhd,      rep(5.67,  nrow(bhd)))  # "56,7" mm -> 5.67 cm, not 567
  expect_equal(bhd$bhd_h,    rep(4.56,  nrow(bhd)))  # "45,6" dm -> 4.56 m
  expect_equal(bhd$pol_entf, rep(123.4, nrow(bhd)))  # "123,4" cm carried through

  krs <- readr::read_delim(file.path(out_dir, "fdinvkrs.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_false(anyNA(krs$repfl))
  expect_equal(krs$repfl,   rep(1.69, nrow(krs)))    # mandatory, "1,69" -> 1.69
  expect_equal(krs$neigung, rep(27.5, nrow(krs)))    # optional, "27,5" -> 27.5

  # ex2 coordinates are lon/lat, so the pre stores them reprojected to UTM32
  # (gauss_rw/gauss_hw hold a metric CRS by convention); coord_sys says so.
  expect_true(all(krs$coord_sys == "utm32"))
  # the comma-decimal lon (orig_x + 0.5) survived parsing AND reprojection
  exp_e <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(
    data.frame(x = orig_x + 0.5,
               y = data_ex2_sample_raw_points$lage_probekreismittelpunkt_y),
    coords = c("x", "y"), crs = 4326), 25832))[, 1]
  expect_equal(sort(krs$gauss_rw), sort(exp_e), tolerance = 1e-3)
})

test_that("format2 reports trees excluded for missing/partial coordinates", {
  skip_below_depth("full")
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  trees <- data_ex2_sample_raw_trees
  # A tree outside the innermost circle (bhd >= 120 mm) keeping its distance but
  # losing every angle source: it cannot be placed and is excluded.
  big <- which(as.numeric(trees$bhd) >= 120)[1]
  trees$pol_wink[big]         <- NA
  trees$rueckazimut_gon[big]  <- NA
  trees$rueckazimut_grad[big] <- NA
  bad_plot <- as.character(trees$koord[big])
  n_raw    <- nrow(trees)

  readr::write_delim(trees, file.path(in_dir, "02_probekreis.txt"), delim = "\t")
  readr::write_delim(data_ex2_sample_raw_points,
                     file.path(in_dir, "01_root_entity.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      input_path = in_dir, output_dir = out_dir,
      dbh_cm_from = c(0, 12, 30), radiuses_m = c(2., 6.31, 12.62),
      inventory_year = 2025)))
  expect_true(res$ok)

  # The problem must be part of the RETURNED report, naming the affected
  # inventory point -- not console-only noise.
  expect_true(any(grepl("no usable position", res$warnings)))
  expect_true(any(grepl("EXCLUDED", res$warnings)))
  expect_true(any(grepl(bad_plot, res$warnings)))

  # And the tree really is excluded (no large tree with NA coords in the pre).
  bhd <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_equal(nrow(bhd), n_raw - 1L)
  expect_equal(sum(bhd$bhd >= 12 & is.na(bhd$pol_wink)), 0L)
})

test_that("format2 treats einheit_azimut as authoritative for the back azimuth", {
  skip_below_depth("full")
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  trees <- data_ex2_sample_raw_trees
  big   <- which(as.numeric(trees$bhd) >= 120)[1]
  bad_plot <- as.character(trees$koord[big])
  n_raw    <- nrow(trees)
  # Declare degrees (2) but populate only the GON column: the unit flag wins, so
  # the tree counts as having no usable azimuth instead of reading gon as
  # degrees (which would be off by 11 %).
  trees$pol_wink[big]         <- NA
  trees$rueckazimut_grad[big] <- NA
  trees$rueckazimut_gon[big]  <- 200
  trees$einheit_azimut[big]   <- 2

  readr::write_delim(trees, file.path(in_dir, "02_probekreis.txt"), delim = "\t")
  readr::write_delim(data_ex2_sample_raw_points,
                     file.path(in_dir, "01_root_entity.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      input_path = in_dir, output_dir = out_dir,
      dbh_cm_from = c(0, 12, 30), radiuses_m = c(2., 6.31, 12.62),
      inventory_year = 2025)))
  expect_true(res$ok)

  # Excluded and reported -- never silently written with a wrong-unit angle.
  expect_true(any(grepl("no usable position", res$warnings)))
  expect_true(any(grepl(bad_plot, res$warnings)))
  # Excluded entirely (count drops by exactly one), so it is never written with
  # the gon value misread as degrees. A value-based check on pol_wink would be
  # fragile: real ex2 trees legitimately carry a 180-degree back azimuth.
  bhd <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"),
                           delim = "\t", show_col_types = FALSE)
  expect_equal(nrow(bhd), n_raw - 1L)
})

test_that("format2 still requires repfl in the point file", {
  skip_below_depth("full")
  in_dir  <- .write_format2_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  # repfl (represented area) stays mandatory; date_created is now optional
  # (the year comes from it OR from inventory_year), so it is not tested here.
  pts <- data_ex2_sample_raw_points
  pts$repfl <- NULL
  readr::write_delim(pts, file.path(in_dir, "inventory_points.txt"), delim = "\t")

  # A fail REPORT (ok = FALSE), not a thrown error.
  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))
  expect_false(res$ok)
  expect_true(any(grepl("repfl", res$errors)))
  expect_false(any(file.exists(file.path(out_dir, .baysf_pre_files))))
})

test_that("format2 fails cleanly asking for inventory_year on the ambiguous native date", {
  skip_below_depth("full")
  in_dir  <- .write_format2_raw()   # ex2 date_created is the native DD-MM-YY
  out_dir <- tempfile(); dir.create(out_dir)

  # No inventory_year, and DD-MM-YY cannot be parsed unambiguously: a clean fail
  # report pointing to inventory_year, not a thrown error (the former blocker).
  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      input_path         = in_dir,
      output_dir         = out_dir,
      treelist_filename  = "tree_layer",
      inv_punkt_filename = "inventory_points",
      dbh_cm_from        = c(0, 12, 30),
      radiuses_m         = c(2., 6.31, 12.62)
    )))
  expect_false(res$ok)
  expect_true(any(grepl("inventory_year", res$errors)))
  expect_false(any(file.exists(file.path(out_dir, .baysf_pre_files))))
})

test_that("format2 fails cleanly on a tree file missing a required column", {
  skip_below_depth("full")
  in_dir  <- .write_format2_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  trees <- data_ex2_sample_raw_trees
  trees$bhd <- NULL                       # drop a mandatory measurement column
  readr::write_delim(trees, file.path(in_dir, "tree_layer.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))
  expect_false(res$ok)
  expect_true(any(grepl("bhd", res$errors)))
})

test_that("format2 rejects a multi-class circle definition when points have no fe_ikl", {
  skip_below_depth("full")
  # Format-2 source tables carry no fe_ikl link column, so only ONE (uniform)
  # circle definition is allowed for all points; a per-class file is rejected.
  in_dir  <- .write_format2_raw()
  out_dir <- tempfile(); dir.create(out_dir)

  cdef <- data.frame(
    fe_ikl      = c(1, 1, 2, 2),          # two circle classes, integer keys
    dbh_cm_from = c(0, 12, 0, 12),
    radiuses_m  = c(2.0, 6.31, 3.0, 7.0),
    stringsAsFactors = FALSE
  )
  readr::write_delim(cdef, file.path(in_dir, "circle_def.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format2_raw_to_pre(
      input_path          = in_dir,
      output_dir          = out_dir,
      treelist_filename   = "tree_layer",
      inv_punkt_filename  = "inventory_points",
      circle_def_filename = "circle_def",
      inventory_year      = 2025
    )
  ))
  expect_false(res$ok)
  expect_true(any(grepl("fe_ikl", res$errors)))
  expect_false(any(file.exists(file.path(out_dir, .baysf_pre_files))))
})


# ---- duplicate plot identifier is rejected early in BOTH formats ------------
# A plot identifier used twice (two distinct plots colliding on the same key)
# must fail at the raw->pre stage with a clear message, not silently collapse
# into one plot and surface later as the cryptic "dbh_lower is not unique"
# error from the pre reader. Guarded symmetrically in Format 1 (plot_id) and
# Format 2 (koord); see .plot_id_duplicate_msg.

test_that("format1 rejects a duplicated plot identifier and writes no pre files", {
  skip_below_depth("full")
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  pts_dup <- dplyr::bind_rows(data_ex1_sample_raw_points,
                              data_ex1_sample_raw_points[1, ])  # id now twice

  readr::write_delim(data_ex1_sample_raw_trees,
                     file.path(in_dir, "tree_layer.txt"),       delim = "\t")
  readr::write_delim(pts_dup,
                     file.path(in_dir, "inventory_points.txt"), delim = "\t")
  readr::write_delim(data_ex1_sample_raw_smalltrees,
                     file.path(in_dir, "ingrowth.txt"),         delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path           = in_dir,
      output_dir           = out_dir,
      treelist_filename    = "tree_layer",
      inv_punkt_filename   = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess        = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_false(res$ok)
  expect_true(any(grepl("inventory_points", res$errors)))
  expect_false(any(file.exists(file.path(out_dir, .baysf_pre_files))))
})

test_that("format2 rejects a duplicated plot identifier and writes no pre files", {
  skip_below_depth("full")
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  pts_dup <- dplyr::bind_rows(data_ex2_sample_raw_points,
                              data_ex2_sample_raw_points[1, ])  # koord now twice

  readr::write_delim(data_ex2_sample_raw_trees,
                     file.path(in_dir, "tree_layer.txt"),       delim = "\t")
  readr::write_delim(pts_dup,
                     file.path(in_dir, "inventory_points.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))

  expect_false(res$ok)
  expect_true(any(grepl("inventory_points", res$errors)))
  expect_false(any(file.exists(file.path(out_dir, .baysf_pre_files))))
})


# ---- an innermost tree with a partial position imports (no sf crash) --------
# A tree inside the innermost circle legitimately needs no position. If it
# nonetheless carries only PART of one -- a distance but no azimuth (or vice
# versa) -- make_object_lists() must treat it as unpositioned, not build a
# half-NA geometry that ForestElementsR/sf rejects with "missing values in
# coordinates not allowed". End-to-end guard through raw->pre->fe_inventory.

test_that("format2: innermost tree with distance but no azimuth still imports", {
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  trees <- data_ex2_sample_raw_trees
  # Make the first tree an innermost-circle tree (dbh < 12 cm -> bhd < 120 in
  # the raw's mm) that has a distance but no usable azimuth.
  trees$bhd[1]              <- 80     # 8 cm -> innermost circle
  trees$pol_entf[1]         <- 150    # distance present
  trees$pol_wink[1]         <- NA     # but no azimuth in any of the three
  trees$rueckazimut_gon[1]  <- NA     # azimuth source columns
  trees$rueckazimut_grad[1] <- NA

  readr::write_delim(trees,
                     file.path(in_dir, "tree_layer.txt"),       delim = "\t", na = "")
  readr::write_delim(data_ex2_sample_raw_points,
                     file.path(in_dir, "inventory_points.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))
  expect_true(res$ok)

  # The partial position is reported identifiably: the report names the case
  # and the affected tree by its dbh (8.0 cm), since ex2 carries no baumnummer.
  expect_true(any(grepl("partial position", res$warnings)))
  expect_true(any(grepl("8.0", res$warnings)))

  # ... and the pre stores that tree with BOTH position components nulled.
  bhd <- readr::read_delim(file.path(out_dir, "fdinvbhd.txt"),
                           delim = "\t", show_col_types = FALSE)
  innermost <- bhd[!is.na(bhd$bhd) & bhd$bhd < 12, ]
  expect_true(all(is.na(innermost$pol_entf) == is.na(innermost$pol_wink)))

  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(out_dir)))
  expect_true(is_fe_inventory(inv))
})


# ---- a living tree without a stand-layer code is excluded, identifiably ------
# A living tree whose stand-layer code is blank (empty layer/bestku) cannot be
# assigned to a layer. Rather than let it abort the whole import later with
# FER's "layer_key must be complete", it is excluded at the raw->pre stage and
# reported identifiably (Peter's decision: drop + loud warning, like a tree
# without a required position). Verified on Format 2 (Format 1 shares the
# helper).

test_that("format2: a living tree without a stand-layer code is excluded", {
  skip_below_depth("full")
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  trees   <- data_ex2_sample_raw_trees
  bad_bhd <- as.numeric(trees$bhd[1]) / 10   # bhd mm -> cm, as the report shows it
  trees$bestku[1] <- NA                       # living tree, blank stand layer

  readr::write_delim(trees,
                     file.path(in_dir, "tree_layer.txt"),       delim = "\t", na = "")
  readr::write_delim(data_ex2_sample_raw_points,
                     file.path(in_dir, "inventory_points.txt"), delim = "\t")

  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))
  expect_true(res$ok)                                   # excluded, not fatal
  expect_true(any(grepl("stand-layer code", res$warnings)))
  expect_true(any(grepl("EXCLUDED", res$warnings)))
  expect_true(any(grepl(sprintf("%.1f", bad_bhd), res$warnings, fixed = TRUE)))

  # the excluded tree is gone; the rest of the inventory still builds
  inv <- suppressMessages(suppressWarnings(
    import_sample_concentric_pre_to_fe_inventory(out_dir)))
  expect_true(is_fe_inventory(inv))
})


# ---- a plot without centre coordinates is rejected early in BOTH formats ----
# An inventory point with an empty centre coordinate cannot be placed and must
# fail at the raw->pre stage with a clear message, not travel into the pre and
# surface later as a cryptic sf "missing values in coordinates" error when the
# pre reader builds the plot geometry. Guarded symmetrically in Format 1
# (coord_x/coord_y) and Format 2 (lage_probekreismittelpunkt_x/y); see
# .missing_coords_msg.

test_that("format1 rejects a plot with an empty centre coordinate", {
  skip_below_depth("full")
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  pts <- data_ex1_sample_raw_points
  pts$coord_x[1] <- NA                                   # one plot loses its x

  readr::write_delim(data_ex1_sample_raw_trees,
                     file.path(in_dir, "tree_layer.txt"),       delim = "\t")
  readr::write_delim(pts,
                     file.path(in_dir, "inventory_points.txt"), delim = "\t", na = "")
  readr::write_delim(data_ex1_sample_raw_smalltrees,
                     file.path(in_dir, "ingrowth.txt"),         delim = "\t")

  res <- suppressMessages(suppressWarnings(
    import_sample_concentric_format1_raw_to_pre(
      input_path           = in_dir,
      output_dir           = out_dir,
      treelist_filename    = "tree_layer",
      inv_punkt_filename   = "inventory_points",
      small_trees_filename = "ingrowth",
      species_guess        = TRUE,
      dbh_cm_from          = c(0, 12, 30, 48),
      radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
      coord_sys            = "gk4"
    )
  ))

  expect_false(res$ok)
  expect_true(any(grepl("inventory_points", res$errors)))
  expect_false(any(file.exists(file.path(out_dir, .baysf_pre_files))))
})

test_that("format2 rejects a plot with an empty centre coordinate", {
  skip_below_depth("full")
  in_dir  <- tempfile(); dir.create(in_dir)
  out_dir <- tempfile(); dir.create(out_dir)

  pts <- data_ex2_sample_raw_points
  pts$lage_probekreismittelpunkt_y[1] <- NA              # one plot loses its y

  readr::write_delim(data_ex2_sample_raw_trees,
                     file.path(in_dir, "tree_layer.txt"),       delim = "\t")
  readr::write_delim(pts,
                     file.path(in_dir, "inventory_points.txt"), delim = "\t", na = "")

  res <- suppressMessages(suppressWarnings(.format2_call(in_dir, out_dir)))

  expect_false(res$ok)
  expect_true(any(grepl("inventory_points", res$errors)))
  expect_false(any(file.exists(file.path(out_dir, .baysf_pre_files))))
})

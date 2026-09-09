## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  message  = FALSE,
  warning  = FALSE
)

## ----setup, message = FALSE---------------------------------------------------
library(FeNEU)

## ----format1------------------------------------------------------------------
raw1 <- system.file("extdata", "data_ex1_sample_raw", package = "FeNEU")
dir(raw1)

pre1 <- file.path(tempdir(), "ex1_pre")

report1 <- import_sample_concentric_format1_raw_to_pre(
  input_path           = raw1,
  output_dir           = pre1,
  small_trees_filename = "Verjuengung",
  dbh_cm_from          = c(0, 12, 30, 48),
  radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
  coord_sys            = "gk4",
  species_guess        = TRUE
)

report1$ok
dir(pre1)

## ----format2------------------------------------------------------------------
raw2 <- system.file("extdata", "data_ex2_sample_raw", package = "FeNEU")
dir(raw2)

pre2 <- file.path(tempdir(), "ex2_pre")

report2 <- import_sample_concentric_format2_raw_to_pre(
  input_path     = raw2,
  output_dir     = pre2,
  dbh_cm_from    = c(0, 12, 30),
  radiuses_m     = c(2, 6.31, 12.62),
  inventory_year = 2025
)

report2$ok
dir(pre2)

## ----baysf_pre----------------------------------------------------------------
pre3 <- system.file("extdata", "data_ex3_sample_pre", package = "FeNEU")
dir(pre3)

inv3 <- import_sample_concentric_pre_to_fe_inventory(pre3)
inv3

## ----relascope_raw------------------------------------------------------------
raw6 <- system.file("extdata", "data_ex6_standwise_raw", package = "FeNEU")
dir(raw6)

pre6 <- file.path(tempdir(), "ex6_pre")

report6 <- import_standwise_relascope_format1_raw_to_pre(
  input_path = raw6,
  output_dir = pre6
)

report6$ok
dir(pre6)

## ----relascope_pre------------------------------------------------------------
pre7 <- system.file("extdata", "data_ex7_standwise_pre", package = "FeNEU")
dir(pre7)

inv7 <- import_standwise_relascope_pre_to_fe_inventory(pre7)
inv7

## ----read_and_convert---------------------------------------------------------
inv3b <- read_and_convert_data(pre3, inventory_type = "sample_concentric")

isTRUE(all.equal(inv3b, inv3))


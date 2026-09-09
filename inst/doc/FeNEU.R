## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 6.5,
  fig.height = 4.5,
  message = FALSE,
  warning = FALSE
)

## ----setup, message = FALSE---------------------------------------------------
library(FeNEU)           # Attach FeNEU
library(ForestElementsR) # FeNEU builds on it, see the note below
library(dplyr)           # used in the code examples below

options(fe_spec_lang = "eng") # display species names in English

## ----install_pkg, eval = FALSE------------------------------------------------
# install.packages("FeNEU")

## ----toolchain_diagnose, message = TRUE---------------------------------------
diagnose_pdf_toolchain()

## ----fe_inventory_object------------------------------------------------------
data_ex3_sample_fe_inventory |> is_fe_inventory()
data_ex3_sample_fe_inventory


## ----check_plots--------------------------------------------------------------
inv_unit <- data_ex3_sample_fe_inventory$plot[[1]]
class(inv_unit)
names(inv_unit)

## ----check_plots_trees--------------------------------------------------------
inv_unit$trees

## ----plot_plot, fig.alt = "Map of an inventory plot with concentric circles"----
plot(inv_unit, dbh_scale = 4)

## ----standwise_raw_to_pre-----------------------------------------------------
raw_dir <- system.file("extdata", "data_ex6_standwise_raw", package = "FeNEU")
dir(raw_dir)

pre_dir <- file.path(tempdir(), "ex6_pre")

report <- import_standwise_relascope_format1_raw_to_pre(
  input_path = raw_dir,
  output_dir = pre_dir
)

report$ok
dir(pre_dir)

## ----sample_raw_to_pre--------------------------------------------------------
raw_dir_ex1 <- system.file("extdata", "data_ex1_sample_raw", package = "FeNEU")
dir(raw_dir_ex1)

pre_dir_ex1 <- file.path(tempdir(), "ex1_pre")

report_ex1 <- import_sample_concentric_format1_raw_to_pre(
  input_path           = raw_dir_ex1,
  output_dir           = pre_dir_ex1,
  small_trees_filename = "Verjuengung", # optional
  dbh_cm_from          = c(0, 12, 30, 48),
  radiuses_m           = c(2.82, 5.64, 11.28, 17.84),
  coord_sys            = "gk4",
  species_guess        = TRUE
)

report_ex1$ok
dir(pre_dir_ex1)

## ----sample_raw_to_pre_warnings-----------------------------------------------
report_ex1$warnings

## ----pre_to_fe_standwise------------------------------------------------------
inv_ex6 <- import_standwise_relascope_pre_to_fe_inventory(pre_dir)

inv_ex6

## ----pre_to_fe_sample---------------------------------------------------------
pre_path <- system.file("extdata", "data_ex3_sample_pre", package = "FeNEU")
dir(pre_path)

fe_inv <- import_sample_concentric_pre_to_fe_inventory(pre_path)

fe_inv

## ----pre_to_fe_check----------------------------------------------------------
isTRUE(all.equal(fe_inv, data_ex3_sample_fe_inventory))

## ----read_and_convert---------------------------------------------------------
fe_inv_2 <- read_and_convert_data(pre_path, inventory_type = "sample_concentric")

isTRUE(all.equal(fe_inv_2, fe_inv))

## ----statusquo_prep-----------------------------------------------------------
# The h_q fallback warning is expected here: a few species x layer groups in the
# example data carry no measured height, so their mean height is modelled.
trees_with_heights <- suppressWarnings(
  data_ex3_sample_fe_inventory |>
    pull_trees() |>
    height_complete_inventory()
)

trees <- data_ex3_sample_fe_inventory |>
  fill_heights_back(trees_with_heights) |>
  pull_trees() |>
  trees_add_essentials(method = "BaySF")

trees

## ----statusquo_basetable------------------------------------------------------
bt <- base_table_age_class_main_stand(trees)

names(bt)

## ----statusquo_basetable_detail-----------------------------------------------
bt$detail |>
  select(species_group, age_class, v_hub_m3, n, d_q_cm, n_plot, conf_95_per)

## ----statusquo_basetable_out--------------------------------------------------
bt |>
  output_base_table() |>
  head(12)

## ----statusquo_basetable_dq---------------------------------------------------
d_classes <- back_table_dclass(trees)

base_table_d_q_class_main_stand(trees, dclass_back = d_classes) |>
  output_base_table() |>
  head(8)

## ----statusquo_pdf, eval = FALSE----------------------------------------------
# bt |>
#   output_base_table() |>
#   output_base_table_pdf(
#     tab_title  = "Example forest enterprise",
#     output_dir = tempdir(),
#     inventory  = data_ex3_sample_fe_inventory
#   )

## ----structuretable-----------------------------------------------------------
st <- structure_table_age_class_main_stand(trees)

st |>
  output_structure_table() |>
  head(10)

## ----increment_repsurv--------------------------------------------------------
inc <- inv_increment_repeated_survey(
  inv_1st       = data_ex3_previous_sample_fe_inventory,
  inv_2nd       = data_ex3_sample_fe_inventory,
  inv_1st_trees = data_ex3_previous_sample_trees_essentials,
  inv_2nd_trees = data_ex3_sample_trees_essentials,
  method        = "rep_classic",
  fill_option   = "standard",
  progress_bar  = FALSE
)

inc

## ----increment_repsurv_parts--------------------------------------------------
names(inc)

## ----increment_basetable------------------------------------------------------
increment_base_table_main_stand(inc, by_class = "age") |>
  output_increment_base_table() |>
  head(10)

## ----increment_overall--------------------------------------------------------
ovr <- output_increment_overall(inc)

ovr$table_combined |>
  select(species_group, is_total_row, iv_m3_ha_yr_st, iv_m3_ha_yr_sum)

## ----increment_gnfi3----------------------------------------------------------
inc_bwi3 <- inv_increment_gnfi3(
  inv       = data_ex3_sample_fe_inventory,
  inv_trees = data_ex3_sample_trees_essentials,
  dt        = 5
)

inc_bwi3

## ----increment_gnfi3_overview-------------------------------------------------
increment_base_table(inc_bwi3, by_class = "age") |>
  output_increment_overview_gnfi3()

## ----increment_ytables--------------------------------------------------------
inc_yt <- inv_increment_ytables(
  inv              = data_ex3_sample_fe_inventory,
  inv_trees        = data_ex3_sample_trees_essentials,
  ytable_selection = ytables_bavrn_state_var_1_feneu
)

inc_yt

inc_yt$overview

## ----tools_pull---------------------------------------------------------------
pull_centers(data_ex3_sample_fe_inventory)

## ----tools_meta---------------------------------------------------------------
inventory_meta(data_ex3_sample_fe_inventory)

## ----tools_period-------------------------------------------------------------
inv_period(data_ex3_previous_sample_fe_inventory, data_ex3_sample_fe_inventory)

## ----tools_plotsheet, eval = FALSE--------------------------------------------
# data_ex3_sample_fe_inventory$plot[[1]] |>
#   plot_info_sheet_pdf(output_dir = tempdir())

## ----tools_circledef, eval = FALSE--------------------------------------------
# generate_circle_definition(
#   output_dir  = tempdir(),
#   dbh_cm_from = c(0, 12, 30),
#   radiuses_m  = c(2.82, 5.64, 12.62)
# )

## ----tools_fieldtable---------------------------------------------------------
fe_species_get_field_table("bavrn_state") |> head(10)


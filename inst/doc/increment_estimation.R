## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  message  = FALSE,
  warning  = FALSE
)

## ----setup, message = FALSE---------------------------------------------------
library(FeNEU)
library(dplyr)

options(fe_spec_lang = "eng") # display species names in English

## ----prep_chain, eval = FALSE-------------------------------------------------
# twh <- inv |> pull_trees() |> height_complete_inventory()
# inv |> fill_heights_back(twh) |> pull_trees() |> trees_add_essentials(method = "BaySF")

## ----data---------------------------------------------------------------------
inv_1st   <- data_ex3_previous_sample_fe_inventory
inv_2nd   <- data_ex3_sample_fe_inventory
trees_1st <- data_ex3_previous_sample_trees_essentials
trees_2nd <- data_ex3_sample_trees_essentials

## ----repsurv------------------------------------------------------------------
inc <- inv_increment_repeated_survey(
  inv_1st       = inv_1st,
  inv_2nd       = inv_2nd,
  inv_1st_trees = trees_1st,
  inv_2nd_trees = trees_2nd,
  method        = "rep_classic",
  fill_option   = "standard",
  progress_bar  = FALSE
)

inc
names(inc)

## ----methods_compare----------------------------------------------------------
overall <- function(inc_m, m) {
  tc  <- output_increment_overall(inc_m)$table_combined
  tot <- tc[tc$is_total_row, ]
  tibble(method = m, iv_m3_ha_yr = tot$iv_m3_ha_yr_st)
}

# `inc` above is already the rep_classic run, so only rep_trans is new here
inc_trans <- inv_increment_repeated_survey(
  inv_1st, inv_2nd, trees_1st, trees_2nd,
  method = "rep_trans", fill_option = "standard", progress_bar = FALSE
)

bind_rows(overall(inc, "rep_classic"), overall(inc_trans, "rep_trans"))

## ----repsurv_basetable--------------------------------------------------------
increment_base_table_main_stand(inc, by_class = "age") |>
  output_increment_base_table() |>
  head(10)

## ----repsurv_overall----------------------------------------------------------
ovr <- output_increment_overall(inc)

ovr$table_combined |>
  select(species_group, is_total_row, iv_m3_ha_yr_st, iv_m3_ha_yr_sum)

## ----repsurv_ci---------------------------------------------------------------
ovr$table_matched |>
  filter(is_total_row) |>
  select(iv_m3_ha_yr_st, ci95_iv_m3_ha_yr_st)

## ----repsurv_pdf, eval = FALSE------------------------------------------------
# increment_base_table_main_stand(inc, by_class = "age") |>
#   output_increment_base_table() |>
#   output_increment_base_table_pdf(
#     tab_title  = "Example forest enterprise",
#     type       = "age",
#     output_dir = tempdir()
#   )
# 
# output_increment_overall_pdf(ovr, output_dir = tempdir())

## ----gnfi3--------------------------------------------------------------------
inc_bwi3 <- inv_increment_gnfi3(
  inv       = inv_2nd,
  inv_trees = trees_2nd,
  dt        = 5
)

inc_bwi3

## ----gnfi3_overview-----------------------------------------------------------
increment_base_table(inc_bwi3, by_class = "age") |>
  output_increment_overview_gnfi3()

## ----ytables------------------------------------------------------------------
inc_yt <- inv_increment_ytables(
  inv              = inv_2nd,
  inv_trees        = trees_2nd,
  ytable_selection = ytables_bavrn_state_var_1_feneu
)

inc_yt

## ----ytables_overview---------------------------------------------------------
inc_yt$overview

## ----ytables_basetable--------------------------------------------------------
increment_ytables_base_table(inc_yt, by_class = "age") |>
  output_increment_base_table() |>
  head(10)


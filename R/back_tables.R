# Copyright (C) 2026  Peter Biber, Astor Torano Caicoya
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Species Group Information Table by Mean Diameter Class
#'
#' Inventory tree data are grouped by species group and mean diameter class. A
#' data frame with group-wise aggregated inventory information is returned.
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @param dclass_back Data frame listing quadratic mean diameter classes per
#'   species group and layer on plot level. Typically the output of
#'   \code{\link{back_table_dclass}}.
#'
#' @param tree_filter Expression describing which trees to use in the function,
#'   internally passed to \code{\link[dplyr]{filter}}. Default is
#'   \code{!removal}, i.e. trees that died or were removed are not included.
#'
#' @return A list containing four data frames. The first one, called *detail*
#'   contains the aggregated information by
#'   species group and mean diameter class. The second data frame, *total* is
#'   aggregated on species level, i.e. one level higher. These values are mostly
#'   sums, but not in all cases. Confidence intervals must be calculated
#'   differently and also \code{n_plot} in total will not always be the sum of
#'   the values in *detail*, as there might be plots which contain two layers
#'   with different d_q classes. The third data frame, *all_species*, holds the
#'   cross-species aggregation by mean diameter class (the body of the "Summe"
#'   block of the base table (Basistabelle)); the fourth, *all_total*, is the grand total
#'   across all species and all classes.
#'
#' @section Attached attribute:
#'   The returned list carries a \code{tree_selection} attribute recording which
#'   tree cohort it represents: \code{"mainstand"} for the \code{*_main_stand()}
#'   functions, \code{"alllayers"} for the general functions called with their
#'   default \code{tree_filter}, or the raw \code{tree_filter} expression for a
#'   custom filter. \code{\link{output_base_table}} /
#'   \code{\link{output_structure_table}} carry this attribute through to their
#'   own output, where the \code{*_pdf()} renderers read it for a
#'   self-describing file name and the cohort disclaimer.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#'
#' # The prepared tree list is shipped with the package: it is this inventory
#' # put through pull_trees() -> height_complete_inventory() ->
#' # fill_heights_back() -> pull_trees() -> trees_add_essentials(). See
#' # ?data_ex3_trees_essentials for that chain spelled out.
#' trees_with_heights <- data_ex3_sample_trees_essentials
#'
#' peg_back_d <- back_table_dclass(trees_with_heights)
#'
#' base_table_d_q_class(trees_with_heights, dclass_back = peg_back_d)
#'
base_table_d_q_class <- function(inv_trees_plus,
                                 dclass_back,
                                 tree_filter = !.data$removal) {
  # Record which tree cohort this table represents. Store the raw tree_filter
  # expression so the PDF subtitle can show it verbatim; it is sanitised only
  # when it goes into a file name (see the *_pdf() functions).
  tree_sel <- if (missing(tree_filter)) "alllayers"
              else rlang::as_label(rlang::enquo(tree_filter))

  # Filter data as indicated
  work_dat <- dclass_back |>
    dplyr::inner_join(inv_trees_plus) |>
    dplyr::filter({{ tree_filter }}) |>
    dplyr::ungroup()

  # The actual aggregation, .sub_plot_areas must be FALSE in call of
  # single_tree_aggregation()
  # detail_dat is the actual goal table
  detail_dat <- work_dat |> single_tree_aggregation(
    .data$species_group, .data$d_q_class,
    .sub_plot_areas = FALSE,
    .standard_error = TRUE
  )

  # total_dat is a species wise summary of all variables that are not table keys
  # across all d_q_classes
  total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group,
      .sub_plot_areas = FALSE,
      .standard_error = TRUE
  )

  # all_species_dat is the cross-species aggregation per d_q class (the body of
  # the "Summe" block at the bottom of the base table (Basistabelle)). all_total_dat is the
  # grand total across all species AND all classes (the Summe block's total
  # cell). Both are aggregated with single_tree_aggregation() so that d_q and
  # h_q are proper weighted means, not naive sums.
  all_species_dat <- work_dat |>
    single_tree_aggregation(
      .data$d_q_class,
      .sub_plot_areas = FALSE,
      .standard_error = TRUE
    ) |>
    dplyr::ungroup()

  all_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp,
      .sub_plot_areas = FALSE,
      .standard_error = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  # Pack everything in a list and hand them back
  out <- list(
    detail      = detail_dat,
    total       = total_dat,
    all_species = all_species_dat,
    all_total   = all_total_dat
  )
  attr(out, "tree_selection") <- tree_sel
  out
}



#' Species Group Information Table by Age Class
#'
#' Inventory tree data are grouped by species group and age class. A data frame
#' with group-wise aggregated inventory information is returned.
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @param tree_filter Expression describing which trees to use in the function,
#'   internally passed to \code{\link[dplyr]{filter}}. Default is
#'   \code{!removal}, i.e. trees that died or were removed are not included.
#'
#' @return A list containing four data frames. The first one, called *detail*
#'   contains the aggregated  information by species group and
#'   age class. The second one, *total* is aggregated on species level, i.e. one
#'   level higher. These values are mostly sums, but not in all cases.
#'   Confidence intervals must be calculated differently and also
#'   \code{n_plot} in total will not always be the sum of the values in
#'   *detail*, as there might be plots which contain two layers with different
#'   d_q classes. The third data frame, *all_species*, holds the cross-species
#'   aggregation by age class (the body of the "Summe" block of the base table
#'   (Basistabelle)); the fourth, *all_total*, is the grand total across all species and
#'   all classes.
#'
#' @section Attached attribute:
#'   The returned list carries a \code{tree_selection} attribute recording which
#'   tree cohort it represents: \code{"mainstand"} for the \code{*_main_stand()}
#'   functions, \code{"alllayers"} for the general functions called with their
#'   default \code{tree_filter}, or the raw \code{tree_filter} expression for a
#'   custom filter. \code{\link{output_base_table}} /
#'   \code{\link{output_structure_table}} carry this attribute through to their
#'   own output, where the \code{*_pdf()} renderers read it for a
#'   self-describing file name and the cohort disclaimer.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#' # The prepared tree list ships with the package; see
#' # ?data_ex3_trees_essentials for the chain that builds it.
#' data_ex3_sample_trees_essentials |>
#'   base_table_age_class()
#'
#'
base_table_age_class <- function(inv_trees_plus,
                                 tree_filter = !.data$removal) {
  # Record which tree cohort this table represents. Store the raw tree_filter
  # expression so the PDF subtitle can show it verbatim; it is sanitised only
  # when it goes into a file name (see the *_pdf() functions).
  tree_sel <- if (missing(tree_filter)) "alllayers"
              else rlang::as_label(rlang::enquo(tree_filter))

  # Filter data as indicated
  work_dat <- inv_trees_plus |>
    dplyr::filter({{ tree_filter }}) |>
    dplyr::ungroup() # for safety's sake

  # The actual aggregation, .sub_plot_areas must be FALSE in call of
  # single_tree_aggregation()
  # detail_dat is the actual goal table
  detail_dat <- work_dat |> single_tree_aggregation(
    .data$species_group, .data$age_class,
    .sub_plot_areas = FALSE
  )

  # total_dat is a species wise summary of all variables that are not table keys
  # across all age_classes
  total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group,
      .sub_plot_areas = FALSE,
      .standard_error = TRUE
    )

  # all_species_dat is the cross-species aggregation per age class (the body of
  # the "Summe" block at the bottom of the base table (Basistabelle)). all_total_dat is the
  # grand total across all species AND all classes (the Summe block's total
  # cell). Both are aggregated with single_tree_aggregation() so that d_q and
  # h_q are proper weighted means, not naive sums.
  all_species_dat <- work_dat |>
    single_tree_aggregation(
      .data$age_class,
      .sub_plot_areas = FALSE,
      .standard_error = TRUE
    ) |>
    dplyr::ungroup()

  all_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp,
      .sub_plot_areas = FALSE,
      .standard_error = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  # Pack everything in a list and hand them back
  out <- list(
    detail      = detail_dat,
    total       = total_dat,
    all_species = all_species_dat,
    all_total   = all_total_dat
  )
  attr(out, "tree_selection") <- tree_sel
  out
}


#' Species Group Information Table by Mean Diameter Class and Single Tree
#' Diameter Class
#'
#' Inventory tree data are grouped by species group, mean diameter class, and
#' single tree diameter class. A data frame with group-wise aggregated inventory
#' information is returned.
#'
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @param dclass_back Data frame listing quadratic mean diameter classes per
#'   species group and layer on plot level. Typically the output of
#'   \code{\link{back_table_dclass}}.
#'
#' @param tree_filter Expression describing which trees to use in the function,
#'   internally passed to \code{\link[dplyr]{filter}}. Default is
#'   \code{!removal}, i.e. trees that died or were removed are not included.
#'
#' @return A list of six data frames. *detail* holds the aggregated information
#'   by species group, mean diameter class, and single tree diameter class.
#'   *total* aggregates across single tree diameter classes, providing correct
#'   weighted \code{d_q_cm}/\code{h_q_m} for the total column of each species
#'   row. *all_species* holds the cross-species aggregation by mean diameter
#'   class and single tree diameter class (body of the "Summe" block). *all_total*
#'   aggregates *all_species* across single tree diameter classes, providing
#'   correct weighted \code{d_q_cm}/\code{h_q_m} for the total column of the
#'   Summe block. *species_total* aggregates across mean diameter classes and
#'   single tree diameter classes (one level up from *total*), i.e. the
#'   species-wise grand total used for the trailing "Summe" row
#'   \code{\link{output_structure_table}} appends within each species' block.
#'   *species_dbh_total* aggregates across mean diameter classes only (keeping
#'   the single tree diameter class breakdown), providing the per-dbh_class
#'   values shown in that same "Summe" row.
#'
#' @section Attached attribute:
#'   The returned list carries a \code{tree_selection} attribute recording which
#'   tree cohort it represents: \code{"mainstand"} for the \code{*_main_stand()}
#'   functions, \code{"alllayers"} for the general functions called with their
#'   default \code{tree_filter}, or the raw \code{tree_filter} expression for a
#'   custom filter. \code{\link{output_base_table}} /
#'   \code{\link{output_structure_table}} carry this attribute through to their
#'   own output, where the \code{*_pdf()} renderers read it for a
#'   self-describing file name and the cohort disclaimer.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#' # The prepared tree list is shipped with the package: it is this inventory
#' # put through pull_trees() -> height_complete_inventory() ->
#' # fill_heights_back() -> pull_trees() -> trees_add_essentials(). See
#' # ?data_ex3_trees_essentials for that chain spelled out.
#' # Three inventory points are enough to show the shape of the result, and
#' # they keep the example quick; the function takes a whole tree list alike.
#' trees_with_heights <- data_ex3_sample_trees_essentials |>
#'   dplyr::filter(plot_id %in% unique(plot_id)[1:3])
#'
#' peg_back_d <- back_table_dclass(trees_with_heights)
#'
#' structure_table_d_q_class(trees_with_heights, dclass_back = peg_back_d)
#'
structure_table_d_q_class <- function(inv_trees_plus,
                                      dclass_back,
                                      tree_filter = !.data$removal) {
  # Structure tables say much less for data without concentric-circle
  # structure; warn, but produce the table (see .warn_if_not_sample_inventory).
  .warn_if_not_sample_inventory(inv_trees_plus, "structure_table_d_q_class()")

  # Record which tree cohort this table represents. Store the raw tree_filter
  # expression so the PDF subtitle can show it verbatim; it is sanitised only
  # when it goes into a file name (see the *_pdf() functions).
  tree_sel <- if (missing(tree_filter)) "alllayers"
              else rlang::as_label(rlang::enquo(tree_filter))

  # Filter data as indicated
  work_dat <- dclass_back |>
    dplyr::inner_join(inv_trees_plus) |>
    dplyr::filter({{ tree_filter }}) |>
    dplyr::ungroup() # for safety's sake

  # The actual aggregation, .sub_plot_areas must be FALSE
  detail_dat <- work_dat |> single_tree_aggregation(
    .data$species_group, .data$d_q_class, .data$dbh_class,
    .sub_plot_areas = FALSE
  )

  # all_species_dat is the cross-species aggregation by mean diameter class and
  # single tree diameter class (the body of the "Summe" block of the Strukturtabelle)
  all_species_dat <- work_dat |>
    single_tree_aggregation(
      .data$d_q_class, .data$dbh_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  # total_dat / all_total_dat: aggregation without dbh_class — provides correct
  # weighted d_q_cm / h_q_m for the "total" column in output_structure_table()
  total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$d_q_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  all_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$d_q_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  # species_total_dat: one level up from total_dat - aggregation by species
  # only (across all dq classes and dbh classes), for the per-species
  # "Summe" row appended by output_structure_table().
  species_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  # species_dbh_total_dat: species-wise aggregation across all dq classes, but
  # keeping the dbh_class breakdown - fills the per-dbh_class cells of each
  # species' "Summe" row (mirrors BaySF's own report, whose Summe line sums
  # every Bhd-Klasse across all Dg-Klassen rather than leaving those cells
  # blank).
  species_dbh_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$dbh_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  # grand_total_dat / grand_dbh_total_dat: one level up again from
  # species_total_dat/species_dbh_total_dat - species AND classes both
  # collapsed (grand_total_dat), or species and classes collapsed but the
  # dbh_class breakdown kept (grand_dbh_total_dat). These fill the trailing
  # "Summe" row output_structure_table() appends to the cross-species "Summe"
  # block itself (the true grand total across everything, as opposed to
  # all_total, which is still per class).
  grand_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  grand_dbh_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp, .data$dbh_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  out <- list(
    detail             = detail_dat,
    total              = total_dat,
    all_species        = all_species_dat,
    all_total          = all_total_dat,
    species_total      = species_total_dat,
    species_dbh_total  = species_dbh_total_dat,
    grand_total        = grand_total_dat,
    grand_dbh_total    = grand_dbh_total_dat
  )
  attr(out, "tree_selection") <- tree_sel
  out
}



#' Species Group Information Table by Age Class and Single Tree Diameter Class
#'
#' Inventory tree data are grouped by species group, age class, and single tree
#' diameter class. A data frame with group-wise aggregated inventory information
#' is returned.
#'
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @param tree_filter  Expression describing which trees to use in the function,
#'   internally passed to \code{\link[dplyr]{filter}}. Default is
#'   \code{!removal}, i.e. trees that died or were removed are not included.
#'
#' @return A list of six data frames. *detail* holds the aggregated information
#'   by species group, age class, and single tree diameter class. *total*
#'   aggregates across single tree diameter classes, providing correct weighted
#'   \code{d_q_cm}/\code{h_q_m} for the total column of each species row.
#'   *all_species* holds the cross-species aggregation by age class and single
#'   tree diameter class (body of the "Summe" block). *all_total* aggregates
#'   *all_species* across single tree diameter classes, providing correct
#'   weighted \code{d_q_cm}/\code{h_q_m} for the total column of the Summe
#'   block. *species_total* aggregates across age classes and single tree
#'   diameter classes (one level up from *total*), i.e. the species-wise
#'   grand total used for the trailing "Summe" row
#'   \code{\link{output_structure_table}} appends within each species' block.
#'   *species_dbh_total* aggregates across age classes only (keeping the
#'   single tree diameter class breakdown), providing the per-dbh_class
#'   values shown in that same "Summe" row.
#'
#' @section Attached attribute:
#'   The returned list carries a \code{tree_selection} attribute recording which
#'   tree cohort it represents: \code{"mainstand"} for the \code{*_main_stand()}
#'   functions, \code{"alllayers"} for the general functions called with their
#'   default \code{tree_filter}, or the raw \code{tree_filter} expression for a
#'   custom filter. \code{\link{output_base_table}} /
#'   \code{\link{output_structure_table}} carry this attribute through to their
#'   own output, where the \code{*_pdf()} renderers read it for a
#'   self-describing file name and the cohort disclaimer.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#'  # The prepared tree list ships with the package; see
#'  # ?data_ex3_trees_essentials for the chain that builds it. Three inventory
#'  # points are enough to show the shape of the result, and they keep the
#'  # example quick; the function takes a whole tree list alike.
#'  data_ex3_sample_trees_essentials |>
#'    dplyr::filter(plot_id %in% unique(plot_id)[1:3]) |>
#'    structure_table_age_class()
#'
structure_table_age_class <- function(inv_trees_plus,
                                      tree_filter = !.data$removal) {
  # Structure tables say much less for data without concentric-circle
  # structure; warn, but produce the table (see .warn_if_not_sample_inventory).
  .warn_if_not_sample_inventory(inv_trees_plus, "structure_table_age_class()")

  # Record which tree cohort this table represents. Store the raw tree_filter
  # expression so the PDF subtitle can show it verbatim; it is sanitised only
  # when it goes into a file name (see the *_pdf() functions).
  tree_sel <- if (missing(tree_filter)) "alllayers"
              else rlang::as_label(rlang::enquo(tree_filter))

  # Filter data as indicated
  work_dat <- inv_trees_plus |>
    dplyr::filter({{ tree_filter }}) |>
    dplyr::ungroup() # for safety's sake

  # The actual aggregation must be FALSE
  detail_dat <- work_dat |> single_tree_aggregation(
    .data$species_group, .data$age_class, .data$dbh_class,
    .sub_plot_areas = FALSE
  )

  # all_species_dat is the cross-species aggregation by age class and single
  # tree diameter class (the body of the "Summe" block of the structure table (Strukturtabelle))
  all_species_dat <- work_dat |>
    single_tree_aggregation(
      .data$age_class, .data$dbh_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  # total_dat / all_total_dat: aggregation without dbh_class — provides correct
  # weighted d_q_cm / h_q_m for the "total" column in output_structure_table()
  total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$age_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  all_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$age_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  # species_total_dat: one level up from total_dat - aggregation by species
  # only (across all age classes and dbh classes), for the per-species
  # "Summe" row appended by output_structure_table().
  species_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  # species_dbh_total_dat: species-wise aggregation across all age classes,
  # but keeping the dbh_class breakdown - fills the per-dbh_class cells of
  # each species' "Summe" row (mirrors BaySF's own report, whose Summe line
  # sums every Bhd-Klasse across all Altersklassen rather than leaving those
  # cells blank).
  species_dbh_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$dbh_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup()

  # grand_total_dat / grand_dbh_total_dat: one level up again from
  # species_total_dat/species_dbh_total_dat - species AND classes both
  # collapsed (grand_total_dat), or species and classes collapsed but the
  # dbh_class breakdown kept (grand_dbh_total_dat). These fill the trailing
  # "Summe" row output_structure_table() appends to the cross-species "Summe"
  # block itself (the true grand total across everything, as opposed to
  # all_total, which is still per class).
  grand_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  grand_dbh_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp, .data$dbh_class,
      .sub_plot_areas = FALSE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  out <- list(
    detail             = detail_dat,
    total              = total_dat,
    all_species        = all_species_dat,
    all_total          = all_total_dat,
    species_total      = species_total_dat,
    species_dbh_total  = species_dbh_total_dat,
    grand_total        = grand_total_dat,
    grand_dbh_total    = grand_dbh_total_dat
  )
  attr(out, "tree_selection") <- tree_sel
  out
}




#' Species Group Information Table by Mean Diameter Class and Single Tree
#' Diameter Class For the Main Stand Cohort
#'
#' Very similar aggregation table to the one produced by
#' \code{\link{base_table_d_q_class}}, but restricted to to the main stand
#' (layer_key == 1) only. It contains, however, estimates of areas covered by
#' species (sub-)groups and ha-related values o these cohorts. Due the
#' methodological dubiousness of species area calculations in mixed stands, this
#' is only done for the main stand (comparably to how this was handled in the
#' 3rd German National Forest Inventory).
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @param dclass_back Data frame listing quadratic mean diameter classes per
#'   species group and layer on plot level. Typically the output of
#'   \code{\link{back_table_dclass}}.
#'
#' @return A list containing four data frames, similar as the output from
#'   \code{\link{base_table_d_q_class}}, however restricted to the main stand,
#'   but with information about the areas covered by species (sub-groups).
#'   The first data frame in the list, called *detail* contains the aggregated
#'   information by species group and mean diameter class. The second one,
#'   *total* is aggregated on species level, i.e. one level higher. These values
#'   are mostly sums, but not in all cases. Confidence intervals must be
#'   calculated differently. The third data frame, *all_species*, holds the
#'   cross-species aggregation by mean diameter class (the body of the "Summe"
#'   block of the base table (Basistabelle)); the fourth, *all_total*, is the grand total
#'   across all species and all classes.
#'
#' @section Attached attribute:
#'   The returned list carries a \code{tree_selection} attribute recording which
#'   tree cohort it represents: \code{"mainstand"} for the \code{*_main_stand()}
#'   functions, \code{"alllayers"} for the general functions called with their
#'   default \code{tree_filter}, or the raw \code{tree_filter} expression for a
#'   custom filter. \code{\link{output_base_table}} /
#'   \code{\link{output_structure_table}} carry this attribute through to their
#'   own output, where the \code{*_pdf()} renderers read it for a
#'   self-describing file name and the cohort disclaimer.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#' # The prepared tree list is shipped with the package: it is this inventory
#' # put through pull_trees() -> height_complete_inventory() ->
#' # fill_heights_back() -> pull_trees() -> trees_add_essentials(). See
#' # ?data_ex3_trees_essentials for that chain spelled out.
#' trees_with_heights <- data_ex3_sample_trees_essentials
#'
#' peg_back_d <- back_table_dclass(trees_with_heights)
#'
#' base_table_d_q_class_main_stand(
#'   trees_with_heights, dclass_back = peg_back_d
#' )
#'
base_table_d_q_class_main_stand <- function(inv_trees_plus,
                                            dclass_back) {
  # Filter data as indicated
  work_dat <- dclass_back |>
    dplyr::inner_join(inv_trees_plus) |>
    dplyr::filter(.data$layer_key == 1, !.data$removal) |> # IMPORTANT: only main stand and standing
    dplyr::ungroup() # for safety's sake


  # Do the actual aggregation, .sub_plot_areas must be TRUE in this case
  # in call of single_tree_aggregation()
  # detail_dat is the actual goal table
  detail_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$d_q_class,
      .sub_plot_areas = TRUE,
      .standard_error = TRUE
    )

  # Calculate the total area per dq class
  detail_dat_sum <- detail_dat |>
    dplyr::group_by(.data$d_q_class) |>
    dplyr::summarise(tot_area_class = sum(.data$area_ha))

  # join the total areas an estimate species percentage per class
  detail_dat <- detail_dat |>
    dplyr::left_join(detail_dat_sum) |>
    dplyr::mutate(per_species = .data$area_ha / .data$tot_area_class * 100)


  # total_dat is a species wise summary of all variables that are not table keys
  # across all d_q_classes
  total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group,
      .sub_plot_areas = TRUE,
      .standard_error = TRUE
    )

  area_sum <- sum(total_dat$area_ha)

  total_dat <- total_dat |>
    dplyr::group_by(.data$species_group) |>
    dplyr::mutate(per_species = 100 * sum(.data$area_ha) / area_sum) |>
    dplyr::ungroup()

  # all_species_dat is the cross-species aggregation per d_q class (the body of
  # the "Summe" block). per_species here is the share of total area covered by
  # each class (the "%Anteil der Durchmesserklasse" of the Basistabelle Summe
  # block). all_total_dat is the grand total across all species and classes.
  all_species_dat <- work_dat |>
    single_tree_aggregation(
      .data$d_q_class,
      .sub_plot_areas = TRUE,
      .standard_error = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(per_species = 100 * .data$area_ha / sum(.data$area_ha))

  all_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp,
      .sub_plot_areas = TRUE,
      .standard_error = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp") |>
    dplyr::mutate(per_species = 100)

  # Pack everything in a list and hand them back
  out <- list(
    detail      = detail_dat,
    total       = total_dat,
    all_species = all_species_dat,
    all_total   = all_total_dat
  )
  attr(out, "tree_selection") <- "mainstand"
  out
}



#' Species Group Information Table by Age Class For the Main Stand Cohort
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @return A list containing four data frames, similar as the output from
#'   \code{\link{base_table_age_class}}, however restricted to the main stand,
#'   but with information about the areas covered by species (sub-groups).
#'   The first data frame in the list, called *detail* contains the aggregated
#'   information by species group and age class. The second one, *total* is
#'   aggregated on species level, i.e. one level higher. These values are mostly
#'   sums, but not in all cases. Confidence intervals must be calculated
#'   differently. The third data frame, *all_species*, holds the cross-species
#'   aggregation by age class (the body of the "Summe" block of the base table
#'   (Basistabelle)); the fourth, *all_total*, is the grand total across all species and
#'   all classes.
#'
#' @section Attached attribute:
#'   The returned list carries a \code{tree_selection} attribute recording which
#'   tree cohort it represents: \code{"mainstand"} for the \code{*_main_stand()}
#'   functions, \code{"alllayers"} for the general functions called with their
#'   default \code{tree_filter}, or the raw \code{tree_filter} expression for a
#'   custom filter. \code{\link{output_base_table}} /
#'   \code{\link{output_structure_table}} carry this attribute through to their
#'   own output, where the \code{*_pdf()} renderers read it for a
#'   self-describing file name and the cohort disclaimer.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#' # The prepared tree list ships with the package; see
#' # ?data_ex3_trees_essentials for the chain that builds it.
#' data_ex3_sample_trees_essentials |>
#'   base_table_age_class_main_stand()
#'
#'
base_table_age_class_main_stand <- function(inv_trees_plus) {

  # Filter data as indicated
  work_dat <- inv_trees_plus |>
    dplyr::filter(.data$layer_key == 1, !.data$removal) |> # IMPORTANT: only main stand and standing
    dplyr::ungroup() # for safety's sake

  # Do the actual aggregation, .sub_plot_areas must be TRUE in this case
  # in call of single_tree_aggregation()
  # detail_dat is the actual goal table
  detail_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$age_class,
      .sub_plot_areas = TRUE,
      .standard_error = TRUE
    )

  # Calculate the total area per age class
  detail_dat_sum <- detail_dat |>
    dplyr::group_by(.data$age_class) |>
    dplyr::summarise(tot_area_class = sum(.data$area_ha))

  # join the total areas and estimate species percentage per class
  detail_dat <- detail_dat |>
    dplyr::left_join(detail_dat_sum) |>
    dplyr::mutate(per_species = .data$area_ha / .data$tot_area_class * 100)


  # total_dat is a species wise summary of all variables that are not table keys
  # across all age_classes
  total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group,
      .sub_plot_areas = TRUE,
      .standard_error = TRUE
    )

  area_sum <- sum(total_dat$area_ha)

  total_dat <- total_dat |>
    dplyr::group_by(.data$species_group) |>
    dplyr::mutate(per_species = 100 * sum(.data$area_ha) / area_sum) |>
    dplyr::ungroup()

  # all_species_dat is the cross-species aggregation per age class (the body of
  # the "Summe" block). per_species here is the share of total area covered by
  # each class (the "%Anteil der Altersklasse" of the Basistabelle Summe block).
  # all_total_dat is the grand total across all species and classes.
  all_species_dat <- work_dat |>
    single_tree_aggregation(
      .data$age_class,
      .sub_plot_areas = TRUE,
      .standard_error = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(per_species = 100 * .data$area_ha / sum(.data$area_ha))

  all_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp,
      .sub_plot_areas = TRUE,
      .standard_error = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp") |>
    dplyr::mutate(per_species = 100)

  # Pack everything in a list and hand them back
  out <- list(
    detail      = detail_dat,
    total       = total_dat,
    all_species = all_species_dat,
    all_total   = all_total_dat
  )
  attr(out, "tree_selection") <- "mainstand"
  out
}


#' Species Group Information Table by Mean Diameter Class and Single Tree
#' Diameter Class
#'
#' Inventory tree data are grouped by species group, mean diameter class, and
#' single tree diameter class. A data frame with group-wise aggregated inventory
#' information is returned.
#'
#' Very similar aggregation table to the one produced by
#' \code{\link{structure_table_d_q_class}}, but restricted to to the main stand
#' (layer_key == 1) only. It contains, however, estimates of areas covered by
#' species (sub-)groups and ha-related values o these cohorts. Due the
#' methodological dubiousness of species area calculations in mixed stands, this
#' is only done for the main stand (comparably to how this was handled in the
#' 3rd German National Forest Inventory).
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @param dclass_back Data frame listing quadratic mean diameter classes per
#'   species group and layer on plot level. Typically the output of
#'   \code{\link{back_table_dclass}}.
#'
#' @return A list of six data frames. *detail* holds the aggregated information
#'   by species group, mean diameter class, and single tree diameter class,
#'   restricted to the main stand, including area estimates. *total* aggregates
#'   across single tree diameter classes, providing correct weighted
#'   \code{d_q_cm}/\code{h_q_m} for the total column of each species row.
#'   *all_species* holds the cross-species aggregation by mean diameter class
#'   and single tree diameter class (body of the "Summe" block). *all_total*
#'   aggregates *all_species* across single tree diameter classes, providing
#'   correct weighted \code{d_q_cm}/\code{h_q_m} for the total column of the
#'   Summe block. *species_total* aggregates across mean diameter classes and
#'   single tree diameter classes (one level up from *total*), i.e. the
#'   species-wise grand total used for the trailing "Summe" row
#'   \code{\link{output_structure_table}} appends within each species' block.
#'   *species_dbh_total* aggregates across mean diameter classes only (keeping
#'   the single tree diameter class breakdown), providing the per-dbh_class
#'   values shown in that same "Summe" row.
#'
#' @section Attached attribute:
#'   The returned list carries a \code{tree_selection} attribute recording which
#'   tree cohort it represents: \code{"mainstand"} for the \code{*_main_stand()}
#'   functions, \code{"alllayers"} for the general functions called with their
#'   default \code{tree_filter}, or the raw \code{tree_filter} expression for a
#'   custom filter. \code{\link{output_base_table}} /
#'   \code{\link{output_structure_table}} carry this attribute through to their
#'   own output, where the \code{*_pdf()} renderers read it for a
#'   self-describing file name and the cohort disclaimer.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#'
#' # The prepared tree list is shipped with the package: it is this inventory
#' # put through pull_trees() -> height_complete_inventory() ->
#' # fill_heights_back() -> pull_trees() -> trees_add_essentials(). See
#' # ?data_ex3_trees_essentials for that chain spelled out.
#' # Three inventory points are enough to show the shape of the result, and
#' # they keep the example quick; the function takes a whole tree list alike.
#' trees_with_heights <- data_ex3_sample_trees_essentials |>
#'   dplyr::filter(plot_id %in% unique(plot_id)[1:3])
#'
#' peg_back_d <- back_table_dclass(trees_with_heights)
#'
#' structure_table_d_q_class_main_stand(trees_with_heights, dclass_back = peg_back_d)
#'
structure_table_d_q_class_main_stand <- function(inv_trees_plus,
                                                 dclass_back) {
  # Structure tables say much less for data without concentric-circle
  # structure; warn, but produce the table (see .warn_if_not_sample_inventory).
  .warn_if_not_sample_inventory(inv_trees_plus,
                                "structure_table_d_q_class_main_stand()")

  # Filter data as indicated
  work_dat <- dclass_back |>
    dplyr::inner_join(inv_trees_plus) |>
    dplyr::filter(.data$layer_key == 1, !.data$removal) |> # IMPORTANT: only main stand and standing
    dplyr::ungroup() # for safety's sake

  # The actual aggregation, .sub_plot_areas must be TRUE
  detail_dat <- work_dat |> single_tree_aggregation(
    .data$species_group, .data$d_q_class, .data$dbh_class,
    .sub_plot_areas = TRUE
  )

  # Calculate the total area per d_q class (across all species and dbh classes)
  area_per_class <- detail_dat |>
    dplyr::group_by(.data$d_q_class) |>
    dplyr::summarise(tot_area_class = sum(.data$area_ha))

  # join the total areas and estimate species percentage per class
  detail_dat <- detail_dat |>
    dplyr::left_join(area_per_class) |>
    dplyr::mutate(per_species = .data$area_ha / .data$tot_area_class * 100)

  # all_species_dat is the cross-species aggregation (the body of the "Summe"
  # block of the structure table (Strukturtabelle)); per_species uses the same per-class total area so
  # the shares are consistent with the species rows.
  all_species_dat <- work_dat |>
    single_tree_aggregation(
      .data$d_q_class, .data$dbh_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(area_per_class) |>
    dplyr::mutate(per_species = .data$area_ha / .data$tot_area_class * 100)

  # total_dat / all_total_dat: aggregation without dbh_class — provides correct
  # weighted d_q_cm / h_q_m for the "total" column in output_structure_table()
  total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$d_q_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup()

  all_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$d_q_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup()

  # species_total_dat: one level up from total_dat - aggregation by species
  # only (across all dq classes and dbh classes), for the per-species
  # "Summe" row appended by output_structure_table().
  species_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup()

  # species_dbh_total_dat: species-wise aggregation across all dq classes, but
  # keeping the dbh_class breakdown - fills the per-dbh_class cells of each
  # species' "Summe" row (mirrors BaySF's own report, whose Summe line sums
  # every Bhd-Klasse across all Dg-Klassen rather than leaving those cells
  # blank).
  species_dbh_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$dbh_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup()

  # grand_total_dat / grand_dbh_total_dat: one level up again from
  # species_total_dat/species_dbh_total_dat - species AND classes both
  # collapsed (grand_total_dat), or species and classes collapsed but the
  # dbh_class breakdown kept (grand_dbh_total_dat). These fill the trailing
  # "Summe" row output_structure_table() appends to the cross-species "Summe"
  # block itself (the true grand total across everything, as opposed to
  # all_total, which is still per class).
  grand_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  grand_dbh_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp, .data$dbh_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  out <- list(
    detail             = detail_dat,
    total              = total_dat,
    all_species        = all_species_dat,
    all_total          = all_total_dat,
    species_total      = species_total_dat,
    species_dbh_total  = species_dbh_total_dat,
    grand_total        = grand_total_dat,
    grand_dbh_total    = grand_dbh_total_dat
  )
  attr(out, "tree_selection") <- "mainstand"
  out
}


#' Species Group Information Table by Age Class and Single Tree Diameter Class
#' for the Main Stand Cohort
#'
#' Inventory tree data are grouped by species group, age class, and single tree
#' diameter class. A data frame with group-wise aggregated inventory information
#' is returned.
#'
#' @param inv_trees_plus Data frame which covers trees from an inventory (each
#'   row is a tree), typically obtained from an \code{\link{fe_inventory}}
#'   object with \code{\link{pull_trees}} and pre-treated with
#'   \code{\link{trees_add_essentials}} (see example).
#'
#' @return A list of six data frames. *detail* holds the aggregated information
#'   by species group, age class, and single tree diameter class, restricted to
#'   the main stand, including area estimates. *total* aggregates across single
#'   tree diameter classes, providing correct weighted
#'   \code{d_q_cm}/\code{h_q_m} for the total column of each species row.
#'   *all_species* holds the cross-species aggregation by age class and single
#'   tree diameter class (body of the "Summe" block). *all_total* aggregates
#'   *all_species* across single tree diameter classes, providing correct
#'   weighted \code{d_q_cm}/\code{h_q_m} for the total column of the Summe
#'   block. *species_total* aggregates across age classes and single tree
#'   diameter classes (one level up from *total*), i.e. the species-wise
#'   grand total used for the trailing "Summe" row
#'   \code{\link{output_structure_table}} appends within each species' block.
#'   *species_dbh_total* aggregates across age classes only (keeping the
#'   single tree diameter class breakdown), providing the per-dbh_class
#'   values shown in that same "Summe" row.
#'
#' @section Attached attribute:
#'   The returned list carries a \code{tree_selection} attribute recording which
#'   tree cohort it represents: \code{"mainstand"} for the \code{*_main_stand()}
#'   functions, \code{"alllayers"} for the general functions called with their
#'   default \code{tree_filter}, or the raw \code{tree_filter} expression for a
#'   custom filter. \code{\link{output_base_table}} /
#'   \code{\link{output_structure_table}} carry this attribute through to their
#'   own output, where the \code{*_pdf()} renderers read it for a
#'   self-describing file name and the cohort disclaimer.
#'
#' @family inventory tables
#'
#' @export
#'
#' @examples
#'  # The prepared tree list ships with the package; see
#'  # ?data_ex3_trees_essentials for the chain that builds it. Three inventory
#'  # points are enough to show the shape of the result, and they keep the
#'  # example quick; the function takes a whole tree list alike.
#'  data_ex3_sample_trees_essentials |>
#'    dplyr::filter(plot_id %in% unique(plot_id)[1:3]) |>
#'    structure_table_age_class_main_stand()
#'
structure_table_age_class_main_stand <- function(inv_trees_plus) {
  # Structure tables say much less for data without concentric-circle
  # structure; warn, but produce the table (see .warn_if_not_sample_inventory).
  .warn_if_not_sample_inventory(inv_trees_plus,
                                "structure_table_age_class_main_stand()")

  # Filter data as indicated
  work_dat <- inv_trees_plus |>
    dplyr::filter(.data$layer_key == 1, !.data$removal) |> # IMPORTANT: only main stand and standing
    dplyr::ungroup() # for safety's sake

  # The actual aggregation, .sub_plot_areas must be TRUE
  detail_dat <- work_dat |> single_tree_aggregation(
    .data$species_group, .data$age_class, .data$dbh_class,
    .sub_plot_areas = TRUE
  )

  # Calculate the total area per age class (across all species and dbh classes)
  area_per_class <- detail_dat |>
    dplyr::group_by(.data$age_class) |>
    dplyr::summarise(tot_area_class = sum(.data$area_ha))

  # join the total areas and estimate species percentage per class
  detail_dat <- detail_dat |>
    dplyr::left_join(area_per_class) |>
    dplyr::mutate(per_species = .data$area_ha / .data$tot_area_class * 100)

  # all_species_dat is the cross-species aggregation (the body of the "Summe"
  # block of the structure table (Strukturtabelle)); per_species uses the same per-class total area so
  # the shares are consistent with the species rows.
  all_species_dat <- work_dat |>
    single_tree_aggregation(
      .data$age_class, .data$dbh_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(area_per_class) |>
    dplyr::mutate(per_species = .data$area_ha / .data$tot_area_class * 100)

  # total_dat / all_total_dat: aggregation without dbh_class — provides correct
  # weighted d_q_cm / h_q_m for the "total" column in output_structure_table()
  total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$age_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup()

  all_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$age_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup()

  # species_total_dat: one level up from total_dat - aggregation by species
  # only (across all age classes and dbh classes), for the per-species
  # "Summe" row appended by output_structure_table().
  species_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup()

  # species_dbh_total_dat: species-wise aggregation across all age classes,
  # but keeping the dbh_class breakdown - fills the per-dbh_class cells of
  # each species' "Summe" row (mirrors BaySF's own report, whose Summe line
  # sums every Bhd-Klasse across all Altersklassen rather than leaving those
  # cells blank).
  species_dbh_total_dat <- work_dat |>
    single_tree_aggregation(
      .data$species_group, .data$dbh_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup()

  # grand_total_dat / grand_dbh_total_dat: one level up again from
  # species_total_dat/species_dbh_total_dat - species AND classes both
  # collapsed (grand_total_dat), or species and classes collapsed but the
  # dbh_class breakdown kept (grand_dbh_total_dat). These fill the trailing
  # "Summe" row output_structure_table() appends to the cross-species "Summe"
  # block itself (the true grand total across everything, as opposed to
  # all_total, which is still per class).
  grand_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  grand_dbh_total_dat <- work_dat |>
    dplyr::mutate(summe_grp = 1L) |>
    single_tree_aggregation(
      .data$summe_grp, .data$dbh_class,
      .sub_plot_areas = TRUE
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"summe_grp")

  out <- list(
    detail             = detail_dat,
    total              = total_dat,
    all_species        = all_species_dat,
    all_total          = all_total_dat,
    species_total      = species_total_dat,
    species_dbh_total  = species_dbh_total_dat,
    grand_total        = grand_total_dat,
    grand_dbh_total    = grand_dbh_total_dat
  )
  attr(out, "tree_selection") <- "mainstand"
  out
}

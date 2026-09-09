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

#' FeNEU Example Data
#'
#' FeNEU ships a set of small example inventories used throughout the
#' documentation, examples and tests. They are fully anonymised and artificial:
#' plot coordinates are randomised and all identifiers are re-assigned, so the
#' data carry no reference to any real forest enterprise and must not be used to
#' draw conclusions about one. The sets are named `data_ex1_...` to
#' `data_ex7_...`. The data represent three different processing stages, and the
#' inventory types currently supported by \code{FeNEU}.
#'
#' @section Processing stages:
#' An inventory passes through up to three stages in FeNEU, and each example is
#' provided at whichever stages are useful for demonstration:
#' \describe{
#'   \item{**raw**}{The unprocessed input tables as delivered by the field
#'     recording software, before any standardisation. Bundled as text files
#'     under `system.file("extdata", ...)`, which is what the
#'     `*_raw_to_pre()` functions read. For ex1 and ex2, the same tables are
#'     additionally bundled as `data/` tibbles (`data_ex1_*`, `data_ex2_*`) so
#'     that they can be inspected without reading a file; the text files are
#'     generated from those tibbles (`data-raw/`), so the two cannot drift
#'     apart.}
#'   \item{**pre** (preprocessed)}{The intermediate, standardised form the
#'     `*_pre_to_fe_inventory()` step consumes -- BaySF-style tab-delimited text
#'     files for the concentric sample type, or a canonical `WZP_Daten.txt` for
#'     the stand-wise type. Bundled as folders under
#'     `system.file("extdata", ...)`.}
#'   \item{**fe_inventory**}{The ready-to-analyse [fe_inventory] object, bundled
#'     as a `data/` object.}
#' }
#'
#' @section Inventory types:
#' FeNEU supports sample and stand-wise inventories. Currently, we cover sample
#' inventories that follow a concentric circle design, and stand-wise
#' inventories with several angle-count samples per stand. In the overview table
#' below, we refer to these types as *sample*, and *standwise*.
#' \describe{
#'   \item{**sample** (concentric circles)}{Sample-point inventory on concentric
#'     circles. Two raw input formats exist for sample inventories, "Format 1"
#'     and "Format 2"; both feed the same preprocessed BaySF-style form. Scope
#'     is counted in inventory points.}
#'   \item{**standwise** (angle-count / relascope)}{Stand-wise inventory in which
#'     each stand is sampled with several relascope angle-count (Winkelzaehlprobe)
#'     points. Currently, only one raw data format is supported for standwise
#'     relascope inventories ("Format 1"); the raw and the preprocessed data
#'     follow the Silvarith-style. Scope is counted in stands; in the resulting
#'     object every angle-count point becomes one `fe_stand` row.}
#' }
#'
#' @section Sample circle geometry:
#' The raw formats of the sample inventories do not carry the geometry of the
#' concentric circles -- it is part of the inventory design, not of the field
#' records. It therefore has to be passed to the `*_raw_to_pre()` functions,
#' through the arguments `dbh_cm_from` (the lower dbh threshold of each circle,
#' in cm) and `radiuses_m` (the matching radii, in m). Getting it wrong scales
#' the representation factors and hence every per-hectare figure, so the
#' designs of the two raw example sets are stated here:
#'
#' | Set | `dbh_cm_from` | `radiuses_m` | resulting circle areas |
#' |-----|---------------|--------------|------------------------|
#' | ex1 | `c(0, 12, 30, 48)` | `c(2.82, 5.64, 11.28, 17.84)` | 25 / 100 / 400 / 1000 m² |
#' | ex2 | `c(0, 12, 30)` | `c(2, 6.31, 12.62)` | 12.6 / 125 / 500 m² |
#'
#' These are the designs of the inventories the anonymised examples were
#' derived from; use them when running the raw import of ex1 or ex2. The sets
#' bundled from the preprocessed stage onwards (ex3 to ex5) carry their circle
#' definition in the data and need no such argument.
#'
#' @section Overview:
#' | Set | Type | Stages bundled | Scope |
#' |-----|------|----------------|-------|
#' | ex1 | sample (concentric) | raw (Format 1) | 10 points |
#' | ex2 | sample (concentric) | raw (Format 2) | 10 points |
#' | ex3 | sample (concentric) | pre, fe_inventory (+ previous) | 10 points (previous 9) |
#' | ex4 | sample (concentric) | pre, fe_inventory (+ previous) | 100 points (previous 95) |
#' | ex5 | sample (concentric) | pre (simulated), fe_inventory (+ previous) | 10 points (previous 10) |
#' | ex6 | standwise (relascope) | raw (Format 1), pre, fe_inventory | 2 stands (17 points) |
#' | ex7 | standwise (relascope) | pre, fe_inventory | 2 stands (15 points) |
#'
#' @section Datasets in each set:
#' \describe{
#'   \item{ex1 -- sample, Format 1, raw}{extdata folder `data_ex1_sample_raw`
#'     (`Baumschicht.txt`, `Inv_punkt.txt`, `Verjuengung.txt`); objects
#'     `data_ex1_sample_raw_trees` (133 rows), `data_ex1_sample_raw_points`
#'     (10), `data_ex1_sample_raw_smalltrees` (43). Circle design:
#'     `dbh_cm_from = c(0, 12, 30, 48)`,
#'     `radiuses_m = c(2.82, 5.64, 11.28, 17.84)`. A dead-wood table exists for
#'     developers only (deadwood import is deferred); it is not shipped with the
#'     package.}
#'   \item{ex2 -- sample, Format 2, raw}{extdata folder `data_ex2_sample_raw`
#'     (`02_probekreis.txt`, `01_root_entity.txt`); objects
#'     `data_ex2_sample_raw_trees` (177 rows), `data_ex2_sample_raw_points`
#'     (10). Circle design: `dbh_cm_from = c(0, 12, 30)`,
#'     `radiuses_m = c(2, 6.31, 12.62)`. A small-tree and a dead-wood table are
#'     not bundled yet (see note).}
#'   \item{ex3 -- sample, concentric}{extdata folder `data_ex3_sample_pre`;
#'     objects `data_ex3_sample_fe_inventory` and
#'     `data_ex3_previous_sample_fe_inventory` (an earlier survey of the same
#'     points, for increment estimation).}
#'   \item{ex4 -- sample, concentric}{extdata folder `data_ex4_sample_pre`;
#'     objects `data_ex4_sample_fe_inventory` and
#'     `data_ex4_previous_sample_fe_inventory`. Same design as ex3, larger.}
#'   \item{ex5 -- sample, concentric, simulated}{extdata folders
#'     `data_ex5_sample_pre` and `data_ex5_previous_sample_pre`; objects
#'     `data_ex5_sample_fe_inventory` and `data_ex5_previous_sample_fe_inventory`.
#'     Carries permanent tree ids (`tree_id_style = "generic"`).}
#'   \item{ex6 -- standwise, relascope}{extdata folders `data_ex6_standwise_raw`
#'     and `data_ex6_standwise_pre`; object `data_ex6_standwise_fe_inventory`.}
#'   \item{ex7 -- standwise, relascope}{extdata folder `data_ex7_standwise_pre`;
#'     object `data_ex7_standwise_fe_inventory`.}
#' }
#'
#' @section Derived test fixtures:
#' A few further bundled objects are derived from ex3 and kept under their
#' historical names; they are not part of the ex1--ex7 scheme and exist for
#' regression testing: `inc_sub10_rep_classic`, `inc_sub10_rep_mean`,
#' `inc_sub10_rep_end`, `inc_sub10_rep_trans` (increment reference results),
#' `processed_pulled_sub10`, `processed_heights_nfi_sub10` and
#' `processed_heights_bav_sub10` (intermediate tree-preparation results).
#'
#' @section Note on the Format-2 example (ex2):
#' ex2 currently provides only the tree and inventory-point tables. A Format-2
#' dead-wood table (the "07" sheet) and a regeneration table are not bundled yet;
#' they can be added later.
#'
#' @section Stand register (Revierbuch):
#' Besides the raw -> pre -> fe_inventory chain, FeNEU offers a separate strand
#' that produces a **stand register** (*Revierbuch*) from the inventory input
#' data. It needs stand-level attributes that an `fe_inventory` object does not
#' carry. For stand-wise inventories these are read from a metadata file with
#' [import_standwise_relascope_format1_metadata()];
#' [import_standwise_relascope_format1_stand_register_bundle()] orchestrates
#' trees and metadata for [stand_register_pdf()]. Set **ex6** bundles such a
#' metadata file (`stand_metadata.txt`) as an example; the three processing
#' stages above never touch it.
#'
#' @seealso The exported import functions that move an inventory between the
#'   stages illustrated by these example data:
#'
#'   * raw to pre: [import_sample_concentric_format1_raw_to_pre()],
#'     [import_sample_concentric_format2_raw_to_pre()],
#'     [import_standwise_relascope_format1_raw_to_pre()]
#'   * pre to fe_inventory: [import_sample_concentric_pre_to_fe_inventory()],
#'     [import_standwise_relascope_pre_to_fe_inventory()]
#'   * the inventory-type-agnostic dispatcher for the pre to fe_inventory step:
#'     [read_and_convert_data()]
#'   * the object constructor itself: [fe_inventory()]
#'   * the stand register strand:
#'     [import_standwise_relascope_format1_metadata()],
#'     [import_standwise_relascope_format1_stand_register_bundle()],
#'     [stand_register_pdf()]
#' @name data_examples_overview
#' @family example data
#' @keywords datasets
NULL

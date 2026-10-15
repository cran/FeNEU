# FeNEU 3.0.3

* Tree positions read through a raw Format 1 import were a hundredth of their
  true distance from the plot centre. Format 1 records that distance in metres,
  the preprocessed BaySF form stores it in centimetres, and the converter passed
  the column through under its new name without converting; the reader then
  divided by a hundred, as it always has. On the bundled `ex1` example, whose
  outermost circle has a radius of 17.84 m, no tree came out further than 17 cm
  from its plot centre.

  Volume, basal area and stem numbers were never affected: which circle a tree
  belongs to is decided by its DBH, not by its position. What was wrong is
  everything that draws or measures the positions -- `plot()` on a plot object,
  the info sheets -- and the matching of trees between the two surveys of a
  repeated inventory, which pairs them by distance and azimuth. Format 2 and the
  preprocessed imports were never affected; they were already in centimetres.
  Data imported from raw Format 1 with an earlier version should be imported
  again.

* `plot_info_sheet_pdf()` now shows all trees of a plot, as `plot()` on the
  plot object does. It used to leave out two groups without saying so: every
  removed tree, in table and map, and every tree without a position, in the
  table as well, although a table needs no position. On the first plot of the
  bundled ex3 example that left one of 14 trees.

  Removed trees are now marked, in the table by the column *Ausg.* and on the
  map by a cross. The new argument `tree_filter` selects the trees for table
  and map alike, as in `plot()`; `tree_filter = !removal` gives the remaining
  stand only. A selection other than the default is printed on the sheet and
  added to the file name, so the sheets with and without removed trees can
  exist side by side; the default keeps the file name it always had.

  Trees without a position appear in the table with empty distance and
  azimuth, and a line on the sheet says how many there are. A further line
  gives the number of small trees, which are not listed individually. The
  table is sorted by tree number, `2_1` before `10_1`, and continues into the
  next column or onto a second page when a plot has more trees than one column
  holds; it used to run off the page. Its header now states the units:
  distance in m, azimuth in degrees, DBH in cm, age in years, height in m.
  The table has no vertical rules any more, and the map starts level with it.

* `plot_info_sheet_pdf()` has a new argument `subtitle`, a line below the
  heading. A `title` passed explicitly is now printed exactly as given; it used
  to be prefixed with "Stichprobenpunkt", which made it unusable for anything
  but a plot number. Without `title`, the heading is "Stichprobenpunkt"
  followed by the plot's `stand_id`, as before. The file name now always
  carries the `stand_id`: with a `title`, both are joined
  (`plot_info_ger_<title>_<stand_id>.pdf`), so that sheets sharing one title,
  such as the enterprise and the inventory year, no longer overwrite each
  other. Without `title`, the file name is unchanged.

* `plot_info_sheet_pdf()` needs the package ggrepel for the labels on its map,
  which was not declared. It is now listed under Suggests, and the function
  says so before rendering when it is missing, instead of failing halfway.
  `check_pdf_dependencies()` takes a new argument `extra_pkgs` for such
  function-specific packages.

* The test suite failed on systems with PROJ 9.9.0. One test rebuilds an
  example inventory and compares it with the packaged copy, and the coordinate
  reference system of every geometry is stored as text written by the PROJ
  installed at the time. PROJ 9.9.0 describes the same system, ETRS89 / UTM
  zone 32N, with a longer text than the version the example was built with. The
  test now compares which system is meant rather than how it is spelled. The
  package itself was not affected: it compares coordinate systems the way `sf`
  does, by meaning.

* The help page of `inv_inc_fill_gaps_gnfi3()` described its result wrongly in
  three places. It opened with "a list comprising five data frames" for a list
  of eight elements, named the two estimate columns without their `_ha`
  (`iv_hub_m3_ha_per_gnfi3`, `iv_hub_m3_ha_yr_gnfi3`), and called `FALSE` the
  default of `include_reptrees`, which is `TRUE`.

* The descriptions of the preprocessed files `fdinvbhd.txt` and `fdinvba.txt`
  are corrected in `vignette("data_formats")`. A row of `fdinvbhd.txt` is a
  tree record. A tree with a position is a single tree, identified by its
  position; only a record without position may stand for several trees of the
  same dimensions (`bhd_anz` greater than 1). A row of `fdinvba.txt` is not a
  tree at all but a cohort of a plot -- one species, stand age and stand layer
  -- to which the tree records refer through `lfd_satz`.

# FeNEU 3.0.2

* `output_dir` is now a required argument of
  `import_sample_concentric_format1_raw_to_pre()`,
  `import_sample_concentric_format2_raw_to_pre()` and
  `import_standwise_relascope_format1_raw_to_pre()`. It used to default to
  `input_path`, so the preprocessed files were written into the folder the raw
  data came from -- a directory the caller never named, and one that may well
  be read-only. A call without the argument now stops before anything is read,
  with a message saying what to pass. The argument keeps its position, so
  positional calls are unaffected.

* A coordinate-system declaration that spells one and the same system in more
  than one way is no longer mistaken for two. Point files carrying both `4326`
  and `EPSG:4326` do occur; they were refused as ambiguous and asked for a
  `coord_sys` argument the file did not need. Declarations are now resolved
  before they are compared. Two genuinely different systems, and values that
  cannot be read at all, are still refused.

* The ex1 example data carried the angle and the distance the wrong way round
  on plot 5, which put six trees between 92 and 326 m from their plot centre.
  Corrected in `data_ex1_sample_raw_trees` and in the matching file under
  `inst/extdata`. Only the tree positions change; every value the converter
  derives from these data is unchanged.

* The examples of the PDF output functions run again. They had been hidden
  behind `\dontrun{}`, where nothing executes them and nothing notices when
  they decay: one called an object that was never defined, another carried a
  literal `...` where a call belonged, and several ended in a viewer command
  that only works on macOS. They are under `\donttest{}` now, write to
  `tempdir()`, and are executed by `example()`.

* Several examples were computing the same thing repeatedly. One help page ran
  the same increment five times over, another ran a full increment chain on
  what is a page about data. They show what they are about, and no more.

* `pull_trees()` documents what it returns: the class, what a row is, and the
  two columns it adds rather than carries over from the inventory plots.

* The Description field cites Kangas and Maltamo (2006), a standard work on
  forest inventory methods.

# FeNEU 3.0.1

* The checks of version 3.0.0 took longer than is welcome on the CRAN check
  farm. Tests and examples now do far less repeated work, and the check time is
  down by about a third.
  
* `fill_heights_back()` no longer spreads small inventories over a cluster.
  Every worker receives a copy of the whole inventory, and below a few hundred
  plots that transfer costs far more than the work itself -- on a ten-plot
  inventory the sequential run is thirty times faster. Inventories large enough
  to benefit are unaffected.

* The two interim stages of the repeated-survey increment are shipped as data:
  `data_ex3_increment_matched` and `data_ex3_increment_fills`. Examples of the
  later steps start from them instead of rebuilding the chain that leads up to
  them, which also puts the subject of each example on its first line. See
  `?data_ex3_increment_interim`.

* The table examples start from `data_ex3_sample_trees_essentials` and
  therefore group species the BaySF way, as the increment documentation does
  throughout.

# FeNEU 3.0.0

* Initial CRAN submission.

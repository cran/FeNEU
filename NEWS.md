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

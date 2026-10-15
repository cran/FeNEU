# FeNEU - Evaluating Forest Inventory Data

<!-- badges: start -->
[![CRAN
version](https://www.r-pkg.org/badges/version/FeNEU)](https://CRAN.R-project.org/package=FeNEU)
[![CRAN
checks](https://badges.cranchecks.info/worst/FeNEU.svg)](https://cran.r-project.org/web/checks/check_results_FeNEU.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/FeNEU)](https://CRAN.R-project.org/package=FeNEU)
[![License: AGPL (\>=
3)](https://img.shields.io/badge/license-AGPL%20(%3E=%203)-blue.svg)](https://CRAN.R-project.org/web/licenses/AGPL-3)
<!-- badges: end -->

*FeNEU* evaluates data from forest inventories of different design. It
covers the assessment of the current state as well as the estimation of
increment. The package is not tied to a single inventory concept; the
combinations supported at present are sample inventories with concentric
circular plots and stand-wise inventories with angle count sampling. By
design, however, the package is open to additional inventory concepts and
data sources in the future.

*FeNEU* reads raw field data, runs plausibility checks, completes
missing tree heights, aggregates base and structure tables, estimates
increment, and renders the results as PDF reports. It builds on
[*ForestElementsR*](https://CRAN.R-project.org/package=ForestElementsR),
which provides the underlying data structures and forest mensurational
methods.

## Installation

```r
install.packages("FeNEU")
```

*ForestElementsR* is installed along with *FeNEU*.

### PDF output — extra setup

The `*_pdf()` functions (e.g. `output_base_table_pdf()`,
`output_structure_table_pdf()`, `output_increment_overall_pdf()`,
`plot_info_sheet_pdf()`) render their results through **rmarkdown** and
**kableExtra** on top of **pandoc** and a **LaTeX distribution**. The
latter two are external to R and are not installed by
`install.packages("FeNEU")`. They are set up once per machine:

1. **The R packages** that drive the rendering:

   ```r
   install.packages(c("rmarkdown", "kableExtra", "tinytex"))
   ```

   `plot_info_sheet_pdf()` additionally needs **ggrepel** for the tree
   labels on its map: `install.packages("ggrepel")`.

2. **Pandoc:** bundled with RStudio, so nothing to do there. Otherwise,
   installers are available from <https://pandoc.org/installing.html>; on
   macOS `brew install pandoc`, on Debian/Ubuntu `apt install pandoc`.

3. **LaTeX:** the shortest path for R users is **TinyTeX**:

   ```r
   tinytex::install_tinytex()
   ```

   This installs a small LaTeX distribution (about 300 MB) into the user's
   home directory. The LaTeX packages *FeNEU*'s templates require
   (siunitx, booktabs, longtable, makecell) are fetched on first render.
   An existing TeX Live or MiKTeX installation works as well.

`diagnose_pdf_toolchain()` reports which of these components are present
and how to install the missing ones; `setup_pdf_toolchain()` installs
TinyTeX if no LaTeX distribution is found (pandoc has to be installed by
hand). The help page `?pdf_dependencies` describes the requirements in
full.

If a component is missing, the `*_pdf()` functions stop with a message
naming it and pointing to `?pdf_dependencies`, rather than with a pandoc
or LaTeX backtrace.

## Documentation

* `vignette("FeNEU")` — the workflow from import to the evaluation
  tables and reports.
* `vignette("data_formats")` — the supported input formats, with a
  runnable import for each of them.
* `vignette("increment_estimation")` — the three ways of estimating
  increment and the methods behind them.
* `?FeNEU` — package overview.
* `?pdf_dependencies` — requirements for the PDF output.

## Acknowledgments

The authors would like to thank the Bavarian Ministry for Nutrition,
Agriculture, Forestry, and Tourism for funding the projects *FeNEU: Ein
innovatives Instrument für die forstliche Planung in Bayern (E062)* and
*Ertragskundliche Betreuung der langfristigen Versuche (W007)*. We also
wish to thank our colleagues at the Bayerische Staatsforsten (BaySF) for
their friendly cooperation. The development of *FeNEU* has benefited
especially from exchange with experienced practitioners. We sincerely
thank Sabine Andersen, Stefan Esser, Markus Fritsch, Christian Winter,
Hans Stark, and Daniel Kraus, representing a much larger group. Among
them is Rainer Dürr, author of the *Silvarith* software, who sadly
died before the release of *FeNEU*.

## License

AGPL (>= 3).

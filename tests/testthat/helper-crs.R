# Re-derive every coordinate reference system inside an object from the
# running PROJ installation.
#
# An sf geometry carries its CRS as WKT text, and that text is written from
# the PROJ database installed when the geometry is built. The packaged example
# objects were built against one PROJ, the checks run against whatever PROJ
# each machine has. When the EPSG database grows -- PROJ 9.9.0 lists about
# fifty more realisations in the ETRS89 ensemble than 9.5.1 -- the same
# EPSG:25832 is spelled differently, and a comparison of a freshly built
# object against a packaged one fails on text that means the same thing. That
# is what CRAN's checks showed for FeNEU 3.0.2 in October 2026.
#
# Each CRS is rebuilt from its `input` (e.g. "EPSG:25832"), the part that says
# which system is meant. Applied to both sides of a comparison, the test then
# asks whether both name the same system and carry the same data, and no
# longer which PROJ wrote the text. Attributes and classes are kept, so the
# objects stay what they were; the recursion works on the unclassed list, so
# no `[<-` method of sf, tibble or ForestElementsR gets a say in the rebuild.
restamp_crs <- function(x) {
  if (inherits(x, "sfc")) {
    crs <- attr(x, "crs")
    if (!is.na(crs)) attr(x, "crs") <- sf::st_crs(crs$input)
    return(x)
  }
  if (is.list(x)) {
    a <- attributes(x)
    x <- lapply(unclass(x), restamp_crs)
    attributes(x) <- a
  }
  x
}

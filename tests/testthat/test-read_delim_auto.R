test_that(".detect_file_encoding distinguishes UTF-8, Latin-1 and BOM", {
  td <- tempfile(); dir.create(td)
  header <- "baumart\thoehe"
  body   <- "Lärche\t23"          # "Lärche" with ä

  # UTF-8 (no BOM)
  f_utf8 <- file.path(td, "utf8.txt")
  writeLines(c(header, body), f_utf8, useBytes = FALSE)
  con <- file(f_utf8, encoding = "UTF-8"); writeLines(c(header, body), con); close(con)

  # Latin-1
  f_lat <- file.path(td, "latin1.txt")
  con <- file(f_lat, encoding = "latin1"); writeLines(c(header, body), con); close(con)

  # UTF-8 with BOM
  f_bom <- file.path(td, "bom.txt")
  con <- file(f_bom, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(enc2utf8(paste0(header, "\n", body, "\n"))), con)
  close(con)

  expect_identical(.detect_file_encoding(f_utf8), "UTF-8")
  expect_identical(.detect_file_encoding(f_lat),  "windows-1252")
  expect_identical(.detect_file_encoding(f_bom),  "UTF-8")
})

test_that(".read_delim_auto reads umlauts correctly regardless of encoding", {
  td <- tempfile(); dir.create(td)
  header <- "baumart\thoehe"
  body   <- "Lärche\t23"

  f_utf8 <- file.path(td, "utf8.txt")
  con <- file(f_utf8, encoding = "UTF-8"); writeLines(c(header, body), con); close(con)
  f_lat <- file.path(td, "latin1.txt")
  con <- file(f_lat, encoding = "latin1"); writeLines(c(header, body), con); close(con)

  d_utf8 <- .read_delim_auto(f_utf8)
  d_lat  <- .read_delim_auto(f_lat)

  expect_identical(d_utf8$baumart[1], "Lärche")
  expect_identical(d_lat$baumart[1],  "Lärche")
  expect_identical(d_utf8$hoehe[1], 23)
})

test_that(".read_delim_auto honours an explicit encoding override and decimal_mark", {
  td <- tempfile(); dir.create(td)
  f <- file.path(td, "comma.txt")
  con <- file(f, encoding = "latin1")
  writeLines(c("baumart\thoehe", "Kiefer\t23,5"), con); close(con)

  d <- .read_delim_auto(f, encoding = "windows-1252", decimal_mark = ",")
  expect_identical(d$hoehe[1], 23.5)
})

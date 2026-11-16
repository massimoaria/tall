test_that("isUTF8File tells UTF-8 from Latin-1 and UTF-16", {
  env <- app_env("tallTextIO.R")

  utf8 <- tempfile(fileext = ".txt")
  writeBin(charToRaw(enc2utf8("A \u00a37m musical")), utf8)
  expect_true(env$isUTF8File(utf8))

  latin1 <- tempfile(fileext = ".txt")
  writeBin(iconv("A \u00a37m musical", "UTF-8", "latin1", toRaw = TRUE)[[1]], latin1)
  expect_false(env$isUTF8File(latin1))

  utf16 <- tempfile(fileext = ".txt")
  writeBin(iconv("plain ascii", "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]], utf16)
  expect_false(env$isUTF8File(utf16))
})

test_that("read_files keeps the pound sign in UTF-8 and Latin-1 .txt files", {
  skip_if_not_installed("readtext")
  env <- app_env("tallTextIO.R")
  dir <- tempfile()
  dir.create(dir)

  ## The BBC entertainment_003.txt shape: mostly ASCII, one "\u00a3" in UTF-8 --
  ## the case readtext's detector labels ISO-8859-1.
  txt <- paste(
    "Frank Capra's 1946 movie starring James Stewart, is being turned into a",
    "\u00a37m musical by producer Jon Thoday. He is working with the show's",
    "co-writer on the adaptation of the classic film."
  )
  f_utf8 <- file.path(dir, "a_utf8.txt")
  writeBin(charToRaw(enc2utf8(txt)), f_utf8)
  f_latin1 <- file.path(dir, "z_latin1.txt")
  writeBin(iconv(txt, "UTF-8", "latin1", toRaw = TRUE)[[1]], f_latin1)

  ## Upload order differs from name order on purpose: readtext() returns its
  ## rows sorted by name, and encodings detected on that batch used to be
  ## paired with the files in upload order.
  files <- data.frame(
    name = basename(c(f_latin1, f_utf8)),
    datapath = c(f_latin1, f_utf8)
  )
  out <- suppressMessages(env$read_files(files, ext = "txt", subfolder = FALSE))

  expect_equal(nrow(out), 2)
  expect_false(any(grepl("\u00c2", out$text)))
  expect_true(all(grepl("\u00a37m", out$text)))
})

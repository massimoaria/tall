test_that("readTermList reads a .csv term list", {
  env <- app_env("tallTextIO.R")
  path <- tempfile(fileext = ".csv")
  writeLines(c("keyword", "machine learning", "text mining"), path)

  x <- env$readTermList(path, "terms.csv")

  expect_equal(nrow(x), 2)
  expect_equal(x[[1]], c("machine learning", "text mining"))
})

test_that("readTermList takes the extension from the original name", {
  env <- app_env("tallTextIO.R")
  path <- tempfile() # Shiny-like datapath without a meaningful extension
  writeLines(c("keyword", "machine learning"), path)

  expect_equal(env$readTermList(path, "Terms.CSV")[[1]], "machine learning")
})

test_that("readTermList rejects unsupported formats with a readable error", {
  env <- app_env("tallTextIO.R")
  path <- tempfile(fileext = ".txt")
  writeLines("machine learning", path)

  expect_error(env$readTermList(path, "terms.txt"), "Unsupported file format")
})

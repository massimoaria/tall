test_that("txt_recode_fast replaces matching values", {
  x <- c("NOUN", "VERB", "NOUN", "ADV")
  result <- txt_recode_fast(x, from = c("VERB", "ADV"), to = c("v", "a"))
  expect_equal(result, c("NOUN", "v", "NOUN", "a"))
})

test_that("txt_recode_fast returns unchanged vector when no matches", {
  x <- c("NOUN", "VERB")
  result <- txt_recode_fast(x, from = c("ADJ"), to = c("adj"))
  expect_equal(result, c("NOUN", "VERB"))
})

test_that("txt_recode_fast handles empty vector", {
  result <- txt_recode_fast(character(0), from = c("A"), to = c("B"))
  expect_equal(result, character(0))
})

test_that("txt_recode_fast handles empty from/to", {
  x <- c("A", "B", "C")
  result <- txt_recode_fast(x, from = character(0), to = character(0))
  expect_equal(result, x)
})

test_that("txt_recode_fast with na.rm sets unmatched to NA", {
  x <- c("NOUN", "VERB", "ADJ")
  result <- txt_recode_fast(x, from = c("NOUN"), to = c("n"), na.rm = TRUE)
  expect_equal(result, c("n", NA, NA))
})

test_that("txt_recode_fast errors when from/to lengths differ", {
  expect_error(
    txt_recode_fast(c("A"), from = c("A", "B"), to = c("a")),
    "length\\(from\\) == length\\(to\\)"
  )
})

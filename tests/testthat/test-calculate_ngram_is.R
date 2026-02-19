test_that("calculate_ngram_is returns expected structure", {
  data(mobydick, package = "tall")
  result <- calculate_ngram_is(mobydick, max_ngram = 3, term = "lemma", min_freq = 2)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("ngram", "n_length", "ngram_freq", "n_lexical", "IS", "IS_norm"))
  expect_true(nrow(result) > 0)
  expect_true(all(result$ngram_freq >= 2))
  expect_true(all(result$n_length <= 3))
  expect_true(all(result$IS_norm >= 0))
})

test_that("calculate_ngram_is respects min_freq filter", {
  data(mobydick, package = "tall")
  result_low <- calculate_ngram_is(mobydick, max_ngram = 2, min_freq = 1)
  result_high <- calculate_ngram_is(mobydick, max_ngram = 2, min_freq = 5)

  expect_true(nrow(result_low) >= nrow(result_high))
  if (nrow(result_high) > 0) {
    expect_true(all(result_high$ngram_freq >= 5))
  }
})

test_that("calculate_ngram_is respects max_ngram", {
  data(mobydick, package = "tall")
  result <- calculate_ngram_is(mobydick, max_ngram = 2, min_freq = 2)

  expect_true(all(result$n_length <= 2))
})

test_that("calculate_ngram_is returns empty tibble with impossible filter", {
  data(mobydick, package = "tall")
  result <- calculate_ngram_is(mobydick, max_ngram = 2, min_freq = 999999)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("ngram", "n_length", "ngram_freq", "n_lexical", "IS", "IS_norm"))
})

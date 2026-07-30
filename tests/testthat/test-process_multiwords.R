test_that("txt_recode_ngram_fast identifies multiword expressions", {
  tokens <- c("machine", "learning", "is", "cool", "machine", "learning")
  compounds <- c("machine learning")
  ngrams <- c(2L)
  result <- txt_recode_ngram_fast(tokens, compounds, ngrams, " ")

  expect_equal(result[1], "machine learning")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "is")
  expect_equal(result[4], "cool")
  expect_equal(result[5], "machine learning")
  expect_true(is.na(result[6]))
})

test_that("txt_recode_ngram_fast handles no matches", {
  tokens <- c("hello", "world")
  compounds <- c("machine learning")
  ngrams <- c(2L)
  result <- txt_recode_ngram_fast(tokens, compounds, ngrams, " ")

  expect_equal(result, c("hello", "world"))
})

test_that("txt_recode_ngram_fast handles empty input", {
  result <- txt_recode_ngram_fast(character(0), character(0), integer(0), " ")
  expect_equal(result, character(0))
})

test_that("process_multiwords_fast returns expected structure", {
  x2 <- data.frame(
    doc_id = rep("doc1", 6),
    term_id = 1:6,
    token = c("machine", "learning", "is", "very", "deep", "learning"),
    lemma = c("machine", "learning", "be", "very", "deep", "learning"),
    upos = c("NOUN", "NOUN", "AUX", "ADV", "ADJ", "NOUN"),
    stringsAsFactors = FALSE
  )
  stats <- data.frame(
    keyword = c("machine learning"),
    ngram = c(2L),
    stringsAsFactors = FALSE
  )

  result <- process_multiwords_fast(x2, stats, term = "lemma")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("doc_id", "term_id", "multiword", "upos_multiword",
                         "ngram", "mw_owner"))
  expect_equal(nrow(result), 6)
})

test_that("process_multiwords_fast tags the absorbed constituents", {
  x2 <- data.frame(
    doc_id = rep("doc1", 6),
    term_id = 1:6,
    token = c("machine", "learning", "is", "very", "deep", "learning"),
    lemma = c("machine", "learning", "be", "very", "deep", "learning"),
    upos = c("NOUN", "NOUN", "AUX", "ADV", "ADJ", "NOUN"),
    stringsAsFactors = FALSE
  )
  stats <- data.frame(keyword = "machine learning", ngram = 2L,
                      stringsAsFactors = FALSE)

  result <- process_multiwords_fast(x2, stats, term = "lemma")

  # row 1 is the multiword, row 2 is the constituent it absorbed: without the
  # NGRAM_MERGED tag the word would stay in the corpus twice
  expect_equal(result$multiword[1], "machine learning")
  expect_equal(result$upos_multiword[1], "MULTIWORD")
  expect_equal(result$upos_multiword[2], "NGRAM_MERGED")
  expect_equal(result$mw_owner[2], "machine learning")
  expect_true(is.na(result$mw_owner[1]))

  # untouched tokens keep their own tag and have no owner
  expect_equal(result$upos_multiword[3:6], c("AUX", "ADV", "ADJ", "NOUN"))
  expect_true(all(is.na(result$mw_owner[3:6])))
})

test_that("process_multiwords_fast leaves an NA term alone", {
  # an unresolved lemma is NOT an absorbed constituent: the kernel blanks both,
  # only the absorbed one may be tagged NGRAM_MERGED
  x2 <- data.frame(
    doc_id = rep("doc1", 4),
    term_id = 1:4,
    token = c("machine", "learning", "x", "cool"),
    lemma = c("machine", "learning", NA_character_, "cool"),
    upos = c("NOUN", "NOUN", "X", "ADJ"),
    stringsAsFactors = FALSE
  )
  stats <- data.frame(keyword = "machine learning", ngram = 2L,
                      stringsAsFactors = FALSE)

  result <- process_multiwords_fast(x2, stats, term = "lemma")

  expect_equal(result$upos_multiword[2], "NGRAM_MERGED")
  expect_true(is.na(result$upos_multiword[3]))   # NA term -> untouched
  expect_true(is.na(result$mw_owner[3]))
})

test_that("reinert returns correct class", {
  data(mobydick, package = "tall")
  res <- reinert(
    x = mobydick,
    k = 5,
    term = "token",
    segment_size = 40,
    min_segment_size = 5,
    min_split_members = 10,
    cc_test = 0.3,
    tsj = 3
  )

  expect_s3_class(res, "reinert_tall")
  expect_s3_class(res, "hclust")
  expect_true("group" %in% names(res))
  expect_true("dtmOriginal" %in% names(res))
  expect_true("corresp_uce_uc_full" %in% names(res))
})

test_that("term_per_cluster returns terms and segments", {
  data(mobydick, package = "tall")
  res <- reinert(
    x = mobydick,
    k = 5,
    term = "token",
    segment_size = 40,
    min_segment_size = 5,
    min_split_members = 10,
    cc_test = 0.3,
    tsj = 3
  )

  tc <- term_per_cluster(res, cutree = NULL, k = 1:3, negative = FALSE)

  expect_type(tc, "list")
  expect_named(tc, c("terms", "segments"))
  expect_s3_class(tc$terms, "data.frame")
  expect_s3_class(tc$segments, "data.frame")
  expect_true("cluster" %in% names(tc$terms))
  expect_true("cluster" %in% names(tc$segments))
})

test_that("reinSummary returns a summary table", {
  data(mobydick, package = "tall")
  res <- reinert(
    x = mobydick,
    k = 5,
    term = "token",
    segment_size = 40,
    min_segment_size = 5,
    min_split_members = 10,
    cc_test = 0.3,
    tsj = 3
  )

  tc <- term_per_cluster(res, cutree = NULL, k = 1:3, negative = FALSE)
  S <- reinSummary(tc, n = 10)

  expect_s3_class(S, "data.frame")
  expect_true("cluster" %in% names(S))
  expect_true("Positively Associated Terms" %in% names(S))
  expect_true(nrow(S) > 0)
})

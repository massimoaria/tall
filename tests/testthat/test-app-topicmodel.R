test_that("betaByTopic sorts every topic on its own, shared words included", {
  env <- app_env("tallTopicModel.R")
  beta <- data.frame(
    word = c("ambiente", "attenzione", "bambino", "zaino"),
    `Topic 1` = c(0.30, 0.05, 0.60, 0.05),
    `Topic 2` = c(0.01, 0.20, 0.70, 0.09),
    `Topic 3` = c(0.10, 0.10, 0.05, 0.75),
    check.names = FALSE
  )
  out <- env$betaByTopic(beta)

  expect_equal(
    names(out),
    c("Topic 1 word", "Topic 1 beta", "Topic 2 word", "Topic 2 beta",
      "Topic 3 word", "Topic 3 beta")
  )
  expect_equal(nrow(out), nrow(beta))
  for (tp in c("Topic 1", "Topic 2", "Topic 3")) {
    expect_false(is.unsorted(rev(out[[paste(tp, "beta")]])))
    # each word keeps its own probability in that topic
    expect_equal(out[[paste(tp, "beta")]],
                 beta[[tp]][match(out[[paste(tp, "word")]], beta$word)])
  }
  expect_equal(out[["Topic 1 word"]][1], "bambino")
  expect_equal(out[["Topic 2 word"]][1], "bambino")
  expect_equal(out[["Topic 3 word"]][1], "zaino")
})

test_that("LDA K tuning runs on a TF-IDF selected vocabulary", {
  skip_on_cran()
  skip_if_not_installed("topicmodels")
  env <- app_env("tallTopicModel.R")
  data(brussels_reviews_anno, package = "udpipe", envir = environment())
  x <- subset(brussels_reviews_anno, language == "fr" & upos == "NOUN")
  dtf <- udpipe::document_term_frequencies(x, document = "doc_id", term = "lemma")
  dtm <- udpipe::dtm_remove_tfidf(udpipe::document_term_matrix(dtf), top = 100)
  # what the K selection page builds for top_by = "tfidf" (documents.R)
  dtm <- tm::as.DocumentTermMatrix(dtm, weighting = tm::weightTf)

  res <- env$tmTuningAsync(dtm, k_seq = 2:3, seed = 1, method = "LDA")
  expect_equal(res$metrics$k, 2:3)
  expect_true(all(is.finite(res$metrics$Perplexity)))
})

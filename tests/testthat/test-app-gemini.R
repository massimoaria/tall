test_that("string_to_sentence_df never fails on what a Gemini call can leave behind", {
  env <- app_env("tallAI.R")
  not_done <- data.frame(TALL_AI = "TALL AI was not performed")

  expect_equal(env$string_to_sentence_df(NULL), not_done)
  expect_equal(env$string_to_sentence_df(NA_character_), not_done)
  expect_equal(env$string_to_sentence_df(character(0)), not_done)
  expect_equal(env$string_to_sentence_df(c("a text", "another")), not_done)
  expect_equal(env$string_to_sentence_df(list("a")), not_done)

  expect_equal(
    env$string_to_sentence_df("First line.\n\n  Second line.  ")$TALL_AI,
    c("First line.", "Second line.")
  )
})

gemini_response <- function(body, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  )
}

test_that("gemini_ai returns one string, without thought parts", {
  skip_if_not_installed("httr2", "1.0.0")
  library(httr2)
  env <- app_env("tallAI.R")
  body <- list(candidates = list(list(
    content = list(parts = list(
      list(text = "thinking...", thought = TRUE),
      list(text = "First part.", thoughtSignature = "abc123"),
      list(text = "Second part.")
    )),
    finishReason = "STOP"
  )))
  httr2::local_mocked_responses(function(req) gemini_response(body))

  res <- env$gemini_ai(prompt = "x", api_key = "test-key")

  expect_type(res, "character")
  expect_length(res, 1)
  expect_equal(res, "First part.\nSecond part.")
})

test_that("gemini_ai returns a message, not NULL, when there is no text", {
  skip_if_not_installed("httr2", "1.0.0")
  library(httr2)
  env <- app_env("tallAI.R")
  httr2::local_mocked_responses(function(req) {
    gemini_response(list(candidates = list(list(finishReason = "SAFETY"))))
  })

  res <- env$gemini_ai(prompt = "x", api_key = "test-key")

  expect_length(res, 1)
  expect_match(res, "SAFETY")
  expect_equal(env$string_to_sentence_df(res)$TALL_AI, res)
})

test_that("gemini_ai returns one string after exhausting 503 retries", {
  skip_if_not_installed("httr2", "1.0.0")
  library(httr2)
  env <- app_env("tallAI.R")
  httr2::local_mocked_responses(function(req) gemini_response(list(), 503L))

  res <- suppressMessages(env$gemini_ai(prompt = "x", api_key = "k", retry_503 = 1))

  expect_length(res, 1)
  expect_match(res, "503")
})

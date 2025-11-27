#' Calculate IS index for n-grams
#'
#' This function calculates the IS (Absorption Index) from Morrone (1996)
#' for all n-grams in the corpus. Only n-grams that start AND end with
#' lexical words are considered.
#'
#' @param dfTag A data frame with tagged text data containing columns: doc_id,
#'   sentence_id, token_id, lemma/token, upos
#' @param max_ngram Maximum length of n-grams to generate (default: 5)
#' @param term Character string indicating which column to use: "lemma" or "token" (default: "lemma")
#' @param pos Character vector of POS tags considered lexical (default: c("NOUN", "ADJ", "ADV", "VERB"))
#' @param min_freq Minimum frequency threshold for n-grams (default: 1)
#' @param min_IS_norm Minimum normalized IS threshold for n-grams (default: 0)
#'
#' @return A tibble with columns: ngram, n_length, ngram_freq, n_lexical, IS, IS_norm
#'
#' @details
#' The IS index is calculated as: IS = (Σ 1/freq_i) × freq_ngram × n_lexical
#' where freq_i is the frequency of each word in the n-gram, freq_ngram is the
#' frequency of the n-gram, and n_lexical is the number of lexical words.
#' IS_norm is the normalized version: IS / L^2 where L is the n-gram length.
#'
#' OPTIMIZATION: Only n-grams that start AND end with lexical words (as defined by
#' the 'pos' parameter) are generated, significantly reducing computation time.
#'
#' @export
#' @examples
#' \dontrun{
#' IS <- calculate_ngram_is(dfTag, max_ngram = 4, term = "lemma", min_freq = 2)
#' head(IS)
#' }
calculate_ngram_is <- function(
  dfTag,
  max_ngram = 5,
  term = "lemma",
  pos = c("NOUN", "ADJ", "ADV", "VERB"),
  min_freq = 1,
  min_IS_norm = 0
) {
  library(dplyr)

  include_pos <- c(
    "DET",
    "NOUN",
    "PRON",
    "ADV",
    "VERB",
    "ADP",
    "AUX",
    "ADJ",
    "CCONJ",
    "INTJ",
    "PART",
    "PROPN",
    "SCONJ"
  )

  # Filter valid tokens
  df_filtered <- dfTag %>%
    filter(upos %in% include_pos, !is.na(!!sym(term))) %>%
    select(doc_id, sentence_id, token_id, term_col = !!sym(term), upos) %>%
    arrange(doc_id, sentence_id, as.numeric(token_id))

  # Calculate global word frequencies
  word_freq_df <- df_filtered %>%
    count(term_col, name = "word_freq")

  # Prepare sentences as lists
  sentences <- df_filtered %>%
    group_by(doc_id, sentence_id) %>%
    summarise(
      terms = list(term_col),
      pos_tags = list(upos),
      .groups = "drop"
    )

  # Generate n-grams using C++ function
  # OPTIMIZATION: Pass lexical_pos to filter n-grams at generation time
  ngrams_list <- generate_ngrams_cpp(
    sentences_terms = sentences$terms,
    sentences_pos = sentences$pos_tags,
    max_ngram = max_ngram,
    lexical_pos = pos # Pass lexical POS for filtering
  )

  # Convert to tibble
  ngrams_df <- tibble(
    ngram = ngrams_list$ngram,
    words = ngrams_list$words,
    pos_sequence = ngrams_list$pos_sequence,
    n_length = ngrams_list$n_length
  )

  # Calculate frequencies and filter
  ngram_counts <- ngrams_df %>%
    count(ngram, n_length, name = "ngram_freq") %>%
    filter(ngram_freq >= min_freq)

  if (nrow(ngram_counts) == 0) {
    return(tibble(
      ngram = character(),
      n_length = integer(),
      ngram_freq = integer(),
      n_lexical = integer(),
      IS = numeric(),
      IS_norm = numeric()
    ))
  }

  # Get metadata
  ngram_metadata <- ngrams_df %>%
    distinct(ngram, n_length, .keep_all = TRUE) %>%
    select(ngram, n_length, words, pos_sequence)

  ngram_freq <- ngram_counts %>%
    left_join(ngram_metadata, by = c("ngram", "n_length"))

  # Calculate IS components using C++ functions
  ngram_freq$n_lexical <- count_lexical_cpp(
    ngram_pos = ngram_freq$pos_sequence,
    lexical_pos = pos
  )

  ngram_freq$sum_reciprocal_freq <- calc_reciprocal_sum_cpp(
    ngram_words = ngram_freq$words,
    word_freq_names = word_freq_df$term_col,
    word_freq_values = word_freq_df$word_freq
  )

  # Final calculations
  ngram_freq <- ngram_freq %>%
    mutate(
      IS = sum_reciprocal_freq * ngram_freq * n_lexical,
      IS_norm = IS / (n_length^2)
    ) %>%
    dplyr::filter(IS_norm >= min_IS_norm) %>%
    select(ngram, n_length, ngram_freq, n_lexical, IS, IS_norm) %>%
    arrange(desc(IS_norm))

  return(ngram_freq)
}

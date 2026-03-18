#' Extract Noun Phrases via Dependency Parsing (C++ backend)
#'
#' @param sent_id Integer vector of sentence IDs
#' @param token_id Integer vector of token IDs
#' @param head_token_id Integer vector of head token IDs
#' @param dep_rel Character vector of dependency relations
#' @param upos Character vector of universal POS tags
#' @param terms Character vector of term values (lemma or token)
#' @param ngram_max Maximum phrase length (default 5)
#' @param max_gap Maximum gap between token positions (default 3)
#' @return A data.frame with columns keyword and ngram
#' @export
extract_noun_phrases <- function(sent_id, token_id, head_token_id, dep_rel, upos, terms, ngram_max = 5L, max_gap = 3L) {
  extract_np_cpp(sent_id, token_id, head_token_id, dep_rel, upos, terms, ngram_max, max_gap)
}

#' Extract SVO (Subject-Verb-Object) Triplets via Dependency Parsing (C++ backend)
#'
#' @param sent_id Integer vector of sentence IDs
#' @param token_id Integer vector of token IDs
#' @param head_token_id Integer vector of head token IDs
#' @param dep_rel Character vector of dependency relations
#' @param upos Character vector of universal POS tags
#' @param terms Character vector of term values (lemma or token)
#' @return A data.frame with columns subject, verb, object, rel_type
#' @export
extract_svo_triplets <- function(sent_id, token_id, head_token_id, dep_rel, upos, terms) {
  extract_svo_cpp(sent_id, token_id, head_token_id, dep_rel, upos, terms)
}

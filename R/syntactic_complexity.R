#' Compute Syntactic Complexity Metrics per Document (C++ backend)
#'
#' @param doc_id Character vector of document IDs
#' @param sent_id Integer vector of sentence IDs
#' @param token_id Integer vector of token IDs
#' @param head_token_id Integer vector of head token IDs
#' @param dep_rel Character vector of dependency relations
#' @param upos Character vector of universal POS tags
#' @return A data.frame with one row per document and complexity metrics
#' @export
compute_syntactic_complexity <- function(doc_id, sent_id, token_id, head_token_id, dep_rel, upos) {
  syntactic_complexity_cpp(doc_id, sent_id, token_id, head_token_id, dep_rel, upos)
}

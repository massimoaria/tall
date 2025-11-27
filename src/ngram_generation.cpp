// ============================================================================
// FILE: src/ngram_generation.cpp
// ============================================================================

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List generate_ngrams_cpp(List sentences_terms,
                         List sentences_pos,
                         int max_ngram) {

  // ... resto del codice uguale ...

  int total_ngrams = 0;
  int n_sentences = sentences_terms.size();

  for (int i = 0; i < n_sentences; i++) {
    CharacterVector terms = sentences_terms[i];
    int n_tokens = terms.size();
    for (int n = 2; n <= max_ngram; n++) {
      if (n_tokens >= n) {
        total_ngrams += (n_tokens - n + 1);
      }
    }
  }

  CharacterVector ngram_text(total_ngrams);
  List ngram_words(total_ngrams);
  List ngram_pos(total_ngrams);
  IntegerVector ngram_length(total_ngrams);

  int idx = 0;

  for (int i = 0; i < n_sentences; i++) {
    CharacterVector terms = sentences_terms[i];
    CharacterVector pos = sentences_pos[i];
    int n_tokens = terms.size();

    for (int n = 2; n <= max_ngram; n++) {
      if (n_tokens >= n) {
        for (int j = 0; j < (n_tokens - n + 1); j++) {

          String ngram_str = "";
          CharacterVector current_words(n);
          CharacterVector current_pos(n);

          for (int k = 0; k < n; k++) {
            if (k > 0) ngram_str += " ";
            ngram_str += terms[j + k];
            current_words[k] = terms[j + k];
            current_pos[k] = pos[j + k];
          }

          ngram_text[idx] = ngram_str;
          ngram_words[idx] = current_words;
          ngram_pos[idx] = current_pos;
          ngram_length[idx] = n;
          idx++;
        }
      }
    }
  }

  return List::create(
    Named("ngram") = ngram_text,
    Named("words") = ngram_words,
    Named("pos_sequence") = ngram_pos,
    Named("n_length") = ngram_length
  );
}

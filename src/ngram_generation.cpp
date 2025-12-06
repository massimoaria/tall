// ============================================================================
// FILE: src/ngram_generation.cpp
// ============================================================================

#include <Rcpp.h>
#include <unordered_set>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
List generate_ngrams_cpp(List sentences_terms,
                         List sentences_pos,
                         int max_ngram,
                         CharacterVector lexical_pos) {

  // Create set of lexical POS for fast lookup
  std::unordered_set<std::string> lexical_set;
  for (int i = 0; i < lexical_pos.size(); i++) {
    lexical_set.insert(Rcpp::as<std::string>(lexical_pos[i]));
  }

  // First pass: count valid n-grams (those starting AND ending with lexical words)
  int total_ngrams = 0;
  int n_sentences = sentences_terms.size();

  for (int i = 0; i < n_sentences; i++) {
    CharacterVector terms = sentences_terms[i];
    CharacterVector pos = sentences_pos[i];
    int n_tokens = terms.size();

    for (int n = 2; n <= max_ngram; n++) {
      if (n_tokens >= n) {
        for (int j = 0; j < (n_tokens - n + 1); j++) {
          // Check if first AND last word are lexical
          std::string first_pos = Rcpp::as<std::string>(pos[j]);
          std::string last_pos = Rcpp::as<std::string>(pos[j + n - 1]);

          if (lexical_set.find(first_pos) != lexical_set.end() &&
              lexical_set.find(last_pos) != lexical_set.end()) {
            total_ngrams++;
          }
        }
      }
    }
  }

  // Pre-allocate vectors for valid n-grams only
  CharacterVector ngram_text(total_ngrams);
  List ngram_words(total_ngrams);
  List ngram_pos(total_ngrams);
  IntegerVector ngram_length(total_ngrams);

  int idx = 0;

  // Second pass: generate only valid n-grams
  for (int i = 0; i < n_sentences; i++) {
    CharacterVector terms = sentences_terms[i];
    CharacterVector pos = sentences_pos[i];
    int n_tokens = terms.size();

    for (int n = 2; n <= max_ngram; n++) {
      if (n_tokens >= n) {
        for (int j = 0; j < (n_tokens - n + 1); j++) {

          // Check if first AND last word are lexical
          std::string first_pos = Rcpp::as<std::string>(pos[j]);
          std::string last_pos = Rcpp::as<std::string>(pos[j + n - 1]);

          if (lexical_set.find(first_pos) != lexical_set.end() &&
              lexical_set.find(last_pos) != lexical_set.end()) {

            // Create n-gram string
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
  }

  return List::create(
    Named("ngram") = ngram_text,
    Named("words") = ngram_words,
    Named("pos_sequence") = ngram_pos,
    Named("n_length") = ngram_length
  );
}

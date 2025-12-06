// ============================================================================
// FILE: src/is_calculation.cpp
// ============================================================================

#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_reciprocal_sum_cpp(List ngram_words,
                                      CharacterVector word_freq_names,
                                      NumericVector word_freq_values) {

  int n = ngram_words.size();
  NumericVector result(n);

  std::unordered_map<std::string, double> freq_map;
  for (int i = 0; i < word_freq_names.size(); i++) {
    freq_map[Rcpp::as<std::string>(word_freq_names[i])] = word_freq_values[i];
  }

  for (int i = 0; i < n; i++) {
    CharacterVector words = ngram_words[i];
    double sum = 0.0;

    for (int j = 0; j < words.size(); j++) {
      std::string word = Rcpp::as<std::string>(words[j]);
      auto it = freq_map.find(word);
      if (it != freq_map.end()) {
        sum += 1.0 / it->second;
      }
    }

    result[i] = sum;
  }

  return result;
}

// [[Rcpp::export]]
IntegerVector count_lexical_cpp(List ngram_pos,
                                CharacterVector lexical_pos) {

  int n = ngram_pos.size();
  IntegerVector result(n);

  std::unordered_set<std::string> lexical_set;
  for (int i = 0; i < lexical_pos.size(); i++) {
    lexical_set.insert(Rcpp::as<std::string>(lexical_pos[i]));
  }

  for (int i = 0; i < n; i++) {
    CharacterVector pos = ngram_pos[i];
    int count = 0;

    for (int j = 0; j < pos.size(); j++) {
      std::string pos_tag = Rcpp::as<std::string>(pos[j]);
      if (lexical_set.find(pos_tag) != lexical_set.end()) {
        count++;
      }
    }

    result[i] = count;
  }

  return result;
}

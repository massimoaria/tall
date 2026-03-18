// ============================================================================
// FILE: src/extract_np.cpp
// DESCRIPTION: Fast dependency-based noun phrase extraction for TALL package
// ============================================================================

#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame extract_np_cpp(
    IntegerVector sent_id,
    IntegerVector token_id,
    IntegerVector head_token_id,
    CharacterVector dep_rel,
    CharacterVector upos,
    CharacterVector terms,
    int ngram_max = 5,
    int max_gap = 3
) {
  int n = sent_id.size();

  // Modifier relations to follow
  std::unordered_set<std::string> mod_rels = {
    "amod", "compound", "flat", "flat:name", "nummod", "nmod"
  };

  // Noun heads
  std::unordered_set<std::string> noun_pos = {"NOUN", "PROPN"};

  // Group tokens by sentence
  // sent_id -> vector of row indices
  std::unordered_map<int, std::vector<int>> sent_tokens;
  for (int i = 0; i < n; i++) {
    sent_tokens[sent_id[i]].push_back(i);
  }

  // Output vectors
  std::vector<std::string> out_keywords;
  std::vector<int> out_ngrams;

  // Process each sentence
  for (auto& kv : sent_tokens) {
    const std::vector<int>& rows = kv.second;

    // Build per-sentence lookup: token_id -> row index
    std::unordered_map<int, int> tid_to_row;
    for (int idx : rows) {
      tid_to_row[token_id[idx]] = idx;
    }

    // Find noun heads in this sentence
    for (int idx : rows) {
      std::string u = as<std::string>(upos[idx]);
      if (noun_pos.find(u) == noun_pos.end()) continue;

      int head_tid = token_id[idx];

      // Collect NP member token_ids: start with the head
      std::vector<int> np_tids;
      np_tids.push_back(head_tid);

      // Find direct modifiers of this head
      bool has_modifier = false;
      std::vector<int> nmod_tids; // track nmod dependents for case lookup

      for (int j : rows) {
        if (head_token_id[j] == head_tid) {
          std::string dr = as<std::string>(dep_rel[j]);
          if (mod_rels.find(dr) != mod_rels.end()) {
            np_tids.push_back(token_id[j]);
            has_modifier = true;
            if (dr == "nmod") {
              nmod_tids.push_back(token_id[j]);
            }
          }
        }
      }

      if (!has_modifier) continue;

      // For nmod dependents, find their "case" token (preposition)
      for (int nmod_tid : nmod_tids) {
        for (int j : rows) {
          if (head_token_id[j] == nmod_tid) {
            std::string dr = as<std::string>(dep_rel[j]);
            if (dr == "case") {
              np_tids.push_back(token_id[j]);
            }
          }
        }
      }

      // Sort and deduplicate
      std::sort(np_tids.begin(), np_tids.end());
      np_tids.erase(std::unique(np_tids.begin(), np_tids.end()), np_tids.end());

      int np_len = np_tids.size();
      if (np_len < 2 || np_len > ngram_max) continue;

      // Check max gap
      int mg = 0;
      for (int k = 1; k < np_len; k++) {
        int gap = np_tids[k] - np_tids[k - 1];
        if (gap > mg) mg = gap;
      }
      if (mg > max_gap) continue;

      // Build phrase string
      std::string phrase;
      for (int k = 0; k < np_len; k++) {
        auto it = tid_to_row.find(np_tids[k]);
        if (it == tid_to_row.end()) goto next_head;
        if (k > 0) phrase += " ";
        phrase += as<std::string>(terms[it->second]);
      }

      out_keywords.push_back(phrase);
      out_ngrams.push_back(np_len);

      next_head:;
    }
  }

  return DataFrame::create(
    Named("keyword") = wrap(out_keywords),
    Named("ngram") = wrap(out_ngrams),
    Named("stringsAsFactors") = false
  );
}

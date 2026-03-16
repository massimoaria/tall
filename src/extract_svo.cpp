// ============================================================================
// FILE: src/extract_svo.cpp
// DESCRIPTION: Fast SVO (Subject-Verb-Object) triplet extraction
// ============================================================================

#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame extract_svo_cpp(
    IntegerVector sent_id,
    IntegerVector token_id,
    IntegerVector head_token_id,
    CharacterVector dep_rel,
    CharacterVector upos,
    CharacterVector terms
) {
  int n = sent_id.size();

  // Subject and object relations
  std::unordered_set<std::string> subj_rels = {"nsubj", "nsubj:pass", "csubj"};
  std::unordered_set<std::string> obj_rels = {"obj", "dobj", "iobj"};
  // Additional complement relations
  std::unordered_set<std::string> comp_rels = {"obl", "xcomp", "ccomp"};

  // Group tokens by sentence
  std::unordered_map<int, std::vector<int>> sent_tokens;
  for (int i = 0; i < n; i++) {
    sent_tokens[sent_id[i]].push_back(i);
  }

  // Output vectors
  std::vector<std::string> out_subject;
  std::vector<std::string> out_verb;
  std::vector<std::string> out_object;
  std::vector<std::string> out_rel_type;

  // Process each sentence
  for (auto& kv : sent_tokens) {
    const std::vector<int>& rows = kv.second;

    // Build token_id -> row index lookup for this sentence
    std::unordered_map<int, int> tid_to_row;
    for (int idx : rows) {
      tid_to_row[token_id[idx]] = idx;
    }

    // Find verbs in this sentence
    for (int idx : rows) {
      std::string u = as<std::string>(upos[idx]);
      if (u != "VERB" && u != "AUX") continue;

      int verb_tid = token_id[idx];
      std::string verb_term = as<std::string>(terms[idx]);

      // Find subjects and objects that depend on this verb
      std::vector<std::pair<std::string, std::string>> subjects; // (term, rel)
      std::vector<std::pair<std::string, std::string>> objects;  // (term, rel)

      for (int j : rows) {
        if (head_token_id[j] != verb_tid) continue;
        std::string dr = as<std::string>(dep_rel[j]);
        std::string t = as<std::string>(terms[j]);

        if (subj_rels.find(dr) != subj_rels.end()) {
          subjects.push_back({t, dr});
        } else if (obj_rels.find(dr) != obj_rels.end()) {
          objects.push_back({t, dr});
        } else if (comp_rels.find(dr) != comp_rels.end()) {
          objects.push_back({t, dr});
        }
      }

      // Generate triplets: each subject paired with each object
      if (!subjects.empty() && !objects.empty()) {
        for (auto& s : subjects) {
          for (auto& o : objects) {
            out_subject.push_back(s.first);
            out_verb.push_back(verb_term);
            out_object.push_back(o.first);
            out_rel_type.push_back(s.second + " + " + o.second);
          }
        }
      }

      // Also emit subject-verb pairs (intransitive)
      if (!subjects.empty() && objects.empty()) {
        for (auto& s : subjects) {
          out_subject.push_back(s.first);
          out_verb.push_back(verb_term);
          out_object.push_back("");
          out_rel_type.push_back(s.second);
        }
      }
    }
  }

  return DataFrame::create(
    Named("subject") = wrap(out_subject),
    Named("verb") = wrap(out_verb),
    Named("object") = wrap(out_object),
    Named("rel_type") = wrap(out_rel_type),
    Named("stringsAsFactors") = false
  );
}

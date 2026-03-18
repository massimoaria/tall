// ============================================================================
// FILE: src/syntactic_complexity.cpp
// DESCRIPTION: Fast syntactic complexity metrics from dependency trees
// ============================================================================

#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include <queue>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame syntactic_complexity_cpp(
    CharacterVector doc_id,
    IntegerVector sent_id,
    IntegerVector token_id,
    IntegerVector head_token_id,
    CharacterVector dep_rel,
    CharacterVector upos
) {
  int n = doc_id.size();

  // Group by (doc_id, sent_id) -> sentence key
  // We use a combined key: doc_id + "_" + sent_id
  struct SentData {
    std::vector<int> tok_ids;
    std::vector<int> head_ids;
    std::vector<std::string> dep_rels;
    std::vector<std::string> upos_vals;
    std::string doc;
  };

  std::unordered_map<long long, SentData> sentences;
  std::unordered_map<std::string, int> doc_sent_count;

  // Assign unique integer key per sentence
  std::unordered_map<std::string, long long> sent_key_map;
  long long next_key = 0;

  for (int i = 0; i < n; i++) {
    std::string dk = as<std::string>(doc_id[i]);
    std::string sk = dk + "_" + std::to_string(sent_id[i]);

    long long key;
    auto it = sent_key_map.find(sk);
    if (it == sent_key_map.end()) {
      key = next_key++;
      sent_key_map[sk] = key;
      sentences[key].doc = dk;
      doc_sent_count[dk]++;
    } else {
      key = it->second;
    }

    sentences[key].tok_ids.push_back(token_id[i]);
    sentences[key].head_ids.push_back(head_token_id[i]);
    sentences[key].dep_rels.push_back(as<std::string>(dep_rel[i]));
    sentences[key].upos_vals.push_back(as<std::string>(upos[i]));
  }

  // Per-sentence metrics
  struct SentMetrics {
    std::string doc;
    int sent_length;
    int tree_depth;
    double mean_dep_distance;
    int n_clauses;
    int n_subordinate;
    int n_coordinate;
    double branching_factor;
  };

  std::vector<SentMetrics> sent_metrics;

  // Clause-indicating dep_rels
  std::unordered_set<std::string> subordinate_rels = {
    "advcl", "acl", "acl:relcl", "ccomp", "xcomp", "csubj"
  };
  std::unordered_set<std::string> coordinate_rels = {"conj"};

  for (auto& kv : sentences) {
    SentData& sd = kv.second;
    int sz = sd.tok_ids.size();
    if (sz == 0) continue;

    // Build children map and compute metrics
    std::unordered_map<int, std::vector<int>> children; // head -> list of dependents
    std::unordered_map<int, int> tid_to_idx;
    int root_tid = -1;

    for (int i = 0; i < sz; i++) {
      tid_to_idx[sd.tok_ids[i]] = i;
      if (sd.head_ids[i] == 0) {
        root_tid = sd.tok_ids[i];
      }
      children[sd.head_ids[i]].push_back(sd.tok_ids[i]);
    }

    // 1. Tree depth (BFS from root)
    int max_depth = 0;
    if (root_tid >= 0) {
      std::queue<std::pair<int, int>> bfs; // (token_id, depth)
      bfs.push({root_tid, 1});
      while (!bfs.empty()) {
        auto [tid, depth] = bfs.front();
        bfs.pop();
        if (depth > max_depth) max_depth = depth;
        auto cit = children.find(tid);
        if (cit != children.end()) {
          for (int child : cit->second) {
            bfs.push({child, depth + 1});
          }
        }
      }
    }

    // 2. Mean dependency distance
    double total_dist = 0;
    int dist_count = 0;
    for (int i = 0; i < sz; i++) {
      if (sd.head_ids[i] > 0) {
        total_dist += std::abs(sd.tok_ids[i] - sd.head_ids[i]);
        dist_count++;
      }
    }
    double mean_dist = (dist_count > 0) ? total_dist / dist_count : 0;

    // 3. Clause counts
    int n_sub = 0, n_coord = 0;
    for (int i = 0; i < sz; i++) {
      if (subordinate_rels.find(sd.dep_rels[i]) != subordinate_rels.end()) n_sub++;
      if (coordinate_rels.find(sd.dep_rels[i]) != coordinate_rels.end() &&
          (sd.upos_vals[i] == "VERB" || sd.upos_vals[i] == "AUX")) n_coord++;
    }
    int n_clauses = 1 + n_sub + n_coord; // main clause + subordinate + coordinate

    // 4. Branching factor (average number of children per non-leaf node)
    int non_leaf = 0;
    int total_children = 0;
    for (auto& ck : children) {
      if (ck.first == 0) continue; // skip root marker
      int nc = ck.second.size();
      if (nc > 0) {
        non_leaf++;
        total_children += nc;
      }
    }
    double branching = (non_leaf > 0) ? (double)total_children / non_leaf : 0;

    // Filter out PUNCT from sentence length
    int content_length = 0;
    for (int i = 0; i < sz; i++) {
      if (sd.upos_vals[i] != "PUNCT") content_length++;
    }

    SentMetrics sm;
    sm.doc = sd.doc;
    sm.sent_length = content_length;
    sm.tree_depth = max_depth;
    sm.mean_dep_distance = mean_dist;
    sm.n_clauses = n_clauses;
    sm.n_subordinate = n_sub;
    sm.n_coordinate = n_coord;
    sm.branching_factor = branching;
    sent_metrics.push_back(sm);
  }

  // Aggregate to document level
  std::unordered_map<std::string, std::vector<SentMetrics*>> doc_sents;
  for (auto& sm : sent_metrics) {
    doc_sents[sm.doc].push_back(&sm);
  }

  // Output vectors
  std::vector<std::string> out_doc;
  std::vector<int> out_n_sentences;
  std::vector<double> out_mean_sent_length;
  std::vector<double> out_mean_tree_depth;
  std::vector<double> out_max_tree_depth;
  std::vector<double> out_mean_dep_distance;
  std::vector<double> out_mean_clauses;
  std::vector<double> out_mean_subordinate;
  std::vector<double> out_mean_coordinate;
  std::vector<double> out_subordination_ratio;
  std::vector<double> out_mean_branching;

  for (auto& dkv : doc_sents) {
    const std::string& doc = dkv.first;
    auto& sents = dkv.second;
    int ns = sents.size();

    double sum_len = 0, sum_depth = 0, sum_dist = 0;
    double sum_clauses = 0, sum_sub = 0, sum_coord = 0, sum_branch = 0;
    int max_d = 0;

    for (auto* s : sents) {
      sum_len += s->sent_length;
      sum_depth += s->tree_depth;
      if (s->tree_depth > max_d) max_d = s->tree_depth;
      sum_dist += s->mean_dep_distance;
      sum_clauses += s->n_clauses;
      sum_sub += s->n_subordinate;
      sum_coord += s->n_coordinate;
      sum_branch += s->branching_factor;
    }

    double sub_ratio = (sum_clauses > 0) ? sum_sub / sum_clauses : 0;

    out_doc.push_back(doc);
    out_n_sentences.push_back(ns);
    out_mean_sent_length.push_back(sum_len / ns);
    out_mean_tree_depth.push_back(sum_depth / ns);
    out_max_tree_depth.push_back(max_d);
    out_mean_dep_distance.push_back(sum_dist / ns);
    out_mean_clauses.push_back(sum_clauses / ns);
    out_mean_subordinate.push_back(sum_sub / ns);
    out_mean_coordinate.push_back(sum_coord / ns);
    out_subordination_ratio.push_back(sub_ratio);
    out_mean_branching.push_back(sum_branch / ns);
  }

  return DataFrame::create(
    Named("doc_id") = wrap(out_doc),
    Named("n_sentences") = wrap(out_n_sentences),
    Named("mean_sent_length") = wrap(out_mean_sent_length),
    Named("mean_tree_depth") = wrap(out_mean_tree_depth),
    Named("max_tree_depth") = wrap(out_max_tree_depth),
    Named("mean_dep_distance") = wrap(out_mean_dep_distance),
    Named("mean_clauses_per_sent") = wrap(out_mean_clauses),
    Named("mean_subordinate") = wrap(out_mean_subordinate),
    Named("mean_coordinate") = wrap(out_mean_coordinate),
    Named("subordination_ratio") = wrap(out_subordination_ratio),
    Named("mean_branching_factor") = wrap(out_mean_branching),
    Named("stringsAsFactors") = false
  );
}

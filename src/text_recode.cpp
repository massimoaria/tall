// ============================================================================
// FILE: src/text_recode.cpp
// DESCRIPTION: Fast text recoding functions for TALL package
// ============================================================================

#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <vector>

using namespace Rcpp;

// Fast text recoding using C++ hash map
// Internal C++ function - documented in R/text_recode_fast.R
// [[Rcpp::export]]
CharacterVector txt_recode_cpp(CharacterVector x,
                               CharacterVector from,
                               CharacterVector to,
                               bool na_rm = false) {

  int n = x.size();
  int m = from.size();

  if(m != to.size()) {
    stop("'from' and 'to' must have the same length");
  }

  // Crea hash map per lookup O(1)
  std::unordered_map<std::string, std::string> lookup;
  lookup.reserve(m * 1.5); // Reserve extra space for efficiency

  // Popola la mappa con i valori da sostituire
  for(int i = 0; i < m; i++) {
    if(from[i] != NA_STRING) {
      lookup[as<std::string>(from[i])] = as<std::string>(to[i]);
    }
  }

  // Se na_rm = FALSE, aggiungi i valori non mappati che devono rimanere uguali
  if(!na_rm) {
    std::unordered_set<std::string> seen;
    seen.reserve(n);

    for(int i = 0; i < n; i++) {
      if(x[i] != NA_STRING) {
        std::string val = as<std::string>(x[i]);
        if(lookup.find(val) == lookup.end() && seen.find(val) == seen.end()) {
          lookup[val] = val;
          seen.insert(val);
        }
      }
    }
  }

  // Esegui il recode
  CharacterVector result(n);
  for(int i = 0; i < n; i++) {
    if(x[i] == NA_STRING) {
      result[i] = NA_STRING;
    } else {
      std::string val = as<std::string>(x[i]);
      auto it = lookup.find(val);
      if(it != lookup.end()) {
        result[i] = it->second;
      } else {
        result[i] = NA_STRING;
      }
    }
  }

  return result;
}


// Fast n-gram recoding for multiword detection
// Internal C++ function - documented in R/text_recode_fast.R
// [[Rcpp::export]]
CharacterVector txt_recode_ngram_cpp(CharacterVector x,
                                     CharacterVector compound,
                                     IntegerVector ngram,
                                     std::string sep = " ") {

  int n = x.size();
  CharacterVector result = clone(x);

  if(n == 0 || compound.size() == 0) {
    return result;
  }

  // Crea mappa: compound -> ngram length
  std::unordered_map<std::string, int> ngram_map;
  ngram_map.reserve(compound.size());

  for(int i = 0; i < compound.size(); i++) {
    if(compound[i] != NA_STRING) {
      ngram_map[as<std::string>(compound[i])] = ngram[i];
    }
  }

  // Trova la lunghezza massima degli n-grammi
  int max_len = 2;
  for(int i = 0; i < ngram.size(); i++) {
    if(ngram[i] > max_len) {
      max_len = ngram[i];
    }
  }

  // Traccia quali posizioni sono già state unite
  std::vector<bool> merged(n, false);

  // Scorri il vettore cercando n-grammi
  for(int i = 0; i < n; i++) {
    if(merged[i] || x[i] == NA_STRING) continue;

    // Prova n-grammi di lunghezza decrescente (più lungo = priorità maggiore)
    for(int len = std::min(max_len, n - i); len >= 2; len--) {

      // Controlla se possiamo costruire un n-gramma di questa lunghezza
      bool can_merge = true;
      std::vector<std::string> tokens;
      tokens.reserve(len);

      for(int j = 0; j < len; j++) {
        if(i + j >= n || merged[i + j] || x[i + j] == NA_STRING) {
          can_merge = false;
          break;
        }
        tokens.push_back(as<std::string>(x[i + j]));
      }

      if(!can_merge) continue;

      // Unisci i tokens con il separatore
      std::string candidate = tokens[0];
      for(size_t j = 1; j < tokens.size(); j++) {
        candidate += sep + tokens[j];
      }

      // Controlla se questo n-gramma è nella lista dei compound
      auto it = ngram_map.find(candidate);
      if(it != ngram_map.end() && it->second == len) {
        // Match trovato!
        result[i] = candidate;

        // Marca le posizioni successive come merged (set to NA)
        for(int j = 1; j < len; j++) {
          result[i + j] = NA_STRING;
          merged[i + j] = true;
        }

        // Una volta trovato un match, non cercare più per questa posizione
        break;
      }
    }
  }

  return result;
}


// Batch text recoding for multiple columns
// Internal C++ function - documented in R/text_recode_fast.R
// [[Rcpp::export]]
List txt_recode_batch_cpp(List x,
                          CharacterVector from,
                          CharacterVector to,
                          bool na_rm = false) {

  int n_vectors = x.size();
  List result(n_vectors);

  // Crea la lookup table una sola volta
  int m = from.size();
  if(m != to.size()) {
    stop("'from' and 'to' must have the same length");
  }

  std::unordered_map<std::string, std::string> lookup;
  lookup.reserve(m * 1.5);

  for(int i = 0; i < m; i++) {
    if(from[i] != NA_STRING) {
      lookup[as<std::string>(from[i])] = as<std::string>(to[i]);
    }
  }

  // Applica il recode a ogni vettore
  for(int v = 0; v < n_vectors; v++) {
    CharacterVector vec = x[v];
    int n = vec.size();
    CharacterVector recoded(n);

    if(!na_rm) {
      // Aggiungi valori non mappati per questo vettore
      std::unordered_set<std::string> seen;
      for(int i = 0; i < n; i++) {
        if(vec[i] != NA_STRING) {
          std::string val = as<std::string>(vec[i]);
          if(lookup.find(val) == lookup.end() && seen.find(val) == seen.end()) {
            lookup[val] = val;
            seen.insert(val);
          }
        }
      }
    }

    // Recode
    for(int i = 0; i < n; i++) {
      if(vec[i] == NA_STRING) {
        recoded[i] = NA_STRING;
      } else {
        std::string val = as<std::string>(vec[i]);
        auto it = lookup.find(val);
        if(it != lookup.end()) {
          recoded[i] = it->second;
        } else {
          recoded[i] = NA_STRING;
        }
      }
    }

    result[v] = recoded;
  }

  return result;
}

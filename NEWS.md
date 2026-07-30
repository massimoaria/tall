# tall (development version)

* Bug fix (Documents > Supervised Classification > Download Results): the
  export read `test_data$target`, but the model frame names that column
  `.target_class`, and "target" is not a prefix of it, so `$` could not reach it
  either. Two outcomes, both reproduced on a real corpus: normally `$target`
  returned `NULL` and `tibble()` raised a recycling error **inside the download
  handler**, so the whole .xlsx failed and none of its five sheets was written;
  and on a corpus that happens to contain a term whose sanitised column name is
  exactly `target` — the US-airlines tweets do — `$` resolved to that TF-IDF
  column instead, and the file was written with numbers in the `Actual` column
  and `Correct` false on every row. The lookup is now `[[".target_class"]]`,
  which also cannot fall back to partial matching.

* Bug fix (Documents > Supervised Classification): `ranger`'s
  `prediction.error` was displayed as "OOB Prediction Error (%)" in the training
  summary, the success dialog and the exported workbook. For a `probability =
  TRUE` forest that field is the out-of-bag **Brier score**, not a
  misclassification rate — 0.21 on the reference run, which invited reading it
  as "21% of documents misclassified". It is now labelled and formatted as a
  Brier score.

* Bug fix (Documents > Supervised Classification): the training-set accuracy was
  computed from **in-sample** predictions, so it is optimistically biased by
  construction (0.92 against 0.75 on the held-out set in the reference run) and
  read like a second, reassuring validation score. It is now labelled
  "Train Set Accuracy (in-sample)".

* Bug fix (Documents > Supervised Classification): training word embeddings from
  this menu overwrote the shared `values$w2v_model` used by Words > Embeddings
  with a differently parameterised, stopword-free model, while leaving
  `values$w2v_stats` and `values$df_EmbeddingDims` describing the previous one —
  so the embedding views silently changed under the user. The model trained here
  is now kept inside the classification menu and the shared one is only read.

* Bug fix (Documents > Supervised Classification): a failed training run left
  the results panel showing the previous model, because the error handler did
  not clear the `trained` flag.

* Bug fix (Documents > Supervised Classification): the prerequisite check
  aborted with "missing value where TRUE/FALSE needed" when `docSelected`
  contained any `NA`, since `sum()` then returns `NA` and `if (NA)` is an error.

* Bug fix (Features > Feature Roles): applying a keyness role to a **binary
  variable that contains missing values** aborted with "the condition has length
  > 1", after `keyness_group` had already been written to the corpus — so the
  roles were applied but the confirmation never appeared. The per-group document
  count did not drop the `NA` group, so `doc_groups$n[doc_groups$keyness_group ==
  1]` returned two elements, the success message became a length-2 vector and the
  final `if (success_message == "")` threw. The count now filters the missing
  group and guards an empty one, as the multi-category branch already did.

* Bug fix (Features > Feature Roles): the time and label roles were assigned
  **before** the keyness groups were validated, and the validation aborts with
  `return()`. Rejecting an Apply for missing or overlapping group assignments
  therefore left a half-applied state (time and label silently assigned, keyness
  not). The group checks now run first, so a rejected Apply changes nothing.

* Bug fix (Features > Feature Roles): the category list of the keyness variable
  was read from the raw token-level column, while the Group 1 / Group 2 pickers
  offer the categories of the *selected* documents. With a filter active the two
  could disagree — a variable reduced to two categories by the filter still took
  the "more than 2 categories" branch and demanded an assignment the pickers
  could not offer. Both now use the same document-level, filtered set.

* Bug fix (Features > Feature Roles > Preview): the "Documents per period" table
  of a date variable was ordered by ascending document count (`sort()` on a
  `table()` sorts the counts), not chronologically.

* Bug fix (Features > Feature Roles): aggregating a date-time variable by **Day**
  did not aggregate at all — `as.character()` on a `POSIXct` keeps the clock
  time, so every distinct second became its own period. It now formats to the
  calendar day.

* Bug fix (Features): `noGroupLabels()` did not reserve `time_agg`, the column
  the time role derives, although it reserves `keyness_group`. After assigning a
  date time role, tall's own aggregation key appeared as a user feature in
  Filters, Groups (where grouping by it would re-key the documents), the topic
  model covariates and Feature Roles itself, and its presence alone could unlock
  the FEATURES menu.

* Bug fix (Pre-processing > Multi-Word Creation / Multi-Word by a List): the
  words absorbed into a multi-word were left in the corpus as separate tokens.
  Merging "natural philosophy" produced the multi-word AND kept `philosophy` as
  its own selected NOUN, so every downstream count (vocabulary, keyness,
  networks, topic models) saw the constituents twice. Two chained causes, both
  fixed: (1) the vectorised rewrite of the tagging step nested what used to be
  two sequential `ifelse()` calls, and on an absorbed position `multiword` is
  `NA`, so the outer test evaluated to `NA` and the `NGRAM_MERGED` branch became
  unreachable — the tags now come from an explicit "was absorbed" flag, which
  also leaves a genuinely `NA` term alone; (2) `applyRake()` kept a
  `NGRAM_MERGED` row only when its own `multiword` was among the selected
  keywords, but that value is `NA` on those rows by construction, so they were
  always dropped and the join restored the original tags. The tagging step now
  reports which keyword absorbed each position (`mw_owner`) and `applyRake()`
  matches the constituents through it, so a partial selection keeps exactly the
  constituents of the keywords you ticked. Constituents are now tagged
  `NGRAM_MERGED` with `POSSelected = FALSE` — their text stays in the sentence,
  they are only excluded from the analyses — and `rakeReset()` ("Back") restores
  the previous selection from `POSSelected_original_nomultiwords`.

* Bug fix (KWIC > In-Document Plot > View): clicking the "View" button of a
  document whose annotation contains an unresolved lemma (`NA`) aborted the
  document modal with "missing value where TRUE/FALSE needed". Comparing an NA
  term with the query yields NA rather than FALSE, and `buildDocumentHTML()`
  branches on that value once per token. The match flag now treats an
  unresolvable term as a non-match, and the token branch uses `isTRUE()` so the
  helper stays total for any caller. Documents without NA terms render exactly
  as before, byte for byte. Note the table lists every selected document,
  including those whose frequency is NA, so the button was reachable on any
  corpus with an unresolved lemma (e.g. a single `upos = "X"` token).

* Bug fix (Overview > Morphological Features): `parseMorphFeatures()` built its
  match mask from the vector returned by `regmatches()`, which is *compacted* to
  the matching elements only. Being all-TRUE and shorter than the input, the mask
  was recycled when used to index the full-length result, so the extracted values
  were sprayed cyclically across every token instead of landing on the tokens
  that actually carry the feature. As a consequence no row was dropped as NA:
  the feature distribution bar chart reported counts inflated to the whole
  corpus token count (percentages were approximately right, absolute counts were
  not) and the feature x part-of-speech cross-tabulation was meaningless. The
  mask is now derived from `regexpr()` over the full column, so non-matching
  tokens correctly stay NA.

* Bug fix (Reinert clustering): in the greedy reallocation step (`switch_docs`),
  the document to move was looked up in the original CA ordering instead of the
  current group order. After the first switch this could move the wrong segment
  and made the clustering depend on the arbitrary sign of the SVD first axis
  (i.e., results could differ across platforms/LAPACK builds on the same data).
  Partitions computed with previous versions may change slightly.

# tall 1.0.0
* Changelog:                                      
                                         
  New Analyses                                                                    
   
  SVO Triplets (Subject-Verb-Object)                                              
  - New analysis for extracting subject-verb-object triplets from the dependency
  tree                                                                            
  - C++ backend (extract_svo.cpp) for high performance
  - Dedicated UI under Documents with Run/Export/Report, results table, Info &    
  References, TALL AI                                                             
  - Exported R function: extract_svo_triplets()                                   
                                                                                  
  Syntactic Complexity                                                            
  - New document-level syntactic complexity analysis based on dependency parsing  
  - C++ backend (syntactic_complexity.cpp) for computing syntactic metrics        
  - Dedicated UI under Documents with Run/Export/Report, results table, Info &    
  References, TALL AI                                                             
  - Exported R function: compute_syntactic_complexity()                           
                                                                                  
  Emotion Analysis (NRC EmoLex)                                                   
  - New emotion analysis based on the NRC Word-Emotion Association Lexicon        
  - Detection of 8 emotions: anger, anticipation, disgust, fear, joy, sadness,    
  surprise, trust                                                                 
  - Core functions: emotionAnalysis(), emotionBarChart(), emotionWordPlot(),      
  emotionHeatmap()                                                          
  - loadEmotionLexicon() for loading NRC data (English: mapped via Italian        
  lexicon's English Word column)                                          
  - Dedicated UI with 6 tabs: Emotion Distribution, Top Words by Emotion, Document
   Heatmap, Table, Info & References, TALL AI                                     
  - PNG export and Excel report integrated                                        
  - Gemini AI integration across all 5 switch points        
                                                                                  
  Noun Phrase Extraction                                                          
  - Noun phrase extraction via dependency tree                                    
  - C++ backend (extract_np.cpp)                                                  
  - Exported R function: extract_noun_phrases()             
                                                                                  
  ***
  Topic Modeling (major enhancement)                                              
                                                            
  - CTM (Correlated Topic Model) and STM (Structural Topic Model) added as new
  methods                                                                         
  - STM prevalence covariates with effect plots and regression coefficients
  - Model diagnostics tab: coherence, exclusivity, log-likelihood                 
  - Multi-metric comparison plot and consensus K recommendation
  - Estimated K automatically transferred to the model estimation panel           
                                                                                  
  ***
  Image Export System (complete rewrite)                                          
                                                                                  
  - Rewrite of plot2png() with DPI-aware rendering (biblioshiny approach)
  - Graph export settings: export/report DPI, height, aspect ratio                
  - Persistent settings, temporary file cleanup                                   
  - JavaScript canvas capture for visNetwork                                      
                                                                                  
  ***
  Homepage & References
                       
  - Complete homepage redesign in biblioshiny style
  - Updated citation to the SoftwareX paper (Aria et al., 2026)                   
  - Added open-access paper and supplementary material links                      
                                                                                  
  ***
  Settings & UI                                             
                                                                                                                  
  - Added icons to all tab panels across the app                                                                  
  - Added colored boxes next to hex color codes in Thematic Map tables                                            
  - Fixed Reinert/CA dendrogram "Words in Context" error (uninitialized variable)                                 
  - Fixed Import tab: table disappearing after Remove, "Back to original text", dfTag sync                        
  - Fixed Topic Modeling estimation error without options and conditional arrow buttons                           
  - Fixed report items overlapping with box title                                                                 
  - Fixed Similarity page margins and Embedding distribution spacing                                              
  - Fixed Supervised Classification header formatting                                                             
  - Moved Info & References to last tab in Thematic Map and Similarity                                            
  - Removed "in TALL" from all Info & References section titles                                                   
  - Updated KWIC Info styling and added Dependency reference in Word Network info                                 
  - Added morphological features stats to Overview report export                                                  
  - Added PoS selection guidance in Overview Frequency options                                                    
  - Updated Custom PoS List template and docs to clarify any custom tag is allowed  
  - Improved working folder selector with visual feedback
  - Reorganized settings layout (2x2 grid)
  - Time variable aggregation for Date types (day/week/month/quarter/year)        
  
  ***
  Dependency Parsing Features                               
                             
  - Dependency-based word network as an alternative to co-occurrence
  - Configurable dependency relation filters (all/noun_mod/SVO/custom)            
                                                                                  
  ***
  Architectural Refactoring                                                       
                                                            
  - Split tallFunctions.R into 10 thematic modules:
  tallSentiment.R, tallNetwork.R, tallTopicModel.R, tallEmbeddings.R,             
  tallOverview.R, tallUtils.R, tallVisualization.R, tallTextIO.R, tallNLP.R,      
  tallReport.R, tallLanguages.R                                                   
  - Extracted static CSS into www/tall-static.css                                 
  - Added www/tall-handlers.js for JavaScript handlers      
  - Modernized icons (migrated to FontAwesome 6)                                  
  - Cleaned up NAMESPACE (removed unnecessary exports)
  - Added test suite: test-calculate_ngram_is.R, test-process_multiwords.R,       
  test-reinert.R, test-txt_recode_fast.R                                          
  - Added stm dependency to DESCRIPTION
                                                                                  
  ***
  Code Quality
              
  - Propagated random seed to all analyses (topic models, networks, wordclouds,
  sampling)                                                                       
  - Default community detection switched from walktrap to louvain
  - Removed deprecated Grako module                                               
  - Renamed "Co-Word Analysis" to "Word Network Analysis"                         
  - Updated all Info & References sections                                        
  - Removed legacy code from tallEmbeddings.R and words.R (~1,160 lines removed)  
                                                                                  
  ***
  Bug Fixes                                                                       
                                                                                  
  - Fixed KWIC on filtered collections                      
  - Fixed overview with filtered corpus                                           
  - Fixed group handling in filters_groups.R
  - Minor fixes in import, edit, settings, collocation  

# tall 0.5.2

* New Features & UI/UX Improvements
  - Added "Add to Report" and "Export Images" buttons to Overview module
  - Added "Add to Report" and "Export Images" buttons to Keyness Analysis module
  - Added "Add to Report" and "Export" buttons to KWIC Network Analysis
  - Improved table layout and aesthetics across multiple modules

* Bug Fixes
  - Fixed issue in Edit -> Split functionality
  - Minor fixes in Import module and Settings

# tall 0.5.1
* Bug Fixes
  - Removed old Gemini models 1.5 and 2.0
  - Fixed issue in api key check
  - Fixed issue in synonims merging 

# tall 0.5.0
* New Features & Modules
  -Supervised Classification
      -New Module: Implemented a full Supervised Classification interface within the "Documents" section.
      - Workflow: Added support for two analysis approaches: DTM (Document-Term Matrix) and Embeddings.
  - Keyness Analysis
      - Two-Corpus Comparison: Added a new approach to compare a target corpus against a reference corpus.
      - Lemma Analysis: Enabled lemma-based analysis for the two-corpus comparison approach.
      - PoS Inheritance: Implemented setting inheritance, allowing Keyness to use the selection made in the "PoS Tag Selection" menu.
      - Visualization: Added the Frequency Context Plot.
  - Multi-Word & Text Processing
      - Methodology: Implemented Morrone's IS index as a new method for automatic Multi-Word creation.
      - Synonyms: Added new functionalities for synonym merging.
      - Normalization: Added a "normalize tokens" option in the PoS tagging menu.
  - Feature Roles: Added a dedicated Feature Roles menu.

* Performance & Code Refactoring
  - C++ Optimization: Rewrote txt_recode and applyRake functions in C++, achieving an approximate 200x speed increase.
  - Modular Architecture: Refactored ui.R and server.R by restructuring menus into modules imported from external files.
  - Pre-Processing: Refactored the layout of the Special Entities menu.

* UI/UX & Visualization Improvements
  - Navigation & Layout
      - Settings Menu: Relocated from the sidebar to the top-right header for better accessibility.
      - Menu Structure: Added labels to separate menu sections; renamed "Custom Term List" to "Custom POS".
      - Aesthetics: Improved the styling of the dropdown options menu (matching Biblioshiny aesthetics).
      - Back to Top: Implemented a "Back to top" scroll button across all pages.
  - Page Layouts: Enhanced layouts for: 
      - Welcome Page
      - Extractive Summarization
      - View Full Document modal
      - Team, Donation, and Credits tabs
      - PoS Tag List (in Selection Menu)

* Visualizations
  - Word Clouds: Implemented a new function to generate word clouds using ggplot2.
  - Correspondence Analysis: Added absolute contribution data to the plot tooltip.
  - Topic Modeling: Improved aesthetics and output for Topic Correlation.
  - Stats Box: Added a corpus size statistics box within the PoS Tag Selection menu.
  - DTformat: Added a parameter to toggle visibility between lemmas and tokens.

* Bug Fixes & Logic Updates
  - Split Logic: Fixed an issue where the split button removed the entire original document instead of just the split segment (corrected doc_id targeting).
  - Statistics: Corrected a calculation error in the Overview statistics.
  - Filters: Synchronized the effect of the Filters menu on the Feature Roles module.
  - Documentation: Added the official bibliographic reference to the CITATION file.


# tall 0.4.0
* Added support to multicolumn pdf
* Added Abstractive summarization
* Improved performance of TALL AI
* Improved performance of Thematic Mapping

# tall 0.3.0
* Added AI assistant called TALL AI

# tall 0.1.2
* Added a new option in Import: Now it is possible to import text file exported from biblioshiny

# tall 0.1.1
* Added new word in context analysis
* Solved a lot of minor issues

# tall 0.1.0

* Initial CRAN submission.

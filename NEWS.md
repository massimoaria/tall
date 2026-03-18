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
                                                                                  
  ---                                                       
  Topic Modeling (major enhancement)                                              
                                                            
  - CTM (Correlated Topic Model) and STM (Structural Topic Model) added as new
  methods                                                                         
  - STM prevalence covariates with effect plots and regression coefficients
  - Model diagnostics tab: coherence, exclusivity, log-likelihood                 
  - Multi-metric comparison plot and consensus K recommendation
  - Estimated K automatically transferred to the model estimation panel           
                                                                                  
  ---
  Image Export System (complete rewrite)                                          
                                                                                  
  - Rewrite of plot2png() with DPI-aware rendering (biblioshiny approach)
  - Graph export settings: export/report DPI, height, aspect ratio                
  - Persistent settings, temporary file cleanup                                   
  - JavaScript canvas capture for visNetwork                                      
                                                                                  
  ---                                                       
  Homepage & References
                       
  - Complete homepage redesign in biblioshiny style
  - Updated citation to the SoftwareX paper (Aria et al., 2026)                   
  - Added open-access paper and supplementary material links                      
                                                                                  
  ---                                                                             
  Settings & UI                                             

  - Improved working folder selector with visual feedback
  - Reorganized settings layout (2x2 grid)
  - Time variable aggregation for Date types (day/week/month/quarter/year)        
  
  ---                                                                             
  Dependency Parsing Features                               
                             
  - Dependency-based word network as an alternative to co-occurrence
  - Configurable dependency relation filters (all/noun_mod/SVO/custom)            
                                                                                  
  ---                                                                             
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
                                                                                  
  ---                                                       
  Code Quality
              
  - Propagated random seed to all analyses (topic models, networks, wordclouds,
  sampling)                                                                       
  - Default community detection switched from walktrap to louvain
  - Removed deprecated Grako module                                               
  - Renamed "Co-Word Analysis" to "Word Network Analysis"                         
  - Updated all Info & References sections                                        
  - Removed legacy code from tallEmbeddings.R and words.R (~1,160 lines removed)  
                                                                                  
  ---                                                                             
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

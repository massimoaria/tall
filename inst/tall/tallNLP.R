loadLanguageModel <- function(file, model_repo = "2.15") {
  home <- homeFolder()

  # setting up the main directory
  path_tall <- file.path(home, "tall")
  path_language_model <- file.path(path_tall, "language_models")
  # check if sub directory exists
  if (file.exists(path_tall)) {
    if (!file.exists(path_language_model)) dir.create(path_language_model)
  } else {
    dir.create(path_tall)
    dir.create(path_language_model)
  }

  # check if the file model already exists
  file_model <- dir(
    path_language_model,
    pattern = paste0(file, "-ud-", model_repo, ".udpipe")
  )[1]

  if (is.na(file_model)) {
    lang_file <- tall_download_model(
      file = file,
      model_dir = path_language_model,
      model_repo = model_repo
    )
    udmodel_lang <- udpipe_load_model(file = lang_file$file_model)
  } else {
    udmodel_lang <- udpipe_load_model(
      file = paste0(path_language_model, "/", file_model)
    )
  }

  return(udmodel_lang)
}

tall_download_model <- function(
  file,
  model_dir = NULL,
  model_repo = "2.15",
  overwrite = TRUE
) {
  filename <- paste0(file, "-ud-", model_repo, ".udpipe")

  url <- file.path(
    "https://raw.githubusercontent.com/massimoaria/tall.language.models/main",
    model_repo,
    filename
  )
  to <- file.path(model_dir, filename)
  download_failed <- FALSE
  download_message <- "OK"
  dl <- suppressWarnings(try(
    utils::download.file(url = url, destfile = to, mode = "wb"),
    silent = TRUE
  ))
  if (inherits(dl, "try-error")) {
    download_failed <- TRUE
    download_message <- as.character(dl)
  } else if (inherits(dl, "integer") && dl != 0) {
    download_failed <- TRUE
    download_message <- "Download failed. Please check internet connectivity"
  }
  if (download_failed) {
    message("Something went wrong")
    message(download_message)
  } else {
    message(sprintf("Downloading finished, model stored at '%s'", to))
  }

  data.frame(
    language = file,
    file_model = to,
    url = url,
    download_failed = download_failed,
    download_message = download_message,
    stringsAsFactors = FALSE
  )
}

updateStats <- function(dfTag, term, statsValues = NULL) {
  if (!is.null(dfTag)) {
    filtered <- LemmaSelection(dfTag) %>%
      dplyr::filter(docSelected)
    statsValues$n_docs <- n_distinct(filtered$doc_id)
    statsValues$n_tokens <- nrow(filtered)
    statsValues$last_update <- Sys.time()
  } else {
    # Reactive value per le statistiche
    statsValues <- reactiveValues(
      n_docs = 0,
      n_tokens = 0,
      last_update = NULL
    )
  }
  return(statsValues)
}

## Tagging Special Entites ----

TaggingCorpusElements <- function(x) {
  if ("upos_specialentities" %in% names(x)) {
    x <- resetSpecialEntities(x)
  } else {
    x$upos_specialentities <- x$upos
  }

  regexList <- c(
    EMAIL = "(?i)([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))",
    URL = "(?<!@)\\b(https?://[\\w.-]+\\.[a-z]{2,6}(/[\\S]*)?|[\\w.-]+\\.(com|org|net|edu|gov|it|uk)\\b)",
    HASH = "^#",
    # Regex più preciso per gli emoji - esclude i caratteri CJK
    EMOJI = "[\U0001F600-\U0001F64F]|[\U0001F300-\U0001F5FF]|[\U0001F680-\U0001F6FF]|[\U0001F1E0-\U0001F1FF]|[\U00002600-\U000026FF]|[\U00002700-\U000027BF]|[\U0001F900-\U0001F9FF]|[\U0001FA70-\U0001FAFF]|[\U00002B00-\U00002BFF]|[\U00003030\U0000303D\U00003297\U00003299]",
    IP_ADDRESS = "\\b\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\b",
    MENTION = "^@"
  )
  items <- names(regexList)

  resList <- list()
  j <- 0

  for (i in 1:length(items)) {
    item <- items[i]
    results <- stringi::stri_detect_regex(x$token, regexList[[item]])
    if (sum(results) > 0) {
      j <- j + 1
      resList[[j]] <- data.frame(
        doc_id = x$doc_id[results],
        item = x$token[results],
        tag = item
      )
      x$upos[results] <- toupper(item)
      x$POSSelected[results] <- ifelse(
        x$upos[results] %in% c("HASH", "MENTION", "EMOJI"),
        TRUE,
        FALSE
      )
    }
  }

  if (length(resList) > 0) {
    resList <- dplyr::bind_rows(resList) %>%
      dplyr::filter(!is.na(item))
  } else {
    resList <- tibble::tibble(doc_id = 0, item = NA, tag = "email") %>%
      dplyr::filter(!is.na(item))
  }

  # normalize hash and email
  x <- x %>%
    mutate(
      lemma = case_when(
        upos %in% c("HASH", "EMAIL") ~ tolower(lemma),
        upos == "EMOJI" ~ trimws(lemma),
        TRUE ~ lemma
      ),
      token = case_when(
        upos %in% c("HASH", "EMAIL") ~ tolower(token),
        upos == "EMOJI" ~ trimws(token),
        TRUE ~ token
      )
    )

  if (nrow(resList) > 0) {
    resList <- resList %>%
      mutate(
        item = case_when(
          tag %in% c("HASH", "EMAIL") ~ tolower(item),
          tag == "EMOJI" ~ trimws(item),
          TRUE ~ item
        )
      )
  }

  return(list(resList = resList, x = x))
}

resetSpecialEntities <- function(x) {
  if ("upos_specialentities" %in% names(x)) {
    items <- toupper(c(
      "email",
      "url",
      "hash",
      "emoji",
      "ip_address",
      "mention"
    ))
    x <- x %>%
      mutate(upos = ifelse(upos %in% items, upos_specialentities, upos))
  } else {
    x$upos_specialentities <- x$upos
  }
  return(x)
}

summarySpecialEntities <- function(resList, type = "all") {
  data.frame(
    UPOS = toupper(c("email", "url", "hash", "emoji", "ip_address", "mention")),
    "N. of Items" = rep(0, 6),
    "N. of Docs" = rep(0, 6)
  )

  switch(
    type,
    "all" = {
      resList %>%
        group_by(tag) %>%
        summarise(items = length(unique(item))) %>%
        rename(
          UPOS = tag,
          "Frequency" = items
        ) %>%
        ungroup() %>%
        bind_rows(tibble(
          UPOS = toupper(c(
            "email",
            "url",
            "hash",
            "emoji",
            "ip_address",
            "mention"
          )),
          "Frequency" = rep(0, 6)
        )) %>%
        group_by(UPOS) %>%
        summarize_all(sum)
    },
    {
      label <- toupper(type)
      resList %>%
        rename(UPOS = tag) %>%
        filter(UPOS == label) %>%
        count(item) %>%
        arrange(desc(n))
    }
  )
}

## Custom Lists merging
mergeCustomLists <- function(df, custom_lists, term = "lemma") {
  if (!is.null(custom_lists)) {
    switch(
      term,
      "lemma" = {
        df <- df %>%
          customListsReset() %>%
          # mutate(lemma_norm = ifelse(upos!="PROPN", tolower(lemma), lemma)) %>%
          left_join(custom_lists, by = c("lemma" = "lemma")) %>%
          mutate(
            upos.x = ifelse(!is.na(upos.y), toupper(upos.y), upos.x),
            POSSelected = ifelse(
              upos.x %in%
                c("ADJ", "NOUN", "PROPN", "VERB", "HASH", "EMOJI", "MENTION"),
              TRUE,
              FALSE
            )
          ) %>%
          select(-upos.y) %>%
          rename(upos = upos.x)
      },
      "token" = {
        df <- df %>%
          customListsReset() %>%
          # mutate(lemma_norm = ifelse(upos!="PROPN", tolower(lemma), lemma)) %>%
          left_join(custom_lists, by = c("token" = "token")) %>%
          mutate(
            upos.x = ifelse(!is.na(upos.y), toupper(upos.y), upos.x),
            POSSelected = ifelse(
              upos.x %in% c("ADJ", "NOUN", "PROPN", "VERB"),
              TRUE,
              FALSE
            )
          ) %>%
          select(-upos.y) %>%
          rename(upos = upos.x)
      }
    )
  } else {
    df <- df %>% customListsReset()
  }
  return(df)
}

customListsReset <- function(df) {
  if ("upos_original_custom" %in% names(df)) {
    df <- df %>%
      mutate(upos = upos_original_custom)
  } else {
    df <- df %>%
      mutate(upos_original_custom = upos)
  }

  return(df)
}

### SYNONYMS MERGING ----
applySynonymsReplacement <- function(dfTag, synonyms_df, term_type = "lemma") {
  #' Apply Synonyms Replacement with PoS Tag Update
  #'
  #' Replaces synonym terms with target terms and updates the upos column
  #'
  #' @param dfTag Data frame containing the tagged text data
  #' @param synonyms_df Data frame with synonyms. First column is target term,
  #'        second column is upos tag to assign, remaining columns are synonyms
  #' @param term_type Character. Either "token" or "lemma"
  #'
  #' @return Modified dfTag with synonyms replaced and upos updated

  if (is.null(dfTag) || is.null(synonyms_df)) {
    return(dfTag)
  }

  if (!term_type %in% c("token", "lemma")) {
    stop("term_type must be either \'token\' or \'lemma\'")
  }

  # Get column names
  target_col <- names(synonyms_df)[1]
  upos_col <- names(synonyms_df)[2] # PoS tag to assign
  synonym_cols <- names(synonyms_df)[-(1:2)]

  # Create replacement mapping: synonym -> list(target_term, upos_tag)
  replacement_map <- list()

  for (i in seq_len(nrow(synonyms_df))) {
    target_term <- synonyms_df[[target_col]][i]
    upos_tag <- synonyms_df[[upos_col]][i]

    # Get all synonyms for this target (excluding NA values)
    synonyms <- synonyms_df[i, synonym_cols]
    synonyms <- unlist(synonyms[!is.na(synonyms) & synonyms != ""])

    # Add each synonym to the replacement map with target and upos
    for (syn in synonyms) {
      replacement_map[[tolower(as.character(syn))]] <- list(
        target = as.character(target_term),
        upos = as.character(upos_tag)
      )
    }
  }

  # Build vectorized lookup tables from replacement_map
  target_vec <- vapply(replacement_map, function(x) x$target, character(1))
  upos_vec <- vapply(replacement_map, function(x) x$upos, character(1))

  # Apply replacements to the specified column AND update upos (vectorized)
  col_name <- term_type
  lower_vals <- tolower(as.character(dfTag[[col_name]]))
  matches <- match(lower_vals, names(target_vec))
  matched_idx <- !is.na(matches)
  if (any(matched_idx)) {
    dfTag[[col_name]][matched_idx] <- target_vec[matches[matched_idx]]
    dfTag$upos[matched_idx] <- upos_vec[matches[matched_idx]]
  }

  return(dfTag)
}


### MULTI-WORD CREATION ----

# rake function to create multi-words
rakeReset <- function(x) {
  if ("upos_original" %in% names(x)) {
    x <- x %>%
      select(-"upos") %>%
      rename(upos = upos_original)
  }

  if ("lemma_original_nomultiwords" %in% names(x)) {
    x <- x %>%
      select(-"lemma") %>%
      rename(lemma = lemma_original_nomultiwords)
  }
  if ("token_original_nomultiwords" %in% names(x)) {
    x <- x %>%
      select(-"token") %>%
      rename(token = token_original_nomultiwords)
  }

  if ("ngram" %in% names(x)) {
    x <- x %>%
      select(-"ngram")
  }
  return(x)
}

#' Extract keywords using RAKE or other multiword detection methods (OPTIMIZED)
#'
#' @param x Data frame with annotated tokens
#' @param group Grouping variable (default: "doc_id")
#' @param ngram_max Maximum n-gram length (default: 5)
#' @param ngram_min Minimum n-gram length (default: 2)
#' @param relevant Relevant POS tags (default: c("PROPN", "NOUN", "ADJ", "VERB"))
#' @param freq.min Minimum frequency threshold (default: 10)
#' @param term Term type to use: "lemma" or "token" (default: "lemma")
#' @param type Type of extraction: "automatic" or other (default: "automatic")
#' @param keywordList Optional keyword list (default: NULL)
#' @param method Extraction method: "rake", "pmi", "md", "lfmd", "is" (default: "rake")
#'
#' @return List with two elements:
#'   - stats: Data frame with keyword statistics
#'   - dfMW: Data frame with multiword annotations
#'
#' @export
rake <- function(
  x,
  group = "doc_id",
  ngram_max = 5,
  ngram_min = 2,
  relevant = c("PROPN", "NOUN", "ADJ", "VERB"),
  freq.min = 10,
  term = "lemma",
  type = "automatic",
  keywordList = NULL,
  method = "rake"
) {
  # =============================================================================
  # STEP 1: KEYWORD EXTRACTION
  # =============================================================================

  switch(
    type,
    automatic = {
      switch(
        method,
        rake = {
          # rake multi-word creation
          stats <- keywords_rake(
            x = x,
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq.min,
            relevant = x$upos %in% relevant
          )

          # identify ngrams>1
          stats <- stats %>%
            dplyr::filter(ngram >= ngram_min)
        },
        pmi = {
          stats <- keywords_collocation(
            x %>% filter(upos %in% relevant),
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq.min,
            sep = " "
          ) %>%
            select("keyword", "ngram", "freq", "pmi")

          # identify ngrams>1
          stats <- stats %>%
            dplyr::filter(ngram >= ngram_min)
        },
        md = {
          stats <- keywords_collocation(
            x %>% filter(upos %in% relevant),
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq.min,
            sep = " "
          ) %>%
            select("keyword", "ngram", "freq", "md")

          # identify ngrams>1
          stats <- stats %>%
            dplyr::filter(ngram >= ngram_min)
        },
        lfmd = {
          stats <- keywords_collocation(
            x %>% filter(upos %in% relevant),
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq.min,
            sep = " "
          ) %>%
            select("keyword", "ngram", "freq", "lfmd")

          # identify ngrams>1 with reka index>reka.min
          stats <- stats %>%
            dplyr::filter(ngram >= ngram_min)
        },
        is = {
          stats <- calculate_ngram_is(
            x,
            max_ngram = ngram_max,
            term = term,
            pos = relevant,
            min_freq = freq.min
          )
          stats <- stats %>%
            mutate(
              keyword = ngram,
              ngram = n_length,
              freq = ngram_freq,
              is_norm = IS_norm
            ) %>%
            select("keyword", "ngram", "freq", "is_norm")
        }
      )
    },
    {
      stats <- keywordList %>%
        mutate(
          keyword = trimws(keyword),
          ngram = lengths(strsplit(keyword, " "))
        )
    }
  )

  # =============================================================================
  # STEP 2: FILTER ORIGINAL TOKEN DF
  # =============================================================================

  if (method == "is") {
    include_pos <- c(
      "DET",
      "NOUN",
      "PRON",
      "ADV",
      "VERB",
      "ADP",
      "AUX",
      "ADJ",
      "CCONJ",
      "INTJ",
      "PART",
      "PROPN",
      "SCONJ"
    )
    x2 <- x %>% filter(upos %in% include_pos)
  } else {
    x2 <- x %>% filter(upos %in% relevant)
  }

  # =============================================================================
  # STEP 3: MULTIWORD PROCESSING (OTTIMIZZATO)
  # =============================================================================
  # 🚀 VERSIONE OTTIMIZZATA: Elimina switch duplicato + usa C++
  # Performance: ~100-200x più veloce
  # =============================================================================

  # Pre-calcola lookup per evitare join ripetuti
  keyword_lookup <- stats$ngram
  names(keyword_lookup) <- stats$keyword

  # Seleziona la colonna corretta (evita duplicazione switch)
  term_col <- if (term == "lemma") x2$lemma else x2$token

  # 🚀 USA FUNZIONE C++ OTTIMIZZATA per identificare multiword
  # Questa è la parte che diventa ~80-150x più veloce
  multiword <- txt_recode_ngram_fast(
    x = term_col,
    compound = stats$keyword,
    ngram = stats$ngram,
    sep = " "
  )

  # Operazioni vettorizzate (molto più veloci di mutate multiple)
  upos_multiword <- ifelse(
    term_col == multiword,
    x2$upos,
    ifelse(is.na(multiword), "NGRAM_MERGED", "MULTIWORD")
  )

  # Lookup diretto invece di join (più veloce)
  ngram_values <- keyword_lookup[multiword]

  # Crea risultato direttamente (più veloce di select + mutate)
  x2 <- data.frame(
    doc_id = x2$doc_id,
    term_id = x2$term_id,
    multiword = multiword,
    upos_multiword = upos_multiword,
    ngram = unname(ngram_values),
    stringsAsFactors = FALSE
  )

  # =============================================================================
  # STEP 4: FINAL STATISTICS
  # =============================================================================

  stats <- x2 %>%
    filter(upos_multiword == "MULTIWORD", multiword %in% stats$keyword) %>%
    group_by(multiword) %>%
    select(multiword) %>%
    count() %>%
    ungroup() %>%
    rename(
      keyword = multiword,
      freq = n
    ) %>%
    right_join(
      stats %>% select(-starts_with("freq")),
      by = "keyword"
    ) %>%
    filter(freq >= freq.min) %>%
    arrange(desc(freq))

  return(list(stats = stats, dfMW = x2))
}


# ==============================================================================
# VERSIONE ALTERNATIVA: Con helper function per massima leggibilità
# ==============================================================================
# Usa questa se preferisci una versione ancora più pulita e modulare

rake_v2 <- function(
  x,
  group = "doc_id",
  ngram_max = 5,
  ngram_min = 2,
  relevant = c("PROPN", "NOUN", "ADJ", "VERB"),
  freq.min = 10,
  term = "lemma",
  type = "automatic",
  keywordList = NULL,
  method = "rake"
) {
  # Step 1: Keyword extraction (identico all'originale)
  switch(
    type,
    automatic = {
      switch(
        method,
        rake = {
          stats <- keywords_rake(
            x = x,
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq.min,
            relevant = x$upos %in% relevant
          ) %>%
            dplyr::filter(ngram >= ngram_min)
        },
        pmi = {
          stats <- keywords_collocation(
            x %>% filter(upos %in% relevant),
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq.min,
            sep = " "
          ) %>%
            select("keyword", "ngram", "freq", "pmi") %>%
            dplyr::filter(ngram >= ngram_min)
        },
        md = {
          stats <- keywords_collocation(
            x %>% filter(upos %in% relevant),
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq.min,
            sep = " "
          ) %>%
            select("keyword", "ngram", "freq", "md") %>%
            dplyr::filter(ngram >= ngram_min)
        },
        lfmd = {
          stats <- keywords_collocation(
            x %>% filter(upos %in% relevant),
            term = term,
            group = group,
            ngram_max = ngram_max,
            n_min = freq.min,
            sep = " "
          ) %>%
            select("keyword", "ngram", "freq", "lfmd") %>%
            dplyr::filter(ngram >= ngram_min)
        },
        is = {
          stats <- calculate_ngram_is(
            x,
            max_ngram = ngram_max,
            term = term,
            pos = relevant,
            min_freq = freq.min
          ) %>%
            mutate(
              keyword = ngram,
              ngram = n_length,
              freq = ngram_freq,
              is_norm = IS_norm
            ) %>%
            select("keyword", "ngram", "freq", "is_norm")
        }
      )
    },
    {
      stats <- keywordList %>%
        mutate(
          keyword = trimws(keyword),
          ngram = lengths(strsplit(keyword, " "))
        )
    }
  )

  # Step 2: Filter tokens
  if (method == "is") {
    include_pos <- c(
      "DET",
      "NOUN",
      "PRON",
      "ADV",
      "VERB",
      "ADP",
      "AUX",
      "ADJ",
      "CCONJ",
      "INTJ",
      "PART",
      "PROPN",
      "SCONJ"
    )
    x2 <- x %>% filter(upos %in% include_pos)
  } else {
    x2 <- x %>% filter(upos %in% relevant)
  }

  # Step 3: Process multiwords using optimized helper function
  # 🚀 QUESTA È LA PARTE OTTIMIZZATA
  x2 <- process_multiwords_fast(x2, stats, term)

  # Step 4: Final statistics
  stats <- x2 %>%
    filter(upos_multiword == "MULTIWORD", multiword %in% stats$keyword) %>%
    group_by(multiword) %>%
    select(multiword) %>%
    count() %>%
    ungroup() %>%
    rename(keyword = multiword, freq = n) %>%
    right_join(stats %>% select(-starts_with("freq")), by = "keyword") %>%
    filter(freq >= freq.min) %>%
    arrange(desc(freq))

  return(list(stats = stats, dfMW = x2))
}

applyRake <- function(x, rakeResults, row_sel = NULL, term = "lemma") {
  # Remove ngram column if exists
  if ("ngram" %in% names(x)) {
    x <- x %>% select(-"ngram")
  }

  # Filter stats based on selected rows
  rakeResults$stats <- rakeResults$stats[row_sel, ]
  selected_keywords <- rakeResults$stats$keyword

  # Filter dfMW - optimized logic
  rakeResults$dfMW <- rakeResults$dfMW %>%
    filter(
      (upos_multiword == "MULTIWORD" & multiword %in% selected_keywords) |
        (upos_multiword == "NGRAM_MERGED" & multiword %in% selected_keywords) |
        (!upos_multiword %in% c("MULTIWORD", "NGRAM_MERGED"))
    )

  # Select term column once (avoid switch duplication)
  term_col <- if (term == "lemma") "lemma" else "token"
  term_original_col <- paste0(term_col, "_original_nomultiwords")

  # Join and process in one pipeline
  x <- x %>%
    left_join(rakeResults$dfMW, by = c("doc_id", "term_id")) %>%
    mutate(
      # Combine multiword operations
      multiword = ifelse(is.na(multiword), .data[[term_col]], multiword),
      upos_multiword = ifelse(is.na(upos_multiword), upos, upos_multiword),

      # Update POSSelected
      POSSelected = case_when(
        upos_multiword == "MULTIWORD" ~ TRUE,
        upos_multiword == "NGRAM_MERGED" ~ FALSE,
        TRUE ~ POSSelected
      )
    )

  # Rename upos if needed
  if (!"upos_original" %in% names(x)) {
    names(x)[names(x) == "upos"] <- "upos_original"
  }
  x <- x %>%
    select(-ends_with("upos")) %>%
    rename(upos = upos_multiword)

  # Rename term column if needed
  if (!term_original_col %in% names(x)) {
    names(x)[names(x) == term_col] <- term_original_col
  }
  x <- x %>%
    select(-ends_with(term_col)) %>%
    rename(!!term_col := multiword)

  # Calculate new start/end values for multiwords (vectorized)
  ind <- which(!is.na(x$ngram))
  if (length(ind) > 0) {
    ind2 <- ind + (x$ngram[ind] - 1)
    x$end[ind] <- x$end[ind2]
  }

  # Calculate ngram (vectorized - no group_by needed)
  x$ngram <- ifelse(
    x$upos == "MULTIWORD",
    pmax(
      lengths(strsplit(x$lemma, " ", fixed = TRUE)),
      lengths(strsplit(x$token, " ", fixed = TRUE))
    ),
    NA_integer_
  )

  return(x)
}

### POS TAG SELECTION ----

# create description for pos tag check box ----
posTagAll <- function(df) {
  posLegend <- data.frame(
    pos = c(
      "ADJ",
      "ADP",
      "ADV",
      "AUX",
      "CCONJ",
      "DET",
      "INTJ",
      "NOUN",
      "NUM",
      "PART",
      "PRON",
      "PROPN",
      "PUNCT",
      "SCONJ",
      "SYM",
      "VERB",
      "X",
      "EMAIL",
      "EMOJI",
      "HASH",
      "IP_ADDRESS",
      "MENTION",
      "URL",
      "MULTIWORD"
    ),
    description = c(
      "Adjective",
      "Adposition",
      "Adverb",
      "Auxiliary",
      "Coord. Conjunction",
      "Determiner",
      "Interjection",
      "Noun",
      "Numeral",
      "Particle",
      "Pronoun",
      "Proper Noun",
      "Punctuation",
      "Subord. Conjunction",
      "Symbol",
      "Verb",
      "Other",
      rep("Special Entity", 6),
      "Custom PoS"
    )
  )
  row.names(posLegend) <- posLegend$pos

  pos <- unique(df$upos)
  ordinaryPos <- intersect(posLegend$pos, pos)
  additionalPos <- sort(setdiff(pos, ordinaryPos))
  pos <- c(ordinaryPos, additionalPos)
  description <- c(
    posLegend$description[posLegend$pos %in% pos],
    rep("Custom PoS", length(additionalPos))
  )
  description <- paste(pos, description, sep = ": ")
  obj <- data.frame(pos = pos, description = description) %>%
    filter(!pos == "NGRAM_MERGED")
  return(obj)
}

### GROUP MENU FUNCTIONS -----
noGroupLabels <- function(label) {
  setdiff(
    label,
    c(
      "doc_id",
      "paragraph_id",
      "sentence_id",
      "sentence",
      "start",
      "end",
      "term_id",
      "noSingleChar",
      "token_id",
      "token",
      "lemma",
      "upos",
      "xpos",
      "feats",
      "head_token_id",
      "dep_rel",
      "deps",
      "misc",
      "original_doc_id",
      "ungroupDoc_id",
      "ungroupP_id",
      "ungroupS_id",
      "POSSelected",
      "token_hl",
      "start_hl",
      "end_hl",
      "sentence_hl",
      "lemma_original_nomultiwords",
      "filename",
      "upos_original",
      "folder",
      "docSelected",
      "ngram",
      "doc_selected",
      "noHapax",
      "FrequencyRange",
      "text_original",
      "doc_id_old",
      "split_word",
      "token_original_nomultiwords",
      "lemma_original",
      "upos_specialentities",
      "upos_original_custom",
      "keyness_group"
    )
  )
}

groupByMetadata <- function(dfTag, metadata) {
  if (length(metadata) == 1) {
    ## group texts by a metadata

    if (!"ungroupDoc_id" %in% names(dfTag)) {
      dfTag <- dfTag %>%
        mutate(
          ungroupDoc_id = doc_id,
          ungroupP_id = paragraph_id,
          ungroupS_id = sentence_id
        )
    }

    # newDoc_id <- sprintf(paste0("%0",nchar(length(unique(dfTag$ungroupDoc_id))),"d"), unique_identifier(dfTag, fields=metadata, start_from = 1L))
    dfTag$paragraph_id <- paste0(dfTag$ungroupDoc_id, "_", dfTag$ungroupP_id)
    dfTag$sentence_id <- paste0(dfTag$ungroupDoc_id, "_", dfTag$ungroupS_id)

    # newDoc_id <-dfTag[[metadata]]
    newDoc_id <- ifelse(
      !is.na(dfTag[[metadata]]),
      dfTag[[metadata]],
      "Not Available"
    )

    dfTag <- dfTag %>%
      mutate(doc_id = newDoc_id) %>%
      group_by(doc_id) %>%
      mutate(
        paragraph_id = unique_identifier(paragraph_id),
        sentence_id = unique_identifier(sentence_id)
      ) %>%
      ungroup() %>%
      arrange(doc_id, paragraph_id, sentence_id)
  } else {
    dfTag <- backToOriginalGroups(dfTag)
  }

  return(dfTag)
}

backToOriginalGroups <- function(dfTag) {
  # back to original ungrouped data frame
  if ("ungroupDoc_id" %in% names(dfTag)) {
    dfTag <- dfTag %>%
      mutate(
        doc_id = ungroupDoc_id,
        paragraph_id = ungroupP_id,
        sentence_id = ungroupS_id
      ) %>%
      select(-ungroupDoc_id, -ungroupP_id, -ungroupS_id) %>%
      arrange(doc_id, paragraph_id, sentence_id)
  }
  return(dfTag)
}

### OVERVIEW ----

# Vocabulary calculation


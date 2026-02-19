trim_text_columns <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      ~ trimws(.x)
    ))
}


clean_text <- function(
  df,
  text_column = "text",
  add_space = TRUE,
  remove_quotes = TRUE,
  punctuation_marks = c(
    ",",
    ";",
    "!",
    "_",
    "»",
    "«",
    "&",
    "(",
    ")",
    "--",
    "..",
    "...",
    "....",
    "--",
    "---",
    ".#",
    "“",
    "‘",
    "”",
    "’",
    "??",
    "???"
  )
) {
  # Improved emoji regex pattern to capture Unicode emojis
  EMOJI <- "[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F700-\U0001F77F\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\U0001FA00-\U0001FA6F\U0001FA70-\U0001FAFF\U00002702-\U000027B0\U000024C2-\U0001F251]"
  # Sort punctuation marks by length (longest first) to prioritize sequences
  punctuation_marks <- punctuation_marks[order(
    nchar(punctuation_marks),
    decreasing = TRUE
  )]
  # Escape special regex characters
  punctuation_marks <- sapply(punctuation_marks, function(x) {
    gsub("([\\^$.|?*+(){}])", "\\\\\\1", x)
  })
  # Create a regex pattern for punctuation sequences
  punctuation_regex <- paste0(
    "(",
    paste0(punctuation_marks, collapse = "|"),
    ")"
  )

  df %>%
    mutate(
      !!text_column := case_when(
        add_space & remove_quotes ~
          {
            text_cleaned <- stringr::str_replace_all(
              .data[[text_column]],
              '[\"]',
              ""
            ) # Remove quotes
            text_cleaned <- stringr::str_replace_all(
              text_cleaned,
              "([a-z])\\.([A-Z])",
              "\\1. \\2"
            ) # Add space between lowercase.Uppercase
            text_cleaned <- stringr::str_replace_all(
              text_cleaned,
              punctuation_regex,
              " \\1 "
            ) # Add spaces around punctuation
            text_cleaned <- stringr::str_replace_all(
              text_cleaned,
              EMOJI,
              " \\0 "
            ) # Add spaces around emojis

            # Remove extra spaces but preserve newlines
            text_cleaned <- stringr::str_replace_all(
              text_cleaned,
              "[ \t]+",
              " "
            ) # Replace multiple spaces/tabs with single space
            stringr::str_replace_all(text_cleaned, "[ \t]*\n[ \t]*", "\n") # Clean spaces around newlines
          },
        add_space ~
          {
            text_cleaned <- stringr::str_replace_all(
              .data[[text_column]],
              "([a-z])\\.([A-Z])",
              "\\1. \\2"
            ) # Add space between lowercase.Uppercase
            text_cleaned <- stringr::str_replace_all(
              text_cleaned,
              punctuation_regex,
              " \\1 "
            )
            text_cleaned <- stringr::str_replace_all(
              text_cleaned,
              EMOJI,
              " \\0 "
            )
            # Remove extra spaces but preserve newlines
            text_cleaned <- stringr::str_replace_all(
              text_cleaned,
              "[ \t]+",
              " "
            ) # Replace multiple spaces/tabs with single space
            stringr::str_replace_all(text_cleaned, "[ \t]*\n[ \t]*", "\n") # Clean spaces around newlines
          },
        remove_quotes ~
          {
            stringr::str_replace_all(.data[[text_column]], '[\"|\']', "")
          },
        TRUE ~ .data[[text_column]]
      )
    )
}

### DATA ----
# IMPORT TEXT FUNCTIONS ----

getFileNameExtension <- function(fn) {
  # remove a path
  splitted <- strsplit(x = fn, split = "/")[[1]]
  # or use .Platform$file.sep in stead of '/'
  fn <- splitted[length(splitted)]
  ext <- ""
  splitted <- strsplit(x = fn, split = "\\.")[[1]]
  l <- length(splitted)
  if (l > 1 && sum(splitted[1:(l - 1)] != "")) {
    ext <- splitted[l]
  }
  # the extention must be the suffix of a non-empty name
  ext
}

read_files <- function(
  files,
  ext = c("txt", "csv", "xlsx", "pdf"),
  subfolder = TRUE,
  line_sep = ","
) {
  # files <- list.files(path=path, pattern = paste0(".",ext,"$"), recursive = subfolder)
  if (is.null(files)) {
    return(data.frame(doc_id = NA, text = NA, folder = NA))
  }

  if ("datapath" %in% names(files)) {
    doc_id <- files$name
    file <- files$datapath
    folder <- NA
  }

  if (getFileNameExtension(file[1]) == "zip") {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    zip_file <- unzip(file[1])
    zip_file <- zip_file[
      (substr(zip_file, nchar(zip_file) - nchar(ext) + 1, nchar(zip_file)) ==
        ext)
    ]
    file <- zip_file[regexpr("__MACOSX", zip_file) == -1]
    doc_id <- unlist(lapply(strsplit(file, "/"), function(l) {
      l[length(l)]
    }))
    folder <- unlist(lapply(strsplit(file, "/"), function(l) {
      l[length(l) - 1]
    }))
  }

  switch(
    ext,
    txt = {
      ## detect text encoding for each file
      df <- readtext::readtext(file)
      encod <- suppressMessages(readtext::encoding(df, verbose = FALSE)$all)
      ## read txt files using the right encoding
      df <- data.frame(
        doc_id = doc_id,
        text = NA,
        folder = folder,
        file = file,
        encod = encod
      ) %>%
        group_by(doc_id) %>%
        mutate(
          text = readtext::readtext(file, encoding = encod, verbosity = 0)$text
        ) %>%
        select(-c(file, encod))
    },
    csv = {
      listdf <- list()
      for (i in seq_len(length(file))) {
        # listdf[[i]] <- readtext::readtext(file[i], fill=TRUE, text_field="text", quote='"') %>%
        #   mutate(doc_id = doc_id[i])
        listdf[[i]] <- read_delim(file[i], delim = line_sep, quote = '"') %>%
          mutate(filename = doc_id[i])
      }

      df <- do.call(rbind, listdf)
    },
    xlsx = {
      listdf <- list()
      for (i in seq_len(length(file))) {
        # listdf[[i]] <- readtext::readtext(file[i], fill=TRUE, text_field="text", quote='"') %>%
        #   mutate(doc_id = doc_id[i])
        listdf[[i]] <- readxl::read_excel(file[i], col_types = "text") %>%
          mutate(filename = doc_id[i])
      }
      df <- do.call(rbind, listdf)
    },
    pdf = {
      listdf <- list()
      for (i in seq_len(length(file))) {
        # listdf[[i]] <- readtext::readtext(file[i], fill=TRUE, text_field="text", quote='"') %>%
        #   mutate(doc_id = doc_id[i])
        listdf[[i]] <- data.frame(
          text = pdf2txt_auto(file[i]),
          filename = doc_id[i]
        )
      }
      df <- do.call(rbind, listdf)
    }
  )
  if ("doc_id" %in% names(df)) {
    if (sum(duplicated(df$doc_id), na.rm = T) > 0) {
      num <- sprintf(paste0("%0", nchar(nrow(df)), "d"), 1:nrow(df))
      df <- df %>%
        mutate(
          original_doc_id = doc_id,
          doc_id = paste0("doc_", num)
        ) %>%
        select(doc_id, everything())
    }
  } else {
    num <- sprintf(paste0("%0", nchar(nrow(df)), "d"), 1:nrow(df))
    df <- df %>%
      mutate(doc_id = paste0("doc_", num)) %>%
      select(doc_id, everything())
  }

  df <- df %>%
    mutate(doc_selected = TRUE)

  return(df)
}

## PDF to TEXT ----
pdf2txt_multicolumn_safe <- function(
  file,
  column_threshold = NULL,
  preserve_structure = TRUE
) {
  # Check compatibility for poppler_config
  has_poppler_config <- exists(
    "poppler_config",
    where = asNamespace("pdftools"),
    mode = "function"
  )

  if (has_poppler_config) {
    if (!pdftools::poppler_config()$has_pdf_data) {
      message(
        "Pdf import feature requires a recent version of libpoppler. Please install it."
      )
      return(NA)
    }
  }

  # Try using pdf_data to handle multi-column documents
  tryCatch(
    {
      # Extract data with positions
      data_list <- pdftools::pdf_data(file)

      all_text <- c()

      for (page_num in seq_along(data_list)) {
        page_data <- data_list[[page_num]]

        if (nrow(page_data) == 0) {
          next
        }

        # Automatically determine column threshold if not specified
        if (is.null(column_threshold)) {
          # Analyze distribution of x positions to detect multiple columns
          x_positions <- page_data$x

          # If there are enough different positions, try to detect multiple columns
          if (length(unique(x_positions)) > 20) {
            # Use clustering to identify potential columns
            tryCatch(
              {
                clusters <- kmeans(x_positions, centers = 2, nstart = 10)
                cluster_centers <- sort(clusters$centers[, 1])

                # Use midpoint between cluster centers as threshold
                column_threshold <- mean(cluster_centers)
              },
              error = function(e) {
                # If clustering fails, use page midpoint
                column_threshold <- (max(page_data$x) + min(page_data$x)) / 2
              }
            )
          } else {
            # For documents with few different x positions, use page midpoint
            column_threshold <- (max(page_data$x) + min(page_data$x)) / 2
          }
        }

        # Separate columns based on x position
        left_column <- page_data[page_data$x < column_threshold, ]
        right_column <- page_data[page_data$x >= column_threshold, ]

        # Check if there are actually two significant columns
        if (nrow(left_column) < 5 || nrow(right_column) < 5) {
          # Probably single-column document, treat everything together
          page_data <- page_data[order(page_data$y, page_data$x), ]
          page_text <- reconstruct_text_structured(
            page_data,
            preserve_structure
          )
        } else {
          # Sort by y (top to bottom) and then by x within each column
          left_column <- left_column[order(left_column$y, left_column$x), ]
          right_column <- right_column[order(right_column$y, right_column$x), ]

          # Reconstruct text for each column maintaining structure
          left_text <- reconstruct_text_structured(
            left_column,
            preserve_structure
          )
          right_text <- reconstruct_text_structured(
            right_column,
            preserve_structure
          )

          # Combine columns
          if (preserve_structure) {
            page_text <- paste(left_text, right_text, sep = "\n\n")
          } else {
            page_text <- paste(left_text, right_text, sep = " ")
          }
        }

        all_text <- c(all_text, page_text)
      }

      # Final post-processing
      if (preserve_structure) {
        txt <- paste(all_text, collapse = "\n\n")

        # Improve detection of paragraphs and sections
        # Identify numbered titles like "1. Introduction", "2.1. Internal processing"
        txt <- gsub(
          "([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})",
          "\n\n\\1",
          txt
        )

        # Identify isolated uppercase sections
        txt <- gsub("\\s+([A-Z][A-Z\\s]{10,60})\\s+", "\n\n\\1\n\n", txt)

        # Separate paragraphs after complete sentences
        txt <- gsub("([.!?])\\s+([A-Z][a-z])", "\\1\n\n\\2", txt)

        # Clean triple \n
        txt <- gsub("\\n{3,}", "\n\n", txt)
      } else {
        txt <- paste(all_text, collapse = " ")
      }

      # Remove line-ending hyphens but preserve structure
      txt <- gsub("-\\s*\n", "", txt)
      txt <- gsub("-\\s+", "", txt)

      # Normalize spaces
      if (preserve_structure) {
        txt <- gsub("[ \t]+", " ", txt) # Normalize spaces and tabs
        txt <- gsub("\\n ", "\n", txt) # Remove spaces after \n
      } else {
        txt <- gsub("\\s+", " ", txt)
      }

      # Remove leading and trailing spaces
      txt <- trimws(txt)

      return(txt)
    },
    error = function(e) {
      message("pdf_data failed, falling back to pdf_text method: ", e$message)

      # Fallback to original function with structural improvements
      pages <- pdftools::pdf_length(file)
      txt <- pdftools::pdf_text(file)

      if (preserve_structure) {
        # Improve structure preservation in fallback
        # Identify numbered titles
        txt <- gsub(
          "([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})",
          "\n\n\\1",
          txt
        )

        # Preserve existing paragraphs
        txt <- gsub("\\n\\s*\\n", "\n\n", txt)

        # Separate sentences into new paragraphs when appropriate
        txt <- gsub("([.!?])\\s*\n\\s*([A-Z])", "\\1\n\n\\2", txt)

        # Remove \n that are not at sentence endings but preserve structure
        txt <- gsub("(?<![.!?\\n])\\n(?![A-Z0-9\\n])", " ", txt, perl = TRUE)

        # Combine pages
        txt <- paste(txt, collapse = "\n\n")
      } else {
        # Original version
        txt <- gsub("(?<![\\s\\.])\\n(?!\\s)", " ", txt, perl = TRUE)
        txt <- gsub("\n  ", "\n\n", txt)
        txt <- paste(txt, collapse = " ")
      }

      # Remove word separation hyphens
      txt <- gsub("-\\s", "", txt)

      return(txt)
    }
  )
}

# Simplified helper function that doesn't use non-existent columns
reconstruct_text_structured <- function(
  column_data,
  preserve_structure = TRUE
) {
  if (nrow(column_data) == 0) {
    return("")
  }

  # Group words by line based on similar y positions
  tolerance <- 4
  column_data$line <- round(column_data$y / tolerance) * tolerance

  # Check which columns actually exist
  available_cols <- names(column_data)

  # Use only available information to identify structures
  if ("height" %in% available_cols) {
    column_data$font_size <- column_data$height
  } else {
    column_data$font_size <- 12 # default
  }

  # Split data by line
  lines <- split(column_data, column_data$line)

  # Reconstruct each line
  line_results <- lapply(lines, function(line) {
    line <- line[order(line$x), ] # Sort by x position

    # Reconstruct line text
    line_text <- paste(line$text, collapse = " ")
    line_text <- trimws(line_text)

    # Line characteristics to identify titles/special structures
    avg_font_size <- mean(line$font_size, na.rm = TRUE)
    is_short <- nchar(line_text) < 80
    is_caps <- grepl("^[A-Z\\s\\d\\.\\-]+$", line_text)
    starts_with_number <- grepl("^\\d+\\.", line_text)
    starts_with_section <- grepl("^\\d+\\.\\d+", line_text)

    # Identify potential titles or special structures
    is_title <- (is_short &&
      (is_caps || starts_with_number || starts_with_section))

    return(list(
      text = line_text,
      y = min(line$y),
      is_title = is_title,
      font_size = avg_font_size,
      starts_with_number = starts_with_number
    ))
  })

  # Remove empty lines
  line_results <- line_results[sapply(line_results, function(x) {
    nchar(x$text) > 0
  })]

  # Sort lines from top to bottom
  line_results <- line_results[order(sapply(line_results, function(x) x$y))]

  if (!preserve_structure) {
    # Simple mode: join everything with spaces
    result <- paste(sapply(line_results, function(x) x$text), collapse = " ")
  } else {
    # Structured mode: preserve paragraphs and titles
    result_parts <- c()

    for (i in seq_along(line_results)) {
      current_line <- line_results[[i]]
      line_text <- current_line$text

      if (nchar(line_text) == 0) {
        next
      }

      # Add appropriate separators
      if (i == 1) {
        # First line
        result_parts <- c(result_parts, line_text)
      } else {
        prev_line <- line_results[[i - 1]]

        # Determine separator type
        if (current_line$is_title) {
          # Titles: double \n before
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (prev_line$is_title) {
          # After a title: double \n
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (
          grepl("[.!?]\\s*$", prev_line$text) &&
            grepl("^[A-Z]", line_text) &&
            !grepl("^[A-Z][a-z]+\\s+[a-z]", line_text)
        ) {
          # End of sentence + start with capital (but not normal continuation)
          result_parts <- c(result_parts, "\n\n", line_text)
        } else {
          # Continuation: single space
          result_parts <- c(result_parts, " ", line_text)
        }
      }
    }

    result <- paste(result_parts, collapse = "")

    # Post-processing to improve structure
    result <- gsub("\\s+", " ", result) # Normalize spaces
    result <- gsub("\\n\\s+", "\n", result) # Remove spaces after \n
    result <- gsub("\\n{3,}", "\n\n", result) # Max 2 consecutive \n
  }

  result <- trimws(result)
  return(result)
}

# Automatic wrapper function
pdf2txt_auto <- function(file, preserve_structure = TRUE) {
  # First try multi-column method
  result <- pdf2txt_multicolumn_safe(
    file,
    preserve_structure = preserve_structure
  )

  # If result seems malformed, try original method
  if (is.na(result) || nchar(result) < 100) {
    message(
      "Multi-column method failed or returned short text, trying original method..."
    )

    # Complete fallback to original method with improvements
    tryCatch(
      {
        pages <- pdftools::pdf_length(file)
        txt <- pdftools::pdf_text(file)

        if (preserve_structure) {
          # Better structure preservation in fallback
          # Identify numbered titles
          txt <- gsub(
            "([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Za-z][A-Za-z\\s]{3,50})",
            "\n\n\\1\n\n",
            txt,
            perl = TRUE
          )

          # Preserve existing paragraphs
          txt <- gsub("\\n\\s*\\n", "\n\n", txt)

          # Separate sentences into paragraphs when they start with capital
          txt <- gsub(
            "([.!?])\\s*\n\\s*([A-Z][a-z])",
            "\\1\n\n\\2",
            txt,
            perl = TRUE
          )

          # Remove internal \n but preserve structure
          txt <- gsub("(?<![.!?\\n])\\n(?![A-Z0-9\\n])", " ", txt, perl = TRUE)

          txt <- paste(txt, collapse = "\n\n")
        } else {
          txt <- gsub("(?<![\\s\\.])\\n(?!\\s)", " ", txt, perl = TRUE)
          txt <- gsub("\n  ", "\n\n", txt)
          txt <- paste(txt, collapse = " ")
        }

        txt <- gsub("-\\s", "", txt)
        return(txt)
      },
      error = function(e) {
        message("All methods failed: ", e$message)
        return(NA)
      }
    )
  }

  return(result)
}

## Remove HTML TAGS ----

removeHTMLTags <- function(text) {
  text <- text %>%
    gsub("&nbsp;|&amp;|&current;|&trade;", " ", .) %>%
    trimws() %>%
    gsub("<br>", "\\\n", .) %>%
    gsub("</p>", "\\\n", .) %>%
    gsub("<.*?>", "", .)
}

### download sample data
loadSampleCollection <- function(sampleName) {
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

  switch(
    sampleName,
    bibliometrix = {
      # check if the file model already exists
      file_lang <- dir(path_language_model, pattern = "tall_bibliometrix.tall")[
        1
      ]
      url <- paste0(
        "https://raw.githubusercontent.com/massimoaria/tall.language.models/main/sample.data/tall_bibliometrix.tall"
      )

      destfile <- paste0(path_language_model, "/tall_bibliometrix.tall")
      file <- paste0(path_language_model, "/tall_bibliometrix.tall")
    },
    bbc = {
      file_lang <- dir(path_language_model, pattern = "bbc.zip")[1]
      url <- paste0(
        "https://raw.githubusercontent.com/massimoaria/tall.language.models/main/sample.data/bbc.zip"
      )
      destfile <- paste0(path_language_model, "/bbc.zip")
      file <- paste0(path_language_model, "/bbc.zip")
    },
    usairlines = {
      file_lang <- dir(path_language_model, pattern = "usairlines.zip")[1]
      url <- paste0(
        "https://raw.githubusercontent.com/massimoaria/tall.language.models/main/sample.data/usairlines.zip"
      )
      destfile <- paste0(path_language_model, "/usairlines.zip")
      file <- paste0(path_language_model, "/usairlines.zip")
    }
  )

  if (is.na(file_lang)) {
    switch(
      Sys.info()[["sysname"]],
      Windows = {
        download.file(url = url, destfile = destfile, mode = "wb")
      },
      {
        download.file(url = url, destfile = destfile)
      }
    )
  }

  return(file)
}

## Wikipedia download sample pages ----
wikiSearch <- function(term, n = 10) {
  if (!inherits(n, "numeric")) {
    n <- 10
  }

  # select n of results
  search_url <- paste0(
    "https://en.wikipedia.org/w/api.php?action=query&generator=search&gsrlimit=",
    n,
    "&prop=extracts&exintro&explaintext&exlimit=max&format=json&gsrsearch="
  )

  term <- gsub("–", " ", gsub("\\s", "_", term))

  result <- jsonlite::fromJSON(paste0(
    search_url,
    term
  ))

  if (!"query" %in% names(result)) {
    return(NULL)
  }

  title <- c()
  abstract <- c()
  url <- c()

  for (i in 1:length(result$query$pages)) {
    wikidata <- result$query$pages[i]
    names(wikidata) <- "content"
    page_abstract <- gsub("<.*?>", "", wikidata$content$extract)

    abstract <- append(abstract, page_abstract)
    title <- append(title, wikidata$content$title)
    url <- append(
      url,
      paste0(
        "https://en.wikipedia.org/wiki/",
        gsub("\\s", "_", wikidata$content$title)
      )
    )
  }
  url <-
    paste0(
      '<a href=\"',
      url,
      '\" target=\"_blank\">',
      url,
      "</a>"
    )

  df <- data.frame(title, abstract, url, text = NA, selected = TRUE)
  return(df)
}

wikiExtract <- function(df) {
  items <- gsub("\\s", "_", df$title)
  for (i in 1:length(items)) {
    if (df$selected[i]) {
      title <- items[i]
      # print(title)
      result <- jsonlite::fromJSON(paste0(
        "https://en.wikipedia.org/w/api.php?action=query&titles=",
        title,
        "&prop=extracts&redirects=&format=json"
      ))
      names(result$query$pages) <- "content"
      text <- result$query$pages$content$extract
      ## remove Latex equations and textstyle
      df$text[i] <- gsub("<.*?>", "", text) %>%
        gsub("\\{\\\\displaystyle.*?\\}\\n", "", .) %>%
        gsub("\\\\textstyle", " ", .)
    }
  }
  return(df)
}

### REBUILD ORIGINAL DOCUMENTS -----

rebuild_documents <- function(df) {
  columns <- intersect(
    names(df),
    c(
      "start",
      "end",
      "term_id",
      "token",
      "token_id",
      "lemma",
      "token_original_nomultiwords",
      "upos_original",
      "feats",
      "head_token_id",
      "dep_rel",
      "deps",
      "POSSelected",
      "upos",
      "ngram",
      "sentence_hl",
      "xpos",
      "noHapax",
      "noSingleChar",
      "lemma_original_nomultiwords",
      "token_hl",
      "start_hl",
      "end_hl",
      "misc",
      "upos_specialentities"
    )
  )

  df <- df %>%
    select(!all_of(columns)) %>%
    distinct(., doc_id, paragraph_id, sentence_id, .keep_all = TRUE)

  # Combine sentences into paragraphs
  paragraphs <- df %>%
    group_by(doc_id, paragraph_id) %>%
    arrange(sentence_id) %>%
    summarise(
      paragraph_text = paste(sentence, collapse = " "),
      .groups = "drop"
    )

  # Combine paragraphs into full documents
  documents <- paragraphs %>%
    group_by(doc_id) %>%
    arrange(paragraph_id) %>%
    summarise(
      text = paste(paragraph_text, collapse = "\n\n"),
      .groups = "drop"
    ) %>%
    left_join(
      df %>%
        select(
          !c("paragraph_id", "sentence_id", "sentence", "doc_selected")
        ) %>%
        distinct(., doc_id, .keep_all = TRUE),
      by = c("doc_id")
    ) %>%
    rename("doc_selected" = "docSelected") %>%
    select(!starts_with("upos")) %>%
    mutate(text_original = text)

  return(documents)
}

### SPLIT TEXT INTO SUB-DOCS
splitDoc <- function(df, word) {
  if (nchar(word) <= 3) {
    return(df)
  }

  df <- df %>% filter(doc_selected)

  if ("split_word" %in% names(df)) {
    df <- df %>% select(-split_word)
  }
  if ("doc_id_old" %in% names(df)) {
    df <- df %>% select(-doc_id_old)
  }

  df_splitted <- list()
  n <- nrow(df)

  for (i in seq_len(n)) {
    testo <- df$text[i]
    delimiter <- "\u0001SPLIT_HERE\u0001"
    testo_marked <- gsub(word, paste0(delimiter, word), testo, fixed = TRUE)
    parti <- unlist(strsplit(testo_marked, delimiter, fixed = TRUE))
    parti <- parti[nchar(trimws(parti)) > 0]
    df_splitted[[i]] <- parti
  }

  doc_id_old <- rep(df$doc_id, lengths(df_splitted))
  total_docs <- sum(lengths(df_splitted))
  padding <- nchar(as.character(total_docs))

  df_result <- data.frame(
    doc_id = paste0("doc_", sprintf(paste0("%0", padding, "d"), 1:total_docs)),
    text = unlist(df_splitted),
    doc_id_old = doc_id_old,
    doc_selected = TRUE,
    stringsAsFactors = FALSE
  )

  df_for_join <- df %>% select(-c(text, doc_selected))

  df_result <- df_result %>%
    left_join(df_for_join, by = c("doc_id_old" = "doc_id")) %>%
    mutate(split_word = word)

  return(df_result)
}

unsplitDoc <- function(df) {
  if ("doc_id_old" %in% names(df)) {
    text_rejoined <- df %>%
      group_by(doc_id_old) %>%
      summarise(text = paste(text, collapse = ""), .groups = "drop")

    cols_to_exclude <- c("doc_id", "text", "doc_selected", "split_word")

    other_columns <- df %>%
      group_by(doc_id_old) %>%
      slice(1) %>%
      ungroup() %>%
      select(-all_of(cols_to_exclude))

    df <- text_rejoined %>%
      left_join(other_columns, by = "doc_id_old") %>%
      mutate(doc_id = doc_id_old, doc_selected = TRUE) %>%
      select(-doc_id_old) %>%
      arrange(doc_id)
  }

  return(df)
}

### TEXT SAMPLING ----
samplingText <- function(txt, n) {
  id <- sample(txt$doc_id, n)
  txt$doc_selected <- (txt$doc_id %in% id)
  return(txt)
}


### EXTERNAL INFORMATION ----
loadExtInfo <- function(file, txt) {
  df <- readxl::read_excel(file)

  txt <- txt %>%
    left_join(df, by = "doc_id")

  return(txt)
}

### PRE_PROCESSING ----

restoreText <- function(x) {
  x <- x %>%
    mutate(text = text_original)
}

### 1. TOKENIZATION ----


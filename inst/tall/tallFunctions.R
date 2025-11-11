### UTILS functions ----

## CPU cores ------
coresCPU <- function() {
  ## set cores for parallel computing
  ncores <- max(1, parallel::detectCores() - 1)

  ## set cores for windows machines
  if (Sys.info()[["sysname"]] == "Windows") {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
  }
  return(ncores)
}

## Check Internet connection ----
is_online <- function() {
  # Attempt to connect to a known online resource (e.g., Google's DNS server)
  result <- try(
    suppressWarnings(system("ping -c 1 8.8.8.8", intern = TRUE)),
    silent = TRUE
  )

  # Check if the ping command was successful
  if (inherits(result, "try-error")) {
    return(FALSE)
  } else {
    # Check if the output contains "ttl" (time to live), which indicates a successful ping
    return(any(grepl("ttl=", result, ignore.case = TRUE)))
  }
}

## clean raw text before apply tokenization ----

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
# clean_text <- function(df, text_column = "text",
#                        add_space = TRUE,
#                        remove_quotes = TRUE,
#                        punctuation_marks = c(",", ";", "!", "_", "»", "«" ,"&", "(", ")","--",
#                        "..","...","....","--","---",".#","“","‘","”","’", "??","???")) {

#   # Sort punctuation marks by length (longest first) to prioritize sequences
#   punctuation_marks <- punctuation_marks[order(nchar(punctuation_marks), decreasing = TRUE)]

#   # Escape special regex characters
#   punctuation_marks <- sapply(punctuation_marks, function(x) gsub("([\\^$.|?*+(){}])", "\\\\\\1", x))

#   # Create a regex pattern for punctuation sequences
#   punctuation_regex <- paste0("(", paste0(punctuation_marks, collapse = "|"), ")")

#   df %>%
#     mutate(!!text_column := case_when(
#       add_space & remove_quotes ~ gsub("\\s+", " ", gsub(punctuation_regex, " \\1 ", gsub('\"|\'', '', .data[[text_column]]), perl = TRUE)),
#       add_space ~ gsub("\\s+", " ", gsub(punctuation_regex, " \\1 ", .data[[text_column]], perl = TRUE)),
#       remove_quotes ~ gsub('"|\'', '', .data[[text_column]]),
#       TRUE ~ .data[[text_column]]
#     ))

# }

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
      df <- readtext(file)
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
  df_splitted <- list()
  n <- length(unique(df$doc_id))
  for (i in seq_len(n)) {
    testo <- df$text[i]
    testo <- unlist(strsplit(testo, word))
    df_splitted[[i]] <- testo[nchar(testo) > 0]
  }
  # })
  doc_id_old <- rep(df$doc_id, lengths(df_splitted))

  df <- data.frame(
    doc_id = paste0(
      "doc_",
      sprintf(
        paste0("%0", nchar(lengths(df_splitted)), "d"),
        1:lengths(df_splitted)
      )
    ),
    text = unlist(df_splitted),
    doc_id_old = doc_id_old,
    doc_selected = TRUE
  ) %>%
    left_join(
      df %>%
        select(-c("doc_selected", "text", "text_original")),
      by = c("doc_id_old" = "doc_id")
    ) %>%
    mutate(
      "text_original" = text,
      "split_word" = word
    )
  return(df)
}

unsplitDoc <- function(df) {
  if ("doc_id_old" %in% names(df)) {
    word <- unique(df$split_word)
    df <- df %>%
      group_by(doc_id_old) %>%
      mutate(
        text = paste(text, collapse = word),
        doc_id = doc_id_old
      ) %>%
      ungroup() %>%
      select(-c("doc_id_old", "split_word")) %>%
      distinct(doc_id, .keep_all = TRUE) %>%
      mutate(doc_selected = TRUE)
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

## Tagging Special Entites ----

# TaggingCorpusElements <- function(x) {
#   if ("upos_specialentities" %in% names(x)) {
#     x <- resetSpecialEntities(x)
#   } else {
#     x$upos_specialentities <- x$upos
#   }
#
#   regexList <- c(
#     EMAIL = "(?i)([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))",
#     URL = "(?<!@)\\b(https?://[\\w.-]+\\.[a-z]{2,6}(/[\\S]*)?|[\\w.-]+\\.(com|org|net|edu|gov|it|uk)\\b)",
#     HASH = "^#",
#     EMOJI = "[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F700-\U0001F77F\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\U0001FA00-\U0001FA6F\U0001FA70-\U0001FAFF\U00002702-\U000027B0\U000024C2-\U0001F251]",
#     IP_ADDRESS = "\\b\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\b",
#     MENTION = "^@"
#   )
#   items <- names(regexList)
#
#   resList <- list()
#   j <- 0
#
#   for (i in 1:length(items)) {
#     item <- items[i]
#     results <- stringi::stri_detect_regex(x$token, regexList[[item]])
#     if (sum(results) > 0) {
#       j <- j + 1
#       resList[[j]] <- data.frame(doc_id = x$doc_id[results], item = x$token[results], tag = item)
#       x$upos[results] <- toupper(item)
#       x$POSSelected[results] <- ifelse(x$upos[results] %in% c("HASH", "MENTION", "EMOJI"), TRUE, FALSE)
#     }
#   }
#
#   if (length(resList) > 0) {
#     resList <- dplyr::bind_rows(resList) %>%
#       dplyr::filter(!is.na(item))
#   } else {
#     resList <- tibble::tibble(doc_id = 0, item = NA, tag = "email") %>% dplyr::filter(!is.na(item))
#   }
#
#   # normalize hash and email
#   x <- x %>%
#     mutate(
#       lemma = case_when(
#         upos %in% c("HASH", "EMAIL") ~ tolower(lemma),
#         upos == "EMOJI" ~ trimws(lemma),
#         TRUE ~ lemma
#       ),
#       token = case_when(
#         upos %in% c("HASH", "EMAIL") ~ tolower(token),
#         upos == "EMOJI" ~ trimws(token),
#         TRUE ~ token
#       )
#     )
#
#   if (nrow(resList) > 0) {
#     resList <- resList %>%
#       mutate(item = case_when(
#         tag %in% c("HASH", "EMAIL") ~ tolower(item),
#         tag == "EMOJI" ~ trimws(item),
#         TRUE ~ item
#       ))
#   }
#
#   return(list(resList = resList, x = x))
# }
#
# resetSpecialEntities <- function(x) {
#   if ("upos_specialentities" %in% names(x)) {
#     items <- toupper(c("email", "url", "hash", "emoji", "ip_address", "mention"))
#     x <- x %>%
#       mutate(upos = ifelse(upos %in% items, upos_specialentities, upos))
#   } else {
#     x$upos_specialentities <- x$upos
#   }
#   return(x)
# }
#
# summarySpecialEntities <- function(resList, type = "all") {
#   data.frame(UPOS = toupper(c("email", "url", "hash", "emoji", "ip_address", "mention")), "N. of Items" = rep(0, 6), "N. of Docs" = rep(0, 6))
#
#   switch(type,
#          "all" = {
#            resList %>%
#              group_by(tag) %>%
#              summarise(items = length(unique(item))) %>%
#              rename(
#                UPOS = tag,
#                "Frequency" = items
#              ) %>%
#              ungroup() %>%
#              bind_rows(tibble(
#                UPOS = toupper(c("email", "url", "hash", "emoji", "ip_address", "mention")),
#                "Frequency" = rep(0, 6)
#              )) %>%
#              group_by(UPOS) %>%
#              summarize_all(sum)
#          },
#          {
#            label <- toupper(type)
#            resList %>%
#              rename(UPOS = tag) %>%
#              filter(UPOS == label) %>%
#              count(item) %>%
#              arrange(desc(n))
#          }
#   )
# }

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
          rename(upos = upos.x) %>%
          highlight()
      },
      "token" = {
        df <- df %>%
          customListsReset() %>%
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
          rename(upos = upos.x) %>%
          highlight()
      }
    )
  } else {
    print("RESET")
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
          rename(upos = upos.x) %>%
          highlight()
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
          rename(upos = upos.x) %>%
          highlight()
      }
    )
  } else {
    print("RESET")
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
  # if ("ngram" %in% names(x)){
  #   x <- x %>%
  #     select(-"ngram")
  # }
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

  # filter original token df removing POS excluded in rake
  x2 <- x %>% filter(upos %in% relevant)

  # combine lemmas or tokens into multi-words

  switch(
    term,
    lemma = {
      x2$multiword <- txt_recode_ngram(
        x2$lemma,
        compound = stats$keyword,
        ngram = stats$ngram,
        sep = " "
      )

      # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
      x2 <- x2 %>%
        mutate(
          upos_multiword = ifelse(lemma == multiword, upos, "MULTIWORD"),
          upos_multiword = ifelse(
            is.na(multiword),
            "NGRAM_MERGED",
            upos_multiword
          )
        ) %>%
        left_join(
          stats %>% select(keyword, ngram),
          by = c("multiword" = "keyword")
        ) %>%
        select(doc_id, term_id, multiword, upos_multiword, ngram)
    },
    token = {
      x2$multiword <- txt_recode_ngram(
        x2$token,
        compound = stats$keyword,
        ngram = stats$ngram,
        sep = " "
      )

      # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
      x2 <- x2 %>%
        mutate(
          upos_multiword = ifelse(token == multiword, upos, "MULTIWORD"),
          upos_multiword = ifelse(
            is.na(multiword),
            "NGRAM_MERGED",
            upos_multiword
          )
        ) %>%
        left_join(
          stats %>% select(keyword, ngram),
          by = c("multiword" = "keyword")
        ) %>%
        select(doc_id, term_id, multiword, upos_multiword, ngram)
    }
  )

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
      stats %>%
        select(-starts_with("freq")),
      by = "keyword"
    ) %>%
    filter(freq >= freq.min) %>%
    arrange(desc(freq))

  return(list(stats = stats, dfMW = x2))
}

applyRake <- function(x, rakeResults, row_sel = NULL, term = "lemma") {
  if ("ngram" %in% names(x)) {
    x <- x %>%
      select(-"ngram")
  }

  rakeResults$stats <- rakeResults$stats[row_sel, ]
  # filter original token df removing POS excluded in rake

  # combine lemmas or tokens into multi-words

  switch(
    term,
    lemma = {
      # rebuild the original tokenized df
      x <- x %>%
        left_join(rakeResults$dfMW, by = c("doc_id", "term_id")) %>%
        mutate(
          multiword = ifelse(is.na(multiword), lemma, multiword),
          upos_multiword = ifelse(is.na(upos_multiword), upos, upos_multiword),
          POSSelected = ifelse(
            upos_multiword == "MULTIWORD",
            TRUE,
            POSSelected
          ),
          POSSelected = ifelse(
            upos_multiword == "NGRAM_MERGED",
            FALSE,
            POSSelected
          )
        )

      if (!"upos_original" %in% names(x)) {
        names(x)[names(x) == "upos"] <- "upos_original"
      }
      x <- x %>%
        select(-ends_with("upos")) %>%
        rename(upos = upos_multiword)

      if (!"lemma_original_nomultiwords" %in% names(x)) {
        names(x)[names(x) == "lemma"] <- "lemma_original_nomultiwords"
      }

      x <- x %>%
        select(-ends_with("lemma")) %>%
        rename(lemma = multiword)
    },
    token = {
      # rebuild the original tokenized df
      x <- x %>%
        left_join(rakeResults$dfMW, by = c("doc_id", "term_id")) %>%
        mutate(
          multiword = ifelse(is.na(multiword), token, multiword),
          upos_multiword = ifelse(is.na(upos_multiword), upos, upos_multiword),
          POSSelected = ifelse(
            upos_multiword == "MULTIWORD",
            TRUE,
            POSSelected
          ),
          POSSelected = ifelse(
            upos_multiword == "NGRAM_MERGED",
            FALSE,
            POSSelected
          )
        )

      if (!"upos_original" %in% names(x)) {
        names(x)[names(x) == "upos"] <- "upos_original"
      }
      x <- x %>%
        select(-ends_with("upos")) %>%
        rename(upos = upos_multiword)

      if (!"token_original_nomultiwords" %in% names(x)) {
        names(x)[names(x) == "token"] <- "token_original_nomultiwords"
      }

      x <- x %>%
        select(-ends_with("token")) %>%
        rename(token = multiword)
    }
  )

  ## calculate new start end values for multiwords
  ind <- which(!is.na(x$ngram))
  ind2 <- ind + (x$ngram[ind] - 1)
  x$end[ind] <- x$end[ind2]

  # calculate ngram (WHY?)
  x <- x %>%
    mutate(id = row_number()) %>%
    group_by(id) %>%
    mutate(
      ngram = ifelse(
        upos == "MULTIWORD",
        max(c(lengths(strsplit(lemma, " "))), lengths(strsplit(token, " "))),
        NA
      )
    ) %>%
    ungroup() %>%
    select(-id)

  obj <- x
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
      "upos_specialentities"
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
vocabulary <- function(dfTag, term) {
  dictFreq <- LemmaSelection(dfTag) %>%
    dplyr::filter(docSelected) %>%
    mutate(token = ifelse(upos == "MULTIWORD", lemma, token))

  if (term == "lemma") {
    dictFreq <- dictFreq %>%
      group_by(upos, lemma) %>%
      summarize(n = n()) %>%
      arrange(desc(n)) %>%
      rename(
        Lemma = lemma,
        Frequency = n,
        "Part of Speech" = upos
      ) %>%
      relocate("Part of Speech", .after = last_col())
  } else {
    dictFreq <- dictFreq %>%
      group_by(upos, token) %>%
      summarize(n = n()) %>%
      arrange(desc(n)) %>%
      rename(
        Token = token,
        Frequency = n,
        "Part of Speech" = upos
      ) %>%
      relocate("Part of Speech", .after = last_col())
  }
  return(dictFreq)
}

# tfidf calculation
tfidfTable <- function(dfTag, term) {
  tfidfDATA <- LemmaSelection(dfTag) %>%
    dplyr::filter(docSelected) %>%
    tfidf(term = term)

  if (term == "lemma") {
    tfidfDATA <- tfidfDATA %>%
      rename(
        "Lemma" = term,
        "TF-IDF" = TFIDF
      )
  } else {
    tfidfDATA <- tfidfDATA %>%
      rename(
        "Token" = term,
        "TF-IDF" = TFIDF
      )
  }
  return(tfidfDATA)
}

# Term Frequency Distributions
freqByPos <- function(df, term = "lemma", pos = "NOUN") {
  obj <- df %>%
    dplyr::filter(upos %in% pos) %>%
    count(term = .[[term]]) %>%
    arrange(desc(n))

  if ("URL" %in% pos) {
    obj$term <- paste0(
      '<a href=\"',
      obj$term,
      '\" target=\"_blank\">',
      obj$term,
      "</a>"
    )
  }
  return(obj)
}

# freqPlotly ----
freqPlotly <- function(
  dfPlot,
  x,
  y,
  n = 10,
  xlabel,
  ylabel,
  scale = c("identity", "log"),
  topicmodel = FALSE,
  color = "#4F7942",
  decimal = 0
) {
  # function to build and plot plotly horizontal barplot
  dfPlot <- dfPlot %>% dplyr::slice_head(n = n)
  xmax <- max(dfPlot[[x]])

  switch(scale, log = {
    dfPlot$n <- log(dfPlot$n)
  })

  if (isTRUE(topicmodel)) {
    fig1 <- plot_ly(
      data = dfPlot,
      x = round(dfPlot[[x]], decimal),
      y = dfPlot[[y]],
      source = "A",
      type = "bar",
      orientation = "h",
      marker = list(
        color = paste0(color, "60"),
        line = list(color = color, width = 1)
      ),
      hovertemplate = "<b>Word</b>: <i>%{y}</i> <br><b><i>Value: %{x}</i></b><extra></extra>"
    )
  } else {
    fig1 <- plot_ly(
      data = dfPlot,
      x = dfPlot[[x]],
      y = ~ reorder(dfPlot[[y]], dfPlot[[x]]),
      source = "A",
      type = "bar",
      orientation = "h",
      marker = list(
        color = paste0(color, "60"),
        line = list(color = color, width = 1)
      ),
      hovertemplate = "<b><i>Word: %{y}</i></b> <br> <b><i>Value: %{x}</i></b><extra></extra>"
    )
  }

  fig1 <- fig1 %>%
    layout(
      yaxis = list(
        title = ylabel,
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        domain = c(0, 1)
      ),
      xaxis = list(
        title = xlabel,
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE
      ),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )

  if (decimal > 0) {
    ann_text <- format(round(dfPlot[[x]], decimal), nsmall = decimal)
  } else {
    ann_text <- dfPlot[[x]]
  }

  fig1 <- fig1 %>%
    add_annotations(
      xref = "x1",
      yref = "y",
      x = dfPlot[[x]] + xmax * 0.015,
      y = dfPlot[[y]],
      text = ann_text,
      font = list(family = "Arial", size = 12, color = color),
      showarrow = FALSE
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    event_register("plotly_selecting")

  fig1
}

# ValueBoxes Indices ----
valueBoxesIndices <- function(x) {
  x <- x %>%
    filter(!upos %in% c("PUNCT", "SYM", "NUM", "X"))

  # 1. # of documents
  nDoc <- length(unique(x$doc_id))

  # 2. # of tokens
  nTokens <- nrow(x)

  # 3. Dictionary: # di termini differenti senza ripetizione
  nDictionary <- length(unique(x$token))

  # 4. # of lemmas
  nLemmas <- length(unique(x$lemma))

  # 5. # of sentences
  nSentences <- x %>%
    group_by(doc_id) %>%
    reframe(nSent = max(sentence_id)) %>%
    ungroup() %>%
    select(nSent) %>%
    reframe(nSent = sum(nSent)) %>%
    as.numeric()

  # 6. # avg document length
  avgDocLength <- x %>%
    group_by(doc_id) %>%
    select(doc_id, sentence) %>%
    reframe(
      nTokens = n(),
      nChars = nchar(paste(sentence, collapse = " "))
    ) %>%
    ungroup() %>%
    reframe(
      avgChars = round(mean(nChars), 0),
      sdChars = round(sd(nChars), 0),
      avgTokens = round(mean(nTokens), 0),
      sdTokens = round(sd(nTokens), 0)
    )

  # 7. # avg length sentence
  avgSentLength <- x %>%
    group_by(doc_id, sentence_id) %>%
    reframe(
      sentLength = n(),
      nChars = nchar(sentence)
    ) %>%
    ungroup() %>%
    reframe(
      avgTokens = round(mean(sentLength), 1),
      sdTokens = round(sd(sentLength), 1),
      avgChars = round(mean(nChars), 1),
      sdChars = round(sd(nChars), 1)
    )

  # 8. TTR: il rapporto tra la varietà del dizionario (Dictionary) e il numero totale di token in una raccolta testuale (# terms); in altre parole, misura la diversità lessicale in un corpus
  TTR <- round(nDictionary / nTokens * 100, 2)

  # 9.  %hapax
  hapax <- x %>%
    group_by(token) %>%
    count() %>%
    filter(n == 1) %>%
    ungroup() %>%
    reframe(n = sum(n)) %>%
    as.numeric() /
    nDictionary *
    100

  # 10. Guiraud
  guiraud <- round(nDictionary / sqrt(nTokens), 1)

  # 11. Lexical density
  lexical_words <- x %>%
    filter(upos %in% c("NOUN", "VERB", "ADJ", "ADV")) # Parole di contenuto

  lexical_density <- (nrow(lexical_words) / nrow(x)) * 100

  # 12. Nominal Ratio (Rapporto Nominale)
  num_nouns <- sum(x$upos == "NOUN", na.rm = TRUE) # Numero di sostantivi
  num_verbs <- sum(x$upos == "VERB", na.rm = TRUE) # Numero di verbi

  nominal_ratio <- ifelse(num_verbs > 0, num_nouns / num_verbs, NA) # Evita divisione per zero

  # 13. Gini Index sui Token (Disomogeneità della Distribuzione)
  token_counts <- x %>%
    count(token, sort = TRUE) %>%
    pull(n) # Ottieni solo il conteggio delle occorrenze

  gini_index <- Gini(token_counts) # Calcola l'indice di Gini sulla distribuzione delle parole

  # 14 Yule’s K (Misura della diversità lessicale)
  words <- x$token[x$token != ""] # Remove empty strings
  word_freq <- table(words)
  M1 <- length(word_freq)
  M2 <- sum(word_freq^2)
  K <- 10000 * (M2 - M1) / (length(words)^2)

  obj <- list(
    nDoc = nDoc,
    nTokens = nTokens,
    nDictionary = nDictionary,
    nLemmas = nLemmas,
    nSentences = nSentences,
    avgDocLengthChars = avgDocLength$avgChars,
    avgDocLengthCharsSD = avgDocLength$sdChars,
    avgDocLengthTokens = avgDocLength$avgTokens,
    avgDocLengthTokensSD = avgDocLength$sdTokens,
    avgSentLengthTokens = avgSentLength$avgTokens,
    avgSentLengthTokensSD = avgSentLength$sdTokens,
    avgSentLengthChars = avgSentLength$avgChars,
    avgSentLengthCharsSD = avgSentLength$sdChars,
    TTR = TTR,
    hapax = round(hapax, 1),
    guiraud = guiraud,
    lexical_density = round(lexical_density, 1),
    nominal_ratio = round(nominal_ratio, 2),
    gini_index = round(gini_index, 2),
    yule_k = round(K, 1)
  )
}

Gini <- function(x, corr = FALSE, na.rm = TRUE) {
  if (!na.rm && any(is.na(x))) {
    return(NA_real_)
  }
  x <- as.numeric(na.omit(x))
  n <- length(x)
  x <- sort(x)
  G <- sum(x * 1L:n)
  G <- 2 * G / sum(x) - (n + 1L)
  if (corr) G / (n - 1L) else G / n
}

## wordcloud2vis

wordcloud2vis <- function(nodes, labelsize = 7, opacity = 1) {
  nodes <- nodes %>%
    mutate(id = row_number())
  # size scaling
  scalemin <- 20 * (1 + labelsize / 5)
  scalemax <- 100 * (1 + labelsize / 5)
  N <- nrow(nodes)

  colorlists <- colorlist()
  colorlists <- sample(colorlists, N, replace = TRUE)

  opacity.min <- 0.6
  shape <- "text"
  layout <- "layout_nicely"

  nodes <- nodes %>%
    mutate(
      font.color = colorlists,
      id = row_number(),
      shape = shape,
      color = colorlists,
      title = paste(
        "<strong>",
        label,
        "</strong>",
        "<br><h5>freq = ",
        value,
        "</h5>",
        sep = ""
      )
    )

  nodes$font.size <- log(nodes$value)
  Min <- min(nodes$font.size)
  Max <- max(nodes$font.size)
  if (Max > Min) {
    size <- (nodes$font.size - Min) / (Max - Min) * 15 * labelsize + 10
  } else {
    size <- 10 * labelsize
  }
  size[size < scalemin] <- scalemin
  size[size > scalemax] <- scalemax
  nodes$font.size <- size

  if (shape %in% c("dot", "square")) {
    nodes$font.vadjust <- -0.7 * nodes$font.size
  } else {
    nodes$font.vadjust <- 0
  }

  ## opacity for label
  opacity_font <- sqrt(
    (nodes$font.size - min(nodes$font.size)) / diff(range(nodes$font.size))
  ) *
    opacity +
    opacity.min +
    0.1
  if (is.nan(opacity_font[1])) {
    opacity_font <- rep(opacity.min, length(opacity_font))
  }

  # node colors
  nodes$opacity.nodes <- (opacity_font - min(opacity_font)) /
    (diff(range(opacity_font))) *
    0.5 +
    opacity.min
  nodes$opacity.nodes[is.nan(nodes$opacity.nodes)] <- 0.5

  VIS <-
    visNetwork::visNetwork(
      nodes = nodes,
      edges = NULL,
      type = "full",
      smooth = TRUE,
      physics = TRUE
    ) %>%
    visNetwork::visNodes(
      shadow = FALSE,
      shape = nodes$shape,
      font = list(
        color = nodes$font.color,
        size = nodes$font.size,
        vadjust = nodes$font.vadjust
      )
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = T, hover = T, degree = 1),
      nodesIdSelection = T
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.2
    ) %>%
    visEvents(
      click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}"
    ) %>%
    visNetwork::visOptions(
      manipulation = FALSE,
      height = "100%",
      width = "100%"
    )
  return(VIS)
}

## wordcloud function
wordcloud2a <- function(
  data,
  size = 1,
  minSize = 0,
  gridSize = 0,
  fontFamily = "Segoe UI",
  fontWeight = "bold",
  color = "random-dark",
  backgroundColor = "transparent",
  minRotation = -pi / 4,
  maxRotation = pi / 4,
  shuffle = TRUE,
  rotateRatio = 0.4,
  shape = "circle",
  ellipticity = 0.65,
  widgetsize = NULL,
  figPath = NULL,
  hoverFunction = NULL
) {
  if ("table" %in% class(data)) {
    dataOut <- data.frame(name = names(data), freq = as.vector(data))
  } else {
    data <- as.data.frame(data)
    dataOut <- data[, 1:2]
    names(dataOut) <- c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath <- strsplit(figPath, "\\.")[[1]]
    len <- length(spPath)
    figClass <- spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 <- base64enc::base64encode(figPath)
    base64 <- paste0(
      "data:image/",
      figClass,
      ";base64,",
      base64
    )
  } else {
    base64 <- NULL
  }
  weightFactor <- size * 180 / max(dataOut$freq)
  settings <- list(
    word = dataOut$name,
    freq = dataOut$freq,
    fontFamily = fontFamily,
    fontWeight = fontWeight,
    color = color,
    minSize = minSize,
    weightFactor = weightFactor,
    backgroundColor = backgroundColor,
    gridSize = gridSize,
    minRotation = minRotation,
    maxRotation = maxRotation,
    shuffle = shuffle,
    rotateRatio = rotateRatio,
    shape = shape,
    ellipticity = ellipticity,
    figBase64 = base64,
    hover = htmlwidgets::JS(hoverFunction)
  )
  chart <- htmlwidgets::createWidget(
    "wordcloud2",
    settings,
    width = widgetsize[1],
    height = widgetsize[2],
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.padding = 0,
      browser.padding = 0,
      browser.fill = TRUE
    )
  )
  chart
}

## TFIDF functions ----
tfidf <- function(dfTag, term = "lemma", document = "doc_id") {
  # calculate tfidf
  dtm <- dfTag %>% dplyr::filter(POSSelected)
  dtm <- document_term_frequencies(dtm, document = document, term = term)
  dtm <- document_term_matrix(dtm)
  tfidf <- dtm_tfidf(dtm)
  tibble(term = names(tfidf), TFIDF = as.numeric(tfidf)) %>%
    arrange(desc(tfidf))
}

### WORD IN CONTEXT ----
get_context_window <- function(
  df,
  target_word,
  n_left = 5,
  n_right = 5,
  term = "token"
) {
  # Ensure term is correctly set
  if (!term %in% c("token", "lemma")) {
    stop("The 'term' argument must be either 'token' or 'lemma'.")
  }

  # Exclude irrelevant POS tags
  no_upos <- c(
    "NGRAM_MERGED",
    "X",
    "PUNCT",
    "SYM",
    "URL",
    "IP_ADDRESS",
    "EMAIL",
    "DET",
    "CCONJ"
  )

  df <- df %>%
    filter(!upos %in% no_upos) %>%
    group_by(doc_id) %>%
    mutate(term_id = row_number()) %>%
    ungroup() %>%
    select(doc_id, term_id, token, lemma, upos) %>%
    mutate(
      token = tolower(token),
      lemma = tolower(lemma) # Ensure consistency in case sensitivity
    )

  # Select target column based on 'term' argument
  target_column <- if (term == "token") "token" else "lemma"

  target_rows <- df %>% filter(!!sym(target_column) == target_word)

  # Initialize list to store context windows
  context_list <- vector("list", length = nrow(target_rows))

  for (i in seq_len(nrow(target_rows))) {
    row <- target_rows[i, ] # Specific occurrence of the target word
    doc_subset <- df %>% filter(doc_id == row$doc_id)

    middle <- row$term_id
    start <- max(1, row$term_id - n_left) # Ensure it doesn't go below 1
    end <- min(max(doc_subset$term_id), row$term_id + n_right) # Ensure it doesn't exceed document length

    # Extract words in left and right context based on 'term'
    context_left <- doc_subset %>%
      filter(term_id >= start & term_id < middle) %>%
      pull(!!sym(term))

    context_right <- doc_subset %>%
      filter(term_id > middle & term_id <= end) %>%
      pull(!!sym(term))

    # Extract POS tags for the context
    context_upos <- doc_subset %>%
      filter(term_id >= start & term_id <= end) %>%
      pull(upos)

    # Store results in a tibble while keeping lists separate
    context_list[[i]] <- tibble(
      doc_id = row$doc_id,
      context_before = list(context_left),
      target_word = row[[target_column]], # Use the correct target reference
      context_after = list(context_right),
      upos = list(context_upos)
    )
  }

  # Combine all tibbles into a single dataframe
  context_df <- bind_rows(context_list)

  return(context_df)
}

## Context network
contextNetwork <- function(df, dfTag, target_word, n = 50) {
  # Espandi le liste nelle colonne context_before, token, context_after, e upos
  longer_df <- df %>%
    mutate(segment_id = row_number()) %>% # Identificatore univoco del segmento
    rowwise() %>%
    mutate(
      words = list(c(
        unlist(context_before),
        target_word,
        unlist(context_after)
      )), # Unisce tutto in un'unica lista
      upos_list = list(unlist(upos)) # Appiattisce la lista dei POS
    ) %>%
    ungroup() %>%
    select(segment_id, words, upos_list) %>%
    unnest(cols = c(words, upos_list)) %>% # Trasforma le liste in formato long
    rename(token = words, upos = upos_list) # Rinomina le colonne

  uposSelected <- unique(LemmaSelection(dfTag) %>% select(upos) %>% pull())

  net <- network(
    longer_df %>% filter(upos %in% uposSelected),
    term = "token",
    group = c("segment_id"),
    n = 50,
    minEdges = 100,
    labelsize = 3,
    opacity = 0.6,
    interLinks = TRUE,
    normalization = "association",
    remove.isolated = FALSE,
    community.repulsion = 0
  )

  net$edges <- net$edges %>%
    filter(
      !(color == "#69696920" &
        !(term_from == target_word) &
        !(term_to == target_word))
    )

  vis <- net2vis(net$nodes, net$edges, click = FALSE)
  return(vis)
}

### CLUSTERING ----
clustering <- function(
  dfTag,
  n = 50,
  group = "doc_id",
  term = "lemma",
  minEdges = 25,
  normalization = "association"
) {
  x <- dfTag # %>% dplyr::filter(POSSelected)

  cooc <- coocMatrix(x, term = term, group = group, n = n, pos = TRUE)

  edges <- cooc %>%
    data.frame() %>%
    rename(s = cooc) %>%
    mutate(
      sA = s / (s_from * s_to),
      sC = s / (sqrt(s_from * s_to)),
      sJ = s / (s_from + s_to - s)
    )

  switch(
    normalization,
    none = {
      edges$value <- edges$s
    },
    association = {
      edges$value <- edges$sA
    },
    cosine = {
      edges$value <- edges$sC
    },
    jaccard = {
      edges$value <- edges$sJ
    }
  )

  tailEdges <- quantile(edges$value, 1 - (minEdges / 100), na.rm = T)

  edges <- edges %>%
    dplyr::filter(value >= tailEdges) %>%
    select(term1, term2, value, s, sA, sC, sJ)

  wordnetwork <- graph_from_data_frame(edges %>% select(term1, term2, value))

  # Community detection via optimization of modularity score
  wordnetwork <- as.undirected(wordnetwork) # an undirected graph
  comm <- igraph::cluster_walktrap(wordnetwork, weights = E(wordnetwork)$value)
  cluster <- data.frame(
    word = c(cooc$term1, cooc$term2),
    frequency = c(cooc$s_from, cooc$s_to)
  ) %>%
    distinct() %>%
    left_join(
      data.frame(word = comm$names, group = comm$membership),
      by = c("word")
    ) %>%
    drop_na() %>%
    group_by(word) %>%
    summarize(
      group = first(group),
      frequency = max(frequency)
    ) %>%
    arrange(group, desc(frequency))
  obj <- list(cluster = cluster, comm = comm)
}

dend2vis <- function(hc, labelsize, nclusters = 1, community = TRUE) {
  # community = TRUE means that hc is an igraph community detection object
  # community = FALSE mean that hc is a hclust object

  # transform and plot a community igraph object using dendrogram
  if (community) {
    hc <- as.hclust(hc, use.modularity = TRUE)
  }

  h_tail <- round((max(hc$height) * 0.12), 1)

  hc$height <- hc$height + h_tail

  if (!"group" %in% names(hc)) {
    hc$group <- nclusters + 1
  }

  if (nclusters < max(hc$group, na.rm = T)) {
    VIS <- visHclust(
      hc,
      cutree = nclusters,
      colorEdges = "grey60",
      horizontal = TRUE,
      export = FALSE
    )
  } else {
    VIS <- visHclust(
      hc,
      colorEdges = "grey60",
      horizontal = TRUE,
      export = FALSE
    )
  }

  VIS$x$edges <- data.frame(color = unique(VIS$x$edges$color)) %>%
    mutate(new_color = colorlist()[1:nrow(.)]) %>%
    right_join(VIS$x$edges, by = "color") %>%
    select(-color) %>%
    rename(color = new_color)
  VIS$x$nodes <- VIS$x$nodes %>%
    mutate(
      label = ifelse(group != "individual", NA, label),
      group = ifelse(group == "individual", "word", group),
      title = gsub("individuals", "words", title),
      value = 1,
      scaling.min = 10,
      scaling.max = 10
    )
  coords <- VIS$x$nodes %>%
    select(x, y) %>%
    as.matrix()

  edges <- VIS$x$edges
  nodes <- VIS$x$nodes %>%
    select(id, label) %>%
    dplyr::filter(label != "1")

  VIS$x$edges <- edges %>%
    select(-id) %>%
    left_join(nodes, by = c("to" = "id")) %>%
    select(-label.x) %>%
    rename(label = label.y) %>%
    mutate(
      value = 10,
      font.color = color,
      font.size = labelsize * 10,
      font.vadjust = -0.1 * font.size,
      label = ifelse(is.na(label), "", label)
    )

  VIS <- VIS %>%
    visGroups(
      groupname = "group",
      color = "gray90",
      shape = "dot",
      size = 10
    ) %>%
    visGroups(
      groupname = "word",
      font = list(size = 0),
      color = list(
        background = "white",
        border = "#80B1D3",
        highlight = "#e2e9e9",
        hover = "orange"
      ),
      shape = "box"
    ) %>%
    visNodes(font = list(align = VIS$x$nodes$font.align)) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = T,
        hover = T,
        degree = list(to = 1000, from = 0),
        algorithm = "hierarchical"
      ),
      nodesIdSelection = FALSE,
      manipulation = FALSE,
      height = "100%",
      width = "100%"
    ) %>%
    visNetwork::visInteraction(
      dragNodes = FALSE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.4
    ) %>%
    visIgraphLayout(
      layout = "layout.norm",
      layoutMatrix = coords,
      type = "full"
    ) %>%
    visEdges(font = list(align = "top", size = VIS$x$edges$font.size))

  for (i in 1:nrow(VIS$x$nodes)) {
    if (VIS$x$nodes$group[i] == "group") {
      old_inertia <- as.character(VIS$x$nodes$inertia[i])
      inertia <- as.character(VIS$x$nodes$inertia[i] - h_tail)
      VIS$x$nodes$title[i] <- gsub(old_inertia, inertia, VIS$x$nodes$title[i])
    }
  }

  if ("dtm" %in% names(hc)) {
    k <- max(hc$group, na.rm = TRUE)
    if (nclusters < k) {
      new_groups <- cutree(hc, nclusters)
      for (i in names(new_groups)) {
        VIS$x$nodes$title[VIS$x$nodes$title == i] <- new_groups[i]
      }
    }
    VIS <- VIS %>%
      visEvents(
        click = "function(nodes){
                  Shiny.onInputChange('click_rein', nodes.nodes[0]);
                  ;}"
      )
  } else {
    VIS <- VIS %>%
      visEvents(
        click = "function(nodes){
                  Shiny.onInputChange('click_dend', nodes.nodes[0]);
                  ;}"
      )
  }

  return(VIS)
}

### CORRESPONDENCE ANALYSIS -----

## Correspondence Analysis on Words ----
wordCA <- function(x, n = 50, term = "lemma", group = c("Documents")) {
  switch(
    group,
    Documents = {
      group <- "doc_id"
    },
    Paragraphs = {
      group <- c("doc_id", "paragraph_id")
    },
    Sentences = {
      group <- c("doc_id", "sentence_id")
    }
  )

  # x <- dfTag %>% dplyr::filter(POSSelected)

  if (length(group) > 1) {
    new_doc_id <- unique_identifier(x, fields = group)
  } else {
    new_doc_id <- x$doc_id
  }
  dtm <- document_term_frequencies(
    x %>% mutate(doc_id = new_doc_id),
    term = term
  )

  # dtm <- document_term_frequencies(x, term=term)
  mat <- document_term_matrix(dtm, weight = "freq")
  mat <- as.matrix(dtm_remove_lowfreq(mat, minfreq = 1, maxterms = n))

  res <- ca::ca(mat)

  # Contribute
  Ncol <- min(10, ncol(res$rowcoord))
  contrib <- data.frame((res$colcoord[, 1:Ncol]^2) * res$colmass)
  colnames(contrib) <- paste0("Contrib", 1:ncol(contrib))

  # Cosines squared
  cosine <- data.frame(((res$colcoord[, 1:Ncol]^2) / (res$coldist)))
  colnames(cosine) <- paste0("Cosine", 1:ncol(contrib))

  # Word Coordinates
  wordCoord <- res$colcoord[, 1:Ncol] %>%
    data.frame() %>%
    mutate(
      label = res$colnames,
      inertia = res$colinertia,
      dist = res$coldist,
      mass = res$colmass
    )

  docContrib <- data.frame((res$rowcoord[, 1:Ncol]^2) * res$rowmass)
  docCoord <- res$rowcoord[, 1:Ncol] %>%
    data.frame() %>%
    mutate(
      label = res$rownames,
      inertia = res$rowinertia,
      dist = res$rowdist,
      mass = res$rowmass
    )

  ## Benzecrì correction
  res$eigCorrected <- ((n / (n - 1))^2 * (res$sv - 1 / n)^2)
  # res$eigCorrected[res$eigCorrected<=1/length(res$eigCorrected)] <- 0
  res$eigCorrectedNorm <- res$eigCorrected / sum(res$eigCorrected) * 100

  ## result object
  results <- list(
    ca = res,
    wordCoord = wordCoord,
    contrib = contrib,
    cosine = cosine,
    docContrib = docContrib,
    docCoord = docCoord
  )

  return(results)
}


## caClustering ----
caClustering <- function(
  results,
  method = "ward.D2",
  nDim = 2,
  nclusters = 1,
  lim.contr
) {
  vars <- "Dim"

  # filter by contribution
  contr <- results$contrib %>%
    select(1:nDim) %>%
    filter_all(all_vars(. < lim.contr)) %>%
    rownames_to_column() %>%
    select("rowname")
  #

  D <- dist(
    results$wordCoord %>%
      select(starts_with(vars)) %>%
      select(all_of(1:nDim)) %>%
      rownames_to_column() %>%
      right_join(contr, by = "rowname") %>%
      column_to_rownames(var = "rowname")
  )
  h <- hclust(D, method = method)

  if (nclusters > 1) {
    groups <- cutree(h, k = nclusters)
  } else {
    groups <- rep(1, length(h$labels))
    names(groups) <- h$labels
  }

  # h$group <- groups
  results$clustering <- list(h = h, groups = groups)

  return(results)
}


## CA Plot ----
ca2plotly <- function(
  results,
  dimX = 1,
  dimY = 2,
  topWordPlot = Inf,
  topDocPlot = 20,
  threshold = 0.03,
  labelsize = 16,
  size = 5,
  lim.contr
) {
  # filter by contribution
  contr <- results$contrib %>%
    select(c(dimX, dimY)) %>%
    filter_all(all_vars(. < lim.contr)) %>%
    rownames_to_column() %>%
    select("rowname")
  #

  results$contrib <- results$contrib %>%
    rownames_to_column() %>%
    right_join(contr, by = "rowname") %>%
    column_to_rownames()

  results$wordCoord <- results$wordCoord %>%
    rownames_to_column() %>%
    right_join(contr, by = "rowname") %>%
    column_to_rownames()

  xlabel <- paste0("Dim", dimX)
  ylabel <- paste0("Dim", dimY)
  dimContrLabel <- paste0("Contrib", c(dimX, dimY))
  ymax <- diff(range((results$wordCoord[[ylabel]])))
  xmax <- diff(range((results$wordCoord[[xlabel]])))
  threshold2 <- threshold * mean(xmax, ymax)

  # scaled size for dots
  dotScale <- (results$contrib[, c(dimX, dimY)] * 200)
  dotScale <- ((dotScale[, 1] + dotScale[, 2]) / 2) + size

  # Threshold labels to plot
  thres <- sort(dotScale, decreasing = TRUE)[min(
    topWordPlot,
    nrow(results$wordCoord)
  )]

  Ncol <- sum(substr(names(results$wordCoord), 1, 3) == "Dim")
  # coordinates to plot
  noCol <- setdiff(1:Ncol, c(dimX, dimY))

  results$wordCoord <- results$wordCoord %>%
    select(-any_of(noCol))

  names(results$wordCoord)[1:2] <- c("Dim1", "Dim2")

  results$wordCoord <- results$wordCoord %>%
    mutate(
      dotSize = dotScale,
      groups = results$clustering$groups,
      labelToPlot = ifelse(dotSize >= thres, label, ""),
      font.color = ifelse(
        labelToPlot == "",
        NA,
        adjustcolor(colorlist()[groups], alpha.f = 0.85)
      ),
      font.size = round(dotSize * 2, 0)
    )

  ## Avoid label overlapping
  labelToRemove <- avoidOverlaps(results$wordCoord, threshold = threshold2)
  results$wordCoord <- results$wordCoord %>%
    mutate(
      labelToPlot = ifelse(labelToPlot %in% labelToRemove, "", labelToPlot)
    )

  hull_data <-
    results$wordCoord %>%
    group_by(.data$groups) %>%
    slice(chull(Dim1, Dim2)) %>%
    rename(color = font.color)

  hull_data <- hull_data %>%
    bind_rows(
      hull_data %>% group_by(groups) %>% slice_head(n = 1)
    ) %>%
    mutate(id = row_number()) %>%
    arrange(groups, id)

  hoverText <- paste(
    " <b>",
    results$wordCoord$label,
    "</b>\n Inertia: ",
    round(results$wordCoord$inertia, 3),
    "\n Mass:   ",
    round(results$wordCoord$mass, 3),
    sep = ""
  )

  ## Plot
  fig <- plot_ly(
    data = results$wordCoord,
    x = results$wordCoord$Dim1,
    y = results$wordCoord$Dim2, # customdata=results$wordCoord,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = dotScale,
      color = adjustcolor(colorlist()[results$wordCoord$groups], alpha.f = 0.3), #' rgb(79, 121, 66, .5)',
      line = list(
        color = adjustcolor(
          colorlist()[results$wordCoord$groups],
          alpha.f = 0.3
        ), #' rgb(79, 121, 66, .8)',
        width = 2
      )
    ),
    text = hoverText,
    hoverinfo = "text",
    alpha = .3
  )

  fig <- fig %>%
    layout(
      yaxis = list(
        title = paste0(
          "Dim ",
          dimY,
          " (",
          round(results$ca$eigCorrectedNorm[2], 2),
          "%)"
        ),
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE,
        domain = c(0, 1)
      ),
      xaxis = list(
        title = paste0(
          "Dim ",
          dimX,
          " (",
          round(results$ca$eigCorrectedNorm[1], 2),
          "%)"
        ),
        zeroline = TRUE,
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE
      ),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      showlegend = F
    )

  for (i in seq_len(max(results$wordCoord$groups))) {
    w <- results$wordCoord %>%
      dplyr::filter(groups == i) %>%
      mutate(
        Dim1 = Dim1 + dotSize * 0.005,
        Dim2 = Dim2 + dotSize * 0.01
      )

    if (max(hull_data$groups > 1)) {
      hull_df <- hull_data %>% dplyr::filter(.data$groups == i)
      fig <- fig %>%
        add_polygons(
          x = hull_df$Dim1,
          y = hull_df$Dim2,
          inherit = FALSE,
          showlegend = FALSE,
          color = I(hull_df$color[1]),
          opacity = 0.3,
          line = list(width = 2),
          text = paste0("Cluster ", i),
          hoverinfo = "text",
          hoveron = "points"
        )
    }

    fig <- fig %>%
      add_annotations(
        data = w,
        x = ~Dim1,
        y = ~Dim2,
        xref = "x1",
        yref = "y",
        text = ~labelToPlot,
        font = list(
          family = "sans serif",
          size = labelsize,
          color = w$font.color[1]
        ),
        showarrow = FALSE
      )
  }

  ## Doc to plot
  if (topDocPlot > 0) {
    results$docContrib <- results$docContrib %>%
      select(-any_of(noCol))
    docContrib <- (results$docContrib[, 1] + results$docContrib[, 2]) / 2

    results$docCoord <- results$docCoord %>%
      mutate(contrib = docContrib)

    docCoord <- results$docCoord %>%
      select(all_of(c(dimX, dimY)), label, contrib) %>%
      slice_max(order_by = contrib, n = topDocPlot) %>%
      mutate(dotScaleDoc = contrib * 50 + size) %>%
      rename(
        labelToPlot = label,
        dotSize = contrib
      )

    names(docCoord)[1:2] <- c("Dim1", "Dim2")

    docLabelToRemove <- avoidOverlaps(docCoord, threshold = threshold2 * 1.5)
    docCoord <- docCoord %>%
      mutate(
        label = labelToPlot,
        labelToPlot = ifelse(
          labelToPlot %in% docLabelToRemove,
          "",
          labelToPlot
        ),
        symbol = "hexagon"
      )

    wDoc <- docCoord %>%
      mutate(
        Dim1 = Dim1 + dotScaleDoc * 0.01,
        Dim2 = Dim2 + dotScaleDoc * 0.015
      )

    fig <- fig %>%
      add_markers(
        data = docCoord,
        x = ~Dim1,
        y = ~Dim2,
        text = ~label,
        # type = "scatter", mode = "markers",
        marker = list(
          symbol = docCoord$symbol,
          size = docCoord$dotScaleDoc,
          color = adjustcolor("#6F7378", alpha.f = 0.3),
          line = list(
            color = adjustcolor("#6F7378", alpha.f = 0.3),
            width = 2
          )
        )
      ) %>%
      add_annotations(
        data = wDoc,
        x = ~Dim1,
        y = ~Dim2,
        xref = "x1",
        yref = "y",
        text = ~labelToPlot,
        font = list(
          family = "sans serif",
          size = labelsize,
          color = adjustcolor("#000000", alpha.f = 0.5)
        ), # 4C4E52
        showarrow = FALSE
      )
  }

  fig <- fig %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    event_register("plotly_selecting")
  return(fig)
}

## function to avoid label overlapping ----
avoidOverlaps <- function(w, threshold = 0.10) {
  w[, 2] <- w[, 2] / 2

  Ds <- dist(
    w %>%
      dplyr::filter(labelToPlot != "") %>%
      select(1:2),
    method = "manhattan",
    upper = T
  ) %>%
    dist2df() %>%
    rename(
      from = row,
      to = col,
      dist = value
    ) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>% select(labelToPlot, dotSize),
      by = c("from" = "labelToPlot")
    ) %>%
    rename(w_from = dotSize) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>% select(labelToPlot, dotSize),
      by = c("to" = "labelToPlot")
    ) %>%
    rename(w_to = dotSize) %>%
    filter(dist < threshold)

  if (nrow(Ds) > 0) {
    st <- TRUE
    i <- 1
    label <- NULL
    case <- "n"

    while (isTRUE(st)) {
      if (Ds$w_from[i] > Ds$w_to[i] & Ds$dist[i] < threshold) {
        case <- "y"
        lab <- Ds$to[i]
      } else if (Ds$w_from[i] <= Ds$w_to[i] & Ds$dist[i] < threshold) {
        case <- "y"
        lab <- Ds$from[i]
      }

      switch(
        case,
        "y" = {
          Ds <- Ds[Ds$from != lab, ]
          Ds <- Ds[Ds$to != lab, ]
          label <- c(label, lab)
        },
        "n" = {
          Ds <- Ds[-1, ]
        }
      )

      if (i >= nrow(Ds)) {
        st <- FALSE
      }
      case <- "n"
      # print(nrow(Ds))
    }
  } else {
    label <- NULL
  }

  label
}

## convert a distance object into a data.frame ----
dist2df <- function(inDist) {
  if (class(inDist) != "dist") {
    stop("wrong input type")
  }
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) {
    sequence(A)
  } else {
    attr(inDist, "Labels")
  }
  if (isTRUE(attr(inDist, "Diag"))) {
    attr(inDist, "Diag") <- FALSE
  }
  if (isTRUE(attr(inDist, "Upper"))) {
    attr(inDist, "Upper") <- FALSE
  }
  data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B) - 1):1),
    value = as.vector(inDist)
  )
}


### NETWORK -----

## cooccurrence matrix
coocMatrix <- function(
  x,
  term = "lemma",
  group = "doc_id",
  n = 50,
  pos = TRUE
) {
  term_old <- term
  if (pos) {
    # new_var <- paste0(term,"_upos")
    x$new_var <- paste0(x[[term]], "_", x$upos)
    term <- "new_var"
  } else {
    term <- term_old
  }

  new_doc_id <- unique_identifier(x, fields = group)
  dtm <- document_term_frequencies(
    x %>% mutate(doc_id = new_doc_id),
    term = term
  )

  dtm <- dtm %>%
    mutate(binary = 1)

  if (length(unique(new_doc_id)) == 1) {
    mat <- document_term_matrix(dtm, weight = "freq")
    lab <- colnames(mat)
    mat <- as.numeric(mat)
    names(mat) <- lab
    mat <- sort(mat, decreasing = TRUE)[1:min(n, length(mat))]
    mat <- matrix(mat, 1, length(mat), dimnames = list(x$doc_id[1], names(mat)))
  } else {
    mat <- document_term_matrix(dtm, weight = "binary")
    mat <- dtm_remove_lowfreq(mat, minfreq = 1, maxterms = n)
  }

  mat <- Matrix::crossprod(mat)
  if (sum(mat) - sum(Matrix::diag(mat)) == 0) {
    return(NA)
  }
  mat <- as_cooccurrence(mat)
  mat <- mat %>%
    group_by(term1) %>%
    mutate(s_from = max(cooc)) %>%
    ungroup() %>%
    group_by(term2) %>%
    mutate(s_to = max(cooc)) %>%
    ungroup() %>%
    filter(term1 != term2) %>%
    data.frame()

  if (pos) {
    mat <- mat %>%
      mutate(
        label1 = gsub("_.*", "", term1),
        label2 = gsub("_.*", "", term2),
        upos_from = gsub(".*_", "", term1),
        upos_to = gsub(".*_", "", term2)
      ) %>%
      select(-term1, -term2) %>%
      rename(
        term1 = label1,
        term2 = label2
      )
  } else {
    mat$upos_from <- mat$upos_to <- ""
  }
  return(mat)
}

# word frequency from cooccurence matrix
cooc_freq <- function(cooc) {
  term_freq <- data.frame(
    term = c(cooc$term1, cooc$term2),
    upos = c(cooc$upos_from, cooc$upos_to),
    n = c(cooc$s_from, cooc$s_to)
  ) %>%
    distinct() %>%
    group_by(term, upos) %>%
    summarize(n = sum(n)) %>%
    ungroup()
}

network <- function(
  x,
  term = "lemma",
  group = c("doc_id", "sentence_id"),
  n,
  minEdges,
  labelsize = 4,
  opacity = 0.6,
  interLinks = FALSE,
  normalization = "none",
  remove.isolated = FALSE,
  community.repulsion = 0
) {
  # size scaling
  scalemin <- 20 * (1 + labelsize / 5)
  scalemax <- 70 * (1 + labelsize / 5)

  colorlist <- colorlist()

  # params
  shape <- "dot"
  opacity.min <- 0.4

  # x <- dfTag %>% dplyr::filter(POSSelected)

  cooc <- coocMatrix(x, term = term, group = group, n = n, pos = FALSE)
  if (is.na(cooc)[1]) {
    obj <- list(nodes = NA, edges = NA)
    return(obj)
  }

  nodes <- cooc_freq(cooc) %>%
    mutate(
      id = row_number(),
      shape = shape,
      color = "navyblue"
    ) %>%
    rename(
      label = term,
      value = n
    )

  nodes$font.size <- log(nodes$value)
  # scalemin <- 20
  # scalemax <- 150
  Min <- min(nodes$font.size)
  Max <- max(nodes$font.size)
  if (Max > Min) {
    size <- (nodes$font.size - Min) / (Max - Min) * 15 * labelsize + 10
  } else {
    size <- 10 * labelsize
  }
  size[size < scalemin] <- scalemin
  size[size > scalemax] <- scalemax
  nodes$font.size <- size

  if (shape %in% c("dot", "square")) {
    nodes$font.vadjust <- -0.7 * nodes$font.size
  } else {
    nodes$font.vadjust <- 0
  }

  Normalize <- function(y) {
    if ((max(y) - min(y)) > 0) {
      y <- (y - min(y)) / (max(y) - min(y))
    } else {
      y <- rep(0.2, length(y))
    }
    return(y)
  }

  ### EDGES
  edges <- cooc %>%
    left_join(
      nodes %>% select(id, label),
      by = c("term1" = "label")
    ) %>%
    rename(from = id) %>%
    left_join(
      nodes %>% select(id, label),
      by = c("term2" = "label")
    ) %>%
    rename(
      to = id,
      s = cooc
    ) %>%
    mutate(
      sA = s / (s_from * s_to),
      sC = s / (sqrt(s_from * s_to)),
      sJ = s / (s_from + s_to - s)
    )

  edges$sNorm <- Normalize(edges$s) * 14 + 1
  edges$sANorm <- Normalize(edges$sA) * 14 + 1
  edges$sCNorm <- Normalize(edges$sC) * 14 + 1
  edges$sJNorm <- Normalize(edges$sJ) * 14 + 1
  # sNorm = ((s-min(s))/diff(range(s)))*14+1,
  # sANorm = ((sA-min(sA))/diff(range(sA)))*14+1,
  # sCNorm = ((sC-min(sC))/diff(range(sC)))*14+1,
  # sJNorm = ((sJ-min(sJ))/diff(range(sJ)))*14+1,

  switch(
    normalization,
    none = {
      edges$value <- edges$sNorm
    },
    association = {
      edges$value <- edges$sANorm
    },
    cosine = {
      edges$value <- edges$sCNorm
    },
    jaccard = {
      edges$value <- edges$sJNorm
    }
  )

  if (minEdges == "Auto") {
    y <- quantile(edges$value, seq(1, 0, -0.01), na.rm = T)
    x <- 1:length(y)
    res <- strucchange::breakpoints(y ~ x)
    tailEdges <- y[res$breakpoints[1]]
    # minEdges <- 10*which.min(diff((quantile(edges$value,1-(seq(0,100,10)/100)))))
  } else {
    minEdges <- as.numeric(gsub("%", "", minEdges))
    tailEdges <- quantile(edges$value, 1 - (minEdges / 100), na.rm = T)
  }

  # tailEdges <- quantile(edges$value,1-(minEdges/100))

  edges <- edges %>%
    dplyr::filter(value >= tailEdges) %>%
    select(term1, term2, from, to, value, s, sA, sC, sJ) %>%
    rename(
      term_from = term1,
      term_to = term2
    )

  ### remove isolated
  if (isTRUE(remove.isolated)) {
    id_remove <- setdiff(nodes$id, unique(c(edges$from, edges$to)))
    if (length(id_remove) > 0) {
      nodes <- nodes %>%
        filter(!id %in% id_remove)
      # opacity_font <- opacity_font[-id_remove]
    }
  }

  ### COMMUNITY DETECTION
  graph <- igraph::graph_from_data_frame(
    edges %>% select(-term_from, -term_to),
    directed = FALSE
  )
  cluster <- igraph::cluster_walktrap(graph)
  cluster_df <- data.frame(as.list(igraph::membership(cluster)))
  cluster_df <- as.data.frame(t(cluster_df)) %>%
    mutate(id = as.numeric(gsub("X", "", rownames(.)))) %>%
    rename(group = "V1")

  # Create group column
  nodes <- left_join(nodes, cluster_df, by = "id") %>%
    drop_na(group)

  # Community repulsion
  if (community.repulsion > 0) {
    community.repulsion <- round(community.repulsion * 100)
    # row <- as_edgelist(bsk.network)
    row <- edges %>%
      select(1:2) %>%
      as.matrix()
    membership <- nodes$group
    names(membership) <- nodes$label

    # membership <- V(bsk.network)$community
    # names(membership) <- V(bsk.network)$name
    repulsion <- community.repulsion * max(edges$value, na.rm = T)
    edges$value <- edges$value +
      apply(row, 1, weight.community, membership, repulsion, 1)
  }

  ## opacity for label
  opacity_font <- sqrt(
    (nodes$font.size - min(nodes$font.size)) / diff(range(nodes$font.size))
  ) *
    opacity +
    opacity.min +
    0.1
  if (is.nan(opacity_font[1])) {
    opacity_font <- rep(opacity.min, length(opacity_font))
  }

  if (labelsize > 0) {
    nodes$font.color <- unlist(lapply(opacity_font, function(x) {
      adjustcolor("black", alpha.f = x)
    }))
  } else {
    nodes$font.color <- adjustcolor("black", alpha.f = 0)
  }

  # node colors
  nodes$opacity.nodes <- (opacity_font - min(opacity_font)) /
    (diff(range(opacity_font))) *
    0.5 +
    opacity.min
  nodes$opacity.nodes[is.nan(nodes$opacity.nodes)] <- 0.5
  nodes$color <- paste0(
    colorlist[nodes$group],
    round(nodes$opacity.nodes, 2) * 100
  )

  if (interLinks) {
    interColor <- "#69696920"
  } else {
    interColor <- "#69696900"
  }
  edges <- edges %>%
    left_join(nodes %>% select(id, group, color), by = c("from" = "id")) %>%
    rename(group_from = group) %>%
    left_join(nodes %>% select(id, group), by = c("to" = "id")) %>%
    rename(group_to = group) %>%
    mutate(
      color = ifelse(
        group_from == group_to,
        paste0(substr(color, 1, 7), "20"),
        interColor
      )
    )

  obj <- list(nodes = nodes, edges = edges)
}

net2vis <- function(nodes, edges, click = TRUE, noOverlap = FALSE) {
  layout <- "layout_nicely"

  if ((is.na(nodes))[1]) {
    VIS <- visNetwork::visNetwork(
      nodes = data.frame(
        id = "Empty Network",
        label = "No Connections Among Nodes",
        size = 0,
        title = "No Connections Among Nodes",
        font.size = 20
      ),
      type = "full",
      smooth = TRUE,
      physics = FALSE,
      x = 1,
      y = 1
    ) %>%
      visNetwork::visOptions(
        highlightNearest = list(enabled = T, hover = T, degree = 1),
        nodesIdSelection = F
      ) %>%
      visNetwork::visInteraction(
        dragNodes = TRUE,
        navigationButtons = F,
        hideEdgesOnDrag = TRUE,
        zoomSpeed = 0.2
      )
    return(VIS)
  }

  VIS <-
    visNetwork::visNetwork(
      nodes = nodes,
      edges = edges,
      type = "full",
      smooth = TRUE,
      physics = FALSE
    ) %>%
    visNetwork::visNodes(
      shadow = TRUE,
      shape = nodes$shape,
      font = list(
        color = nodes$font.color,
        size = nodes$font.size,
        vadjust = nodes$font.vadjust
      )
    ) %>%
    visNetwork::visIgraphLayout(layout = layout, type = "full")

  # avoid overlaps among node labels
  ## avoid label overlaps
  if (noOverlap) {
    coords <- VIS$x$nodes %>%
      select(x, y)

    threshold <- 0.03
    ymax <- diff(range(coords[, 2]))
    xmax <- diff(range(coords[, 1]))
    threshold2 <- threshold * mean(xmax, ymax)
    w <- data.frame(
      x = coords[, 1],
      y = coords[, 2],
      labelToPlot = VIS$x$nodes$label,
      dotSize = VIS$x$nodes$font.size,
      row.names = VIS$x$nodes$label
    )
    labelToRemove <- avoidNetOverlaps(w, threshold = threshold2)
  } else {
    labelToRemove <- ""
  }

  VIS$x$nodes <- VIS$x$nodes %>%
    mutate(
      title = label,
      label = ifelse(label %in% labelToRemove, "", label)
    )

  VIS <- VIS %>%
    visNetwork::visEdges(smooth = list(type = "horizontal")) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = T, hover = T, degree = 1),
      nodesIdSelection = T
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.2
    )

  if (click) {
    VIS <- VIS %>%
      visEvents(
        click = "function(nodes){
      Shiny.onInputChange('click', nodes.nodes[0]);
      ;}"
      )
  }

  VIS <- VIS %>%
    # visNetwork::visPhysics(barnesHut=list(avoidOverlap=1)) %>%
    visNetwork::visOptions(
      manipulation = FALSE,
      height = "100%",
      width = "100%"
    )
}

weight.community <- function(row, membership, weigth.within, weight.between) {
  if (
    as.numeric(membership[which(names(membership) == row[1])]) ==
      as.numeric(membership[which(names(membership) == row[2])])
  ) {
    weight <- weigth.within
  } else {
    weight <- weight.between
  }
  return(weight)
}

## function to avoid label overlapping ----
avoidNetOverlaps <- function(w, threshold = 0.10) {
  w[, 2] <- w[, 2] / 3

  Ds <- dist(
    w %>%
      dplyr::filter(labelToPlot != "") %>%
      select(1:2),
    method = "manhattan",
    upper = T
  ) %>%
    dist2df() %>%
    rename(
      from = row,
      to = col,
      dist = value
    ) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>% select(labelToPlot, dotSize),
      by = c("from" = "labelToPlot")
    ) %>%
    rename(w_from = dotSize) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>% select(labelToPlot, dotSize),
      by = c("to" = "labelToPlot")
    ) %>%
    rename(w_to = dotSize) %>%
    filter(dist < threshold)

  if (nrow(Ds) > 0) {
    st <- TRUE
    i <- 1
    label <- NULL
    case <- "n"

    while (isTRUE(st)) {
      if (Ds$w_from[i] > Ds$w_to[i] & Ds$dist[i] < threshold) {
        case <- "y"
        lab <- Ds$to[i]
      } else if (Ds$w_from[i] <= Ds$w_to[i] & Ds$dist[i] < threshold) {
        case <- "y"
        lab <- Ds$from[i]
      }

      switch(
        case,
        "y" = {
          Ds <- Ds[Ds$from != lab, ]
          Ds <- Ds[Ds$to != lab, ]
          label <- c(label, lab)
        },
        "n" = {
          Ds <- Ds[-1, ]
        }
      )

      if (i >= nrow(Ds)) {
        st <- FALSE
      }
      case <- "n"
      # print(nrow(Ds))
    }
  } else {
    label <- NULL
  }
  label
}
## THEMATIC MAP ----
tallThematicmap <- function(
  dfTag,
  term = "lemma",
  group = "doc_id",
  n = 100,
  labelsize = 10,
  n.labels = 1,
  opacity = 0.8
) {
  net <- network(
    LemmaSelection(dfTag) %>% filter(docSelected),
    term = term,
    group = group,
    n = n,
    minEdges = "Auto",
    labelsize = labelsize,
    opacity = opacity,
    interLinks = FALSE,
    normalization = "association",
    remove.isolated = FALSE,
    community.repulsion = 0
  )
  nodes <- net$nodes
  edges <- net$edges %>%
    mutate(sE = sC^2)

  # centrality and density calculation
  thematicIndices <- edges %>%
    mutate(ext = ifelse(group_from != group_to, 1, 0)) %>%
    # filter(group_from == group_to) %>%
    group_by(group_from) %>%
    mutate(k = n()) %>%
    reframe(
      n = max(k),
      centrality = sum(sE * ext) * 10,
      density = sum(sE * (1 - ext) / k) * 100
    ) %>%
    rename(group = group_from) %>%
    left_join(
      nodes %>%
        select(group, label, value) %>%
        group_by(group) %>%
        slice_max(order_by = value, n = 1),
      by = "group"
    ) %>%
    mutate(
      rcentrality = rank(centrality),
      rdensity = rank(density)
    )

  df <- nodes %>%
    group_by(group) %>% # dplyr::filter(sC>1) %>%
    arrange(desc(value), .by_group = TRUE) %>%
    mutate(freq = sum(value)) %>%
    slice_max(n = 10, value, with_ties = FALSE) %>%
    reframe(
      wordlist = paste(label, value, collapse = "\n"),
      name_full = paste(label[1:min(n.labels, n())], collapse = "\n"),
      name_full_gemini = paste(label[1:min(10, n())], collapse = "\n"),
      color = color[1],
      freq = max(freq)
    ) %>%
    right_join(., thematicIndices, by = "group") %>%
    rename(
      name = label,
      groups = group,
      words = wordlist
    ) %>%
    select(-value)

  df_lab <- nodes %>%
    rename(
      Words = label,
      Occurrences = value,
      Cluster = group,
      Color = color
    ) %>%
    group_by(Cluster) %>%
    arrange(desc(Occurrences), .by_group = TRUE) %>%
    mutate(
      Cluster_Label = Words[1],
      Cluster_Frequency = sum(Occurrences)
    ) %>%
    ungroup() %>%
    select(
      "Occurrences",
      "Words",
      "Cluster",
      "Color",
      "Cluster_Label",
      "Cluster_Frequency"
    )

  return(list(net = net, df = df, df_lab = df_lab))
}

plotTM <- function(df, size = 0.5, gemini = FALSE) {
  if (gemini) {
    df$name_full <- df$name_full_gemini
  }
  meandens <- mean(df$rdensity)
  meancentr <- mean(df$rcentrality)
  xlimits <- c(0, max(df$rcentrality) + 1)
  ylimits <- c(0, max(df$rdensity) + 1)
  # size <- 0.5
  df_labels <- df[df$freq > 1, ]
  df_labels <- df_labels %>%
    mutate(size = log(as.numeric(freq)) * (5 + size))

  if (nrow(df_labels) > 1) {
    df_labels <- adjust_positions_oblique(
      df_labels,
      xvar = "rcentrality",
      yvar = "rdensity",
      min_dist = 1
    )
  }

  annotations <- data.frame(
    xpos = sort(c(xlimits, xlimits)),
    ypos = c(ylimits, ylimits),
    words = c(
      "Peripheral Topics",
      "Niche Topics",
      "Basic Topics ",
      "Hot Topics "
    ),
    hjustvar = c(0, 0, 1, 1),
    vjustvar = c(0, 1.0, 0, 1)
  )

  # 1. Crea i punti con hover e dimensione proporzionale
  fig <- plot_ly(
    data = df_labels,
    x = ~rcentrality,
    y = ~rdensity,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = ~size,
      color = ~color,
      opacity = 0.5
    ),
    hovertext = ~words,
    hovertemplate = "%{hovertext}<extra></extra>"
    # hoverinfo = 'hovertext'
  )

  # 2. Aggiungi le etichette (solo se freq > 1)

  fig <- fig %>%
    add_trace(
      data = df_labels,
      x = ~rcentrality,
      y = ~rdensity,
      type = "scatter",
      mode = "text",
      text = ~ tolower(name_full),
      textfont = list(
        color = "rgba(0,0,0,0.7)",
        size = 12 * (1 + size)
      ),
      # hoverinfo = 'none',
      showlegend = FALSE
    )

  # 3. Aggiungi linee medie
  fig <- fig %>%
    add_trace(
      data = NULL,
      inherit = FALSE,
      type = "scatter",
      mode = "lines",
      x = c(xlimits[1], xlimits[2]),
      y = c(meandens, meandens),
      line = list(dash = "dash", color = "rgba(0,0,0,0.7)"),
      marker = list(opacity = 0),
      showlegend = FALSE,
      hoverinfo = "none"
    ) %>%
    add_trace(
      data = NULL,
      inherit = FALSE,
      type = "scatter",
      mode = "lines",
      x = c(meancentr, meancentr),
      y = c(ylimits[1], ylimits[2]),
      line = list(dash = "dash", color = "rgba(0,0,0,0.7)"),
      marker = list(opacity = 0),
      showlegend = FALSE,
      hoverinfo = "none"
    )

  # 4. Eventuali annotazioni (puoi rimuovere questo blocco se non ne hai)
  if (exists("annotations")) {
    k <- 0
    for (i in 1:nrow(annotations)) {
      if (i > 2) {
        k <- max(xlimits) * 0.05
      }
      fig <- fig %>%
        add_annotations(
          x = annotations$xpos[i] - k,
          y = annotations$ypos[i],
          text = annotations$words[i],
          showarrow = FALSE,
          font = list(
            color = "rgba(50,50,50,0.5)",
            size = 8 * (1 + size * 2)
          ),
          xanchor = "center",
          yanchor = "middle"
        )
    }
  }

  # 5. Layout
  fig <- fig %>%
    layout(
      xaxis = list(
        title = "Relevance degree<br>(Centrality)",
        range = xlimits,
        showticklabels = FALSE,
        showline = FALSE,
        zeroline = FALSE,
        ticks = "",
        showgrid = TRUE
      ),
      yaxis = list(
        title = "Development degree<br>(Density)",
        range = ylimits,
        showticklabels = FALSE,
        showline = FALSE,
        zeroline = FALSE,
        ticks = "",
        showgrid = TRUE
      ),
      plot_bgcolor = "#FFFFFF",
      showlegend = FALSE
    )

  return(fig)
}

adjust_positions_oblique <- function(
  df,
  xvar = "rcentrality",
  yvar = "rdensity",
  min_dist = 0.5,
  max_iter = 100,
  step_factor = 0.5,
  jitter_strength = 0.1
) {
  df_adj <- df

  for (iter in 1:max_iter) {
    moved <- FALSE
    for (i in 1:(nrow(df_adj) - 1)) {
      for (j in (i + 1):nrow(df_adj)) {
        xi <- df_adj[[xvar]][i]
        yi <- df_adj[[yvar]][i]
        xj <- df_adj[[xvar]][j]
        yj <- df_adj[[yvar]][j]

        dx <- xi - xj
        dy <- yi - yj
        dist <- sqrt(dx^2 + dy^2)

        # Se perfettamente sovrapposti, applica jitter obliquo casuale
        if (dist == 0) {
          jitter_angle <- runif(1, 0, 2 * pi)
          offset <- jitter_strength

          df_adj[[xvar]][i] <- xi + cos(jitter_angle) * offset
          df_adj[[yvar]][i] <- yi + sin(jitter_angle) * offset
          df_adj[[xvar]][j] <- xj - cos(jitter_angle) * offset
          df_adj[[yvar]][j] <- yj - sin(jitter_angle) * offset

          moved <- TRUE
        } else if (dist < min_dist) {
          angle <- atan2(dy, dx)
          offset <- (min_dist - dist) * step_factor

          df_adj[[xvar]][i] <- xi + cos(angle) * offset
          df_adj[[yvar]][i] <- yi + sin(angle) * offset
          df_adj[[xvar]][j] <- xj - cos(angle) * offset
          df_adj[[yvar]][j] <- yj - sin(angle) * offset

          moved <- TRUE
        }
      }
    }
    if (!moved) break
  }

  return(df_adj)
}


## WORD EMBEDDING TRAINING ----
w2vTraining <- function(x, term = "lemma", dim = 100, iter = 20) {
  # Filter tokens excluding those with upos PUNCT or X
  x <- x %>%
    filter(!upos %in% c("PUNCT", "X")) %>%
    mutate(id = paste0(doc_id, "_", sentence_id))

  stopwords <- x %>%
    filter(upos %in% c("AUX", "DET", "ADP", "CCONJ", "SCONJ", "INTJ")) %>%
    pull(!!sym(term)) %>%
    unique()

  # Group by sentence_id and create a list of token/lemma vectors.
  word_list <- split(x %>% select(any_of(term)) %>% pull() %>% tolower(), x$id)

  w2v_model <- word2vec(
    x = word_list,
    type = "cbow",
    dim = dim,
    iter = iter,
    stopwords = stopwords,
    threads = coresCPU()
  )
  return(w2v_model)
}

summary_stats_embeddings <- function(embedding_matrix, as_tibble = TRUE) {
  # Skewness personalizzata (corretta per bias)
  skewness_custom <- function(x) {
    m <- mean(x)
    s <- sd(x)
    n <- length(x)
    if (s == 0) {
      return(0)
    }
    sum(((x - m) / s)^3) * (n / ((n - 1) * (n - 2)))
  }

  # Kurtosis personalizzata (excess kurtosis corretta per bias)
  kurtosis_custom <- function(x) {
    m <- mean(x)
    s <- sd(x)
    n <- length(x)
    if (s == 0) {
      return(0)
    }
    term1 <- sum(((x - m) / s)^4) *
      (n * (n + 1)) /
      ((n - 1) * (n - 2) * (n - 3))
    term2 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
    return(term1 - term2)
  }

  stats <- data.frame(
    Mean = apply(embedding_matrix, 2, mean),
    Median = apply(embedding_matrix, 2, median),
    SD = apply(embedding_matrix, 2, sd),
    Min = apply(embedding_matrix, 2, min),
    P25 = apply(embedding_matrix, 2, quantile, probs = 0.25),
    P75 = apply(embedding_matrix, 2, quantile, probs = 0.75),
    Max = apply(embedding_matrix, 2, max),
    Range = apply(embedding_matrix, 2, function(x) max(x) - min(x)),
    Skewness = apply(embedding_matrix, 2, skewness_custom),
    Kurtosis = apply(embedding_matrix, 2, kurtosis_custom)
  )

  # Se richiesto, restituisci una tibble (se disponibile)
  if (as_tibble && "tibble" %in% rownames(installed.packages())) {
    stats <- tibble::as_tibble(stats, rownames = "Dimension")
  } else {
    stats$Dimension <- rownames(stats)
    stats <- stats[, c("Dimension", setdiff(names(stats), "Dimension"))]
  }

  return(stats)
}


## cosine among matrix vectors
distance_similarity_stats <- function(embedding_matrix) {
  n <- nrow(embedding_matrix)

  # --- Distanza euclidea ---
  dist_euclidean <- dist(embedding_matrix, method = "euclidean")
  mean_euclidean <- mean(as.vector(dist_euclidean))

  # --- Similarità coseno manuale ---
  cosine_similarity_matrix <- function(mat) {
    # Numeratore: prodotto scalare tra righe
    dot_products <- mat %*% t(mat)
    # Denominatore: norme
    norms <- sqrt(rowSums(mat^2))
    denom <- outer(norms, norms)
    sim <- dot_products / denom
    sim[is.na(sim)] <- 0 # nel caso ci siano divisioni per zero
    return(sim)
  }

  cosine_matrix <- cosine_similarity_matrix(embedding_matrix)
  # Consideriamo solo i valori nella parte superiore senza la diagonale
  cosine_values <- cosine_matrix[upper.tri(cosine_matrix)]
  mean_cosine <- mean(cosine_values, na.rm = TRUE)

  return(list(
    Mean_Euclidean_Distance = mean_euclidean,
    Mean_Cosine_Similarity = mean_cosine
  ))
}


pca_analysis_embeddings <- function(embedding_matrix) {
  pca <- prcomp(embedding_matrix, center = TRUE, scale. = TRUE)
  var_explained <- summary(pca)$importance[2, ]
  return(var_explained)
}

## WORD EMBEDDING SIMILARITY ----
w2vNetwork <- function(w2v_model, dfTag, term, n = 100) {
  w2v_matrix <- as.matrix(w2v_model)

  ## similarity
  top_words <- dfTag %>%
    filter(docSelected) %>%
    filter(upos %in% c("NOUN", "PROPN", "ADJ")) %>%
    group_by(!!sym(term)) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    slice_head(n = n) %>%
    pull(!!sym(term)) %>%
    tolower()

  # remove top_words felt in the stop_word list
  top_words <- intersect(top_words, row.names(as.matrix(w2v_model)))

  similarity <- predict(w2v_model, newdata = top_words)
  df_similarity <- bind_rows(similarity) %>%
    select(-rank) %>%
    rename(from = term1, to = term2)

  # Nodi unici
  nodes <- data.frame(
    id = as.character(unique(c(df_similarity$from, df_similarity$to)))
  ) %>%
    mutate(
      label = id,
      shape = ifelse(id %in% top_words, "triangle", "dot"),
      size = 10,
      font.size = 35
    ) %>%
    arrange(id)

  # Edges
  edges <- df_similarity %>%
    filter(similarity >= 0.5) %>%
    mutate(width = similarity * 10)

  ### COMMUNITY DETECTION
  graph <- igraph::graph_from_data_frame(edges, directed = FALSE)
  cluster <- igraph::cluster_walktrap(graph)
  cluster_df <- data.frame(as.list(igraph::membership(cluster)))
  cluster_df <- as.data.frame(t(cluster_df)) %>%
    rownames_to_column(var = "id") %>%
    rename(group = "V1")

  # Create group column
  nodes <- left_join(nodes, cluster_df, by = "id") %>%
    drop_na(group)

  return(list(nodes = nodes, edges = edges, top_words = top_words))
}

apply_horizontal_transparency <- function(
  nodes,
  x_col = "x",
  y_col = "y",
  threshold = 30,
  type = "transparency"
) {
  # Check that the specified x and y coordinate columns exist in the data frame
  if (!all(c(x_col, y_col) %in% names(nodes))) {
    stop("The specified coordinate columns are not found in the data frame.")
  }

  # Extract the x and y coordinates from the node data frame
  x_coords <- nodes[[x_col]]
  y_coords <- nodes[[y_col]]

  # Compute the absolute pairwise horizontal (x-axis) distances between all nodes
  distance_matrix_x <- outer(x_coords, x_coords, FUN = function(a, b) {
    abs(a - b)
  })

  # Compute vertical (y-axis) distances, but consider only values below a small threshold (0.05),
  # otherwise assign Inf to ignore vertical mismatches
  distance_matrix_y <- outer(y_coords, y_coords, FUN = function(a, b) {
    ifelse(abs(a - b) < 0.05, abs(a - b), Inf)
  })

  # Set the diagonal elements to Inf to avoid self-comparisons
  diag(distance_matrix_x) <- Inf
  diag(distance_matrix_y) <- Inf

  # Identify nodes whose combined horizontal + (negligible) vertical distance is less than the threshold
  overlapping_nodes <- apply(
    distance_matrix_x + distance_matrix_y,
    1,
    function(row) any(row < threshold)
  )

  # Assign font color based on overlap:
  # use semi-transparent black (alpha = 0.3) for overlapping nodes,
  # and fully opaque black (alpha = 0.9) otherwise
  switch(
    type,
    "transparency" = {
      nodes$font.color <- ifelse(
        overlapping_nodes,
        "rgba(0, 0, 0, 0.3)", # semi-transparent label
        "rgba(0, 0, 0, 0.9)"
      ) # fully opaque label
    },
    hide = {
      nodes <- nodes %>%
        mutate(
          overlapping_nodes = overlapping_nodes,
          label = ifelse(overlapping_nodes, "", label)
        ) %>%
        select(-overlapping_nodes)
    }
  )

  return(nodes)
}

w2v2Vis <- function(
  nodes,
  edges,
  layout = "layout_nicely",
  size = 20,
  labelsize = 35,
  overlap = "none"
) {
  nodes$font.size <- labelsize * 2.5
  nodes$size <- round(labelsize / 1.2, 0)
  nodes$font.vadjust = -20
  K <- max(nodes$group)
  colors <- paste0((rep(colorlist(), ceiling(K / 35))), "70", sep = "")
  nodes$color = colors[nodes$group]

  VIS <- visNetwork(
    nodes,
    edges,
    type = "full",
    smooth = TRUE,
    physics = FALSE,
    x = 1,
    y = 1
  ) %>%
    visEdges(smooth = TRUE) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = FALSE)
    ) %>%
    visIgraphLayout(layout = "layout_nicely", type = "full") %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.2
    )

  if (overlap != "none") {
    VIS$x$nodes <- apply_horizontal_transparency(
      VIS$x$nodes,
      threshold = labelsize * 0.0035714,
      type = overlap
    )
  }

  VIS$x$nodes <- VIS$x$nodes %>%
    mutate(title = id)

  return(VIS)
}

w2vUMAP <- function(w2v_model, top_words) {
  cbow_embedding <- as.matrix(w2v_model)
  visualization <- umap(cbow_embedding, n_neighbors = 15, n_threads = 2)

  df <- data.frame(
    word = rownames(cbow_embedding),
    x = visualization$layout[, 1],
    y = visualization$layout[, 2],
    stringsAsFactors = FALSE
  ) %>%
    filter(word %in% top_words)
  return(df)
}

reduce_overlap <- function(df, jitter_amount = 0.05, min_dist = 0.05) {
  df_sorted <- df[order(df$x, df$y), ] # ordinamento spaziale semplice
  for (i in 2:nrow(df_sorted)) {
    dx <- df_sorted$x[i] - df_sorted$x[i - 1]
    dy <- df_sorted$y[i] - df_sorted$y[i - 1]
    dist <- sqrt(dx^2 + dy^2)
    if (dist < min_dist) {
      df_sorted$y[i] <- df_sorted$y[i] + runif(1, -jitter_amount, jitter_amount)
      df_sorted$x[i] <- df_sorted$x[i] + runif(1, -jitter_amount, jitter_amount)
    }
  }
  return(df_sorted)
}


adjust_labels_iterative_with_opacity <- function(
  df,
  min_dist = 0.03,
  max_iter = 50,
  shift_step = 0.05,
  alpha_low = 0.4
) {
  df$opacity_val <- rep(0.9, nrow(df)) # inizialmente opacità massima

  for (iter in seq_len(max_iter)) {
    overlap_found <- FALSE
    for (i in 1:(nrow(df) - 1)) {
      for (j in (i + 1):nrow(df)) {
        dx <- df$x[i] - df$x[j]
        dy <- df$y[i] - df$y[j]
        dist <- sqrt(dx^2 + dy^2)
        if (dist < min_dist) {
          overlap_found <- TRUE
          angle <- atan2(dy, dx) + pi / 2
          df$x[i] <- df$x[i] + shift_step * cos(angle)
          df$y[i] <- df$y[i] + shift_step * sin(angle)
          df$x[j] <- df$x[j] - shift_step * cos(angle)
          df$y[j] <- df$y[j] - shift_step * sin(angle)

          # Diminuzione dell'opacità per j (o entrambi, a scelta)
          df$opacity_val[j] <- min(df$opacity_val[j], alpha_low)
        }
      }
    }
    if (!overlap_found) break
  }

  # Assegnazione colore con trasparenza RGBA
  base_rgb <- "79,121,66" # colore #4F7942 in formato RGB
  df$text_color <- paste0("rgba(", base_rgb, ",", df$opacity_val, ")")
  return(df)
}


## GRAKO ----
grako <- function(
  dfTag,
  normalization = "association",
  n = 50,
  labelsize = 4,
  opacity = 0.6,
  minEdges = 50,
  singleWords = TRUE,
  term = "lemma"
) {
  opacity.min <- 0.5

  # n is the number of NOUNS AND PROPER NOUNS
  if (singleWords) {
    ngram_min <- 1
    dfTag <- rake(
      dfTag,
      group = "doc_id",
      ngram_max = 5,
      ngram_min = ngram_min,
      relevant = c("PROPN"),
      rake.min = -Inf,
      term = term
    )$dfTag %>%
      mutate(
        upos = ifelse(upos == "PROPN", "MULTIWORD", upos),
        ngram = ifelse(upos == "MULTIWORD" & is.na(ngram), 1, ngram)
      )
  } else {
    ngram_min <- 2
    dfTag <- rake(
      dfTag,
      group = "doc_id",
      ngram_max = 5,
      ngram_min = ngram_min,
      relevant = c("PROPN"),
      rake.min = -Inf,
      term = term
    )$dfTag
  }

  # x <- dfTag %>% highlight() %>% dplyr::filter(upos %in% c("MULTIWORD", "VERB"))

  ### EDGES
  x <- dfTag %>%
    highlight() %>%
    dplyr::filter(
      upos %in% c("MULTIWORD", "NOUN", "PROPN", "ADJ", "VERB", "PUNCT")
    )
  cooc <- grakoCoocMatrix(
    x,
    term = term,
    group = c("doc_id", "sentence_id"),
    n = n^2,
    pos = TRUE
  )

  # calculate local occurrences for nodes

  nodes <- cooc_freq(cooc) %>%
    rename(
      label = term,
      value = n
    ) %>%
    filter(upos %in% c("MULTIWORD", "VERB")) %>%
    mutate(
      id = row_number(),
      shape = ifelse(upos == "VERB", "text", "text"),
      color = ifelse(upos == "VERB", "#E41A1C", "#4F7942")
    )

  edges <- cooc %>%
    dplyr::filter(
      upos_from %in%
        c("VERB", "MULTIWORD") &
        upos_to %in% c("VERB", "MULTIWORD")
    ) %>%
    dplyr::filter(
      !upos_from == upos_to &
        !(upos_from == "MULTIWORD" & upos_to == "PROPN") &
        !(upos_to == "MULTIWORD" & upos_from == "PROPN")
    ) %>%
    left_join(
      nodes %>% select(id, label, upos),
      by = c("term1" = "label", "upos_from" = "upos")
    ) %>%
    rename(from = id) %>%
    left_join(
      nodes %>% select(id, label, upos),
      by = c("term2" = "label", "upos_to" = "upos")
    ) %>%
    rename(
      to = id,
      s = cooc
    ) %>%
    drop_na() %>%
    filter(s > 1) %>%
    mutate(
      sA = s / (s_from * s_to),
      sC = s / (sqrt(s_from * s_to)),
      sJ = s / (s_from + s_to - s),
      sNorm = ((s - min(s)) / diff(range(s))),
      role = ifelse(upos_from != "VERB", "active", "passive"),
      color = ifelse(role == "active", "#4F794250", "#E41A1C50")
    )
  switch(
    normalization,
    none = {
      edges$value <- edges$sNorm * 14 + 1
    },
    association = {
      edges$value <- edges$sA * 14 + 1
    },
    cosine = {
      edges$value <- edges$sC * 14 + 1
    },
    jaccard = {
      edges$value <- edges$sJ * 14 + 1
    }
  )

  edges <- edges %>%
    arrange(desc(value)) %>%
    slice_head(n = n)

  tailEdges <- quantile(edges$value, 1 - (minEdges / 100))

  edges <- edges %>%
    dplyr::filter(value >= tailEdges) %>%
    select(
      upos_from,
      term1,
      upos_to,
      term2,
      from,
      to,
      value,
      s,
      sA,
      sC,
      sJ,
      role,
      color
    ) %>%
    rename(
      term_from = term1,
      term_to = term2
    )

  nodes <- nodes %>%
    filter(nodes$id %in% c(edges$from, edges$to))

  nodes$font.size <- log(nodes$value)
  scalemin <- 30
  scalemax <- 60
  Min <- min(nodes$font.size)
  Max <- max(nodes$font.size)
  if (Max > Min) {
    size <- (nodes$font.size - Min) / (Max - Min) * 15 * labelsize + 10
  } else {
    size <- 10 * labelsize
  }
  size[size < scalemin] <- scalemin
  size[size > scalemax] <- scalemax
  nodes$font.size <- size

  nodes$font.vadjust <- ifelse(nodes$shape == "box", -0.7 * nodes$font.size, 0)

  ## opacity for label
  opacity_font <- sqrt(
    (nodes$font.size - min(nodes$font.size)) / diff(range(nodes$font.size))
  ) *
    opacity +
    opacity.min +
    0.1

  if (is.nan(opacity_font[1])) {
    opacity_font <- rep(opacity.min, length(opacity_font))
  }

  # node colors
  nodes$opacity.nodes <- round(
    ((opacity_font - min(opacity_font)) /
      (diff(range(opacity_font))) *
      0.5 +
      opacity.min) *
      100,
    0
  )

  if (labelsize > 0) {
    nodes <- nodes %>%
      mutate(
        opacity.nodes = ifelse(opacity.nodes >= 100, 99, opacity.nodes),
        font.color = ifelse(upos == "VERB", "#E41A1C", "#4F7942")
      )
    # font.color = ifelse(upos=="VERB", paste0("#E41A1C",opacity.nodes), paste0("#4F7942",opacity.nodes)))
    # nodes$font.color <- unlist(lapply(opacity_font, function(x) adjustcolor("black",alpha.f = x)))
  } else {
    nodes <- nodes %>%
      mutate(
        font.color = ifelse(
          upos == "VERB",
          adjustcolor("#E41A1C", alpha.f = 0),
          adjustcolor("#4F7942", alpha.f = 0)
        )
      )
  }

  nodes <- nodes %>%
    mutate(
      title = label,
      label = ifelse(
        upos == "VERB",
        paste0("<i>", label, "</i>"),
        paste0("<b>", label, "</b>")
      ),
      font.multi = "html"
    )

  # info for word in context
  x$grako <- paste0(x[[term]], " ", c(x[[term]][-1], ""))

  obj <- list(
    nodes = nodes,
    edges = edges,
    multiwords = x %>%
      dplyr::filter(upos %in% c("MULTIWORD", "VERB")) %>%
      select(doc_id, sentence_id, sentence_hl, token, lemma, upos, grako)
  )
}

grakoCoocMatrix <- function(
  x,
  term = "lemma",
  group = "doc_id",
  n = 50,
  pos = TRUE
) {
  term_old <- term
  if (pos) {
    # new_var <- paste0(term,"_upos")
    x$new_var <- paste0(x[[term]], "_", x$upos)
    term <- "new_var"
  } else {
    term <- term_old
  }

  mat <- cooccurrence(x[[term]], relevant = rep(TRUE, nrow(x)), skipgram = 0)
  mat <- mat %>%
    group_by(term1) %>%
    mutate(s_from = max(cooc)) %>%
    ungroup() %>%
    group_by(term2) %>%
    mutate(s_to = max(cooc)) %>%
    ungroup() %>%
    filter(term1 != term2) %>%
    data.frame()

  if (pos) {
    mat <- mat %>%
      mutate(
        label1 = gsub("_.*", "", term1),
        label2 = gsub("_.*", "", term2),
        upos_from = gsub(".*_", "", term1),
        upos_to = gsub(".*_", "", term2)
      ) %>%
      select(-term1, -term2) %>%
      rename(
        term1 = label1,
        term2 = label2
      )
  } else {
    mat$upos_from <- mat$upos_to <- ""
  }

  return(mat)
}
grako2vis <- function(nodes, edges) {
  # nodes data.frame for legend
  lnodes <- data.frame(
    label = c("<b>Proper Noun</b>", "<i>Verb</i>"),
    shape = c("text", "text"),
    font.color = c("#4F794290", "#E41A1C90"),
    title = " ",
    id = 1:2,
    font.multi = "html",
    font.size = 14
  ) %>%
    mutate(title = c("Proper Noun", "Verb"))
  # ,
  #                     font.style="font-weight:bold")

  # edges data.frame for legend
  ledges <- data.frame(
    color = c("#4F794270", "#E41A1C90"),
    label = c("active", "passive"),
    arrows = c("to", "to"),
    font.size = 10,
    font.vadjust = -8
  )

  layout <- "layout_nicely"
  VIS <- visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    type = "full",
    smooth = TRUE,
    physics = TRUE,
    export = FALSE
  ) %>%
    visNetwork::visNodes(
      shadow = FALSE,
      shape = nodes$shape,
      font = list(
        color = nodes$font.color,
        size = nodes$font.size,
        vadjust = nodes$font.vadjust,
        multi = nodes$font.multi
      )
    ) %>%
    visNetwork::visIgraphLayout(layout = layout, type = "full") %>%
    visNetwork::visEdges(smooth = list(type = "horizontal")) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = T, hover = T, degree = 1),
      nodesIdSelection = T
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.2
    ) %>%
    visNetwork::visEvents(
      click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}"
    ) %>%
    visNetwork::visOptions(
      manipulation = FALSE,
      height = "100%",
      width = "100%"
    ) %>%
    visNetwork::visLegend(
      addEdges = ledges,
      addNodes = lnodes,
      useGroups = FALSE,
      width = 0.1
    )
}


#### TOPIC MODELING ----

### model tuning

CaoJuan2009 <- function(models) {
  metrics <- sapply(models, function(model) {
    # topic-word matrix
    m1 <- exp(model@beta)
    # pair-wise cosine distance
    pairs <- utils::combn(nrow(m1), 2)
    cos.dist <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      # dist <- lsa::cosine(x, y)
      dist <- crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
      return(dist)
    })
    # metric
    metric <- sum(cos.dist) / (model@k * (model@k - 1) / 2)
    return(metric)
  })
  return(metrics)
}

Arun2010 <- function(models, dtm) {
  # length of documents (count of words)
  len <- slam::row_sums(dtm)
  # evaluate metrics
  metrics <- sapply(models, FUN = function(model) {
    # matrix M1 topic-word
    m1 <- exp(model@beta) # rowSums(m1) == 1
    m1.svd <- svd(m1)
    cm1 <- as.matrix(m1.svd$d)
    # matrix M2 document-topic
    m2 <- model@gamma # rowSums(m2) == 1
    cm2 <- len %*% m2 # crossprod(len, m2)
    norm <- norm(as.matrix(len), type = "m")
    cm2 <- as.vector(cm2 / norm)
    # symmetric Kullback-Leibler divergence
    divergence <- sum(cm1 * log(cm1 / cm2)) + sum(cm2 * log(cm2 / cm1))
    return(divergence)
  })
  return(metrics)
}

Deveaud2014 <- function(models) {
  metrics <- sapply(models, function(model) {
    # topic-word matrix
    m1 <- exp(model@beta)
    # prevent NaN
    if (any(m1 == 0)) {
      m1 <- m1 + .Machine$double.xmin
    }
    # pair-wise Jensen-Shannon divergence
    pairs <- utils::combn(nrow(m1), 2)
    jsd <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      ### divergence by Deveaud2014
      jsd <- 0.5 * sum(x * log(x / y)) + 0.5 * sum(y * log(y / x))
      return(jsd)
    })

    # metric
    metric <- sum(jsd) / (model@k * (model@k - 1))
    return(metric)
  })
  return(metrics)
}

# Funzione per valutare un singolo modello
evaluate_single_k <- function(k, dtm, seed = 1234) {
  model <- LDA(dtm, k = k, method = "VEM", control = list(seed = seed))
  log_lik <- logLik(model)
  perp <- perplexity(model, newdata = dtm)
  return(list(k = k, logLik = log_lik, Perplexity = perp, model = model))
}

# Funzione parallela completa
evaluate_lda_parallel <- function(
  dtm,
  k_seq = 2:20,
  seed = 1234,
  n_cores = detectCores() - 1
) {
  cl <- parallel::makeCluster(n_cores)
  parallel::clusterEvalQ(cl, {
    library(topicmodels)
  })
  parallel::clusterExport(
    cl,
    varlist = c("dtm", "seed", "evaluate_single_k"),
    envir = environment()
  )

  results <- parallel::parLapply(cl, k_seq, function(k) {
    evaluate_single_k(k, dtm, seed)
  })
  parallel::stopCluster(cl)

  # Estrai metriche e modelli
  metrics <- do.call(
    rbind,
    lapply(results, function(l) {
      data.frame(
        k = l$k,
        logLik = as.numeric(l$logLik),
        Perplexity = l$Perplexity
      )
    })
  )
  models <- setNames(lapply(results, function(x) x$model), paste0("k_", k_seq))

  metrics$CaoJuan2009 <- CaoJuan2009(models)
  metrics$Arun2010 <- Arun2010(models, dtm)
  metrics$Deveaud2014 <- Deveaud2014(models)

  return(list(metrics = metrics, models = models))
}

find_elbow <- function(k, metric, decreasing = TRUE, plot = TRUE) {
  # Normalizza i dati
  x <- as.numeric(scale(k))
  y <- as.numeric(scale(if (decreasing) -metric else metric))

  # Calcola distanza punto-linea per ogni punto
  point1 <- c(x[1], y[1])
  point2 <- c(x[length(x)], y[length(y)])
  line_vec <- point2 - point1
  line_vec_norm <- line_vec / sqrt(sum(line_vec^2))

  distances <- sapply(1:length(x), function(i) {
    p <- c(x[i], y[i])
    vec_from_line <- p - point1
    proj_len <- sum(vec_from_line * line_vec_norm)
    proj_point <- point1 + proj_len * line_vec_norm
    dist <- sqrt(sum((p - proj_point)^2))
    return(dist)
  })

  # Trova il massimo della distanza: è il gomito
  elbow_idx <- which.max(distances)
  elbow_k <- k[elbow_idx]

  if (plot) {
    plot(
      k,
      metric,
      type = "b",
      pch = 16,
      xlab = "Number of Topics (k)",
      ylab = "Metric",
      main = "Elbow Method"
    )
    points(k[elbow_idx], metric[elbow_idx], col = "red", pch = 19, cex = 1.5)
    legend("topright", legend = paste("k =", elbow_k), col = "red", pch = 19)
  }

  return(elbow_k)
}

tmTuning <- function(
  x,
  group = c("doc_id", "sentence_id"),
  term = "lemma",
  metric = c("CaoJuan2009", "Deveaud2014", "Arun2010", "Perplexity"),
  n = 100,
  top_by = c("freq", "tfidf"),
  minK = 2,
  maxK = 20,
  Kby = 1
) {
  ## check min and max K
  ClusterRange <- sort(c(minK, maxK))
  minK <- ClusterRange[1]
  maxK <- ClusterRange[2]
  minK <- max(minK, 1)
  maxK <- min(maxK, length(unique(x$doc_id)))
  ###

  # x <- dfTag %>% dplyr::filter(POSSelected)
  x$topic_level_id <- unique_identifier(x, fields = group)

  dtf <- document_term_frequencies(
    x,
    document = "topic_level_id",
    term = "lemma"
  )

  dtm <- document_term_matrix(x = dtf)

  switch(
    top_by,
    freq = {
      dtm <- dtm_remove_lowfreq(dtm, minfreq = 1, maxterms = n)
      dtm <- tm::as.DocumentTermMatrix(dtm, weighting = tm::weightTf)
    },
    tfidf = {
      dtm <- dtm_remove_tfidf(dtm, top = n)
      dtm <- tm::as.DocumentTermMatrix(dtm, weighting = tm::weightTfIdf)
    }
  )

  ## find optimal number of topics K using the librare ldatuning
  result <- evaluate_lda_parallel(
    dtm,
    k_seq = seq(from = minK, to = maxK, by = Kby),
    seed = 1234,
    n_cores = coresCPU()
  )

  return(result)
}

tmTuningPlot <- function(result, metric) {
  df <- result$metrics %>%
    rename(topics = k)

  switch(
    metric,
    CaoJuan2009 = {
      bestT <- find_elbow(
        df$topics,
        df$CaoJuan2009,
        decreasing = TRUE,
        plot = FALSE
      )
    },
    Arun2010 = {
      bestT <- find_elbow(
        df$topics,
        df$Arun2010,
        decreasing = FALSE,
        plot = FALSE
      )
    },
    Deveaud2014 = {
      bestT <- find_elbow(
        df$topics,
        df$Deveaud2014,
        decreasing = TRUE,
        plot = FALSE
      )
    },
    Perplexity = {
      bestT <- find_elbow(
        df$topics,
        df$Perplexity,
        decreasing = TRUE,
        plot = FALSE
      )
    }
  )
  df <- df %>%
    select("topics", any_of(metric))
  names(df) <- c("x", "y")
  df <- df %>%
    mutate(
      y = (y - min(y)) / diff(range(y)),
      color = ifelse(x == bestT, "#cb453e", "#6CC283"),
      size = ifelse(x == bestT, 20, 10)
    )

  hoverText <- paste(
    " <b>Topic ",
    df$x,
    "</b>\n ",
    metric,
    ": ",
    round(df$y, 2),
    sep = ""
  )

  fig <- plot_ly(
    df,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "markers+lines",
    line = list(color = "#6CC28360", width = 2),
    marker = list(
      size = ~size,
      color = ~color, #"#6CC283",
      line = list(
        color = ~color, #color = "#6CC283",
        width = 2
      )
    ),
    text = hoverText,
    hoverinfo = "text"
  ) %>%
    layout(
      annotations = list(
        text = paste0(
          "K selection by Elbow Method with ",
          metric,
          " metric: Optimal K: ",
          bestT
        ),
        xref = "paper",
        x = 0.5,
        yref = "paper",
        y = 1,
        yshift = 30,
        showarrow = FALSE,
        font = list(size = 24, color = "gray30")
      ),
      # title = paste0("K selection by ",metric," metric"),
      paper_bgcolor = "rgb(255,255,255)",
      plot_bgcolor = "rgb(255,255,255)",
      xaxis = list(
        title = "Topics",
        gridcolor = "rgb(229,229,229)",
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE,
        tickcolor = "rgb(229,229,229)",
        ticks = "outside",
        zeroline = TRUE,
        range = c(0, max(df$x) + 1),
        dtick = 1,
        tick0 = 0
      ),
      yaxis = list(
        title = metric,
        gridcolor = "rgb(229,229,229)",
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        tickcolor = "rgb(229,229,229)",
        ticks = "inside",
        zeroline = TRUE,
        range = c(-0.02, 1.05),
        dtick = 0.20,
        tick0 = 0
      ),
      showlegend = FALSE
    )

  fig <- fig %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    event_register("plotly_selecting")

  return(fig)
}

### model estimation

tmEstimate <- function(
  x,
  K,
  group = c("doc_id", "sentence_id"),
  term = "lemma",
  n = 100,
  top_by = c("freq", "tfidf")
) {
  # x <- dfTag %>% dplyr::filter(POSSelected)
  x$topic_level_id <- unique_identifier(x, fields = group)

  dtf <- document_term_frequencies(x, document = "topic_level_id", term = term)

  dtm <- document_term_matrix(x = dtf)

  switch(
    top_by,
    freq = {
      dtm <- dtm_remove_lowfreq(dtm, minfreq = 1, maxterms = n)
    },
    tfidf = {
      dtm <- dtm_remove_tfidf(dtm, top = n)
    }
  )

  # compute the LDA model, inference via 1000 iterations of Gibbs sampling

  topicModel <- LDA(dtm, K, method = "Gibbs", control = list(iter = 500))

  # have a look a some of the results (posterior distributions)
  tmResult <- posterior(topicModel)

  # topics are probability distributions over the entire vocabulary
  beta <- tmResult$terms # get beta from results
  # K distributions over nTerms(DTM) terms

  beta_norm <- beta / matrix(colSums(beta), K, ncol(beta), byrow = TRUE)
  beta_norm <- t(beta_norm) %>%
    as.data.frame() %>%
    mutate(word = colnames(beta_norm))

  variables <- as.character(1:K)
  beta <- t(beta) %>%
    as.data.frame() %>%
    mutate(word = colnames(beta)) %>%
    select(word, all_of(variables))

  # for every document we have a probability distribution of its contained topics
  row_label <- unique(x$doc_id)[as.numeric(row.names(tmResult$topics))]
  theta <- tmResult$topics %>%
    as.data.frame() %>%
    mutate(doc = row_label) %>%
    select(doc, all_of(variables))

  results <- list(
    topicModel = topicModel,
    tmResult = tmResult,
    beta = beta,
    beta_norm = beta_norm,
    theta = theta
  )

  return(results)
}


## hellinger distance ----
hellinger <- function(beta) {
  beta <- sqrt(beta)
  B <- matrix(NA, ncol(beta), ncol(beta))
  for (i in 1:ncol(beta)) {
    for (j in i:ncol(beta)) {
      B[i, j] <- sum((beta[, i] - beta[, j])^2)
    }
  }

  H <- sqrt(B) * (1 / sqrt(2))
}

tmNetwork <- function(beta, minEdge) {
  beta <- as.matrix(results$beta[, -1])

  H <- 1 - hellinger(beta)
  diag(H) <- NA

  topics <- paste0("Topic_", seq(1, nrow(H)))

  H <- data.frame(H)
  colnames(H) <- topics
  H$from <- topics

  H <- H %>%
    pivot_longer(
      cols = 1:length(topics),
      names_to = "to",
      values_to = "size"
    ) %>%
    drop_na()
  edges <- H %>%
    mutate(size = size * 10) %>%
    filter(size > minEdge * 10) %>%
    drop_na()

  nodes <- data.frame(
    id = topics,
    size = 10,
    color = "#4F7942",
    title = topics,
    label = topics,
    font.color = adjustcolor("black", alpha.f = 0.6)
  )

  VIS <- visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    type = "full",
    smooth = TRUE,
    physics = FALSE
  ) %>%
    visNetwork::visIgraphLayout(layout = "layout_nicely", type = "full") %>%
    visNetwork::visEdges(smooth = list(type = "horizontal")) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = T, hover = T, degree = 1),
      nodesIdSelection = T
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.2
    ) %>%
    visNetwork::visOptions(
      manipulation = FALSE,
      height = "100%",
      width = "100%"
    )

  results <- list(H = H %>% rename(value = size), VIS = VIS)
  return(results)
}

tmHeatmap <- function(beta) {
  data <- cor(as.matrix(beta[, -1]))
  diag(data) <- 0

  data <- data[nrow(data):1, ]

  df <- data.frame(data)
  # x <- y <- colnames(df) <- row.names(df) <- paste0("topic ",nrow(data):1)
  id <- sprintf(paste0("%0", nchar(nrow(data)), "d"), nrow(data):1)
  y <- row.names(df) <- paste0("topic ", id)

  x <- colnames(df) <- sort(y)

  df <- df %>%
    rownames_to_column("y") %>%
    pivot_longer(
      cols = starts_with("topic "),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(value = round(value, 3))

  pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))(30)
  pal[1] <- c("#FFFFFF")

  Hplot <- plot_ly(
    z = data,
    x = x,
    y = y,
    text = data,
    type = "heatmap",
    hoverinfo = "none",
    colors = pal,
    zauto = F,
    zmin = -1,
    zmax = 1
  ) %>%
    add_annotations(
      data = df,
      x = ~variable,
      y = ~y,
      text = ~value,
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      font = list(color = "black", size = 10)
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  # create a ggplot2 graph to be exported
  HplotStatic <- ggplot(data = df, aes(x = variable, y = y, fill = value)) +
    geom_tile(color = "white") + # Heatmap con celle delineate in bianco
    scale_fill_gradientn(colors = pal, limits = c(-1, 1)) + # Palette e limiti del gradiente
    geom_text(aes(label = value), color = "black", size = 3) + # Annotazioni con i valori
    theme_minimal() + # Tema minimal
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotazione asse X per leggibilità
      panel.grid = element_blank(), # Rimuove la griglia
      panel.border = element_blank() # Rimuove il bordo
    ) +
    labs(x = NULL, y = NULL, fill = "Value") # Etichette asse

  return(list(Hplot = Hplot, HplotStatic = HplotStatic))
}

tmTopicPlot <- function(beta, topic = 1, nPlot = 10) {
  dfPlot <- beta %>%
    select(word, any_of(as.character(topic)))
  names(dfPlot)[2] <- "y"
  dfPlot <- dfPlot %>%
    arrange(desc(y)) %>%
    slice_max(y, n = nPlot, with_ties = FALSE) %>%
    mutate(
      y = y + runif(nPlot, 0, 1) / (10^7),
      word = factor(word, levels = unique(word)[order(y, decreasing = FALSE)])
    )

  fig <- freqPlotly(
    dfPlot,
    x = "y",
    y = "word",
    n = nPlot,
    ylabel = "Words",
    xlabel = "Beta Probability",
    scale = "identity",
    topicmodel = TRUE,
    colorlist()[topic],
    decimal = 4
  ) %>%
    layout(
      annotations = list(
        text = paste0("Topic ", topic),
        xref = "paper",
        x = 0.5,
        yref = "paper",
        y = 1,
        yshift = 30,
        showarrow = FALSE,
        font = list(size = 24, color = "gray30")
      )
    )

  fig <- fig %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    event_register("plotly_selecting")

  return(fig)
}

tmDocPlot <- function(theta, topic = 1, nPlot = 10) {
  nPlot <- min(nPlot, nrow(theta))
  dfPlot <- theta %>%
    select(doc, any_of(as.character(topic)))
  names(dfPlot)[2] <- "y"
  dfPlot <- dfPlot %>%
    arrange(desc(y)) %>%
    slice_max(y, n = nPlot, with_ties = FALSE) %>%
    mutate(
      y = y + runif(nPlot, 0, 1) / (10^7),
      doc = factor(doc, levels = unique(doc)[order(y, decreasing = FALSE)])
    )

  fig <- freqPlotly(
    dfPlot,
    x = "y",
    y = "doc",
    n = nPlot,
    ylabel = "Documents",
    xlabel = "Theta Probability",
    scale = "identity",
    topicmodel = TRUE,
    colorlist()[topic],
    decimal = 4
  ) %>%
    layout(
      annotations = list(
        text = paste0("Topic ", topic),
        xref = "paper",
        x = 0.5,
        yref = "paper",
        y = 1,
        yshift = 30,
        showarrow = FALSE,
        font = list(size = 24, color = "gray30")
      )
    )

  fig <- fig %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    event_register("plotly_selecting")

  return(fig)
}

### POLARITY DETECTION ----

# download sentiment lexicons
loadSentimentLanguage <- function(language) {
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
  file_lang <- dir(path_language_model, pattern = paste0(language, ".lexicon"))[
    1
  ]

  if (is.na(file_lang)) {
    switch(
      Sys.info()[["sysname"]],
      Windows = {
        download.file(
          url = paste0(
            "https://raw.githubusercontent.com/massimoaria/tall.language.models/main/lexicon.data/",
            language,
            ".lexicon"
          ),
          destfile = paste0(path_language_model, "/", language, ".lexicon"),
          mode = "wb"
        )
      },
      {
        download.file(
          url = paste0(
            "https://raw.githubusercontent.com/massimoaria/tall.language.models/main/lexicon.data/",
            language,
            ".lexicon"
          ),
          destfile = paste0(path_language_model, "/", language, ".lexicon")
        )
      }
    )
  }

  load(file = paste0(path_language_model, "/", language, ".lexicon"))

  return(sentimentData)
}

polarity_colors <- function() {
  c("#FF6666", "#FFB266", "#FFFF66", "#66FF66", "#00FF00")
}

## polarity unit choice ###
ids <- function(dfTag, type) {
  if (is.null(type)) {
    type <- "Documents"
  }
  if (type == "Documents" & "ungroupDoc_id" %in% names(dfTag)) {
    dfTag <- backToOriginalGroups(dfTag)
  }
  unique(dfTag$doc_id[dfTag$docSelected])
}

freqPlotlySentiment <- function(
  dfPlot,
  x,
  y,
  xlabel,
  ylabel,
  scale = c("identity", "log"),
  decimal = 0
) {
  polarity_colors <- polarity_colors()

  # function to build and plot plotly horizontal barplot
  dfPlot <- dfPlot %>%
    group_by(lemma) %>%
    mutate(tot = sum(n)) %>%
    ungroup() %>%
    arrange(tot, lemma, doc_pol_clas)

  xmax <- max(dfPlot[[x]])

  switch(scale, log = {
    # dfPlot$scale <- log(obj$n)
    dfPlot$n <- log(dfPlot$n)
  })

  fig1 <- plot_ly(
    data = dfPlot,
    x = dfPlot[[x]],
    y = ~ reorder(dfPlot[[y]], dfPlot[["tot"]]),
    type = "bar",
    orientation = "h",
    hovertext = ~doc_pol_clas,
    marker = list(
      color = ~ paste0(polarity_colors[as.numeric(doc_pol_clas)], "60"),
      line = list(
        color = ~ polarity_colors[as.numeric(doc_pol_clas)],
        width = 1
      )
    ),
    hovertemplate = "<b><i>Word: %{hovertext}</i></b> <br> <b><i>N. Docs: %{x}</i></b><extra></extra>"
  )

  fig1 <- fig1 %>%
    layout(
      yaxis = list(
        title = ylabel,
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        domain = c(0, 1)
      ),
      xaxis = list(
        title = xlabel,
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE
      ),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )

  fig1 <- fig1 %>%
    # add_annotations(xref = 'x1', yref = 'y',
    #                                x = dfPlot[[x]] + xmax*0.015,  y = dfPlot[[y]],
    #                                text = ann_text,
    #                                font = list(family = 'Arial', size = 12, color = color),
    #                                showarrow = FALSE) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    event_register("plotly_selecting")

  fig1
}

sentimentAnalysis <- function(
  dfTag,
  language = "english",
  lexicon_model = "huliu"
) {
  # lexicon for polarity detection
  if (
    !language %in%
      c(
        "ancient_greek",
        "classical_chinese",
        "coptic",
        "gothic",
        "north_sami",
        "old_church_slavonic",
        "old_french",
        "old_russian",
        "scottish_gaelic",
        "wolof"
      )
  ) {
    sentimentData <- loadSentimentLanguage(language)
  } else {
    return(NA)
  }

  amplifiers <- attr(sentimentData, "amplifiers")
  de_amplifiers <- attr(sentimentData, "de_amplifiers")
  negators <- attr(sentimentData, "negators")

  if (language == "english") {
    sentimentData <- sentimentData %>%
      filter(lexicon == lexicon_model)
  }

  polarity_terms <- data.frame(
    term = sentimentData$Word,
    polarity = sentimentData$sentiment
  )

  sentiment <- txt_sentiment(
    dfTag,
    term = "lemma",
    polarity_terms = polarity_terms,
    polarity_negators = negators,
    polarity_amplifiers = amplifiers,
    polarity_deamplifiers = de_amplifiers,
    amplifier_weight = 0.8,
    n_before = 4,
    n_after = 2,
    constrain = TRUE
  )

  s_data <- sentiment$data
  s_overall <- sentiment$overall

  s_data <- s_data %>%
    left_join(
      s_overall %>%
        select(doc_id, sentiment_polarity) %>%
        rename(doc_polarity = sentiment_polarity),
      by = "doc_id"
    ) %>%
    filter(!is.na(polarity)) %>%
    mutate(
      doc_pol_clas = cut(
        .data$doc_polarity,
        breaks = c(-1, -0.6, -0.2, 0.2, 0.6, 1),
        labels = c(
          "Very Negative",
          "Negative",
          "Neutral",
          "Positive",
          "Very Positive"
        ),
        # labels=c("Very Positive", "Positive", "Neutral", "Negative", "Very Negative"),
        include.lowest = T,
        ordered_result = TRUE
      )
    )

  s_overall <- s_overall %>%
    mutate(
      doc_pol_clas = cut(
        .data$sentiment_polarity,
        breaks = c(-1, -0.6, -0.2, 0.2, 0.6, 1),
        labels = c(
          "Very Negative",
          "Negative",
          "Neutral",
          "Positive",
          "Very Positive"
        ),
        include.lowest = T,
        ordered_result = TRUE
      )
    )

  results <- list(sent_data = s_data, sent_overall = s_overall)
  return(results)
}

sentimentWordPlot <- function(sent_data, n = 10) {
  # sent_dist <- sent_data %>%
  #   group_by(doc_id, doc_pol_clas) %>%
  #   distinct(doc_id) %>%
  #   ungroup() %>%
  #   count(doc_pol_clas, name = "doc_N")

  top_words <- sent_data %>%
    group_by(doc_id, polarity) %>%
    distinct(lemma) %>%
    ungroup() %>%
    group_by(polarity) %>%
    count(lemma) %>%
    slice_max(n, n = 10)

  voc_sent <- sent_data %>%
    group_by(polarity, doc_pol_clas, doc_id) %>%
    distinct(lemma) %>%
    ungroup() %>%
    group_by(polarity, doc_pol_clas) %>%
    count(lemma, sort = TRUE) %>%
    filter(lemma %in% top_words$lemma) # %>%
  # left_join(sent_dist, by="doc_pol_clas") %>%
  # mutate(perc = n/doc_N*100)

  fig_pos <- voc_sent %>%
    dplyr::filter(polarity == 1) %>%
    freqPlotlySentiment(
      x = "n",
      y = "lemma",
      xlabel = "Polarized words count",
      ylabel = "word",
      scale = "identity",
      decimal = 0
    )

  fig_neg <- voc_sent %>%
    dplyr::filter(polarity == -1) %>%
    freqPlotlySentiment(
      x = "n",
      y = "lemma",
      xlabel = "Polarized words count",
      ylabel = "word",
      scale = "identity",
      decimal = 0
    )

  plots <- list(positive = fig_pos, negative = fig_neg)
  return(plots)
}

sentimentPieChart <- function(df) {
  plotly::plot_ly(
    data = df,
    values = ~n,
    labels = ~ factor(Polarity),
    sort = FALSE,
    marker = list(colors = paste0(polarity_colors(), "60")),
    textposition = "outside",
    type = "pie",
    hole = 0.4,
    domain = list(x = c(0, 1), y = c(0, 1))
  ) %>%
    layout(
      legend = list(x = -0.1, y = 0.9),
      xaxis = list(
        ticktext = list(
          "Very Negative",
          "Negative",
          "Neutral",
          "Positive",
          "Very Positive"
        ),
        tickvals = list(-0.8, -0.4, 0, 0.4, 0.8),
        tickmode = "array"
      )
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    ) %>%
    event_register("plotly_click")
}

sentimentDensityPlot <- function(x, from = -1, to = 1) {
  fit <- density(x, from = from, to = to)

  plot_ly(
    x = fit$x,
    y = fit$y,
    type = "scatter",
    mode = "lines",
    color = I("#6CC283"),
    fill = "tozeroy",
    text = NULL,
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(
        ticktext = list(
          "Very Negative",
          "Negative",
          "Neutral",
          "Positive",
          "Very Positive"
        ),
        tickvals = list(-0.8, -0.4, 0, 0.4, 0.8),
        tickmode = "array",
        zeroline = FALSE
      ),
      yaxis = list(domain = c(0, 0.90)),
      annotations = list(
        text = "Density Plot",
        xref = "paper",
        x = 0.5,
        yref = "paper",
        y = 0.95,
        yshift = 30,
        showarrow = FALSE,
        font = list(size = 20, color = "gray30")
      )
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
}

sentimentBoxPlot <- function(sent_overall) {
  plot_ly(
    data = sent_overall,
    x = ~ round(sentiment_polarity, 4),
    y = "",
    type = "box",
    hoverinfo = "x",
    boxpoints = "all",
    jitter = 0.3,
    color = I("#6CC283"),
    pointpos = -1.8
  ) %>%
    layout(
      yaxis = list(
        zeroline = FALSE,
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE,
        domain = c(0, 0.9)
      ),
      xaxis = list(
        title = "",
        zeroline = FALSE,
        showgrid = TRUE,
        showline = FALSE,
        showticklabels = TRUE,
        range = c(-1, 1),
        ticktext = list(
          "Very Negative",
          "Negative",
          "Neutral",
          "Positive",
          "Very Positive"
        ),
        tickvals = list(-0.8, -0.4, 0, 0.4, 0.8),
        tickmode = "array"
      ),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      annotations = list(
        text = "Box Plot",
        xref = "paper",
        x = 0.5,
        yref = "paper",
        y = 0.95,
        yshift = 30,
        showarrow = FALSE,
        font = list(size = 20, color = "gray30")
      )
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        #' toImage',
        "sendDataToCloud",
        "pan2d",
        "select2d",
        "lasso2d",
        "toggleSpikelines",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
}

### TEXT SUMMARIZATION: TEXTRANK -----

highlightSentences <- function(dfTag, id) {
  df <- dfTag %>%
    filter(doc_id == id)

  n <- max(3, round(0.05 * max(df$sentence_id)))

  sentences <- df %>%
    select(sentence_id, sentence) %>%
    distinct()

  terminology <- df %>%
    filter(POSSelected) %>%
    select(sentence_id, lemma)

  tr <- textrank_sentences(data = sentences, terminology = terminology)
  s <- tr$sentences %>%
    arrange(desc(textrank))

  n <- min(n, nrow(s))

  s$h <- c(rep(1, n), rep(0, nrow(s) - n))
  s <- s %>%
    left_join(
      df %>% select(paragraph_id, sentence_id) %>% distinct(),
      by = c("textrank_id" = "sentence_id")
    ) %>%
    mutate(
      sentence = ifelse(
        h == 1,
        paste0("<mark><strong>", sentence, "</strong></mark>"),
        sentence
      )
    ) %>%
    arrange(textrank_id) %>%
    group_by(paragraph_id) %>%
    summarize(
      paragraph = paste(sentence, collapse = " "),
      highlighted = ifelse(sum(h) > 0, "Yes", "No")
    ) %>%
    filter(highlighted == "Yes") %>%
    arrange(paragraph_id) %>%
    select(paragraph_id, paragraph) %>%
    rename(
      "Paragraph ID" = paragraph_id,
      "Paragraph" = paragraph
    )
  return(s)
}

textrankDocument <- function(dfTag, id) {
  df <- dfTag[dfTag$doc_id == id, ]

  # n <- max(3,round(0.05*max(df$sentence_id)))

  sentences <- df %>%
    select(sentence_id, sentence) %>%
    distinct()

  terminology <- df %>%
    filter(POSSelected) %>%
    select(sentence_id, lemma)

  tr <- textrank_sentences(data = sentences, terminology = terminology)
  s <- tr$sentences %>%
    arrange(desc(textrank))

  s <- s %>%
    left_join(
      df %>% select(paragraph_id, sentence_id, sentence) %>% distinct(),
      by = c("sentence")
    )
  results <- list(
    s = s,
    id = id,
    sentences = tr$sentences %>% arrange(desc(textrank))
  )
  return(results)
}

abstractingDocument <- function(s, n, id) {
  switch(
    n,
    "More Concise" = {
      n <- "5%"
    },
    "Less Concise" = {
      n <- "100%"
    },
    {
      n <- n
    }
  )
  n <- as.numeric(gsub("%", "", n))
  n <- ceiling(n * nrow(s) / 100) ## calculate n from %
  n <- min(n, nrow(s))

  s$h <- c(rep(1, n), rep(0, nrow(s) - n))
  # s <- s %>%
  #   left_join(df %>% select(paragraph_id,sentence_id) %>% distinct(), by = c("textrank_id"="sentence_id"))

  abstract <- s %>%
    filter(h == 1) %>%
    group_by(paragraph_id) %>%
    arrange(sentence_id, .by_group = TRUE) %>%
    summarize(paragraph = paste(sentence, collapse = " ")) %>%
    ungroup() %>%
    summarize(
      text = paste(paragraph, collapse = "<br><br>&nbsp&nbsp&nbsp&nbsp&nbsp")
    ) %>%
    mutate(
      text = paste0(
        "<h3>Document: <strong>",
        id,
        "</strong></h3><hr><br><em>",
        text,
        "</em>"
      )
    )

  s <- s %>%
    mutate(
      sentence = ifelse(
        h == 1,
        paste0("<mark><strong>", sentence, "</strong></mark>"),
        sentence
      )
    ) %>%
    arrange(textrank_id) %>%
    group_by(paragraph_id) %>%
    summarize(
      paragraph = paste(sentence, collapse = " "),
      highlighted = ifelse(sum(h) > 0, "Yes", "No")
    ) %>%
    # filter(highlighted=="Yes") %>%
    arrange(paragraph_id) %>%
    select(paragraph_id, paragraph) %>%
    rename(
      "Paragraph ID" = paragraph_id,
      "Paragraph" = paragraph
    )

  results <- list(document = s, abstract = abstract$text[1])
  return(results)
}

### ABSTRACTIVE TEXT SUMMARIZATION: ----

abstractive_summary <- function(
  values,
  input,
  id,
  nL = 250,
  maxTokens = 16384,
  api_key = NULL,
  model = "2.0-flash",
  retry_attempts = 5
) {
  # Input validation
  if (missing(values) || missing(id)) {
    stop("Both 'values' and 'id' parameters are required")
  }

  if (!is.data.frame(values$dfTag)) {
    stop("values$txt must be a data frame")
  }

  # Validate numeric parameters
  if (!is.numeric(nL) || nL <= 0) {
    stop("nL must be a positive number")
  }

  if (!is.numeric(maxTokens) || maxTokens <= 0) {
    stop("maxTokens must be a positive number")
  }

  # Extract document text based on provided ID
  doc_data <- values$dfTag %>%
    filter(doc_id == !!id) %>%
    rebuild_documents()

  # doc_data <- values$txt %>%
  #   filter(doc_id == !!id)

  # Check if document exists
  if (nrow(doc_data) == 0) {
    warning(paste("Document with ID", id, "not found"))
    return(NA)
  }

  # Extract text content
  doc <- doc_data %>%
    pull(text)

  # Handle case where multiple documents have same ID (take first one)
  if (length(doc) > 1) {
    warning(paste(
      "Multiple documents found for ID",
      id,
      "- using first occurrence"
    ))
    doc <- doc[1]
  }

  # Check for empty or missing text
  if (is.na(doc) || nchar(trimws(doc)) == 0) {
    warning(paste("Document", id, "contains no text content"))
    return(NA)
  }

  # Estimate token count for the document
  tryCatch(
    {
      n_tokens <- estimate_gemini_tokens(doc)
    },
    error = function(e) {
      warning(paste("Token estimation failed for document", id, ":", e$message))
      return(NA)
    }
  )

  # Check if document exceeds token limit
  if (n_tokens > maxTokens) {
    return(paste(
      "Document",
      id,
      "too long (",
      n_tokens,
      "tokens >",
      maxTokens,
      "limit), skipping summarization"
    ))
  }

  # Construct detailed prompt for summarization
  prompt <- paste0(
    "Create a comprehensive abstractive summary of the following text. ",
    "Requirements:\n",
    "- Capture all main points and key details\n",
    "- Maintain clarity and readability\n",
    "- Preserve important context and nuances\n",
    "- Target length: approximately ",
    nL,
    " words\n",
    "- Use clear, concise language\n",
    "- Maintain the original tone when appropriate\n\n",
    "Text to summarize:\n\n",
    doc
  )

  if (!is.null(input$abstractivePrompt)) {
    prompt <- paste(input$abstractivePrompt, prompt, sep = "\n\n")
  }

  # Handle API key configuration
  if (is.null(api_key)) {
    # Try to get from environment variable
    api_key <- Sys.getenv("GEMINI_API_KEY", unset = NA)
    if (is.na(api_key)) {
      stop(
        "API key must be provided either as parameter or GEMINI_API_KEY environment variable"
      )
    }
  }

  res <- gemini_ai(
    image = NULL,
    prompt = prompt,
    model = model,
    type = "text",
    retry_503 = retry_attempts,
    api_key = api_key,
    outputSize = "medium"
  )
  return(res)
}

### EXCEL REPORT FUNCTIONS ----
addDataWb <- function(list_df, wb, sheetname, startRow = 1) {
  l <- length(list_df)
  # startRow <- 1
  for (i in 1:l) {
    df <- list_df[[i]]
    n <- nrow(df)
    writeDataTable(
      wb,
      sheetname,
      df,
      startRow = startRow,
      startCol = 1,
      tableStyle = "TableStyleMedium20"
    )
    startRow <- startRow + n + 3
  }
  return(wb)
}

addDataScreenWb <- function(list_df, wb, sheetname) {
  ind <- which(regexpr(sheetname, wb$sheet_names) > -1)
  if (length(ind) > 0) {
    sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
  }
  addWorksheet(wb = wb, sheetName = sheetname, gridLines = FALSE)
  if (!is.null(list_df)) {
    addDataWb(list_df, wb, sheetname, startRow = 1)
    col <- max(unlist(lapply(list_df, ncol))) + 2
  } else {
    col <- 1
  }

  results <- list(wb = wb, col = col, sheetname = sheetname)
  return(results)
}

addGgplotsWb <- function(
  list_plot,
  wb,
  sheetname,
  col,
  width = 10,
  height = 7,
  dpi = 75,
  startRow = 1
) {
  l <- length(list_plot)
  # startRow <- 1
  for (i in 1:l) {
    fileName <- tempfile(
      pattern = "figureImage",
      fileext = ".png"
    )
    if (inherits(list_plot[[i]], "ggplot")) {
      ggsave(
        plot = list_plot[[i]],
        filename = fileName,
        width = width,
        height = height,
        units = "in",
        dpi = dpi
      )
    }
    if (inherits(list_plot[[i]], "igraph")) {
      igraph2PNG(
        x = list_plot[[i]],
        filename = fileName,
        width = width,
        height = height,
        dpi = dpi
      )
    }
    insertImage(
      wb = wb,
      sheet = sheetname,
      file = fileName,
      width = width,
      height = height,
      startRow = startRow,
      startCol = col,
      units = "in",
      dpi = dpi
    )
    startRow <- startRow + (height * 6) + 1
  }
  return(wb)
}

# from igraph to png file
igraph2PNG <- function(x, filename, width = 10, height = 7, dpi = 75) {
  V(x)$centr <- centr_betw(x)$res
  df <- data.frame(
    name = V(x)$label,
    cluster = V(x)$color,
    centr = V(x)$centr
  ) %>%
    group_by(cluster) %>%
    slice_head(n = 3)
  V(x)$label[!(V(x)$label %in% df$name)] <- ""
  png(
    filename = filename,
    width = width,
    height = height,
    unit = "in",
    res = dpi
  )
  grid::grid.draw(plot(x))
  dev.off()
}


addScreenWb <- function(df, wb, width = 14, height = 8, dpi = 75) {
  names(df) <- c("sheet", "file", "n")
  if (nrow(df) > 0) {
    sheet <- unique(df$sheet)
    for (i in 1:length(sheet)) {
      sh <- sheet[i]
      df_sh <- df %>% dplyr::filter(.data$sheet == sh)
      l <- nrow(df_sh)
      startRow <- 30
      for (j in 1:l) {
        fileName <- df_sh$file[j]
        insertImage(
          wb = wb,
          sheet = sh,
          file = fileName,
          width = width,
          height = height,
          startRow = startRow,
          startCol = df_sh$n[j],
          units = "in",
          dpi = dpi
        )
        startRow <- startRow + (height * 10) + 3
      }
    }
  }
  return(wb)
}

addSheetToReport <- function(
  list_df,
  list_plot,
  sheetname,
  wb,
  dpi = 75,
  startRow = 1
) {
  ind <- which(regexpr(sheetname, wb$sheet_names) > -1)
  if (length(ind) > 0) {
    sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
  }
  addWorksheet(wb, sheetname, gridLines = FALSE)

  if (!is.null(list_df)) {
    col <- max(unlist(lapply(list_df, ncol))) + 2
    wb <- addDataWb(list_df, wb = wb, sheetname = sheetname, startRow = 1)
  } else {
    col <- 1
  }

  if (!is.null(list_plot)) {
    wb <- addGgplotsWb(
      list_plot,
      wb = wb,
      sheetname = sheetname,
      col = col,
      dpi = dpi,
      startRow = startRow
    )
  }
  # values$sheet_name <- sheetname
  return(wb)
}

short2long <- function(df, myC) {
  z <- unlist(lapply(myC, function(x) {
    y <- gsub(r"{\s*\([^\)]+\)}", "", x)
    gsub(y, df$long[df$short == y], x)
  }))
  names(myC) <- z
  return(myC)
}


## Labels sheets Report
dfLabel <- function() {
  short <- c(
    "Empty Report",
    "Overview",
    "WordsFreq",
    "PoSFreq",
    "Reinert",
    "CorrespondenceAnalysis",
    "CoWord",
    "ThematicMap",
    "Grako",
    "EmbeddingTraining",
    "EmbeddingSimilarity",
    "KChoice",
    "ModelEstim",
    "PolarityDetection",
    "AbstractiveSummarization",
    "ExtractiveSummarization"
  )

  long <- c(
    "Empty Report",
    "Overview",
    "Words Frequency",
    "PoS Tag Frequency",
    "Reinert Clustering",
    "Correspondence Analysis",
    "Co-Word Analysis",
    "Thematic Map",
    "Grako",
    "Word Embedding Training",
    "Word Embedding Similarity",
    "TM-K choice",
    "TM-Model Estimation",
    "Polarity Detection",
    "Abstractive Summarization",
    "Extractive Summarization"
  )

  data.frame(short = short, long = long)
}

## Add to Report PopUp
popUp <- function(title = NULL, type = "success", btn_labels = "OK") {
  switch(
    type,
    saved = {
      title <- title
      subtitle <- ""
      btn_colors <- "#1d8fe1"
      showButton <- TRUE
      timer <- 3000
    },
    success = {
      title <- paste(title, "\n added to report", sep = "")
      subtitle <- ""
      btn_colors <- "#1d8fe1"
      showButton <- TRUE
      timer <- 3000
    },
    error = {
      title <- "No results to add to the report "
      subtitle <- "Please Run the analysis and then Add it to the report"
      btn_colors <- "#913333"
      showButton <- TRUE
      timer <- 3000
    },
    waiting = {
      title <- "Please wait... "
      subtitle <- "Adding results to report"
      btn_colors <- "#FFA800"
      showButton <- FALSE
      btn_labels <- NA
      timer <- NA
    }
  )
  if (type == "saved") {
    type <- "success"
  }
  show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = btn_colors,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}

## generic popup ###

popUpGeneric <- function(
  title = NULL,
  type = "success",
  color = c("#1d8fe1", "#913333", "#FFA800"),
  subtitle = "",
  btn_labels = "OK",
  html = FALSE
) {
  showButton <- TRUE
  timer <- NA
  show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = html,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = color,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}


### UTILITY FUNCTIONS ----
# find home folder
homeFolder <- function() {
  switch(
    Sys.info()[["sysname"]],
    Windows = {
      home <- Sys.getenv("R_USER")
    },
    Linux = {
      home <- Sys.getenv("HOME")
    },
    Darwin = {
      home <- Sys.getenv("HOME")
    }
  )
  return(home)
}
## check working folder
wdFolder <- function() {
  home <- paste0(homeFolder(), "/tall")
  wdFile <- paste0(home, "/tallWD.tall")
  wdTall <- NULL

  if (file.exists(wdFile)) {
    wdTall <- readLines(wdFile)
    if (!file.exists(wdTall)) {
      file.remove(wdFile)
      wdTall <- NULL
    }
  }
  return(wdTall)
}

# add working folder path to a file name
destFolder <- function(file, folder) {
  paste0(folder, "/", file)
}

sys.time <- function() {
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
}

## Reset reactive values
resetValues <- function() {
  ### Initial values ----
  values <- list()
  values <- reactiveValues()

  if (inherits(try(pagedown::find_chrome(), silent = T), "try-error")) {
    Chrome_url <- NULL
  } else {
    Chrome_url <- pagedown::find_chrome()
  }

  #  Sys.setenv (CHROMOTE_CHROME = Chrome_url)

  ## chrome configuration for shinyapps server

  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c(
          "--disable-gpu",
          "--no-sandbox",
          "--disable-dev-shm-usage", # required bc the target easily crashes
          c("--force-color-profile", "srgb")
        )
      ))
    )
  }
  ## end configuration

  ## Check if Chrome browser is installed on the computer
  if (is.null(Chrome_url)) {
    showModal(modalDialog(
      title = strong("Warning message!"),
      HTML(
        "Chrome or a Chromium-based browser is not installed on your computer.<br>
If you do not have either of these browsers installed, TALL will be unable to export graphs.<br>
To ensure the functionality of Biblioshiny,
           please download Chrome by <a href='https://www.google.com/chrome/' target='_blank' > <b>clicking here</b></a>."
      ),
      footer = modalButton("Dismiss"),
      easyClose = TRUE
    ))
  } else {
    Sys.setenv(CHROMOTE_CHROME = Chrome_url)
  }
  values$Chrome_url <- Chrome_url
  values$biblioshiny <- NULL
  values$resetNeed <- FALSE
  values$normButton <- FALSE
  values$path <- NULL
  values$custom_lists <- NULL
  values$txt <- data.frame()
  values$corpusElements <- data.frame()
  values$txtOriginal <- data.frame()
  values$list_file <- data.frame(sheet = NULL, file = NULL, n = NULL)
  values$POSTagSelected <- ""
  values$wb <- openxlsx::createWorkbook()
  values$dfLabel <- dfLabel()
  values$posMwSel <- c("ADJ", "NOUN", "PROPN") # POS selected by default for multiword creation
  values$myChoices <- "Empty Report"
  values$generalTerm <- "lemma"

  accuracy <- model_accuracy()
  values$accuracy <- accuracy
  languages_df <- langrepo()
  values$language <- "english"
  values$languages <- languages_df
  label_lang <- unique(languages_df$language_name)
  names(label_lang) <- gsub("_", " ", label_lang)
  values$label_lang <- label_lang
  values$treebank <- languages_df$treebank[1]
  values$chapter <- languages_df$chapter
  values$flag <- "GB.svg"
  values$TMplotIndex <- 1
  values$TMdocIndex <- 1
  values$tmTopSentences <- FALSE
  values$selectedGroups <- NULL
  values$selectedFilter <- ""
  values$wdTall <- wdFolder()
  if (is.null(wdFolder())) {
    values$menu <- -2
  } else {
    values$menu <- -1
  }

  ## gemini api and model
  home <- homeFolder()
  path_gemini_key <- paste0(
    file.path(home, "tall"),
    "/.tall_gemini_key.txt",
    collapse = ""
  )
  # check if sub directory exists
  values$geminiAPI <- load_api_key(path_gemini_key)
  values$corpus_description <- NULL
  values$gemini_additional <- NULL

  path_gemini_model <- path_gemini_key <- paste0(
    file.path(home, "tall"),
    "/.tall_gemini_model.txt",
    collapse = ""
  )
  gemini_api_model <- loadGeminiModel(path_gemini_model)
  values$gemini_api_model <- gemini_api_model[1]
  values$gemini_output_size <- gemini_api_model[2]

  values$abstractivePrompt <- NULL

  return(values)
}

firstUp <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

## Color palette for plots
colorlist <- function() {
  c(
    "#4DAF4A",
    "#E41A1C",
    "#377EB8",
    "#984EA3",
    "#FF7F00",
    "#A65628",
    "#F781BF",
    "#999999",
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854",
    "#FFD92F",
    "#B3B3B3",
    "#A6CEE3",
    "#1F78B4",
    "#B2DF8A",
    "#33A02C",
    "#FB9A99",
    "#E31A1C",
    "#FDBF6F",
    "#FF7F00",
    "#CAB2D6",
    "#6A3D9A",
    "#B15928",
    "#8DD3C7",
    "#BEBADA",
    "#FB8072",
    "#80B1D3",
    "#FDB462",
    "#B3DE69",
    "#D9D9D9",
    "#BC80BD",
    "#CCEBC5"
  )
}


## POS selection functions ----
# Set TRUE PoS selected ##
posSel <- function(dfTag, pos) {
  dfTag <- dfTag %>% mutate(POSSelected = ifelse(upos %in% pos, TRUE, FALSE))
  # dfTag <- highlight(dfTag)
}

# remove Hapax and lowwer and higher lemmas
removeHapaxFreq <- function(dfTag, hapax, singleChar) {
  ## reset noHapax column
  dfTag <- dfTag %>%
    mutate(noHapax = TRUE)

  ## Hapax
  if (is.null(hapax)) {
    H <- dfTag %>%
      group_by(lemma) %>%
      count() %>%
      filter(n == 1) %>%
      select(lemma)
    H <- unique(H$lemma)
    dfTag <- dfTag %>%
      mutate(noHapax = ifelse(lemma %in% H, FALSE, TRUE))
  }

  ## Single Char
  if (is.null(singleChar)) {
    dfTag <- dfTag %>%
      mutate(
        noSingleChar = ifelse(nchar(lemma) > 1, TRUE, FALSE),
        noSingleChar = ifelse(
          upos %in% c("EMOJI", "MENTION", "HASH", "IP_ADDRESS", "URL", "EMAIL"),
          TRUE,
          noSingleChar
        )
      )
  }

  return(dfTag)
}

# Select lemmas by PoS Tags, Hapax and Frequency Range #
LemmaSelection <- function(dfTag) {
  if (!"noHapax" %in% names(dfTag)) {
    dfTag <- dfTag %>%
      mutate(noHapax = TRUE)
  }
  if (!"noSingleChar" %in% names(dfTag)) {
    dfTag <- dfTag %>%
      mutate(noSingleChar = TRUE)
  }

  dfTag <- dfTag %>%
    dplyr::filter(POSSelected, noHapax, noSingleChar) %>%
    arrange(doc_id, paragraph_id, sentence_id)

  return(dfTag)
}


# ## Highlight function ----
highlight_segments <- function(tc, n) {
  segments <- tc$segments
  n <- 10
  id <- tc$terms %>%
    slice_max(order_by = chi_square, n = n)
  id <- id$term
  for (i in 1:length(id)) {
    segments$segment <- stringi::stri_replace_all_fixed(
      segments$segment,
      id[i],
      paste0("<mark><strong>", id[i], "</strong></mark>")
    )
  }
  tc$segments <- segments
  return(tc)
}

highlight_word <- function(input_string, target_word, upos) {
  # Controllo della validità della parola target
  if (
    is.na(target_word) ||
      target_word == "" ||
      upos %in%
        c("DET", "PART", "PUNCT", "X", "SYM", "INTJ", "NUM", "NGRAM_MERGED")
  ) {
    return(input_string)
  }

  # Escape dei caratteri speciali
  target_word_escaped <- gsub(
    "([.\\^$*+?()\\[\\]|\\\\])",
    "\\\\\\1",
    target_word,
    perl = TRUE
  )

  # Controllo se la parola contiene caratteri non alfanumerici
  if (grepl("\\W", target_word, perl = TRUE)) {
    word_boundary <- "" # Nessun confine di parola se contiene caratteri speciali
  } else {
    word_boundary <- "\\b"
  }

  # Sostituzione con evidenziazione HTML
  highlighted_string <- gsub(
    paste0(word_boundary, target_word_escaped, word_boundary),
    paste0("<mark><strong>", target_word, "</strong></mark>"),
    input_string,
    perl = TRUE
  )

  return(highlighted_string)
}

# highlight <- function(df){
#   df <- df %>%
#     mutate(sentence_hl = mapply(highlight_word, sentence, token, upos))
# }

highlight <- function(df, term = "lemma", upos = NULL) {
  # Ensure term is valid
  if (!term %in% c("lemma", "token")) {
    stop("Invalid term. Use 'lemma' or 'token'.")
  }

  # Dynamically select the column based on term argument
  term_col <- sym(term)

  if (is.null(upos)) {
    df <- df %>%
      mutate(sentence_hl = mapply(highlight_word, sentence, !!term_col, upos))
  } else {
    dfUpos <- df %>%
      mutate(id = row_number()) %>%
      filter(upos %in% upos) %>%
      mutate(
        sentence_hl = mapply(highlight_word, sentence, !!term_col, upos)
      ) %>%
      select("id", "sentence_hl")

    df <- df %>%
      mutate(id = row_number()) %>%
      left_join(dfUpos, by = "id") %>%
      mutate(sentence_hl = coalesce(sentence_hl.y, sentence_hl.x)) %>%
      select(-sentence_hl.x, -sentence_hl.y, -id) # Remove the extra columns
  }

  return(df)
}


## saveTall function ----
saveTall <- function(
  dfTag,
  custom_lists,
  language,
  treebank,
  menu,
  where,
  file,
  generalTerm,
  corpus_description
) {
  D <- date()
  D <- strsplit(gsub("\\s+", " ", D), " ")
  D <- paste(unlist(D)[c(1, 2, 3, 5)], collapse = " ")
  save(
    dfTag,
    custom_lists,
    language,
    treebank,
    menu,
    D,
    where,
    file = file,
    generalTerm,
    corpus_description
  )
}

### Export Tall analysis in .tall file ----

# SIDEBARMENU DYNAMIC ----
menuList <- function(menu) {
  switch(
    as.character(menu),
    "-2" = {
      list(
        menuItem("Settings", tabName = "settings", icon = icon("tasks"))
      )
    },
    "0" = {
      list(
        menuItem(
          "Import",
          tabName = "import_tx",
          icon = icon("open-file", lib = "glyphicon")
        ),
        menuItem(
          "Edit",
          tabName = "edit_tx",
          icon = icon("edit", lib = "glyphicon"),
          menuSubItem(
            "Split",
            tabName = "split_tx",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "Random Selection",
            tabName = "randomText",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "External Information",
            tabName = "extInfo",
            icon = icon("chevron-right")
          )
        ),
        menuItem(
          "Pre-processing",
          tabName = "prePro",
          icon = icon("indent-right", lib = "glyphicon"),
          startExpanded = TRUE,
          menuSubItem(
            "Tokenization & PoS Tagging",
            tabName = "tokPos",
            icon = icon("chevron-right"),
            selected = TRUE
          )
        ),
        menuItem("Settings", tabName = "settings", icon = icon("tasks"))
      )
    },
    "1" = {
      list(
        menuItem(
          "Import",
          tabName = "import_tx",
          icon = icon("open-file", lib = "glyphicon")
        ),
        # menuItem("Edit", tabName = "edit_tx", icon = icon("edit", lib="glyphicon"),
        #          menuSubItem("Split", tabName = "split_tx", icon = icon("chevron-right")),
        #          menuSubItem("Random Selection", tabName = "randomText", icon = icon("chevron-right")),
        #          menuSubItem("External Information", tabName = "extInfo", icon = icon("chevron-right"))),
        menuItem(
          "Pre-processing",
          tabName = "prePro",
          icon = icon("indent-right", lib = "glyphicon"),
          startExpanded = TRUE,
          menuSubItem(
            "Tokenization & PoS Tagging",
            tabName = "tokPos",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "Tagging Special Entities",
            tabName = "posSpecial",
            icon = icon("chevron-right")
          ),
          menuItem(
            "Multi-Word",
            tabName = "multiword",
            icon = icon("chevron-right"),
            startExpanded = TRUE,
            menuSubItem(
              "Automatic",
              tabName = "multiwordCreat",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "By a List",
              tabName = "multiwordByList",
              icon = icon("chevron-right")
            )
          ),
          menuSubItem(
            "Custom Term List",
            tabName = "custTermList",
            icon = icon("chevron-right"),
            selected = TRUE
          ),
          menuSubItem(
            "PoS Tag Selection",
            tabName = "posTagSelect",
            icon = icon("chevron-right")
          )
        ),
        menuItem("Settings", tabName = "settings", icon = icon("tasks"))
      )
    },
    "2" = {
      list(
        menuItem(
          "Import",
          tabName = "import_tx",
          icon = icon("open-file", lib = "glyphicon")
        ),
        # menuItem("Edit", tabName = "edit_tx", icon = icon("edit", lib="glyphicon"),
        #          menuSubItem("Split", tabName = "split_tx", icon = icon("chevron-right")),
        #          menuSubItem("Random Selection", tabName = "randomText", icon = icon("chevron-right")),
        #          menuSubItem("External Information", tabName = "extInfo", icon = icon("chevron-right"))),
        menuItem(
          "Pre-processing",
          tabName = "prePro",
          icon = icon("indent-right", lib = "glyphicon"),
          startExpanded = TRUE,
          menuSubItem(
            "Tokenization & PoS Tagging",
            tabName = "tokPos",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "Tagging Special Entities",
            tabName = "posSpecial",
            icon = icon("chevron-right")
          ),
          menuItem(
            "Multi-Word",
            tabName = "multiword",
            icon = icon("chevron-right"),
            startExpanded = TRUE,
            menuSubItem(
              "Automatic",
              tabName = "multiwordCreat",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "By a List",
              tabName = "multiwordByList",
              icon = icon("chevron-right")
            )
          ),
          menuSubItem(
            "Custom Term List",
            tabName = "custTermList",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "PoS Tag Selection",
            tabName = "posTagSelect",
            icon = icon("chevron-right")
          ),
          selected = TRUE
        ),
        # menuItem("Filter", tabName = "filter_text", icon = icon("filter")),
        # menuItem("Groups",tabName = "defineGroups", icon = icon("th", lib="glyphicon")),
        menuItem(
          "Overview",
          tabName = "overview",
          icon = icon("search", lib = "glyphicon")
        ),
        menuItem(
          "Words",
          tabName = "words",
          icon = icon("font", lib = "glyphicon"),
          # menuItem("Frequencies",
          #   tabName = "freqList", icon = icon("chevron-right"),
          #   menuSubItem("Words", tabName = "w_freq", icon = icon("chevron-right")),
          #   menuSubItem("Part of Speech", tabName = "w_pos", icon = icon("chevron-right"))
          # ),
          menuSubItem(
            "Words in Context",
            tabName = "wordCont",
            icon = icon("chevron-right")
          ),
          # menuSubItem("Clustering", tabName = "w_clustering", icon = icon("chevron-right")),
          menuSubItem(
            "Reinert Clustering",
            tabName = "w_reinclustering",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "Correspondence Analysis",
            tabName = "ca",
            icon = icon("chevron-right")
          ),
          menuItem(
            "Network",
            tabName = "w_network",
            icon = icon("chevron-right"),
            menuSubItem(
              "Co-word analysis",
              tabName = "w_networkCooc",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "Thematic Map",
              tabName = "w_networkTM",
              icon = icon("chevron-right")
            )
            # ,menuSubItem("Grako", tabName = "w_networkGrako", icon = icon("chevron-right"))
          ),
          menuItem(
            "Word Embeddings",
            tabName = "w_embeddings",
            icon = icon("chevron-right"),
            menuSubItem(
              "Training",
              tabName = "w_word2vec",
              icon = icon("chevron-right")
            ),
            # if (embedding){
            menuSubItem(
              "Similarity",
              tabName = "w_w2v_similarity",
              icon = icon("chevron-right")
            )
            # }
          )
        ),
        menuItem(
          "Documents",
          tabName = "documents",
          icon = icon(name = "duplicate", lib = "glyphicon"),
          menuItem(
            "Topic Modeling",
            tabName = "d_topicMod",
            icon = icon("chevron-right"),
            menuSubItem(
              "K choice",
              tabName = "d_tm_select",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "Model Estimation",
              tabName = "d_tm_estim",
              icon = icon("chevron-right")
            )
          ),
          menuSubItem(
            "Polarity Detection",
            tabName = "d_polDet",
            icon = icon("chevron-right")
          ),
          menuItem(
            "Summarization",
            tabName = "summarization",
            icon = icon("chevron-right"),
            menuSubItem(
              "Abstractive",
              tabName = "d_astractive",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "Extractive",
              tabName = "d_summarization",
              icon = icon("chevron-right")
            )
          )
        ),
        menuItem("Report", tabName = "report", icon = icon("list-alt")),
        menuItem("Settings", tabName = "settings", icon = icon("tasks"))
      )
    },
    "3" = {
      list(
        menuItem(
          "Import",
          tabName = "import_tx",
          icon = icon("open-file", lib = "glyphicon")
        ),
        # menuItem("Edit", tabName = "edit_tx", icon = icon("edit", lib="glyphicon"),
        #          menuSubItem("Split", tabName = "split_tx", icon = icon("chevron-right")),
        #          menuSubItem("Random Selection", tabName = "randomText", icon = icon("chevron-right")),
        #          menuSubItem("External Information", tabName = "extInfo", icon = icon("chevron-right"))),
        menuItem(
          "Pre-processing",
          tabName = "prePro",
          icon = icon("indent-right", lib = "glyphicon"),
          startExpanded = TRUE,
          menuSubItem(
            "Tokenization & PoS Tagging",
            tabName = "tokPos",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "Tagging Special Entities",
            tabName = "posSpecial",
            icon = icon("chevron-right")
          ),
          menuItem(
            "Multi-Word",
            tabName = "multiword",
            icon = icon("chevron-right"),
            startExpanded = TRUE,
            menuSubItem(
              "Automatic",
              tabName = "multiwordCreat",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "By a List",
              tabName = "multiwordByList",
              icon = icon("chevron-right")
            )
          ),
          menuSubItem(
            "Custom Term List",
            tabName = "custTermList",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "PoS Tag Selection",
            tabName = "posTagSelect",
            icon = icon("chevron-right")
          ),
          selected = TRUE
        ),
        menuItem("Filter", tabName = "filter_text", icon = icon("filter")),
        menuItem(
          "Groups",
          tabName = "defineGroups",
          icon = icon("th", lib = "glyphicon")
        ),
        menuItem(
          "Overview",
          tabName = "overview",
          icon = icon("search", lib = "glyphicon")
        ),
        menuItem(
          "Words",
          tabName = "words",
          icon = icon("font", lib = "glyphicon"),
          # menuItem("Frequencies",
          #   tabName = "freqList", icon = icon("chevron-right"),
          #   menuSubItem("Words", tabName = "w_freq", icon = icon("chevron-right")),
          #   menuSubItem("Part of Speech", tabName = "w_pos", icon = icon("chevron-right"))
          # ),
          menuSubItem(
            "Words in Context",
            tabName = "wordCont",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "Reinert Clustering",
            tabName = "w_reinclustering",
            icon = icon("chevron-right")
          ),
          menuSubItem(
            "Correspondence Analysis",
            tabName = "ca",
            icon = icon("chevron-right")
          ),
          menuItem(
            "Network",
            tabName = "w_network",
            icon = icon("chevron-right"),
            menuSubItem(
              "Co-word analysis",
              tabName = "w_networkCooc",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "Thematic Map",
              tabName = "w_networkTM",
              icon = icon("chevron-right")
            )
            # ,menuSubItem("Grako", tabName = "w_networkGrako", icon = icon("chevron-right"))
          ),
          menuItem(
            "Word Embeddings",
            tabName = "w_embeddings",
            icon = icon("chevron-right"),
            menuSubItem(
              "Training",
              tabName = "w_word2vec",
              icon = icon("chevron-right")
            ),
            # if (embedding){
            menuSubItem(
              "Similarity",
              tabName = "w_w2v_similarity",
              icon = icon("chevron-right")
            )
            # }
          )
        ),
        menuItem(
          "Documents",
          tabName = "documents",
          icon = icon(name = "duplicate", lib = "glyphicon"),
          menuItem(
            "Topic Modeling",
            tabName = "d_topicMod",
            icon = icon("chevron-right"),
            menuSubItem(
              "K choice",
              tabName = "d_tm_select",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "Model Estimation",
              tabName = "d_tm_estim",
              icon = icon("chevron-right")
            )
          ),
          menuSubItem(
            "Polarity Detection",
            tabName = "d_polDet",
            icon = icon("chevron-right")
          ),
          menuItem(
            "Summarization",
            tabName = "summarization",
            icon = icon("chevron-right"),
            menuSubItem(
              "Abstractive",
              tabName = "d_astractive",
              icon = icon("chevron-right")
            ),
            menuSubItem(
              "Extractive",
              tabName = "d_summarization",
              icon = icon("chevron-right")
            )
          )
        ),
        menuItem("Report", tabName = "report", icon = icon("list-alt")),
        menuItem("Settings", tabName = "settings", icon = icon("tasks"))
      )
    },
    {
      list(
        menuItem(
          "Import",
          tabName = "import_tx",
          icon = icon("open-file", lib = "glyphicon")
        ),
        menuItem("Settings", tabName = "settings", icon = icon("tasks"))
      )
    }
  )
}

# DATA TABLE FORMAT ----
DTformat <- function(
  df,
  nrow = 10,
  filename = "Table",
  pagelength = TRUE,
  left = NULL,
  right = NULL,
  numeric = NULL,
  dom = TRUE,
  size = "85%",
  filter = "top",
  columnShort = NULL,
  columnSmall = NULL,
  round = 2,
  title = "",
  button = FALSE,
  delete = FALSE,
  escape = FALSE,
  selection = FALSE,
  specialtags = FALSE
) {
  if ("text" %in% names(df)) {
    df <- df %>%
      mutate(text = gsub("<|>", "", text))
  }

  if (length(columnShort) > 0) {
    columnDefs <- list(
      list(
        className = "dt-center",
        targets = 0:(length(names(df)) - 1)
      ),
      list(
        targets = columnShort - 1,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 500 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 500) + '...</span>' : data;",
          "}"
        )
      )
    )
  } else {
    columnDefs <- list(list(
      className = "dt-center",
      targets = 0:(length(names(df)) - 1)
    ))
  }

  if (isTRUE(pagelength)) {
    buttons <- list(
      list(extend = "pageLength"),
      list(
        extend = "excel",
        filename = paste0(filename, "_tall_", sys.time()),
        title = " ",
        header = TRUE,
        exportOptions = list(
          modifier = list(page = "all")
        )
      )
    )
  } else {
    buttons <- list(
      list(
        extend = "excel",
        filename = paste0(filename, "_tall_", sys.time()),
        title = " ",
        header = TRUE,
        exportOptions = list(
          modifier = list(page = "all")
        )
      )
    )
  }

  if (isTRUE(dom)) {
    dom <- "Brtip"
  } else {
    dom <- "Bt"
  }

  if (nchar(title) > 0) {
    caption <- htmltools::tags$caption(
      style = "caption-side: top; text-align: center; color:black;  font-size:140% ;",
      title
    )
  } else {
    caption <- htmltools::tags$caption(
      style = "caption-side: top; text-align: center; color:black;  font-size:140% ;",
      ""
    )
  }

  if (isTRUE(button)) {
    df <- df %>%
      mutate(
        Document = paste0(
          '<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'',
          doc_id,
          '\')">View</button>'
        )
      ) %>%
      select(Document, everything())
  }

  if (isTRUE(specialtags)) {
    df <- df %>%
      rename("Special Entity" = "UPOS") %>%
      mutate(
        "Frequency Distribution" = paste0(
          '<button id2="custom_btn" onclick="Shiny.onInputChange(\'button_id2\', \'',
          `Special Entity`,
          '\')">View</button>'
        )
      ) %>%
      select("Frequency Distribution", everything())
  }

  if (isTRUE(delete)) {
    df <- df %>%
      mutate(
        Remove = paste0(
          '<button id="custom_btn_del" onclick="Shiny.onInputChange(\'button_id_del\', \'',
          doc_id,
          '\')">Remove</button>'
        )
      ) %>%
      select(Document, Remove, everything())
  }

  if (isTRUE(selection)) {
    extensions <- c("Buttons", "Select", "ColReorder", "FixedHeader")
    buttons <- c(buttons, c("selectAll", "selectNone"))
    select <- list(style = "multiple", items = "row", selected = 1:nrow(df))
    # selection = list(mode = 'multiple', selected = 1:nrow(df), target = 'row')
  } else {
    extensions <- c("Buttons", "ColReorder", "FixedHeader")
    select <- NULL
    # selection = "none"
  }

  tab <- DT::datatable(
    df,
    escape = escape,
    rownames = FALSE,
    caption = caption,
    selection = "none",
    extensions = extensions,
    filter = filter,
    options = list(
      colReorder = TRUE,
      fixedHeader = TRUE,
      pageLength = nrow,
      autoWidth = FALSE,
      scrollX = TRUE,
      dom = dom,
      buttons = buttons,
      select = select,
      lengthMenu = list(
        c(10, 25, 50, -1),
        c("10 rows", "25 rows", "50 rows", "Show all")
      ),
      columnDefs = columnDefs
    ),
    class = "cell-border compact stripe"
  ) %>%
    DT::formatStyle(
      names(df),
      backgroundColor = "white",
      textAlign = "center",
      fontSize = size
    )

  ## left aligning

  if (!is.null(left)) {
    tab <- tab %>%
      DT::formatStyle(
        names(df)[left],
        backgroundColor = "white",
        textAlign = "left",
        fontSize = size
      )
  }

  # right aligning
  if (!is.null(right)) {
    tab <- tab %>%
      DT::formatStyle(
        names(df)[right],
        backgroundColor = "white",
        textAlign = "right",
        fontSize = size
      )
  }

  # numeric round
  if (!is.null(numeric)) {
    tab <- tab %>%
      formatRound(names(df)[c(numeric)], digits = round)
  }

  tab
}


### FUNCTIONS FOR EXPORTING PLOTS ----

plot2png <- function(p, filename, zoom = 2, type = "vis") {
  html_name <- tempfile(fileext = ".html")
  switch(
    type,
    vis = {
      visSave(p, html_name)
    },
    plotly = {
      htmlwidgets::saveWidget(p, file = html_name)
    }
  )

  tallShot(html_name, zoom = zoom, file = filename)
}

## freqGgplot ----
## ggplot for frequency plots to download

freqGgplot <- function(df, x = 2, y = 1, n = 20, title = "NOUN Frequency") {
  df <- df %>%
    dplyr::slice_head(n = n) %>%
    data.frame()
  g <- ggplot(df, aes(x = df[, x], y = df[, y], label = df[, x])) +
    geom_col(color = "#c3d1be", fill = "#96af8e") +
    geom_text(
      aes(label = df[, x]),
      position = position_dodge(width = 0.9),
      hjust = -0.4,
      color = "#4f7942",
      size = 3.7
    ) +
    labs(title = title, y = "", x = "Frequency") +
    scale_y_discrete(limits = rev(df[, y])) +
    scale_x_continuous(
      limits = c(0, df[, x] + max(df[, x]) * 0.06),
      expand = c(0, 0)
    ) +
    theme(
      axis.text.y = element_text(angle = 0, hjust = 0, size = 9),
      axis.text.x = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    )
  return(g)
}

topicGplot <- function(x, nPlot = 10, type = "beta") {
  # Identify ID column based on type
  id_col <- if (type == "beta") "word" else "doc"
  topic_names <- setdiff(colnames(x), id_col)

  # Reshape and filter top n per topic
  long_data <- x %>%
    pivot_longer(
      cols = all_of(topic_names),
      names_to = "topic",
      values_to = "probability"
    ) %>%
    group_by(topic) %>%
    slice_max(order_by = probability, n = nPlot) %>%
    arrange(desc(probability), .by_group = TRUE) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic))

  # Reorder factor levels for y-axis
  long_data <- long_data %>%
    group_by(topic) %>%
    mutate(
      label = factor(.data[[id_col]], levels = rev(unique(.data[[id_col]])))
    ) %>%
    ungroup()

  # Select the required number of colors
  unique_topics <- unique(long_data$topic)
  colors <- colorlist()[seq_along(unique_topics)]

  # Build the plot
  g <- ggplot(long_data, aes(x = probability, y = label, fill = topic)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free") +
    scale_fill_manual(values = setNames(colors, unique_topics)) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y = element_text(angle = 0, hjust = 0),
      panel.grid.major.y = element_blank()
    ) +
    labs(y = ifelse(type == "beta", "Word", "Document"), x = "Probability")

  return(g)
}

### deleteCache ------
deleteCache <- function() {
  home <- homeFolder()

  # setting up the main directory
  path_tall <- file.path(home, "tall")
  result <- unlink(path_tall, recursive = TRUE)
  btn_labels <- "OK"

  if (result == 0) {
    subtitle <- paste0(
      "The folder '",
      path_tall,
      "' \nand files contained in it have been correctly deleted"
    )
    title <- ""
    btn_colors <- "#1d8fe1"
    showButton <- TRUE
    timer <- 3000
    type <- "success"
  } else {
    subtitle <- paste0(
      "The folder '",
      path_tall,
      "' \ndoes not exist or you don't have permissions to delete it"
    )
    title <- ""
    btn_colors <- "#913333"
    showButton <- TRUE
    timer <- 3000
    type <- "error"
  }
  show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = btn_colors,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}

## RESET MODAL DIALOG INPUTS
resetModalButtons <- function(session) {
  session$sendCustomMessage("button_id", "null")
  session$sendCustomMessage("click", "null")
  session$sendCustomMessage("click-dend", "null")
  runjs("Shiny.setInputValue('plotly_click-A', null);")
  return(session)
}

#### language models repo ----

langrepo <- function() {
  language_name <- c(
    "afrikaans",
    "ancient_greek",
    "ancient_greek",
    "arabic",
    "armenian",
    "basque",
    "belarusian",
    "bulgarian",
    "catalan",
    "chinese",
    "chinese",
    "classical_chinese",
    "coptic",
    "croatian",
    "czech",
    "czech",
    "czech",
    "czech",
    "danish",
    "dutch",
    "dutch",
    "english",
    "english",
    "english",
    "english",
    "estonian",
    "estonian",
    "finnish",
    "finnish",
    "french",
    "french",
    "french",
    "galician",
    "galician",
    "german",
    "german",
    "gothic",
    "greek",
    "hebrew",
    "hindi",
    "hungarian",
    "indonesian",
    "irish",
    "italian",
    "italian",
    "italian",
    "italian",
    "italian",
    "japanese",
    "korean",
    "korean",
    "latin",
    "latin",
    "latin",
    "latvian",
    "lithuanian",
    "lithuanian",
    "maltese",
    "marathi",
    "norwegian",
    "norwegian",
    "old_church_slavonic",
    "persian",
    "polish",
    "polish",
    "portuguese",
    "portuguese",
    "romanian",
    "romanian",
    "russian",
    "russian",
    "russian",
    "scottish_gaelic",
    "serbian",
    "slovak",
    "slovenian",
    "slovenian",
    "spanish",
    "spanish",
    "swedish",
    "swedish",
    "tamil",
    "telugu",
    "turkish",
    "ukrainian",
    "urdu",
    "uyghur",
    "vietnamese",
    "wolof"
  )

  treebank <- c(
    "AfriBooms",
    "Perseus",
    "PROIEL",
    "PADT",
    "ArmTDP",
    "BDT",
    "HSE",
    "BTB",
    "AnCora",
    "GSD",
    "GSDSimp",
    "Kyoto",
    "Scriptorium",
    "SET",
    "CAC",
    "CLTT",
    "FicTree",
    "PDT",
    "DDT",
    "Alpino",
    "LassySmall",
    "EWT",
    "GUM",
    "LinES",
    "ParTUT",
    "EDT",
    "EWT",
    "FTB",
    "TDT",
    "GSD",
    "ParTUT",
    "Sequoia",
    "CTG",
    "TreeGal",
    "GSD",
    "HDT",
    "PROIEL",
    "GDT",
    "HTB",
    "HDTB",
    "Szeged",
    "GSD",
    "IDT",
    "ISDT",
    "ParTUT",
    "PoSTWITA",
    "TWITTIRO",
    "VIT",
    "GSD",
    "GSD",
    "Kaist",
    "ITTB",
    "Perseus",
    "PROIEL",
    "LVTB",
    "ALKSNIS",
    "HSE",
    "MUDT",
    "UFAL",
    "Bokmaal",
    "Nynorsk",
    "PROIEL",
    "Seraji",
    "LFG",
    "PDB",
    "Bosque",
    "GSD",
    "Nonstandard",
    "RRT",
    "GSD",
    "SynTagRus",
    "Taiga",
    "ARCOSG",
    "SET",
    "SNK",
    "SSJ",
    "SST",
    "AnCora",
    "GSD",
    "LinES",
    "Talbanken",
    "TTB",
    "MTG",
    "IMST",
    "IU",
    "UDTB",
    "UDT",
    "VTB",
    "WTB"
  )

  description <- c(
    "UD Afrikaans-AfriBooms is a conversion of the AfriBooms Dependency Treebank, originally annotated with a simplified PoS set and dependency relations according to a subset of the Stanford tag set. The corpus consists of public government documents.",
    "This Universal Dependencies Ancient Greek Treebank consists of an automatic conversion of a selection of passages from the Ancient Greek and Latin Dependency Treebank 2.1.",
    "UD_Ancient_Greek-PROIEL is converted from the Ancient Greek data in the PROIEL treebank, and consists of the New Testament plus selections from Herodotus.",
    "The Arabic-PADT UD treebank is based on the",
    "A Universal Dependencies treebank for Eastern Armenian developed for UD originally by the ArmTDP team led by Marat M. Yavrumyan at the Yerevan State University.",
    "The Basque UD treebank is based on a automatic conversion from part of the Basque Dependency Treebank (BDT), created at the University of of the Basque Country by the IXA NLP research group. The treebank consists of 8.993 sentences (121.443 tokens) and covers mainly literary and journalistic texts.",
    "The Belarusian UD treebank is based on a sample of the news texts included in the Belarusian-Russian parallel subcorpus of the Russian National Corpus,",
    "UD_Bulgarian-BTB is based on the HPSG-based BulTreeBank.",
    "Catalan data from the [AnCora](http://clic.ub.edu/corpus/) corpus.",
    "Traditional Chinese Universal Dependencies Treebank annotated and converted by Google.",
    "Simplified Chinese Universal Dependencies dataset converted from the GSD (traditional) dataset with manual corrections.",
    "Classical Chinese Universal Dependencies Treebank annotated and converted by Institute for Research in Humanities, Kyoto University.",
    "UD Coptic contains manually annotated Sahidic Coptic texts, including Biblical texts, sermons, letters, and hagiography.",
    "The Croatian UD treebank is based on the extension of the SETimes-HR corpus, the [hr500k](http://hdl.handle.net/11356/1183) corpus.",
    "The UD_Czech-CAC treebank is based on the Czech Academic Corpus 2.0 (CAC) created at Charles University in Prague.",
    "The UD_Czech-CLTT treebank is based on the Czech Legal Text Treebank 2.0,",
    "FicTree is a treebank of Czech fiction, automatically converted into the UD format. The treebank was built at Charles University in Prague.",
    "The Czech-PDT UD treebank is based on the Prague Dependency Treebank – Consolidated 1.0, created at the Charles University in Prague.",
    "The Danish UD treebank is a conversion of the Danish Dependency Treebank.",
    "This corpus consists of samples from various treebanks annotated at the University of Groningen using the Alpino annotation tools and guidelines.",
    "This corpus contains sentences from the Wikipedia section of the Lassy Small Treebank.",
    "A Gold Standard Universal Dependencies Corpus for English, built over the source material of the English Web Treebank LDC2012T13 (https://catalog.ldc.upenn.edu/LDC2012T13).",
    "Universal Dependencies syntax annotations from the GUM corpus (https://gucorpling.org/gum/)",
    "UD English_LinES is the English half of the LinES Parallel Treebank with the original dependency annotation first automatically converted into Universal Dependencies and then partially reviewed. Its contents cover literature, an online manual and Europarl data.",
    "UD_English-ParTUT is a conversion of a multilingual parallel treebank developed at the University of Turin, and consisting of a variety of text genres, including talks, legal texts and Wikipedia articles, among others.",
    "UD Estonian is a converted version of the Estonian Dependency Treebank (EDT), originally annotated in the Constraint Grammar (CG) annotation scheme, and consisting of genres of fiction, newspaper texts and scientific texts. The treebank contains 30,972 trees, 437,769 tokens.",
    "UD EWT treebank consists of different genres of new media. The treebank contains 7,190 trees, 90,585 tokens.",
    "FinnTreeBank 1 consists of manually annotated grammatical examples from VISK. The UD version of FinnTreeBank 1 was converted from a native annotation model with a script and later manually revised.",
    "UD_Finnish-TDT is based on the Turku Dependency Treebank (TDT), a broad-coverage dependency treebank of general Finnish covering numerous genres. The conversion to UD was followed by extensive manual checks and corrections, and the treebank closely adheres to the UD guidelines.",
    "The UD_French-GSD was converted in 2015 from the content head version of the universal dependency treebank v2.0 (https://github.com/ryanmcd/uni-dep-tb). It is updated since 2015 independently from the previous source.",
    "UD_French-ParTUT is a conversion of a multilingual parallel treebank developed at the University of Turin, and consisting of a variety of text genres, including talks, legal texts and Wikipedia articles, among others.",
    "**UD_French-Sequoia** is an automatic conversion of the [SUD_French-Sequoia](https://github.com/surfacesyntacticud/SUD_French-Sequoia) treebank, which comes from the former corpus [French Sequoia corpus](http://deep-sequoia.inria.fr).",
    "The Galician UD treebank is based on the automatic parsing of the Galician Technical Corpus (http://sli.uvigo.gal/CTG) created at the University of Vigo by the the TALG NLP research group.",
    "The Galician-TreeGal is a treebank for Galician developed at LyS Group (Universidade da Coruña) and at CiTIUS (Universidade de Santiago de Compostela).",
    "The German UD is converted from the content head version of the [universal dependency treebank v2.0 (legacy)](https://github.com/ryanmcd/uni-dep-tb).",
    "UD German-HDT is a conversion of the Hamburg Dependency Treebank, created at the University of Hamburg through manual annotation in conjunction with a standard for morphologically and syntactically annotating sentences as well as a constraint-based parser.",
    "The UD Gothic treebank is based on the Gothic data from the PROIEL treebank, and consists of Wulfila's Bible translation.",
    "The Greek UD treebank (UD_Greek-GDT) is derived from the Greek Dependency Treebank.",
    "A Universal Dependencies Corpus for Hebrew.",
    "The Hindi UD treebank is based on the Hindi Dependency Treebank (HDTB), created at IIIT Hyderabad, India.",
    "The Hungarian UD treebank is derived from the Szeged Dependency Treebank (Vincze et al. 2010).",
    "The Indonesian-GSD treebank was originally converted from the content head version of the [universal dependency treebank v2.0 (legacy)](https://github.com/ryanmcd/uni-dep-tb) in 2015. In order to comply with the latest Indonesian annotation guidelines, the treebank has undergone a major revision between UD releases v2.8 and v2.9 (2021).",
    "A Universal Dependencies 4910-sentence treebank for modern Irish.",
    "The Italian corpus annotated according to the UD annotation scheme was obtained by conversion from ISDT (Italian Stanford Dependency Treebank), released for the dependency parsing shared task of Evalita-2014 (Bosco et al. 2014).",
    "UD_Italian-ParTUT is a conversion of a multilingual parallel treebank developed at the University of Turin, and consisting of a variety of text genres, including talks, legal texts and Wikipedia articles, among others.",
    "PoSTWITA-UD is a collection of Italian tweets annotated in Universal Dependencies that can be exploited for the training of NLP systems to enhance their performance on social media texts.",
    "TWITTIRÒ-UD is a collection of ironic Italian tweets annotated in Universal Dependencies. The treebank can be exploited for the training of NLP systems to enhance their performance on social media texts, and in particular, for irony detection purposes.",
    "The UD_Italian-VIT corpus was obtained by conversion from VIT (Venice Italian Treebank), developed at the Laboratory of Computational Linguistics of the Università Ca' Foscari in Venice (Delmonte et al. 2007; Delmonte 2009; http://rondelmo.it/resource/VIT/Browser-VIT/index.htm).",
    "This Universal Dependencies (UD) Japanese treebank is based on the definition of UD Japanese convention described in the UD documentation.  The original sentences are from Google UDT 2.0.",
    "The Google Korean Universal Dependency Treebank is first converted from the [Universal Dependency Treebank v2.0 (legacy)](https://github.com/ryanmcd/uni-dep-tb), and then enhanced by Chun et al., 2018.",
    "The KAIST Korean Universal Dependency Treebank is generated by Chun et al., 2018 from the constituency trees in the [KAIST Tree-Tagging Corpus](http://semanticweb.kaist.ac.kr/home/index.php/Corpus4).",
    "Latin data from the _Index Thomisticus_ Treebank. Data are taken from the _Index Thomisticus_ corpus by Roberto Busa SJ, which contains the complete work by Thomas Aquinas (1225–1274; Medieval Latin) and by 61 other authors related to Thomas.",
    "This Universal Dependencies Latin Treebank consists of an automatic conversion of a selection of passages from the Ancient Greek and Latin Dependency Treebank 2.1",
    "The Latin PROIEL treebank is based on the Latin data from the PROIEL treebank, and contains most of the Vulgate New Testament translations plus selections from Caesar's Gallic War, Cicero's Letters to Atticus, Palladius' Opus Agriculturae and the first book of Cicero's De officiis.",
    "Latvian UD Treebank is based on Latvian Treebank ([LVTB](http://sintakse.korpuss.lv)), being created at University of Latvia, Institute of Mathematics and Computer Science, [Artificial Intelligence Laboratory](http://ailab.lv).",
    "The Lithuanian dependency treebank ALKSNIS v3.0 (Vytautas Magnus University).",
    "Lithuanian treebank annotated manually (dependencies) using the Morphological Annotator by CCL, Vytautas Magnus University (http://tekstynas.vdu.lt/) and manual disambiguation.",
    "MUDT (Maltese Universal Dependencies Treebank) is a manually annotated treebank of Maltese, a Semitic language of Malta descended from North African Arabic with a significant amount of Italo-Romance influence.",
    "UD Marathi is a manually annotated treebank consisting primarily of stories from Wikisource, and parts of an article on Wikipedia.",
    "The Norwegian UD treebank is based on the Bokmål section of the Norwegian Dependency Treebank (NDT), which is a syntactic treebank of Norwegian. The current version of NDT has been automatically converted to the UD scheme by Ingerid Løyning Dale, Per Erik Solberg and Andre Kåsen at the Norwegian Language Bank at the National Library of Norway. This conversion builds to a large extent on previous conversions by Lilja Øvrelid at the University of Oslo.",
    "The Norwegian UD treebank is based on the Nynorsk section of the Norwegian Dependency Treebank (NDT), which is a syntactic treebank of Norwegian. NDT has been automatically converted to the UD scheme by Lilja Øvrelid at the University of Oslo.",
    "The Old Church Slavonic (OCS) UD treebank is based on canonical Old Church Slavonic data from the PROIEL and TOROT treebanks.",
    "The Persian Universal Dependency Treebank (Seraji) is based on Uppsala Persian Dependency Treebank (UPDT).",
    "The LFG Enhanced UD treebank of Polish is based on a corpus of LFG (Lexical Functional Grammar) syntactic structures generated by an LFG grammar of Polish, POLFIE, and manually disambiguated by human annotators.",
    "The Polish PDB-UD treebank is automatically converted from the Polish Dependency Bank 2.0 (PDB 2.0). Both treebanks were created at the [Institute of Computer Science, Polish Academy of Sciences](https://ipipan.waw.pl/en/) in Warsaw (Poland).",
    "This Universal Dependencies (UD) Portuguese treebank is based on the Constraint Grammar converted version of the Bosque, which is part of the Floresta Sintá(c)tica treebank. It contains both European (CETEMPúblico) and Brazilian (CETENFolha) variants.",
    "The Brazilian Portuguese UD is converted from the [Google Universal Dependency",
    "The Romanian Non-standard UD treebank (called UAIC-RoDia) is based on UAIC-RoDia Treebank. UAIC-RoDia = ISLRN 156-635-615-024-0",
    "The Romanian UD treebank (called RoRefTrees) (Barbu Mititelu et al., 2016) is the reference treebank in UD format for standard Romanian.",
    "Russian Universal Dependencies Treebank annotated and converted by Google.",
    "Russian data from the SynTagRus corpus.",
    "Universal Dependencies treebank is based on data samples extracted from Taiga Corpus and MorphoRuEval-2017 and GramEval-2020 shared tasks collections.",
    "A treebank of Scottish Gaelic based on the [Annotated Reference Corpus Of Scottish Gaelic (ARCOSG)](https://github.com/Gaelic-Algorithmic-Research-Group/ARCOSG).",
    "The Serbian UD treebank is based on the [SETimes-SR](http://hdl.handle.net/11356/1200) corpus and additional news documents from the Serbian web.",
    "The Slovak UD treebank is based on data originally annotated as part of the Slovak National Corpus, following the annotation style of the Prague Dependency Treebank.",
    "The SSJ treebank is the reference UD treebank for Slovenian, consisting of approximately 13,000 sentences and 267,097 tokens from fiction, non-fiction, periodical and Wikipedia texts in standard modern Slovenian. As of UD release 2.10 in May 2022, the original version of the SSJ UD treebank has been partially manually revised and extended with new manually annotated data.",
    "The Spoken Slovenian Treebank (SST) is a manually annotated collection of transcribed audio recordings featuring spontaneous speech in various everyday situations. It includes 344 unique speech events (documents) amounting to approximately 10 hours of speech, encompassing a total of 6,108 utterances and 98,396 tokens.",
    "Spanish data from the [AnCora](http://clic.ub.edu/corpus/) corpus.",
    "The Spanish UD is converted from the content head version of the [universal dependency treebank v2.0 (legacy)](https://github.com/ryanmcd/uni-dep-tb).",
    "UD Swedish_LinES is the Swedish half of the LinES Parallel Treebank with UD annotations. All segments are translations from English and the sources cover literary genres, online manuals and Europarl data.",
    "The Swedish-Talbanken treebank is based on Talbanken, a treebank developed at Lund University in the 1970s.",
    "The UD Tamil treebank is based on the Tamil Dependency Treebank created at the Charles University in Prague by Loganathan Ramasamy.",
    "The Telugu UD treebank is created in UD based on manual annotations of sentences from a grammar book.",
    "The UD Turkish Treebank, also called the IMST-UD Treebank, is a semi-automatic conversion of the IMST Treebank (Sulubacak&Eryiğit, 2018; Sulubacak et al., 2016).",
    "Gold standard Universal Dependencies corpus for Ukrainian, developed for UD originally, by [Institute for Ukrainian](https://mova.institute), NGO.   [[українською](https://mova.institute/золотий_стандарт)]",
    "The Urdu Universal Dependency Treebank was automatically converted from Urdu Dependency Treebank (UDTB) which is part of an ongoing effort of creating multi-layered treebanks for Hindi and Urdu.",
    "The Uyghur UD treebank is based on the Uyghur Dependency Treebank (UDT), created at the Xinjiang University, China.",
    "The Vietnamese UD treebank is a conversion of the constituent treebank created in the VLSP project (https://vlsp.hpda.vn/).",
    "UD_Wolof-WTB is a natively manual developed treebank for Wolof. Sentences were collected from encyclopedic, fictional, biographical, religious texts and news."
  )

  contributors <- c(
    "Peter Dirix, Liesbeth Augustinus, Daniel van Niekerk",
    "Giuseppe G. A. Celano, Daniel Zeman",
    "Dag Haug",
    "Daniel Zeman, Zdeněk Žabokrtský, Shadi Saleh",
    "Marat M. Yavrumyan",
    "Maria Jesus Aranzabe, Aitziber Atutxa, Kepa Bengoetxea, Arantza Diaz de Ilarraza, Iakes Goenaga, Koldo Gojenola, Larraitz Uria",
    "Olga Lyashevskaya, Angelika Peljak-Łapińska, Daria Petrova, Yana Shishkina",
    "Kiril Simov, Petya Osenova, Martin Popel",
    "Héctor Martínez Alonso, Elena Pascual, Daniel Zeman",
    "Mo Shen, Ryan McDonald, Daniel Zeman, Peng Qi",
    "Peng Qi, Koichi Yasuoka",
    "Koichi Yasuoka, Christian Wittern, Tomohiko Morioka, Takumi Ikeda, Naoki Yamazaki, Yoshihiro Nikaido, Shingo Suzuki, Shigeki Moro, Yuan Li, Hiroyuki Shirasu, Kazunori Fujita",
    "Mitchell Abrams, Elizabeth Davidson, Amir Zeldes",
    "Željko Agić, Nikola Ljubešić, Daniel Zeman",
    "Barbora Hladká, Daniel Zeman",
    "Barbora Hladká, Daniel Zeman, Martin Popel",
    "Tomáš Jelínek, Daniel Zeman",
    "Daniel Zeman, Jan Hajič",
    "Anders Johannsen, Héctor Martínez Alonso, Barbara Plank",
    "Daniel Zeman, Zdeněk Žabokrtský, Gosse Bouma, Gertjan van Noord",
    "Gosse Bouma, Gertjan van Noord",
    "Natalia Silveira, Timothy Dozat, Christopher Manning, Sebastian Schuster, Ethan Chi, John Bauer, Miriam Connor, Marie-Catherine de Marneffe, Nathan Schneider, Sam Bowman, Hanzhi Zhu, Daniel Galbraith, John Bauer",
    "Siyao Peng, Amir Zeldes",
    "Lars Ahrenberg",
    "Cristina Bosco, Manuela Sanguinetti",
    "Kadri Muischnek, Kaili Müürisep, Tiina Puolakainen, Andriela Rääbis, Liisi Torga",
    "Kadri Muischnek, Kaili Müürisep, Tiina Puolakainen, Dage Särg, Sandra Eiche, Andriela Rääbis",
    "Jussi Piitulainen, Hanna Nurmi, Jack Rueter",
    "Filip Ginter, Jenna Kanerva, Veronika Laippala, Niko Miekka, Anna Missilä, Stina Ojala, Sampo Pyysalo",
    "Marie-Catherine de Marneffe, Bruno Guillaume, Ryan McDonald, Alane Suhr, Joakim Nivre, Matias Grioni, Carly Dickerson, Guy Perrier",
    "Cristina Bosco, Manuela Sanguinetti",
    "Marie Candito, Djamé Seddah, Guy Perrier, Bruno Guillaume",
    "Xavier Gómez Guinovart",
    "Marcos Garcia, Xulia Sánchez-Rodríguez",
    "Slav Petrov, Wolfgang Seeker, Ryan McDonald, Joakim Nivre, Daniel Zeman, Adriane Boyd",
    "Emanuel Borges Völker, Felix Hennig, Arne Köhn, Maximilan Wendt, Verena Blaschke, Nina Böbel, Leonie Weissweiler",
    "Dag Haug",
    "Prokopis Prokopidis",
    "Yoav Goldberg, Reut Tsarfaty, Amir More, Shoval Sadde, Victoria Basmov, Yuval Pinter",
    "Riyaz Ahmad Bhat, Daniel Zeman",
    "Richárd Farkas, Katalin Simkó, Zsolt Szántó, Viktor Varga, Veronika Vincze",
    "Ryan McDonald, Joakim Nivre, Daniel Zeman, Septina Dian Larasati, Ika Alfina",
    "Teresa Lynn, Jennifer Foster, Sarah McGuinness, Abigail Walsh, Jason Phelan, Kevin Scannell",
    "Cristina Bosco, Alessandro Lenci, Simonetta Montemagni, Maria Simi",
    "Cristina Bosco, Manuela Sanguinetti",
    "Cristina Bosco, Manuela Sanguinetti",
    "Alessandra T. Cignarella, Cristina Bosco, Manuela Sanguinetti",
    "Fabio Tamburini, Maria Simi, Cristina Bosco",
    "Mai Omura, Yusuke Miyao, Hiroshi Kanayama, Hiroshi Matsuda, Aya Wakasa, Kayo Yamashita, Masayuki Asahara, Takaaki Tanaka, Yugo Murawaki, Yuji Matsumoto, Shinsuke Mori, Sumire Uematsu, Ryan McDonald, Joakim Nivre, Daniel Zeman",
    "Ryan McDonald, Joakim Nivre, Daniel Zeman, Jinho Choi, Na-Rae Han, Jena Hwang, Jayeol Chun",
    "Jinho Choi, Na-Rae Han, Jena Hwang, Jayeol Chun",
    "Marco Passarotti, Marinella Testori, Daniel Zeman, Berta González Saavedra, Flavio Massimiliano Cecchini",
    "Giuseppe G. A. Celano, Daniel Zeman, Federica Gamba",
    "Dag Haug",
    "Lauma Pretkalniņa, Laura Rituma, Gunta Nešpore-Bērzkalne, Baiba Saulīte, Artūrs Znotiņš, Normunds Grūzītis",
    "Andrius Utka, Erika Rimkutė, Agnė Bielinskienė, Jolanta Kovalevskaitė, Loïc Boizou, Gabrielė Aleksandravičiūtė, Kristina Brokaitė, Daniel Zeman, Natalia Perkova, Bernadeta Griciūtė",
    "Olga Lyashevskaya, Dmitri Sitchinava",
    "Slavomír Čéplö, Daniel Zeman",
    "Vinit Ravishankar",
    "Lilja Øvrelid, Fredrik Jørgensen, Petter Hohle, Ingerid Løyning Dale, Per Erik Solberg, Andre Kåsen",
    "Lilja Øvrelid, Fredrik Jørgensen, Petter Hohle, Ingerid Løyning Dale, Per Erik Solberg, Andre Kåsen",
    "Dag Haug",
    "Mojgan Seraji, Filip Ginter, Joakim Nivre, Martin Popel, Daniel Zeman",
    "Agnieszka Patejuk, Adam Przepiórkowski",
    "Alina Wróblewska, Daniel Zeman, Jan Mašek, Rudolf Rosa",
    "Alexandre Rademaker, Cláudia Freitas, Elvis de Souza, Aline Silveira, Tatiana Cavalcanti, Wograine Evelyn, Luisa Rocha, Isabela Soares-Bastos, Eckhard Bick, Fabricio Chalub, Guilherme Paulino-Passos, Livy Real, Valeria de Paiva, Daniel Zeman, Martin Popel, David Mareček, Natalia Silveira, André Martins",
    "Alexandre Rademaker, Ryan McDonald, Joakim Nivre, Daniel Zeman, Fabricio Chalub, Carlos Ramisch, Juan Belieni, Vanessa Berwanger Wille, Rodrigo Pintucci",
    "Cătălina Mărănduc, Cenel-Augusto Perez, Victoria Bobicev, Cătălin Mititelu, Florinel Hociung, Valentin Roșca, Roman Untilov, Petru Rebeja",
    "Verginica Barbu Mititelu, Elena Irimia, Cenel-Augusto Perez, Radu Ion, Radu Simionescu, Martin Popel",
    "Ryan McDonald, Vitaly Nikolaev, Olga Lyashevskaya",
    "Kira Droganova, Olga Lyashevskaya, Daniel Zeman",
    "Olga Lyashevskaya, Olga Rudina, Natalia Vlasova, Anna Zhuravleva",
    "Colin Batchelor",
    "Tanja Samardžić, Nikola Ljubešić",
    "Katarína Gajdošová, Mária Šimková, Daniel Zeman",
    "Kaja Dobrovoljc, Tomaž Erjavec, Simon Krek",
    "Kaja Dobrovoljc, Joakim Nivre",
    "Héctor Martínez Alonso, Daniel Zeman",
    "Miguel Ballesteros, Héctor Martínez Alonso, Ryan McDonald, Elena Pascual, Natalia Silveira, Daniel Zeman, Joakim Nivre",
    "Lars Ahrenberg",
    "Joakim Nivre, Aaron Smith, Victor Norrman",
    "Loganathan Ramasamy, Daniel Zeman",
    "Taraka Rama, Sowmya Vajjala",
    "Utku Türk, Şaziye Betül Özateş, Büşra Marşan, Salih Furkan Akkurt, Çağrı Çöltekin, Gülşen Cebiroğlu Eryiğit, Memduh Gökırmak, Hüner Kaşıkara, Umut Sulubacak, Francis Tyers",
    "Natalia Kotsyba, Bohdan Moskalevskyi, Mykhailo Romanenko",
    "Riyaz Ahmad Bhat, Daniel Zeman",
    "Marhaba Eli, Daniel Zeman, Francis Tyers",
    "Lương Nguyễn Thị, Linh Hà Mỹ, Phương Lê Hồng, Huyền Nguyễn Thị Minh",
    "Bamba Dione"
  )

  file <- c(
    "af_afribooms",
    "grc_perseus",
    "grc_proiel",
    "ar_padt",
    "hy_armtdp",
    "eu_bdt",
    "be_hse",
    "bg_btb",
    "ca_ancora",
    "zh_gsd",
    "zh_gsdsimp",
    "lzh_kyoto",
    "cop_scriptorium",
    "hr_set",
    "cs_cac",
    "cs_cltt",
    "cs_fictree",
    "cs_pdt",
    "da_ddt",
    "nl_alpino",
    "nl_lassysmall",
    "en_ewt",
    "en_gum",
    "en_lines",
    "en_partut",
    "et_edt",
    "et_ewt",
    "fi_ftb",
    "fi_tdt",
    "fr_gsd",
    "fr_partut",
    "fr_sequoia",
    "gl_ctg",
    "gl_treegal",
    "de_gsd",
    "de_hdt",
    "got_proiel",
    "el_gdt",
    "he_htb",
    "hi_hdtb",
    "hu_szeged",
    "id_gsd",
    "ga_idt",
    "it_isdt",
    "it_partut",
    "it_postwita",
    "it_twittiro",
    "it_vit",
    "ja_gsd",
    "ko_gsd",
    "ko_kaist",
    "la_ittb",
    "la_perseus",
    "la_proiel",
    "lv_lvtb",
    "lt_alksnis",
    "lt_hse",
    "mt_mudt",
    "mr_ufal",
    "no_bokmaal",
    "no_nynorsk",
    "cu_proiel",
    "fa_seraji",
    "pl_lfg",
    "pl_pdb",
    "pt_bosque",
    "pt_gsd",
    "ro_nonstandard",
    "ro_rrt",
    "ru_gsd",
    "ru_syntagrus",
    "ru_taiga",
    "gd_arcosg",
    "sr_set",
    "sk_snk",
    "sl_ssj",
    "sl_sst",
    "es_ancora",
    "es_gsd",
    "sv_lines",
    "sv_talbanken",
    "ta_ttb",
    "te_mtg",
    "tr_imst",
    "uk_iu",
    "ur_udtb",
    "ug_udt",
    "vi_vtb",
    "wo_wtb"
  )

  hub_link <- paste0(
    "https://universaldependencies.org/treebanks/",
    file,
    "/index.html"
  )

  tokens <- c(
    "22369",
    "202989",
    "214005",
    "93712",
    "52220",
    "121443",
    "305417",
    "156149",
    "537767",
    "123291",
    "123291",
    "433168",
    "26837",
    "2247925",
    "494142",
    "36007",
    "166602",
    "1526323",
    "506231",
    "208745",
    "297486",
    "251536",
    "208446",
    "93200",
    "49601",
    "438312",
    "50473",
    "159314",
    "202209",
    "389367",
    "27638",
    "68593",
    "126011",
    "23479",
    "287721",
    "3399390",
    "87266",
    "61773",
    "114648",
    "351704",
    "1182501",
    "119830",
    "115990",
    "278461",
    "51614",
    "119334",
    "28384",
    "259625",
    "193654",
    "80322",
    "350090",
    "450480",
    "28868",
    "328476",
    "328305",
    "70049",
    "5356",
    "44162",
    "13089",
    "310221",
    "301353",
    "553686",
    "151625",
    "130967",
    "347319",
    "210958",
    "296169",
    "1896343",
    "218522",
    "97994",
    "1517881",
    "197001",
    "86089",
    "97673",
    "365490",
    "267097",
    "98393",
    "555670",
    "423346",
    "90961",
    "96859",
    "8635",
    "6465",
    "56422",
    "122934",
    "138077",
    "40236",
    "58069",
    "42832"
  )

  words <- c(
    "25017",
    "202989",
    "214005",
    "94390",
    "52585",
    "121443",
    "305417",
    "156149",
    "553638",
    "123291",
    "123291",
    "433168",
    "57098",
    "2252453",
    "495219",
    "36023",
    "167226",
    "1529074",
    "506232",
    "208746",
    "297486",
    "254865",
    "212035",
    "94217",
    "49632",
    "438313",
    "50486",
    "159612",
    "202453",
    "400391",
    "28576",
    "70545",
    "138837",
    "25548",
    "292769",
    "3455580",
    "88934",
    "63441",
    "160195",
    "351704",
    "1183884",
    "122019",
    "115990",
    "298375",
    "55558",
    "124515",
    "29602",
    "280153",
    "193654",
    "80322",
    "350090",
    "450517",
    "29221",
    "328476",
    "328305",
    "70049",
    "5356",
    "44162",
    "13089",
    "310221",
    "301353",
    "553686",
    "152920",
    "130967",
    "349978",
    "227827",
    "318666",
    "1896343",
    "218522",
    "97994",
    "1517881",
    "197001",
    "89958",
    "97673",
    "365490",
    "267097",
    "98393",
    "568249",
    "431600",
    "90961",
    "96859",
    "9581",
    "6465",
    "58096",
    "122983",
    "138077",
    "40236",
    "58069",
    "44258"
  )

  sentences <- c(
    "1975",
    "13919",
    "17082",
    "4800",
    "2500",
    "8993",
    "25231",
    "11138",
    "16678",
    "4997",
    "4997",
    "86239",
    "2203",
    "127794",
    "24709",
    "1121",
    "12760",
    "87907",
    "30723",
    "13603",
    "17120",
    "16622",
    "12146",
    "5243",
    "2090",
    "30930",
    "2829",
    "18723",
    "15136",
    "16342",
    "1020",
    "3099",
    "3993",
    "1000",
    "15590",
    "189928",
    "4328",
    "2521",
    "6143",
    "16649",
    "53564",
    "5598",
    "4910",
    "14167",
    "2090",
    "6712",
    "1424",
    "10087",
    "8100",
    "6339",
    "27363",
    "26977",
    "2273",
    "19387",
    "19367",
    "3642",
    "263",
    "2074",
    "1144",
    "20044",
    "17575",
    "40085",
    "5997",
    "17246",
    "22152",
    "9357",
    "12020",
    "116324",
    "9524",
    "5030",
    "87336",
    "17872",
    "4741",
    "4384",
    "19543",
    "13435",
    "6108",
    "17662",
    "16014",
    "5243",
    "6026",
    "600",
    "1328",
    "5635",
    "7092",
    "5130",
    "3456",
    "3323",
    "2107"
  )

  folder <- paste0("UD_", language_name, "-", treebank) # Generazione dinamica

  chapter <- c(
    "Hoofstuk", # Afrikaans
    "κεφάλαιο", # Ancient Greek
    "κεφάλαιο", # Ancient Greek (duplicated)
    "فصل", # Arabic
    "Գլուխ", # Armenian
    "Kapitulua", # Basque
    "Глава", # Belarusian
    "Глава", # Bulgarian
    "Capítol", # Catalan
    "章节", # Chinese (Simplified)
    "章", # Chinese (Traditional)
    "章节", # Classical Chinese
    "ⲕⲁⲡⲓⲧⲓⲗⲟⲛ", # Coptic
    "Poglavlje", # Croatian
    "Kapitola", # Czech
    "Kapitola", # Czech (duplicated)
    "Kapitola", # Czech (duplicated)
    "Kapitola", # Czech (duplicated)
    "Kapitel", # Danish
    "Hoofdstuk", # Dutch
    "Hoofdstuk", # Dutch (duplicated)
    "Chapter", # English
    "Chapter", # English (duplicated)
    "Chapter", # English (duplicated)
    "Chapter", # English (duplicated)
    "Peatükk", # Estonian
    "Peatükk", # Estonian (duplicated)
    "Luku", # Finnish
    "Luku", # Finnish (duplicated)
    "Chapitre", # French
    "Chapitre", # French (duplicated)
    "Chapitre", # French (duplicated)
    "Capítulo", # Galician
    "Capítulo", # Galician (duplicated)
    "Kapitel", # German
    "Kapitel", # German (duplicated)
    "𐍉𐌼𐌿𐌽𐌳𐌰𐌽𐌾𐌾𐌰", # Gothic
    "Κεφάλαιο", # Greek
    "פרק", # Hebrew
    "अध्याय", # Hindi
    "Fejezet", # Hungarian
    "Bab", # Indonesian
    "Caibidil", # Irish
    "Capitolo", # Italian
    "Capitolo", # Italian (duplicated)
    "Capitolo", # Italian (duplicated)
    "Capitolo", # Italian (duplicated)
    "Capitolo", # Italian (duplicated)
    "章", # Japanese
    "장", # Korean
    "장", # Korean (duplicated)
    "Capitulum", # Latin
    "Capitulum", # Latin (duplicated)
    "Capitulum", # Latin (duplicated)
    "Nodaļa", # Latvian
    "Skyrius", # Lithuanian
    "Skyrius", # Lithuanian (duplicated)
    "Kapitel", # Maltese
    "अध्याय", # Marathi
    "Kapittel", # Norwegian
    "Kapittel", # Norwegian (duplicated)
    "Глава", # Old Church Slavonic
    "فصل", # Persian
    "Rozdział", # Polish
    "Rozdział", # Polish (duplicated)
    "Capítulo", # Portuguese
    "Capítulo", # Portuguese (duplicated)
    "Capitol", # Romanian
    "Capitol", # Romanian (duplicated)
    "Глава", # Russian
    "Глава", # Russian (duplicated)
    "Глава", # Russian (duplicated)
    "Caibideil", # Scottish Gaelic
    "Глава", # Serbian
    "Kapitola", # Slovak
    "Poglavje", # Slovenian
    "Poglavje", # Slovenian (duplicated)
    "Capítulo", # Spanish
    "Capítulo", # Spanish (duplicated)
    "Kapitel", # Swedish
    "Kapitel", # Swedish (duplicated)
    "அத்தியாயம்", # Tamil
    "అధ్యాయం", # Telugu
    "Bölüm", # Turkish
    "Розділ", # Ukrainian
    "باب", # Urdu
    "باب", # Uyghur
    "Chương", # Vietnamese
    "Kàll" # Wolof
  )

  flag <- c(
    "AF.svg",
    "GRC.svg",
    "GRC.svg",
    "AR.svg",
    "HY.svg",
    "EU.svg",
    "BE.svg",
    "BG.svg",
    "CA.svg",
    "ZH.svg",
    "ZH.svg",
    "LZH.svg",
    "COP.svg",
    "HR.svg",
    "CS.svg",
    "CS.svg",
    "CS.svg",
    "CS.svg",
    "DA.svg",
    "NL.svg",
    "NL.svg",
    "EN.svg",
    "EN.svg",
    "EN.svg",
    "EN.svg",
    "ET.svg",
    "ET.svg",
    "FI.svg",
    "FI.svg",
    "FR.svg",
    "FR.svg",
    "FR.svg",
    "GL.svg",
    "GL.svg",
    "DE.svg",
    "DE.svg",
    "GOT.svg",
    "EL.svg",
    "HE.svg",
    "HI.svg",
    "HU.svg",
    "ID.svg",
    "GA.svg",
    "IT.svg",
    "IT.svg",
    "IT.svg",
    "IT.svg",
    "IT.svg",
    "JA.svg",
    "KO.svg",
    "KO.svg",
    "LA.svg",
    "LA.svg",
    "LA.svg",
    "LV.svg",
    "LT.svg",
    "LT.svg",
    "MT.svg",
    "MR.svg",
    "NO.svg",
    "NO.svg",
    "CU.svg",
    "FA.svg",
    "PL.svg",
    "PL.svg",
    "PT.svg",
    "PT.svg",
    "RO.svg",
    "RO.svg",
    "RU.svg",
    "RU.svg",
    "RU.svg",
    "GD.svg",
    "SR.svg",
    "SK.svg",
    "SL.svg",
    "SL.svg",
    "ES.svg",
    "ES.svg",
    "SV.svg",
    "SV.svg",
    "TA.svg",
    "TE.svg",
    "TR.svg",
    "UK.svg",
    "UR.svg",
    "UG.svg",
    "VI.svg",
    "WO.svg"
  )

  ranking <- c(
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    29,
    19,
    20,
    30,
    31,
    32,
    33,
    34,
    35,
    36,
    37,
    38,
    39,
    2,
    1,
    3,
    4,
    40,
    41,
    42,
    43,
    10,
    11,
    12,
    44,
    45,
    16,
    15,
    46,
    47,
    48,
    49,
    50,
    51,
    52,
    5,
    6,
    7,
    8,
    9,
    53,
    54,
    55,
    56,
    57,
    58,
    59,
    60,
    61,
    62,
    63,
    64,
    65,
    66,
    67,
    68,
    69,
    17,
    18,
    70,
    71,
    72,
    73,
    74,
    75,
    76,
    77,
    78,
    79,
    13,
    14,
    80,
    81,
    82,
    83,
    84,
    85,
    86,
    87,
    88,
    89
  )

  df <- data.frame(
    language_name = language_name,
    treebank = treebank,
    contributors = contributors,
    description = description,
    file = file,
    folder = folder,
    hub_page_link = hub_link,
    flag = flag,
    tokens = tokens,
    words = words,
    sentences = sentences,
    chapter = toupper(chapter),
    index = ranking
  )
  # df <- df[index,]
  df <- df %>%
    arrange(index) %>%
    select(-index) %>%
    filter(!file %in% c("gl_treegal", "la_perseus"))

  return(df)
}

model_accuracy <- function() {
  # Creazione dei vettori stringa per ciascuna colonna
  Language <- c(
    "Afrikaans",
    "Arabic",
    "Belarusian",
    "Bulgarian",
    "Catalan",
    "Coptic",
    "Czech",
    "Czech",
    "Czech",
    "Czech",
    "Old Church Slavonic",
    "Danish",
    "German",
    "Greek",
    "English",
    "English",
    "English",
    "English",
    "Spanish",
    "Spanish",
    "Estonian",
    "Estonian",
    "Basque",
    "Persian",
    "Finnish",
    "Finnish",
    "French",
    "French",
    "French",
    "Irish",
    "Scottish Gaelic",
    "Galician",
    "Gothic",
    "Ancient Greek",
    "Ancient Greek",
    "Hebrew",
    "Hindi",
    "Croatian",
    "Hungarian",
    "Armenian",
    "Indonesian",
    "Italian",
    "Italian",
    "Italian",
    "Italian",
    "Italian",
    "Japanese",
    "Korean",
    "Korean",
    "Latin",
    "Latin",
    "Lithuanian",
    "Lithuanian",
    "Latvian",
    "Classical Chinese",
    "Marathi",
    "Maltese",
    "Dutch",
    "Dutch",
    "Norwegian",
    "Norwegian",
    "Polish",
    "Polish",
    "Portuguese",
    "Portuguese",
    "Romanian",
    "Romanian",
    "Russian",
    "Russian",
    "Russian",
    "Slovak",
    "Slovenian",
    "Slovenian",
    "Serbian",
    "Swedish",
    "Swedish",
    "Tamil",
    "Telugu",
    "Turkish",
    "Uyghur",
    "Ukrainian",
    "Urdu",
    "Vietnamese",
    "Wolof",
    "Chinese",
    "Chinese",
    "German"
  )

  Treebank <- c(
    "AfriBooms",
    "PADT",
    "HSE",
    "BTB",
    "AnCora",
    "Scriptorium",
    "CAC",
    "CLTT",
    "FicTree",
    "PDT",
    "PROIEL",
    "DDT",
    "GSD",
    "GDT",
    "EWT",
    "GUM",
    "LinES",
    "ParTUT",
    "AnCora",
    "GSD",
    "EDT",
    "EWT",
    "BDT",
    "Seraji",
    "FTB",
    "TDT",
    "GSD",
    "ParTUT",
    "Sequoia",
    "IDT",
    "ARCOSG",
    "CTG",
    "PROIEL",
    "Perseus",
    "PROIEL",
    "HTB",
    "HDTB",
    "SET",
    "Szeged",
    "ArmTDP",
    "GSD",
    "ISDT",
    "ParTUT",
    "PoSTWITA",
    "TWITTIRO",
    "VIT",
    "GSD",
    "GSD",
    "Kaist",
    "ITTB",
    "PROIEL",
    "ALKSNIS",
    "HSE",
    "LVTB",
    "Kyoto",
    "UFAL",
    "MUDT",
    "Alpino",
    "LassySmall",
    "Bokmaal",
    "Nynorsk",
    "LFG",
    "PDB",
    "Bosque",
    "GSD",
    "Nonstandard",
    "RRT",
    "GSD",
    "SynTagRus",
    "Taiga",
    "SNK",
    "SSJ",
    "SST",
    "SET",
    "LinES",
    "Talbanken",
    "TTB",
    "MTG",
    "IMST",
    "UDT",
    "IU",
    "UDTB",
    "VTB",
    "WTB",
    "GSD",
    "GSDSimp",
    "HDT"
  )

  Words <- c(
    99.93,
    94.51,
    99.28,
    99.92,
    99.94,
    75.38,
    99.96,
    99.37,
    99.99,
    99.91,
    100.00,
    99.93,
    99.56,
    99.89,
    99.14,
    99.57,
    99.94,
    99.78,
    99.94,
    99.72,
    99.88,
    98.95,
    99.98,
    99.65,
    99.99,
    99.68,
    98.89,
    99.41,
    99.09,
    99.67,
    97.39,
    99.14,
    100.00,
    99.98,
    100.00,
    85.08,
    99.99,
    99.91,
    99.69,
    99.38,
    99.45,
    99.80,
    99.70,
    99.36,
    99.02,
    99.75,
    95.77,
    99.85,
    99.99,
    99.97,
    99.99,
    99.95,
    97.65,
    99.23,
    97.70,
    95.27,
    99.79,
    99.59,
    99.83,
    99.82,
    99.90,
    99.86,
    99.85,
    99.67,
    99.27,
    98.88,
    99.68,
    99.53,
    99.57,
    98.74,
    100.00,
    99.89,
    100.00,
    100.00,
    99.96,
    99.91,
    94.93,
    100.00,
    97.60,
    99.74,
    99.71,
    99.99,
    85.11,
    99.16,
    89.89,
    89.05,
    99.9
  )

  Sentences <- c(
    99.65,
    81.43,
    84.80,
    94.67,
    99.08,
    30.20,
    99.76,
    93.79,
    97.56,
    91.91,
    34.43,
    89.92,
    78.69,
    89.51,
    86.17,
    95.07,
    87.88,
    99.02,
    97.92,
    94.54,
    89.81,
    75.16,
    99.42,
    98.00,
    86.93,
    85.59,
    93.45,
    98.64,
    85.01,
    97.57,
    56.07,
    94.80,
    28.25,
    98.73,
    45.89,
    99.69,
    97.69,
    93.54,
    94.48,
    97.28,
    90.47,
    96.59,
    99.02,
    24.37,
    28.57,
    96.73,
    99.45,
    93.63,
    100.00,
    92.53,
    28.48,
    87.75,
    92.04,
    99.06,
    42.03,
    81.19,
    82.32,
    89.99,
    81.93,
    97.34,
    92.74,
    99.83,
    96.06,
    89.56,
    87.99,
    96.92,
    94.01,
    97.41,
    97.55,
    84.37,
    77.73,
    99.07,
    93.73,
    93.84,
    86.83,
    94.20,
    95.08,
    98.98,
    97.51,
    82.17,
    96.71,
    97.12,
    97.02,
    90.27,
    98.80,
    98.80,
    92.6
  )

  UPOS <- c(
    95.79,
    88.91,
    96.07,
    98.01,
    98.22,
    72.69,
    98.56,
    94.87,
    97.60,
    98.63,
    91.36,
    95.24,
    64.12,
    95.13,
    94.10,
    95.77,
    95.07,
    94.30,
    98.25,
    95.49,
    95.45,
    88.85,
    92.43,
    96.02,
    92.01,
    94.28,
    96.24,
    95.26,
    96.42,
    93.43,
    91.38,
    96.23,
    93.73,
    81.44,
    95.34,
    80.84,
    95.84,
    96.67,
    90.96,
    92.39,
    93.22,
    97.24,
    96.73,
    94.25,
    90.37,
    96.38,
    93.50,
    91.40,
    92.95,
    98.42,
    94.53,
    90.37,
    71.39,
    95.23,
    87.70,
    78.79,
    93.79,
    94.18,
    94.79,
    96.39,
    95.92,
    96.86,
    97.30,
    96.12,
    95.58,
    94.94,
    96.91,
    95.00,
    97.42,
    93.42,
    92.98,
    96.78,
    94.83,
    97.36,
    94.67,
    95.71,
    78.41,
    90.29,
    88.85,
    88.80,
    94.85,
    91.53,
    76.10,
    91.73,
    82.84,
    82.28,
    97.8
  )

  XPOS <- c(
    91.39,
    82.35,
    95.40,
    94.72,
    95.04,
    72.55,
    90.88,
    82.33,
    90.26,
    93.12,
    91.63,
    99.92,
    64.29,
    95.13,
    93.23,
    95.32,
    94.08,
    93.86,
    94.92,
    99.72,
    96.80,
    91.47,
    99.98,
    95.78,
    90.74,
    95.43,
    98.89,
    94.69,
    99.09,
    92.20,
    83.27,
    95.80,
    94.20,
    71.34,
    95.65,
    80.84,
    94.91,
    89.99,
    99.69,
    99.38,
    91.90,
    97.11,
    96.37,
    93.88,
    89.60,
    95.37,
    92.80,
    81.07,
    80.41,
    93.26,
    94.73,
    81.26,
    70.36,
    87.53,
    87.29,
    95.27,
    93.49,
    91.51,
    92.21,
    97.70,
    97.30,
    87.18,
    88.49,
    99.67,
    82.76,
    90.06,
    95.94,
    94.74,
    99.57,
    98.74,
    77.04,
    90.26,
    88.51,
    90.76,
    91.95,
    93.90,
    76.99,
    90.29,
    88.54,
    90.12,
    84.03,
    89.00,
    75.07,
    91.42,
    83.56,
    82.98,
    97.4
  )

  UFeats <- c(
    93.67,
    82.44,
    88.30,
    95.77,
    97.68,
    72.88,
    89.58,
    82.64,
    90.96,
    93.21,
    82.50,
    94.48,
    53.65,
    89.96,
    94.37,
    95.10,
    94.94,
    94.21,
    97.94,
    95.97,
    93.71,
    86.94,
    87.17,
    95.92,
    92.21,
    90.54,
    95.34,
    92.58,
    94.91,
    84.15,
    85.18,
    98.92,
    86.76,
    84.62,
    87.71,
    78.57,
    90.17,
    90.59,
    87.20,
    83.70,
    94.17,
    97.12,
    96.40,
    94.26,
    90.00,
    96.33,
    95.75,
    99.50,
    99.99,
    93.98,
    86.89,
    82.42,
    66.42,
    91.82,
    88.09,
    70.06,
    99.79,
    93.28,
    93.95,
    95.06,
    94.93,
    87.48,
    88.84,
    94.07,
    91.78,
    88.31,
    96.09,
    85.57,
    90.50,
    86.33,
    79.32,
    90.53,
    88.67,
    91.10,
    90.11,
    94.18,
    79.37,
    98.89,
    84.31,
    83.63,
    83.67,
    80.26,
    85.11,
    90.89,
    88.57,
    87.79,
    91.4
  )

  AllTags <- c(
    91.26,
    82.16,
    87.69,
    94.39,
    94.83,
    71.87,
    89.45,
    82.23,
    90.14,
    92.83,
    80.73,
    93.22,
    50.34,
    88.75,
    91.61,
    93.69,
    91.84,
    92.45,
    94.54,
    93.55,
    92.15,
    83.72,
    84.68,
    95.31,
    89.00,
    89.50,
    94.51,
    91.12,
    94.17,
    81.76,
    82.49,
    95.38,
    84.55,
    71.24,
    86.28,
    77.75,
    87.68,
    89.95,
    85.84,
    82.79,
    86.65,
    96.33,
    95.19,
    92.50,
    87.00,
    94.18,
    92.30,
    78.74,
    80.41,
    92.63,
    85.59,
    80.88,
    60.98,
    87.48,
    84.79,
    67.88,
    93.20,
    90.78,
    91.41,
    93.74,
    93.29,
    86.56,
    87.88,
    92.66,
    82.58,
    87.67,
    95.83,
    84.50,
    90.11,
    85.42,
    76.77,
    89.80,
    87.49,
    90.73,
    86.66,
    92.43,
    72.93,
    90.29,
    81.58,
    76.78,
    82.86,
    75.43,
    75.04,
    88.62,
    82.30,
    81.76,
    91.0
  )

  Lemma <- c(
    96.40,
    87.36,
    88.77,
    92.88,
    98.19,
    73.30,
    96.24,
    92.93,
    95.60,
    97.50,
    81.73,
    94.46,
    86.07,
    92.74,
    95.15,
    96.55,
    95.78,
    94.76,
    98.17,
    94.08,
    87.93,
    84.30,
    91.09,
    92.74,
    85.76,
    84.52,
    96.78,
    95.84,
    96.60,
    91.96,
    91.33,
    95.18,
    89.51,
    79.90,
    90.77,
    79.01,
    98.05,
    93.30,
    87.29,
    90.83,
    87.32,
    96.98,
    96.35,
    93.84,
    89.43,
    96.52,
    94.21,
    86.96,
    88.12,
    98.04,
    91.73,
    86.89,
    68.95,
    93.35,
    97.46,
    83.15,
    99.79,
    90.82,
    92.16,
    96.04,
    95.47,
    92.56,
    94.28,
    96.50,
    91.72,
    91.61,
    95.16,
    89.90,
    95.24,
    89.54,
    83.32,
    95.04,
    94.84,
    93.40,
    94.50,
    95.13,
    82.41,
    100.00,
    85.77,
    83.47,
    91.39,
    91.47,
    83.66,
    92.81,
    89.83,
    89.00,
    92.3
  )

  UAS <- c(
    82.55,
    67.64,
    77.08,
    87.98,
    86.97,
    50.32,
    84.81,
    72.06,
    84.95,
    83.49,
    66.17,
    77.57,
    71.43,
    83.36,
    81.03,
    81.52,
    81.11,
    82.15,
    85.23,
    83.40,
    77.08,
    66.96,
    74.57,
    79.98,
    77.89,
    76.79,
    86.39,
    85.33,
    84.69,
    80.52,
    74.56,
    75.72,
    70.88,
    58.70,
    68.94,
    59.74,
    90.44,
    82.08,
    70.59,
    75.17,
    80.88,
    87.46,
    86.26,
    72.70,
    70.77,
    81.76,
    84.20,
    60.94,
    72.96,
    79.31,
    63.36,
    65.80,
    43.34,
    78.23,
    65.90,
    62.55,
    75.31,
    78.21,
    80.11,
    85.36,
    83.85,
    90.64,
    84.74,
    83.83,
    82.18,
    81.26,
    82.69,
    81.58,
    83.19,
    72.77,
    78.84,
    84.17,
    70.58,
    84.90,
    81.31,
    81.51,
    59.66,
    87.93,
    61.37,
    67.06,
    77.54,
    81.25,
    47.83,
    75.18,
    58.96,
    58.27,
    77.0
  )

  LAS <- c(
    77.97,
    62.60,
    73.45,
    83.52,
    83.75,
    48.26,
    81.13,
    67.99,
    80.79,
    80.18,
    59.74,
    73.77,
    65.53,
    79.58,
    77.64,
    78.18,
    76.60,
    78.75,
    81.52,
    79.41,
    73.13,
    60.45,
    69.35,
    75.19,
    72.65,
    72.74,
    83.00,
    82.11,
    81.63,
    72.83,
    68.31,
    72.53,
    63.47,
    51.74,
    63.73,
    56.38,
    85.86,
    77.39,
    64.95,
    67.58,
    74.29,
    84.47,
    83.08,
    67.74,
    64.11,
    77.58,
    82.36,
    51.64,
    61.60,
    75.58,
    57.80,
    59.65,
    31.05,
    73.52,
    60.15,
    51.15,
    68.95,
    73.20,
    75.79,
    82.30,
    80.62,
    86.73,
    80.83,
    79.97,
    78.52,
    75.55,
    77.96,
    76.41,
    79.37,
    67.12,
    74.61,
    80.51,
    65.41,
    80.63,
    76.54,
    77.23,
    49.62,
    75.45,
    52.36,
    53.01,
    72.78,
    74.23,
    40.71,
    69.40,
    55.27,
    54.47,
    81.4
  )

  df <- data.frame(
    language_name = gsub(" ", "_", tolower(Language)),
    treebank = Treebank,
    Words = Words,
    Sentences = Sentences,
    UPOS = UPOS,
    XPOS = XPOS,
    UFeats = UFeats,
    AllTags = AllTags,
    Lemma = Lemma,
    UAS = UAS,
    LAS = LAS,
    stringsAsFactors = FALSE # Per mantenere le colonne di testo come stringhe
  )

  return(df)
}

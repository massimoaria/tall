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
    )
  # %>%
  #   event_register("plotly_selecting")

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
  # nSentences <- x %>%
  #   group_by(doc_id) %>%
  #   reframe(nSent = max(sentence_id)) %>%
  #   ungroup() %>%
  #   select(nSent) %>%
  #   reframe(nSent = sum(nSent)) %>%
  #   as.numeric()
  nSentences <- x %>%
    select(doc_id, sentence) %>%
    distinct() %>%
    nrow()

  # 6. # avg document length
  # avgDocLength <- x %>%
  #   group_by(doc_id) %>%
  #   select(doc_id, sentence) %>%
  #   reframe(
  #     nTokens = n(),
  #     nChars = nchar(paste(sentence, collapse = " "))
  #   ) %>%
  #   ungroup() %>%
  #   reframe(
  #     avgChars = round(mean(nChars), 0),
  #     sdChars = round(sd(nChars), 0),
  #     avgTokens = round(mean(nTokens), 0),
  #     sdTokens = round(sd(nTokens), 0)
  #   )

  avgDocLength <- x %>%
    group_by(doc_id) %>%
    summarise(
      char_length = sum(nchar(token), na.rm = TRUE),
      token_length = n()
    ) %>%
    summarise(
      avgChars = mean(char_length, na.rm = TRUE),
      sdChars = sd(char_length, na.rm = TRUE),
      avgTokens = mean(token_length, na.rm = TRUE),
      sdTokens = sd(token_length, na.rm = TRUE)
    )

  # 7. # avg length sentence
  # avgSentLength <- x %>%
  #   group_by(doc_id, sentence_id) %>%
  #   reframe(
  #     sentLength = n(),
  #     nChars = nchar(sentence)
  #   ) %>%
  #   ungroup() %>%
  #   reframe(
  #     avgTokens = round(mean(sentLength), 1),
  #     sdTokens = round(sd(sentLength), 1),
  #     avgChars = round(mean(nChars), 1),
  #     sdChars = round(sd(nChars), 1)
  #   )
  avgSentLength <- x %>%
    group_by(doc_id, sentence_id) %>%
    summarise(
      char_length = sum(nchar(token), na.rm = TRUE),
      token_length = n(),
      .groups = "drop"
    ) %>%
    summarise(
      avgChars = mean(char_length, na.rm = TRUE),
      sdChars = sd(char_length, na.rm = TRUE),
      avgTokens = mean(token_length, na.rm = TRUE),
      sdTokens = sd(token_length, na.rm = TRUE)
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
    avgDocLengthChars = round(avgDocLength$avgChars, 0),
    avgDocLengthCharsSD = round(avgDocLength$sdChars, 0),
    avgDocLengthTokens = round(avgDocLength$avgTokens, 0),
    avgDocLengthTokensSD = round(avgDocLength$sdTokens, 0),
    avgSentLengthTokens = round(avgSentLength$avgTokens, 0),
    avgSentLengthTokensSD = round(avgSentLength$sdTokens, 0),
    avgSentLengthChars = round(avgSentLength$avgChars, 0),
    avgSentLengthCharsSD = round(avgSentLength$sdChars, 0),
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

# wordcloud2vis <- function(nodes, labelsize = 7, opacity = 1) {
#   nodes <- nodes %>%
#     mutate(id = row_number())
#   # size scaling
#   scalemin <- 20 * (1 + labelsize / 5)
#   scalemax <- 100 * (1 + labelsize / 5)
#   N <- nrow(nodes)
#
#   colorlists <- colorlist()
#   colorlists <- sample(colorlists, N, replace = TRUE)
#
#   opacity.min <- 0.6
#   shape <- "text"
#   layout <- "layout_nicely"
#
#   nodes <- nodes %>%
#     mutate(
#       font.color = colorlists,
#       id = row_number(),
#       shape = shape,
#       color = colorlists,
#       title = paste(
#         "<strong>",
#         label,
#         "</strong>",
#         "<br><h5>freq = ",
#         value,
#         "</h5>",
#         sep = ""
#       )
#     )
#
#   nodes$font.size <- log(nodes$value)
#   Min <- min(nodes$font.size)
#   Max <- max(nodes$font.size)
#   if (Max > Min) {
#     size <- (nodes$font.size - Min) / (Max - Min) * 15 * labelsize + 10
#   } else {
#     size <- 10 * labelsize
#   }
#   size[size < scalemin] <- scalemin
#   size[size > scalemax] <- scalemax
#   nodes$font.size <- size
#
#   if (shape %in% c("dot", "square")) {
#     nodes$font.vadjust <- -0.7 * nodes$font.size
#   } else {
#     nodes$font.vadjust <- 0
#   }
#
#   ## opacity for label
#   opacity_font <- sqrt(
#     (nodes$font.size - min(nodes$font.size)) / diff(range(nodes$font.size))
#   ) *
#     opacity +
#     opacity.min +
#     0.1
#   if (is.nan(opacity_font[1])) {
#     opacity_font <- rep(opacity.min, length(opacity_font))
#   }
#
#   # node colors
#   nodes$opacity.nodes <- (opacity_font - min(opacity_font)) /
#     (diff(range(opacity_font))) *
#     0.5 +
#     opacity.min
#   nodes$opacity.nodes[is.nan(nodes$opacity.nodes)] <- 0.5
#
#   VIS <-
#     visNetwork::visNetwork(
#       nodes = nodes,
#       edges = NULL,
#       type = "full",
#       smooth = TRUE,
#       physics = TRUE
#     ) %>%
#     visNetwork::visNodes(
#       shadow = FALSE,
#       shape = nodes$shape,
#       font = list(
#         color = nodes$font.color,
#         size = nodes$font.size,
#         vadjust = nodes$font.vadjust
#       )
#     ) %>%
#     visNetwork::visOptions(
#       highlightNearest = list(enabled = T, hover = T, degree = 1),
#       nodesIdSelection = T
#     ) %>%
#     visNetwork::visInteraction(
#       dragNodes = TRUE,
#       navigationButtons = F,
#       hideEdgesOnDrag = TRUE,
#       zoomSpeed = 0.2
#     ) %>%
#     visEvents(
#       click = "function(nodes){
#                   Shiny.onInputChange('click', nodes.nodes[0]);
#                   ;}"
#     ) %>%
#     visNetwork::visOptions(
#       manipulation = FALSE,
#       height = "100%",
#       width = "100%"
#     )
#   return(VIS)
# }

## wordcloud function
# wordcloud2a <- function(
#   data,
#   size = 1,
#   minSize = 0,
#   gridSize = 0,
#   fontFamily = "Segoe UI",
#   fontWeight = "bold",
#   color = "random-dark",
#   backgroundColor = "transparent",
#   minRotation = -pi / 4,
#   maxRotation = pi / 4,
#   shuffle = TRUE,
#   rotateRatio = 0.4,
#   shape = "circle",
#   ellipticity = 0.65,
#   widgetsize = NULL,
#   figPath = NULL,
#   hoverFunction = NULL
# ) {
#   if ("table" %in% class(data)) {
#     dataOut <- data.frame(name = names(data), freq = as.vector(data))
#   } else {
#     data <- as.data.frame(data)
#     dataOut <- data[, 1:2]
#     names(dataOut) <- c("name", "freq")
#   }
#   if (!is.null(figPath)) {
#     if (!file.exists(figPath)) {
#       stop("cannot find fig in the figPath")
#     }
#     spPath <- strsplit(figPath, "\\.")[[1]]
#     len <- length(spPath)
#     figClass <- spPath[len]
#     if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
#       stop("file should be a jpeg, jpg, png, bmp or gif file!")
#     }
#     base64 <- base64enc::base64encode(figPath)
#     base64 <- paste0(
#       "data:image/",
#       figClass,
#       ";base64,",
#       base64
#     )
#   } else {
#     base64 <- NULL
#   }
#   weightFactor <- size * 180 / max(dataOut$freq)
#   settings <- list(
#     word = dataOut$name,
#     freq = dataOut$freq,
#     fontFamily = fontFamily,
#     fontWeight = fontWeight,
#     color = color,
#     minSize = minSize,
#     weightFactor = weightFactor,
#     backgroundColor = backgroundColor,
#     gridSize = gridSize,
#     minRotation = minRotation,
#     maxRotation = maxRotation,
#     shuffle = shuffle,
#     rotateRatio = rotateRatio,
#     shape = shape,
#     ellipticity = ellipticity,
#     figBase64 = base64,
#     hover = htmlwidgets::JS(hoverFunction)
#   )
#   chart <- htmlwidgets::createWidget(
#     "wordcloud2",
#     settings,
#     width = widgetsize[1],
#     height = widgetsize[2],
#     sizingPolicy = htmlwidgets::sizingPolicy(
#       viewer.padding = 0,
#       browser.padding = 0,
#       browser.fill = TRUE
#     )
#   )
#   chart
# }

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

### WORDCLOUD ----
#' Create a word cloud using ggwordcloud
#'
#' This function creates customizable word clouds using the ggwordcloud package.
#' It supports various shapes, colors, rotations, and faceting for comparative visualizations.
#'
#' @param data A data frame with at least two columns: the first column contains words,
#'   the second column contains the size/frequency of words. Additional columns can be
#'   used for faceting.
#' @param col_names Optional character vector of length 2 specifying column names
#'   for words and sizes. If NULL, uses first two columns (default: NULL)
#' @param max_size Maximum font size for words (default: 20)
#' @param min_size Minimum font size for words (default: 2)
#' @param eccentricity Eccentricity of the spiral (default: 0.65).
#'   Values > 1 make it wider, < 1 make it taller
#' @param shape Shape of the word cloud. One of "circle", "cardioid", "diamond",
#'   "square", "triangle-forward", "triangle-upright", "pentagon", "star" (default: "circle")
#' @param rm_outside Remove words that don't fit (default: FALSE)
#' @param rot_per Proportion of words with 90 degree rotation (default: 0.1)
#' @param colors Color palette or single color for words (default: "black").
#'   When faceting and length(colors) equals the number of facets, each facet
#'   will use a single color. Otherwise, colors are assigned to individual words.
#' @param color_by_size If TRUE, color words by their size using a gradient (default: FALSE)
#' @param color_gradient_low Low end of color gradient when color_by_size = TRUE (default: "darkblue")
#' @param color_gradient_high High end of color gradient when color_by_size = TRUE (default: "lightblue")
#' @param seed Random seed for reproducibility (default: NA)
#' @param area_corr Use area correction - font size proportional to sqrt of size (default: TRUE)
#' @param rstep Radial step size for spiral (default: 0.01)
#' @param tstep Angular step size for spiral (default: 0.02)
#' @param grid_size Grid size for collision detection (default: 4)
#' @param max_steps Maximum number of steps to find position for each word (default: 10)
#' @param xlim X-axis limits as c(min, max) (default: c(NA, NA))
#' @param ylim Y-axis limits as c(min, max) (default: c(NA, NA))
#' @param mask Optional mask image (PNG array) for custom shapes (default: NA)
#' @param show_boxes Show bounding boxes for debugging (default: FALSE)
#' @param background_color Background color of the plot (default: "white")
#' @param facet_by Optional column name for faceting. Creates separate word clouds
#'   for each unique value in this column (default: NULL)
#' @param facet_ncol Number of columns in facet layout (default: NULL, automatic)
#' @param facet_nrow Number of rows in facet layout (default: NULL, automatic)
#' @param facet_scales Control facet scales: "fixed", "free", "free_x", or "free_y" (default: "fixed")
#' @param facet_labeller Labeller for facet titles (default: "label_value")
#' @param facet_text_size Font size for facet titles (default: 16)
#' @param facet_text_face Font face for facet titles: "plain", "bold", "italic", or "bold.italic" (default: "bold")
#' @param facet_text_color Color for facet titles (default: "black")
#' @param ... Additional arguments passed to geom_text_wordcloud_area or geom_text_wordcloud
#'
#' @return A ggplot object with the word cloud visualization
#'
#' @examples
#' library(ggwordcloud)
#' library(dplyr)
#'
#' # Simple word cloud
#' df <- data.frame(
#'   word = c("statistics", "machine learning", "data", "analysis", "visualization"),
#'   freq = c(50, 45, 40, 30, 25)
#' )
#' wordcloud(df)
#'
#' # With custom colors and rotation
#' wordcloud(df,
#'           colors = "darkblue",
#'           rot_per = 0.3,
#'           shape = "diamond",
#'           max_size = 30)
#'
#' # With color gradient by size
#' wordcloud(df,
#'           color_by_size = TRUE,
#'           color_gradient_low = "blue",
#'           color_gradient_high = "red",
#'           seed = 42)
#'
#' # Multiple colors for individual words
#' colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
#' wordcloud(df,
#'           colors = colors,
#'           seed = 123)
#'
#' # With faceting - comparing two groups
#' df_facet <- data.frame(
#'   word = c("research", "analysis", "study", "data", "method",
#'            "treatment", "patient", "disease", "clinical", "therapy"),
#'   freq = c(100, 85, 75, 60, 55, 90, 80, 70, 65, 50),
#'   category = c(rep("Research", 5), rep("Clinical", 5))
#' )
#'
#' # One color per facet
#' wordcloud(df_facet,
#'           facet_by = "category",
#'           facet_ncol = 2,
#'           colors = c("#4575B4", "#D73027"),  # 2 colors for 2 facets
#'           facet_text_size = 16,
#'           seed = 42)
#'
#' # With larger facet titles
#' wordcloud(df_facet,
#'           facet_by = "category",
#'           facet_nrow = 1,
#'           colors = c("#E41A1C", "#377EB8"),
#'           facet_text_size = 18,
#'           facet_text_face = "bold",
#'           facet_text_color = "darkblue",
#'           max_size = 25,
#'           seed = 42)
#'
#' # Different shapes and eccentricity
#' wordcloud(df,
#'           shape = "star",
#'           eccentricity = 1.5,
#'           rot_per = 0.2,
#'           colors = "#FF7F00",
#'           max_size = 35,
#'           seed = 42)
#'
#' # Using bind_rows for comparative analysis
#' corpus1 <- data.frame(word = c("land", "restoration", "ecosystem"),
#'                       freq = c(50, 45, 40),
#'                       corpus = "Corpus 1")
#' corpus2 <- data.frame(word = c("disease", "treatment", "patient"),
#'                       freq = c(55, 50, 45),
#'                       corpus = "Corpus 2")
#'
#' wordcloud(bind_rows(corpus1, corpus2),
#'           facet_by = "corpus",
#'           facet_ncol = 2,
#'           colors = c("#1B9E77", "#D95F02"),
#'           facet_text_size = 16,
#'           max_size = 30,
#'           seed = 123)

posSel <- function(dfTag, pos) {
  dfTag <- dfTag %>% mutate(POSSelected = ifelse(upos %in% pos, TRUE, FALSE))
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


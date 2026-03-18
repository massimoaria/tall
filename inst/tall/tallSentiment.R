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
    )
  # %>%
  #   event_register("plotly_selecting")

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
    )
  # %>%
  #   event_register("plotly_click")
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

### EMOTION ANALYSIS: NRC EmoLex -----

emotion_colors <- function() {
  c(
    anger = "#D32F2F",
    anticipation = "#FF9800",
    disgust = "#6D4C41",
    fear = "#7B1FA2",
    joy = "#FFD600",
    sadness = "#5C6BC0",
    surprise = "#FF6F00",
    trust = "#388E3C"
  )
}

loadEmotionLexicon <- function(language) {
  emotion_names <- c(
    "anger", "anticipation", "disgust", "fear",
    "joy", "sadness", "surprise", "trust"
  )

  if (language == "english") {
    # English lexicon lacks emotion columns; extract from Italian lexicon
    # which has English Word -> emotion mapping from NRC EmoLex
    sentimentData <- loadSentimentLanguage("italian")
    if (!"English Word" %in% names(sentimentData)) return(NULL)
    emotion_lexicon <- sentimentData %>%
      select(`English Word`, all_of(emotion_names)) %>%
      rename(Word = `English Word`) %>%
      distinct(Word, .keep_all = TRUE) %>%
      filter(rowSums(across(all_of(emotion_names))) > 0)
  } else {
    sentimentData <- loadSentimentLanguage(language)
    if (!all(emotion_names %in% names(sentimentData))) return(NULL)
    emotion_lexicon <- sentimentData %>%
      select(Word, all_of(emotion_names)) %>%
      distinct(Word, .keep_all = TRUE) %>%
      filter(rowSums(across(all_of(emotion_names))) > 0)
  }
  return(emotion_lexicon)
}

emotionAnalysis <- function(dfTag, language = "english") {
  unsupported <- c(
    "ancient_greek", "classical_chinese", "coptic", "gothic",
    "north_sami", "old_church_slavonic", "old_french",
    "old_russian", "scottish_gaelic", "wolof"
  )
  if (language %in% unsupported) return(NA)

  emotion_names <- c(
    "anger", "anticipation", "disgust", "fear",
    "joy", "sadness", "surprise", "trust"
  )

  emotion_lexicon <- loadEmotionLexicon(language)
  if (is.null(emotion_lexicon)) return(NA)

  # Get lemmas from tagged data
  df <- dfTag %>% filter(docSelected)

  # Join lemmas with emotion lexicon
  word_emotions <- df %>%
    select(doc_id, lemma) %>%
    inner_join(emotion_lexicon, by = c("lemma" = "Word"))

  if (nrow(word_emotions) == 0) return(NA)

  # Per-document emotion counts
  doc_emotions <- word_emotions %>%
    group_by(doc_id) %>%
    summarise(across(all_of(emotion_names), sum), .groups = "drop") %>%
    mutate(total = rowSums(across(all_of(emotion_names)))) %>%
    mutate(
      across(
        all_of(emotion_names),
        ~ ifelse(total > 0, . / total, 0),
        .names = "{.col}_prop"
      )
    )

  # Corpus-level proportions
  total_count <- sum(colSums(doc_emotions[emotion_names]))
  corpus_emotions <- data.frame(
    emotion = emotion_names,
    count = as.numeric(colSums(doc_emotions[emotion_names])),
    stringsAsFactors = FALSE
  ) %>%
    mutate(proportion = if (total_count > 0) count / total_count else 0)

  # Word-level emotion counts (long format for word plots)
  word_emotion_long <- word_emotions %>%
    group_by(lemma) %>%
    summarise(across(all_of(emotion_names), sum), .groups = "drop") %>%
    pivot_longer(
      cols = all_of(emotion_names),
      names_to = "emotion",
      values_to = "count"
    ) %>%
    filter(count > 0)

  # Doc emotions in long format for heatmap
  doc_emotions_long <- doc_emotions %>%
    select(doc_id, all_of(paste0(emotion_names, "_prop"))) %>%
    pivot_longer(cols = -doc_id, names_to = "emotion", values_to = "proportion") %>%
    mutate(emotion = gsub("_prop$", "", emotion))

  return(list(
    doc_emotions = doc_emotions,
    doc_emotions_long = doc_emotions_long,
    corpus_emotions = corpus_emotions,
    word_emotions = word_emotion_long
  ))
}

emotionBarChart <- function(corpus_emotions) {
  colors <- emotion_colors()

  fig <- plot_ly(
    data = corpus_emotions,
    x = ~emotion,
    y = ~proportion,
    type = "bar",
    marker = list(
      color = colors[corpus_emotions$emotion],
      line = list(color = "white", width = 1)
    ),
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      "Proportion: %{y:.3f}<br>",
      "Count: ", corpus_emotions$count,
      "<extra></extra>"
    )
  ) %>%
    layout(
      xaxis = list(
        title = "Emotion",
        categoryorder = "total descending"
      ),
      yaxis = list(title = "Proportion"),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "pan2d", "select2d", "lasso2d",
        "toggleSpikelines", "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  fig
}

emotionWordPlot <- function(word_emotions, emotion_sel, n = 10) {
  colors <- emotion_colors()

  df <- word_emotions %>%
    filter(emotion == emotion_sel) %>%
    slice_max(count, n = n) %>%
    arrange(count)

  if (nrow(df) == 0) return(NULL)

  fig <- plot_ly(
    data = df,
    x = ~count,
    y = ~ reorder(lemma, count),
    type = "bar",
    orientation = "h",
    marker = list(
      color = paste0(colors[emotion_sel], "80"),
      line = list(color = colors[emotion_sel], width = 1)
    ),
    hovertemplate = "<b>%{y}</b><br>Count: %{x}<extra></extra>"
  ) %>%
    layout(
      xaxis = list(title = "Count"),
      yaxis = list(title = ""),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "pan2d", "select2d", "lasso2d",
        "toggleSpikelines", "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  fig
}

emotionRadarPlot <- function(corpus_emotions) {
  colors <- emotion_colors()

  # Close the polygon by repeating the first point
  df <- rbind(corpus_emotions, corpus_emotions[1, ])

  fig <- plot_ly(
    type = "scatterpolar",
    r = df$proportion,
    theta = df$emotion,
    fill = "toself",
    fillcolor = "rgba(74, 124, 89, 0.25)",
    line = list(color = "#4a7c59", width = 2),
    marker = list(
      color = colors[df$emotion],
      size = 8,
      line = list(color = "white", width = 1)
    ),
    hovertemplate = paste0(
      "<b>%{theta}</b><br>",
      "Proportion: %{r:.3f}<br>",
      "Count: ", df$count,
      "<extra></extra>"
    )
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, max(corpus_emotions$proportion) * 1.15),
          showticklabels = TRUE,
          tickfont = list(size = 10, color = "gray50")
        ),
        angularaxis = list(
          tickfont = list(size = 13, color = "gray30")
        )
      ),
      showlegend = FALSE,
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "pan2d", "select2d", "lasso2d",
        "toggleSpikelines", "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  fig
}

emotionHeatmap <- function(doc_emotions_long) {
  fig <- plot_ly(
    data = doc_emotions_long,
    x = ~emotion,
    y = ~doc_id,
    z = ~proportion,
    type = "heatmap",
    colorscale = list(c(0, "#F7F9FA"), c(1, "#4a7c59")),
    hovertemplate = paste0(
      "<b>Doc:</b> %{y}<br>",
      "<b>Emotion:</b> %{x}<br>",
      "<b>Proportion:</b> %{z:.3f}",
      "<extra></extra>"
    )
  ) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "", autorange = "reversed"),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud", "pan2d", "select2d", "lasso2d",
        "toggleSpikelines", "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )

  fig
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

# Helper function to create styled HTML box for abstract
create_abstract_box <- function(abstract_text) {
  html_content <- paste0(
    "<div style='",
    "background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);",
    "border-radius: 12px;",
    "padding: 30px;",
    "box-shadow: 0 8px 16px rgba(0,0,0,0.1);",
    "border-left: 5px solid #4CAF50;",
    "font-family: \"Georgia\", \"Times New Roman\", serif;",
    "line-height: 1.8;",
    "color: #2c3e50;",
    "max-height: 600px;",
    "overflow-y: auto;",
    "'>",
    "<div style='",
    "font-size: 1.1em;",
    "text-align: justify;",
    "'>",
    abstract_text,
    "</div>",
    "</div>"
  )
  return(html_content)
}

# Helper function to create document HTML with highlighted sentences
create_document_box <- function(
  document_df,
  doc_id,
  summarization_type = "extractive"
) {
  if (summarization_type != "original_text") {
    # Extract paragraphs and combine them
    paragraphs <- document_df$Paragraph

    # Replace <mark><strong> tags with styled span for highlighting
    paragraphs <- gsub(
      "<mark><strong>(.*?)</strong></mark>",
      "<span style='background-color: #ffeb3b; padding: 2px 4px; border-radius: 3px; font-weight: 500;'>\\1</span>",
      paragraphs
    )

    # Create paragraph HTML
    paragraph_html <- paste0(
      "<p style='margin-bottom: 20px; text-indent: 30px;'>",
      paragraphs,
      "</p>"
    )

    full_text <- paste(paragraph_html, collapse = "\n")
  } else {
    full_text <- document_df
  }

  if (summarization_type == "extractive") {
    legend = paste0(
      "<span style='",
      "display: inline-block;",
      "background-color: #ffeb3b;",
      "padding: 4px 8px;",
      "border-radius: 3px;",
      "margin-right: 8px;",
      "'>Highlighted</span> ",
      "= Sentences selected for summarization"
    )
  } else {
    legend = ""
  }

  html_content <- paste0(
    "<div style='",
    "background: #ffffff;",
    "border-radius: 12px;",
    "padding: 30px;",
    "box-shadow: 0 4px 12px rgba(0,0,0,0.08);",
    "border: 1px solid #e0e0e0;",
    "max-height: 600px;",
    "overflow-y: auto;",
    "'>",
    "<h3 style='",
    "color: #2c3e50;",
    "border-bottom: 2px solid #4CAF50;",
    "padding-bottom: 10px;",
    "margin-bottom: 25px;",
    "font-family: \"Arial\", sans-serif;",
    "'>Document: <strong>",
    doc_id,
    "</strong></h3>",
    "<div style='",
    "font-family: \"Georgia\", \"Times New Roman\", serif;",
    "font-size: 1.05em;",
    "line-height: 1.8;",
    "color: #34495e;",
    "text-align: justify;",
    "'>",
    full_text,
    "</div>",
    "<div style='",
    "margin-top: 20px;",
    "padding-top: 15px;",
    "border-top: 1px solid #e0e0e0;",
    "font-size: 0.9em;",
    "color: #7f8c8d;",
    "'>",
    legend,
    # "<span style='",
    # "display: inline-block;",
    # "background-color: #ffeb3b;",
    # "padding: 4px 8px;",
    # "border-radius: 3px;",
    # "margin-right: 8px;",
    # "'>Highlighted</span> ",
    # "= Sentences selected for summarization",
    "</div>",
    "</div>"
  )

  return(html_content)
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


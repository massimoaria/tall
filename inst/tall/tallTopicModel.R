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
    )
  # %>%
  #   event_register("plotly_selecting")

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
  # Calculate correlation matrix
  data <- cor(as.matrix(beta[, -1]))
  diag(data) <- 0

  df <- data.frame(data)

  # Create topic labels - ordinamento naturale
  n_topics <- nrow(data)
  id <- sprintf(paste0("%0", nchar(n_topics), "d"), 1:n_topics)
  x <- colnames(df) <- paste0("topic ", id)
  y <- row.names(df) <- paste0("topic ", id)

  # Prepare data for plotting
  df_long <- df %>%
    rownames_to_column("y") %>%
    pivot_longer(
      cols = starts_with("topic "),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      value = round(value, 3),
      abs_value = abs(value),
      topic_y_num = as.numeric(gsub("topic ", "", y)),
      topic_x_num = as.numeric(gsub("topic ", "", variable))
    )

  # Create mini scatterplot data for each cell
  scatter_data_list <- list()
  beta_matrix <- as.matrix(beta[, -1])
  n_topics_beta <- ncol(beta_matrix)

  for (i in 1:nrow(df_long)) {
    row_info <- df_long[i, ]
    topic_y <- row_info$topic_y_num
    topic_x <- row_info$topic_x_num

    if (topic_x != topic_y) {
      # Sample points for scatter (max 50 points per cell)
      n_points <- min(50, nrow(beta_matrix))
      sample_idx <- sample(1:nrow(beta_matrix), n_points)

      scatter_data_list[[i]] <- data.frame(
        cell_id = i,
        x_pos = row_info$variable,
        y_pos = row_info$y,
        x_cat_num = row_info$topic_x_num,
        y_cat_num = row_info$topic_y_num,
        scatter_x = beta_matrix[sample_idx, topic_x],
        scatter_y = beta_matrix[sample_idx, topic_y],
        correlation = row_info$value
      )
    }
  }

  scatter_data <- bind_rows(scatter_data_list)

  # Modern color palette
  pal <- colorRampPalette(c(
    "#d73027",
    "#f46d43",
    "#fdae61",
    "#fee090",
    "#ffffbf",
    "#e0f3f8",
    "#abd9e9",
    "#74add1",
    "#4575b4"
  ))(100)

  # PLOTLY VERSION - Interactive with borders and scatter
  Hplot <- plot_ly()

  # Add heatmap base - topic 1 at top
  Hplot <- Hplot %>%
    add_trace(
      data = df_long,
      x = ~ topic_x_num - 1,
      y = ~ topic_y_num - 1,
      z = ~value,
      type = "heatmap",
      colorscale = list(
        list(0, "#d73027"),
        list(0.25, "#fdae61"),
        list(0.5, "#ffffbf"),
        list(0.75, "#abd9e9"),
        list(1, "#4575b4")
      ),
      zmin = -1,
      zmax = 1,
      hovertemplate = "Correlation: %{z:.3f}<extra></extra>",
      # hovertemplate = paste(
      #   "<b>%{customdata[0]} vs %{customdata[1]}</b><br>",
      #   "Correlation: %{z:.3f}<br>",
      #   "<extra></extra>"
      # ),
      customdata = ~ cbind(y, variable),
      showscale = TRUE,
      xgap = 3,
      ygap = 3,
      colorbar = list(
        title = "Correlation",
        titleside = "right",
        tickmode = "linear",
        tick0 = -1,
        dtick = 0.5,
        len = 0.7,
        thickness = 15,
        x = 1.02
      )
    )

  # Add scatter points for each non-diagonal cell
  if (nrow(scatter_data) > 0) {
    for (cell_id in unique(scatter_data$cell_id)) {
      cell_scatter <- scatter_data[scatter_data$cell_id == cell_id, ]

      # Get cell position
      x_pos <- unique(cell_scatter$x_cat_num) - 1
      y_pos <- unique(cell_scatter$y_cat_num) - 1

      # Normalize scatter values to fit within cell (±0.35 offset)
      x_vals <- cell_scatter$scatter_x
      if (length(unique(x_vals)) > 1) {
        x_norm <- (x_vals - mean(x_vals)) / (max(x_vals) - min(x_vals))
        x_norm <- x_norm * 0.35
      } else {
        x_norm <- rep(0, length(x_vals))
      }

      y_vals <- cell_scatter$scatter_y
      if (length(unique(y_vals)) > 1) {
        y_norm <- (y_vals - mean(y_vals)) / (max(y_vals) - min(y_vals))
        y_norm <- y_norm * 0.35
      } else {
        y_norm <- rep(0, length(y_vals))
      }

      # Add scatter trace
      Hplot <- Hplot %>%
        add_markers(
          x = x_pos + x_norm,
          y = y_pos + y_norm,
          marker = list(
            size = 4,
            color = "rgba(50, 50, 50, 0.3)",
            line = list(width = 0)
          ),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
  }

  # Add text annotations - POSITIONED AT TOP OF CELL
  Hplot <- Hplot %>%
    add_annotations(
      data = df_long,
      x = ~ topic_x_num - 1,
      y = ~ topic_y_num - 1,
      text = ~ ifelse(value == 0, "—", as.character(value)),
      xref = "x",
      yref = "y",
      yshift = 25,
      showarrow = FALSE,
      font = list(
        color = ~ ifelse(abs(value) > 0.5, "white", "black"),
        size = 14,
        family = "Arial, sans-serif",
        weight = "bold"
      )
    ) %>%
    layout(
      title = list(
        text = "<b>Topic Correlation Matrix</b>",
        font = list(size = 18, color = "#2c3e50"),
        x = 0.5,
        xanchor = "center"
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = 0:(n_topics - 1),
        ticktext = x,
        tickangle = -45,
        tickfont = list(
          size = 11,
          family = "Arial, sans-serif",
          color = "black"
        ),
        side = "bottom",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        tickmode = "array",
        tickvals = 0:(n_topics - 1),
        ticktext = y, # NOT reversed - use natural order
        tickfont = list(
          size = 11,
          family = "Arial, sans-serif",
          color = "black"
        ),
        showgrid = FALSE,
        zeroline = FALSE,
        autorange = "reversed" # This makes y=0 appear at top
      ),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "white",
      margin = list(l = 100, r = 120, t = 80, b = 100),
      hovermode = "closest"
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
        "hoverCompareCartesian",
        "autoScale2d",
        "zoom2d"
      )
    )

  # GGPLOT2 VERSION - Static with scatterplots in cells
  df_long$y <- factor(df_long$y, levels = rev(y))
  df_long$variable <- factor(df_long$variable, levels = x)

  if (nrow(scatter_data) > 0) {
    scatter_data$y_pos <- factor(scatter_data$y_pos, levels = rev(y))
    scatter_data$x_pos <- factor(scatter_data$x_pos, levels = x)
  }

  HplotStatic <- ggplot() +
    # Base heatmap
    geom_tile(
      data = df_long,
      aes(x = variable, y = y, fill = value),
      color = "white",
      size = 1.5
    ) +
    # Add mini scatterplots in each cell
    {
      if (nrow(scatter_data) > 0) {
        geom_point(
          data = scatter_data,
          aes(
            x = as.numeric(x_pos) +
              (scatter_x - mean(scatter_x)) /
                (max(scatter_x) - min(scatter_x) + 0.001) *
                0.35,
            y = as.numeric(y_pos) +
              (scatter_y - mean(scatter_y)) /
                (max(scatter_y) - min(scatter_y) + 0.001) *
                0.35
          ),
          alpha = 0.3,
          size = 0.8,
          color = "gray30"
        )
      }
    } +
    # Add text annotations - POSITIONED AT TOP OF CELL
    geom_text(
      data = df_long,
      aes(
        x = variable,
        y = y,
        label = ifelse(value == 0, "—", value),
        color = abs(value) > 0.5
      ),
      size = 4.5,
      fontface = "bold",
      family = "sans",
      vjust = -0.5
    ) +
    # Color scales
    scale_fill_gradientn(
      colors = c("#d73027", "#fdae61", "#ffffbf", "#abd9e9", "#4575b4"),
      limits = c(-1, 1),
      breaks = seq(-1, 1, 0.5),
      guide = guide_colorbar(
        title = "Correlation",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 1,
        barheight = 15,
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    scale_color_manual(
      values = c("TRUE" = "white", "FALSE" = "black"),
      guide = "none"
    ) +
    # Theme and styling
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 16,
        color = "#2c3e50",
        margin = margin(b = 15)
      ),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 10,
        face = "bold"
      ),
      axis.text.y = element_text(
        size = 10,
        face = "bold"
      ),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#f8f9fa", color = NA),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
      title = "Topic Correlation Matrix with Data Distribution"
    ) +
    coord_fixed(ratio = 1)

  return(list(
    Hplot = Hplot,
    HplotStatic = HplotStatic
  ))
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
    )
  # %>%
  #   event_register("plotly_selecting")

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


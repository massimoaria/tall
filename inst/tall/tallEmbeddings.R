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

  ### EDGES
  x <- dfTag %>%
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



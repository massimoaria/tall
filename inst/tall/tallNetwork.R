contextNetwork <- function(df, dfTag, target_word, n = 50) {
  # Espandi le liste nelle colonne context_before, token, context_after, e upos
  longer_df <- df %>%
    mutate(segment_id = row_number()) %>%
    rowwise() %>%
    mutate(
      words = list(c(
        unlist(context_before),
        target_word,
        unlist(context_after)
      )),
      upos_list = list(unlist(upos))
    ) %>%
    ungroup() %>%
    select(segment_id, words, upos_list) %>%
    unnest(cols = c(words, upos_list)) %>%
    rename(token = words, upos = upos_list)

  uposSelected <- unique(LemmaSelection(dfTag) %>% select(upos) %>% pull())

  net <- network(
    longer_df %>% filter(upos %in% uposSelected),
    term = "token",
    group = c("segment_id"),
    n = n,
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
  lim.contr = 2
) {
  vars <- "Dim"

  # filter by contribution
  contr <- results$contrib %>%
    select(1:nDim) %>%
    ## sum the square of all variables
    mutate(contr_tot = rowSums(across(everything()))) %>%
    filter(contr_tot <= lim.contr) %>%
    rownames_to_column() %>%
    select("rowname")
  #

  dati <- results$wordCoord %>%
    select(starts_with(vars)) %>%
    select(all_of(1:nDim)) %>%
    rownames_to_column() %>%
    filter(rowname %in% contr$rowname) %>%
    column_to_rownames()

  D <- dist(
    dati
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
  lim.contr = 2
) {
  target_cols <- names(results$contrib)[c(dimX, dimY)]
  # filter by contribution
  contr <- results$contrib %>%
    as.data.frame() %>%
    rownames_to_column("rowname") %>%
    # Selezioniamo solo le colonne che ci interessano + il nome
    select(rowname, all_of(target_cols)) %>%
    # Calcoliamo la somma delle due colonne (senza quadrato!)
    mutate(contr_tot = rowSums(across(all_of(target_cols)))) %>%
    filter(contr_tot <= lim.contr) %>%
    select(rowname, contr_tot)
  #

  results$contrib <- results$contrib %>%
    rownames_to_column("word") %>%
    dplyr::filter(word %in% contr$rowname) %>%
    select(!word)

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

  hoverText <- paste0(
    " <b>",
    results$wordCoord$label,
    "</b>\n Inertia: ",
    round(results$wordCoord$inertia, 3),
    "\n Mass:   ",
    round(results$wordCoord$mass, 3),
    "\n Contribute:   ",
    round(results$wordCoord$contr_tot, 3)
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
    )
  # %>%
  #   event_register("plotly_selecting")
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

# Modified network() function with seed fixing and improved community repulsion
# Based on bibliometrix networkPlot() implementation

# Add these new parameters to the network() function signature:
# - seed: Random seed for clustering reproducibility (default = 123)
# - cluster: Type of clustering algorithm (default = "walktrap")

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
  community.repulsion = 0.5, # Changed default from 0 to 0.5
  seed = 123,
  cluster = "louvain"
) {
  # size scaling
  scalemin <- 20 * (1 + labelsize / 5)
  scalemax <- 70 * (1 + labelsize / 5)

  colorlist <- colorlist()

  # params
  shape <- "dot"
  opacity.min <- 0.4

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
  } else {
    minEdges <- as.numeric(gsub("%", "", minEdges))
    tailEdges <- quantile(edges$value, 1 - (minEdges / 100), na.rm = T)
  }

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
    }
  }

  ### COMMUNITY DETECTION WITH SEED FIXING STRATEGY
  graph <- igraph::graph_from_data_frame(
    edges %>% select(-term_from, -term_to),
    directed = FALSE
  )

  # STRATEGY 1: SEED FIXING FOR STOCHASTIC ALGORITHMS
  # For stochastic algorithms (louvain, leiden), run multiple times and keep best result
  if (cluster %in% c("louvain", "leiden")) {
    n_runs <- 10
    best_modularity <- -Inf
    best_result <- NULL

    for (i in 1:n_runs) {
      set.seed(seed + i - 1) # Different seed for each run

      if (cluster == "louvain") {
        result <- igraph::cluster_louvain(graph)
      } else if (cluster == "leiden") {
        result <- igraph::cluster_leiden(
          graph,
          objective_function = "modularity",
          n_iterations = 3,
          resolution_parameter = 0.75
        )
      }

      current_modularity <- igraph::modularity(graph, result$membership)

      if (current_modularity > best_modularity) {
        best_modularity <- current_modularity
        best_result <- result
      }
    }

    net_groups <- best_result
  } else {
    # For other algorithms, use standard logic with set.seed
    set.seed(seed)

    switch(
      cluster,
      walktrap = {
        net_groups <- igraph::cluster_walktrap(graph)
      },
      optimal = {
        net_groups <- igraph::cluster_optimal(graph)
      },
      fast_greedy = {
        net_groups <- igraph::cluster_fast_greedy(graph)
      },
      leading_eigen = {
        net_groups <- igraph::cluster_leading_eigen(graph)
      },
      spinglass = {
        net_groups <- igraph::cluster_spinglass(graph)
      },
      infomap = {
        net_groups <- igraph::cluster_infomap(graph)
      },
      edge_betweenness = {
        net_groups <- igraph::cluster_edge_betweenness(graph)
      },
      {
        # Default to walktrap
        net_groups <- igraph::cluster_walktrap(graph)
      }
    )
  }

  cluster_df <- data.frame(as.list(igraph::membership(net_groups)))
  cluster_df <- as.data.frame(t(cluster_df)) %>%
    mutate(id = as.numeric(gsub("X", "", rownames(.)))) %>%
    rename(group = "V1")

  # Create group column
  nodes <- left_join(nodes, cluster_df, by = "id") %>%
    drop_na(group)

  # STRATEGY 2: IMPROVED COMMUNITY REPULSION
  if (community.repulsion > 0) {
    # Extract community structure information
    membership <- nodes$group
    names(membership) <- nodes$label
    n_communities <- length(unique(membership))
    n_nodes <- nrow(nodes)

    # Calculate statistics for adaptive normalization
    community_sizes <- table(membership)
    avg_community_size <- mean(community_sizes)

    # Calculate adaptive repulsion strength
    repulsion_strength <- adaptive_repulsion_strength(
      community.repulsion,
      n_nodes,
      n_communities,
      avg_community_size
    )

    # Get edge matrix
    row <- edges %>%
      select(term_from, term_to) %>%
      as.matrix()

    # Save original values
    original_values <- edges$value

    # Apply new weighting scheme with gradual growth
    new_values <- numeric(nrow(row))

    for (i in 1:nrow(row)) {
      node1 <- row[i, 1]
      node2 <- row[i, 2]

      comm1 <- membership[which(names(membership) == node1)]
      comm2 <- membership[which(names(membership) == node2)]

      if (comm1 == comm2) {
        # INTRA-COMMUNITY Edge
        # Moderate increase with sub-linear growth
        multiplier <- 1 + (repulsion_strength^0.7) * 1.5
        new_values[i] <- original_values[i] * multiplier
      } else {
        # INTER-COMMUNITY Edge
        # Gradual reduction with attenuated exponential function
        divisor <- 1 + exp(repulsion_strength * 1.2) - 1
        new_values[i] <- original_values[i] / divisor
      }
    }

    # Apply new values
    edges$value <- new_values
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
  return(obj)
}

# Helper function for adaptive community repulsion
adaptive_repulsion_strength <- function(
  community.repulsion,
  n_nodes,
  n_communities,
  avg_community_size
) {
  # Scale factor based on network size
  scale_factor <- log10(n_nodes + 10) / log10(100)

  # Correction factor based on the number of communities
  community_factor <- 1 + (n_communities - 2) * 0.1
  community_factor <- max(0.5, min(community_factor, 2))

  # Sigmoidal transformation of the user parameter
  x <- community.repulsion * 10
  sigmoid_transform <- x / (1 + x)

  # Combine the factors
  strength <- sigmoid_transform * scale_factor * community_factor

  return(strength)
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


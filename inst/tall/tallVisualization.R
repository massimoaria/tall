wordcloud <- function(
  data,
  col_names = NULL,
  max_size = 20,
  min_size = 2,
  eccentricity = 0.65,
  shape = "circle",
  rm_outside = FALSE,
  rot_per = 0.1,
  colors = "black",
  color_by_size = FALSE,
  color_gradient_low = "darkblue",
  color_gradient_high = "lightblue",
  seed = NA,
  area_corr = TRUE,
  rstep = 0.01,
  tstep = 0.02,
  grid_size = 4,
  max_steps = 10,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  mask = NA,
  show_boxes = FALSE,
  background_color = "white",
  facet_by = NULL,
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_scales = "fixed",
  facet_labeller = "label_value",
  facet_text_size = 16,
  facet_text_face = "bold",
  facet_text_color = "black",
  ...
) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (ncol(data) < 2) {
    stop("'data' must have at least 2 columns")
  }

  # Handle column names
  if (is.null(col_names)) {
    words_col <- names(data)[1]
    size_col <- names(data)[2]
  } else {
    if (length(col_names) != 2) {
      stop("'col_names' must be a character vector of length 2")
    }
    if (!all(col_names %in% names(data))) {
      stop("Column names specified in 'col_names' not found in data")
    }
    words_col <- col_names[1]
    size_col <- col_names[2]
  }

  # Validate facet_by if provided
  if (!is.null(facet_by)) {
    if (!facet_by %in% names(data)) {
      stop("Column '", facet_by, "' specified in 'facet_by' not found in data")
    }
  }

  # Prepare data
  plot_data <- data.frame(
    label = as.character(data[[words_col]]),
    size = as.numeric(data[[size_col]]),
    stringsAsFactors = FALSE
  )

  # Add facet variable if specified
  if (!is.null(facet_by)) {
    plot_data$facet_var <- data[[facet_by]]
  }

  # Remove NA values
  plot_data <- plot_data[!is.na(plot_data$label) & !is.na(plot_data$size), ]

  if (nrow(plot_data) == 0) {
    stop("No valid data to plot after removing NAs")
  }

  # Add rotation angle if rot_per > 0
  if (rot_per > 0) {
    n <- nrow(plot_data)
    plot_data$angle <- 90 *
      sample(c(0, 1), n, replace = TRUE, prob = c(1 - rot_per, rot_per))
  } else {
    plot_data$angle <- 0
  }

  # Handle colors
  if (color_by_size) {
    # Use size for color gradient
    plot_data$color_var <- plot_data$size
  } else {
    # Check if we have faceting and colors match number of facets
    if (!is.null(facet_by) && length(colors) > 1) {
      # Get unique facets
      unique_facets <- unique(plot_data$facet_var)
      n_facets <- length(unique_facets)

      # If number of colors equals number of facets, assign one color per facet
      if (length(colors) == n_facets) {
        # Create a named vector mapping facets to colors
        facet_colors <- setNames(colors, unique_facets)
        # Assign colors based on facet
        plot_data$color_var <- facet_colors[as.character(plot_data$facet_var)]
      } else {
        # Otherwise, assign colors to individual words
        plot_data$color_var <- rep_len(colors, nrow(plot_data))
      }
    } else {
      # No faceting or single color
      if (length(colors) == 1) {
        plot_data$color_var <- colors
      } else {
        # If colors vector is shorter than data, recycle it
        plot_data$color_var <- rep_len(colors, nrow(plot_data))
      }
    }
  }

  # Set seed if provided
  if (!is.na(seed)) {
    set.seed(seed)
  }

  # Build the base plot with color aesthetic
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(label = label, size = size, angle = angle, color = color_var)
  )

  # Choose geom based on area_corr
  if (area_corr) {
    p <- p +
      ggwordcloud::geom_text_wordcloud_area(
        eccentricity = eccentricity,
        shape = shape,
        rm_outside = rm_outside,
        rstep = rstep,
        tstep = tstep,
        grid_size = grid_size,
        max_steps = max_steps,
        xlim = xlim,
        ylim = ylim,
        mask = mask,
        area_corr = TRUE,
        show_boxes = show_boxes,
        ...
      )
  } else {
    p <- p +
      ggwordcloud::geom_text_wordcloud(
        eccentricity = eccentricity,
        shape = shape,
        rm_outside = rm_outside,
        rstep = rstep,
        tstep = tstep,
        grid_size = grid_size,
        max_steps = max_steps,
        xlim = xlim,
        ylim = ylim,
        mask = mask,
        area_corr = FALSE,
        show_boxes = show_boxes,
        ...
      )
  }

  # Add size scale
  p <- p + ggplot2::scale_size_area(max_size = max_size)

  # Add color scale
  if (color_by_size) {
    p <- p +
      ggplot2::scale_color_gradient(
        low = color_gradient_low,
        high = color_gradient_high,
        guide = "none"
      )
  } else {
    if (length(colors) == 1) {
      p <- p + ggplot2::scale_color_identity()
    } else {
      p <- p + ggplot2::scale_color_identity()
    }
  }

  # Add faceting if specified
  if (!is.null(facet_by)) {
    p <- p +
      ggplot2::facet_wrap(
        ~facet_var,
        ncol = facet_ncol,
        nrow = facet_nrow,
        scales = facet_scales,
        labeller = facet_labeller
      )
  }

  # Add theme
  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background_color,
        color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = background_color,
        color = NA
      ),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "none",
      strip.text = ggplot2::element_text(
        size = facet_text_size,
        face = facet_text_face,
        color = facet_text_color
      )
    )

  return(p)
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

tallThematicmap <- function(
  dfTag,
  term = "lemma",
  group = "doc_id",
  n = 100,
  labelsize = 10,
  n.labels = 1,
  opacity = 0.8,
  seed = 1234
) {
  net <- network(
    LemmaSelection(dfTag) %>% dplyr::filter(docSelected),
    term = term,
    group = group,
    n = n,
    minEdges = "Auto",
    labelsize = labelsize,
    opacity = opacity,
    interLinks = FALSE,
    normalization = "association",
    remove.isolated = FALSE,
    community.repulsion = 0.5,
    seed = seed,
    cluster = "louvain"
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

menuList <- function(menu) {
  CORPUS <- tags$div(
    id = "corpus-header",
    style = "display: flex;
        align-items: center;
        font-size: 14px;
        font-weight: 600;
        color: #FFFFFF;
        background: rgba(255,255,255,0.1);
        padding: 10px 10px;
        margin: 15px 8px 8px 8px;
        border-radius: 6px;
        border-left: 3px solid #26A69A;
        letter-spacing: 0.8px;",
    tags$span(
      style = "background: #26A69A;
          padding: 4px 8px;
          border-radius: 4px;
          margin-right: 10px;
          font-size: 12px;",
      icon("database")
    ),
    "CORPUS"
  )

  PREPROCESSING <- tags$div(
    id = "preprocessing-header",
    style = "display: flex;
        align-items: center;
        justify-content: space-between;
        font-size: 14px;
        font-weight: 600;
        color: #FFFFFF;
        background: rgba(255,255,255,0.1);
        padding: 10px 10px;
        margin: 15px 8px 8px 8px;
        border-radius: 6px;
        border-left: 3px solid #9C27B0;
        letter-spacing: 0.8px;",
    tags$div(
      style = "display: flex; align-items: center;",
      tags$span(
        style = "background: #9C27B0;
            padding: 4px 8px;
            border-radius: 4px;
            margin-right: 10px;
            font-size: 12px;",
        icon("wand-magic-sparkles")
      ),
      "PREPROCESSING"
    )
  )

  FEATURES <- tags$div(
    id = "features-header",
    style = "display: flex;
        align-items: center;
        font-size: 14px;
        font-weight: 600;
        color: #FFFFFF;
        background: rgba(255,255,255,0.1);
        padding: 10px 10px;
        margin: 15px 8px 8px 8px;
        border-radius: 6px;
        border-left: 3px solid #5C6BC0;
        letter-spacing: 0.8px;",
    tags$span(
      style = "background: #5C6BC0;
          padding: 4px 8px;
          border-radius: 4px;
          margin-right: 10px;
          font-size: 12px;",
      icon("puzzle-piece")
    ),
    "FEATURES"
  )

  ANALYSIS <- tags$div(
    id = "analysis-header",
    style = "display: flex;
        align-items: center;
        font-size: 14px;
        font-weight: 600;
        color: #FFFFFF;
        background: rgba(255,255,255,0.1);
        padding: 10px 10px;
        margin: 15px 8px 8px 8px;
        border-radius: 6px;
        border-left: 3px solid #FF5722;
        letter-spacing: 0.8px;",
    tags$span(
      style = "background: #FF5722;
          padding: 4px 8px;
          border-radius: 4px;
          margin-right: 10px;
          font-size: 12px;",
      icon("microscope")
    ),
    "ANALYSIS"
  )

  import_menu <- menuItem(
    "Import",
    tabName = "import_tx",
    icon = icon("open-file", lib = "glyphicon")
  )

  edit_menu <- menuItem(
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
  )

  preprocessing_menu0 <- menuItem(
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
  )

  preprocessing_menu1 <- menuItem(
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
      "Custom PoS List",
      tabName = "custTermList",
      icon = icon("chevron-right"),
      selected = TRUE
    ),
    menuSubItem(
      "Synonyms Merging",
      tabName = "synonymsMgmt",
      icon = icon("chevron-right")
    ),
    menuSubItem(
      "PoS Tag Selection",
      tabName = "posTagSelect",
      icon = icon("chevron-right")
    )
  )

  overview_menu <- menuItem(
    "Overview",
    tabName = "overview",
    icon = icon("search", lib = "glyphicon")
  )

  keyness_menu <- menuItem(
    "Keyness",
    tabName = "keyness",
    icon = icon("key")
  )

  kwic_menu <- menuItem(
    "KWIC",
    tabName = "kwic",
    icon = icon("align-center")
  )

  word_menu <- menuItem(
    "Words",
    tabName = "words",
    icon = icon("font", lib = "glyphicon"),
    # menuSubItem(
    #   "KWIC",
    #   tabName = "wordCont",
    #   icon = icon("chevron-right")
    # ),
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
  )
  document_menu <- menuItem(
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
      "Supervised Classification",
      tabName = "doc_classification",
      icon = icon("chevron-right")
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
  )

  features_menu <- list(
    menuItem(
      "Filter",
      tabName = "filter_text",
      icon = icon("filter")
    ),
    menuItem(
      "Groups",
      tabName = "defineGroups",
      icon = icon("th", lib = "glyphicon")
    ),
    menuItem(
      "Feature Roles",
      tabName = "feature_roles",
      icon = icon("tags", lib = "glyphicon")
    )
  )

  setting_menu <- tags$div(
    style = "display: none;",
    menuItem("Settings", tabName = "settings", icon = icon("tasks"))
  )

  switch(
    as.character(menu),
    "-2" = {
      list(
        CORPUS,
        PREPROCESSING,
        FEATURES,
        ANALYSIS,
        setting_menu
      )
    },
    "0" = {
      list(
        CORPUS,
        import_menu,
        edit_menu,
        PREPROCESSING,
        preprocessing_menu0,
        FEATURES,
        ANALYSIS,
        setting_menu
      )
    },
    "1" = {
      list(
        CORPUS,
        import_menu,
        edit_menu,
        PREPROCESSING,
        preprocessing_menu1,
        FEATURES,
        ANALYSIS,
        setting_menu
      )
    },
    "2" = {
      list(
        CORPUS,
        import_menu,
        edit_menu,
        PREPROCESSING,
        preprocessing_menu1,
        FEATURES,
        ANALYSIS,
        overview_menu,
        keyness_menu,
        kwic_menu,
        word_menu,
        document_menu,
        tags$div(style = "margin-top: 20px;"),
        menuItem("Report", tabName = "report", icon = icon("list-alt")),
        setting_menu
      )
    },
    "3" = {
      list(
        CORPUS,
        import_menu,
        edit_menu,
        PREPROCESSING,
        preprocessing_menu1,
        FEATURES,
        features_menu,
        ANALYSIS,
        overview_menu,
        keyness_menu,
        kwic_menu,
        word_menu,
        document_menu,
        tags$div(style = "margin-top: 20px;"),
        menuItem("Report", tabName = "report", icon = icon("list-alt")),
        setting_menu
      )
    },
    {
      list(
        CORPUS,
        import_menu,
        PREPROCESSING,
        FEATURES,
        ANALYSIS,
        setting_menu
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
  specialtags = FALSE,
  col_to_remove = NULL
) {
  tall_primary <- "#4a7c59"
  tall_secondary <- "#5a9269"
  tall_accent <- "#F7F9FA"
  tall_text <- "#2D3748"
  tall_border <- "#E2E8F0"
  tall_white <- "#FFFFFF"

  if (!is.null(col_to_remove)) {
    col_to_remove_lower <- tolower(col_to_remove)

    if (col_to_remove_lower == "lemma") {
      if ("token" %in% names(df)) {
        df <- df %>% select(-token)
      }
      if ("Token" %in% names(df)) {
        df <- df %>% select(-Token)
      }
    } else if (col_to_remove_lower == "token") {
      if ("lemma" %in% names(df)) {
        df <- df %>% select(-lemma)
      }
      if ("Lemma" %in% names(df)) {
        df <- df %>% select(-Lemma)
      }
    }
  }

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
        text = '<i class="fa fa-file-excel"></i> Excel',
        className = 'btn btn-success',
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
        text = '<i class="fa fa-file-excel"></i> Excel',
        className = 'btn btn-success',
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
    select <- list(style = "os", items = "row")
  } else {
    extensions <- c("Buttons", "ColReorder", "FixedHeader")
    select <- NULL
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
      columnDefs = columnDefs,
      initComplete = DT::JS(
        paste0(
          "function(settings, json) {
          var api = this.api();

          $(api.table().header()).css({
            'background': '",
          tall_primary,
          "',
            'color': 'white',
            'font-weight': '600',
            'font-size': '13px',
            'padding': '12px 8px',
            'border-bottom': '2px solid ",
          tall_secondary,
          "',
            'text-transform': 'uppercase',
            'letter-spacing': '0.5px'
          });

          $(api.table().container()).find('.dataTables_wrapper').css({
            'font-family': '-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, sans-serif'
          });

          $(api.table().container()).find('.dataTables_filter input').css({
            'border': '1px solid ",
          tall_border,
          "',
            'border-radius': '4px',
            'padding': '6px 12px',
            'margin-left': '8px',
            'font-size': '13px',
            'outline': 'none'
          });

          $(api.table().container()).find('.dataTables_length select').css({
            'border': '1px solid ",
          tall_border,
          "',
            'border-radius': '4px',
            'padding': '4px 8px',
            'margin': '0 8px',
            'font-size': '13px'
          });

          $(api.table().container()).find('.dt-buttons .dt-button').css({
            'background': '",
          tall_primary,
          "',
            'color': 'white',
            'border': 'none',
            'border-radius': '4px',
            'padding': '6px 14px',
            'margin-right': '6px',
            'font-size': '13px',
            'font-weight': '500',
            'cursor': 'pointer',
            'box-shadow': '0 1px 3px rgba(0,0,0,0.1)'
          });

          $(api.table().container()).find('.dataTables_paginate .paginate_button').css({
            'border-radius': '4px',
            'padding': '6px 12px',
            'margin': '0 2px',
            'border': '1px solid ",
          tall_border,
          "',
            'font-size': '13px'
          });

          $(api.table().container()).find('.dataTables_paginate .paginate_button.current').css({
            'background': '",
          tall_primary,
          "',
            'color': 'white',
            'border-color': '",
          tall_primary,
          "'
          });
        }"
        )
      ),
      rowCallback = DT::JS(
        paste0(
          "function(row, data, index) {
          if(index % 2 === 0) {
            $(row).css('background-color', '",
          tall_accent,
          "');
          } else {
            $(row).css('background-color', '",
          tall_white,
          "');
          }
        }"
        )
      )
    ),
    class = "cell-border stripe hover"
  ) %>%
    DT::formatStyle(
      names(df),
      textAlign = "center",
      fontSize = size,
      fontFamily = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      color = tall_text,
      padding = '10px 8px'
    )

  if (!is.null(left)) {
    tab <- tab %>%
      DT::formatStyle(
        names(df)[left],
        textAlign = "left"
      )
  }

  if (!is.null(right)) {
    tab <- tab %>%
      DT::formatStyle(
        names(df)[right],
        textAlign = "right"
      )
  }

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
    dplyr::slice_head(n = n)

  col_x <- names(df)[x]
  col_y <- names(df)[y]

  max_val <- max(df[[col_x]], na.rm = TRUE)
  limit_x <- max_val + (max_val * 0.06)

  g <- ggplot(
    df,
    aes(x = .data[[col_x]], y = reorder(.data[[col_y]], .data[[col_x]]))
  ) +
    geom_col(color = "#c3d1be", fill = "#96af8e") +
    geom_text(
      aes(label = .data[[col_x]]),
      hjust = -0.2,
      color = "#4f7942",
      size = 3.7
    ) +
    labs(title = title, y = "", x = "Frequency") +
    scale_x_continuous(
      limits = c(0, limit_x),
      expand = c(0, 0)
    ) +
    theme(
      axis.text.y = element_text(size = 9),
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


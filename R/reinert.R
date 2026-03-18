utils::globalVariables(c(
  "doc_id",
  "uc",
  "uce",
  "segment_size",
  "upos",
  "noSingleChar",
  "token",
  "lemma",
  "freq",
  "chi_square",
  "cluster",
  "negative",
  "positive",
  "term",
  "freq_true",
  "indep",
  "p_value",
  ".",
  "segment",
  "seg_doc",
  "prev_doc"
))


#' Segment clustering based on the Reinert method - Simple clustering
#'
#' @param x tall data frame of documents
#' @param k maximum number of clusters to compute
#' @param term indicates the type of form "lemma" or "token". Default value is term = "lemma".
#' @param segment_size number of forms by document. Default value is segment_size = 40
#' @param min_segment_size minimum number of forms by document. Default value is min_segment_size = 5
#' @param min_split_members minimum number of segment in a cluster
#' @param cc_test contingency coefficient value for feature selection
#' @param tsj minimum frequency value for feature selection
#'
#' @details
#' See the references for original articles on the method.
#' Special thanks to the authors of the rainette package (https://github.com/juba/rainette)
#' for inspiring the coding approach used in this function.
#'
#'
#' @return
#' The result is a list of both class `hclust` and `reinert_tall`.
#'
#' @references
#'
#' - Reinert M, Une methode de classification descendante hierarchique: application à l'analyse lexicale par contexte, Cahiers de l'analyse des donnees, Volume 8, Numéro 2, 1983. <https://www.numdam.org/item/?id=CAD_1983__8_2_187_0>
#' - Reinert M., Alceste une méthodologie d'analyse des données textuelles et une application: Aurelia De Gerard De Nerval, Bulletin de Methodologie Sociologique, Volume 26, Numero 1, 1990. \doi{10.1177/075910639002600103}
#' - Barnier J., Privé F., rainette: The Reinert Method for Textual Data Clustering, 2023, \doi{10.32614/CRAN.package.rainette}
#'
#' @examples
#' \donttest{
#' data(mobydick)
#' res <- reinert(
#'   x = mobydick,
#'   k = 10,
#'   term = "token",
#'   segment_size = 40,
#'   min_segment_size = 5,
#'   min_split_members = 10,
#'   cc_test = 0.3,
#'   tsj = 3
#' )
#' }
#'
#' @export
#'

reinert <- function(
  x,
  k = 10,
  term = "token",
  segment_size = 40,
  min_segment_size = 3,
  min_split_members = 5,
  cc_test = 0.3,
  tsj = 3
) {
  if (min_split_members < 3) {
    warning("min_split_members has been set to 3, its smallest possible value.")
    min_split_members <- 3
  }

  uposList <- c(
    "ADP",
    "DET",
    "INTJ",
    "NUM",
    "PART",
    "PRON",
    "PUNCT",
    "SCONJ",
    "SYM",
    "X",
    "EMAIL",
    "EMOJI",
    "HASH",
    "IP_ADDRESS",
    "MENTION",
    "URL"
  )

  ## Create DTM from TAll Data Frame
  switch(
    term,
    token = {
      x <- x %>%
        dplyr::filter(!upos %in% uposList & noSingleChar) %>%
        mutate(token_rainette = tolower(token))
      colTerm <- "token_rainette"
    },
    lemma = {
      x <- x %>%
        dplyr::filter(!upos %in% uposList & noSingleChar) %>%
        mutate(lemma_rainette = tolower(lemma))
      colTerm <- "lemma_rainette"
    }
  )

  ## Create segments by segment_size
  idTable <- split_segments(x$doc_id, segment_size)

  ## Correspondance table between uce and uc
  corresp_uce_uc_full <- merge_small_segments(
    idTable,
    min_length = min_segment_size
  )

  x <- x %>% bind_cols(corresp_uce_uc_full %>% select(-"doc_id"))

  corresp_uce_uc_full$term <- x[[colTerm]]

  ## Table with assosacions among uc and uce
  corresp_uce_uc <- corresp_uce_uc_full %>%
    select(-"doc_id", -"term") %>%
    distinct() %>%
    as.data.frame()

  ## Table with segments
  corresp_uce_uc_full <- corresp_uce_uc_full %>%
    group_by(doc_id, uc) %>%
    summarize(segment = paste0(term, collapse = " ")) %>%
    as.data.frame()

  corresp_uce_uc_full <- corresp_uce_uc_full %>%
    group_by(uc) %>%
    reframe(
      doc_id = doc_id[1],
      segment = paste0(segment, collapse = " ")
    ) %>%
    distinct() %>%
    data.frame()

  row.names(corresp_uce_uc_full) <- corresp_uce_uc_full$uc

  dtf <- document_term_frequencies(x, document = "uc", term = colTerm)

  ## Apply binary weighting
  dtf <- dtf %>%
    mutate(freq = freq^0)

  ## create DTM
  dtmOriginal <- document_term_matrix(dtf, weight = "freq")

  dtmOriginal <- dtm_remove_lowfreq(dtmOriginal, minfreq = 3)
  ## Remove low frequency terms
  dtm <- as.matrix(dtmOriginal)

  ## Remove empty strings to avoid subcript out of bounds errors
  ind <- rowSums(dtm) > 0
  dtm <- dtm[ind, ]

  message("  Clustering...")

  ## Initialize results list with first dtm
  res <- list(list(tabs = list(dtm)))

  exclusion_list <- NULL

  i <- 1

  while (i < k) {
    ## Split the biggest group
    nrows <- purrr::map(res[[i]]$tabs, nrow)
    names(nrows) <- 1:length(nrows)
    # nrows <- lapply(nrows, function(x) if (is.null(x)) 0 else x)
    # biggest_group <- which.max(nrows)
    if (length(exclusion_list) > 0) {
      biggest_group <- as.numeric(names(which.max(nrows[-exclusion_list])))
    } else {
      biggest_group <- as.numeric(names(which.max(nrows)))
    }

    if (nrow(res[[i]]$tabs[[biggest_group]]) < min_split_members) {
      message(
        "! No more group bigger than min_split_members. Stopping after iteration ",
        i,
        "."
      )
      k <- i
      break
    }
    tab <- res[[i]]$tabs[[biggest_group]]
    ## textmodel_ca only works if nrow >= 3 and ncol >= 3
    if (nrow(tab) < 3 || ncol(tab) < 3) {
      message(
        "! Tab to be splitted is not big enough. Stopping after iteration ",
        i,
        "."
      )
      k <- i
      break
    }

    ## Remove documents and features with zero occurrences
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]

    clusters <- cluster_tab(tab, cc_test = cc_test, tsj = tsj)
    if (
      "matrix" %in%
        class(clusters$tabs[[1]]) &
        "matrix" %in% class(clusters$tabs[[2]])
    ) {
      ## Populate results
      res[[i + 1]] <- list()
      res[[i + 1]]$height <- clusters$height
      res[[i + 1]]$splitted <- c(biggest_group, biggest_group + 1)
      res[[i + 1]]$tabs <- append(
        res[[i]]$tabs[-biggest_group],
        clusters$tabs,
        after = biggest_group - 1
      )
      res[[i + 1]]$groups <- append(
        res[[i]]$groups[-biggest_group],
        clusters$groups,
        after = biggest_group - 1
      )
      i <- i + 1
    } else {
      exclusion_list <- c(exclusion_list, biggest_group)
    }
  }
  # })

  if (k == 1) {
    message("! No computed clusters. Returning NULL.")
    return(NULL)
  }

  res <- res[-1]

  ## Compute the merge element of resulting hclust result
  groups <- 1:k
  merge <- matrix(nrow = 0, ncol = 2)
  for (i in (k - 1):1) {
    split <- res[[i]]$splitted
    merge <- rbind(merge, -groups[split])
    groups <- groups[-split[2]]
    groups[split[1]] <- -(k - i)
  }

  # Compute groups by uce at each k
  uce_groups <- list()
  for (i in 1:(k - 1)) {
    # group <- rep(NA, nrow(corresp_uce_uc))
    group <- rep(NA, nrow(corresp_uce_uc_full))
    indices <- res[[i]]$groups
    for (group_index in seq_along(indices)) {
      group[corresp_uce_uc_full[indices[[group_index]], "uc"]] <- group_index
      # group[corresp_uce_uc_full$uc %in% as.numeric(indices[[group_index]])] <- group_index
    }
    uce_groups[[i]] <- group
  }

  ## Get the final group element of resulting hclust result
  ## by uce
  group <- uce_groups[[k - 1]]

  message("  Done.")

  ## Compute and return hclust-class result
  hres <- list(
    method = "reinert",
    call = match.call(),
    height = cumsum(rev(purrr::map_dbl(res, ~ .x$height))),
    order = 1:k,
    labels = as.character(1:k),
    merge = merge,
    group = group,
    uce_groups = uce_groups,
    corresp_uce_uc = corresp_uce_uc,
    corresp_uce_uc_full = corresp_uce_uc_full,
    dtmOriginal = dtmOriginal
  )

  class(hres) <- c("reinert_tall", "hclust")
  hres
}


order_docs <- function(m) {
  ## Compute first factor of CA on DTM
  ## Code taken from getAnywhere(textmodel_ca.dfm)
  p <- m / sum(m)
  rm <- rowSums(p)
  cm <- colSums(p)
  ep <- tcrossprod(rm, cm)
  s <- (p - ep) / sqrt(ep)
  dec <- RSpectra::svds(s, k = 1, nv = 0)
  coord <- dec$u[, 1] / sqrt(rm)

  ## Order documents by their first factor coordinates
  indices <- (seq_along(coord))[order(coord)]

  return(indices)
}


switch_docs <- function(m, indices, max_index, max_chisq) {
  ## Group indices and tabs
  group1 <- indices[1:which(indices == max_index)]
  group2 <- indices[(which(indices == max_index) + 1):length(indices)]

  switched <- TRUE

  ## Run while points are switched
  while (switched) {
    switched <- FALSE

    tab1 <- m[group1, , drop = FALSE]
    tab2 <- m[group2, , drop = FALSE]

    chisq_values <- cpp_switch_docs(tab1, tab2)
    current_max <- max(chisq_values, na.rm = TRUE)

    if (current_max > max_chisq) {
      switched <- TRUE
      to_switch <- indices[which.max(chisq_values)]
      if (to_switch %in% group1) {
        group1 <- group1[group1 != to_switch]
        group2 <- c(group2, to_switch)
      } else {
        group2 <- group2[group2 != to_switch]
        group1 <- c(group1, to_switch)
      }
      max_chisq <- current_max
    }
  }

  return(list(
    indices1 = group1,
    indices2 = group2,
    chisq = max_chisq
  ))
}


select_features <- function(m, indices1, indices2, cc_test = 0.3, tsj = 3) {
  ## features count for each group
  tab1 <- colSums(m[indices1, , drop = FALSE])
  tab2 <- colSums(m[indices2, , drop = FALSE])
  ## Total number of features in each group
  nfeat_group1 <- sum(tab1)
  nfeat_group2 <- sum(tab2)
  ## Observed frequency of features
  observed <- rbind(tab1, tab2)
  ## Expected frequency of features
  expected_prop <- (tab1 + tab2) / sum(tab1 + tab2)
  expected <- rbind(expected_prop * nfeat_group1, expected_prop * nfeat_group2)
  ## Chi2 and contingency coefficients for each feature
  feat_chisq <- colSums((observed - expected)^2 / expected)
  ## C contingency coefficient, sqrt(khi2 / (khi2 + N))
  feat_cc <- sqrt(feat_chisq / (feat_chisq + colSums(observed)))

  ## Features selection
  cols1 <- character()
  cols2 <- character()

  for (i in seq_along(feat_cc)) {
    cc <- feat_cc[i]
    name <- names(feat_cc)[i]
    ## Keep feature if cc <= cc_test and frequency > 0
    if (cc <= cc_test && tab1[i] >= tsj) {
      cols1 <- c(cols1, name)
    }
    if (cc <= cc_test && tab2[i] >= tsj) {
      cols2 <- c(cols2, name)
    }
    ## If cc > cc_test, only keep feature in the group
    ## where observed frequency > expected frequency
    if (cc > cc_test) {
      if (tab1[i] > expected[1, i] && tab1[i] >= tsj) {
        cols1 <- c(cols1, name)
      }
      if (tab2[i] > expected[2, i] && tab2[i] >= tsj) {
        cols2 <- c(cols2, name)
      }
    }
  }

  return(list(
    cols1 = cols1,
    cols2 = cols2
  ))
}


cluster_tab <- function(m, cc_test = 0.3, tsj = 3) {
  uc <- row.names(m)
  ## Remove features with zero
  storage.mode(m) <- "integer"

  ## First step : CA partition

  indices <- order_docs(m)
  res <- cpp_split_tab(m, indices)
  max_index <- res$max_index
  max_chisq <- res$max_chisq

  ## Second step : switching docs

  res <- switch_docs(m, indices, max_index, max_chisq)
  indices1 <- res$indices1
  indices2 <- res$indices2
  chisq <- res$chisq

  ## Third step : features selection

  res <- select_features(m, indices1, indices2, cc_test, tsj)
  cols1 <- res$cols1
  cols2 <- res$cols2

  return(list(
    groups = list(
      rainette_uc_id = uc[indices1],
      rainette_uc_id = uc[indices2]
    ),
    tabs = list(
      m[indices1, cols1],
      m[indices2, cols2]
    ),
    height = chisq
  ))
}


## Split segments
split_segments <- function(doc_id, segment_size) {
  segment_id <- integer(length(doc_id))
  current_segment <- 1
  count <- 0

  for (i in seq_along(doc_id)) {
    count <- count + 1
    segment_id[i] <- current_segment

    # Controllo fine del segmento o cambio di documento
    if (
      count == segment_size ||
        (i < length(doc_id) && doc_id[i] != doc_id[i + 1])
    ) {
      current_segment <- current_segment + 1
      count <- 0
    }
  }

  return(data.frame(doc_id = doc_id, uce = segment_id))
}

merge_small_segments <- function(idTable, min_length = 5) {
  # Compute segment sizes and first doc_id per segment
  seg_info <- idTable %>%
    group_by(uce) %>%
    mutate(segment_size = n()) %>%
    ungroup()

  # Get the doc_id of the first row of each segment
  seg_doc <- seg_info %>%
    group_by(uce) %>%
    summarize(seg_doc = doc_id[1], .groups = "drop")

  # Build lookup for previous segment's doc
  seg_doc <- seg_doc %>%
    mutate(prev_doc = dplyr::lag(seg_doc, default = ""))

  seg_info <- seg_info %>%
    left_join(seg_doc, by = "uce") %>%
    mutate(
      # Only merge with previous segment if same document
      uc = if_else(segment_size < min_length & seg_doc == prev_doc, uce - 1, uce)
    ) %>%
    select(-segment_size, -seg_doc, -prev_doc)

  seg_info
}

#' Extract Terms and Segments for Document Clusters
#'
#' This function processes the results of a document clustering algorithm based on the Reinert method.
#' It computes the terms and their significance for each cluster, as well as the associated document segments.
#'
#' @param res A list containing the results of the Reinert clustering algorithm. Must include at least `dtm` (a document-term matrix) and `corresp_uce_uc_full` (a correspondence between segments and clusters).
#' @param cutree A custom cutree structure. If `NULL`, the default `cutree_reinart` is used to determine cluster membership.
#' @param k A vector of integers specifying the clusters to analyze. Default is `1`.
#' @param negative Logical. If `TRUE`, include negative terms in the results. If `FALSE`, exclude them. Default is `TRUE`.
#'
#' @return A list with the following components:
#' \item{terms}{A data frame of significant terms for each cluster. Columns include:
#'   \itemize{
#'     \item \code{chi_square}: Chi-squared statistic for the term.
#'     \item \code{p_value}: P-value of the chi-squared test.
#'     \item \code{sign}: Significance of the term (\code{positive}, \code{negative}, or \code{none}).
#'     \item \code{term}: The term itself.
#'     \item \code{freq}: Observed frequency of the term in the cluster.
#'     \item \code{indep}: Expected frequency of the term under independence.
#'     \item \code{cluster}: The cluster ID.
#'   }
#' }
#' \item{segments}{A data frame of document segments associated with each cluster. Columns include:
#'   \itemize{
#'     \item \code{uc}: Unique segment identifier.
#'     \item \code{doc_id}: Document ID for the segment.
#'     \item \code{cluster}: Cluster ID.
#'     \item \code{segment}: The text content of each segment.
#'   }
#' }
#'
#' @details The function integrates document-term matrix rows for missing segments, calculates term statistics for each cluster,
#' and filters terms based on their significance. Terms can be excluded based on their significance (\code{signExcluded}).
#'
#' @examples
#' \donttest{
#' data(mobydick)
#' res <- reinert(
#'   x = mobydick,
#'   k = 10,
#'   term = "token",
#'   segment_size = 40,
#'   min_segment_size = 5,
#'   min_split_members = 10,
#'   cc_test = 0.3,
#'   tsj = 3
#' )
#'
#' tc <- term_per_cluster(res, cutree = NULL, k = 1:10, negative = FALSE)
#'
#' head(tc$segments, 10)
#'
#' head(tc$terms, 10)
#' }
#'
#' @export

term_per_cluster <- function(res, cutree = NULL, k = 1, negative = TRUE) {
  k <- k[!is.na(k)]
  dtm <- res$dtmOriginal
  groups <- cutree_reinart(res, cutree)
  terms <- colnames(dtm)

  ## integrate dtm with removed segments (by full-zero rows)
  label <- row.names(dtm)
  max_ind <- max(res$corresp_uce_uc_full$uc)

  ind <- setdiff(as.character(1:max_ind), label)

  if (length(ind) > 0) {
    m <- Matrix::Matrix(0, nrow = length(ind), ncol = ncol(dtm), sparse = TRUE)
    rownames(m) <- ind
    colnames(m) <- colnames(dtm)
    dtm <- rbind(dtm, m)
    dtm <- dtm[order(as.numeric(rownames(dtm))), ]
  }

  terms_list <- list()
  segments_list <- list()
  K <- k
  for (i in 1:length(K)) {
    k <- K[i]
    ## list of segments following into the cluster k
    select <- (groups == k & !is.na(groups))
    segments <- row.names(dtm)[select]
    segments_df <- tibble(uc = as.numeric(segments)) %>%
      left_join(res$corresp_uce_uc_full, by = "uc") %>%
      mutate(cluster = k)
    segments_list[[k]] <- segments_df

    chi_res_df <- dtm_chisq(dtm, groups = select) %>%
      mutate(indep = sum(freq_true) / sum(freq)) %>% # expected proportion for independence
      mutate(cluster = k) %>%
      rename(
        "chi_square" = "chisq",
        "p_value" = "p.value"
      )

    terms_list[[k]] <- chi_res_df
  }

  if (isTRUE(negative)) {
    signExcluded <- c("none")
  } else {
    signExcluded <- c("none", "negative")
  }

  terms_list <- bind_rows(terms_list) %>%
    group_by(term) %>%
    mutate(freq = sum(freq_true)) %>%
    ungroup() %>%
    filter(p_value <= 0.001) %>%
    mutate(
      freq = freq_true / freq,
      sign = ifelse(freq > indep, "positive", "negative")
    ) %>%
    filter(!sign %in% signExcluded)

  segments_list <- bind_rows(segments_list) %>% drop_na("doc_id")

  return(list(terms = terms_list, segments = segments_list))
}

# plot for terms by cluster

#' Plot Terms by Cluster
#'
#' This function creates a horizontal bar plot to visualize the most significant terms
#' for each cluster, based on their Chi-squared statistics.
#'
#' @param terms A data frame containing terms and their associated statistics, such as Chi-squared values,
#' generated by the `term_per_cluster` function. The data frame must include the following columns:
#' \itemize{
#'   \item \code{term}: The term to plot.
#'   \item \code{chi_square}: The Chi-squared statistic associated with the term.
#'   \item \code{sign}: The sign of the term (\code{"positive"} or \code{"negative"}).
#' }
#' @param nPlot Integer. The number of top terms to plot for each sign (\code{"positive"} and \code{"negative"}). Default is 10.
#'
#' @return An interactive horizontal bar plot (using `plotly`) displaying the top terms for each cluster. The plot includes:
#' \itemize{
#'   \item Bars representing the Chi-squared values of terms.
#'   \item Hover information displaying the term and its Chi-squared value.
#' }
#'
#' @details The function organizes the input data by Chi-squared values and selects the top terms for each sign.
#' The plot uses different colors for positive and negative terms, with hover tooltips providing detailed information.
#'
#' @seealso \code{\link{term_per_cluster}}
#'
#' @examples
#' \dontrun{
#' data(mobydick)
#' res <- reinert(
#'   x = mobydick,
#'   k = 10,
#'   term = "token",
#'   segment_size = 40,
#'   min_segment_size = 5,
#'   min_split_members = 10,
#'   cc_test = 0.3,
#'   tsj = 3
#' )
#'
#' tc <- term_per_cluster(res, cutree = NULL, k = 1, negative = FALSE)
#'
#' fig <- reinPlot(tc$terms, nPlot = 10)
#' }
#'
#' @export
#'
reinPlot <- function(terms, nPlot = 10) {
  # Separate positive and negative terms
  dfPositive <- terms %>%
    filter(sign == "positive") %>%
    ungroup() %>%
    arrange(desc(chi_square)) %>%
    slice_head(n = nPlot)

  dfNegative <- terms %>%
    filter(sign == "negative") %>%
    ungroup() %>%
    arrange(desc(chi_square)) %>%
    slice_head(n = nPlot)

  # Combine positive and negative terms
  dfPlot <- bind_rows(dfPositive, dfNegative) %>%
    mutate(
      term = factor(term, levels = rev(c(dfPositive$term, dfNegative$term))) # Invertire l'ordine
    )

  # Assign colors
  color <- colorlist()

  # build the plot
  fig1 <- plot_ly(
    data = dfPlot,
    x = ~chi_square,
    y = ~term,
    type = "bar",
    orientation = "h",
    marker = list(color = ~ ifelse(sign == "positive", color[1], color[2])),
    hovertemplate = "<b><i>Term: %{y}</i></b> <br> <b><i>Chi2: %{x}</i></b><extra></extra>"
  )

  # layout
  fig1 <- fig1 %>%
    plotly::layout(
      yaxis = list(
        title = "Terms",
        showgrid = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        domain = c(0, 1)
      ),
      xaxis = list(
        title = "Chi2",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE
      ),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    plotly::config(
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

  return(fig1)
}

# summarize Reinert Results

#' Summarize Reinert Clustering Results
#'
#' This function summarizes the results of the Reinert clustering algorithm, including the most frequent documents and significant terms for each cluster.
#' The input is the result returned by the `term_per_cluster` function.
#'
#' @param tc A list returned by the \code{term_per_cluster} function. The list includes:
#' \itemize{
#'   \item \code{segments}: A data frame with segments information, including \code{cluster} and \code{doc_id}.
#'   \item \code{terms}: A data frame with terms information, including \code{cluster}, \code{sign}, \code{chi_square}, and \code{term}.
#' }
#' @param n Integer. The number of top terms (based on Chi-squared value) to include in the summary for each cluster and sign. Default is 10.
#'
#' @return A data frame summarizing the clustering results. The table includes:
#' \itemize{
#'   \item \code{cluster}: The cluster ID.
#'   \item \code{Positive terms}: The top \code{n} positive terms for each cluster, concatenated into a single string.
#'   \item \code{Negative terms}: The top \code{n} negative terms for each cluster, concatenated into a single string.
#'   \item \code{Most frequent document}: The document ID that appears most frequently in each cluster.
#'   \item \code{N. of Documents per Cluster}: The number of documents in each cluster.
#' }
#'
#' @details This function performs the following steps:
#' \enumerate{
#'   \item Extracts the most frequent document for each cluster.
#'   \item Summarizes the number of documents per cluster.
#'   \item Selects the top \code{n} terms for each cluster, separated by positive and negative signs.
#'   \item Combines the terms and segment information into a final summary table.
#' }
#'
#'
#' @seealso \code{\link{term_per_cluster}}, \code{\link{reinPlot}}
#'
#' @examples
#' \donttest{
#' data(mobydick)
#' res <- reinert(
#'   x = mobydick,
#'   k = 10,
#'   term = "token",
#'   segment_size = 40,
#'   min_segment_size = 5,
#'   min_split_members = 10,
#'   cc_test = 0.3,
#'   tsj = 3
#' )
#'
#' tc <- term_per_cluster(res, cutree = NULL, k = 1:10, negative = FALSE)
#'
#' S <- reinSummary(tc, n = 10)
#'
#' head(S, 10)
#' }
#'
#' @export
#'
reinSummary <- function(tc, n = 10) {
  segments <- tc$segments %>%
    group_by(cluster) %>%
    # summarise("Number of Segments" = n()) %>%
    # group_by(cluster) %>%
    summarize(
      "Most frequent document" = names(which.max(table(doc_id))),
      "N. of Segments per Cluster" = n()
    )

  terms <- tc$terms %>%
    group_by(cluster, sign) %>%
    slice_max(order_by = chi_square, n = 10, with_ties = TRUE) %>%
    summarize(terms = paste0(term, collapse = "; "), .groups = "drop") %>%
    pivot_wider(
      names_from = sign,
      values_from = terms,
      names_prefix = "",
      values_fill = list(terms = NA) # Riempie con NA se mancano valori
    )

  if (!"positive" %in% names(terms)) {
    terms$positive <- ""
  } else {
    terms$positive[is.na(terms$positive)] <- ""
  }
  if (!"negative" %in% names(terms)) {
    terms$negative <- ""
  } else {
    terms$negative[is.na(terms$negative)] <- ""
  }

  terms <- terms %>%
    rename(
      "Positively Associated Terms" = positive,
      "Negatively Associated Terms" = negative
    )

  summaryTable <- terms %>%
    left_join(segments, by = "cluster")

  return(summaryTable)
}

cutree_reinart <- function(res, k = NULL) {
  if (!is.null(k)) {
    res$uce_groups[[min(c(k - 1, length(res$uce_groups)))]]
  } else {
    res$uce_groups[[length(res$uce_groups)]]
  }
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

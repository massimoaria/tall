utils::globalVariables(c("doc_id","uc","uce","segment_size","upos","noSingleChar","token","lemma","freq","chi_square"))


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
#' The result is a list of both class `hclust` and `reinert_tall`. Besides the elements
#' of an `hclust` object, two more results are available :
#'
#' - `uce_groups` give the group of each document for each k
#' - `group` give the group of each document for the maximum value of k available
#'
#'
#' @references
#'
#' - Reinert M, Une méthode de classification descendante hiérarchique : application à l'analyse lexicale par contexte, Cahiers de l'analyse des données, Volume 8, Numéro 2, 1983. <http://www.numdam.org/item/?id=CAD_1983__8_2_187_0>
#' - Reinert M., Alceste une méthodologie d'analyse des données textuelles et une application: Aurelia De Gerard De Nerval, Bulletin de Méthodologie Sociologique, Volume 26, Numéro 1, 1990. \doi{10.1177/075910639002600103}
#' - Barnier J., Privé F., rainette: The Reinert Method for Textual Data Clustering, 2023, \doi{10.32614/CRAN.package.rainette}
#'
#' @export
#'

reinert <- function(
  x, k = 10, term="token",
  segment_size = 40,
  min_segment_size = 3,
  min_split_members = 5,
  cc_test = 0.3, tsj = 3
  ) {

  if (min_split_members < 3) {
    warning("min_split_members has been set to 3, its smallest possible value.")
    min_split_members <- 3
  }

  uposList <- c('ADP','DET','INTJ','NUM','PART','PRON','PUNCT','SCONJ','SYM',
    'X','EMAIL','EMOJI','HASH','IP_ADDRESS','MENTION','URL')

  ## Create DTM from TAll Data Frame
  switch(term,
         token={
           x <- x %>%
             dplyr::filter(!upos %in% uposList & noSingleChar) %>%
             mutate(token_rainette=tolower(token))
           colTerm <- "token_rainette"
         },
         lemma={
           x <- x %>%
             dplyr::filter(!upos %in% uposList & noSingleChar) %>%
             mutate(lemma_rainette=tolower(lemma))
           colTerm <- "lemma_rainette"
         })


  ## Create segments by segment_size
  idTable <- split_segments(x$doc_id, segment_size)

  ## Correspondance table between uce and uc
  corresp_uce_uc_full <- merge_small_segments(idTable, min_length = min_segment_size)

  x <- x %>% bind_cols(corresp_uce_uc_full %>% select(-"doc_id"))

  corresp_uce_uc_full$term <- x[[colTerm]]

  ## Table with assosacions among uc and uce
  corresp_uce_uc <- corresp_uce_uc_full %>% select(-"doc_id", -"term") %>% distinct() %>% as.data.frame()

  ## Table with segments
  corresp_uce_uc_full <- corresp_uce_uc_full %>%
    group_by(doc_id, uc) %>%
    summarize(segment = paste0(term,collapse = " "))

  dtf <- document_term_frequencies(x, document="uc",term=colTerm)


  ## Apply binary weighting
  dtf <- dtf %>%
    mutate(freq=freq^0)

  ## create DTM
  dtm <- document_term_matrix(dtf, weight="freq")

  ## Remove low frequency terms
  dtm <- as.matrix(dtm_remove_lowfreq(dtm, minfreq = 3))

  ## Remove empty strings to avoid subcript out of bounds errors
  ind <- rowSums(dtm)>0
  dtm <- dtm[ind,]

  message("  Clustering...")

  ## Initialize results list with first dtm
  res <- list(list(tabs = list(dtm)))

  exclusion_list <- NULL
  ## Display progress bar
  # progressr::with_progress({
  #   p <- progressr::progressor(along = seq_len(k - 1))

    #for (i in 1:(k - 1)) {
  i <- 1

  while(i<k){

      ## Split the biggest group
      nrows <- purrr::map(res[[i]]$tabs, nrow)
      names(nrows) <- 1:length(nrows)
      #nrows <- lapply(nrows, function(x) if (is.null(x)) 0 else x)
      #biggest_group <- which.max(nrows)
      if (length(exclusion_list)>0){
        biggest_group <- as.numeric(names(which.max(nrows[-exclusion_list])))
      } else {
        biggest_group <- as.numeric(names(which.max(nrows)))
      }

      if (nrow(res[[i]]$tabs[[biggest_group]]) < min_split_members) {
        message("! No more group bigger than min_split_members. Stopping after iteration ", i, ".")
        k <- i
        break
      }
      tab <- res[[i]]$tabs[[biggest_group]]
      ## textmodel_ca only works if nrow >= 3 and ncol >= 3
      if (nrow(tab) < 3 || ncol(tab) < 3) {
        message("! Tab to be splitted is not big enough. Stopping after iteration ", i, ".")
        k <- i
        break
      }

      ## Remove documents and features with zero occurrences
      tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]

      clusters <- cluster_tab(tab, cc_test = cc_test, tsj = tsj)
      if ("matrix" %in% class(clusters$tabs[[1]]) & "matrix" %in% class(clusters$tabs[[2]])){
        ## Populate results
        res[[i + 1]] <- list()
        res[[i + 1]]$height <- clusters$height
        res[[i + 1]]$splitted <- c(biggest_group, biggest_group + 1)
        res[[i + 1]]$tabs <- append(res[[i]]$tabs[-biggest_group],
                                    clusters$tabs,
                                    after = biggest_group - 1
        )
        res[[i + 1]]$groups <- append(res[[i]]$groups[-biggest_group],
                                      clusters$groups,
                                      after = biggest_group - 1
        )
        i <- i + 1
      } else {
        exclusion_list <- c(exclusion_list, biggest_group)
      }

    }
  #})

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
    groups[split[1]] <- - (k - i)
  }

  # Compute groups by uce at each k
  uce_groups <- list()
  for (i in 1:(k - 1)) {
    group <- rep(NA, nrow(corresp_uce_uc))
    indices <- res[[i]]$groups
    for (group_index in seq_along(indices)) {
      group[corresp_uce_uc$uc %in% indices[[group_index]]] <- group_index
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
    dtm=dtm
  )

  class(hres) <- c("reinert_tall", "hclust")
  hres
}


#' return documents indices ordered by CA first axis coordinates
#'
#' @param m is the dtm on which to compute the CA and order documents, converted to an integer matrix.
#'
#' @details
#' Internal function, not to be used directly
#'
#' @return ordered list of document indices

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


#' Switch documents between two groups to maximize chi-square value
#'
#' @param m the original dtm created from x
#' @param indices documents indices orderes by first CA axis coordinates
#' @param max_index document index where the split is maximum
#' @param max_chisq maximum chi-square value
#'
#' @details
#' Internal function, not to be used directly
#'
#' @return a list of two vectors `indices1` and `indices2`, which contain
#' the documents indices of each group after documents switching, and a `chisq` value,
#' the new corresponding chi-square value after switching

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


#' Remove features from the dtm of each group base don cc_test and tsj
#'
#' @param m is the original dtm obtained from x
#' @param indices1 indices of documents of group 1
#' @param indices2 indices of documents of group 2
#' @param cc_test maximum contingency coefficient value for the
#' feature to be kept in both groups.
#' @param tsj minimum feature frequency in the dtm
#'
#' @details
#' Internal function, not to be used directly
#'
#' @return a list of two character vectors : `cols1` is the name of features to
#' keep in group 1, `cols2` the name of features to keep in group 2

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


#' Split a dtm into two clusters with reinert algorithm
#'
#' @param m id the dtm to be split, passed by `reinart`
#' @param cc_test maximum contingency coefficient value for the
#' feature to be kept in both groups.
#' @param tsj minimum feature frequency in the dtm
#'
#' @details
#' Internal function, not to be used directly
#'
#' @return
#' A list with the two dtms returned from splitting algorithm

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
      rainette_uc_id=uc[indices1],
      rainette_uc_id=uc[indices2]
    ),
    tabs = list(
      m[indices1, cols1],
      m[indices2, cols2]
    ),
    height = chisq
  ))
}


#' split documents into segments
#'
#' @param doc_id is the  vector of doc_id labels extracted from x
#' @param segment_size number of forms by document. Default value is segment_size = 40
#'
#' @details
#' Internal function, not to be used directly
#'
#' @return ordered list of document indices

## Split segments
split_segments <- function(doc_id, segment_size) {
  segment_id <- integer(length(doc_id))
  current_segment <- 1
  count <- 0

  for (i in seq_along(doc_id)) {
    count <- count + 1
    segment_id[i] <- current_segment

    # Controllo fine del segmento o cambio di documento
    if (count == segment_size || (i < length(doc_id) && doc_id[i] != doc_id[i + 1])) {
      current_segment <- current_segment + 1
      count <- 0
    }
  }

  return(data.frame(doc_id=doc_id,uce=segment_id))
}

#' merge small segments with the previous ones
#'
#' @param idTable is a data frame obtained by split_segments
#' @param min_length min number of forms by segment. Default value is segment_size = 5
#'
#' @details
#' Internal function, not to be used directly
#'
#' @return ordered list of document indices
merge_small_segments <- function(idTable, min_length=5) {
  idTable %>%
    group_by(uce) %>%
    mutate(segment_size = n()) %>%  # Calcola la dimensione di ogni segmento
    #ungroup() %>%
    mutate(
      uc = if_else(segment_size < min_length, uce-1, uce)
    ) %>%
    select(-segment_size)  # Rimuove la colonna temporanea
}

# cutree_reinart <- function(res, k = NULL) {
#   if (!is.null(k)){
#     res$uce_groups[[min(c(k-1,length(res$uce_groups)))]]
#   } else{
#     res$uce_groups[[length(res$uce_groups)]]
#   }
#
# }
#
# term_per_cluster <- function(res, cutree=NULL, k=1, negative=FALSE){
#
#   dtm <- res$dtm
#   groups <- cutree_reinart(res,cutree)
#   terms <- colnames(dtm)
#
#   ## integrate dtm with removed segments (by full-zero rows)
#   label <- row.names(dtm)
#   max_ind <- max(res$corresp_uce_uc_full$uc)
#
#   ind <- setdiff(as.character(1:max_ind),label)
#
#   if (length(ind)>0){
#     m <- matrix(0,length(ind),ncol(dtm))
#     row.names(m) <- ind
#     dtm <- rbind(dtm,m)
#     dtm <- dtm[order(as.numeric(rownames(dtm))), ]
#   }
#
#   terms_list <- list()
#   segments_list <- list()
#   K <- k
#   for (i in 1:length(K)){
#     k <- K[i]
#     ## list of segments following into the cluster k
#     select <- (groups == k & !is.na(groups))
#     segments <- row.names(dtm)[select]
#     segments_df <- tibble(uc=as.numeric(segments)) %>%
#       left_join(res$corresp_uce_uc_full, by="uc") %>%
#       mutate(cluster = k)
#     segments_list[[k]] <-segments_df
#
#
#     m1 <- colSums(dtm[select,])
#     m0 <- colSums(dtm[!select,])
#
#     totm1 <- sum(m1)
#     totm0 <- sum(m0)
#
#     chi_res <- list()
#
#     for (i in 1:length(m1)){
#       tab <- matrix(c(m1[i],m0[i],totm1,totm0),2,2)
#       chi_res[[i]] <- chi_squared_test(tab)
#       chi_res[[i]]$term <- terms[i]
#
#     }
#
#     signExcluded <- ifelse(isTRUE(negative),c("none"),c("none","negative"))
#
#     chi_res_df <- do.call(rbind, lapply(chi_res, function(x) {
#       data.frame(
#         chi_square = x$chi_square,
#         p_value = x$p_value,
#         sign = x$sign,
#         term = x$term,
#         freq = x$tab[1,1],
#         indep = x$tab[1,2]
#       )
#     })) %>% filter(!sign %in% signExcluded) %>% arrange(desc(chi_square))
#
#     row.names(chi_res_df) <- chi_res_df$term
#     chi_res_df$cluster <- k
#     terms_list[[k]] <- chi_res_df
#   }
#
#   terms_list <- bind_rows(terms_list)
#   segments_list <- bind_rows(segments_list) %>% drop_na("doc_id")
#
#
#   return(list(terms =  terms_list, segments = segments_list))
#
#   ### DA AGGIUNGERE L'EVIDENZIAZIONE DEI TERMINI DEI SEGMENTI CHE APPARTENGONO AL CLUSTER
# }
#
#
# ## Chi Square test between observed and theoretical distribution
# chi_squared_test <- function(tab) {
#   # Controlla che la tabella sia una matrice o un data frame
#   if (!is.matrix(tab) && !is.data.frame(tab)) {
#     stop("La tabella deve essere una matrice o un data frame.")
#   }
#
#   # Esegui il test chi-quadrato
#    suppressWarnings(test_result <-chisq.test(tab))
#
#   # Estrai i valori di interesse
#   chi_square_value <- test_result$statistic
#   p_value <- test_result$p.value
#
#   tab <- prop.table(tab,2)
#
#   if (p_value<0.001){
#     if ((tab[1,1]-tab[1,2])>0){
#       sign <- "positive"
#     }else{
#       sign <- "negative"
#     }
#   }else{
#     sign <- "none"
#   }
#
#   # Return results into a list
#   return(list(chi_square = chi_square_value, p_value = p_value, tab=tab, sign=sign))
# }
#

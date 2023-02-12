
### DATA ----
# IMPORT TEXT FUNCTIONS ----

getFileNameExtension <- function (fn) {
  # remove a path
  splitted    <- strsplit(x=fn, split='/')[[1]]
  # or use .Platform$file.sep in stead of '/'
  fn          <- splitted [length(splitted)]
  ext         <- ''
  splitted    <- strsplit(x=fn, split='\\.')[[1]]
  l           <-length (splitted)
  if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l]
  # the extention must be the suffix of a non-empty name
  ext
}

read_files <- function(files, ext=c("txt","csv", "xlsx"), subfolder=TRUE, line_sep=FALSE){

  #files <- list.files(path=path, pattern = paste0(".",ext,"$"), recursive = subfolder)
  if (is.null(files)) return(data.frame(doc_id=NA,text=NA, folder=NA))

  if ("datapath" %in% names(files)){
    doc_id <- files$name
    file <- files$datapath
    folder=NA
  }

  if (getFileNameExtension(file[1])=="zip"){
    zip_file <-  unzip(file[1])
    zip_file <- zip_file[(substr(zip_file,nchar(zip_file)-nchar(ext)+1,nchar(zip_file))==ext)]
    file <- zip_file[regexpr("__MACOSX",zip_file)==-1]
    doc_id <- unlist(lapply(strsplit(file,"/"), function(l){l[length(l)]}))
    folder <- unlist(lapply(strsplit(file, "/"), function(l){l[length(l)-1]}))
  }

  switch(ext,
         txt={
           if (isTRUE(line_sep)){line_sep <- ". "}else{line_sep <- " "}

           df <- data.frame(doc_id=doc_id,text=NA, folder=folder, file=file) %>%
             group_by(doc_id) %>%
             mutate(text = gsub("\\.\\.","\\.",paste(read_lines(file,skip_empty_rows = TRUE),sep="",collapse=line_sep))) %>%
             select(-file)
         },
         csv={
           listdf <- list()
           for (i in seq_len(length(file))){
             listdf[[i]] <- read_csv(file[i], show_col_types=FALSE) %>%
               mutate(doc_id = doc_id[i])
           }

           df <- do.call(rbind,listdf)
         },

         xlsx={
           df <- readxl::read_excel(file, col_types = "text")
         }
         )

return(df)
}

### PRE_PROCESSING ----

### 1. TOKENIZATION ----
loadLanguageModel <- function(language, model_version="-ud-2.5", model_repo = "jwijffels/udpipe.models.ud.2.5"){
  switch(Sys.info()[['sysname']],
         Windows= {home <- Sys.getenv('R_USER')},
         Linux  = {home <- Sys.getenv('HOME')},
         Darwin = {home <- Sys.getenv('HOME')})

  # setting up the main directory
  path_tall <- file.path(home,"tall")
  path_language_model <- file.path(path_tall, "language_models")
  # check if sub directory exists
  if (file.exists(path_tall)){
    if (!file.exists(path_language_model)) dir.create(path_language_model)
  } else {
    dir.create(path_tall)
    dir.create(path_language_model)
  }

  #check if the file model already exists
  file_model <- dir(path_language_model,pattern=paste0(language,model_version))[1]

  if (is.na(file_model)){
    lang_file <- udpipe_download_model(language = language, udpipe_model_repo = model_repo, model_dir=path_language_model)
    udmodel_lang <- udpipe_load_model(file = lang_file$file_model)
  } else {
    udmodel_lang <- udpipe_load_model(file = paste0(path_language_model,"/",file_model))
  }

  return(udmodel_lang)
}

## Custom Lists merging
mergeCustomLists <- function(df,custom_lists){

  ## merge term custom lists
  df %>%
    mutate(lemma_original = lemma) %>%
    #mutate(lemma_norm = ifelse(upos!="PROPN", tolower(lemma), lemma)) %>%
    left_join(custom_lists, by = c("lemma" = "lemma")) %>%
    mutate(upos.x = ifelse(!is.na(upos.y),toupper(upos.y),upos.x),
           POSSelected = ifelse(upos.x %in% c("ADJ","NOUN","PROPN", "VERB"), TRUE, FALSE)) %>%
    select(-upos.y) %>%
    rename(upos = upos.x)

}


### MULTI-WORD CREATION ----

# rake function to create multi-words
rake <- function(x, group = "doc_id", ngram_max=5, relevant = c("PROPN", "NOUN", "ADJ", "VERB"), rake.min=2){

  if ("upos_original" %in% names(x)){
    x <- x %>%
      mutate(upos = upos_original) %>%
      select(-upos_original)
  }
  if ("lemma_original_nomultiwords" %in% names(x)){
    x <- x %>%
      mutate(lemma = lemma_original_nomultiwords) %>%
      select(-"lemma_original_nomultiwords")
  }
  # rake multi-word creation
  stats <- keywords_rake(x = x, term = "lemma", group = group, ngram_max = ngram_max,
                         relevant = x$upos %in% relevant)

  # identify ngrams>1 with reka index>reka.min
  stats <- stats %>%
    dplyr::filter(rake>=rake.min & ngram>1)

  # filter original token df removing POS excluded in reka
  x2 <- x %>% filter(upos %in% relevant)

  # combine lemmas into multi-words
  x2$multiword <- txt_recode_ngram(x2$lemma, compound=stats$keyword, ngram=stats$ngram, sep = " ")

  # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
  x2 <- x2 %>%
    mutate(upos_multiword = ifelse(lemma==multiword,upos,"MULTIWORD"),
           upos_multiword =ifelse(is.na(multiword), "NGRAM_MERGED",upos_multiword))

  # rebuild the original tokenized df
  x <- x %>%
    left_join(x2 %>% select(doc_id,term_id,multiword,upos_multiword), by = c("doc_id","term_id")) %>%
    mutate(multiword = ifelse(is.na(multiword),lemma,multiword),
           upos_multiword = ifelse(is.na(upos_multiword),upos,upos_multiword),
           POSSelected = ifelse(upos_multiword == "MULTIWORD", TRUE, POSSelected),
           POSSelected = ifelse(upos_multiword == "NGRAM_MERGED", FALSE, POSSelected)) %>%
    rename(upos_original = upos,
           upos = upos_multiword)

  names(x)[names(x) == "lemma"] <- "lemma_original_nomultiwords"
  names(x)[names(x) == "multiword"] <- "lemma"


  obj <- list(dfTag=x, multiwords=stats)

}


### POS TAG SELECTION ----

# create description for pos tag check box ----
posTagAll <- function(df){
  posLegend <- data.frame(pos=c('ADJ',
                                'ADP',
                                'ADV',
                                'AUX',
                                'CCONJ',
                                'DET',
                                'INTJ',
                                'NOUN',
                                'NUM',
                                'PART',
                                'PRON',
                                'PROPN',
                                'PUNCT',
                                'SCONJ',
                                'SYM',
                                'VERB',
                                'X'),
                          description=c('Adjective',
                                        'Adposition',
                                        'Adverb',
                                        'Auxiliary',
                                        'Coord. Conjunction',
                                        'Determiner',
                                        'Interjection',
                                        'Noun',
                                        'Numeral',
                                        'Particle',
                                        'Pronoun',
                                        'Proper Noun',
                                        'Punctuation',
                                        'Subord. Conjunction',
                                        'Symbol',
                                        'Verb',
                                        'Other'))

  pos <- unique(df$upos)
  additionalPos <- sort(setdiff(pos, posLegend$pos))
  ordinaryPos <- sort(pos[!pos %in% additionalPos])
  pos <- c(ordinaryPos,additionalPos)
  description <- c(posLegend$description[posLegend$pos %in% pos], rep("Custom PoS", length(additionalPos)))
  description <- paste(pos, description,sep=": ")
  obj <- data.frame(pos=pos, description=description)
  return(obj)
}


### OVERVIEW ----

# Term Frequency Distributions
freqByPos <- function(df, term="lemma", pos="NOUN"){
  obj <- df %>%
    dplyr::filter(upos %in% pos) %>%
    count(term=.[[term]]) %>%
    arrange(desc(n))
}

# freqPlotly ----
freqPlotly <- function(dfPlot,x,y,n=10, xlabel,ylabel, scale=c("identity", "log")){
  # function to build and plot plotly horizontal barplot
  dfPlot <- dfPlot %>% dplyr::slice_head(n=n)
  xmax <- max(dfPlot[[x]])

  switch(scale,
         log={
           #dfPlot$scale <- log(obj$n)
           dfPlot$n <- log(dfPlot$n)

         }
  )

  fig1 <- plot_ly(data=dfPlot , x = dfPlot[[x]], y = ~reorder(dfPlot[[y]], dfPlot[[x]]),
                  type = 'bar', orientation = 'h',
                  marker = list(color = 'rgba(79, 121, 66, 0.6)',
                                line = list(color = 'rgba(79, 121, 66, 1.0)', width = 1)))

  fig1 <- fig1 %>% layout(yaxis = list(title =ylabel, showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 1)),
                          xaxis = list(title = xlabel, zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = FALSE),
                          plot_bgcolor  = "rgba(0, 0, 0, 0)",
                          paper_bgcolor = "rgba(0, 0, 0, 0)")

  fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                   x = dfPlot[[x]] + xmax*0.015,  y = dfPlot[[y]],
                                   text = dfPlot[[x]],
                                   font = list(family = 'Arial', size = 12, color = 'rgb(79, 121, 66)'),
                                   showarrow = FALSE) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c(
             #'toImage',
             'sendDataToCloud',
             'pan2d',
             'select2d',
             'lasso2d',
             'toggleSpikelines',
             'hoverClosestCartesian',
             'hoverCompareCartesian'
           )) %>%
    event_register("plotly_selecting")

  fig1
}

# ValueBoxes Indices ----
valueBoxesIndices <- function(x){

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
  nSentences <- x %>% group_by(doc_id) %>%
    summarize(nSent = max(sentence_id)) %>%
    ungroup() %>% select(nSent) %>%
    summarize(nSent = sum(nSent)) %>%
    as.numeric()

  # 6. # avg document length
  avgDocLength <- x %>% group_by(doc_id) %>%
    select(doc_id,sentence) %>%
    summarize(nTokens = n(),
              nChars = nchar(paste(sentence, collapse=" "))) %>%
    ungroup() %>%
    summarize(avgChars = round(mean(nChars),0),
              avgTokens = round(mean(nTokens),0))

  # 7. # avg length sentence
  avgSentLength <- x %>% group_by(doc_id,sentence_id) %>%
    summarize(sentLength = n(),
              nChars = nchar(sentence)) %>%
    ungroup() %>%
    summarize(avgTokens = round(mean(sentLength),1),
              avgChars = round(mean(nChars),1))

  # 8. TTR: il rapporto tra la varietà del dizionario (Dictionary) e il numero totale di token in una raccolta testuale (# terms); in altre parole, misura la diversità lessicale in un corpus
  TTR = round(nDictionary/nTokens,2)

  # 9.  %hapax
  hapax <- x %>% group_by(token) %>%
    count() %>%
    filter(n==1) %>%
    ungroup() %>%
    summarize(n=sum(n)) %>%
    as.numeric() / nTokens *100

  # 10. Guiraud
  guiraud <- round(nDictionary/sqrt(nTokens),1)

  obj <- list(nDoc=nDoc,
              nTokens=nTokens,
              nDictionary=nDictionary,
              nLemmas=nLemmas,
              nSentences=nSentences,
              avgDocLengthChars=avgDocLength$avgChars,
              avgDocLengthTokens=avgDocLength$avgTokens,
              avgSentLengthTokens=avgSentLength$avgTokens,
              avgSentLengthChars=avgSentLength$avgChars,
              TTR=TTR,
              hapax=round(hapax,1),
              guiraud=guiraud
  )
}

### EXCEL REPORT FUNCTIONS ----
addDataWb <- function(list_df, wb, sheetname){
  l <- length(list_df)
  startRow <- 1
  for (i in 1:l){
    df <- list_df[[i]]
    n <- nrow(df)
    writeDataTable(wb, sheetname, df, startRow = startRow, startCol = 1, tableStyle = "TableStyleMedium20")
    startRow <- startRow + n + 3
  }
  return(wb)
}

addDataScreenWb <- function(list_df, wb, sheetname){
  ind <- which(regexpr(sheetname,wb$sheet_names)>-1)
  if (length(ind)>0){
    sheetname <- paste(sheetname,"(",length(ind)+1,")",sep="")
  }
  addWorksheet(wb=wb, sheetName=sheetname, gridLines = FALSE)
  if (!is.null(list_df)){
    addDataWb(list_df, wb, sheetname)
    col <- max(unlist(lapply(list_df,ncol))) + 2
  } else {
    col <- 1
  }

  results <- list(wb=wb,col=col, sheetname=sheetname)
  return(results)
}

addGgplotsWb <- function(list_plot, wb, sheetname, col, width=10, height=7, dpi=75){
  l <- length(list_plot)
  startRow <- 1
  for (i in 1:l){
    fileName <- tempfile(pattern = "figureImage",
                         fileext = ".png")
    if (inherits(list_plot[[i]], "ggplot")){
      ggsave(plot = list_plot[[i]], filename = fileName, width = width, height = height,
             units = "in", dpi = dpi)
    }
    if (inherits(list_plot[[i]], "igraph")){
      igraph2PNG(x = list_plot[[i]], filename = fileName, width = width, height = height, dpi=dpi)
    }
    insertImage(wb = wb, sheet = sheetname, file = fileName, width = width,
                height = height, startRow = startRow, startCol = col,
                units = "in", dpi = dpi)
    startRow <- startRow + (height*6)+1
  }
  return(wb)
}

screenSh <- function(selector){
  fileName <- tempfile(pattern = "figureImage",
                       tmpdir = "",
                       fileext = "") %>% substr(.,2,nchar(.))
  if (is.null(selector)){
    shinyscreenshot::screenshot(filename=fileName, download=FALSE, server_dir = tempdir())
  } else {
    shinyscreenshot::screenshot(selector=selector, filename=fileName, download=FALSE, server_dir = tempdir())
  }
  file <- paste(tempdir(),"/",fileName,".png",sep="")
  return(file)
}

addScreenWb <- function(df, wb, width=14, height=8, dpi=75){
  names(df) <- c("sheet","file","n")
  if (nrow(df)>0){
    sheet <- unique(df$sheet)
    for (i in 1:length(sheet)){
      sh <- sheet[i]
      df_sh <- df %>% dplyr::filter(.data$sheet==sh)
      l <- nrow(df_sh)
      startRow <- 1
      for (j in 1:l){
        fileName <- df_sh$file[j]
        insertImage(wb = wb, sheet = sh, file = fileName, width = width,
                    height = height, startRow = startRow, startCol = df_sh$n[j],
                    units = "in", dpi = dpi)
        startRow <- startRow + (height*10)+3
      }
    }
  }
  return(wb)
}

addSheetToReport <- function(list_df, list_plot, sheetname, wb, dpi=75){
  ind <- which(regexpr(sheetname,wb$sheet_names)>-1)
  if (length(ind)>0){
    sheetname <- paste(sheetname,"(",length(ind)+1,")",sep="")
  }
  addWorksheet(wb, sheetname, gridLines = FALSE)

  if (!is.null(list_df)) {
    col <- max(unlist(lapply(list_df,ncol))) + 2
    wb <- addDataWb(list_df, wb = wb, sheetname = sheetname)
  } else {col <- 1}

  if (!is.null(list_plot)){
    wb <- addGgplotsWb(list_plot, wb = wb, sheetname = sheetname, col = col, dpi = dpi)
  }
  #values$sheet_name <- sheetname
  return(wb)
}

short2long <- function(df, myC){
  z <- unlist(lapply(myC, function(x){
    y <- gsub(r"{\s*\([^\)]+\)}","",x)
    gsub(y,df$long[df$short==y],x)
  }))
  names(myC) <- z
  return(myC)
}








## Labels sheets Report
dfLabel <- function(){
  short <- c("Empty Report")#, "MainInfo",            "AnnualSciProd",       "AnnualCitPerYear",    "ThreeFieldsPlot",     "MostRelSources",      "MostLocCitSources",   "BradfordLaw",         "SourceLocImpact",


  long <- c("Empty Report")#, "Main Information", "Annual Scientific Production", "Annual Citation Per Year", "Three-Field Plot", "Most Relevant Sources","Most Local Cited Sources","Bradfords Law","Sources Local Impact",

  data.frame(short=short,long=long)
}

## Add to Report PopUp
popUp <- function(title=NULL, type="success", btn_labels="OK"){
  switch(type,
         success={
           title <- paste(title,"\n added to report",sep="")
           subtitle <- ""
           btn_colors = "#1d8fe1"
             showButton = TRUE
             timer = 3000
         },
         error={
           title <- "No results to add to the report "
           subtitle <- "Please Run the analysis and then Add it to the report"
           btn_colors = "#913333"
             showButton = TRUE
             timer = 3000
         },
         waiting={
           title <- "Please wait... "
           subtitle <- "Adding results to report"
           btn_colors = "#FFA800"
             showButton = FALSE
             btn_labels = NA
             timer = NA
         })

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


### UTILITY FUNCTIONS ----

## POS selection function ----
posSel <- function(df, pos){
  df <- df %>% mutate(POSSelected = ifelse(upos %in% pos, TRUE, FALSE))
  df <- highlight(df)
}

## Highlight function ----
highlight <- function(df){
  ## create highlighted tokens
  posUnsel <- c("PUNCT","X","SYM","NUM", "NGRAM_MERGED")
  df <- df %>%
    group_by(token) %>%
    mutate(token_hl = ifelse(!upos %in% posUnsel,
                             paste0("<mark><strong>", token, "</strong></mark>"), token)) %>%
    group_by(doc_id,sentence_id) %>%
    mutate(start_hl = start-(first(start)-1),
           end_hl = start_hl+(end-start)) %>%
    mutate(sentence_hl = ifelse(!upos %in% posUnsel,
                                paste0(substr(sentence,0,start_hl-1),token_hl,substr(sentence,end_hl+1,nchar(sentence))),
                                sentence)) %>% ungroup()
}

## saveTall function ----
saveTall <- function(dfTag,custom_lists,menu,where,file){
  D <- date()
  save(dfTag,custom_lists,menu,D,where, file=file)
}

###Export Tall analysis in .tall file ----

# Pre-processing - export function

# output$preProSave <- downloadHandler(
#   filename = function() {
#     paste("TallAnalysis-Export-File-", Sys.Date(), ".rdata" , sep="")
#   },
#   content <- function(file) {
#
#     tall_analysis <- list(df=values$txt)
#
#     save(tall_analysis, file=file)
#   }, contentType = "rdata"
# )


# SIDEBARMENU DYNAMIC ----
menuList <- function(menu){

  switch(as.character(menu),
         "0"={
           list(
             menuItem("Data", tabName = "data", icon = icon("open-file", lib="glyphicon"),
                      menuSubItem("Import texts", tabName = "import_tx", icon = icon("chevron-right")),
                      menuSubItem("Add metadata", tabName = "add_meta", icon = icon("chevron-right")),
                      menuSubItem("Filter text", tabName = "filter_text", icon = icon("chevron-right"))),
             menuItem("Pre-processing", tabName = "prePro", icon = icon("indent-right", lib="glyphicon"), startExpanded = TRUE,
                      menuSubItem("Tokenization & PoS Tagging", tabName = "tokPos",icon = icon("chevron-right"), selected = TRUE)
             )
           )
         },
         "1"={
           list(
             menuItem("Data", tabName = "data", icon = icon("open-file", lib="glyphicon"),
                      menuSubItem("Import texts", tabName = "import_tx", icon = icon("chevron-right")),
                      menuSubItem("Add metadata", tabName = "add_meta", icon = icon("chevron-right")),
                      menuSubItem("Filter text", tabName = "filter_text", icon = icon("chevron-right"))),
             menuItem("Pre-processing", tabName = "prePro", icon = icon("indent-right", lib="glyphicon"), startExpanded = TRUE,
                      menuSubItem("Tokenization & PoS Tagging", tabName = "tokPos",icon = icon("chevron-right")),
                      menuSubItem("Custom Term Lists", tabName = "custTermList",icon = icon("chevron-right"), selected = TRUE),
                      menuSubItem("Multi-Word Creation", tabName = "multiwordCreat",icon = icon("chevron-right")),
                      menuSubItem("PoS Tag Selection", tabName = "posTagSelect",icon = icon("chevron-right"))
                      )
           )
         },
         "2"={
           list(
             menuItem("Data", tabName = "data", icon = icon("open-file", lib="glyphicon"),
                      menuSubItem("Import texts", tabName = "import_tx", icon = icon("chevron-right")),
                      menuSubItem("Add metadata", tabName = "add_meta", icon = icon("chevron-right")),
                      menuSubItem("Filter text", tabName = "filter_text", icon = icon("chevron-right"))),
             menuItem("Pre-processing", tabName = "prePro", icon = icon("indent-right", lib="glyphicon"), startExpanded = TRUE,
                      menuSubItem("Tokenization & PoS Tagging", tabName = "tokPos",icon = icon("chevron-right")),
                      menuSubItem("Custom Term Lists", tabName = "custTermList",icon = icon("chevron-right")),
                      menuSubItem("Multi-Word Creation", tabName = "multiwordCreat",icon = icon("chevron-right")),
                      menuSubItem("PoS Tag Selection", tabName = "posTagSelect",icon = icon("chevron-right")), selected = TRUE),
             menuItem("Overview", tabName = "overview", icon = icon("search", lib="glyphicon")),
             menuItem("Words", tabName = "words", icon = icon("font", lib = "glyphicon"),
                      menuItem("Most Used Words", tabName = "freqList", icon = icon("chevron-right"),
                               menuSubItem("NOUN", tabName = "w_noun", icon = icon("chevron-right")),
                               menuSubItem("PROPN", tabName = "w_propn", icon = icon("chevron-right")),
                               menuSubItem("ADJ", tabName = "w_adj", icon = icon("chevron-right")),
                               menuSubItem("VERB", tabName = "w_verb", icon = icon("chevron-right")),
                               menuSubItem("OTHER", tabName = "w_other", icon = icon("chevron-right")),
                               menuSubItem("Part of Speech", tabName = "w_pos", icon = icon("chevron-right"))),
                      menuSubItem("Clustering", tabName = "w_clustering", icon = icon("chevron-right")),
                      menuSubItem("Correspondence Analysis", tabName = "ca", icon = icon("chevron-right")),
                      menuSubItem("Network", tabName = "w_network", icon = icon("chevron-right"))),
             menuItem("Documents",tabName = "documents", icon = icon(name="duplicate", lib="glyphicon"),
                      menuSubItem("Topic Modeling", tabName = "d_topicMod", icon = icon("chevron-right")),
                      menuSubItem("Clustering", tabName = "d_clustering", icon = icon("chevron-right")),
                      menuSubItem("Network", tabName = "d_network", icon = icon("chevron-right")),
                      menuSubItem("Summarization", tabName = "d_summarization", icon = icon("chevron-right")),
                      menuSubItem("Polarity Detection", tabName = "d_polDet", icon = icon("chevron-right"))),
             menuItem("Groups",tabName = "groups", icon = icon("th", lib="glyphicon"),
                      menuSubItem("Topic Modeling", tabName = "g_topicMod", icon = icon("chevron-right")),
                      menuSubItem("Clustering", tabName = "g_clustering", icon = icon("chevron-right")),
                      menuSubItem("Network", tabName = "g_network", icon = icon("chevron-right")),
                      menuSubItem("Summarization", tabName = "g_summarization", icon = icon("chevron-right")),
                      menuSubItem("Polarity Detection", tabName = "g_polDet", icon = icon("chevron-right"))),
             menuItem("Report",tabName = "report", icon = icon("list-alt")),
             menuItem("Settings",tabName = "settings", icon = icon("tasks"))
           )
         },
         {
           list(
             menuItem("Data", tabName = "data", icon = icon("open-file", lib="glyphicon"),
                      menuSubItem("Import texts", tabName = "import_tx", icon = icon("chevron-right")),
                      menuSubItem("Add metadata", tabName = "add_meta", icon = icon("chevron-right")),
                      menuSubItem("Filter text", tabName = "filter_text", icon = icon("chevron-right")))
           )
           }
  )
}

# DATA TABLE FORMAT ----
DTformat <- function(df, nrow=10, filename="Table", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, size='85%'){

  if (isTRUE(pagelength)){
    buttons = list(list(extend = 'pageLength'),
                   list(extend = 'excel',
                        filename = paste0(filename,"_tall_",Sys.Date()),
                        title = " ",
                        header = TRUE,
                        exportOptions = list(
                          modifier = list(page = "all")
                        )))
  } else{
    buttons = list(list(extend = 'excel',
                        filename = paste0(filename,"_tall_",Sys.Date()),
                        title = " ",
                        header = TRUE,
                        exportOptions = list(
                          modifier = list(page = "all")
                        )))
  }

  if (isTRUE(dom)){
    dom <- "Bfrtip"
  } else{
    dom <- "Bt"
  }

  tab <- DT::datatable(df,escape = FALSE,rownames = FALSE, extensions = c("Buttons"),
                options = list(
                  pageLength = nrow,
                  autoWidth = FALSE, scrollX = TRUE,
                  dom = dom,
                  buttons = buttons,
                  lengthMenu = list(c(10, 25, 50, -1),
                                    c('10 rows', '25 rows', '50 rows', 'Show all')),
                  columnDefs = list(list(
                    className = 'dt-center', targets = 0:(length(names(df)) - 1)
                  ))
                ),
                class = 'cell-border compact stripe'
  ) %>%
    DT::formatStyle(
      names(df),
      backgroundColor = 'white',
      textAlign = 'center',
      fontSize = size
    )

  ## left aligning

  if (!is_null(left)){
    tab <- tab %>%
      DT::formatStyle(
        names(df)[left],
        backgroundColor = 'white',
        textAlign = 'left',
        fontSize = size
      )
  }

  # right aligning
  if (!is_null(right)){
    tab <- tab %>%
      DT::formatStyle(
        names(df)[right],
        backgroundColor = 'white',
        textAlign = 'right',
        fontSize = size
      )
  }

  # numeric round
  if (!is_null(numeric)){
    tab <- tab %>%
      formatRound(names(df)[c(numeric)], digits=2)
  }
  tab
}


### FUNCTIONS FOR UI ----








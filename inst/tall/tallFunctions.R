
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

read_files <- function(files, ext=c("txt","csv", "xlsx", "pdf"), subfolder=TRUE, line_sep=","){

  #files <- list.files(path=path, pattern = paste0(".",ext,"$"), recursive = subfolder)
  if (is.null(files)) return(data.frame(doc_id=NA,text=NA, folder=NA))

  if ("datapath" %in% names(files)){
    doc_id <- files$name
    file <- files$datapath
    folder=NA
  }

  if (getFileNameExtension(file[1])=="zip"){
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    zip_file <-  unzip(file[1])
    zip_file <- zip_file[(substr(zip_file,nchar(zip_file)-nchar(ext)+1,nchar(zip_file))==ext)]
    file <- zip_file[regexpr("__MACOSX",zip_file)==-1]
    doc_id <- unlist(lapply(strsplit(file,"/"), function(l){l[length(l)]}))
    folder <- unlist(lapply(strsplit(file, "/"), function(l){l[length(l)-1]}))
  }

  switch(ext,
         txt={
           ## detect text encoding for each file
           df <- readtext(file)
           encod <- suppressMessages(encoding(df, verbose=FALSE)$all)
           ## read txt files using the right encoding
           df <- data.frame(doc_id=doc_id,text=NA, folder=folder, file=file, encod=encod) %>%
             group_by(doc_id) %>%
             mutate(text=readtext::readtext(file, encoding = encod, verbosity = 0)$text) %>%
             select(-c(file,encod))
         },
         csv={
           listdf <- list()
           for (i in seq_len(length(file))){
             # listdf[[i]] <- readtext::readtext(file[i], fill=TRUE, text_field="text", quote='"') %>%
             #   mutate(doc_id = doc_id[i])
             listdf[[i]] <- read_delim(file[i], delim=line_sep, quote='"') %>%
               mutate(filename = doc_id[i])
           }

           df <- do.call(rbind,listdf)
         },
         xlsx={
           listdf <- list()
           for (i in seq_len(length(file))){
             # listdf[[i]] <- readtext::readtext(file[i], fill=TRUE, text_field="text", quote='"') %>%
             #   mutate(doc_id = doc_id[i])
             listdf[[i]] <- readxl::read_excel(file[i], col_types = "text") %>%
               mutate(filename = doc_id[i])
           }
           df <- do.call(rbind,listdf)
         },
         pdf={
           listdf <- list()
           for (i in seq_len(length(file))){
             # listdf[[i]] <- readtext::readtext(file[i], fill=TRUE, text_field="text", quote='"') %>%
             #   mutate(doc_id = doc_id[i])
             listdf[[i]] <- data.frame(text=pdf2txt(file[i]), filename = doc_id[i])
           }
           df <- do.call(rbind,listdf)
         }
  )
  if ("doc_id" %in% names(df)){
    if (sum(duplicated(df$doc_id), na.rm=T)>0){
      num <- sprintf(paste0("%0",nchar(nrow(df)),"d"), 1:nrow(df))
      df <- df %>% mutate(original_doc_id = doc_id,
                          doc_id = paste0("doc_",num)) %>% select(doc_id, everything())
    }
  }else{
    num <- sprintf(paste0("%0",nchar(nrow(df)),"d"), 1:nrow(df))
    df <- df %>% mutate(doc_id = paste0("doc_",num)) %>% select(doc_id, everything())
  }

  df <- df %>%
    mutate(doc_selected = TRUE)

  return(df)
}

## pdf to txt ----
pdf2txt <- function(file){
  if (!poppler_config()$has_pdf_data){
    message("Pdf import feature requires a recent version of libpoppler. Please install it. ")
    return(NA)
  }

  # 1. Estrazione di tutto il testo

  pages <- pdftools::pdf_length(file)

  txt <- pdftools::pdf_text(file)

  # remove \n at the end of the rows
  txt <- gsub("(?<![\\s\\.])\\n(?!\\s)", " ", txt, perl = TRUE)

  # remove word sep -
  txt <- gsub("-\\s", "", txt)

  # replace \n and spaces with \n\n
  txt <- gsub("\n  ","\n\n",txt)

  txt <- paste(txt, collapse = " ")

  return(txt)

}

removeHTMLTags <- function(text){
  text <- text %>%
    gsub("&nbsp;|&amp;|&current;|&trade;", " ", .) %>%
    trimws() %>%
    gsub("<br>", "\\\n", .) %>%
    gsub("</p>", "\\\n", .) %>%
    gsub("<.*?>", "", .)
}

### download sample data
loadSampleCollection <- function(sampleName){
  switch(Sys.info()[['sysname']],
         Windows= {
           home <- Sys.getenv('R_USER')},
         Linux  = {
           home <- Sys.getenv('HOME')},
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

  switch(sampleName,
         bibliometrix={
           #check if the file model already exists
           file_lang <- dir(path_language_model,pattern="tall_bibliometrix.tall")[1]
           url <- paste0("https://www.bibliometrix.org/tall_lexicon/sampleData/tall_bibliometrix.tall")
           destfile <- paste0(path_language_model,"/tall_bibliometrix.tall")
           file <- paste0(path_language_model,"/tall_bibliometrix.tall")
         },
         bbc={
           file_lang <- dir(path_language_model,pattern="bbc.zip")[1]
           url <- paste0("https://www.bibliometrix.org/tall_lexicon/sampleData/bbc.zip")
           destfile <- paste0(path_language_model,"/bbc.zip")
           file <- paste0(path_language_model,"/bbc.zip")
         })


  if (is.na(file_lang)){
    switch(Sys.info()[['sysname']],
           Windows= {
             download.file(url = url, destfile = destfile, mode = "wb")},
           {
             download.file(url = url, destfile = destfile)
           })
  }

  return(file)
}

## Wikipedia download sample pages ----
wikiSearch = function(term, n = 10){

  if (!inherits(n,"numeric")) n = 10

  # select n of results
  search_url <- paste0("https://en.wikipedia.org/w/api.php?action=query&generator=search&gsrlimit=",n,"&prop=extracts&exintro&explaintext&exlimit=max&format=json&gsrsearch=")

  term = gsub("–", " ", gsub("\\s", "_", term))

  result = jsonlite::fromJSON(paste0(search_url,
                                     term))

  if (!"query" %in% names(result)){
    return(NULL)
  }

  title = c()
  abstract = c()
  url = c()

  for(i in 1:length(result$query$pages)){

    wikidata = result$query$pages[i]
    names(wikidata) = "content"
    page_abstract <- gsub("<.*?>", "", wikidata$content$extract)

    abstract = append(abstract, page_abstract)
    title = append(title, wikidata$content$title)
    url = append(url, paste0("https://en.wikipedia.org/wiki/",gsub("\\s","_",wikidata$content$title)))
  }
  url <-
    paste0(
      '<a href=\"',
      url,
      '\" target=\"_blank\">',
      url,
      '</a>'
    )

  df <- data.frame(title, abstract, url, text=NA, selected=TRUE)
  return(df)
}

wikiExtract <- function(df){

  items <- gsub("\\s","_",df$title)
  for (i in 1:length(items)){
    if (df$selected[i]){
      title <- items[i]
      #print(title)
      result = jsonlite::fromJSON(paste0("https://en.wikipedia.org/w/api.php?action=query&titles=", title, "&prop=extracts&redirects=&format=json"))
      names(result$query$pages) = "content"
      text <- result$query$pages$content$extract
      ## remove Latex equations and textstyle
      df$text[i] <- gsub("<.*?>", "", text) %>%
        gsub("\\{\\\\displaystyle.*?\\}\\n", "", .) %>%
        gsub("\\\\textstyle"," ",.)
    }
  }
  return(df)
}

### SPLIT TEXT INTO SUB-DOCS
splitDoc <- function(df, word){
  if (nchar(word)<=3){
    return(df)
  }
  df <- df %>% filter(doc_selected)
  df_splitted <- list()
  n <- length(unique(df$doc_id))
  for (i in seq_len(n)){
    testo <- df$text[i]
    testo <- unlist(strsplit(testo, word))
    df_splitted[[i]] <- testo[nchar(testo)>0]
  }
  # })
  doc_id_old <- rep(df$doc_id,lengths(df_splitted))

  df <- data.frame(doc_id=paste0("doc_",sprintf(paste0("%0",nchar(lengths(df_splitted)),"d"), 1:lengths(df_splitted))),
                   text = unlist(df_splitted),
                   doc_id_old=doc_id_old,
                   doc_selected=TRUE) %>%
    left_join(df %>%
                select(-c("doc_selected", "text", "text_original")),
              by = c("doc_id_old"="doc_id")
    ) %>%
    mutate("text_original" = text,
           "split_word" = word)
  return(df)
}

unsplitDoc <- function(df){
  if ("doc_id_old" %in% names(df)){
    word=df$split_word[1]
    df <- df %>%
      group_by(doc_id_old) %>%
      mutate(text = paste(text, collapse=word),
             doc_id=doc_id_old) %>%
      ungroup() %>%
      select(-c("doc_id_old","split_word")) %>%
      distinct(doc_id, .keep_all = TRUE)
  }
  return(df)
}

### TEXT SAMPLING ----
samplingText <- function(txt, n){
  id <- sample(txt$doc_id,n)
  txt$doc_selected <- (txt$doc_id %in% id)
  return(txt)
}


### EXTERNAL INFORMATION ----
loadExtInfo <- function(file, txt){
  df <- readxl::read_excel(file)

  txt <- txt %>%
    left_join(df, by = "doc_id")

  return(txt)
}

### PRE_PROCESSING ----

# ## 0. NORMALIZATION
# normalizationOptions <- function(){
#   item <- c(
#     ## Web and social corpus
#     "url",        # website or ftp sites
#     "hash",       # hastags
#     "youtube_id", # youtube ids
#     "emoticon",   # emoticons
#     "tag",        # user tags in posts like: @massimoaria
#     "ip_address", # if addresses
#     ## ordinary corpus
#     "email",      # email
#     #"endmark",    # endmark punctuation
#     #"extraspaces",# two o more spaces
#     "non_ascii",  # non ascii chars
#     "percent",    # percentage symbols %
#     #"number",     # numbers
#     "time2",       # time (time2 pattern)
#     "date",       # date
#     "zip",        # postal code
#     "pages",      # remove pp. and page numbers
#     "citation"    # bibliographic citations
#   )
#   label <- c(
#     ## Web and social corpus
#     "URLs",        # website or ftp sites
#     "Hashtags #",    # hashtags
#     "YouTube IDs", # youtube ids
#     "Emoji",      # emoticons
#     "Tags @",        # user tags in posts like: @massimoaria
#     "IP Addresses", # if addresses
#     ## ordinary corpus
#     "E-mails",      # email
#     #"Endmarks",    # endmark punctuation
#     #"Extra spaces",# two o more spaces
#     "Non ASCII chars",  # non ascii chars
#     "Percent symbol %",    # percentage symbols %
#     #"Numbers",     # numbers
#     "Time",       # time (time2 pattern)
#     "Dates",       # date
#     "Zip postal codes",        # postal code
#     "Page abbrev.",      # remove pp. and page numbers
#     "Bibliographic Citations"    # bibliographic citations
#   )
#
#   # id <- c(1,2,3,4,7,5,6,16,15,14,13,8,9,10,11,12)
#   #id <- c(1,2,3,4,7,5,6,14,13,12,8,9,10,11)
#   id <- c(2,3,4,5,7,6,1,14,13,12,8,9,10,11)
#
#   what <- data.frame(label,item,id)
#   return(what)
# }
#
# extractCorpusElements <- function(x,
#                                   regex_list=regex_list){
#
#   resList <- list()
#   what <- normalizationOptions() %>% arrange(id)
#
#   for (i in 1:nrow(what)){
#     item <- what$item[i]
#
#     results <- stringi::stri_extract_all_regex(x$text, regex_list[[item]])
#     resList[[i]] <- data.frame(doc_id=rep(x$doc_id,lengths(results)), item=unlist(results, recursive = F), tag=what$label[i])
#   }
#
#   resList <- bind_rows(resList) %>%
#     filter(!is.na(item))
#
#   return(resList)
# }
#
# summaryCorpusElements <- function(CorpusElements, type="all"){
#   what <- normalizationOptions()
#   if (!type %in% c("all",what$item)) type <- "all"
#
#   switch(type,
#          "all"={
#            CorpusElements %>%
#              group_by(tag) %>%
#              summarise(items = length(unique(item)),
#                        docs = length(unique(doc_id))
#              ) %>%
#              rename(Tag = tag,
#                     "N. of Items" = items,
#                     "N. of Docs" = docs)
#          },
#          {
#            label <- what$label[what$item==type]
#            CorpusElements %>%
#              filter(tag == label) %>%
#              count(item) %>%
#              arrange(desc(n))
#          })
# }
#
# applyNormalization <- function(x,textNormWebList,textNormCorpusList, regex_list){
#   items <- c(textNormWebList,textNormCorpusList)
#   if (length(items)>0){
#     what <- normalizationOptions()
#     what <- what %>%
#       filter(label %in% items) %>%
#       arrange(id)
#
#     for (i in 1:nrow(what)){
#       x <- removeCorpusElements(x,
#                                 what = what$item[i],
#                                 replaceElement = "",
#                                 regex_list = regex_list)
#     }
#     x <- x %>%
#       mutate(text = trimws(text))
#   } else {
#     x <- restoreText(x)
#   }
#   return(x)
# }
#
# removeCorpusElements <- function(x,
#                                  what,
#                                  replaceElement="",
#                                  regex_list=regex_list){
#   if (length(what)!=1){
#     message("Please provide a valid pattern name.")
#     return(NA)
#   }
#
#   if (what == "extraspaces"){
#     x <- x %>%
#       mutate(text = gsub("\\s+"," ",text))
#   } else if (what == "citation"){
#     x$text <- stringi::stri_replace_all_regex(x$text, regex_list[[what]], replaceElement)
#     x$text <- stringi::stri_replace_all_fixed(x$text,paste0("(",replaceElement,")"),"")
#   }
#   else {
#     x$text <- stringi::stri_replace_all_regex(x$text, regex_list[[what]], replaceElement)
#   }
#   return(x)
# }

restoreText <- function(x){
  x <- x %>%
    mutate(text = text_original)
}

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

## Tagging Special Entites ----
TaggingCorpusElements <- function(x){

  if ("upos_specialentities" %in% names(x)){
    x <- resetSpecialEntities(x)
  } else {
    x$upos_specialentities <- x$upos
  }

  regexList <- c(
    EMAIL="(?i)([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))",
    #url="(https?://)?(www\\.)?([\\w.-]+\\.[a-z]{2,})(/[\\w\\-./?=&%]*)?",
    URL="(?<!@)\\b(https?://[\\w.-]+\\.[a-z]{2,6}(/[\\S]*)?|[\\w.-]+\\.(com|org|net|edu|gov|it|uk)\\b)",
    #URL="\\b(https?://[\\w.-]+\\.[a-z]{2,6}(/\\S*)?|[\\w.-]+\\.(com|org|net|edu|gov|it|uk)\\b)",
    HASH="^#",
    #emoji="([:;=8X][-~^]?[()\\[\\]{}|/\\\\DpP3><]+|[<>]?[:;=8xX][-~^o]?\\)+|<3|</3|[xX][-~^]?[DdPpOo]+|[\\p{So}\\p{Sk}\\p{Emoji_Presentation}])",
    EMOJI="(?<!\\w)([:;=8][-o*']?[:()DPp3]|<3|[\\p{So}\\p{Sk}]+)(?!\\w)",
    IP_ADDRESS="\\b\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\b",
    MENTION="^@"
  )
  items <- names(regexList)

  resList <- list()
  j <- 0

  for (i in 1:length(items)){
    item <- items[i]
    results <- stringi::stri_detect_regex(x$token, regexList[[item]])
    if (sum(results)>0){
      j <- j+1
      resList[[j]] <- data.frame(doc_id=x$doc_id[results], item = x$token[results], tag=item)
      x$upos[results] <- toupper(item)
      x$POSSelected[results] <- FALSE
    }
  }

  if (length(resList)>0){
    resList <- bind_rows(resList) %>%
      filter(!is.na(item))
  } else {
    resList <- tibble(doc_id=0, item=NA, tag="email") %>% filter(!is.na(item))
  }

  return(list(resList=resList,x=x))
}

resetSpecialEntities <- function(x){
  if ("upos_specialentities" %in% names(x)){
    items <- toupper(c("email", "url", "hash", "emoji", "ip_address", "mention"))
    x <- x %>%
      mutate(upos = ifelse(upos %in% items,upos_specialentities,upos))
  } else {
    x$upos_specialentities <- x$upos
  }
  return(x)
}

summarySpecialEntities <- function(resList, type="all"){

  data.frame(UPOS = toupper(c("email", "url", "hash", "emoji", "ip_address", "mention")), "N. of Items"=rep(0,6), "N. of Docs"=rep(0,6))

  switch(type,
         "all"={
           resList %>%
             group_by(tag) %>%
             summarise(items = length(unique(item))) %>%
             rename(UPOS = tag,
                    "Frequency" = items) %>%
             ungroup() %>%
             bind_rows(tibble(UPOS = toupper(c("email", "url", "hash", "emoji", "ip_address", "mention")),
                              "Frequency"=rep(0,6))) %>%
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
         })
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
rakeReset <- function(x){
  if ("upos_original" %in% names(x)){
    x <- x %>%
      select(-"upos") %>%
      rename(upos = upos_original)
  }

  if ("lemma_original_nomultiwords" %in% names(x)){
    x <- x %>%
      select(-"lemma") %>%
      rename(lemma = lemma_original_nomultiwords)
  }
  if ("token_original_nomultiwords" %in% names(x)){
    x <- x %>%
      select(-"token") %>%
      rename(token = token_original_nomultiwords)
  }

  if ("ngram" %in% names(x)){
    x <- x %>%
      select(-"ngram")
  }
  return(x)
}

rake <- function(x, group = "doc_id", ngram_max=5, ngram_min=2,relevant = c("PROPN", "NOUN", "ADJ", "VERB"), rake.min=2, freq.min=10, term="lemma", type="automatic", keywordList=NULL){

  if ("ngram" %in% names(x)){
    x <- x %>%
      select(-"ngram")
  }
  switch(type,
         automatic={
           # rake multi-word creation
           stats <- keywords_rake(x = x, term = term, group = group, ngram_max = ngram_max,
                                  relevant = x$upos %in% relevant)

           # identify ngrams>1 with reka index>reka.min
           stats <- stats %>%
             dplyr::filter(rake>=rake.min & ngram>=ngram_min)
         },
         {
           stats <- keywordList %>%
             mutate(keyword = trimws(keyword),
                    ngram = lengths(strsplit(keyword," ")))
         })


  # filter original token df removing POS excluded in rake
  x2 <- x %>% filter(upos %in% relevant)

  # combine lemmas or tokens into multi-words

  switch(term,
         lemma={
           x2$multiword <- txt_recode_ngram(x2$lemma, compound=stats$keyword, ngram=stats$ngram, sep = " ")

           # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
           x2 <- x2 %>%
             mutate(upos_multiword = ifelse(lemma==multiword,upos,"MULTIWORD"),
                    upos_multiword =ifelse(is.na(multiword), "NGRAM_MERGED",upos_multiword)) %>%
             left_join(stats %>% select(keyword, ngram), by = c("multiword" = "keyword"))

           # rebuild the original tokenized df
           x <- x %>%
             left_join(x2 %>% select(doc_id,term_id,multiword,upos_multiword, ngram), by = c("doc_id","term_id")) %>%
             mutate(multiword = ifelse(is.na(multiword),lemma,multiword),
                    upos_multiword = ifelse(is.na(upos_multiword),upos,upos_multiword),
                    POSSelected = ifelse(upos_multiword == "MULTIWORD", TRUE, POSSelected),
                    POSSelected = ifelse(upos_multiword == "NGRAM_MERGED", FALSE, POSSelected))

           if (!"upos_original" %in% names(x)) names(x)[names(x) == "upos"] <- "upos_original"
           x <- x %>% select(-ends_with("upos")) %>%
             rename(upos = upos_multiword)

           if (!"lemma_original_nomultiwords" %in% names(x)) names(x)[names(x) == "lemma"] <- "lemma_original_nomultiwords"

           x <- x %>%
             select(-ends_with("lemma")) %>%
             rename(lemma = multiword)


           # names(x)[names(x) == "lemma"] <- "lemma_original_nomultiwords"
           # names(x)[names(x) == "multiword"] <- "lemma"

           stats<- x %>%
             filter(upos == "MULTIWORD", lemma %in% stats$keyword) %>%
             group_by(lemma) %>%
             select(lemma) %>%
             count() %>%
             ungroup() %>%
             rename(keyword = lemma,
                    freq = n) %>%
             right_join(stats %>%
                          select(-starts_with("freq")),
                        by="keyword") %>%
             filter(freq>=freq.min)
         },
         token={
           x2$multiword <- txt_recode_ngram(x2$token, compound=stats$keyword, ngram=stats$ngram, sep = " ")

           # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
           x2 <- x2 %>%
             mutate(upos_multiword = ifelse(token==multiword,upos,"MULTIWORD"),
                    upos_multiword =ifelse(is.na(multiword), "NGRAM_MERGED",upos_multiword)) %>%
             left_join(stats %>% select(keyword, ngram), by = c("multiword" = "keyword"))

           # rebuild the original tokenized df
           x <- x %>%
             left_join(x2 %>% select(doc_id,term_id,multiword,upos_multiword, ngram), by = c("doc_id","term_id")) %>%
             mutate(multiword = ifelse(is.na(multiword),token,multiword),
                    upos_multiword = ifelse(is.na(upos_multiword),upos,upos_multiword),
                    POSSelected = ifelse(upos_multiword == "MULTIWORD", TRUE, POSSelected),
                    POSSelected = ifelse(upos_multiword == "NGRAM_MERGED", FALSE, POSSelected))

           if (!"upos_original" %in% names(x)) names(x)[names(x) == "upos"] <- "upos_original"
           x <- x %>% select(-ends_with("upos")) %>%
             rename(upos = upos_multiword)

           if (!"token_original_nomultiwords" %in% names(x)) names(x)[names(x) == "token"] <- "token_original_nomultiwords"

           x <- x %>%
             select(-ends_with("token")) %>%
             rename(token = multiword)
           # names(x)[names(x) == "token"] <- "token_original_nomultiwords"
           # names(x)[names(x) == "multiword"] <- "token"

           stats<- x %>%
             filter(upos == "MULTIWORD", token %in% stats$keyword) %>%
             group_by(token) %>%
             select(token) %>%
             count() %>%
             ungroup() %>%
             rename(keyword = token,
                    freq = n) %>%
             right_join(stats %>%
                          select(-starts_with("freq")),
                        by="keyword") %>%
             filter(freq>=freq.min)
         })

  return(stats %>% arrange(desc(freq)))

}

applyRake <- function(x, stats, relevant = c("PROPN", "NOUN", "ADJ", "VERB"), term="lemma"){

  if ("ngram" %in% names(x)){
    x <- x %>%
      select(-"ngram")
  }
  # filter original token df removing POS excluded in rake
  x2 <- x %>% filter(upos %in% relevant)

  # combine lemmas or tokens into multi-words

  switch(term,
         lemma={
           x2$multiword <- txt_recode_ngram(x2$lemma, compound=stats$keyword, ngram=stats$ngram, sep = " ")

           # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
           x2 <- x2 %>%
             mutate(upos_multiword = ifelse(lemma==multiword,upos,"MULTIWORD"),
                    upos_multiword =ifelse(is.na(multiword), "NGRAM_MERGED",upos_multiword)) %>%
             left_join(stats %>% select(keyword, ngram), by = c("multiword" = "keyword"))

           # rebuild the original tokenized df
           x <- x %>%
             left_join(x2 %>% select(doc_id,term_id,multiword,upos_multiword, ngram), by = c("doc_id","term_id")) %>%
             mutate(multiword = ifelse(is.na(multiword),lemma,multiword),
                    upos_multiword = ifelse(is.na(upos_multiword),upos,upos_multiword),
                    POSSelected = ifelse(upos_multiword == "MULTIWORD", TRUE, POSSelected),
                    POSSelected = ifelse(upos_multiword == "NGRAM_MERGED", FALSE, POSSelected))

           if (!"upos_original" %in% names(x)) names(x)[names(x) == "upos"] <- "upos_original"
           x <- x %>% select(-ends_with("upos")) %>%
             rename(upos = upos_multiword)

           if (!"lemma_original_nomultiwords" %in% names(x)) names(x)[names(x) == "lemma"] <- "lemma_original_nomultiwords"

           x <- x %>%
             select(-ends_with("lemma")) %>%
             rename(lemma = multiword)
         },
         token={
           x2$multiword <- txt_recode_ngram(x2$token, compound=stats$keyword, ngram=stats$ngram, sep = " ")

           # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
           x2 <- x2 %>%
             mutate(upos_multiword = ifelse(token==multiword,upos,"MULTIWORD"),
                    upos_multiword =ifelse(is.na(multiword), "NGRAM_MERGED",upos_multiword)) %>%
             left_join(stats %>% select(keyword, ngram), by = c("multiword" = "keyword"))

           # rebuild the original tokenized df
           x <- x %>%
             left_join(x2 %>% select(doc_id,term_id,multiword,upos_multiword, ngram), by = c("doc_id","term_id")) %>%
             mutate(multiword = ifelse(is.na(multiword),token,multiword),
                    upos_multiword = ifelse(is.na(upos_multiword),upos,upos_multiword),
                    POSSelected = ifelse(upos_multiword == "MULTIWORD", TRUE, POSSelected),
                    POSSelected = ifelse(upos_multiword == "NGRAM_MERGED", FALSE, POSSelected))

           if (!"upos_original" %in% names(x)) names(x)[names(x) == "upos"] <- "upos_original"
           x <- x %>% select(-ends_with("upos")) %>%
             rename(upos = upos_multiword)

           if (!"token_original_nomultiwords" %in% names(x)) names(x)[names(x) == "token"] <- "token_original_nomultiwords"

           x <- x %>%
             select(-ends_with("token")) %>%
             rename(token = multiword)
         })

  ## calculate new start end values for multiwords
  ind <- which(!is.na(x$ngram))
  ind2 <- ind+(x$ngram[ind]-1)
  x$end[ind] <- x$end[ind2]

  # calculate ngram
  x <- x %>%
    mutate(id=row_number()) %>%
    group_by(id) %>%
    mutate(ngram=ifelse(upos=="MULTIWORD", max(c(lengths(strsplit(lemma," "))),lengths(strsplit(token," "))), NA)) %>%
    ungroup() %>%
    select(-id)

  obj <- list(dfTag=x, multiwords=stats)

}

# rake <- function(x, group = "doc_id", ngram_max=5, ngram_min=2,relevant = c("PROPN", "NOUN", "ADJ", "VERB"), rake.min=2, term="lemma", type="automatic", keywordList=NULL){
#
#   if ("ngram" %in% names(x)){
#     x <- x %>%
#       select(-"ngram")
#   }
#   switch(type,
#          automatic={
#            # rake multi-word creation
#            stats <- keywords_rake(x = x, term = term, group = group, ngram_max = ngram_max,
#                                   relevant = x$upos %in% relevant)
#
#            # identify ngrams>1 with reka index>reka.min
#            stats <- stats %>%
#              dplyr::filter(rake>=rake.min & ngram>=ngram_min)
#          },
#          {
#            stats <- keywordList %>%
#              mutate(keyword = trimws(keyword),
#                     ngram = lengths(strsplit(keyword," ")))
#          })
#
#
#   # filter original token df removing POS excluded in rake
#   x2 <- x %>% filter(upos %in% relevant)
#
#   # combine lemmas or tokens into multi-words
#
#   switch(term,
#          lemma={
#            x2$multiword <- txt_recode_ngram(x2$lemma, compound=stats$keyword, ngram=stats$ngram, sep = " ")
#
#            # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
#            x2 <- x2 %>%
#              mutate(upos_multiword = ifelse(lemma==multiword,upos,"MULTIWORD"),
#                     upos_multiword =ifelse(is.na(multiword), "NGRAM_MERGED",upos_multiword)) %>%
#              left_join(stats %>% select(keyword, ngram), by = c("multiword" = "keyword"))
#
#            # rebuild the original tokenized df
#            x <- x %>%
#              left_join(x2 %>% select(doc_id,term_id,multiword,upos_multiword, ngram), by = c("doc_id","term_id")) %>%
#              mutate(multiword = ifelse(is.na(multiword),lemma,multiword),
#                     upos_multiword = ifelse(is.na(upos_multiword),upos,upos_multiword),
#                     POSSelected = ifelse(upos_multiword == "MULTIWORD", TRUE, POSSelected),
#                     POSSelected = ifelse(upos_multiword == "NGRAM_MERGED", FALSE, POSSelected))
#
#            if (!"upos_original" %in% names(x)) names(x)[names(x) == "upos"] <- "upos_original"
#              x <- x %>% select(-ends_with("upos")) %>%
#              rename(upos = upos_multiword)
#
#            if (!"lemma_original_nomultiwords" %in% names(x)) names(x)[names(x) == "lemma"] <- "lemma_original_nomultiwords"
#
#            x <- x %>%
#              select(-ends_with("lemma")) %>%
#              rename(lemma = multiword)
#
#
#            # names(x)[names(x) == "lemma"] <- "lemma_original_nomultiwords"
#            # names(x)[names(x) == "multiword"] <- "lemma"
#
#            stats<- x %>%
#              filter(upos == "MULTIWORD", lemma %in% stats$keyword) %>%
#              group_by(lemma) %>%
#              select(lemma) %>%
#              count() %>%
#              ungroup() %>%
#              rename(keyword = lemma,
#                     freq = n) %>%
#              right_join(stats %>%
#                           select(-starts_with("freq")),
#                         by="keyword")
#          },
#          token={
#            x2$multiword <- txt_recode_ngram(x2$token, compound=stats$keyword, ngram=stats$ngram, sep = " ")
#
#            # assign new POS tags "MULTIWORD" for combined lemmas and "NGRAM_MERGED" for lemmas to be removed because combined
#            x2 <- x2 %>%
#              mutate(upos_multiword = ifelse(token==multiword,upos,"MULTIWORD"),
#                     upos_multiword =ifelse(is.na(multiword), "NGRAM_MERGED",upos_multiword)) %>%
#              left_join(stats %>% select(keyword, ngram), by = c("multiword" = "keyword"))
#
#            # rebuild the original tokenized df
#            x <- x %>%
#              left_join(x2 %>% select(doc_id,term_id,multiword,upos_multiword, ngram), by = c("doc_id","term_id")) %>%
#              mutate(multiword = ifelse(is.na(multiword),token,multiword),
#                     upos_multiword = ifelse(is.na(upos_multiword),upos,upos_multiword),
#                     POSSelected = ifelse(upos_multiword == "MULTIWORD", TRUE, POSSelected),
#                     POSSelected = ifelse(upos_multiword == "NGRAM_MERGED", FALSE, POSSelected))
#
#            if (!"upos_original" %in% names(x)) names(x)[names(x) == "upos"] <- "upos_original"
#            x <- x %>% select(-ends_with("upos")) %>%
#              rename(upos = upos_multiword)
#
#            if (!"token_original_nomultiwords" %in% names(x)) names(x)[names(x) == "token"] <- "token_original_nomultiwords"
#
#            x <- x %>%
#              select(-ends_with("token")) %>%
#              rename(token = multiword)
#            # names(x)[names(x) == "token"] <- "token_original_nomultiwords"
#            # names(x)[names(x) == "multiword"] <- "token"
#
#            stats<- x %>%
#              filter(upos == "MULTIWORD", token %in% stats$keyword) %>%
#              group_by(token) %>%
#              select(token) %>%
#              count() %>%
#              ungroup() %>%
#              rename(keyword = token,
#                     freq = n) %>%
#              right_join(stats %>%
#                           select(-starts_with("freq")),
#                         by="keyword")
#          })
#
#
#
#   ## calculate new start end values for multiwords
#   ind <- which(!is.na(x$ngram))
#   ind2 <- ind+(x$ngram[ind]-1)
#   x$end[ind] <- x$end[ind2]
#
#   # calculate ngram
#   x <- x %>%
#     mutate(id=row_number()) %>%
#     group_by(id) %>%
#     mutate(ngram=ifelse(upos=="MULTIWORD", max(c(lengths(strsplit(lemma," "))),lengths(strsplit(token," "))), NA)) %>%
#     ungroup() %>%
#     select(-id)
#
#   obj <- list(dfTag=x, multiwords=stats)
#
# }


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
  obj <- data.frame(pos=pos, description=description) %>%
    filter(!pos=="NGRAM_MERGED" )
  return(obj)
}

### GROUP MENU FUNCTIONS -----
noGroupLabels <- function(label){
  setdiff(label, c("doc_id","paragraph_id","sentence_id","sentence","start","end","term_id", "noSingleChar",
                   "token_id","token","lemma","upos","xpos","feats","head_token_id","dep_rel",
                   "deps","misc","original_doc_id","ungroupDoc_id","ungroupP_id", "ungroupS_id",
                   "POSSelected","token_hl","start_hl","end_hl","sentence_hl","lemma_original_nomultiwords",
                   "filename", "upos_original", "folder", "docSelected", "ngram","doc_selected", "noHapax",
                   "FrequencyRange","text_original", "doc_id_old", "split_word", "token_original_nomultiwords",
                   "lemma_original", "upos_specialentities")
  )
}

groupByMetadata <- function(dfTag, metadata){
  if (length(metadata)==1){
    ## group texts by a metadata

    if (!"ungroupDoc_id" %in% names(dfTag)){
      dfTag <- dfTag %>%
        mutate(ungroupDoc_id = doc_id,
               ungroupP_id = paragraph_id,
               ungroupS_id = sentence_id)
    }

    #newDoc_id <- sprintf(paste0("%0",nchar(length(unique(dfTag$ungroupDoc_id))),"d"), unique_identifier(dfTag, fields=metadata, start_from = 1L))
    dfTag$paragraph_id <- paste0(dfTag$ungroupDoc_id,"_",dfTag$ungroupP_id)
    dfTag$sentence_id <- paste0(dfTag$ungroupDoc_id,"_",dfTag$ungroupS_id)

    #newDoc_id <-dfTag[[metadata]]
    newDoc_id <- ifelse(!is.na(dfTag[[metadata]]),dfTag[[metadata]],"Not Available")

    dfTag <- dfTag %>%
      mutate(doc_id = newDoc_id) %>%
      group_by(doc_id) %>%
      mutate(paragraph_id = unique_identifier(paragraph_id),
             sentence_id = unique_identifier(sentence_id)) %>%
      ungroup() %>%
      arrange(doc_id,paragraph_id,sentence_id)
  } else {
    dfTag <- backToOriginalGroups(dfTag)
  }

  return(dfTag)
}

backToOriginalGroups <- function(dfTag){
  # back to original ungrouped data frame
  if ("ungroupDoc_id" %in% names(dfTag)){
    dfTag <- dfTag %>%
      mutate(doc_id = ungroupDoc_id,
             paragraph_id = ungroupP_id,
             sentence_id = ungroupS_id) %>%
      select(-ungroupDoc_id,-ungroupP_id,-ungroupS_id) %>%
      arrange(doc_id,paragraph_id,sentence_id)
  }
  return(dfTag)
}



### OVERVIEW ----

# Term Frequency Distributions
freqByPos <- function(df, term="lemma", pos="NOUN"){
  obj <- df %>%
    dplyr::filter(upos %in% pos) %>%
    count(term=.[[term]]) %>%
    arrange(desc(n))

  if(pos=="URL"){
    obj$term <- paste0(
      '<a href=\"',
      obj$term,
      '\" target=\"_blank\">',
      obj$term,
      '</a>'
    )
  }
  return(obj)
}

# freqPlotly ----
freqPlotly <- function(dfPlot,x,y,n=10, xlabel,ylabel, scale=c("identity", "log"), topicmodel=FALSE, color="#4F7942", decimal=0){
  # function to build and plot plotly horizontal barplot
  dfPlot <- dfPlot %>% dplyr::slice_head(n=n)
  xmax <- max(dfPlot[[x]])

  switch(scale,
         log={
           dfPlot$n <- log(dfPlot$n)

         }
  )

  if (isTRUE(topicmodel)){
    fig1 <- plot_ly(data=dfPlot , x = round(dfPlot[[x]], decimal), y = dfPlot[[y]],
                    source = "A",
                    type = 'bar', orientation = 'h',
                    marker = list(color = paste0(color,"60"),
                                  line = list(color = color, width = 1)),
                    hovertemplate = "<b>Word</b>: <i>%{y}</i> <br><b><i>Value: %{x}</i></b><extra></extra>")
  } else {
    fig1 <- plot_ly(data=dfPlot , x = dfPlot[[x]], y = ~reorder(dfPlot[[y]], dfPlot[[x]]),
                    source = "A",
                    type = 'bar', orientation = 'h',
                    marker = list(color = paste0(color,"60"),
                                  line = list(color = color, width = 1)),
                    hovertemplate = "<b><i>Word: %{y}</i></b> <br> <b><i>Value: %{x}</i></b><extra></extra>")
  }

  fig1 <- fig1 %>% layout(yaxis = list(title =ylabel, showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 1)),
                          xaxis = list(title = xlabel, zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = FALSE),
                          plot_bgcolor  = "rgba(0, 0, 0, 0)",
                          paper_bgcolor = "rgba(0, 0, 0, 0)")

  if (decimal>0) {
    ann_text <- format(round(dfPlot[[x]], decimal), nsmall = decimal)
  } else {ann_text <- dfPlot[[x]]}

  fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                   x = dfPlot[[x]] + xmax*0.015,  y = dfPlot[[y]],
                                   text = ann_text,
                                   font = list(family = 'Arial', size = 12, color = color),
                                   showarrow = FALSE) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c(
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
  TTR = round(nDictionary/nTokens*100,2)

  # 9.  %hapax
  hapax <- x %>% group_by(token) %>%
    count() %>%
    filter(n==1) %>%
    ungroup() %>%
    summarize(n=sum(n)) %>%
    as.numeric() / nDictionary *100

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

## wordcloud2vis

wordcloud2vis <- function(nodes, labelsize=7, opacity=1){

  nodes <- nodes %>%
    mutate(id = row_number())
  # size scaling
  scalemin <- 20*(1+labelsize/5)
  scalemax <- 100*(1+labelsize/5)
  N <- nrow(nodes)


  colorlists <- colorlist()
  colorlists <- sample(colorlists,N, replace=TRUE)

  opacity.min <- 0.6
  shape <- "text"
  layout <- "layout_nicely"

  nodes <- nodes %>%
    mutate(font.color=colorlists,
           id = row_number(),
           shape=shape,
           color = colorlists,
           title = paste("<strong>",label,"</strong>","<br><h5>freq = ",value,"</h5>", sep=""))

  nodes$font.size <- log(nodes$value)
  Min <- min(nodes$font.size)
  Max <- max(nodes$font.size)
  if (Max>Min){
    size <- (nodes$font.size-Min)/(Max-Min)*15*labelsize+10
  } else {size=10*labelsize}
  size[size<scalemin]=scalemin
  size[size>scalemax]=scalemax
  nodes$font.size <- size

  if (shape %in% c("dot","square")){
    nodes$font.vadjust <- -0.7*nodes$font.size
  }else{
    nodes$font.vadjust <-0
  }

  ## opacity for label
  opacity_font <- sqrt((nodes$font.size-min(nodes$font.size))/diff(range(nodes$font.size)))*opacity+opacity.min+0.1
  if(is.nan(opacity_font[1])) opacity_font <- rep(opacity.min,length(opacity_font))


  # node colors
  nodes$opacity.nodes <- (opacity_font-min(opacity_font))/(diff(range(opacity_font)))*0.5+opacity.min
  nodes$opacity.nodes[is.nan(nodes$opacity.nodes)] <- 0.5


  VIS <-
    visNetwork::visNetwork(nodes = nodes, edges = NULL, type="full", smooth=TRUE, physics=TRUE) %>%
    visNetwork::visNodes(shadow=FALSE, shape=nodes$shape, font=list(color=nodes$font.color, size=nodes$font.size,vadjust=nodes$font.vadjust)) %>%
    # visPhysics(solver = "barnesHut", barnesHut=list(
    #   gravitationalConstant=-2000,
    #   avoidOverlap=1
    # )) %>%
    visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
    visNetwork::visInteraction(dragNodes = TRUE, navigationButtons = F, hideEdgesOnDrag = TRUE, zoomSpeed = 0.2) %>%
    visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}"
    ) %>%
    visNetwork::visOptions(manipulation = FALSE, height ="100%", width = "100%")
  return(VIS)
}

## wordcloud function
wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI",
                         fontWeight = "bold", color = "random-dark", backgroundColor = "transparent",
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65,
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,",
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq,
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color,
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor,
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation,
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape,
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings,
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0,
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

## TFIDF functions ----
tfidf <- function(dfTag, term="lemma", document="doc_id"){
  # calculate tfidf
  dtm <- dfTag %>% dplyr::filter(POSSelected)
  dtm <- document_term_frequencies(dtm, document = document, term = term)
  dtm <- document_term_matrix(dtm)
  tfidf <- dtm_tfidf(dtm)
  tibble(term=names(tfidf), TFIDF=as.numeric(tfidf)) %>% arrange(desc(tfidf))
}

### CLUSTERING ----
clustering <- function(dfTag, n=50, group="doc_id", term="lemma",minEdges=25, normalization="association"){

  x <- dfTag #%>% dplyr::filter(POSSelected)

  cooc <- coocMatrix(x, term=term, group=group, n=n, pos=TRUE)

  edges <- cooc %>%
    data.frame() %>%
    rename(s = cooc) %>%
    mutate(sA = s/(s_from*s_to),
           sC = s/(sqrt(s_from*s_to)),
           sJ = s/(s_from+s_to-s)
    )

  switch(normalization,
         none={edges$value <- edges$s},
         association={edges$value <- edges$sA},
         cosine={edges$value <- edges$sC},
         jaccard={edges$value <- edges$sJ})

  tailEdges <- quantile(edges$value,1-(minEdges/100))

  edges <- edges %>%
    dplyr::filter(value >= tailEdges) %>%
    select(term1, term2, value, s, sA, sC, sJ)

  wordnetwork <- graph_from_data_frame(edges %>% select(term1,term2,value))

  # Community detection via optimization of modularity score
  wordnetwork <- as.undirected(wordnetwork) # an undirected graph
  comm <- igraph::cluster_walktrap(wordnetwork, weights = E(wordnetwork)$value)
  cluster <- data.frame(word=c(cooc$term1,cooc$term2),frequency=c(cooc$s_from,cooc$s_to)) %>%
    distinct() %>%
    left_join(data.frame(word=comm$names,group=comm$membership), by=c("word")) %>%
    drop_na() %>%
    group_by(word) %>%
    summarize(group=first(group),
              frequency=max(frequency)) %>%
    arrange(group,desc(frequency))
  obj <- list(cluster=cluster,comm=comm)
}

dend2vis <- function(hc, labelsize, nclusters=1, community=TRUE){

  # community = TRUE means that hc is an igraph community detection object
  # community = FALSE mean that hc is a hclust object

  # transform and plot a community igraph object using dendrogram
  if (community){
    hc=as.hclust(hc, use.modularity = TRUE)
  }

  h_tail <- round((max(hc$height)*0.12),1)

  hc$height <- hc$height+h_tail

  VIS <- visHclust(hc, cutree = nclusters, colorEdges = "grey60", horizontal = TRUE, export=FALSE)
  VIS$x$edges <- data.frame(color=unique(VIS$x$edges$color)) %>%
    mutate(new_color=colorlist()[1:nrow(.)]) %>%
    right_join(VIS$x$edges, by = "color") %>%
    select(-color) %>%
    rename(color = new_color)
  VIS$x$nodes <- VIS$x$nodes %>%
    mutate(
      label = ifelse(group!="individual", NA,label),
      group=ifelse(group=="individual","word",group),
      title=gsub("individuals","words",title),
      value=1,
      scaling.min=10,
      scaling.max=10)
  coords <- VIS$x$nodes %>% select(x,y) %>% as.matrix()

  edges <- VIS$x$edges
  nodes <- VIS$x$nodes %>% select(id,label) %>% dplyr::filter(label!="1")

  VIS$x$edges <- edges %>%
    select(-id) %>%
    left_join(nodes, by=c("to" = "id")) %>%
    select(-label.x) %>%
    rename(label=label.y) %>%
    mutate(value=10,
           font.color=color,
           font.size=labelsize*10,
           font.vadjust=-0.1*font.size,
           label = ifelse(is.na(label),"",label))

  VIS <- VIS %>% visGroups(groupname = "group", color ="gray90",
                           shape = "dot", size = 10)  %>%
    visGroups(groupname = "word",
              font = list(size = 0),
              color = list(background = "white", border = "#80B1D3",
                           highlight = "#e2e9e9", hover = "orange"), shape = "box") %>%
    visNodes(font=list(align=VIS$x$nodes$font.align)) %>%
    visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=list(to=1000,from=0), algorithm="hierarchical"), nodesIdSelection = FALSE,
                           manipulation = FALSE, height ="100%", width = "100%") %>%
    visNetwork::visInteraction(dragNodes = FALSE, navigationButtons = F, hideEdgesOnDrag = TRUE, zoomSpeed=0.4) %>%
    visIgraphLayout(layout = "layout.norm", layoutMatrix = coords, type="full") %>%
    visEdges(font = list(align="top", size=VIS$x$edges$font.size)) %>%
    visEvents(click = "function(nodes){
                  Shiny.onInputChange('click_dend', nodes.nodes[0]);
                  ;}"
    )

  for (i in 1:nrow(VIS$x$nodes)){
    if (VIS$x$nodes$group[i]=="group"){
      old_inertia <- as.character(VIS$x$nodes$inertia[i])
      inertia <- as.character(VIS$x$nodes$inertia[i]-h_tail)
      VIS$x$nodes$title[i] <- gsub(old_inertia,inertia,VIS$x$nodes$title[i])
    }
  }

  return(VIS)
}

### CORRESPONDENCE ANALYSIS -----

## Correspondence Analysis on Words ----
wordCA <- function(x, n=50,  term="lemma", group=c("Documents")){
  switch(group,
         Documents={group <- "doc_id"},
         Paragraphs={group <- c("doc_id", "paragraph_id")},
         Sentences={group <- c("doc_id", "sentence_id")})

  #x <- dfTag %>% dplyr::filter(POSSelected)

  if (length(group)>1){
    new_doc_id <- unique_identifier(x, fields = group)
  }else {
    new_doc_id <- x$doc_id
  }
  dtm <- document_term_frequencies(x %>% mutate(doc_id=new_doc_id), term=term)

  #dtm <- document_term_frequencies(x, term=term)
  mat <- document_term_matrix(dtm, weight="freq")
  mat <- as.matrix(dtm_remove_lowfreq(mat, minfreq = 1, maxterms=n))

  res <- ca::ca(mat)

  # Contribute
  Ncol <- min(10,ncol(res$rowcoord))
  contrib <- data.frame((res$colcoord[,1:Ncol]^2)*res$colmass)
  colnames(contrib) <- paste0("Contrib",1:ncol(contrib))


  # Cosines squared
  cosine <- data.frame(((res$colcoord[,1:Ncol]^2)/(res$coldist)))
  colnames(cosine) <- paste0("Cosine",1:ncol(contrib))

  # Word Coordinates
  wordCoord <- res$colcoord[,1:Ncol] %>%
    data.frame() %>%
    mutate(label = res$colnames,
           inertia = res$colinertia,
           dist = res$coldist,
           mass = res$colmass)

  docContrib <- data.frame((res$rowcoord[,1:Ncol]^2)*res$rowmass)
  docCoord <- res$rowcoord[,1:Ncol] %>%
    data.frame() %>%
    mutate(label = res$rownames,
           inertia = res$rowinertia,
           dist = res$rowdist,
           mass = res$rowmass)

  ## Benzecrì correction
  res$eigCorrected <- ((n/(n-1))^2*(res$sv-1/n)^2)
  #res$eigCorrected[res$eigCorrected<=1/length(res$eigCorrected)] <- 0
  res$eigCorrectedNorm <- res$eigCorrected/sum(res$eigCorrected)*100

  ## result object
  results <- list(ca=res, wordCoord=wordCoord, contrib = contrib, cosine=cosine, docContrib=docContrib, docCoord=docCoord)

  return(results)
}


## caClustering ----
caClustering <- function(results, method = "ward.D2", nDim=2, nclusters=1, lim.contr){
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
      right_join(contr, by="rowname") %>%
      column_to_rownames(var="rowname")
  )
  h <- hclust(D, method=method)

  if (nclusters >1) {
    groups <- cutree(h, k = nclusters)
  } else {
    groups <- rep(1,length(h$labels))
    names(groups) <- h$labels
  }

  results$clustering <- list(h=h, groups=groups)

  return(results)
}


## CA Plot ----
ca2plotly <- function(results, dimX = 1, dimY = 2, topWordPlot = Inf, topDocPlot=20, threshold=0.03, labelsize=16, size=5, lim.contr){

  # filter by contribution
  contr <- results$contrib %>%
    select(c(dimX,dimY)) %>%
    filter_all(all_vars(. < lim.contr)) %>%
    rownames_to_column() %>%
    select("rowname")
  #

  results$contrib <- results$contrib %>%
    rownames_to_column() %>%
    right_join(contr,by="rowname") %>%
    column_to_rownames()

  results$wordCoord <-  results$wordCoord %>%
    rownames_to_column() %>%
    right_join(contr,by="rowname") %>%
    column_to_rownames()

  xlabel <- paste0("Dim",dimX)
  ylabel <- paste0("Dim",dimY)
  dimContrLabel <- paste0("Contrib",c(dimX,dimY))
  ymax <- diff(range((results$wordCoord[[ylabel]])))
  xmax <- diff(range((results$wordCoord[[xlabel]])))
  threshold2 <- threshold*mean(xmax,ymax)

  # scaled size for dots
  dotScale <- (results$contrib[,c(dimX,dimY)]*200)
  dotScale <- ((dotScale[,1]+dotScale[,2])/2)+size

  #Threshold labels to plot
  thres <- sort(dotScale, decreasing = TRUE)[min(topWordPlot, nrow(results$wordCoord))]

  Ncol <- sum(substr(names(results$wordCoord),1,3)=="Dim")
  # coordinates to plot
  noCol <- setdiff(1:Ncol,c(dimX,dimY))

  results$wordCoord <- results$wordCoord %>%
    select(-any_of(noCol))

  names(results$wordCoord)[1:2] <- c("Dim1","Dim2")

  results$wordCoord <- results$wordCoord %>%
    mutate(dotSize = dotScale,
           groups = results$clustering$groups,
           labelToPlot = ifelse(dotSize>=thres, label, ""),
           font.color = ifelse(labelToPlot=="", NA, adjustcolor(colorlist()[groups], alpha.f = 0.85)),
           font.size = round(dotSize*2 ,0))

  ## Avoid label overlapping
  labelToRemove <- avoidOverlaps(results$wordCoord, threshold = threshold2)
  results$wordCoord <- results$wordCoord %>%
    mutate(labelToPlot = ifelse(labelToPlot %in% labelToRemove, "",labelToPlot))

  hull_data <-
    results$wordCoord %>%
    group_by(.data$groups) %>%
    slice(chull(Dim1, Dim2)) %>%
    rename(color=font.color)

  hull_data <- hull_data %>%
    bind_rows(
      hull_data %>% group_by(groups) %>% slice_head(n=1)
    ) %>%
    mutate(id = row_number()) %>%
    arrange(groups,id)

  hoverText <- paste(" <b>", results$wordCoord$label,"</b>\n Inertia: ", round(results$wordCoord$inertia,3), "\n Mass:   ", round(results$wordCoord$mass,3), sep="")


  ## Plot
  fig <- plot_ly(data = results$wordCoord, x = results$wordCoord$Dim1, y = results$wordCoord$Dim2, #customdata=results$wordCoord,
                 type="scatter",
                 mode   = 'markers',
                 marker = list(
                   size = dotScale,
                   color = adjustcolor(colorlist()[results$wordCoord$groups], alpha.f = 0.3), #'rgb(79, 121, 66, .5)',
                   line = list(color = adjustcolor(colorlist()[results$wordCoord$groups], alpha.f = 0.3), #'rgb(79, 121, 66, .8)',
                               width = 2)
                 ),
                 text = hoverText,
                 hoverinfo = 'text',
                 alpha = .3
  )

  fig <- fig %>% layout(yaxis = list(title = paste0("Dim ",dimY," (",round(results$ca$eigCorrectedNorm[2],2),"%)"),
                                     showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 1)),
                        xaxis = list(title = paste0("Dim ",dimX," (",round(results$ca$eigCorrectedNorm[1],2),"%)"),
                                     zeroline = TRUE, showgrid = TRUE, showline = FALSE, showticklabels = TRUE),
                        plot_bgcolor  = "rgba(0, 0, 0, 0)",
                        paper_bgcolor = "rgba(0, 0, 0, 0)",
                        showlegend = F)

  for (i in seq_len(max(results$wordCoord$groups))){
    w <- results$wordCoord %>% dplyr::filter(groups == i) %>%
      mutate(Dim1 = Dim1+dotSize*0.005,
             Dim2 = Dim2+dotSize*0.01)

    if (max(hull_data$groups>1)){
      hull_df <- hull_data %>% dplyr::filter(.data$groups==i)
      fig <- fig  %>%
        add_polygons(x = hull_df$Dim1, y=hull_df$Dim2, inherit = FALSE, showlegend = FALSE,
                     color = I(hull_df$color[1]), opacity=0.3, line=list(width=2),
                     text=paste0("Cluster ",i), hoverinfo = 'text', hoveron="points")
    }

    fig <- fig %>%
      add_annotations(data = w, x = ~Dim1, y = ~Dim2, xref = 'x1', yref = 'y',
                      text = ~labelToPlot,
                      font = list(family = 'sans serif', size = labelsize, color = w$font.color[1]),
                      showarrow = FALSE)
  }

  ## Doc to plot
  if (topDocPlot>0){
    results$docContrib <- results$docContrib %>%
      select(-any_of(noCol))
    docContrib <- (results$docContrib[,1]+results$docContrib[,2])/2

    results$docCoord <- results$docCoord %>%
      mutate(contrib = docContrib)

    docCoord <- results$docCoord %>%
      select(all_of(c(dimX,dimY)), label, contrib) %>%
      slice_max(order_by = contrib, n=topDocPlot) %>%
      mutate(dotScaleDoc = contrib*50+size) %>%
      rename(labelToPlot = label,
             dotSize = contrib)

    names(docCoord)[1:2] <- c("Dim1", "Dim2")

    docLabelToRemove <- avoidOverlaps(docCoord, threshold = threshold2*1.5)
    docCoord <- docCoord %>%
      mutate(label = labelToPlot,
             labelToPlot = ifelse(labelToPlot %in% docLabelToRemove, "",labelToPlot),
             symbol = "hexagon")

    wDoc <- docCoord %>%
      mutate(Dim1 = Dim1+dotScaleDoc*0.01,
             Dim2 = Dim2+dotScaleDoc*0.015)

    fig <- fig %>%
      add_markers(data = docCoord, x = ~Dim1, y = ~Dim2,
                text = ~label,
                #type = "scatter", mode = "markers",
                marker = list(
                  symbol = docCoord$symbol,
                  size = docCoord$dotScaleDoc,
                  color = adjustcolor("#6F7378", alpha.f = 0.3),
                  line = list(color = adjustcolor("#6F7378", alpha.f = 0.3),
                              width = 2)
                )
      ) %>%
      add_annotations(data = wDoc, x = ~Dim1, y = ~Dim2, xref = 'x1', yref = 'y',
                      text = ~labelToPlot,
                      font = list(family = 'sans serif', size = labelsize, color = adjustcolor("#000000", alpha.f = 0.5)), #4C4E52
                      showarrow = FALSE)
  }

  fig <- fig %>% config(displaylogo = FALSE,
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
  return(fig)

}

## function to avoid label overlapping ----
avoidOverlaps <- function(w,threshold=0.10){

  w[,2] <- w[,2]/2

  Ds <- dist(w %>%
               dplyr::filter(labelToPlot!="") %>%
               select(1:2),
             method="manhattan", upper=T) %>%
    dist2df() %>%
    rename(from = row,
           to = col,
           dist = value) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot!="") %>%
        select(labelToPlot, dotSize),
      by=c("from" = "labelToPlot")
    ) %>%
    rename(w_from = dotSize) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot!="") %>%
        select(labelToPlot, dotSize),
      by=c("to" = "labelToPlot")
    ) %>%
    rename(w_to = dotSize) %>%
    filter(dist<threshold)

  if (nrow(Ds)>0){
    st <- TRUE
    i <- 1
    label <- NULL
    case <- "n"

    while(isTRUE(st)){
      if (Ds$w_from[i]>Ds$w_to[i] & Ds$dist[i]<threshold){
        case <- "y"
        lab <- Ds$to[i]

      } else if (Ds$w_from[i]<=Ds$w_to[i] & Ds$dist[i]<threshold){
        case <- "y"
        lab <- Ds$from[i]
      }

      switch(case,
             "y"={
               Ds <- Ds[Ds$from != lab,]
               Ds <- Ds[Ds$to != lab,]
               label <- c(label,lab)
             },
             "n"={
               Ds <- Ds[-1,]
             })

      if (i>=nrow(Ds)){
        st <- FALSE
      }
      case <- "n"
      #print(nrow(Ds))
    }
  } else {
    label <-  NULL
  }

  label

}

## convert a distance object into a data.frame ----
dist2df <- function(inDist) {
  if (class(inDist) != "dist") stop("wrong input type")
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) sequence(A) else attr(inDist, "Labels")
  if (isTRUE(attr(inDist, "Diag"))) attr(inDist, "Diag") <- FALSE
  if (isTRUE(attr(inDist, "Upper"))) attr(inDist, "Upper") <- FALSE
  data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B)-1):1),
    value = as.vector(inDist))
}


### NETWORK -----

## cooccurrence matrix
coocMatrix <- function(x, term="lemma", group="doc_id", n=50, pos=TRUE){
  term_old <- term
  if (pos){
    #new_var <- paste0(term,"_upos")
    x$new_var <- paste0(x[[term]],"_",x$upos)
    term="new_var"
  } else {
    term <- term_old
  }

  new_doc_id <- unique_identifier(x, fields = group)
  dtm <- document_term_frequencies(x %>% mutate(doc_id=new_doc_id), term=term)

  dtm <- dtm %>%
    mutate(binary=1)

  if (length(unique(new_doc_id))==1){
    mat <- document_term_matrix(dtm, weight="freq")
    lab <- colnames(mat)
    mat <- as.numeric(mat)
    names(mat) <- lab
    mat <- sort(mat, decreasing = TRUE)[1:min(n,length(mat))]
    mat <- matrix(mat, 1, length(mat), dimnames = list(x$doc_id[1],names(mat)))
  } else{
    mat <- document_term_matrix(dtm, weight="binary")
    mat <- dtm_remove_lowfreq(mat, minfreq = 1, maxterms=n)
  }

  mat <- Matrix::crossprod(mat)
  if (sum(mat)-sum(Matrix::diag(mat))==0){
    return(NA)
  }
  mat <- as_cooccurrence(mat)
  mat <- mat %>%
    group_by(term1) %>%
    mutate(s_from=max(cooc)) %>%
    ungroup() %>%
    group_by(term2) %>%
    mutate(s_to=max(cooc)) %>%
    ungroup() %>%
    filter(term1 != term2) %>%
    data.frame()

  if (pos){
    mat <- mat %>%
      mutate(label1 = gsub("_.*","",term1),
             label2 = gsub("_.*","",term2),
             upos_from = gsub(".*_","",term1),
             upos_to = gsub(".*_","",term2)) %>%
      select(-term1,-term2) %>%
      rename(term1=label1,
             term2=label2)
  } else{
    mat$upos_from <- mat$upos_to <-  ""
  }
  return(mat)
}

# word frequency from cooccurence matrix
cooc_freq <- function(cooc){
  term_freq <- data.frame(term=c(cooc$term1,cooc$term2),
                          upos = c(cooc$upos_from,cooc$upos_to),
                          n=c(cooc$s_from,cooc$s_to)) %>%
    distinct() %>%
    group_by(term,upos) %>%
    summarize(n = sum(n)) %>%
    ungroup()

}

network <- function(x, term="lemma", group=c("doc_id", "sentence_id"), n, minEdges, labelsize=4, opacity=0.6,
                    interLinks=FALSE, normalization="none", remove.isolated=FALSE, community.repulsion=0){

  # size scaling
  scalemin <- 20*(1+labelsize/5)
  scalemax <- 70*(1+labelsize/5)

  colorlist <- colorlist()

  # params
  shape <- "dot"
  opacity.min <- 0.4

  #x <- dfTag %>% dplyr::filter(POSSelected)

  cooc <- coocMatrix(x, term=term, group=group, n=n, pos=FALSE)
  if (is.na(cooc)[1]){
    obj <- list(nodes=NA, edges=NA)
    return(obj)
  }

  nodes <- cooc_freq(cooc) %>%
    mutate(id = row_number(),
           shape=shape,
           color = "navyblue") %>%
    rename(label = term,
           value = n)

  nodes$font.size <- log(nodes$value)
  # scalemin <- 20
  # scalemax <- 150
  Min <- min(nodes$font.size)
  Max <- max(nodes$font.size)
  if (Max>Min){
    size <- (nodes$font.size-Min)/(Max-Min)*15*labelsize+10
  } else {size=10*labelsize}
  size[size<scalemin]=scalemin
  size[size>scalemax]=scalemax
  nodes$font.size <- size

  if (shape %in% c("dot","square")){
    nodes$font.vadjust <- -0.7*nodes$font.size
  }else{
    nodes$font.vadjust <-0
  }


  ### EDGES
  edges <- cooc %>%
    left_join(
      nodes %>% select(id,label), by = c("term1" = "label")) %>%
    rename(from = id) %>%
    left_join(
      nodes %>% select(id,label), by = c("term2" = "label")) %>%
    rename(to = id,
           s = cooc) %>%
    mutate(sA = s/(s_from*s_to),
           sC = s/(sqrt(s_from*s_to)),
           sJ = s/(s_from+s_to-s),
           sNorm = ((s-min(s))/diff(range(s)))*14+1,
           sANorm = ((sA-min(sA))/diff(range(sA)))*14+1,
           sCNorm = ((sC-min(sC))/diff(range(sC)))*14+1,
           sJNorm = ((sJ-min(sJ))/diff(range(sJ)))*14+1,
    )

  switch(normalization,
         none={edges$value <- edges$sNorm},
         association={edges$value <- edges$sANorm},
         cosine={edges$value <- edges$sCNorm},
         jaccard={edges$value <- edges$sJNorm})


  if (minEdges == "Auto"){
    y <- quantile(edges$value,seq(1,0,-0.01))
    x=1:length(y)
    res <- strucchange::breakpoints(y~x)
    tailEdges <- y[res$breakpoints[1]]
    #minEdges <- 10*which.min(diff((quantile(edges$value,1-(seq(0,100,10)/100)))))
  } else{
    minEdges <- as.numeric(gsub("%","",minEdges))
    tailEdges <- quantile(edges$value,1-(minEdges/100))
  }

  #tailEdges <- quantile(edges$value,1-(minEdges/100))

  edges <- edges %>%
    dplyr::filter(value >= tailEdges) %>%
    select(term1, term2, from,to,value, s, sA, sC, sJ) %>%
    rename(term_from=term1,
           term_to=term2)

  ### remove isolated
  if (isTRUE(remove.isolated)){
    id_remove <- setdiff(nodes$id,unique(c(edges$from,edges$to)))
    if (length(id_remove)>0){
      nodes <- nodes %>%
        filter(!id %in% id_remove)
      #opacity_font <- opacity_font[-id_remove]
    }
  }

  ### COMMUNITY DETECTION
  graph <- igraph::graph_from_data_frame(edges %>% select(-term_from, -term_to), directed = FALSE)
  cluster <- igraph::cluster_walktrap(graph)
  cluster_df <- data.frame(as.list(igraph::membership(cluster)))
  cluster_df <- as.data.frame(t(cluster_df)) %>%
    mutate(id = as.numeric(gsub("X","",rownames(.)))) %>%
    rename(group = "V1")

  #Create group column
  nodes <- left_join(nodes, cluster_df, by = "id") %>%
    drop_na(group)

  # Community repulsion
  if (community.repulsion>0){
    community.repulsion = round(community.repulsion*100)
    # row <- as_edgelist(bsk.network)
    row <- edges %>% select(1:2) %>% as.matrix()
    membership <- nodes$group
    names(membership) <- nodes$label

    # membership <- V(bsk.network)$community
    # names(membership) <- V(bsk.network)$name
    repulsion <- community.repulsion * max(edges$value, na.rm=T)
    edges$value <-  edges$value+apply(row,1,weight.community,membership,repulsion,1)

  }




  ## opacity for label
  opacity_font <- sqrt((nodes$font.size-min(nodes$font.size))/diff(range(nodes$font.size)))*opacity+opacity.min+0.1
  if(is.nan(opacity_font[1])) opacity_font <- rep(opacity.min,length(opacity_font))

  if (labelsize>0){
    nodes$font.color <- unlist(lapply(opacity_font, function(x) adjustcolor("black",alpha.f = x)))
  }else{
    nodes$font.color <- adjustcolor("black", alpha.f = 0)
  }

  # node colors
  nodes$opacity.nodes <- (opacity_font-min(opacity_font))/(diff(range(opacity_font)))*0.5+opacity.min
  nodes$opacity.nodes[is.nan(nodes$opacity.nodes)] <- 0.5
  nodes$color <- paste0(colorlist[nodes$group],round(nodes$opacity.nodes,2)*100)

  if(interLinks){
    interColor <- "#69696920"
  }else{
    interColor <- "#69696900"
  }
  edges <- edges %>%
    left_join(nodes %>% select(id,group,color), by=c("from"="id")) %>%
    rename(group_from=group) %>%
    left_join(nodes %>% select(id,group), by=c("to"="id")) %>%
    rename(group_to = group) %>%
    mutate(color = ifelse(group_from==group_to, paste0(substr(color,1,7),"20"), interColor))

  obj <- list(nodes=nodes, edges=edges)
}

net2vis <- function(nodes,edges){

  layout <- "layout_nicely"

  if ((is.na(nodes))[1]){
    VIS <- visNetwork::visNetwork(nodes = data.frame(id="Empty Network",label="No Connections Among Nodes",
                                                     size= 0, title="No Connections Among Nodes", font.size=20),
                                  type="full", smooth=TRUE, physics=FALSE,
                                  x=1,y=1) %>%
      visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = F) %>%
      visNetwork::visInteraction(dragNodes = TRUE, navigationButtons = F, hideEdgesOnDrag = TRUE, zoomSpeed = 0.2)
    return(VIS)
  }



  VIS<-
    visNetwork::visNetwork(nodes = nodes, edges = edges, type="full", smooth=TRUE, physics=FALSE) %>%
    visNetwork::visNodes(shadow=TRUE, shape=nodes$shape, font=list(color=nodes$font.color, size=nodes$font.size,vadjust=nodes$font.vadjust)) %>%
    visNetwork::visIgraphLayout(layout = layout, type = "full")

# avoid overlaps among node labels
  ## avoid label overlaps
  coords <- VIS$x$nodes %>%
    select(x,y)

  threshold <- 0.03
  ymax <- diff(range(coords[,2]))
  xmax <- diff(range(coords[,1]))
  threshold2 <- threshold*mean(xmax,ymax)
  w <- data.frame(x=coords[,1],y=coords[,2],labelToPlot=VIS$x$nodes$label, dotSize=VIS$x$nodes$font.size, row.names = VIS$x$nodes$label)
  labelToRemove <- avoidNetOverlaps(w, threshold = threshold2)

  VIS$x$nodes <- VIS$x$nodes %>%
    mutate(title = label,
      label = ifelse(label %in% labelToRemove, "",label))

  VIS <- VIS %>%
    visNetwork::visEdges(smooth = list(type="horizontal")) %>%
    visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
    visNetwork::visInteraction(dragNodes = TRUE, navigationButtons = F, hideEdgesOnDrag = TRUE, zoomSpeed = 0.2) %>%
    visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}"
    ) %>%
    #visNetwork::visPhysics(barnesHut=list(avoidOverlap=1)) %>%
    visNetwork::visOptions(manipulation = FALSE, height ="100%", width = "100%") #%>%
  #visNetwork::addFontAwesome()
}

weight.community=function(row,membership,weigth.within,weight.between){
  if(as.numeric(membership[which(names(membership)==row[1])])==as.numeric(membership[which(names(membership)==row[2])])){
    weight=weigth.within
  }else{
    weight=weight.between
  }
  return(weight)
}

## function to avoid label overlapping ----
avoidNetOverlaps <- function(w,threshold=0.10){

  w[,2] <- w[,2]/3

  Ds <- dist(w %>%
               dplyr::filter(labelToPlot!="") %>%
               select(1:2),
             method="manhattan", upper=T) %>%
    dist2df() %>%
    rename(from = row,
           to = col,
           dist = value) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot!="") %>%
        select(labelToPlot, dotSize),
      by=c("from" = "labelToPlot")
    ) %>%
    rename(w_from = dotSize) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot!="") %>%
        select(labelToPlot, dotSize),
      by=c("to" = "labelToPlot")
    ) %>%
    rename(w_to = dotSize) %>%
    filter(dist<threshold)

  if (nrow(Ds)>0){
    st <- TRUE
    i <- 1
    label <- NULL
    case <- "n"

    while(isTRUE(st)){
      if (Ds$w_from[i]>Ds$w_to[i] & Ds$dist[i]<threshold){
        case <- "y"
        lab <- Ds$to[i]

      } else if (Ds$w_from[i]<=Ds$w_to[i] & Ds$dist[i]<threshold){
        case <- "y"
        lab <- Ds$from[i]
      }

      switch(case,
             "y"={
               Ds <- Ds[Ds$from != lab,]
               Ds <- Ds[Ds$to != lab,]
               label <- c(label,lab)
             },
             "n"={
               Ds <- Ds[-1,]
             })

      if (i>=nrow(Ds)){
        st <- FALSE
      }
      case <- "n"
      #print(nrow(Ds))
    }
  } else{
    label = NULL
  }
  label

}

## GRAKO ----
grako <- function(dfTag, normalization="association", n=50, labelsize=4, opacity=0.6, minEdges=50, singleWords=TRUE, term="lemma"){

  opacity.min=0.5

  # n is the number of NOUNS AND PROPER NOUNS
  if (singleWords){
    ngram_min <- 1
    dfTag <- rake(dfTag, group = "doc_id", ngram_max=5, ngram_min=ngram_min, relevant = c("PROPN"), rake.min=-Inf, term=term)$dfTag %>%
      mutate(upos = ifelse(upos =="PROPN", "MULTIWORD", upos),
             ngram = ifelse(upos=="MULTIWORD" & is.na(ngram), 1, ngram))
  } else {
    ngram_min <- 2
    dfTag <- rake(dfTag, group = "doc_id", ngram_max=5, ngram_min=ngram_min, relevant = c("PROPN"), rake.min=-Inf, term=term)$dfTag
  }

  #x <- dfTag %>% highlight() %>% dplyr::filter(upos %in% c("MULTIWORD", "VERB"))

  ### EDGES
  x <- dfTag %>% highlight() %>%
    dplyr::filter(upos %in% c("MULTIWORD", "NOUN", "PROPN", "ADJ", "VERB", "PUNCT"))
  cooc <- grakoCoocMatrix(x, term=term, group=c("doc_id", "sentence_id"), n=n^2, pos=TRUE)

  # calculate local occurrences for nodes

  nodes <- cooc_freq(cooc) %>%
    rename(label = term,
           value = n) %>%
    filter(upos %in% c("MULTIWORD", "VERB")) %>%
    mutate(id=row_number(),
           shape=ifelse(upos =="VERB", "text", "text"),
           color = ifelse(upos =="VERB", "#E41A1C", "#4F7942"))


  edges <- cooc %>%
    dplyr::filter(upos_from %in% c("VERB", "MULTIWORD") & upos_to %in% c("VERB", "MULTIWORD")) %>%
    dplyr::filter(!upos_from==upos_to & !(upos_from == "MULTIWORD" & upos_to == "PROPN") &
                    !(upos_to == "MULTIWORD" & upos_from == "PROPN")) %>%
    left_join(nodes %>% select(id,label, upos), by = c("term1" = "label", "upos_from" = "upos")) %>%
    rename(from = id) %>%
    left_join(nodes %>% select(id,label,upos), by = c("term2" = "label", "upos_to" = "upos")) %>%
    rename(to = id,
           s= cooc) %>%
    drop_na() %>%
    filter(s>1) %>%
    mutate(sA = s/(s_from*s_to),
           sC = s/(sqrt(s_from*s_to)),
           sJ = s/(s_from+s_to-s),
           sNorm = ((s-min(s))/diff(range(s))),
           role = ifelse(upos_from!="VERB","active","passive"),
           color = ifelse(role=="active", "#4F794250", "#E41A1C50")
    )
  switch(normalization,
         none={edges$value <- edges$sNorm*14+1},
         association={edges$value <- edges$sA*14+1},
         cosine={edges$value <- edges$sC*14+1},
         jaccard={edges$value <- edges$sJ*14+1})

  edges <- edges %>%
    arrange(desc(value)) %>%
    slice_head(n=n)

  tailEdges <- quantile(edges$value,1-(minEdges/100))

  edges <- edges %>%
    dplyr::filter(value >= tailEdges) %>%
    select(upos_from,term1, upos_to,term2,from,to,value, s, sA, sC, sJ, role, color) %>%
    rename(term_from=term1,
           term_to=term2)

  nodes <- nodes %>%
    filter(nodes$id %in% c(edges$from,edges$to))

  nodes$font.size <- log(nodes$value)
  scalemin <- 30
  scalemax <- 60
  Min <- min(nodes$font.size)
  Max <- max(nodes$font.size)
  if (Max>Min){
    size <- (nodes$font.size-Min)/(Max-Min)*15*labelsize+10
  } else {size=10*labelsize}
  size[size<scalemin]=scalemin
  size[size>scalemax]=scalemax
  nodes$font.size <- size

  nodes$font.vadjust <- ifelse(nodes$shape=="box",-0.7*nodes$font.size, 0)

  ## opacity for label
  opacity_font <- sqrt((nodes$font.size-min(nodes$font.size))/diff(range(nodes$font.size)))*opacity+opacity.min+0.1

  if(is.nan(opacity_font[1])) opacity_font <- rep(opacity.min,length(opacity_font))

  # node colors
  nodes$opacity.nodes <- round(((opacity_font-min(opacity_font))/(diff(range(opacity_font)))*0.5+opacity.min)*100,0)

  if (labelsize>0){
    nodes <- nodes %>%
      mutate(
        opacity.nodes = ifelse(opacity.nodes>=100,99,opacity.nodes),
        font.color = ifelse(upos=="VERB", "#E41A1C", "#4F7942"))
    #font.color = ifelse(upos=="VERB", paste0("#E41A1C",opacity.nodes), paste0("#4F7942",opacity.nodes)))
    #nodes$font.color <- unlist(lapply(opacity_font, function(x) adjustcolor("black",alpha.f = x)))
  }else{
    nodes <- nodes %>%
      mutate(font.color = ifelse(upos=="VERB", adjustcolor("#E41A1C",alpha.f = 0), adjustcolor("#4F7942",alpha.f = 0)))
  }

  nodes <- nodes %>%
    mutate(title = label,
           label = ifelse(upos=="VERB", paste0("<i>",label,"</i>"), paste0("<b>",label,"</b>")),
           font.multi = "html")

  # info for word in context
  x$grako <- paste0(x[[term]]," ",c(x[[term]][-1],""))

  obj <- list(nodes=nodes, edges=edges, multiwords=x %>%
                dplyr::filter(upos %in% c("MULTIWORD", "VERB")) %>%
                select(doc_id, sentence_id, sentence_hl, token, lemma, upos, grako))
}

grakoCoocMatrix <- function(x, term="lemma", group="doc_id", n=50, pos=TRUE){
  term_old <- term
  if (pos){
    #new_var <- paste0(term,"_upos")
    x$new_var <- paste0(x[[term]],"_",x$upos)
    term="new_var"
  } else {
    term <- term_old
  }

  mat <- cooccurrence(x[[term]], relevant = rep(TRUE,nrow(x)), skipgram = 0)
  mat <- mat %>%
    group_by(term1) %>%
    mutate(s_from=max(cooc)) %>%
    ungroup() %>%
    group_by(term2) %>%
    mutate(s_to=max(cooc)) %>%
    ungroup() %>%
    filter(term1 != term2) %>%
    data.frame()

  if (pos){
    mat <- mat %>%
      mutate(label1 = gsub("_.*","",term1),
             label2 = gsub("_.*","",term2),
             upos_from = gsub(".*_","",term1),
             upos_to = gsub(".*_","",term2)) %>%
      select(-term1,-term2) %>%
      rename(term1=label1,
             term2=label2)
  } else{
    mat$upos_from <- mat$upos_to <-  ""
  }

  return(mat)
}
grako2vis <- function(nodes, edges){
  # nodes data.frame for legend
  lnodes <- data.frame(label = c("<b>Proper Noun</b>", "<i>Verb</i>"),
                       shape = c("text", "text"), font.color = c("#4F794290", "#E41A1C90"),
                       title = " ", id = 1:2, font.multi = "html",
                       font.size=14) %>% mutate(title=c("Proper Noun", "Verb"))
  #,
  #                     font.style="font-weight:bold")

  # edges data.frame for legend
  ledges <- data.frame(color = c("#4F794270", "#E41A1C90"),
                       label = c("active", "passive"), arrows =c("to", "to"),
                       font.size=10,
                       font.vadjust = -8)

  layout="layout_nicely"
  VIS <- visNetwork::visNetwork(nodes = nodes, edges = edges, type="full", smooth=TRUE, physics=TRUE, export=FALSE) %>%
    visNetwork::visNodes(shadow=FALSE, shape=nodes$shape,
                         font=list(color=nodes$font.color, size=nodes$font.size,vadjust=nodes$font.vadjust,
                                   multi=nodes$font.multi)) %>%
    visNetwork::visIgraphLayout(layout = layout, type = "full") %>%
    visNetwork::visEdges(smooth = list(type="horizontal")) %>%
    visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
    visNetwork::visInteraction(dragNodes = TRUE, navigationButtons = F, hideEdgesOnDrag = TRUE, zoomSpeed = 0.2) %>%
    visNetwork::visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}"
    ) %>%
    visNetwork::visOptions(manipulation = FALSE, height ="100%", width = "100%") %>%
    visNetwork::visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, width = 0.1)
}


#### TOPIC MODELING ----

### model tuning
tmTuning <- function(x, group=c("doc_id", "sentence_id"), term="lemma",
                     metric=c("CaoJuan2009",  "Deveaud2014", "Arun2010", "Griffiths2004"),
                     n=100, top_by=c("freq","tfidf"), minK=2, maxK=20, Kby=1){

  ## check min and max K
  ClusterRange <- sort(c(minK,maxK))
  minK <- ClusterRange[1]
  maxK <- ClusterRange[2]
  minK <- max(minK,1)
  maxK <- min(maxK,length(unique(x$doc_id)))
  ###

  #x <- dfTag %>% dplyr::filter(POSSelected)
  x$topic_level_id <- unique_identifier(x, fields = group)

  dtf <- document_term_frequencies(x, document = "topic_level_id", term = "lemma")

  dtm <- document_term_matrix(x = dtf)

  switch(top_by,
         freq={
           dtm <- dtm_remove_lowfreq(dtm, minfreq = 1, maxterms = n)
         },
         tfidf={
           dtm <- dtm_remove_tfidf(dtm, top = n)
         }
  )

  ## find optimal number of topics K using the librare ldatuning
  result <- ldatuning::FindTopicsNumber(
    dtm,
    topics = seq(from = minK, to = maxK, by = Kby),
    metrics = metric,
    method = "Gibbs",
    control = list(seed = 77),
    verbose = TRUE
  )
  return(result)
}

tmTuningPlot <- function(result, metric){

  df <- result

  switch(metric,
         CaoJuan2009={
           bestT <- df$topics[which.min(df[,2])][1]
         },
         Arun2010={
           bestT <- df$topics[which.min(df[,2])][1]
         },
         Griffiths2004={
           bestT <- df$topics[which.max(df[,2])][1]
         },
         Deveaud2014={
           bestT <- df$topics[which.max(df[,2])][1]
         }
  )
  names(df) <- c("x","y")
  df <- df %>%
    mutate(y = (y-min(y))/diff(range(y)))

  hoverText <- paste(" <b>Topic ", df$x,"</b>\n ",metric, ": ",
                     round(df$y,2), sep="")

  fig <- plot_ly(df, x = ~x, y = ~y, type = 'scatter', mode = 'markers+lines',
                 line = list(color="#6CC28360", width=2),
                 marker = list(
                   size = 5,
                   color = "#6CC283",
                   line = list(color = "#6CC283",
                               width = 2)
                 ),
                 text = hoverText,
                 hoverinfo = 'text') %>%
    layout(annotations=list(text=paste0("K selection by ",metric," metric: Optimal N. of Topics ",bestT ),xref="paper",x=0.5,
                            yref="paper",y=1,yshift=30,showarrow=FALSE,
                            font=list(size=24,color="gray30")),
           #title = paste0("K selection by ",metric," metric"),
           paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(255,255,255)',
           xaxis = list(title = "Topics",
                        gridcolor = 'rgb(229,229,229)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(229,229,229)',
                        ticks = 'outside',
                        zeroline = TRUE,
                        range = c(0, max(df$x)+1),
                        dtick = 1,
                        tick0 = 0),
           yaxis = list(title = metric,
                        gridcolor = 'rgb(229,229,229)',
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(229,229,229)',
                        ticks = 'inside',
                        zeroline = TRUE,
                        range = c(-0.02, 1.05),
                        dtick = 0.20,
                        tick0 = 0),
           showlegend = FALSE)

  fig <- fig %>% config(displaylogo = FALSE,
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

  return(fig)
}

### model estimation

tmEstimate <- function(x, K, group=c("doc_id", "sentence_id"), term="lemma", n=100, top_by=c("freq","tfidf")){

  #x <- dfTag %>% dplyr::filter(POSSelected)
  x$topic_level_id <- unique_identifier(x, fields = group)

  dtf <- document_term_frequencies(x, document = "topic_level_id", term = term)

  dtm <- document_term_matrix(x = dtf)

  switch(top_by,
         freq={
           dtm <- dtm_remove_lowfreq(dtm, minfreq = 1, maxterms = n)
         },
         tfidf={
           dtm <- dtm_remove_tfidf(dtm, top = n)
         }
  )

  # compute the LDA model, inference via 1000 iterations of Gibbs sampling

  topicModel <- LDA(dtm, K, method="Gibbs", control=list(iter = 500))

  # have a look a some of the results (posterior distributions)
  tmResult <- posterior(topicModel)

  # topics are probability distributions over the entire vocabulary
  beta <- tmResult$terms # get beta from results
  # K distributions over nTerms(DTM) terms

  beta_norm <- beta/matrix(colSums(beta),K,ncol(beta), byrow = TRUE)
  beta_norm <- t(beta_norm) %>%
    as.data.frame() %>%
    mutate(word=colnames(beta_norm))

  variables <- as.character(1:K)
  beta <- t(beta) %>%
    as.data.frame() %>%
    mutate(word=colnames(beta)) %>%
    select(word, all_of(variables))

  # for every document we have a probability distribution of its contained topics
  row_label <- unique(x$doc_id)[as.numeric(row.names(tmResult$topics))]
  theta <- tmResult$topics%>%
    as.data.frame() %>%
    mutate(doc=row_label) %>%
    select(doc, all_of(variables))

  results <- list(topicModel=topicModel, tmResult=tmResult, beta=beta, beta_norm=beta_norm, theta=theta)

  return(results)

}


## hellinger distance ----
hellinger <- function(beta){
  beta <- sqrt(beta)
  B=matrix(NA,ncol(beta),ncol(beta))
  for (i in 1:ncol(beta)){
    for (j in i:ncol(beta)){
      B[i,j]=sum((beta[,i]-beta[,j])^2)
    }
  }

  H <- sqrt(B)*(1/sqrt(2))
}

tmNetwork <- function(beta, minEdge){
  beta <- as.matrix(results$beta[,-1])

  H <- 1- hellinger(beta)
  diag(H) <- NA

  topics <- paste0("Topic_",seq(1,nrow(H)))

  H <- data.frame(H)
  colnames(H) <- topics
  H$from <- topics

  H <- H %>%
    pivot_longer(cols = 1:length(topics), names_to = "to", values_to = "size") %>%
    drop_na()
  edges <- H %>%
    mutate(size = size*10) %>%
    filter(size>minEdge*10) %>%
    drop_na()

  nodes <- data.frame(id = topics, size = 10, color = "#4F7942",
                      title = topics,
                      label = topics,
                      font.color = adjustcolor("black", alpha.f = 0.6))

  VIS <- visNetwork::visNetwork(nodes= nodes, edges = edges, type="full", smooth=TRUE, physics=FALSE) %>%
    visNetwork::visIgraphLayout(layout = "layout_nicely", type = "full") %>%
    visNetwork::visEdges(smooth = list(type="horizontal")) %>%
    visNetwork::visOptions(highlightNearest =list(enabled = T, hover = T, degree=1), nodesIdSelection = T) %>%
    visNetwork::visInteraction(dragNodes = TRUE, navigationButtons = F, hideEdgesOnDrag = TRUE, zoomSpeed = 0.2) %>%
    visNetwork::visOptions(manipulation = FALSE, height ="100%", width = "100%")

  results <- list(H=H %>% rename(value=size), VIS=VIS)
  return(results)
}

tmHeatmap <- function(beta){

  data <- cor(as.matrix(beta[,-1]))
  diag(data) <- 0

  data <- data[nrow(data):1,]

  df = data.frame(data)
  # x <- y <- colnames(df) <- row.names(df) <- paste0("topic ",nrow(data):1)
  id <- sprintf(paste0("%0",nchar(nrow(data)),"d"), nrow(data):1)
  y <- row.names(df) <- paste0("topic ",id)

  x <- colnames(df) <- sort(y)

  df <- df %>%
    rownames_to_column("y") %>%
    pivot_longer(cols = starts_with("topic "), names_to = "variable", values_to = "value") %>%
    mutate(value = round(value,3))

  pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))(30)
  pal[1] <-c("#FFFFFF")

  Hplot <- plot_ly(
    z = data,
    x = x,
    y = y,
    text = data,
    type = "heatmap",
    hoverinfo='none',
    colors = pal,
    zauto = F,
    zmin=-1,
    zmax=1) %>%
    add_annotations(
      data = df,
      x = ~variable,
      y = ~y,
      text = ~value,
      xref = 'x',
      yref = 'y',
      showarrow = FALSE,
      font=list(color='black', size=10)) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c(
             'sendDataToCloud',
             'pan2d',
             'select2d',
             'lasso2d',
             'toggleSpikelines',
             'hoverClosestCartesian',
             'hoverCompareCartesian'
           ))

  return(list(Hplot=Hplot))
}

tmTopicPlot <- function(beta, topic=1, nPlot=10){

  dfPlot <- beta %>%
    select(word, any_of(as.character(topic)))
  names(dfPlot)[2] <- "y"
  dfPlot <- dfPlot %>% arrange(desc(y)) %>%
    slice_max(y, n=nPlot, with_ties = FALSE) %>%
    mutate(
      y = y+runif(nPlot,0,1)/(10^7),
      word = factor(word, levels = unique(word)[order(y, decreasing = FALSE)]))

  fig <- freqPlotly(dfPlot,x="y", y="word", n=nPlot, ylabel="Words", xlabel="Beta Probability",
                    scale="identity", topicmodel = TRUE, colorlist()[topic], decimal = 4) %>%
    layout(annotations=list(text=paste0("Topic ",topic),xref="paper",x=0.5,
                            yref="paper",y=1,yshift=30,showarrow=FALSE,
                            font=list(size=24,color="gray30")))


  fig <- fig %>% config(displaylogo = FALSE,
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

  return(fig)

}

tmDocPlot <- function(theta, topic=1, nPlot=10){

  nPlot <- min(nPlot, nrow(theta))
  dfPlot <- theta %>%
    select(doc, any_of(as.character(topic)))
  names(dfPlot)[2] <- "y"
  dfPlot <- dfPlot %>% arrange(desc(y)) %>%
    slice_max(y, n=nPlot, with_ties = FALSE) %>%
    mutate(
      y = y+runif(nPlot,0,1)/(10^7),
      doc = factor(doc, levels = unique(doc)[order(y, decreasing = FALSE)]))

  fig <- freqPlotly(dfPlot,x="y", y="doc", n=nPlot, ylabel="Documents", xlabel="Theta Probability",
                    scale="identity", topicmodel = TRUE, colorlist()[topic], decimal = 4) %>%
    layout(annotations=list(text=paste0("Topic ",topic),xref="paper",x=0.5,
                            yref="paper",y=1,yshift=30,showarrow=FALSE,
                            font=list(size=24,color="gray30")))


  fig <- fig %>% config(displaylogo = FALSE,
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

  return(fig)

}

### POLARITY DETECTION ----

# download sentiment lexicons
loadSentimentLanguage <- function(language){
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
  file_lang <- dir(path_language_model,pattern=paste0(language,".lexicon"))[1]

  if (is.na(file_lang)){
    switch(Sys.info()[['sysname']],
           Windows={
             download.file(url = paste0("https://www.bibliometrix.org/tall_lexicon/",language,".lexicon"),
                           destfile = paste0(path_language_model,"/",language,".lexicon"), mode = "wb")
           },
           {
             download.file(url = paste0("https://www.bibliometrix.org/tall_lexicon/",language,".lexicon"),
                           destfile = paste0(path_language_model,"/",language,".lexicon"))
           }
    )
  }

  load(file=paste0(path_language_model,"/",language,".lexicon"))

  return(sentimentData)
}

polarity_colors <- function(){
  c("#FF6666", "#FFB266", "#FFFF66", "#66FF66", "#00FF00")
}

## polarity unit choice ###
ids <- function(dfTag, type){
  if (is.null(type)) type="Documents"
  if (type=="Documents" & "ungroupDoc_id" %in% names(dfTag)){
    dfTag <- backToOriginalGroups(dfTag)
  }
  unique(dfTag$doc_id[dfTag$docSelected])
}

freqPlotlySentiment <- function(dfPlot,x,y, xlabel,ylabel, scale=c("identity", "log"), decimal=0){


  polarity_colors <- polarity_colors()

  # function to build and plot plotly horizontal barplot
  dfPlot <- dfPlot %>%
    group_by(lemma) %>%
    mutate(tot=sum(n)) %>%
    ungroup() %>%
    arrange(tot,lemma, doc_pol_clas)


  xmax <- max(dfPlot[[x]])

  switch(scale,
         log={
           #dfPlot$scale <- log(obj$n)
           dfPlot$n <- log(dfPlot$n)

         }
  )

  fig1 <- plot_ly(data=dfPlot , x = dfPlot[[x]], y = ~reorder(dfPlot[[y]], dfPlot[["tot"]]),
                  type = 'bar', orientation = 'h',
                  hovertext = ~doc_pol_clas,
                  marker = list(color = ~paste0(polarity_colors[as.numeric(doc_pol_clas)],"60"),
                                line = list(color = ~polarity_colors[as.numeric(doc_pol_clas)], width = 1)),
                  hovertemplate = "<b><i>Word: %{hovertext}</i></b> <br> <b><i>N. Docs: %{x}</i></b><extra></extra>")

  fig1 <- fig1 %>% layout(yaxis = list(title =ylabel, showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 1)),
                          xaxis = list(title = xlabel, zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = FALSE),
                          plot_bgcolor  = "rgba(0, 0, 0, 0)",
                          paper_bgcolor = "rgba(0, 0, 0, 0)")

  fig1 <- fig1 %>%
    # add_annotations(xref = 'x1', yref = 'y',
    #                                x = dfPlot[[x]] + xmax*0.015,  y = dfPlot[[y]],
    #                                text = ann_text,
    #                                font = list(family = 'Arial', size = 12, color = color),
    #                                showarrow = FALSE) %>%
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

sentimentAnalysis <- function(dfTag, language="english", lexicon_model="huliu"){
  # lexicon for polarity detection
  if (!language %in% c("ancient_greek", "classical_chinese", "coptic", "gothic","north_sami","old_church_slavonic","old_french","old_russian","scottish_gaelic","wolof")){
    sentimentData <- loadSentimentLanguage(language)
  } else{
    return(NA)
  }

  amplifiers <- attr(sentimentData, "amplifiers")
  de_amplifiers <- attr(sentimentData, "de_amplifiers")
  negators <- attr(sentimentData, "negators")

  if (language == "english"){
    sentimentData <- sentimentData %>%
      filter(lexicon == lexicon_model)
  }

  polarity_terms<-data.frame(term=sentimentData$Word ,polarity=sentimentData$sentiment)

  sentiment <- txt_sentiment(
    dfTag,
    term = "lemma",
    polarity_terms=polarity_terms,
    polarity_negators = negators,
    polarity_amplifiers = amplifiers,
    polarity_deamplifiers = de_amplifiers,
    amplifier_weight = 0.8,
    n_before = 4,
    n_after = 2,
    constrain = TRUE
  )

  s_data<-sentiment$data
  s_overall<-sentiment$overall

  s_data <- s_data %>%
    left_join(s_overall %>% select(doc_id, sentiment_polarity) %>% rename(doc_polarity=sentiment_polarity), by="doc_id") %>%
    filter(!is.na(polarity)) %>%
    mutate(doc_pol_clas = cut(.data$doc_polarity, breaks=c(-1,-0.6,-0.2,0.2,0.6,1),
                              labels=c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive"),
                              #labels=c("Very Positive", "Positive", "Neutral", "Negative", "Very Negative"),
                              include.lowest = T, ordered_result = TRUE))

  s_overall <- s_overall %>%
    mutate(doc_pol_clas = cut(.data$sentiment_polarity, breaks=c(-1,-0.6,-0.2,0.2,0.6,1),
                              labels=c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive"),
                              include.lowest = T, ordered_result = TRUE))

  results <- list(sent_data=s_data, sent_overall=s_overall)
  return(results)
}

sentimentWordPlot <- function(sent_data, n=10){

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
    slice_max(n, n=10)

  voc_sent<-sent_data %>%
    group_by(polarity, doc_pol_clas, doc_id) %>%
    distinct(lemma) %>%
    ungroup() %>%
    group_by(polarity, doc_pol_clas) %>%
    count(lemma, sort = TRUE) %>%
    filter(lemma %in% top_words$lemma) #%>%
    # left_join(sent_dist, by="doc_pol_clas") %>%
    # mutate(perc = n/doc_N*100)


  fig_pos <- voc_sent %>% dplyr::filter(polarity==1) %>%
    freqPlotlySentiment(x="n",y="lemma", xlabel="Polarized words count",ylabel="word", scale="identity", decimal=0)

  fig_neg <- voc_sent %>% dplyr::filter(polarity==-1) %>%
    freqPlotlySentiment(x="n",y="lemma", xlabel="Polarized words count",ylabel="word", scale="identity", decimal=0)

  plots <- list(positive=fig_pos, negative=fig_neg)
  return(plots)

}

sentimentPieChart <- function(df){
  plotly::plot_ly(data=df,values=~n,labels=~factor(Polarity),sort = FALSE,
                  marker=list(colors=paste0(polarity_colors(),"60")),
                  textposition = "outside",
                  type="pie", hole=0.4,
                  domain = list(x = c(0, 1), y = c(0, 1))) %>%
    layout(legend = list(x = -0.1, y = 0.9),
           xaxis = list(
             ticktext = list("Very Negative", "Negative", "Neutral", "Positive", "Very Positive"),
             tickvals = list(-0.8, -0.4, 0, 0.4, 0.8),
             tickmode = "array"
           )) %>%
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
    event_register("plotly_click")
}

sentimentDensityPlot <- function(x, from=-1, to=1){
  fit <- density(x, from=from,to=to)

  plot_ly(x = fit$x, y = fit$y, type = 'scatter', mode = 'lines', color=I("#6CC283"), fill='tozeroy',
          text=NULL,
          hoverinfo="text") %>%
    layout(xaxis = list(
      ticktext = list("Very Negative", "Negative", "Neutral", "Positive", "Very Positive"),
      tickvals = list(-0.8, -0.4, 0, 0.4, 0.8),
      tickmode = "array",
      zeroline = FALSE
    ),
    yaxis = list(domain=c(0,0.90)),
    annotations=list(text="Density Plot",xref="paper",x=0.5,
                     yref="paper",y=0.95,yshift=30,showarrow=FALSE,
                     font=list(size=20,color="gray30"))) %>%
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
           ))
}

sentimentBoxPlot <- function(sent_overall){
  plot_ly(data=sent_overall, x = ~round(sentiment_polarity,4), y="",type = "box",
          hoverinfo = 'x',
          boxpoints = "all", jitter = 0.3, color=I("#6CC283"),
          pointpos = -1.8) %>% layout(yaxis = list(zeroline = FALSE, showgrid = TRUE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.9)),
                                      xaxis = list(title ="", zeroline = FALSE, showgrid = TRUE, showline = FALSE, showticklabels = TRUE, range = c(-1,1),
                                                   ticktext = list("Very Negative", "Negative", "Neutral", "Positive", "Very Positive"),
                                                   tickvals = list(-0.8, -0.4, 0, 0.4, 0.8),
                                                   tickmode = "array"),
                                      plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                      paper_bgcolor = "rgba(0, 0, 0, 0)",
                                      annotations=list(text="Box Plot",xref="paper",x=0.5,
                                                       yref="paper",y=0.95,yshift=30,showarrow=FALSE,
                                                       font=list(size=20,color="gray30"))) %>%
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
           ))
}

### TEXT SUMMARIZATION: TEXTRANK -----

highlightSentences <- function(dfTag, id){
  df <- dfTag %>%
    filter(doc_id==id)

  n <- max(3,round(0.05*max(df$sentence_id)))

  sentences <- df %>%
    select(sentence_id,sentence) %>%
    distinct()

  terminology <- df %>%
    filter(POSSelected) %>%
    select(sentence_id, lemma)

  tr <- textrank_sentences(data = sentences, terminology = terminology)
  s <- tr$sentences %>%
    arrange(desc(textrank))

  n <- min(n,nrow(s))

  s$h <- c(rep(1,n),rep(0,nrow(s)-n))
  s <- s %>%
    left_join(df %>% select(paragraph_id,sentence_id) %>% distinct(), by = c("textrank_id"="sentence_id")) %>%
    mutate(sentence = ifelse(h==1, paste0("<mark><strong>", sentence, "</strong></mark>"), sentence)) %>%
    arrange(textrank_id) %>%
    group_by(paragraph_id) %>%
    summarize(paragraph=paste(sentence, collapse=" "),
              highlighted=ifelse(sum(h)>0,"Yes","No")) %>%
    filter(highlighted=="Yes") %>%
    arrange(paragraph_id) %>%
    select(paragraph_id,paragraph) %>%
    rename("Paragraph ID" = paragraph_id,
           "Paragraph" = paragraph)
  return(s)
}

textrankDocument <- function(dfTag, id){
  df <- dfTag[dfTag$doc_id==id,]

  #n <- max(3,round(0.05*max(df$sentence_id)))

  sentences <- df %>%
    select(sentence_id,sentence) %>%
    distinct()

  terminology <- df %>%
    filter(POSSelected) %>%
    select(sentence_id, lemma)

  tr <- textrank_sentences(data = sentences, terminology = terminology)
  s <- tr$sentences %>%
    arrange(desc(textrank))

  s <- s %>%
    left_join(df %>% select(paragraph_id,sentence_id) %>% distinct(), by = c("textrank_id"="sentence_id"))
  results <- list(s=s,id=id,sentences=tr$sentences %>% arrange(desc(textrank)))
  return(results)
}

abstractingDocument <- function(s,n,id){

  switch(n,
         "More Concise" ={n <- "5%"},
         "Less Concise" ={n <- "100%"},
         {n <- n}
  )
  n <- as.numeric(gsub("%","",n))
  n <- ceiling(n*nrow(s)/100) ## calculate n from %
  n <- min(n,nrow(s))

  s$h <- c(rep(1,n),rep(0,nrow(s)-n))
  # s <- s %>%
  #   left_join(df %>% select(paragraph_id,sentence_id) %>% distinct(), by = c("textrank_id"="sentence_id"))

  abstract <- s %>%
    filter(h==1) %>%
    group_by(paragraph_id) %>%
    summarize(paragraph = paste(sentence, collapse=" ")) %>%
    ungroup %>%
    summarize(text=paste(paragraph, collapse="<br><br>&nbsp&nbsp&nbsp&nbsp&nbsp")) %>%
    mutate(text= paste0("<h3>Document: <strong>",id,"</strong></h3><hr><br><em>",text,"</em>"))

  s <- s %>%
    mutate(sentence = ifelse(h==1, paste0("<mark><strong>", sentence, "</strong></mark>"), sentence)) %>%
    arrange(textrank_id) %>%
    group_by(paragraph_id) %>%
    summarize(paragraph=paste(sentence, collapse=" "),
              highlighted=ifelse(sum(h)>0,"Yes","No")) %>%
    #filter(highlighted=="Yes") %>%
    arrange(paragraph_id) %>%
    select(paragraph_id,paragraph) %>%
    rename("Paragraph ID" = paragraph_id,
           "Paragraph" = paragraph)

  results <- list(document=s, abstract=abstract$text[1])
  return(results)
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

# screenSh <- function(selector){
#   fileName <- tempfile(pattern = "figureImage",
#                        tmpdir = "",
#                        fileext = "") %>% substr(.,2,nchar(.))
#   if (is.null(selector)){
#     shinyscreenshot::screenshot(filename=fileName, download=FALSE, server_dir = tempdir())
#   } else {
#     shinyscreenshot::screenshot(selector=selector, filename=fileName, download=FALSE, server_dir = tempdir())
#   }
#   file <- paste(tempdir(),"/",fileName,".png",sep="")
#   return(file)
# }

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
## Labels sheets Report
dfLabel <- function(){
  short <- c("Empty Report",
             "Overview",
             "WordsFreq",
             # "Propn",
             # "Adj",
             # "Verb",
             # "MultiWords",
             "PoSFreq",
             "Clustering",
             "CorrespondenceAnalysis",
             "CoWord",
             "Grako",
             "KChoice",
             "ModelEstim",
             "PolarityDetection",
             "Summarization")


  long <- c("Empty Report",
            "Overview",
            "Words Frequency",
            #"Most used Words-NOUN",
            # "Most Used Words-PROPN",
            # "Most Used Words-ADJ",
            # "Most Used Words-VERB",
            # "Most Used Words-MULTIWORDS",
            "PoS Tag Frequency",
            "Clustering",
            "Correspondence Analysis",
            "Co-Word Analysis",
            "Grako",
            "TM-K choice",
            "TM-Model Estimation",
            "Polarity Detection",
            "Summarization")


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

## generic popup ###

popUpGeneric <- function(title=NULL, type="success", color=c("#1d8fe1","#913333","#FFA800"),
                  subtitle="",
                  btn_labels="OK"){
  showButton = TRUE
  timer = NA
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
    btn_colors = color,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}


### UTILITY FUNCTIONS ----
## Reset reactive values
resetValues <- function(){

  ### Initial values ----
  values <- list()
  values <- reactiveValues()
  values$resetNeed <- FALSE
  values$normButton <- FALSE
  values$path <- NULL
  values$menu <- -1
  values$custom_lists <- NULL
  values$txt <- data.frame()
  values$corpusElements <- data.frame()
  values$txtOriginal <- data.frame()
  values$list_file <- data.frame(sheet=NULL,file=NULL,n=NULL)
  values$POSTagSelected <- ""
  values$wb <-  openxlsx::createWorkbook()
  values$dfLabel <- dfLabel()
  values$posMwSel <- c("ADJ", "NOUN", "PROPN") # POS selected by default for multiword creation
  values$myChoices <- "Empty Report"

  languages <- langrepo()
  label_lang <- languages$repo
  names(label_lang) <- languages$short
  values$label_lang <- label_lang
  values$chapter <- languages$chapter
  values$TMplotIndex <- 1
  values$TMdocIndex <- 1
  values$tmTopSentences <- FALSE
  values$selectedGroups <- NULL
  values$selectedFilter <- ""

  return(values)
}

firstUp <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

## Color palette for plots
colorlist <- function(){
  c("#4DAF4A", "#E41A1C","#377EB8","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F"
    ,"#B3B3B3","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#8DD3C7","#BEBADA"
    ,"#FB8072","#80B1D3","#FDB462","#B3DE69","#D9D9D9","#BC80BD","#CCEBC5")
}


## POS selection functions ----
# Set TRUE PoS selected ##
posSel <- function(dfTag, pos){

  dfTag <- dfTag %>% mutate(POSSelected = ifelse(upos %in% pos, TRUE, FALSE))
  dfTag <- highlight(dfTag)
}

# remove Hapax and lowwer and higher lemmas
removeHapaxFreq <- function(dfTag,hapax,singleChar){

  #posTagFreq <- as.numeric(gsub("%","",posTagFreq))


  ## reset noHapax column
  dfTag <- dfTag %>%
    mutate(noHapax = TRUE)

  ## Hapax
  if (is.null(hapax)){
    H <- dfTag %>%
      group_by(lemma) %>%
      count() %>%
      filter(n==1) %>%
      select(lemma)
    H <- unique(H$lemma)
    dfTag <- dfTag %>%
      mutate(noHapax = ifelse(lemma %in% H,FALSE,TRUE))
  }

  ## Single Char
  if (is.null(singleChar)){
    dfTag <- dfTag %>%
      mutate(noSingleChar = ifelse(nchar(lemma)>1,TRUE,FALSE))
  }

  # ## reset FrequencyRange column
  # dfTag <- dfTag %>%
  #   mutate(FrequencyRange = TRUE)
  #
  #
  # # min and max frequency
  # Freq <- dfTag %>%
  #   group_by(doc_id,lemma) %>%
  #   count() %>%
  #   ungroup() %>%
  #   group_by(doc_id) %>%
  #   mutate(perc=n/sum(n)*100) %>%
  #   ungroup() %>%
  #   dplyr::filter(perc<posTagFreq[1]|perc>posTagFreq[2])
  #
  # FREQ <- unique(Freq$lemma)
  #
  # if (length(FREQ)>0){
  #   dfTag <- dfTag %>%
  #     mutate(FrequencyRange = ifelse(lemma %in% FREQ,FALSE,TRUE))
  # }

  return(dfTag)
}

# Select lemmas by PoS Tags, Hapax and Frequency Range #
LemmaSelection <- function(dfTag){
  if (!"noHapax" %in% names(dfTag)){
    dfTag <- dfTag %>%
      mutate(noHapax = TRUE)
  }
  if (!"noSingleChar" %in% names(dfTag)){
    dfTag <- dfTag %>%
      mutate(noSingleChar = TRUE)
  }

  dfTag <- dfTag %>%
    dplyr::filter(POSSelected,noHapax,noSingleChar) %>%
    arrange(doc_id, paragraph_id, sentence_id)

  return(dfTag)
}


# ## Highlight function ----

highlight <- function(df){
  ## create highlighted tokens
  posUnsel <- c("PUNCT","X","SYM","NUM", "NGRAM_MERGED")

  df <- df %>%
    group_by(token) %>%
    mutate(token_hl = ifelse(!upos %in% posUnsel,
                             paste0("<mark><strong>", token, "</strong></mark>"), token),
           token_hl = ifelse(upos=="MULTIWORD", paste0("<mark><strong>", lemma, "</strong></mark>"), token_hl)
    ) %>%
    group_by(doc_id,sentence_id) %>%
    mutate(start_hl = start-(first(start)-1),
           end_hl = start_hl+(end-start)) %>%
    mutate(sentence_hl = ifelse(!upos %in% posUnsel,
                                paste0(substr(sentence,0,start_hl-1),token_hl,substr(sentence,end_hl+1,nchar(sentence))),
                                sentence)) %>% ungroup()
}

## saveTall function ----
saveTall <- function(dfTag,custom_lists,language,menu,where,file){
  D <- date()
  D <- strsplit(gsub("\\s+", " ", D)," ")
  D <- paste(unlist(D)[c(1,2,3,5)],collapse=" ")
  save(dfTag,custom_lists,language,menu,D,where,file=file)
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
             menuItem("Import", tabName = "import_tx", icon = icon("open-file", lib="glyphicon")),
             menuItem("Edit", tabName = "edit_tx", icon = icon("edit", lib="glyphicon"),
                      menuSubItem("Split", tabName = "split_tx", icon = icon("chevron-right")),
                      menuSubItem("Random Selection", tabName = "randomText", icon = icon("chevron-right")),
                      menuSubItem("External Information", tabName = "extInfo", icon = icon("chevron-right"))),
             menuItem("Pre-processing", tabName = "prePro", icon = icon("indent-right", lib="glyphicon"), startExpanded = TRUE,
                      menuSubItem("Tokenization & PoS Tagging", tabName = "tokPos",icon = icon("chevron-right"), selected = TRUE)
             ),
             menuItem("Settings",tabName = "settings", icon = icon("tasks"))
           )
         },
         "1"={
           list(
             menuItem("Import", tabName = "import_tx", icon = icon("open-file", lib="glyphicon")),
             # menuItem("Edit", tabName = "edit_tx", icon = icon("edit", lib="glyphicon"),
             #          menuSubItem("Split", tabName = "split_tx", icon = icon("chevron-right")),
             #          menuSubItem("Random Selection", tabName = "randomText", icon = icon("chevron-right")),
             #          menuSubItem("External Information", tabName = "extInfo", icon = icon("chevron-right"))),
             menuItem("Pre-processing", tabName = "prePro", icon = icon("indent-right", lib="glyphicon"), startExpanded = TRUE,
                      menuSubItem("Tokenization & PoS Tagging", tabName = "tokPos",icon = icon("chevron-right")),
                      menuSubItem("Tagging Special Entities", tabName = "posSpecial",icon = icon("chevron-right")),
                      menuItem("Multi-Word", tabName = "multiword", icon = icon("chevron-right"), startExpanded = TRUE,
                               menuSubItem("Automatic", tabName = "multiwordCreat",icon = icon("chevron-right")),
                               menuSubItem("By a List", tabName = "multiwordByList",icon = icon("chevron-right"))),
                      menuSubItem("Custom Term Lists", tabName = "custTermList",icon = icon("chevron-right"), selected = TRUE),
                      menuSubItem("PoS Tag Selection", tabName = "posTagSelect",icon = icon("chevron-right"))
             ),
             menuItem("Settings",tabName = "settings", icon = icon("tasks"))
           )
         },
         "2"={
           list(
             menuItem("Import", tabName = "import_tx", icon = icon("open-file", lib="glyphicon")),
             # menuItem("Edit", tabName = "edit_tx", icon = icon("edit", lib="glyphicon"),
             #          menuSubItem("Split", tabName = "split_tx", icon = icon("chevron-right")),
             #          menuSubItem("Random Selection", tabName = "randomText", icon = icon("chevron-right")),
             #          menuSubItem("External Information", tabName = "extInfo", icon = icon("chevron-right"))),
             menuItem("Pre-processing", tabName = "prePro", icon = icon("indent-right", lib="glyphicon"), startExpanded = TRUE,
                      # menuItem("Text Normalization", tabName = "TextNorm", icon = icon("chevron-right"), startExpanded = TRUE,
                      #          menuSubItem("Explore Tags", tabName = "textNormExpl",icon = icon("chevron-right"), selected = TRUE),
                      #          menuSubItem("Remove Tags", tabName = "textNormRemove",icon = icon("chevron-right"), selected = TRUE)
                      # ),
                      # menuSubItem("Tokenization & PoS Tagging", tabName = "tokPos",icon = icon("chevron-right")),
                      menuSubItem("Tagging Special Entities", tabName = "posSpecial",icon = icon("chevron-right")),
                      menuItem("Multi-Word", tabName = "multiword", icon = icon("chevron-right"), startExpanded = TRUE,
                               menuSubItem("Automatic", tabName = "multiwordCreat",icon = icon("chevron-right")),
                               menuSubItem("By a List", tabName = "multiwordByList",icon = icon("chevron-right"))),
                      menuSubItem("Custom Term Lists", tabName = "custTermList",icon = icon("chevron-right")),
                      menuSubItem("PoS Tag Selection", tabName = "posTagSelect",icon = icon("chevron-right")), selected = TRUE),
             # menuItem("Filter", tabName = "filter_text", icon = icon("filter")),
             # menuItem("Groups",tabName = "defineGroups", icon = icon("th", lib="glyphicon")),
             menuItem("Overview", tabName = "overview", icon = icon("search", lib="glyphicon")),
             menuItem("Words", tabName = "words", icon = icon("font", lib = "glyphicon"),
                      menuItem("Frequencies", tabName = "freqList", icon = icon("chevron-right"),
                               menuSubItem("Words", tabName = "w_freq", icon = icon("chevron-right")),
                               menuSubItem("Part of Speech", tabName = "w_pos", icon = icon("chevron-right"))),
                      menuSubItem("Words in Context", tabName = "wordCont", icon = icon("chevron-right")),
                      menuSubItem("Clustering", tabName = "w_clustering", icon = icon("chevron-right")),
                      menuSubItem("Correspondence Analysis", tabName = "ca", icon = icon("chevron-right")),
                      menuItem("Network", tabName = "w_network", icon = icon("chevron-right"),
                               menuSubItem("Co-word analysis", tabName = "w_networkCooc", icon = icon("chevron-right"))
                               # ,menuSubItem("Grako", tabName = "w_networkGrako", icon = icon("chevron-right"))
                               )),
             menuItem("Documents",tabName = "documents", icon = icon(name="duplicate", lib="glyphicon"),
                      menuItem("Topic Modeling", tabName = "d_topicMod", icon = icon("chevron-right"),
                               menuSubItem("K choice", tabName = "d_tm_select", icon = icon("chevron-right")),
                               menuSubItem("Model Estimation", tabName = "d_tm_estim", icon = icon("chevron-right"))),
                      menuSubItem("Polarity Detection", tabName = "d_polDet", icon = icon("chevron-right")),
                      menuSubItem("Summarization", tabName = "d_summarization", icon = icon("chevron-right"))),
             menuItem("Report",tabName = "report", icon = icon("list-alt")),
             menuItem("Settings",tabName = "settings", icon = icon("tasks"))
           )
         },
         "3"={
           list(
             menuItem("Import", tabName = "import_tx", icon = icon("open-file", lib="glyphicon")),
             # menuItem("Edit", tabName = "edit_tx", icon = icon("edit", lib="glyphicon"),
             #          menuSubItem("Split", tabName = "split_tx", icon = icon("chevron-right")),
             #          menuSubItem("Random Selection", tabName = "randomText", icon = icon("chevron-right")),
             #          menuSubItem("External Information", tabName = "extInfo", icon = icon("chevron-right"))),
             menuItem("Pre-processing", tabName = "prePro", icon = icon("indent-right", lib="glyphicon"), startExpanded = TRUE,
                      # menuItem("Text Normalization", tabName = "TextNorm", icon = icon("chevron-right"), startExpanded = FALSE,
                      #          menuSubItem("Explore Tags", tabName = "textNormExpl",icon = icon("chevron-right"), selected = TRUE),
                      #          menuSubItem("Remove Tags", tabName = "textNormRemove",icon = icon("chevron-right"), selected = TRUE)
                      # ),
                      # menuSubItem("Tokenization & PoS Tagging", tabName = "tokPos",icon = icon("chevron-right")),
                      menuSubItem("Tagging Special Entities", tabName = "posSpecial",icon = icon("chevron-right")),
                      menuItem("Multi-Word", tabName = "multiword", icon = icon("chevron-right"), startExpanded = TRUE,
                      menuSubItem("Automatic", tabName = "multiwordCreat",icon = icon("chevron-right")),
                      menuSubItem("By a List", tabName = "multiwordByList",icon = icon("chevron-right"))),
                      menuSubItem("Custom Term Lists", tabName = "custTermList",icon = icon("chevron-right")),
                      menuSubItem("PoS Tag Selection", tabName = "posTagSelect",icon = icon("chevron-right")), selected = TRUE),
             menuItem("Filter", tabName = "filter_text", icon = icon("filter")),
             menuItem("Groups",tabName = "defineGroups", icon = icon("th", lib="glyphicon")),
             menuItem("Overview", tabName = "overview", icon = icon("search", lib="glyphicon")),
             menuItem("Words", tabName = "words", icon = icon("font", lib = "glyphicon"),
                      menuItem("Frequencies", tabName = "freqList", icon = icon("chevron-right"),
                               menuSubItem("Words", tabName = "w_freq", icon = icon("chevron-right")),
                               menuSubItem("Part of Speech", tabName = "w_pos", icon = icon("chevron-right"))),
                      menuSubItem("Words in Context", tabName = "wordCont", icon = icon("chevron-right")),
                      menuSubItem("Clustering", tabName = "w_clustering", icon = icon("chevron-right")),
                      menuSubItem("Correspondence Analysis", tabName = "ca", icon = icon("chevron-right")),
                      menuItem("Network", tabName = "w_network", icon = icon("chevron-right"),
                               menuSubItem("Co-word analysis", tabName = "w_networkCooc", icon = icon("chevron-right"))
                               # ,menuSubItem("Grako", tabName = "w_networkGrako", icon = icon("chevron-right"))
                               )),
             menuItem("Documents",tabName = "documents", icon = icon(name="duplicate", lib="glyphicon"),
                      menuItem("Topic Modeling", tabName = "d_topicMod", icon = icon("chevron-right"),
                               menuSubItem("K choice", tabName = "d_tm_select", icon = icon("chevron-right")),
                               menuSubItem("Model Estimation", tabName = "d_tm_estim", icon = icon("chevron-right"))),
                      menuSubItem("Polarity Detection", tabName = "d_polDet", icon = icon("chevron-right")),
                      menuSubItem("Summarization", tabName = "d_summarization", icon = icon("chevron-right"))),
             menuItem("Report",tabName = "report", icon = icon("list-alt")),
             menuItem("Settings",tabName = "settings", icon = icon("tasks"))
           )
         },
         {
           list(
             menuItem("Import", tabName = "import_tx", icon = icon("open-file", lib="glyphicon")),
             menuItem("Settings",tabName = "settings", icon = icon("tasks"))
           )
         }
  )
}

# DATA TABLE FORMAT ----
DTformat <- function(df, nrow=10, filename="Table", pagelength=TRUE, left=NULL, right=NULL, numeric=NULL, dom=TRUE, size='85%', filter="top",
                     columnShort=NULL, columnSmall=NULL, round=2, title="", button=FALSE, delete=FALSE, escape=FALSE, selection=FALSE, specialtags=FALSE){

  if ("text" %in% names(df)){
    df <- df %>%
      mutate(text = gsub("<|>","",text))
  }

  if (length(columnShort)>0){
    columnDefs = list(list(
      className = 'dt-center', targets = 0:(length(names(df)) - 1)),
      list(
        targets = columnShort-1,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 500 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 500) + '...</span>' : data;",
          "}")
      ))
  } else{
    columnDefs = list(list(
      className = 'dt-center', targets = 0:(length(names(df)) - 1)
    ))
  }

  if (isTRUE(pagelength)){
    buttons = list(
      list(extend = 'pageLength'),
      list(extend = 'excel',
           filename = paste0(filename,"_tall_",Sys.Date()),
           title = " ",
           header = TRUE,
           exportOptions = list(
             modifier = list(page = "all")
           ))
      )
  } else{
    buttons = list(
      list(extend = 'excel',
           filename = paste0(filename,"_tall_",Sys.Date()),
           title = " ",
           header = TRUE,
           exportOptions = list(
             modifier = list(page = "all")
           )))
  }

  if (isTRUE(dom)){
    dom <- "Brtip"
  } else{
    dom <- "Bt"
  }

  if (nchar(title)>0){
    caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black;  font-size:140% ;',title)
  } else {
    caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black;  font-size:140% ;',"")
  }

  if (isTRUE(button)){
      df <- df %>%
        mutate(Document = glue::glue('<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'{doc_id}\')">View</button>')) %>%
        select(Document, everything())
  }

  if (isTRUE(specialtags)){
    df <- df %>%
      mutate(Table = glue::glue('<button id2="custom_btn" onclick="Shiny.onInputChange(\'button_id2\', \'{UPOS}\')">View</button>')) %>%
      select(Table, everything())
  }

  if (isTRUE(delete)){
    df <- df %>%
      mutate(Remove = glue::glue('<button id="custom_btn_del" onclick="Shiny.onInputChange(\'button_id_del\', \'{doc_id}\')">Remove</button>')) %>%
      select(Document, Remove, everything())
  }

  if (isTRUE(selection)){
    extensions = c("Buttons", "Select", "ColReorder", "FixedHeader")
    buttons <- c(buttons, c('selectAll', 'selectNone'))
    select <- list(style='multiple', items='row', selected = 1:nrow(df))
    #selection = list(mode = 'multiple', selected = 1:nrow(df), target = 'row')
  } else {
    extensions = c("Buttons", "ColReorder", "FixedHeader")
    select <- NULL
    #selection = "none"
  }

  tab <- DT::datatable(df, escape = escape,rownames = FALSE,
                       caption = caption,
                       selection= "none",
                       extensions = extensions,
                       filter = filter,
                       options = list(
                         colReorder = TRUE,
                         fixedHeader = TRUE,
                         pageLength = nrow,
                         autoWidth = FALSE, scrollX = TRUE,
                         dom = dom,
                         buttons = buttons,
                         select = select,
                         lengthMenu = list(c(10, 25, 50, -1),
                                           c('10 rows', '25 rows', '50 rows', 'Show all')),
                         columnDefs = columnDefs
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

  if (!is.null(left)){
    tab <- tab %>%
      DT::formatStyle(
        names(df)[left],
        backgroundColor = 'white',
        textAlign = 'left',
        fontSize = size
      )
  }

  # right aligning
  if (!is.null(right)){
    tab <- tab %>%
      DT::formatStyle(
        names(df)[right],
        backgroundColor = 'white',
        textAlign = 'right',
        fontSize = size
      )
  }

  # numeric round
  if (!is.null(numeric)){
    tab <- tab %>%
      formatRound(names(df)[c(numeric)], digits=round)
  }

  tab
}



### FUNCTIONS FOR EXPORTING PLOTS ----

plot2png <- function(p, filename, zoom = 2, type="vis"){
  html_name <- tempfile(fileext = ".html")
  switch(type,
         vis={
           visSave(p, html_name)
         },
         plotly={
           htmlwidgets::saveWidget(p, file=html_name)
         })

  webshot2::webshot(html_name, zoom = zoom, file = filename)
}

## freqGgplot ----
## ggplot for frequency plots to download

freqGgplot <- function(df,x=2,y=1,n=20, title="NOUN Frequency"){

  df <- df %>% dplyr::slice_head(n=n) %>%
    data.frame()
  g <- ggplot(df, aes(x =df[,x], y = df[,y], label = df[,x])) +
    geom_col(color = "#c3d1be", fill="#96af8e")+
    geom_text(aes(label=df[,x]), position=position_dodge(width=0.9), hjust=-0.4, color="#4f7942", size=3.7)+
    labs(title=title, y="", x = "Frequency")+
    scale_y_discrete(limits = rev(df[,y]))+
    scale_x_continuous(limits=c(0,df[,x]+max(df[,x])*0.06), expand = c(0,0))+
    theme(axis.text.y  = element_text(angle=0, hjust=0, size=9),
          axis.text.x  = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  return(g)
}

topicGplot <- function(x, nPlot=10, type="beta"){

  beta_long <- x %>%
    pivot_longer(cols=2:ncol(.), names_to = "topic", values_to = "probability") %>%
    group_by(topic) %>%
    slice_max(order_by=probability, n=nPlot) %>%
    arrange(desc(probability), .by_group = T) %>%
    ungroup() %>%
    mutate(topic = paste0("topic ", topic))

  switch(type,
         beta={
           g <- ggplot(beta_long, aes(x=probability, y=word, fill = factor(topic)))
         },
         theta={
           g <- ggplot(beta_long, aes(x=probability, y=doc, fill = factor(topic)))
         })
  g + geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    theme(axis.text.y  = element_text(angle=0, hjust=0, size=9),
          axis.text.x  = element_text(size=10),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())

}

### deleteCache ------
deleteCache <- function(){
  switch(Sys.info()[['sysname']],
         Windows= {home <- Sys.getenv('R_USER')},
         Linux  = {home <- Sys.getenv('HOME')},
         Darwin = {home <- Sys.getenv('HOME')})

  # setting up the main directory
  path_tall <- file.path(home,"tall")
  result <- unlink(path_tall, recursive = TRUE)
  btn_labels="OK"


  if (result==0){
    subtitle <- paste0("The folder '",path_tall,"' \nand files contained in it have been correctly deleted")
    title <- ""
    btn_colors = "#1d8fe1"
    showButton = TRUE
    timer = 3000
    type = "success"
  } else {
    subtitle <- paste0("The folder '",path_tall,"' \ndoes not exist or you don't have permissions to delete it")
    title <- ""
    btn_colors = "#913333"
    showButton = TRUE
    timer = 3000
    type="error"
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

#### language models repo ----
langrepo <- function(){
  known_models<- c(
    "english-ewt", "english-gum", "english-lines",
    "english-partut","italian-isdt",
    "italian-partut", "italian-postwita",
    "italian-twittiro", "italian-vit",
    "french-gsd", "french-partut", "french-sequoia",
    "french-spoken",
    "german-gsd", "german-hdt",
    "spanish-ancora", "spanish-gsd",
    "afrikaans-afribooms",
    "ancient_greek-perseus", "ancient_greek-proiel", "arabic-padt",
    "armenian-armtdp", "basque-bdt", "belarusian-hse", "bulgarian-btb",
    "catalan-ancora", "chinese-gsd", "chinese-gsdsimp", "classical_chinese-kyoto",
    "coptic-scriptorium", "croatian-set", "czech-cac", "czech-cltt",
    "czech-fictree", "czech-pdt", "danish-ddt", "dutch-alpino",
    "dutch-lassysmall", "estonian-edt", "estonian-ewt", "finnish-ftb",
    "finnish-tdt",  "galician-ctg", "galician-treegal",
    "gothic-proiel", "greek-gdt",
    "hebrew-htb", "hindi-hdtb", "hungarian-szeged", "indonesian-gsd",
    "irish-idt", "italian-isdt", "japanese-gsd", "korean-gsd",
    "korean-kaist", "latin-ittb", "latin-perseus", "latin-proiel",
    "latvian-lvtb", "lithuanian-alksnis", "lithuanian-hse",
    "maltese-mudt", "marathi-ufal", "north_sami-giella",
    "norwegian-bokmaal", "norwegian-nynorsk", "norwegian-nynorsklia",
    "old_church_slavonic-proiel", "old_french-srcmf", "old_russian-torot",
    "persian-seraji", "polish-lfg", "polish-pdb", "portuguese-bosque",
    "portuguese-gsd", "romanian-nonstandard", "romanian-rrt",
    "russian-gsd", "russian-syntagrus", "russian-taiga",
    "scottish_gaelic-arcosg", "serbian-set", "slovak-snk",
    "slovenian-ssj", "slovenian-sst",
    "swedish-lines", "swedish-talbanken", "tamil-ttb", "telugu-mtg",
    "turkish-imst", "ukrainian-iu", "urdu-udtb", "uyghur-udt",
    "vietnamese-vtb", "wolof-wtb")

  default_models <-  gsub("-.*","",known_models)

  chapter_vec <- c(
    "CHAPTER","CAPITOLO","CHAPITRE","KAPITEL","CAPÍTULO","HOOFSTUK"  ,"ΚΕΦΑΛΑΙΟ","فصل","ԳԼՈՒԽ","KAPITULU","ГЛАВА","ГЛАВА",
    "CAPÍTOL","章","章","ⲡⲏⲡⲟⲗⲁⲓⲟⲥ", "POGLAVLJE", "KAPITOLA",  "KAPITEL",   "HOOFDSTUK", "PEATÜKK",   "LUKU",      "CAPÍTULO",  "KAPITULS",
    "ΚΕΦΑΛΑΙΟ",  "פרק",       "अध्याय",     "FEJEZET",   "BAB",       "CAIBIDIL",  "章"  ,      "장"  ,      "CAPITULUM", "NODAĻA",    "SKYRIUS",   "KAPITOLU",
    "अध्याय",     "CAŊÁLAŠ",   "KAPITTEL",  "ГЛАВА" ,    "CHAPITRE",  "ГЛАВА" ,    "فصل"  ,     "ROZDZIAŁ",  "CAPÍTULO",  "CAPITOL",   "ГЛАВА"  ,   "CAIBIDIL" ,
    "ГЛАВА" ,    "KAPITOLA" , "POGLAVJE"  ,"KAPITEL"  , "அதிர்வேகம்",  "అధ్యాయం"  ,   "BÖLÜM",     "ГЛАВА",     "باب",       "BAB",       "CHƯƠNG",    "KOW")

  languages <- tibble(short=default_models, repo=known_models) %>%
    group_by(short) %>%
    mutate(repo=repo[1]) %>%
    distinct()

  languages$chapter <- chapter_vec
  return(languages)
}

## RESET MODAL DIALOG INPUTS
resetModalButtons <- function(session){
  session$sendCustomMessage("button_id", 'null')
  session$sendCustomMessage("click", 'null')
  session$sendCustomMessage("click-dend", 'null')
  runjs("Shiny.setInputValue('plotly_click-A', null);")
  return(session)
}

#

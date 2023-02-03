# Button for Folder Selection ----

choose.dir = function(default = NA, caption = NA, useNew=TRUE) {
  if (Sys.info()['sysname'] == 'Darwin') {
    return(choose.dir.darwin(default = default, caption = caption))
  } else if (Sys.info()['sysname'] == 'Linux') {
    return(choose.dir.linux(default = default, caption = caption))
  } else if (Sys.info()['sysname'] == 'Windows') {
    # Use batch script to circumvent issue w/ `choose.dir`/`tcltk::tk_choose.dir`
    # window popping out unnoticed in the back of the current window
    return(choose.dir.windows(default = default, caption = caption, useNew = useNew))
  }
  return(paste("Error: don't know how to show a folder dialog in", Sys.info()['sysname']) )
}

choose.dir.darwin <- function(default = NA, caption = NA) {
  command = 'osascript'
  args = '-e "POSIX path of (choose folder{{prompt}}{{default}})"'

  if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
    prompt = sprintf(' with prompt \\"%s\\"', caption)
  } else {
    prompt = ''
  }
  args = sub('{{prompt}}', prompt, args, fixed = T)

  if (!is.null(default) && !is.na(default) && nzchar(default)) {
    default = sprintf(' default location \\"%s\\"', path.expand(default))
  } else {
    default = ''
  }
  args = sub('{{default}}', default, args, fixed = T)

  suppressWarnings({
    path = system2(command, args = args, stderr = TRUE)
  })
  if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
    # user canceled
    path = NA
  } else {
    # cut any extra output lines, like "Class FIFinderSyncExtensionHost ..."
    path = tail(path, n=1)
  }

  return(path)
}

choose.dir.linux <- function(default = NA, caption = NA) {
  command = 'zenity'
  args = '--file-selection --directory'

  if (!is.null(default) && !is.na(default) && nzchar(default)) {
    args = paste(args, sprintf('--filename="%s"', default))
  }

  if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
    args = paste(args, sprintf('--title="%s"', caption))
  }

  suppressWarnings({
    path = system2(command, args = args, stderr = TRUE)
  })

  #Return NA if user hits cancel
  if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
    # user canceled
    return(NA)
  }

  #Error: Gtk-Message: GtkDialog mapped without a transient parent
  if(length(path) > 1){
    path = path[(length(path))]
  }

  return(path)
}

choose.dir.windows <- function(default = NA, caption = NA, useNew = TRUE) {
  if(useNew){
    ## uses a powershell script rather than the bat version, gives a nicer interface
    ## and allows setting of the default directory and the caption
    whereisutils <- system.file("utils", 'newFolderDialog.ps1', package = "tall")
    command = 'powershell'
    args = paste('-NoProfile -ExecutionPolicy Bypass -File',normalizePath(whereisutils))
    if (!is.null(default) && !is.na(default) && nzchar(default)) {
      args = paste(args, sprintf('-default "%s"', normalizePath(default)))
    }

    if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
      args = paste(args, sprintf('-caption "%s"', caption))
    }

    suppressWarnings({
      path = system2(command, args = args, stdout = TRUE)
    })
   } else {
    whereisutils <- system.file("utils", 'choose_dir.bat', package = "tall")
    command = normalizePath(whereisutils)
    args = if (is.na(caption)) '' else sprintf('"%s"', caption)
    suppressWarnings({
      path = system2(command, args = args, stdout = TRUE)
    })
  }
  if (path == 'NONE') path = NA
  return(path)
}

directoryInput = function(inputId, label, value = NULL) {
  if (!is.null(value) && !is.na(value)) {
    value = path.expand(value)
  }
  version <- as.character(packageVersion("tall")[[1]])
  dep <- htmltools::htmlDependency(
    name = "tall-assets", version = version,
    package = "tall",
    src = "assets",
    script = "js/directory_input_binding.js"
  )
  tagList(
    shiny::div(
      class = 'form-group directory-input-container',
      style = 'width:100% ',

      `%AND%`(label, tags$label(label, style = 'width:100% ')),
      shiny::div(
        shiny::span(
        #    class = 'col-xs-9 col-md-11',
            style = 'width:100% ',#'padding-left: 70px; padding-right: 0px',
        #shiny::div(
        class = 'input-group shiny-input-container'),
        style = 'width:100%;',
        shiny::span(
          class = 'shiny-input-container',
          style =  'width:100% ',
          tags$button(
            id = inputId,
            style =  'height:40px; width:100% ', #'padding-left: 30px; padding-right: 30px;
            title="Browse", # Tips
            class = 'btn btn-default directory-input',icon('folder-open', lib="glyphicon")
            #)
          ),

          # tags$input(
          #   id = sprintf('%s__chosen_dir', inputId),
          #   value = value,
          #   type = 'text',
          #   class = 'form-control directory-input-chosen-dir',
          #   style = 'font-size: 12px;', #align??
          #   readonly = 'readonly'
          # )
        ),
        #' shiny::span(
        #'   class = 'shiny-input-container',
        #'   tags$button(
        #'     id = inputId,
        #'     style = 'padding-left: 20px; padding-right: 20px',
        #'
        #'     class = 'btn btn-default directory-input',icon('folder-open', lib="glyphicon")
        #'     #'...'
        #'   )
        #' )
      )
    ),
    dep
  )

}

updateDirectoryInput = function(session, inputId, value = NULL, ...) {
  if (is.null(value)) {
    value = choose.dir(...)
  }
  session$sendInputMessage(inputId, list(chosen_dir = value))
}

readDirectoryInput = function(session, inputId) {
  session$input[[sprintf('%s__chosen_dir', inputId)]]
}

# AND infix operator ----
## Given x and y, return y only if both x and y are set
`%AND%` <- function(x, y) {
  if (!is.null(x) && !isTRUE(is.na(x)))
    if (!is.null(y) && !isTRUE(is.na(y)))
      return(y)
  return(NULL)
}



# IMPORT TEXT FUNCTIONS ----

read_files <- function(path, ext=c("txt","csv", "xlsx"), subfolder=TRUE){

  files <- list.files(path=path, pattern = paste0(".",ext,"$"), recursive = subfolder)

  if (!length(files)>0) return(data.frame(doc_id=NA,text=NA,path=path))

  files <- paste0(path,"/",files)
  doc_id <- unlist(lapply(strsplit(files,"/"), function(l){l[length(l)]}))

  switch(ext,
         txt={
           df <- data.frame(doc_id=doc_id,text=NA,folder=NA,files=files) %>%
             group_by(doc_id) %>%
             mutate(folder = gsub(paste0("/",doc_id),"",files),
                    text = gsub("\\.\\.","\\.",paste(read_lines(files,skip_empty_rows = TRUE),sep="",collapse=". "))) %>%
             select(-files)
         },
         csv={
           listdf <- list()
           for (i in seq_len(length(files))){
             listdf[[i]] <- read_csv(files[i], show_col_types=FALSE) %>%
               mutate(doc_id = doc_id[i],
                      folder = gsub(paste0("/",doc_id[i]),"",files[i]))
           }

           df <- do.call(rbind,listdf)
         },

         xlsx={
           df <- readxl::read_excel(files, col_types = "text")
         }
         )

return(df)
}

shortpath <- function(path){
  if (inherits(path,"character")){
    unlist(lapply(strsplit(path,"/"), function(l){
      l[length(l)]
    }))
  } else {NULL}
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
    mutate(upos.x = ifelse(!is.na(upos.y),toupper(upos.y),upos.x)) %>%
    select(-upos.y) %>%
    rename(upos = upos.x)

}

### 3. POS TAG SELECTION ----

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
                                        'Other'),
                          selected=c(TRUE,
                                     FALSE,
                                     FALSE,
                                     FALSE,
                                     FALSE,
                                     FALSE,
                                     FALSE,
                                     TRUE,
                                     FALSE,
                                     FALSE,
                                     FALSE,
                                     TRUE,
                                     FALSE,
                                     FALSE,
                                     FALSE,
                                     TRUE,
                                     FALSE))

  pos <- sort(unique(df$upos))
  additionalPos <- setdiff(pos, posLegend$pos)
  ordinaryPos <- pos[!pos %in% additionalPos]
  description <- c(posLegend$description[posLegend$pos %in% pos], rep("Custom PoS", length(additionalPos)))
  description <- paste(c(ordinaryPos, additionalPos),description,sep=": ")
  selected <- c(posLegend$selected[posLegend$pos %in% pos], rep(FALSE, length(additionalPos)))
  obj <- data.frame(pos=pos, description=description, selected=selected)
  return(obj)
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

# DATA TABLE FORMAT ----
DTformat <- function(df, nrow=10){
  # da completare
  DT::datatable(df,escape = FALSE,rownames = FALSE, extensions = c("Buttons"),
                options = list(
                  pageLength = nrow,
                  autoWidth = FALSE, scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(list(extend = 'pageLength'),
                                 list(extend = 'print')),
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
      fontSize = '85%'
    )
}














### QUANTEDA CORPUS FUNTIONS ----

## convert a corpus obj into a tibble ----
corpus2df <- function(obj){
  df <- tibble(doc_id=rep(names(obj), lengths(obj)), text=unlist(obj))
  docvars <- attr(obj, "docvars")
  df <- df %>% left_join(docvars, by = c("doc_id" = "docid_"))
}


## calculate frequency distribution for wordcloud, etc. ----
distrib <- function(obj, scale="identity"){
  obj <- obj %>%
    corpus2df() %>%
    count(text) %>%
    arrange(desc(n))

  switch(scale,
         identity={
           obj$scale <- obj$n
         },
         log={
           obj$scale <- log(obj$n)
         }
  )
  return(obj)
}

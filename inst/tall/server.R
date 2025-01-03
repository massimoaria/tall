##  Server ####
source("tallFunctions.R", local=TRUE)

## suppress warnings
options(warn = -1)

## file upload max size
maxUploadSize <- 200 # default value
maxUploadSize <- getShinyOption("maxUploadSize", maxUploadSize)
options(shiny.maxRequestSize=maxUploadSize*1024^2)

param_stay_page <- FALSE

server <- function(input, output, session){

  if(inherits(try(pagedown::find_chrome(), silent=T), "try-error")) {
    Chrome_url <- NULL
  }else{
    Chrome_url <- pagedown::find_chrome()
  }

  #  Sys.setenv (CHROMOTE_CHROME = Chrome_url)

  ## chrome configuration for shinyapps server

  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c("--disable-gpu",
                 "--no-sandbox",
                 "--disable-dev-shm-usage", # required bc the target easily crashes
                 c("--force-color-profile", "srgb"))
      ))
    )
  }
  ## end configuration

  ## Check if Chrome browser is installed on the computer
  if(is.null(Chrome_url)){
    showModal(modalDialog(
      title = strong("Warning message!"),
      HTML("Chrome or a Chromium-based browser is not installed on your computer.<br>
If you do not have either of these browsers installed, TALL will be unable to export graphs.<br>
To ensure the functionality of TALL,
           please download Chrome by <a href='https://www.google.com/chrome/' target='_blank' > <b>clicking here</b></a>."),
      footer = modalButton("Dismiss"),
      easyClose = TRUE
    ))
  } else {
    Sys.setenv (CHROMOTE_CHROME = Chrome_url)
  }

  ## Code to reset shiny app
  reset_rv <- reactiveVal(value = 0L)
  session$onSessionEnded(function(){
    #x <- Inf
    x <- isolate(reset_rv())

    if (!is.null(x)){
      if(x==0) {
        stopApp()
      }
    }
  })
  ###

  output$resetButton <- renderUI({
    reset_bttn <- list(
      label = NULL,
      style="margin-top: -8px; font-size: 8px; border-radius:2%",
      #style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
      icon = icon(name ="refresh", lib="glyphicon")
    )
    do.call("actionButton", c(reset_bttn, list(
      inputId = "resetApp")
    ))
  })

  observeEvent(input$resetApp, {
    ask_confirmation(
      inputId = "reset_confirmation",
      title = "Restart TALL",
      text = HTML('Restarting TAll will result in the loss of all analyses currently in progress<br><br>
                  <b>Do you want to confirm?</b>'),
      html=TRUE,
      type = "warning",
      btn_labels = c("CANCEL", "CONFIRM")
    )
  })

  observeEvent(input$reset_confirmation, {
    if (isTRUE(input$reset_confirmation)){
    reset_rv(input$resApp)
    session$reload()
    param_stay_page <<- TRUE
    }
  })

  if(param_stay_page){
    updateTabItems(session, "sidebarmenu", "import_tx")
    param_stay_page_newPT <<- FALSE
  }


  ## suppress summarise message
  options(dplyr.summarise.inform = FALSE)
  # languages <- langrepo()

  ### Initial values ----
  values <- resetValues()

  ## Setting plot values
  values$h <- 7
  values$zoom <- 2
  dpi <- 300
  set.seed(5)
  #load("data/regex_list.tall")



  ### SIDEBARMENU ----
  output$rest_of_sidebar <- renderMenu({
    if (values$menu==2){
      if (length(noGroupLabels(names(values$dfTag)))>0){
        values$menu <- 3
      }
    }
    sidebarMenu(.list=menuList(values$menu))
  })

  observeEvent(input$runImport, {
    updateTabItems(session, "sidebarmenu", "import_tx")
  })

  observeEvent(input$tokPosRun, {
    updateTabItems(session, "sidebarmenu", "tokPos")
  })

  observeEvent(input$custTermListRun, {
    updateTabItems(session, "sidebarmenu", "custTermList")
  })

  observeEvent(input$posTagSelectRun, {
    updateTabItems(session, "sidebarmenu", "posTagSelect")
  })

  observeEvent(input$multiwordCreatRun, {
    updateTabItems(session, "sidebarmenu", "multiwordCreat")
  })

  output$runButton <- renderUI({

    if (!isTRUE(values$resetNeed)){
      list(
        selectInput("load", "Please, choose what to do",
                    choices = c(
                      " "= "null",
                      "Load text files"="import",
                      "Load Tall structured files"="load_tall",
                      "Use a sample collection"="demo",
                      "Wikipedia pages"="wiki"
                    ),
                    selected = "null"
        ),
        conditionalPanel(
          condition="input.load == 'wiki'",
          textInput(inputId="wikiWord",
                    label="Search Wikipedia",
                    value=NULL),
          sliderTextInput(
            inputId = "wikiN",
            label = "Pages",
            choices = seq(1,20),
            selected = 1,
            animate = TRUE
          ),
          helpText(em("By specifying a search phrase in 'Search Wikipedia',
                                         the content of up to 20 Wikipedia pages can be downloaded."),
                   br(),
                   br(),
                   em("The content of each wiki page will be stored in the 'text' column.
                                         In addition, the page title, abstract and url will also be stored."),
                   br(),
                   br(),
                   em("The page title will be used as the 'doc_id'."))
        ),
        conditionalPanel(
          condition="input.load == 'import'",
          fluidRow(column(6,
                          selectizeInput(
                            'ext', label="File format",choices = c(
                              "txt"="txt",
                              "csv"="csv",
                              "excel"="xlsx",
                              "pdf"="pdf"),
                            tags$style("height: 50px")
                          )
          ),
          conditionalPanel(
            condition="input.load == 'import' & input.ext=='csv'",
            column(6,
                   selectizeInput(
                     'line_sep', label="CSV Separator",choices = c(
                       " , "=",",
                       " ; "=";"),
                     tags$style("height: 50px")
                   )
            )
          )
          ),
          uiOutput("file_raw"),
          uiOutput(outputId = "infoImport"),
          conditionalPanel(
            condition= "input.ext == 'xlsx' ||  input.ext =='csv'",
            uiOutput(outputId = "infoTextLabel")
          )
        ),
        conditionalPanel(
          condition="input.load=='demo'",
          selectInput("demo_file",
                      label="Select sample texts",
                      choices=c(
                        "BBC news" = "bbc",
                        "Bibliometrix" = "bibliometrix"
                      ),
                      selected = "bibliometrix"
          ),
          conditionalPanel(
            condition = "input.demo_file=='bibliometrix'",
            helpText(em("The dataset is composed of a collection of 444 scientific articles written in English
                                           in which the authors used the Bibliometrix R package to perform systematic literature reviews."),
                     br(),
                     br(),
                     em("The textual data consists of the article abstracts, while the additional information includes
                                           metadata such as the list of co-authors, first author, year of publication, and journal name."),
                     br(),
                     br(),
                     em("The abstracts have already been tokenized and POS tagged."))
          ),
          conditionalPanel(
            condition = "input.demo_file=='bbc'",
            helpText(em("A collection of 386 short news stories published in the entertainment section of the BBC News website."),
                     br(),
                     br(),
                     em("The texts are in English."))
          )
        ),
        conditionalPanel(
          condition = "input.load == 'load_tall'",
          helpText(em("Load a collection previously exported from Tall")),
          fileInput(
            "file1",
            "Choose a file",
            multiple = FALSE,
            accept = c(
              ".tall"
            )
          )
        ),
        conditionalPanel(condition = "input.load != 'null'",
                         div(
                           align = "center",
                           width=12,
                           actionButton(inputId="runImport",
                                        label = div(icon(name="play",lib = "glyphicon"),strong("START")),
                                        icon = NULL,
                                        style = "border-radius: 20px; border-width: 1px;
                                                                    font-size: 17px; color: #ffff;")
                         )
        )
      )
    } else {
      list(
      helpText(em("To load a new text collection,",
                  br(),"it is necessary to reset the app."),
               ),
      br(),
      br(),
      div(
        align = "center",
        width=12,
        actionButton(inputId="runReset2",
                     label = div(icon(name ="refresh", lib="glyphicon"),strong("RESET")),
                     icon = NULL,
                     style = "border-radius: 20px; border-width: 1px;
                                                                    font-size: 17px; color: #ffff;")
      )
      )
    }


  })




observeEvent(input$runReset2, {
  ask_confirmation(
    inputId = "reset_confirmation2",
    title = "Restart TALL",
    text = HTML('Restarting TAll will result in the loss of all analyses currently in progress<br><br>
                  <b>Do you want to confirm?</b>'),
    html=TRUE,
    type = "warning",
    btn_labels = c("CANCEL", "CONFIRM")
  )
})

observeEvent(input$reset_confirmation2, {
  if (isTRUE(input$reset_confirmation2)){
    reset_rv(input$runReset2)
    session$reload()
    param_stay_page <<- TRUE
  }
})
  ### IMPORT ----

  output$file_raw <- renderUI({
    switch(input$ext,
           txt = {
             ext <- c("text/plain", ".txt",".zip")
           },
           csv = {
             ext <- c("text/csv", ".csv",".zip")
           },
           xlsx = {
             ext <- c("excel", ".xlsx", ".xls",".zip")
           },
           pdf = {
             ext <- c(".pdf",".zip")
           })

    fileInput(
      "file_raw",
      "Select file(s) containing text",
      multiple = TRUE,
      accept = ext,
      placeholder = "No file(s) selected"
    )
  })

  output$infoTextLabel <- renderUI({

      shinyWidgets::alert(
        icon("warning"),
        tags$b("Warning!"),
        br(),
        HTML("The column including text(s) in your CSV or EXCEL file must be named <b>text</b>"),
        status = "warning"
      )
    #}
  })

  ### dataImported ----
  DATAloading<- eventReactive(input$runImport,{
    switch(input$load,
           import={
             if (!is.null(req(input$file_raw))){
               file <- input$file_raw
               txt <- read_files(file,ext=input$ext, subfolder=FALSE, line_sep=input$line_sep)
               values$menu <- 0
               values$custom_lists <- NULL
               values$txt <- txt %>%
                 mutate(text = removeHTMLTags(text),
                   text_original = text) %>%
                 arrange(doc_id)
               values$resetNeed <- TRUE
               #showModal(corpusModal(session))
               #values$metadata <- setdiff(names(values$txt), c("text", "doc_id","original_doc_id"))
             }
           },
           load_tall={
             req(input$file1)
             file_tall <- input$file1$datapath
             #print(file_tall)
             load(file_tall)
             values$menu <- menu
             values$dfTag <- dfTag
             values$custom_lists <- custom_lists
             values$language <- language
             values$D <- D
             values$where <- where
             values$resetNeed <- TRUE
             #values$metadata <- metadata
             if (values$menu==1) updateTabItems(session, "sidebarmenu", "custTermList")
             if (values$menu>1) updateTabItems(session, "sidebarmenu", "posTagSelect")
             if (ncol(values$dfTag)>1){showModal(loadTallgModal(session))}
           },
           demo={
             switch(input$demo_file,
                    bibliometrix={
                      file_tall <- loadSampleCollection("bibliometrix")
                      load(file_tall)
                      values$menu <- menu
                      values$dfTag <- dfTag
                      values$custom_lists <- custom_lists
                      values$language <- language
                      values$D <- D
                      values$where <- where
                      values$resetNeed <- TRUE
                      if (values$menu==1) updateTabItems(session, "sidebarmenu", "custTermList")
                      if (values$menu>1) updateTabItems(session, "sidebarmenu", "posTagSelect")
                      if (ncol(values$dfTag)>1){showModal(loadTallgModal(session))}
                    },
                    bbc={
                      file_tall <- loadSampleCollection("bbc")
                      files <- list(name="bbc.zip", datapath=file_tall)
                      txt <- read_files(files,ext="txt", subfolder=FALSE)
                      values$menu <- 0
                      values$custom_lists <- NULL
                      values$resetNeed <- TRUE
                      values$txt <- txt %>%
                        mutate(text_original = text) %>%
                        arrange(doc_id)
                    })
           },
           wiki = {
               df <- wikiSearch(input$wikiWord, n=as.numeric(input$wikiN))
             if (is.null(df)){

               show_alert(
                 title = "No results found!",
                 text = "It seems there are no Wikipedia pages matching your search.",
                 type = "error"
               )
               values$resetNeed <- FALSE

             } else{
               values$menu <- 0
               values$custom_lists <- NULL
               values$txt <- wikiExtract(df) %>%
                 mutate(text_original = text) %>%
                 rename(doc_id = title,
                        doc_selected = selected)

               values$resetNeed <- TRUE

             }
           }
    )
  })

  output$dataImported <- DT::renderDT({
    DATAloading()
    if (values$menu==0){
      DTformat(values$txt %>%
                 filter(doc_selected) %>%
                 mutate(text = paste0(substr(text,1,500),"...")) %>%
                 select(doc_id, text, everything()) %>%
                 select(-doc_selected,-text_original),
               left=3, nrow=5, filter="none", button=TRUE, delete=TRUE)
    }
  })
  ### shortpath for folder path ----
  output$folder <-  renderUI({
    path <- shortpath(values$path)
    if (is.null(path)) path <- " --- "
    HTML(paste("<pre class='tab'>",path, sep = ''))
  })

  loadTallgModal <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Tall Data Overview"))),
      br(),
      uiOutput("loadSynthesis"),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        actionButton(label="Custom Lists", inputId = "modalCustomLists", style="color: #ffff;",
                     icon = icon("th-list", lib = "glyphicon")),
        actionButton(label="Close", inputId = "closeModalCustomLists", style="color: #ffff;",
                     icon = icon("remove", lib = "glyphicon"))
      ),
    )
  }

  observeEvent(input$closeModalCustomLists,{
    removeModal(session = getDefaultReactiveDomain())
  })


  output$loadSynthesis <- renderUI({
    ndocs <- length(unique(values$dfTag$doc_id))
    txt1 <- (paste0("Tall file contains: ",strong(ndocs),strong(" documents")))
    txt2 <- (paste0("Last modified date: ", strong(values$D)))
    txt2b <- (paste0("Language: ", strong(values$language)))
    if(!is.null(dim(values$custom_lists))){
      ncust <- nrow(values$custom_lists)
      txt3 <- (paste0("Tall file includes a custom list of: ",strong(ncust), strong(" words")))
    } else{
      txt3 <- (paste0("Tall file does not include a custom word list"))
    }
    txt4 <- (paste0("The last pre-processing step performed is: ",strong(values$where)))
    text <- paste0(txt1,"<br><br>",txt2,"<br><br>",txt2b,"<br><br>",txt3,"<br><br>",txt4)
    tagList(
      div(
        h4(HTML(text)),
        style="text-align:left")
    )
  })

  observeEvent(input$modalCustomLists,{
    if (!is.null(values$custom_lists)){
      text =tagList(
        DTformat(values$custom_lists, left=c(1,2), filename="Custom_lists_table")
      )
    }else{
      text =tagList(
        div(
          h4(HTML("No custom lists to show.")),
          style="text-align:left")
      )
    }

    show_alert(
      title = "Custom Word Lists",
      text =text,
      type = NULL,
      size = "m",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      btn_labels = "OK",
      btn_colors = "#6CC283",
      timer = NULL,
      imageUrl = "",
      animation = TRUE
    )
  })

  ### Convert Raw Data in Excel functions ----
  # output$collection.save <- downloadHandler(
  #   filename = function() {
  #     paste("Tall-Export-File-", Sys.Date(), ".xlsx", sep="")
  #   },
  #   content <- function(file) {
  #     suppressWarnings(openxlsx::write.xlsx(values$txt, file=file))
  #   }, contentType = "xlsx"
  # )
  output$collection.save <- downloadHandler(
    filename = function() {
      paste("Tall-Export-File-", Sys.Date(), ".csv", sep="")
    },
    content <- function(file) {
      write_csv(
        x=values$txt,
        file=file,
        na = "NA",
        append = FALSE,
        col_names = TRUE
        # quote = c("needed"),
        # escape = c("backslash"),
        # eol = "\n"
      )
    }, contentType = "csv"
  )

  # Back to the original import text ----
  observeEvent(input$importTextBack, {
    values$txt <- values$txt %>%
      mutate(doc_selected = TRUE)
  })

  output$infoGroups <- renderUI({
    if (length(input$defineGroupsList) >1) {
      shinyWidgets::alert(
        icon("info"),
        " You need to select only one field",
        status = "danger"
      )
    }
  })


  ## EDIT ----

  ### SPLIT ----

  splitDocFunc <- eventReactive(input$splitTextRun,{
    if (nchar(input$txSplitWord)<3){
      popUpGeneric(title="Error",
                   type="error", color=c("#913333"),
                   subtitle="Sequence must be at least 3 characters long",
                   btn_labels="OK")
    } else {
      values$txt <- splitDoc(values$txt, word=input$txSplitWord)
      popUpGeneric(title=paste0("Split by: '",input$txSplitWord,"'"),
                   type="success", color=c("#1d8fe1"),
                   subtitle=paste0("Now you have ",nrow(values$txt)," documents"),
                   btn_labels="OK")
    }

  })


  ## back to the original txt
  observeEvent(input$splitTextBack, {
    values$txt <- unsplitDoc(values$txt)
    popUpGeneric(title="Restored",
                 type="waiting", color=c("#FFA800"),
                 subtitle=paste0("Now you have ",nrow(values$txt)," documents"),
                 btn_labels="OK")
  })

  output$splitTextData <- DT::renderDT({
    splitDocFunc()
    DTformat(values$txt %>%
               mutate(text = paste0(substr(text,1,500),"...")) %>%
               select(-c("text_original", ends_with("id_old"))) %>%
               filter(doc_selected) ,
             left=2, nrow=5, filter="none", button=TRUE, delete=TRUE)
  })

  output$splitTextSave <- downloadHandler(
    filename = function() {
      paste("Tall-Export-File-", Sys.Date(), ".csv", sep="")
    },
    content <- function(file) {
      write_csv(
        x=values$txt %>%
          filter(doc_selected) %>%
          select(-c("text_original", "doc_selected", ends_with("id_old"))),
        file=file,
        na = "NA",
        append = FALSE,
        col_names = TRUE
        # quote = c("needed"),
        # escape = c("backslash"),
        # eol = "\n"
      )
    }, contentType = "csv"
  )

  # Back to the original import text ----
  observeEvent(input$extInfoTextBack, {
    values$txt <- values$txt %>%
      mutate(doc_selected = TRUE)
  })


  ### RANDOM SELECTION ----

  output$randomDescription <- renderUI({

    HTML(paste("Number of imported texts: <b>", nrow(values$txt), "</b>"))

  })

  randomTextFunc <- eventReactive(input$randomTextRun,{

    values$txt <- samplingText(values$txt,
                               n=as.numeric(round((input$sampleSize/100)*nrow(values$txt)),0))
  })

  output$randomTextData <- DT::renderDT({
    randomTextFunc()
    DTformat(values$txt %>%
               filter(doc_selected) %>%
               select(-c("doc_selected","text_original")) %>%
               mutate(text = paste0(substr(text,1,500),"...")),
             left=2, nrow=5, filter="none", button=TRUE)
  })


  ## back to the original txt
  observeEvent(input$randomTextBack, {
    values$txt <- values$txt %>%
      mutate(doc_selected = TRUE)
  })


  ### EXTERNAL INFORMATION ----

  EXTINFOloading<- eventReactive(input$extInfoRun,{
    req(input$extInfoFile)
    file_extinfo <- input$extInfoFile$datapath
    #print(file_tall)
    values$txt <- loadExtInfo(file_extinfo, values$txt)
  })

  output$extInfoData <- DT::renderDT({
    EXTINFOloading()
    DTformat(values$txt %>%
               filter(doc_selected) %>%
               select(-c("text_original","doc_selected")) %>%
               mutate(text = paste0(substr(text,1,250),"...")),
             left=4, nrow=3, filter="top", button=TRUE, delete=TRUE)
  })


  output$doc_idExport <- downloadHandler(
    filename = function() {
      paste("Tall-Export-File-", Sys.Date(), ".xlsx", sep="")
    },
    content <- function(file) {
      suppressWarnings(openxlsx::write.xlsx(values$txt %>%
                                              filter(doc_selected) %>%
                                              select(doc_id),
                                            file=file))
    }, contentType = "xlsx"
  )

  # # Aggiunta del downloadHandler per il file .xlsx
  # output$extInfoSave <- downloadHandler(
  #   filename = function() {
  #     paste("Tall_Export_File_", Sys.Date(), ".xlsx", sep = "")
  #   },
  #   content <- function(file) {
  #     suppressWarnings(openxlsx::write.xlsx(values$txt %>%
  #                                             filter(doc_selected) %>%
  #                                           select(-c("text_original","doc_selected")),
  #                                           file=file))
  #   }, contentType = "xlsx"
  # )

  output$extInfoSave <- downloadHandler(
    filename = function() {
      paste("Tall-Export-File-", Sys.Date(), ".csv", sep="")
    },
    content <- function(file) {
      write_csv(
        x=values$txt %>%
          filter(doc_selected) %>%
          select(-c("text_original","doc_selected")),
        file=file,
        na = "NA",
        append = FALSE,
        col_names = TRUE
        # quote = c("needed"),
        # escape = c("backslash"),
        # eol = "\n"
      )
    }, contentType = "csv"
  )

  # Back to the original import text ----
  observeEvent(input$extInfoTextBack, {
    values$txt <- values$txt %>%
      mutate(doc_selected = TRUE)
  })

  ### PRE-PROCESSING ----

  ## Tokenization & PoS Tagging ----

  # output$optionsTokenization <- renderUI({
  #   selectInput(
  #     inputId = 'language_model', label="Select language", choices = values$label_lang,
  #     multiple=FALSE,
  #     width = "100%"
  #   )
  # })

  observeEvent(input$language_model, {
    selected_treebanks <- values$languages$treebank[values$languages$language_name == input$language_model]
    updateSelectInput(session, "treebank", choices = selected_treebanks)
  })
  

  posTagging <- eventReactive({
    input$tokPosRun
  },{
    values$language <- input$language_model
    values$language_file <- values$languages %>% filter(language_name==input$language_model, treebank==input$treebank) %>% select(file) %>% as.character()
    ## download and load model language
    udmodel_lang <- loadLanguageModel(file = values$language_file)

    ## set cores for parallel computing
    ncores <- max(1,parallel::detectCores()-1)

    ## set cores for windows machines
    if (Sys.info()[["sysname"]]=="Windows") {
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
    }

    #Lemmatization and POS Tagging
    values$dfTag <- udpipe(object=udmodel_lang, x = values$txt %>%
                             filter(doc_selected),
                           parallel.cores=ncores)

    # Merge metadata from the original txt object
    values$dfTag <- values$dfTag %>%
      left_join(values$txt %>% select(-text, -text_original), by = "doc_id") %>%
      filter(!is.na(upos)) %>%
      posSel(., c("ADJ","NOUN","PROPN", "VERB"))
    values$dfTag <- highlight(values$dfTag)
    values$dfTag$docSelected <- TRUE
    values$menu <- 1

  }
  )

  output$tokPosTagData<- DT::renderDT({
    posTagging()

    if(!is.null(values$dfTag)){
      DTformat(values$dfTag %>% filter(docSelected) %>% select(doc_id, paragraph_id, sentence_id,sentence,token,lemma, upos) %>%
                 rename(D_id=doc_id,
                        P_id=paragraph_id,
                        S_id=sentence_id,
                        Sentence=sentence,
                        Token=token,
                        Lemma=lemma,
                        "Part of Speech"=upos)
      )
    }
  })

  output$tokPosSave <- downloadHandler(
    filename = function() {
      paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
    },
    content <- function(file) {
      saveTall(values$dfTag, values$custom_lists, values$language, values$menu, "Custom Term Lists", file)
      # D <- date()
      # save(dfTag,menu,D,where, file=file)
    }, contentType = "tall"
  )

  ## Tagging Special Entities ----
  posSpecialTagging <- eventReactive({
    input$posSpecialRun
  },{
    res <- TaggingCorpusElements(values$dfTag)

    values$dfTag <- res$x %>% filter(!token %in% c("#","@")) # remove empty hast and tags
    values$posSpecialData <- res$resList %>% filter(!item %in% c("#","@"))

    rm(res)
    values$posSpecialTaggingDT <- DTformat(values$posSpecialData %>%
                                             summarySpecialEntities(type="all"),
                                          nrow=nrow(df), filter="none", button=F, delete=F, dom=FALSE,pagelength=FALSE,
                                          size="110%",
                                          filename="TaggingSpecialEntities", title="", specialtags=TRUE)

  }, ignoreNULL = TRUE)

  output$posSpecialTags <- DT::renderDT({

    posSpecialTagging()
    values$posSpecialTaggingDT
  })

  output$posSpecialData <- DT::renderDT({

    posSpecialTagging()

    if(!is.null(values$dfTag)){
      DTformat(values$dfTag %>% filter(docSelected) %>% select(doc_id, paragraph_id, sentence_id,sentence,token,lemma, upos) %>%
                 rename(D_id=doc_id,
                        P_id=paragraph_id,
                        S_id=sentence_id,
                        Sentence=sentence,
                        Token=token,
                        Lemma=lemma,
                        "Part of Speech"=upos)
      )
    }
  })

  ## back to the original txt
  observeEvent(input$posSpecialBack, {
    values$dfTag <- resetSpecialEntities(values$dfTag)
    proxy2 <- dataTableProxy('posSpecialTags')
    # tibble(UPOS = toupper(c("email", "url", "hash", "emoji", "ip_address", "mention")),
    #        "Frequency"=rep(0,6)) %>%
    #   mutate(Table = glue::glue('<button id2="custom_btn" onclick="Shiny.onInputChange(\'button_id2\', \'{UPOS}\')">View</button>')) %>%
    #   select(Table, everything())
    replaceData(proxy2, data.frame(Table=rep(NA,6), UPOS = toupper(c("email", "url", "hash", "emoji", "ip_address", "mention")), Frequency = rep(0,6)), resetPaging = FALSE)

    popUpGeneric(title="Special Entities Tags Removed",
                 type="waiting", color=c("#FFA800"),
                 subtitle=paste0("Now all Special Entities Tags have been remove from your documents"),
                 btn_labels="OK")


  })

  output$posSpecialSave <- downloadHandler(
    filename = function() {
      paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
    },
    content <- function(file) {
      saveTall(values$dfTag, values$custom_lists, values$language, values$menu, "POS Tag Selection", file)
      # D <- date()
      # save(dfTag,menu,D,where, file=file)
    }, contentType = "tall"
  )

  ## Click on Tagging Special Entities ----
  observeEvent(ignoreNULL = TRUE,
               eventExpr={input$button_id2},
               handlerExpr = {
                 if (input$button_id2!="null"){
                   showModal(modalSpecialEntities(session))
                 }
               })

  modalSpecialEntities <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(paste0("Frequency Distribution of ", firstUp(input$button_id2)," Entities"))),
      DTOutput(ns("specialEntityFreq")),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(label="Close", inputId = "closeModalSpecialEntities", style="color: #ffff;",
                     icon = icon("remove", lib = "glyphicon"))
      ),
    )
  }

  observeEvent(input$closeModalSpecialEntities,{
    removeModal(session = getDefaultReactiveDomain())
    #session$sendCustomMessage("click", 'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  output$specialEntityFreq <- renderDT(server=FALSE,{
    if (!is.null(input$button_id2)) id <- input$button_id2
    summarySpecialEntity <- values$posSpecialData %>% summarySpecialEntities(type=id)

    if (id=="URL"){
      summarySpecialEntity$item <- paste0(
        '<a href=\"',
        summarySpecialEntity$item ,
        '\" target=\"_blank\">',
        summarySpecialEntity$item ,
        '</a>'
      )
    }
    # find sentences containing the tokens/lemmas
    DTformat(summarySpecialEntity, size='100%', button = FALSE)
  }, escape=FALSE)



  ## Custom Term List Merging ----

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$custom_lists
    },
    handlerExpr = {
      file <- input$custom_lists
      req(file$datapath[1])
      custom_lists <- lapply(file$datapath,function(x){
        x <- read_excel(x) %>% select(c(1,2))
        names(x) <- c("lemma", "upos")
        return(x)
      })
      custom_lists <- do.call(rbind,custom_lists)
      values$custom_lists <- custom_lists
    }
  )

  customListMerging <- eventReactive({
    input$custTermListRun
  },{
    if (!is.null(values$custom_lists)){
      values$dfTag <- mergeCustomLists(values$dfTag,values$custom_lists)
      values$dfTag <- highlight(values$dfTag)
    }
  })

  output$customPosTagData<- DT::renderDT({
    customListMerging()

    if(!is.null(values$dfTag)){
      DTformat(values$dfTag%>% select(doc_id, sentence_id,sentence,token,lemma, upos) %>%
                 rename(D_id=doc_id,
                        S_id=sentence_id,
                        Sentence=sentence,
                        Token=token,
                        Lemma=lemma,
                        "Part of Speech"=upos)
      )
    }
  })

  output$customListData<- DT::renderDT(server=FALSE,{
    customListMerging()

    if (is.null(values$custom_lists)){
      DTformat(data.frame(Lemma=NULL,POSTag=NULL), n=1)
    } else {
      DTformat(values$custom_lists %>% rename(
        Lemma = lemma,
        "Part of Speech"=upos))
    }
  })

  output$custTermSave <- downloadHandler(
    filename = function() {
      paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
    },
    content <- function(file) {
      saveTall(values$dfTag, values$custom_lists, values$language, values$menu, "Custom Term Lists", file)
      # D <- date()
      # save(dfTag,menu,D,where, file=file)
    }, contentType = "tall"
  )

  ## Multi-Word Creation ----

  output$multiwordPosSel <- renderUI({
    checkboxGroupInput("multiwordPosSel", label=NULL,
                       choices = posTagAll(values$dfTag %>% dplyr::filter(!upos %in% c("MULTIWORD", "NGRAM_MERGED", "PUNCT", "SYM", "X", "NUM")))$description,
                       selected = posTagAll(values$dfTag %>% dplyr::filter(upos %in%  values$posMwSel))$description

    )
  })

  multiword <- eventReactive({
    input$multiwordCreatRun
  },{

    ### REKA Algorithm

    values$dfTag <- rakeReset(values$dfTag) ## reset previous multiword creation steps

    values$posMwSel <- gsub(":","",gsub(":.*","",input$multiwordPosSel))

    values$stats <- rake(values$dfTag, group = "doc_id", ngram_max=input$ngram_max, relevant = values$posMwSel, rake.min=input$rake.min, freq.min=input$freq_minMW, term=input$term)

  })

  observeEvent(ignoreNULL = FALSE,
                eventExpr = {input$multiwordList_rows_selected},
               handlerExpr = {
                 if (length(input$multiwordList_rows_selected)>0){
                   output$multiwordCreatApply <- renderUI({
                     run_bttn <- list(
                       label = strong("Apply List"),
                       style ="border-radius: 15px; border-width: 1px; font-size: 15px; text-align: center; color: #ffff; ",
                       icon = NULL
                     )
                     div(
                       align = "center",style="margin-top:-5px",
                       width=12,
                       helpText("Please note",br(),"pressing 'Apply List' will delete previous multiword entries"),
                       do.call("actionButton", c(run_bttn, list(
                         inputId = "multiwordCreatApply")
                       ))
                     )
                   })
                 } else {
                   output$multiwordCreatApply <- renderUI({})
                 }

               })
  output$multiwordList <- renderDT(server=FALSE,{
    multiword()
    DTformat(values$stats %>%  rename("Multi-Words" = keyword,
                                           "Lenght" = ngram,
                                           "Freq"=freq,
                                           "Rake value" = rake) %>%
               arrange(desc(Freq), .by_group = FALSE),
             numeric=4,
             selection=TRUE, nrow=20)
  })

  output$multiwordData <- renderDT(server=TRUE,{
    DTformat(values$dfTag %>% dplyr::filter(docSelected) %>%
               select(doc_id, sentence,token,lemma, upos) %>%
               rename(D_id=doc_id,
                      Sentence=sentence,
                      Token=token,
                      Lemma=lemma,
                      "Part of Speech"=upos),
             size="60%"
    )
  })
  observeEvent(input$multiwordCreatApply,{
    row_sel <- input$multiwordList_rows_selected

    # if (is.null(row_sel)) {
    #   row_sel = 1:nrow(values$stats)
    # }

    stats <- values$stats[row_sel,]

    obj <- applyRake(values$dfTag, stats=stats, relevant = values$posMwSel, term=input$term)

    values$dfTag <- obj$dfTag

    values$dfTag <- highlight(values$dfTag)
    values$multiwords <- obj$multiwords

    # text <- tagList(
    #
    # )

    show_alert(
      title = "Annotated Data with Multi-Words",
      text = DTOutput("multiwordData"),
      type = NULL,
      width = "80%",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      btn_labels = "OK",
      btn_colors = "#6CC283",
      timer = NULL,
      imageUrl = "",
      animation = TRUE
    )
  })

  output$multiwordCreatSave <- downloadHandler(
    filename = function() {
      paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
    },
    content <- function(file) {
      saveTall(values$dfTag, values$custom_lists, values$language, values$menu, "Multi-Word Creation", file)
      # D <- date()
      # save(dfTag,menu,D,where, file=file)
    }, contentType = "tall"
  )

  ## back to the original txt
  observeEvent(input$multiwordCreatBack, {
    values$dfTag <- rakeReset(values$dfTag)
    values$multiwords <- data.frame(keyword="",ngram=NA, freq=NA, rake=NA)
    popUpGeneric(title="Multiword Removed",
                 type="waiting", color=c("#FFA800"),
                 subtitle=paste0("Now all multiwords have been remove from your documents"),
                 btn_labels="OK")
  })


  ## Multi-Word by a list ----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$multiword_lists
    },
    handlerExpr = {
      file <- input$multiword_lists
      req(file$datapath[1])
      keyword_lists <- lapply(file$datapath,function(x){
        x <- read_excel(x) %>% select(1)
        names(x) <- "keyword"
        return(x)
      })
      keywordList <- do.call(rbind,keyword_lists)
      values$keywordList <- keywordList
    }
  )

  multiword2 <- eventReactive({
    input$multiwordListRun
  },{

    # to replace with input values
    term <-  input$termMWL

    relevant <- unique(values$dfTag$upos)

    stats <- rake(values$dfTag, relevant = relevant, term=term, type="bylist", keywordList = values$keywordList)

    obj <- applyRake(values$dfTag, stats=stats, relevant = relevant, term=term)

    values$dfTag <- obj$dfTag

    values$dfTag <- highlight(values$dfTag)
    values$multiwords <- obj$multiwords
  })

  output$multiwordData2 <- renderDT({
    multiword2()
    DTformat(values$dfTag %>% dplyr::filter(POSSelected) %>%
               group_by(doc_id,sentence_id) %>%
               select(doc_id, sentence_id,sentence,token,lemma, upos) %>%
               rename(D_id=doc_id,
                      S_id=sentence_id,
                      Sentence=sentence,
                      Token=token,
                      Lemma=lemma,
                      "Part of Speech"=upos)
    )
  })

  output$multiwordList2 <- renderDT(server=FALSE,{
    multiword2()
    DTformat(values$multiwords %>%  rename("Multi-Words" = keyword,
                                           "Lenght" = ngram,
                                           "Freq"=freq) %>%
               arrange(desc(Freq), .by_group = FALSE))
  })

  output$multiwordListSave <- downloadHandler(
    filename = function() {
      paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
    },
    content <- function(file) {
      saveTall(values$dfTag, values$custom_lists, values$language, values$menu, "Multi-Word by a List", file)
      # D <- date()
      # save(dfTag,menu,D,where, file=file)
    }, contentType = "tall"
  )

  ## back to the original txt
  observeEvent(input$multiwordListBack, {
    values$dfTag <- rakeReset(values$dfTag)
    values$multiwords <- data.frame(keyword="",ngram=NA, freq=NA)

    popUpGeneric(title="Multiword Removed",
                 type="waiting", color=c("#FFA800"),
                 subtitle=paste0("Now all multiwords have been remove from your documents"),
                 btn_labels="OK")
  })

  ## PoS Tag Selection ----

  observe({
    output$posTagLists <- renderUI({
      checkboxGroupInput("posTagLists", label=NULL,
                         choices = posTagAll(values$dfTag)$description,
                         selected = (posTagAll(values$dfTag %>% dplyr::filter(POSSelected)))$description
      )
    })
  })

  PosFilterData <- eventReactive({
    input$posTagSelectRun
  },{
    selected <- (posTagAll(values$dfTag) %>% dplyr::filter(description %in% (input$posTagLists)))$pos
    values$dfTag <- removeHapaxFreq(values$dfTag,input$posTagHapax, input$posTagSingleChar)
    values$dfTag <- posSel(values$dfTag, pos=selected)
    values$menu <- 2


    # Update the DT proxy
    proxy <- dataTableProxy('posTagSelectData')
    replaceData(proxy, values$dfTag, resetPaging = FALSE)

  })

  output$posTagSelectData <- DT::renderDT({
    PosFilterData()

    if(!"lemma_original_nomultiwords" %in% names(values$dfTag)) values$dfTag <- values$dfTag %>% mutate(lemma_original_nomultiwords=lemma)

    DTformat(
      LemmaSelection(values$dfTag) %>%
        group_by(doc_id,sentence_id) %>%
        mutate(SentenceByPos = paste(lemma_original_nomultiwords, collapse=" ")) %>%
        select(doc_id, sentence_id,sentence,SentenceByPos,token,lemma, upos) %>%
        rename(D_id=doc_id,
               S_id=sentence_id,
               Sentence=sentence,
               Token=token,
               Lemma=lemma,
               "Part of Speech"=upos) %>% ungroup()
    )
  })

  output$posTagSelectSave <- downloadHandler(
    filename = function() {
      paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
    },
    content <- function(file) {
      saveTall(values$dfTag, values$custom_lists, values$language, values$menu, "POS Tag Selection", file)
      # D <- date()
      # save(dfTag,menu,D,where, file=file)
    }, contentType = "tall"
  )

  ## FILTER ----
  output$filterList <- renderUI({
    label <- c("",sort(noGroupLabels(names(values$dfTag))))
    selectInput(
      inputId = "filterList",
      label = NULL,
      choices = label,
      selected = values$selectedFilter,
      width = "100%"
    )
  })

    observeEvent(ignoreNULL = TRUE,
                 eventExpr={input$filterList},
                 handlerExpr = {
                   if (nchar(input$filterList)>0){
                     filtervalues <- LemmaSelection(values$dfTag) %>%
                       select(all_of(input$filterList)) %>%
                       distinct()
                     values$filtervalues <- sort(filtervalues[[1]])
                     values$selectedFilter <- input$filterList

                     output$filterValue <- renderUI({
                       multiInput(
                         inputId="filterValue",
                         label=NULL,
                         choices = values$filtervalues,
                         selected = NULL,
                         width = "100%"
                       )
                     })
                   }
     })

  observeEvent(ignoreNULL = TRUE,
               eventExpr={input$filterAll},
               handlerExpr = {
                 updateMultiInput(
                   session = session,
                   inputId = "filterValue",
                   selected = values$filtervalues
                 )
               })

  observeEvent(ignoreNULL = TRUE,
               eventExpr={input$filterNone},
               handlerExpr = {
                 updateMultiInput(
                   session = session,
                   inputId = "filterValue",
                   selected = ""
                 )
               })


  filterDATA <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$filterRun},
    valueExpr = {
      if (!is.null(input$filterValue)){
        values$dfTag$docSelected <- ifelse(values$dfTag[[values$selectedFilter]] %in% input$filterValue,TRUE,FALSE)
      } else {
        values$dfTag$docSelected <- TRUE
      }
      values$dfTag
    })

  output$filterData <- renderDT({
    filterDATA()
    DTformat(LemmaSelection(values$dfTag) %>%
               dplyr::filter(docSelected), nrow=3, size='100%', title=paste0("Filtered Data by ", input$filterList))
  })


  ## Data filtered by dynamic text on dashboardHeader

  output$dataFilteredBy <- renderText({
    if (!is.null(input$filterValue)){
      req(input$filterRun)
      HTML(paste("Documents filtered by: <b>", input$filterList, "</b>"))
    } else {
      HTML("")
    }
  })


  ## GROUPS ----

  ### Define groups ----

  output$defineGroupsList <- renderUI({
    label <- noGroupLabels(names(values$dfTag))
    multiInput(
      inputId = "defineGroupsList",
      label = NULL,
      choices = label,
      selected = values$selectedGroups,
      width = "100%"
    )

  })

  output$infoGroups <- renderUI({
    if (length(input$defineGroupsList) >1) {
      shinyWidgets::alert(
        icon("info"),
        " You need to select only one field",
        status = "danger"
      )
    }
  })

  groupMetadata <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$defineGroupsRun},
    valueExpr ={
      values$selectedGroups <- input$defineGroupsList
      values$dfTag <- groupByMetadata(values$dfTag, metadata=input$defineGroupsList)
      if (length(input$defineGroupsList) == 1){
        showModal(groupModal(session))
      } else {
        showModal(ungroupModal(session))
      }
    })

  ## Data grouped by dynamic text on dashboardHeader
  output$dataGroupedBy <- renderText({
    if (length(input$defineGroupsList) == 1){
      req(input$defineGroupsRun)
      HTML(paste("Documents grouped by: <b>", input$defineGroupsList, "</b>"))
    } else {
      HTML("")
    }
  })


  output$defineGroupsData <- renderDT({
    groupMetadata()
    DTformat(values$dfTag %>% filter(docSelected), nrow=3, size='100%', title="Data Grouped By External Information")
  })

  groupModal <- function(session) {
    ns <- session$ns
    values$newGr <- values$dfTag %>%
      filter(docSelected) %>%
      count(doc_id, ungroupDoc_id) %>%
      group_by(doc_id) %>%
      count()
    names(values$newGr) = c(input$defineGroupsList, "N. of Docs")
    txt <- paste0("<hr><br><br>The original <b>", sum(values$newGr[,2]),
                  "</b> documents have been partitioned into <b>",
                  nrow(values$newGr),"</b> groups <br><br>")
    modalDialog(
      h3(strong(paste0("Documents grouped by ",input$defineGroupsList))),
      h4(HTML(txt)),
      br(),
      DTOutput(ns("groupData")),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")),
    )
  }

  ungroupModal <- function(session) {
    ns <- session$ns
    txt <- paste0("<hr><br><br>The original partitioning<br>into documents has been correctly restored.<br><br><hr>")
    modalDialog(
      h3(HTML(txt)),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")),
    )
  }

  output$groupData <- renderDT(server=FALSE,{
    DTformat(values$newGr,nrow=10, size='100%', title="Groups By External Information", left=1)
  })


  ## OVERVIEW ----

  ## BOX ----


  #### box1 ---------------
  output$nDoc <- renderValueBox({
    values$vb <- valueBoxesIndices(values$dfTag %>% filter(docSelected))

    values$VbData <- data.frame(Description=c("Documents", "Tokens", "Types", "Lemmas", "Sentences",
                                              "Docs Avg Length in Chars", "Doc Avg Length in Tokens",
                                              "Sent Avg Length in Tokens", "Sent Avg Length in Chars",
                                              "TTR", "Hapax (%)", "Guiraud Index"),
                                Values=unlist(values$vb))
    valueBox(value = p(ifelse(is.null(input$defineGroupsList), "Documents", paste0("Docs grouped by ",input$defineGroupsList)), style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$nDoc), style = 'font-size:36px;color:white;', align="center"),
             icon = icon("duplicate", lib="glyphicon" ), color = "olive",
             width = NULL)
  })

  onclick('clickbox1', showModal(modalDialog(
    title = ifelse(is.null(input$defineGroupsList), "Documents", paste0("Docs grouped by ",input$defineGroupsList)),
    h3("Number of Documents"),
    easyClose = TRUE
  )))


  #### box2 ---------------
  output$avgDocLengthChar <- renderValueBox({
    valueBox(value = p("Doc Avg Length in Chars", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$avgDocLengthChars), style = 'font-size:36px;color:white;', align="center"),
             icon = icon("duplicate", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox2', showModal(modalDialog(
    title = "Doc Avg Length in Chars",
    h3("Average Document's Length by characters"),
    easyClose = TRUE
  )))

  #### box3 ------------
  output$avgDocLengthTokens <- renderValueBox({
    valueBox(value = p("Doc Avg Length in Tokens", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$avgDocLengthTokens), style = 'font-size:36px;color:white;', align="center"),
             icon = icon("duplicate", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox3', showModal(modalDialog(
    title = "Doc Avg Length in tokens",
    h3("Average Document's Length by tokens"),
    easyClose = TRUE
  )))

  #### box4 ---------------
  output$nSentences <- renderValueBox({
    valueBox(value = p("Sentences", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$nSentences), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="align-left", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox4', showModal(modalDialog(
    title = "Sentences",
    h3("Number of Sentences"),
    easyClose = TRUE
  )))

  #### box5 --------------------
  output$avgSentLengthChar <- renderValueBox({
    valueBox(value = p("Sent Avg Length in Chars", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$avgSentLengthChars), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="align-left", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox5', showModal(modalDialog(
    title = "Sent Avg Length in Chars",
    h3("Average Sentence's Length by characters"),
    easyClose = TRUE
  )))

  #### box6 -------------
  output$avgSentLengthTokens <- renderValueBox({
    valueBox(value = p("Sent Avg Length in Tokens", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$avgSentLengthTokens), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="align-left", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox6', showModal(modalDialog(
    title = "Sent Avg Length in Tokens",
    h3("Average Sentence's Length by tokens"),
    easyClose = TRUE
  )))

  #### box7 ----------------
  output$nDictionary <- renderValueBox({
    valueBox(value = p("Types", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$nDictionary), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="font", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox7', showModal(modalDialog(
    title = "Types",
    h3("Number of Types"),
    hr(),
    p("The size of the vocabulary in terms of different/distinct words", style = 'font-size:16px'),
    easyClose = TRUE
  )))

  #### box8 ---------------
  output$nTokens <- renderValueBox({
    valueBox(value = p("Tokens", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$nTokens), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="font", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox8', showModal(modalDialog(
    title = "Tokens",
    h3("Number of Tokens"),
    hr(),
    p("The size of the collection in terms of occurrences",style = 'font-size:16px'),
    easyClose = TRUE
  )))

  #### box9 ---------------
  output$nLemmas <- renderValueBox({
    valueBox(value = p("Lemmas", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$nLemmas), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="font", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox9', showModal(modalDialog(
    title = "Lemmas",
    h3("Number of Lemmas"),
    hr(),
    p(HTML("<p><span style='text-align: start; color: rgb(32, 33, 34); background-color: rgb(255, 255, 255); font-size: 16px; font-family: Calibri;'>&nbsp;</span><span style='text-align: start; color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); font-size: 16px;'>A <em>lemma&nbsp;</em>is the <strong>canonical form</strong>, or <strong>d</strong><strong>ictionary form</strong> of a set of words forms.</span></p>")),
    easyClose = TRUE
  )))

  #### box10 ------------------
  output$TTR <- renderValueBox({
    valueBox(value = p("TTR", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$TTR), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="stats", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox10', showModal(modalDialog(
    title = "TTR",
    h3("Types/Tokens Ratio"),
    hr(),
    p(HTML("<p><span style='font-family: Calibri, sans-serif; font-size: 16px;'>The <strong>type-token ratio (TTR)</strong> is a measure of the <em>lexical diversity</em> of a text.&nbsp;</span></p>
<p style='text-align: center;'><span style='font-size: 16px;'><em><span style='font-family: Calibri, sans-serif;'>TTR = (number of types / number of tokens) * 100</span></em></span></p>
<p><span style='font-family: Calibri, sans-serif; font-size: 16px;'>A high TTR indicates that the text uses a wide variety of words, while a low TTR indicates that the text uses a relatively small number of words repeatedly.</span></p>
<p><br></p>
<p style='margin:0cm;font-size:16px;font-family:'Calibri',sans-serif;text-align:center;'><br></p>"), style = 'font-size:16px'),
    easyClose = TRUE
  )))

#McMurtry, J., & Mather, N. (2013). Assessing lexical diversity in written language: A review of methods. Written Communication, 30(1), 1-36.

  #### box11 ------
  output$hapax <- renderValueBox({
    valueBox(value = p("Hapax (%)", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$hapax), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="stats", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox11', showModal(modalDialog(
    title = "Hapax (%)",
    h3("Percentage of Hapax"),
    hr(),
    p(HTML("<p style='margin:0cm;font-size:16px;font-family:'Calibri',sans-serif;'><span style='font-family:Helvetica;color:black;'>The words that occur just one time are called <strong style='font-weight: 700; color: rgb(0, 0, 0); font-family: Helvetica; font-size: 16px; font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; white-space: normal; background-color: rgb(255, 255, 255); text-decoration-thickness: initial; text-decoration-style: initial; text-decoration-color: initial;'>Hapax</strong> - the number of words that appear only once in a text.</span></p>
<p style='margin:0cm;font-size:16px;font-family:'Calibri',sans-serif;'><span style='font-family:Helvetica;color:black;'><strong>Hapax (%)</strong> can be used to measure the <em>lexical richness</em> of a text.&nbsp;</span></p>
<p style='margin:0cm;font-size:16px;font-family:'Calibri',sans-serif;'><br></p>
<p style='margin: 0cm; font-size: 16px; font-family: Calibri, sans-serif; text-align: center;'><span style='font-family:Helvetica;color:black;'><span style='color: rgb(0, 0, 0); font-family: Helvetica; font-size: 16px; font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: center; text-indent: 0px; text-transform: none; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; white-space: normal; background-color: rgb(255, 255, 255); text-decoration-thickness: initial; text-decoration-style: initial; text-decoration-color: initial; display: inline !important; float: none;'><em>Hapax (%) = (number of hapax / number of types) * 100</em></span></span></p>
<p style='margin: 0cm; font-size: 16px; font-family: Calibri, sans-serif; text-align: center;'><br></p>
<p style='margin:0cm;font-size:16px;font-family:'Calibri',sans-serif;'><span style='font-family:Helvetica;color:black;'>A text with a high Hapax (%) is likely to be more lexically rich than a text with a low Hapax (%). It can also be used to compare different texts. For example, a text written in a technical field is likely to have a higher Hapax (%) than a text written in a general-interest field.&nbsp;</span><span style='font-family:Helvetica;color:black;'>&nbsp;</span></p>
<p style='margin:0cm;font-size:16px;font-family:'Calibri',sans-serif;text-align:center;'><br></p>"), style = 'font-size:16px'),
    easyClose = TRUE
  )))

  #### box12 -------
  output$guiraud <- renderValueBox({
    valueBox(value = p("Guiraud Index", style = 'font-size:16px;color:white;'),
             subtitle = p(strong(values$vb$guiraud), style = 'font-size:36px;color:white;', align="center"),
             icon = icon(name="stats", lib="glyphicon"), color = "olive",
             width = NULL)
  })

  onclick('clickbox12', showModal(modalDialog(
    title = "Guiraud Index",
    h3("Guiraud Index"),
    hr(),
    p(HTML("<span style='font-family: Calibri, sans-serif; font-size: 16px;'>The <strong>Guiraud Index (GI)</strong> is a <em>lexical diversity</em> measure calculated by dividing the number of unique terms (types) by the square root of the total number of terms (tokens) in the text. The formula is:</span>
             <div style='font-size:1.2em; color:'black'; text-align: center; margin:10px 0;'>
          <p style='text-align: center;'><span style='font-size: 16px;'><em><span style='font-family: Calibri, sans-serif;'> Guiraud Index = (Number of Types / &#8730;Number of Tokens) * 100 </span></em></span></p>
             </div>
             <span style='font-family: Calibri, sans-serif; font-size: 16px;'> GI is less sensitive to text length than a simple type-to-token ratio, making it particularly useful for comparing texts of varying lengths. A higher </strong>GI</strong> indicates a richer vocabulary, reflecting greater lexical diversity.</span>"), style = 'font-size:16px'),
    easyClose = TRUE
  )))


  ## Overview Table ----

  output$overviewData <- renderDT(server = FALSE,{
    DTformat(values$VbData,n=12, left=1, right=2, numeric=2, pagelength=FALSE, dom=FALSE, size='110%', filename="Overview")
  })

  ## Report

  observeEvent(input$overviewReport,{
    if(!is.null(values$VbData)){
      sheetname <- "Overview"
      list_df <- list(values$VbData)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      values$wb <- res$wb
      popUp(title="Overview Table", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })


  ## WORDCLOUD ----

  wcData<- eventReactive({
    input$wcApply
  },
  {

    N <- input$nWC #showing the first 100 lemmas
    pos <- LemmaSelection(values$dfTag) %>%
      dplyr::filter(docSelected) %>%
      select(upos)
    pos <- unique(pos$upos)

    values$wcDfPlot <- freqByPos(LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected),
                                 term=input$termWC,
                                 pos=pos) %>%
      slice_head(n=N) %>%
      rename(label = term,
             value = n)
    values$WC2VIS <- wordcloud2vis(values$wcDfPlot, labelsize=input$labelsizeWC, opacity=1)
    })

  output$wordcloudPlot <- renderVisNetwork({
    wcData()
    values$WC2VIS
  })
  # output$wordcloudPlot <- renderWordcloud2({
  #   wcData()
  #   values$wcPlot <- wordcloud2a(values$wcDfPlot,
  #                                           fontFamily = "Impact", fontWeight = "normal", minSize=0,
  #                                           minRotation = 0, maxRotation = 0, shuffle = TRUE,
  #                                           rotateRatio = 0.7, shape = "circle",ellipticity = 0.65,
  #                                           #widgetsize = "100%",
  #                                           figPath = NULL,
  #                                           #size = ifelse(length(values$wcDfPlot$text)>100,1,1.3),
  #                                           color = "random-dark", backgroundColor = "transparent")
  #   values$wcPlot
  # })


  # output$wcDfData <- renderDT(server=FALSE,{
  #   wcData()
  #   DTformat(values$wcDfPlot,n=15, left=1, right=2, numeric=2, pagelength=TRUE, dom=TRUE, size='110%', filename="WordCloudData")
  # })

  ## export WordClud button
  output$wcSave <- downloadHandler(
    filename = function() {
      paste("Wordcloud-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      plot2png(values$WC2VIS, filename=file, zoom = values$zoom)
    },
    contentType = "png"
  )

  # ## export PNG button
  # observeEvent(input$wcSave,{
  #   # remove old file
  #   switch(Sys.info()[['sysname']],
  #          Windows= {home <- Sys.getenv('R_USER')},
  #          Linux  = {home <- Sys.getenv('HOME')},
  #          Darwin = {home <- Sys.getenv('HOME')})
  #   filename <- paste(home,"/Downloads/WordCloud-", paste0(Sys.Date(),"_",format(Sys.time(),'%H:%M:%S')), ".png", sep="")
  #   file_old <- dir(paste0(home,"/Downloads"), pattern=paste("WordCloud-", Sys.Date(), ".png", sep=""))[1]
  #   if(!is.na(file_old)){
  #     file.remove(filename)
  #   }
  #   plot2png(values$wcPlot, filename=filename, zoom = values$zoom, type="plotly")
  # })


  ## VOCABULARY ----
  dictionary <- eventReactive({
    input$dictionaryApply
  },
  {
    #values$dictFreq <- freqByPos(values$dfTag, term=input$termDict, pos=c("PROPN", "NOUN", "ADJ", "VERB"))
    Term <- input$termDict
    values$dictFreq <- LemmaSelection(values$dfTag) %>%
      dplyr::filter(docSelected) %>%
      mutate(token = ifelse(upos == "MULTIWORD", lemma,token))

    if (Term=="lemma"){
      values$dictFreq <- values$dictFreq %>%
        group_by(upos, lemma) %>%
        summarize(n=n()) %>%
        arrange(desc(n)) %>%
        rename(Lemma = lemma,
               Frequency = n,
               "Part of Speech"=upos) %>%
      relocate("Part of Speech", .after = last_col())
    } else {
      values$dictFreq <- values$dictFreq %>%
        group_by(upos, token) %>%
        summarize(n=n()) %>%
        arrange(desc(n)) %>%
        rename(Token = token,
               Frequency = n,
               "Part of Speech"=upos) %>%
        relocate("Part of Speech", .after = last_col())
    }
  })


  output$dictionaryData <- renderDT(server=FALSE,{
    dictionary()
    DTformat(values$dictFreq,
             left=c(1,2), nrow=15, pagelength=TRUE, filename="Dictionary", dom=TRUE, size="110%")
  })

  ## TF-IDF ----

  tf_idf <- eventReactive({
    input$tfidfApply
  },
  {
    values$tfidfDATA <- LemmaSelection(values$dfTag) %>%
      dplyr::filter(docSelected) %>%
      tfidf(term=input$termTfidf)
  })


  output$tfidfData <- renderDT(server=FALSE,{
    tf_idf()
    if(input$termTfidf=="lemma"){
    DTformat(values$tfidfDATA  %>%
               rename(
                 Lemma = term,
                 "TF-IDF" = TFIDF),
             left=1, numeric=2,round=4, size="110%"
    )}
    else{DTformat(values$tfidfDATA  %>%
                     rename(
                       Token = term,
                       "TF-IDF" = TFIDF),
                   left=1, numeric=2,round=4, size="110%"
    )}
  })



  ### WORDS ----

  ## Click on Plotly graphs: WORDS IN CONTEXT ----
  observeEvent(event_data("plotly_click"), {
    d <- event_data("plotly_click")
    elementY <- d$y[1]
    if (!is.null(elementY)){
      if (input$sidebarmenu == "d_tm_estim" & elementY %in% unique(values$dfTag$doc_id)){
        showModal(plotModalDoc(session))
      } else if (input$sidebarmenu %in% c("w_freq", "ca", "d_tm_estim")){
        showModal(plotModalTerm(session))
      }
    }
  })

  plotModalTerm <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Words in Context"))),
      DTOutput(ns("wordInContext")),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(label="Close", inputId = "closePlotModalTerm", style="color: #ffff;",
                     icon = icon("remove", lib = "glyphicon"))
      ),
    )
  }

  observeEvent(input$closePlotModalTerm,{
    removeModal(session = getDefaultReactiveDomain())
    resetModalButtons(session = getDefaultReactiveDomain())
    #runjs("Shiny.setInputValue('plotly_click-A', null);")
  })

  output$wordInContext <- renderDT(server=FALSE,{
    values$d <- event_data("plotly_click")
    word <- values$d$y

    # for URL frequency #
    if (!is.null(word)) if (substr(word,1,8)=="<a href="){
      href_regex <- "href=\"(https?://[^\"]+)\""
      word <- sub(href_regex, "\\1", unlist(regmatches(word, gregexpr(href_regex, word, perl = TRUE))))
    }
    ###

    if (input$sidebarmenu=="w_other"){
      word_search <- word
      sentences <- values$dfTag %>%
        filter(docSelected) %>%
        filter(lemma %in% word_search) %>%
        ungroup() %>% select(doc_id, lemma, token, sentence_hl) ## add doci_id
    } else if (input$sidebarmenu=="ca"){
      if (!is.null(values$d)){
        X <- round(values$d$x,6)
        Y <- round(values$d$y,6)
        word <- values$CA$wordCoord %>%
          dplyr::filter(round(.[,1],6)==X,round(.[,2],6)==Y)
        word <- word$label
        word_search <- unique(c(word, values$dfTag$token[values$dfTag$lemma==word]))
        sentences <- values$dfTag %>%
          filter(docSelected) %>%
          filter(token %in% word_search) %>%
          ungroup() %>% select(doc_id, lemma, token, sentence_hl)
      }
    } else {
      word_search <- unique(c(word, values$dfTag$token[values$dfTag$lemma==word]))
      sentences <- values$dfTag %>%
        filter(docSelected) %>%
        filter(token %in% word_search) %>%
        ungroup() %>% select(doc_id, lemma, token, sentence_hl)
    }
    # find sentences containing the tokens/lemmas
    DTformat(sentences, size='100%', button = TRUE)
  }, escape=FALSE)


  ## Click on Plotly graphs: DOCS IN CONTEXT ----
  plotModalDoc <- function(session) {
    ns <- session$ns
    modalDialog(
      #h3(strong(("Docs in Context"))),
      DTOutput(ns("docInContext")),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(inputId="tmTopSentences",
                     label = strong("Relevant Sentences"),
                     icon = icon(name="text-background",lib = "glyphicon"),
                     style = "border-radius: 20px; border-width: 1px; font-size: 16px; text-align: center; color: #ffff; padding-left: 20px; padding-right: 20px"),
        actionButton(label="Close", inputId = "closePlotModalDoc", style="color: #ffff;",
                     icon = icon("remove", lib = "glyphicon"))),
    )
  }


observeEvent(input$closePlotModalDoc,{
  removeModal(session = getDefaultReactiveDomain())
  #runjs("Shiny.setInputValue('plotly_click-A', null);")
  resetModalButtons(session = getDefaultReactiveDomain())
})

  plotModalDocHigh <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Relevant sentences by TextRank"))),
      DTOutput(ns("docInContextHigh")),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(label="Close", inputId = "closeplotModalDocHigh", style="color: #ffff;",
                     icon = icon("remove", lib = "glyphicon"))
      ),
    )
  }

  observeEvent(input$closeplotModalDocHigh,{
    removeModal(session = getDefaultReactiveDomain())
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  observeEvent(input$tmTopSentences,{
    showModal(plotModalDocHigh(session))
  })

  output$docInContext <- renderDT(server=FALSE,{
    if (!is.null(event_data("plotly_click"))) {
      values$d <- event_data("plotly_click")
    }
    doc <- values$d$y
    paragraphs <- values$dfTag %>% filter(doc_id==doc) %>%
      distinct(paragraph_id,sentence_id, sentence) %>%
      group_by(paragraph_id) %>%
      summarize(paragraph=paste0(sentence,collapse=" ")) %>%
      ungroup() %>%
      rename("Paragraph ID" = paragraph_id,
             "Paragraph" = paragraph)
    DTformat(paragraphs, nrow=3, size='100%', title=paste0("Doc_id: ",doc))
    # HTML(paste0(paste0("<h3>Doc_id: ",doc,"</h3><hr>"),
    #   paste(sentences$sentence,collapse=" ")))
  })

  output$docInContextHigh <- renderDT(server=FALSE,{
    if (!is.null(event_data("plotly_click"))) {
      values$d <- event_data("plotly_click")
    }
    doc <- values$d$y
    DTformat(highlightSentences(values$dfTag, id=doc), nrow=3, size='100%', title=paste0("Doc_id: ",doc))
    #HTML(paste0(paste0("<h3>Doc_id: ",doc,"</h3><hr>"), highlightSentences(values$dfTag, id=doc, n=3)))
  })

  ## Frequency List ----

  ## Words Frequency by PoS ----

  output$posSelectionFreq <-  renderUI({


    selectInput(inputId = "posSelectionFreq",
                "PoS Tag:",
                choices = posTagAll(values$dfTag)$pos,
                selected = "NOUN"
    )
  })

  wFreq <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$wFreqApply
    },
    valueExpr = {
      if(!is.null(input$posSelectionFreq)){
        values$wFreq <- freqByPos(values$dfTag %>% filter(docSelected), term=input$wFreqTerm, pos=input$posSelectionFreq)
        values$wFreqPlotly <- freqPlotly(values$wFreq,x="n",y="term",n=input$wFreqN, xlabel="Frequency",ylabel=input$posSelectionFreq, scale="identity")

        values$wFreqData <- values$wFreq %>%
          rename(Word = term,
                 Frequency = n)
      }else{
        popUpGeneric(title="No PoS Tag/Term selected!", type="error", color=c("#1d8fe1","#913333","#FFA800"),
                     subtitle="Please, click on Option button and select by which PoS Tag and term /n
                     measure the frequency distribution",
                     btn_labels="OK")
      }

    }
  )

  output$wFreqPlot <- renderPlotly({
    wFreq()
    values$wFreqPlotly
  })

  output$wFreqTable <- renderDT(server=FALSE,{
    wFreq()
    DTformat(values$wFreqData, left=1, right=2, round=0, numeric=2, filename="WordsFreqList", dom=FALSE, size="110%")
  })

  output$wFreqExport <- downloadHandler(
    filename = function() {

      paste("WordsFrequency-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      values$wFreqGgplot <- freqGgplot(values$wFreq,x=2, y=1,n=input$wFreqN,
                                       title = paste0("Words Frequency by ", input$posSelectionFreq))
      ggsave(filename = file, plot = values$wFreqGgplot, dpi = dpi, height = values$h, width = values$h*2, bg="transparent")
    },
    contentType = "png"
  )

  ## Report

  observeEvent(input$wFreqReport,{
    if(!is.null(values$wFreq)){
      values$wFreqGgplot <- freqGgplot(values$wFreq,x=2, y=1,n=input$wFreqN,
                                       title = paste0("Words Frequency by ", input$posSelectionFreq)
      )
      list_df <- list(values$wFreqData)
      list_plot <- list(values$wFreqGgplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "WordsFreq", wb=values$wb)
      values$wb <- wb
      popUp(title=paste0("Words Frequency by-",input$posSelectionFreq) , type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })


  ## PART OF SPEECH ----

  posFreq <- eventReactive(
    eventExpr = {
      input$posApply
    },
    valueExpr = {
      values$freqPOS <- values$dfTag %>%
        filter(docSelected) %>%
        dplyr::filter(!upos %in% c("PUNCT", "SYM", "NUM")) %>%
        group_by(upos) %>%
        count() %>%
        arrange(desc(n)) %>%
        rename(PoS = upos)

      values$posPlotly <- freqPlotly(values$freqPOS,x="n",y="PoS",n=nrow(values$freqPOS), xlabel="Frequency",ylabel="Part of Speech", scale="identity")

      values$freqPOSData <- values$freqPOS  %>%
        rename(Frequency = n)
    }
  )

  output$posPlot <- renderPlotly({
    posFreq()
    values$posPlotly
  })

  output$posTable <- renderDT(server=FALSE,{
    posFreq()
    DTformat(values$freqPOSData, left=1, right=2, round=0, numeric=2, filename="POSFreqList", dom=FALSE, size="110%")
  })

  output$posExport <- downloadHandler(
    filename = function() {

      paste("PoSFrequency-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      values$posGgplot <- freqGgplot(data.frame(values$freqPOS),x=2, y=1,n=length(values$freqPOS$PoS),
                                     title = "PoS Frequency")
      ggsave(filename = file, plot = values$posGgplot, dpi = dpi, height = values$h, width = values$h*2, bg="transparent")
    },
    contentType = "png"
  )

  ## Report

  observeEvent(input$posReport,{
    values$posGgplot <- freqGgplot(data.frame(values$freqPOS),x=2, y=1,n=length(values$freqPOS$PoS),
                                   title = "PoS Tag Frequency")
    if(!is.null(values$freqPOS)){
      list_df <- list(values$freqPOSData)
      list_plot <- list(values$posGgplot)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "PoSFreq", wb=values$wb)
      values$wb <- wb
      popUp(title="PoS Tag Frequency", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })

  ## Words in Context ----

  wordsInContextMenu <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$wordsContSearch},
    valueExpr = {
      word_search <- req(tolower(input$wordsContSearch))
      values$wordInContext <- values$dfTag %>%
        filter(docSelected) %>%
        filter(tolower(lemma) %in% word_search | tolower(token) %in% word_search) %>%
        ungroup() %>% select(doc_id, lemma, token, sentence_hl) %>%
        rename(Lemma=lemma, Token=token, Sentence=sentence_hl)
    })

  output$wordsContData <- renderDT(server=FALSE,{
    wordsInContextMenu()
    DTformat(values$wordInContext, size='100%', button=TRUE)
  }, escape=FALSE)


  ## Reinert Clustering ----
  dendReinFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$w_reinclusteringApply},
    valueExpr ={
      values$reinert <- tall::reinert(x=values$dfTag, k = input$w_rein_k, term = input$termReinClustering,
        segment_size = input$w_rein_segments_size,
        min_segment_size = input$w_rein_min_segments,
        min_split_members = input$w_rein_min_split_members,
        cc_test = input$w_rein_cc_test, tsj = input$w_rein_tsj
      )

      values$tc <- term_per_cluster(values$reinert, cutree=NULL, k=unique(values$reinert$group))

      #groups <- tibble(uc=1:length(values$reinert$group), Cluster=values$reinert$group)
      values$tc$segments <- values$tc$segments %>%
        group_by(cluster) %>%
        arrange(uc, .by_group = TRUE) %>%
        select(-"uc")

      output$ReinCutree <- renderUI({
        req(input$w_rein_k)
        fluidRow(column(9),
                 column(3,
                        selectInput("ReinCutree",
                                    label = "Dendrogram Pruning",
                                    choices = input$w_rein_k:1,
                                    selected = input$w_rein_k
                        )))
      })

      values$ReinertDendrogram <- dend2vis(values$reinert, labelsize=10, nclusters = input$w_rein_k, community=FALSE)
    }
  )

  cutree_event <- eventReactive(ignoreNULL = TRUE,
                                eventExpr = {input$ReinCutree},
                                valueExpr ={
                                  values$ReinertDendrogram <- dend2vis(values$reinert,
                                                                       labelsize=10,
                                                                       nclusters = as.numeric(input$ReinCutree),
                                                                       community=FALSE)
                                })

  output$w_ReinClusteringPlot <- renderVisNetwork({
    dendReinFunction()
    cutree_event()
    values$ReinertDendrogram
  })

  output$w_ReinClusteringTableSegments <- renderDT({
    dendReinFunction()
    # find sentences containing the tokens/lemmas
    DTformat(values$tc$segments, size='100%', button=TRUE)
  })

  output$w_ReinClusteringTableTerms <- renderDT({
    dendReinFunction()
    DTformat(values$tc$terms %>%
               mutate(freq = round(freq*100,1)) %>%
               select(term, freq, chi_square, p_value, sign, cluster) %>%
               rename("Term" = term,
                      "% in Cluster" = freq,
                      "Chi^2" = chi_square,
                      "P-Value" = p_value,
                      "Sign" = sign,
                      "Cluster" = cluster),
             size='85%', button=FALSE, numeric=c(3,4), round=3)
  })

  ## Clustering ----

  ## Dendrogramm ----
  dendFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$w_clusteringApply},
    valueExpr ={
      results <- clustering(LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected), n=input$w_clusteringNMax,
                            group="doc_id", minEdges=25, term=input$termClustering,
                            normalization=input$w_clusteringSimilarity)
      values$wordCluster <- results$cluster
      values$wordCluster<-values$wordCluster %>%
                          rename(Word=word, Group=group, Frequency=frequency)
      values$wordComm <- results$comm
      if (input$w_clusteringMode == "auto"){
        nclusters <- max(values$wordComm$membership)
      } else {
        nclusters <- min(input$w_nclusters, length(values$wordComm$membership)-1)
      }
      values$WordDendrogram <- dend2vis(values$wordComm, labelsize=input$w_clusteringLabelSize, nclusters = nclusters)
    }
  )

  output$w_clusteringPlot <- renderVisNetwork({
    dendFunction()
    values$WordDendrogram
  })

  output$w_clusteringTable <- renderDT(server=FALSE,{
    dendFunction()
    DTformat(values$wordCluster, size='100%',filename="ClusterWordsTable", pagelength=TRUE, left=1, right=NULL,
             numeric=NULL, dom=TRUE, filter="top")
  })

  ## export CLustering button
  output$w_clusteringExport <- downloadHandler(
    filename = function() {
      paste("Dendrogram-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      plot2png(values$WordDendrogram, filename=file, zoom = values$zoom)
    },
    contentType = "png"
  )

  ## Report

  observeEvent(input$w_clusteringReport,{
    if(!is.null(values$wordCluster)){
      popUp(title=NULL, type="waiting")
      sheetname <- "Clustering"
      list_df <- list(values$wordCluster)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileDend <- plot2png(values$WordDendrogram, filename="Clustering.png", zoom = values$zoom)
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileDend,res$col))
      popUp(title="Clustering Results", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })

  ## Correspondence Analysis ----

  # CA plot
  caPlotFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$caApply},
    valueExpr ={
      ## check to verify if groups exist or not
      if (input$groupCA == "Documents" & "ungroupDoc_id" %in% names(values$dfTag)){
        values$CA <- wordCA(backToOriginalGroups(LemmaSelection(values$dfTag)) %>% filter(docSelected), n=input$nCA, term=input$termCA, group = input$groupCA)
      } else {
        values$CA <- wordCA(LemmaSelection(values$dfTag) %>% filter(docSelected), n=input$nCA, term=input$termCA, group = input$groupCA)
      }
      ##
      values$CA <- caClustering(values$CA, nclusters = input$nClustersCA, nDim=input$nDimsCA, lim.contr=input$lim.contribCA)
      values$CAdimY <- as.numeric(input$dimPlotCA)*2
      values$CAdimX <- values$CAdimY-1
      values$plotCA <- ca2plotly(values$CA, dimX = values$CAdimX, dimY = values$CAdimY,
                                 topWordPlot = input$nCA, topDocPlot=input$nDocCA, threshold=0.03, labelsize = input$labelsizeCA, size=input$sizeCA, lim.contr=input$lim.contribCA)
      values$CADendrogram <- dend2vis(values$CA$clustering$h, labelsize=input$labelsizeCA, nclusters=input$nClustersCA, community=FALSE)

      #wordCoordData
      values$CA$wordCoordData <- values$CA$wordCoord %>%
        select(label, everything()) %>%
        left_join(
          data.frame(label=names(values$CA$clustering$groups), Group=values$CA$clustering$groups), by = "label") %>%
        rename(Label = label) %>%
        select(Label, Group, everything()) %>%
        rename(Cluster=Group)

      #contribData
      values$CA$contribData <- values$CA$contrib %>%
        rownames_to_column() %>%
        rename(Label = rowname)

      #cosineData
      values$CA$cosineData <- values$CA$cosine %>%
        rownames_to_column() %>%
        rename(Label = rowname)

      #dfCA
      if (length(values$CA$ca$sv)<10)
      {values$dfCA <- data.frame(dim=paste0("Dim ",1:length(values$CA$ca$sv)),sv=(values$CA$ca$sv/sum(values$CA$ca$sv)*100), svcorr=values$CA$ca$eigCorrectedNorm)}
      else
      {values$dfCA <- data.frame(dim=paste0("Dim ",1:10),sv=(values$CA$ca$sv/sum(values$CA$ca$sv)*100)[1:10], svcorr=values$CA$ca$eigCorrectedNorm[1:10])}
      values$dfCA <- values$dfCA %>%
        rename("Factorial Dimension" = dim,
               "Singular Values" = sv,
               "Corrected Explained Inertia" = svcorr)

    }
  )

  output$caPlot <- renderPlotly({
    caPlotFunction()
    values$plotCA
  })

  output$caDendrogram <- renderVisNetwork({
    caPlotFunction()
    values$CADendrogram
  })


  # CA Table
  output$caCoordTable <- renderDT(server=FALSE,{
    caPlotFunction()
    DTformat(values$CA$wordCoordData, size='100%',filename="CAWordCoordinatesTable", pagelength=TRUE, left=1, right=2:ncol(values$CA$wordCoordData),
             numeric=3:ncol(values$CA$wordCoordData), dom=TRUE, filter="top", round=3)
  })

  output$caContribTable <- renderDT(server=FALSE,{
    caPlotFunction()
    DTformat(values$CA$contribData,
             size='100%',filename="CAWordContributesTable", pagelength=TRUE, left=1, #right=2:(ncol(values$CA$contrib)+1),
             numeric=2:(ncol(values$CA$contrib)+1), dom=TRUE, filter="top", round=3)
  })

  output$caCosineTable <- renderDT(server=FALSE,{
    caPlotFunction()
    DTformat(values$CA$cosineData,
             size='100%',filename="CAWordCosinesTable", pagelength=TRUE, left=1, #right=2:(ncol(values$CA$cosine)+1),
             numeric=2:(ncol(values$CA$cosine)+1), dom=TRUE, filter="top", round=3)
  })

  output$caSingularValueTable <- renderDT(server=FALSE,{
    caPlotFunction()
    DTformat(values$dfCA,
             size='100%',filename="CAWordSingualValueTable", pagelength=TRUE, left=1, #right=2:3,
             numeric=2:3, dom=TRUE, filter="top", round=2)
  })


  output$caExport <- downloadHandler(
    filename = function() {
      paste("CAPlots-", Sys.Date(), ".zip", sep="")
    },
    content <- function(file) {
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      plot2png(values$plotCA, filename="CAMap.png", type="plotly")
      plot2png(values$CADendrogram, filename="CADendrogram.png", type="vis")
      zip(file,c("CAMap.png","CADendrogram.png"))
    },
    contentType = "zip"
  )

  ## Report

  observeEvent(input$caReport,{
    if(!is.null(values$CA)){
      popUp(title=NULL, type="waiting")
      sheetname <- "CorrespondenceAnalysis"
      list_df <- list(values$CA$wordCoordData
                                          ,values$CA$contribData
                                          ,values$CA$cosineData
                                          ,values$dfCA
                                          )
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileplotCA <- plot2png(values$plotCA, filename="CAMap.png", type="plotly", zoom = values$zoom)
      values$fileCADendrogram <-plot2png(values$CADendrogram, filename="CADendrogram.png", type="vis",zoom = values$zoom)
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileplotCA,res$col),
                                c(sheetname=res$sheetname, values$fileCADendrogram,res$col))
      popUp(title="Correspondence Analysis Results", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })


  ## Network ----

  ## Co-word analysis ----
  netFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$w_networkCoocApply},
    valueExpr ={
      switch(input$w_groupNet,
             Groups = {group = "doc_id"},
             Documents = {group <- "doc_id"},
             Paragraphs = {group <- c("doc_id", "paragraph_id")},
             Sentences = {group <- c("doc_id", "sentence_id")})
      ## check to verify if groups exist or not

      #community.repulsion <- as.numeric(gsub("%","",input$community.repulsion))/100
      community.repulsion <- 0

      if (input$w_groupNet == "Documents" & "ungroupDoc_id" %in% names(values$dfTag)){
        values$network <- network(backToOriginalGroups(LemmaSelection(values$dfTag)) %>% filter(docSelected), term=input$w_term, group=group,
                                  n=input$nMax, minEdges=input$minEdges,
                                  labelsize=input$labelSize, opacity=input$opacity,
                                  interLinks=input$interLinks, normalization=input$normalizationCooc,
                                  remove.isolated=input$removeIsolated, community.repulsion=community.repulsion)
      } else {
        values$network <- network(LemmaSelection(values$dfTag) %>% filter(docSelected), term=input$w_term, group=group,
                                  n=input$nMax, minEdges=input$minEdges,
                                  labelsize=input$labelSize, opacity=input$opacity,
                                  interLinks=input$interLinks, normalization=input$normalizationCooc,
                                  remove.isolated=input$removeIsolated, community.repulsion=community.repulsion)
      }
      ## end check
      #net=values$network
      #save(net, file="network.rdata")

      values$netVis <- net2vis(nodes=values$network$nodes, edges=values$network$edges)

      #network$nodes
      if (is.na(values$network$nodes)[1]){
        values$network$nodesData <- data.frame(Word="", Frequency=NA, Group=NA,"Color Group"="")
        values$network$edgesData <- data.frame(From=NA,To=NA,"Co-occurence"=0,
                                               "Association Index"=0,
                                               "Cosine Similarity"=0,
                                               "Jaccard Index"=0,
                                               "Group From"=0,
                                               "Group To"=0)
      } else {
      values$network$nodesData <- values$network$nodes %>%
        select(label, value, group, color) %>%
        rename(Word=label,
               Frequency=value,
               Group=group,
               "Color Group"=color)

      #network$edges
      values$network$edgesData <- values$network$edges %>%
        select(term_from, term_to,group_from, group_to, s,sA, sC, sJ) %>%
        rename(From=term_from,
               To=term_to,
               "Co-occurence"=s,
               "Association Index"=sA,
               "Cosine Similarity"=sC,
               "Jaccard Index"=sJ,
               "Group From"=group_from,
               "Group To"=group_to)
      }

    }
  )

  output$w_networkCoocPlot <- renderVisNetwork({
    netFunction()
    values$netVis
  })

  output$w_networkCoocNodesTable <- renderDT(server=FALSE,{
    netFunction()
    DTformat(values$network$nodesData, size='100%',filename="NetworkWordsTable", pagelength=TRUE, left=NULL, right=NULL,
             numeric=NULL, dom=TRUE, filter="top")
  })

  output$w_networkCoocEdgesTable <- renderDT(server=FALSE,{
    netFunction()
    DTformat(values$network$edgesData,
             size='100%',filename="NetworkLinksTable", numeric=6:8, round=4)
  })

  ## export Network button
  output$w_networkCoocExport <- downloadHandler(
    filename = function() {
      paste("Network-Docs-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      plot2png(values$netVis, filename=file, zoom = values$zoom)
    },
    contentType = "png"
  )


  ## Click on visNetwork: WORDS IN CONTEXT ----
  observeEvent(ignoreNULL = TRUE,
               eventExpr={input$click},
               handlerExpr = {
                 if (input$click!="null"){
                 showModal(plotModalTermNet(session))
                 }
               })

  plotModalTermNet <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Words in Context"))),
      DTOutput(ns("wordInContextNet")),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(label="Close", inputId = "closePlotModalTermNet", style="color: #ffff;",
                     icon = icon("remove", lib = "glyphicon"))
      ),
    )
  }

  observeEvent(input$closePlotModalTermNet,{
    removeModal(session = getDefaultReactiveDomain())
    #session$sendCustomMessage("click", 'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  output$wordInContextNet <- renderDT(server=FALSE,{
    if (!is.null(input$click)) id <- input$click
    switch(input$sidebarmenu,
           "w_networkGrako"={
             word_search<- values$grako$nodes$title[values$grako$nodes$id==id]

             selectedEdges <- values$grako$edges %>%
               filter(term_from %in% word_search | term_to %in% word_search) %>%
               mutate(grako = paste0(term_from, " ",term_to))

             sentences <- values$grako$multiwords %>%
               filter(grako %in% selectedEdges$grako) %>%
               select(doc_id, sentence_hl) %>%
               distinct()
           },
           "overview"={
             word_search<- values$WC2VIS$x$nodes$label[values$WC2VIS$x$nodes$id==id]
             sentences <- values$dfTag %>%
               filter(docSelected) %>%
               filter(lemma %in% word_search|token %in% word_search) %>%
               ungroup() %>% select(doc_id, lemma, token, sentence_hl)
           },
           {
             word_search<- values$network$nodes$label[values$network$nodes$id==id]
             sentences <- values$dfTag %>%
               filter(docSelected) %>%
               filter(lemma %in% word_search) %>%
               ungroup() %>% select(doc_id, lemma, token, sentence_hl)
           })

    # find sentences containing the tokens/lemmas
    DTformat(sentences, size='100%', button = TRUE)
  }, escape=FALSE)



  ## Click on Dendrogram: WORDS IN CONTEXT ----
  observeEvent(ignoreNULL = TRUE,
               eventExpr={input$click_dend},
               handlerExpr = {
                 if (input$click_dend!="null"){
                   showModal(plotModalTermDend(session))
                 }
               })

  plotModalTermDend <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Words in Context"))),
      DTOutput(ns("wordInContextDend")),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(label="Close", inputId = "closeplotModalTermDend", style="color: #ffff;",
                     icon = icon("remove", lib = "glyphicon"))
      ),
    )
  }

  observeEvent(input$closeplotModalTermDend,{
    removeModal(session = getDefaultReactiveDomain())
    #session$sendCustomMessage("click_dend",'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  output$wordInContextDend <- renderDT(server=FALSE,{
    if (!is.null(input$click_dend)) id <- unlist(input$click_dend)
    switch(input$sidebarmenu,
           "w_clustering"={
             words_id <- c(id, unlist(values$WordDendrogram$x$nodes$neib[values$WordDendrogram$x$nodes$id==id]))
             words <- unlist(values$WordDendrogram$x$nodes$label[values$WordDendrogram$x$nodes$id %in% words_id])
             word_search <- words[!is.na(words)]
           },
           "ca"={
             words_id <- c(id, unlist(values$CADendrogram$x$nodes$neib[values$CADendrogram$x$nodes$id==id]))
             words <- unlist(values$CADendrogram$x$nodes$label[values$CADendrogram$x$nodes$id %in% words_id])
             word_search <- words[!is.na(words)]
           })

    sentences <- values$dfTag %>%
      filter(docSelected) %>%
      filter(lemma %in% word_search) %>%
      ungroup() %>% select(doc_id, lemma, token, sentence_hl)

    # find sentences containing the tokens/lemmas
    DTformat(sentences, size='100%', button=TRUE)
  }, escape=FALSE)

  ## Report

  observeEvent(input$w_networkCoocReport,{
    if(!is.null(values$network$nodes)){
      popUp(title=NULL, type="waiting")
      sheetname <- "CoWord"
      list_df <- list(values$network$nodesData
                      ,values$network$edgesData
      )
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$filenetVis <- plot2png(values$netVis, filename="CoWord.png", zoom = values$zoom)
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$filenetVis,res$col))
      popUp(title="Co-Word Analysis Results", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })


  ## Click on Reinert Dendrogram: WORDS IN CONTEXT ----
  observeEvent(ignoreNULL = TRUE,
               eventExpr={input$click_rein},
               handlerExpr = {
                 if (input$click_rein!="null"){
                   id <- unlist(input$click_rein)
                   words_id <- c(id, unlist(values$ReinertDendrogram$x$nodes$neib[values$ReinertDendrogram$x$nodes$id==id]))
                   words <- unlist(values$ReinertDendrogram$x$nodes$label[values$ReinertDendrogram$x$nodes$id %in% words_id])
                   word_search <- as.numeric(words[!is.na(words)])
                   values$word_search_rein <- word_search

                   if (length(word_search)>0){
                     #values$tc <- term_per_cluster(res, cutree = NULL, k=word_search)
                     values$tc_k <- values$tc
                     values$tc_k$terms <- values$tc_k$terms %>% filter(cluster %in% word_search)
                     values$tc_k$segments <- values$tc_k$segments %>% filter(cluster %in% word_search)
                     segments <- values$tc
                     values$tc_k <- highlight_segments(values$tc_k, n=10)

                     # values$tc_k$segments <- values$tc_k$segments %>%
                     #   group_by(doc_id) %>%
                     #   arrange(uc, .by_group = TRUE) %>%
                     #   select(doc_id, uc, segment, cluster)
                   }
                   showModal(plotModalTermRein(session))
                 }
               })

  plotModalTermRein <- function(session) {
    ns <- session$ns
      modalDialog(
        tabsetPanel(type = "tabs",
                    tabPanel("Terms by Cluster",
                             h3(strong((paste0("Terms associated to Cluster(s): ",paste0(values$word_search_rein,collapse=", "), collape="")))),
                             plotlyOutput(ns("plotInContextRein"))
        ),
        tabPanel("Segments by Cluster",
                 h3(strong((paste0("Segments associated to Cluster(s): ",paste0(values$word_search_rein,collapse=", "), collape="")))),
                 DTOutput(ns("wordInContextRein"))
        ),
        ),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton(label="Close", inputId = "closeplotModalTermRein", style="color: #ffff;",
                       icon = icon("remove", lib = "glyphicon"))
        ),
      )
  }

  observeEvent(input$closeplotModalTermRein,{
    removeModal(session = getDefaultReactiveDomain())
    #session$sendCustomMessage("click_dend",'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  output$wordInContextRein <- renderDT(server=FALSE,{
    # find sentences containing the tokens/lemmas
    DTformat(values$tc_k$segments, nrow=5, size='80%', button=TRUE)
  }, escape=FALSE)

  output$plotInContextRein <- renderPlotly({
    reinPlot(values$tc_k$terms, nPlot=10)
  })


  ## Report

  observeEvent(input$w_networkCoocReport,{
    if(!is.null(values$network$nodes)){
      popUp(title=NULL, type="waiting")
      sheetname <- "CoWord"
      list_df <- list(values$network$nodesData
                      ,values$network$edgesData
      )
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$filenetVis <- plot2png(values$netVis, filename="CoWord.png", zoom = values$zoom)
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$filenetVis,res$col))
      popUp(title="Co-Word Analysis Results", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })



  ## GRAKO ----
  grakoFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$w_networkGrakoApply},
    valueExpr ={
      values$grako <- grako(values$dfTag %>% filter(docSelected), n=input$grakoNMax, minEdges=input$grakoMinEdges,
                            labelsize=input$grakoLabelSize, opacity=input$grakoOpacity,
                            normalization=input$grakoNormalization,
                            singleWords=input$grakoUnigram,term=input$grako_term)

      values$grakoVis <- grako2vis(nodes=values$grako$nodes, edges=values$grako$edges)

      #grako$nodes
      values$grako$nodesData <- values$grako$nodes %>%
        select(upos, label, value) %>%
        mutate(label=gsub("<.*?>", "", label)) %>%
        rename("Part of Speech"=upos,
               Word=label,
               Frequency=value) %>%
        relocate("Part of Speech", .after = last_col())

      #grako$edges
      values$grako$edgesData <- values$grako$edges %>%
        select(term_from, term_to,upos_from, upos_to, role, s,sA, sC, sJ) %>%
        rename(From=term_from,
               To=term_to,
               "Co-occurence"=s,
               "Association Index"=sA,
               "Cosine Similarity"=sC,
               "Jaccard Index"=sJ,
               "PoS From"=upos_from,
               "PoS To"=upos_to,
               "Action"=role)

    }
  )

  output$w_networkGrakoPlot <- renderVisNetwork({
    grakoFunction()
    values$grakoVis
  })

  output$w_networkGrakoNodesTable <- renderDT(server=FALSE,{
    grakoFunction()
    DTformat(values$grako$nodesData, size='100%',filename="GrakoWordsTable", pagelength=TRUE, left=NULL, right=NULL,
             numeric=NULL, dom=TRUE, filter="top")
  })

  output$w_networkGrakoEdgesTable <- renderDT(server=FALSE,{
    grakoFunction()
    DTformat(values$grako$edgesData,
             size='100%',filename="GrakoLinksTable", numeric=7:9, round=4)
  })

  ## export Network button
  output$w_networkGrakoExport <- downloadHandler(
    filename = function() {
      paste("Grako-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      plot2png(values$grakoVis, filename=file, zoom = values$zoom)
    },
    contentType = "png"
  )

  ## Report

  observeEvent(input$w_networkGrakoReport,{
    if(!is.null(values$grako$nodes)){
      popUp(title=NULL, type="waiting")
      sheetname <- "Grako"
      list_df <- list(values$grako$nodesData
                      ,values$grako$edgesData
      )
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileGrako <- plot2png(values$grakoVis, filename="grako.png", zoom = values$zoom)
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileGrako,res$col))
      popUp(title="Grako Results", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })


  ## DOCUMENTS ----

  ## Topic Modeling ----
  ## K choice ----

  netTMKselect <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$d_tm_selectApply},
    valueExpr ={
      switch(input$groupTm,
             Groups = {groupTm = "doc_id"},
             {groupTm = input$groupTm})
      ## check to verify if groups exist or not
      if (input$groupTm == "doc_id" & "ungroupDoc_id" %in% names(values$dfTag)){
        values$TMKresult <- tmTuning(backToOriginalGroups(LemmaSelection(values$dfTag)) %>% filter(docSelected), group=groupTm, term=input$termTm,
                                     metric=input$metric, n=input$nTm, top_by=input$top_by, minK=input$minK, maxK=input$maxK, Kby=input$Kby)
      } else {
        values$TMKresult <- tmTuning(LemmaSelection(values$dfTag) %>% filter(docSelected), group=groupTm, term=input$termTm,
                                     metric=input$metric, n=input$nTm, top_by=input$top_by, minK=input$minK, maxK=input$maxK, Kby=input$Kby)
      }
      ## End check ###

      values$TMKplot <- tmTuningPlot(values$TMKresult, metric=input$metric)

      #d_tm_selectTable
      values$df <- values$TMKresult %>% arrange(topics) %>%
        rename("N. of Topics" = topics)
      values$df$Normalized <- (values$df[,2]-min(values$df[,2]))/diff(range(values$df[,2]))

    }
  )

  output$d_tm_selectPlot <- renderPlotly({
    netTMKselect()
    values$TMKplot
  })

  output$d_tm_selectTable <- renderDataTable({
    netTMKselect()
    # df <- values$TMKresult %>% arrange(topics) %>%
    #   rename("N. of Topics" = topics)
    # df$Normalized <- (df[,2]-min(df[,2]))/diff(range(df[,2]))

    DTformat(values$df, numeric=c(2,3), round=2, nrow=nrow(df), size="110%")
  })

  output$d_tm_selectExport <- downloadHandler(
    filename = function() {
      paste("TMTopicSelection-", Sys.Date(), ".png", sep="")
    },
    content <- function(file) {
      plot2png(values$TMKplot, filename=file, zoom = values$zoom)
    },
    contentType = "png"
  )

  ## Report

  observeEvent(input$d_tm_selectReport,{
    if(!is.null(values$df)){
      popUp(title=NULL, type="waiting")
      sheetname <- "KChoice"
      list_df <- list(values$df)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileKchoice <- plot2png(values$TMKplot, filename="kchoiche.png", zoom = values$zoom)
      values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$fileKchoice,res$col))
      popUp(title="K choice Results", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })


  ## Model estimation ----

  netTMestim <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$d_tm_estimApply},
    valueExpr ={
      values$TMplotIndex <- 1
      values$TMdocIndex <- 1

      switch(input$groupTmEstim,
             Groups = {groupTmEstim = "doc_id"},
             {groupTmEstim = input$groupTmEstim})

      ## check to verify if groups exist or not
      if (input$groupTmEstim == "doc_id" & "ungroupDoc_id" %in% names(values$dfTag)){
        if (isTRUE(input$tmKauto)){
          values$TMKresult <- tmTuning(backToOriginalGroups(LemmaSelection(values$dfTag)) %>% filter(docSelected),
                                       group=groupTmEstim, term=input$termTmEstim,
                                       metric="CaoJuan2009", n=input$nTmEstim, top_by=input$top_byEstim,
                                       minK=2, maxK=20, Kby=1)
          K <- values$TMKresult %>% slice_min(CaoJuan2009, n=1)
          values$tmK <- K$topics
        } else{
          values$tmK <- input$KEstim
        }
        values$TMplotList <- split(1:values$tmK, ceiling(seq_along(1:values$tmK)/3))
        values$TMestim_result <- tmEstimate(backToOriginalGroups(LemmaSelection(values$dfTag)) %>% filter(docSelected),
                                            K=values$tmK, group=groupTmEstim,
                                            term=input$termTmEstim, n=input$nTmEstim, top_by=input$top_byEstim)
      } else {
        if (isTRUE(input$tmKauto)){
          values$TMKresult <- tmTuning(LemmaSelection(values$dfTag) %>% filter(docSelected), group=groupTmEstim,
                                       term=input$termTmEstim, metric="CaoJuan2009", n=input$nTmEstim,
                                       top_by=input$top_byEstim, minK=2, maxK=20, Kby=1)
          K <- values$TMKresult %>% slice_min(CaoJuan2009, n=1)
          values$tmK <- K$topics
        } else{
          values$tmK <- input$KEstim
        }
        values$TMplotList <- split(1:values$tmK, ceiling(seq_along(1:values$tmK)/3))
        values$TMestim_result <- tmEstimate(LemmaSelection(values$dfTag) %>% filter(docSelected), K=values$tmK, group=groupTmEstim,
                                            term=input$termTmEstim, n=input$nTmEstim, top_by=input$top_byEstim)
      }
      ## End check ###

      ### BETA PROBABILITY
      values$beta <- values$TMestim_result$beta

      names(values$beta)[2:ncol(values$beta)] <- paste0("Topic ",1:(ncol(values$beta)-1))
      values$tmHeatmap <- tmHeatmap(values$beta)

      ### THETA PROBABILITY
      values$theta <- values$TMestim_result$theta
      names(values$theta)[2:ncol(values$theta)] <- paste0("Topic ",1:(ncol(values$theta)-1))
    }
  )

  output$d_tm_networkPlot <- renderPlotly({
    netTMestim()
    values$tmHeatmap$Hplot
    })

  # output$d_tm_networkTable <- renderDataTable(server = FALSE,{
  #   netTMestim()
  #   DTformat(values$tmHeatmap$H, left=1, numeric=c(2:ncol(values$tmHeatmap$H)), round=4, nrow=10, size="85%", filename = "TopicModel_TopicCorrelations")
  # })

  observeEvent(input$TMplotRight,{
    if (values$TMplotIndex<ceiling(req(values$tmK)/3)){
      values$TMplotIndex <- values$TMplotIndex+1
    }
  })

  observeEvent(input$TMplotLeft,{
    if (req(values$TMplotIndex)>1){
      values$TMplotIndex <- values$TMplotIndex-1
    }
  })

  output$d_tm_estimTPlot1 <- renderPlotly({
    netTMestim()
    if (!values$TMplotIndex %in% 1:length(values$TMplotList)) values$TMplotIndex <- 1
    topic1 <- values$TMplotList[[values$TMplotIndex]]
    values$TMestim_plot1 <- tmTopicPlot(values$TMestim_result$beta, topic=topic1[[1]], nPlot=input$nTopicPlot)
    values$TMestim_plot1
  })

  output$d_tm_estimTPlot2 <- renderPlotly({
    topic2 <- values$TMplotList[[values$TMplotIndex]]
    if (length(topic2)>=2){
      values$TMestim_plot2 <- tmTopicPlot(values$TMestim_result$beta, topic=topic2[[2]], nPlot=input$nTopicPlot)
      values$TMestim_plot2
    }
  })

  output$d_tm_estimTPlot3 <- renderPlotly({
    topic3 <- values$TMplotList[[values$TMplotIndex]]
    if (length(topic3)==3){
      values$TMestim_plot3 <- tmTopicPlot(values$TMestim_result$beta, topic=topic3[[3]], nPlot=input$nTopicPlot)
      values$TMestim_plot3
    }
  })

  output$d_tm_estimBpTable <- renderDataTable(server = FALSE, {
    netTMestim()
    DTformat(values$beta, left=1,numeric=c(2:ncol(values$TMestim_result$beta)), round=4, nrow=10, size="85%", filename = "TopicModel_BetaTable")
  })

  observeEvent(input$TMdocRight,{
    if (values$TMdocIndex<ceiling(req(values$tmK)/3)){
      values$TMdocIndex <- values$TMdocIndex+1
    }
  })

  observeEvent(input$TMdocLeft,{
    if (req(values$TMdocIndex)>1){
      values$TMdocIndex <- values$TMdocIndex-1
    }
  })

  output$d_tm_DocPlot1 <- renderPlotly({
    netTMestim()
    if (!values$TMdocIndex %in% 1:length(values$TMplotList)) values$TMdocIndex <- 1
    topic1 <- values$TMplotList[[values$TMdocIndex]]
    values$TMdoc_plot1 <- tmDocPlot(values$TMestim_result$theta, topic=topic1[[1]], nPlot=input$nTopicPlot)
    values$TMdoc_plot1
  })

  output$d_tm_DocPlot2 <- renderPlotly({
    topic2 <- values$TMplotList[[values$TMdocIndex]]
    if (length(topic2)>=2){
      values$TMdoc_plot2 <- tmDocPlot(values$TMestim_result$theta, topic=topic2[[2]], nPlot=input$nTopicPlot)
      values$TMdoc_plot2
    }
  })

  output$d_tm_DocPlot3 <- renderPlotly({
    topic3 <- values$TMplotList[[values$TMdocIndex]]
    if (length(topic3)==3){
      values$TMdoc_plot3 <- tmDocPlot(values$TMestim_result$theta, topic=topic3[[3]], nPlot=input$nTopicPlot)
      values$TMdoc_plot3
    }
  })

  output$d_tm_estimTpTable <- renderDataTable(server = FALSE,{
    netTMestim()
    DTformat(values$theta, left=1,numeric=c(2:ncol(values$TMestim_result$theta)), round=4, nrow=10, size="85%", filename = "TopicModel_ThetaTable")
  })

  output$d_tm_estimExport <- downloadHandler(
    filename = function() {

      paste("TopicModeling-", Sys.Date(), ".zip", sep="")
    },
    content <- function(file) {
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$tmGplotBeta <- topicGplot(values$TMestim_result$beta, nPlot=input$nTopicPlot, type="beta")
      values$tmGplotTheta <- topicGplot(values$TMestim_result$theta, nPlot=input$nTopicPlot, type="theta")
      ggsave(filename = "TMTermPlots.png", plot = values$tmGplotBeta, dpi = dpi, height = values$h, width = values$h*2, bg="transparent")
      ggsave(filename = "TMDocPlots.png", plot = values$tmGplotTheta, dpi = dpi, height = values$h, width = values$h*2, bg="transparent")
      zip(file,c("TMTermPlots.png","TMDocPlots.png"))
    },
    contentType = "zip"
  )

 ## Report

  observeEvent(input$d_tm_estimReport,{
    if(!is.null(values$TMestim_result$beta)){
      popUp(title=NULL, type="waiting")
      values$tmGplotBeta <- topicGplot(values$TMestim_result$beta, nPlot=input$nTopicPlot, type="beta")
      values$tmGplotTheta <- topicGplot(values$TMestim_result$theta, nPlot=input$nTopicPlot, type="theta")
      list_df <- list(values$beta, values$theta)
      list_plot <- list(values$tmGplotBeta,values$tmGplotTheta)
      wb <- addSheetToReport(list_df,list_plot,sheetname = "ModelEstim", wb=values$wb)
      values$wb <- wb
      popUp(title="Model Estimation Results", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })


  ## Polarity detection ----

  output$lexiconD_polarity <- renderUI({
    if (values$language == "english"){
      selectInput(
        inputId = "lexiconD_polarity", label="Select lexicon",
        choices = c("huliu",
                    "loughran_mcdonald",
                    "nrc"),
        selected = "huliu"
      )
    }
    # )
  })

  ## Model estimation ----
  docPolarityEstim <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$d_polDetApply},
    valueExpr ={
      choices = c("english","italian","french","german","spanish","afrikaans","arabic","armenian","basque","belarusian","bulgarian","catalan","chinese",
                  "croatian","czech","danish","dutch","estonian","finnish","galician","greek","hebrew","hindi","hungarian","indonesian","irish","japanese",
                  "korean","latin","latvian","lithuanian","maltese","marathi","norwegian","persian","polish","portuguese",
                  "romanian","russian","serbian","slovak","slovenian","swedish","tamil","telugu","turkish","ukrainian","urdu","uyghur","vietnamese")
      if (values$language %in% choices){
        if (is.null(input$lexiconD_polarity)){
          lexiconD_polarity <- "huliu"
        }  else {
          lexiconD_polarity <- input$lexiconD_polarity
        }

        ## check to verify if groups exist or not
        if (input$groupPolarity == "doc_id" & "ungroupDoc_id" %in% names(values$dfTag)){
          values$docPolarity <- sentimentAnalysis(backToOriginalGroups(values$dfTag) %>% filter(docSelected), language = values$language, lexicon_model=lexiconD_polarity)
        } else {
          values$docPolarity <- sentimentAnalysis(values$dfTag %>% filter(docSelected), language = values$language, lexicon_model=lexiconD_polarity)
        }
        values$docPolPlots <- sentimentWordPlot(values$docPolarity$sent_data, n=10)
      }

      values$docPolarityOverallData <- values$docPolarity$sent_overall %>%
        select(doc_id, sentiment_polarity, doc_pol_clas, terms_positive, terms_negative) %>%
        rename(Polarity = sentiment_polarity,
               "Polarity Category" = doc_pol_clas,
               "Positive Words" = terms_positive,
               "Negative Words" = terms_negative)

    }
  )

  output$d_polPiePlot <- renderPlotly({
    docPolarityEstim()
    df <- values$docPolarity$sent_overall %>%
      count(doc_pol_clas) %>%
      rename("Polarity" = doc_pol_clas)
    values$sentimentPieChart <- sentimentPieChart(df)
    values$sentimentPieChart
  })

  output$d_polDensPlot <- renderPlotly({
    docPolarityEstim()
    values$sentimentDensityPlot <- sentimentDensityPlot(values$docPolarity$sent_overall$sentiment_polarity, from = -1, to=1)
    values$sentimentDensityPlot
  })

  output$d_polBoxPlot <- renderPlotly({
    docPolarityEstim()
    values$sentimentBoxPlot <- sentimentBoxPlot(values$docPolarity$sent_overall)
    values$sentimentBoxPlot
  })

  output$d_polDetPlotPos <- renderPlotly({
    docPolarityEstim()
    values$docPolPlots$positive
  })
  output$d_polDetPlotNeg <- renderPlotly({
    docPolarityEstim()
    values$docPolPlots$negative
  })


  output$d_polDetTable <- renderDT(server=FALSE,{
    docPolarityEstim()
    DTformat(values$docPolarityOverallData, filename = "DocPolarity", left=c(2,4,5,6), numeric = 3, round=4, button=TRUE)
  })

    output$d_polDetExport <- downloadHandler(
      filename = function() {
        paste("PolarityPlots-", Sys.Date(), ".zip", sep="")
      },
      content <- function(file) {
        #go to a temp dir to avoid permission issues
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- c("PieChart.png", "DensDensity.png","BoxPlot.png", "Positive.png", "Negative.png")
        plot2png(values$sentimentPieChart, filename=files[1], zoom = values$zoom)
        plot2png(values$sentimentDensityPlot, filename=files[2], zoom = values$zoom)
        plot2png(values$sentimentBoxPlot, filename=files[3], zoom = values$zoom)
        plot2png(values$docPolPlots$positive, filename=files[4], zoom = values$zoom)
        plot2png(values$docPolPlots$negative, filename=files[5], zoom = values$zoom)
        zip(file,files)
      },
      contentType = "zip"
    )


## Report

observeEvent(input$d_polDetReport,{
  if(!is.null(values$docPolarityOverallData)){
    popUp(title=NULL, type="waiting")
    sheetname <- "PolarityDetection"
    list_df <- list(values$docPolarityOverallData
    )
    res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
    #values$wb <- res$wb
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    files <- c("PieChart.png", "DensDensity.png","BoxPlot.png", "Positive.png", "Negative.png")
    values$filePieChart <- plot2png(values$sentimentPieChart, filename=files[1], zoom = values$zoom)
    values$fileDensityPlot <- plot2png(values$sentimentDensityPlot, filename=files[2], zoom = values$zoom)
    values$fileBoxPlot <- plot2png(values$sentimentBoxPlot, filename=files[3], zoom = values$zoom)
    values$filedocPolPos <- plot2png(values$docPolPlots$positive, filename=files[4], zoom = values$zoom)
    values$filedocPolNeg <- plot2png(values$docPolPlots$negative, filename=files[5], zoom = values$zoom)
    values$list_file <- rbind(values$list_file,
                              c(sheetname=res$sheetname,values$filePieChart,res$col),
                              c(sheetname=res$sheetname, values$fileDensityPlot,res$col),
                              c(sheetname=res$sheetname, values$fileBoxPlot,res$col),
                              c(sheetname=res$sheetname, values$filedocPolPos,res$col),
                              c(sheetname=res$sheetname, values$filedocPolNeg,res$col)
                              )
    popUp(title="Polarity Detection Results", type="success")
    values$myChoices <- sheets(values$wb)
  } else {
    popUp(type="error")
  }
})


  ## Summarization ----

output$optionsUnitSummarization <- renderUI({
  selectInput(
    inputId = 'unit_selection', label="Summarize ", choices = c("Groups","Documents"),
    selected = "Documents",
    multiple=FALSE,
    width = "100%"
  )
})

output$optionsSummarization <- renderUI({
    selectInput(
      inputId = 'document_selection',
      label = ifelse(input$unit_selection=="Documents","Select Document","Select Group"),
      choices = ids(values$dfTag %>% dplyr::filter(docSelected),type=input$unit_selection),
               multiple=FALSE,
               width = "100%"
        )
})


  docExtraction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {input$d_summarizationApply},
    valueExpr ={
      values$docExtracted <- textrankDocument(values$dfTag, id=input$document_selection)
      values$docExtraction <- abstractingDocument(values$docExtracted$s,n="5%",id=input$document_selection)
      values$docExtraction$sentences <- values$docExtracted$sentences %>% rename(S_id=textrank_id, Ranking=textrank)
    })

  output$sliderAbstractData <- renderUI({
    docExtraction()
    choices <- c("More Concise",paste0(seq(from = 10, to = 95,by = 5),"%"),"Less Concise")
    sliderTextInput(inputId = "sliderAbstractData",
                    label = "Summarization",
                    choices = choices,
                    selected = choices[1],
                    grid = FALSE,
                    hide_min_max = FALSE,
                    animate = TRUE
    )
  })

  docExtractionVisualize <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr ={input$sliderAbstractData},
    valueExpr = {
      req(values$docExtracted)
      values$docExtraction <- abstractingDocument(values$docExtracted$s,n=input$sliderAbstractData,id=input$document_selection)
      values$docExtraction$sentences <- values$docExtracted$sentences %>% rename(S_id=textrank_id, Ranking=textrank)
    }
  )

  output$abstractData <- renderUI({
    #docExtraction()
    docExtractionVisualize()
    HTML(values$docExtraction$abstract)
  })


  output$RelSentData <- renderDT(server=FALSE,{
    docExtraction()
    DTformat(values$docExtraction$sentences, nrow=10, size='85%', title=paste0("Doc_id: ",input$document_selection), left=1:2,numeric=3, round=4)
  })

  output$documentData <- renderDT(server=FALSE,{
    docExtraction()

    DTformat(values$docExtraction$document, nrow=3, size='100%', title=paste0("Doc_id: ",input$document_selection), left=2)
  })

  ## Report

  observeEvent(input$d_summarizationReport,{
    if(!is.null(values$docExtraction$sentences)){
      popUp(title=NULL, type="waiting")
      sheetname <- "Summarization"

      values$docExtraction$abstractData <- data.frame("Abstract"=values$docExtraction$abstract)
      values$docExtraction$abstractData <- values$docExtraction$abstractData %>%
        mutate(Abstract= gsub("<.*?>", "", Abstract))

      list_df <- list(as.data.frame(values$docExtraction$abstractData),
                      values$docExtraction$sentences,
                      values$docExtraction$document)
      res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
      #values$wb <- res$wb
      popUp(title="Summarization Results", type="success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type="error")
    }
  })



  ## REPORT ----
  ### Report Save xlsx ----
  output$report.save <- downloadHandler(
    filename = function() {
      paste("TallReport-", Sys.Date(), ".xlsx", sep="")
    },
    content <- function(file) {
      wb_export <- copyWorkbook(values$wb)
      if (nrow(values$list_file)>0){
        wb_export <- addScreenWb(df=values$list_file, wb=wb_export)#, width=10, height=7, dpi=300)
      }
      sheetToRemove <- setdiff(sheets(wb_export),input$reportSheets)
      if (length(sheetToRemove)>0) for (i in sheetToRemove) removeWorksheet(wb_export,i)
      sheetToAdd <- sheets(wb_export)
      for (i in sheetToAdd) setColWidths(wb_export,sheet=i,cols=1,widths = 30, hidden = FALSE)
      openxlsx::saveWorkbook(wb_export, file = file)
    },
    contentType = "xlsx"
  )

  ### Report UI elements
  observe({
    output$reportSheets <- renderUI({
      prettyCheckboxGroup(
        inputId = "reportSheets",
        label = NULL, #short2long(df=values$dfLabel, myC=values$myChoices),
        choices = short2long(df=values$dfLabel, myC=values$myChoices),
        selected = values$myChoices,
        icon = icon("check"),
        animation = "pulse",
        status = "primary",
        bigger = T,
        fill = TRUE
      )
    })
  })

  observe({
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "reportSheets",
      #label = short2long(df=values$dfLabel, myC=values$myChoices),
      choices = short2long(df=values$dfLabel, myC=values$myChoices),
      selected = if(!input$noSheets) values$myChoices,
      prettyOptions = list(
        animation = "pulse",
        status = "info",
        bigger = T
      )
    )
  })

  observe({
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "reportSheets",
      choices = short2long(df=values$dfLabel, myC=values$myChoices),
      selected = if(input$allSheets) values$myChoices,
      prettyOptions = list(
        animation = "pulse",
        status = "info",
        bigger = T
      )
    )
  })

  observeEvent(input$deleteAll, {
    ask_confirmation(
      inputId = "delete_confirmation",
      title = "Want to confirm?",
      text = "All the results will be removed from the report",
      type = "warning",
      btn_labels = c("CANCEL", "CONFIRM"),
    )
  })

  observeEvent(input$delete_confirmation, {
    if (isTRUE(input$delete_confirmation)) {
      values$myChoices <- "Empty Report"
      values$list_file <- data.frame(sheet=NULL,file=NULL,n=NULL)
      values$wb <-  openxlsx::createWorkbook()
    }
  }, ignoreNULL = TRUE
  )


  ## SETTINGS ----

  ## UTILITY ----

  observeEvent(input$d_summarizationView, {
    showModal(showDocumentSummarizationModal(session))
  })

  showDocumentSummarizationModal <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Document corpus"))),
      br(),
      uiOutput("showDocumentSummarization"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")),
    )
  }

  output$showDocumentSummarization <- renderUI({
    txt1 <- (paste0("Document ID: ",input$document_selection))
    doc <- values$dfTag %>% filter(doc_id==input$document_selection) %>%
      distinct(paragraph_id,sentence_id, sentence) %>%
      group_by(paragraph_id) %>%
      summarize(paragraph=paste0(sentence,collapse=" ")) %>%
      ungroup()
    txt2 <- paste(doc$paragraph,collapse="<br><br>")
    text <- paste0(txt1,"<br><br>",txt2)

    tagList(
      div(
        h4(HTML(text)),
        style="text-align:left")
    )
  })

  ## table click button ----
  observeEvent(input$button_id, {
    if (input$button_id!="null"){
      showModal(showDocumentModal(session))
    }
  })

  showDocumentModal <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Document corpus"))),
      br(),
      uiOutput("showDocument"),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(label="Close", inputId = "closeShowDocument", style="color: #ffff;",
                     icon = icon("remove", lib = "glyphicon"))
      ),
    )
  }

  observeEvent(input$closeShowDocument,{
    removeModal(session = getDefaultReactiveDomain())
    #session$sendCustomMessage("button_id", 'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  output$showDocument <- renderUI({
    if (input$sidebarmenu %in% c("import_tx","split_tx", "extInfo", "textNorm")){
      text <- values$txt %>% filter(doc_id==input$button_id)
      text <- gsub("\n\n","<br><br>",text$text)
    } else{
      txt1 <- (paste0("Document ID: ",input$button_id))
      doc <- values$dfTag %>% filter(doc_id==input$button_id) %>%
        distinct(paragraph_id,sentence_id, sentence) %>%
        group_by(paragraph_id) %>%
        summarize(paragraph=paste0(sentence,collapse=" ")) %>%
        ungroup()
      txt2 <- paste(doc$paragraph,collapse="<br><br>")
      text <- paste0(txt1,"<br><br>",txt2)
    }

    tagList(
      div(
        h4(HTML(text)),
        style="text-align:left")
    )
  })

  output$showDocumentInOption <- renderUI({
    if (input$sidebarmenu %in% c("import_tx","split_tx", "extInfo")){
      text <- values$txt %>% filter(doc_id==values$button_id)
      text <- gsub("\n\n","<br><br>",text$text)
    } else{
      txt1 <- (paste0("Document ID: ",input$button_id))
      doc <- values$dfTag %>% filter(doc_id==input$button_id) %>%
        distinct(paragraph_id,sentence_id, sentence) %>%
        group_by(paragraph_id) %>%
        summarize(paragraph=paste0(sentence,collapse=" ")) %>%
        ungroup()
      txt2 <- paste(doc$paragraph,collapse="<br><br>")
      text <- paste0(txt1,"<br><br>",txt2)
    }

    tagList(
      div(
        h4(HTML(text)),
        style="text-align:left")
    )
  })

  observeEvent(input$button_id_del, {
    if (input$sidebarmenu %in% c("import_tx","split_tx", "extInfo")){
      values$txt <- values$txt %>%
        mutate(doc_selected = ifelse(doc_id==input$button_id_del, FALSE, doc_selected))
    }
    output$dataImported <- DT::renderDT({
      #DATAloading()
      if (values$menu==0){
        DTformat(values$txt %>%
                   filter(doc_selected) %>%
                   mutate(text = paste0(substr(text,1,500),"...")) %>%
                   select(doc_id, text, everything()) %>%
                   select(-doc_selected, -text_original),
                 left=2, nrow=5, filter="none", button=TRUE, delete=TRUE)
      }
    })
  })

  ### SETTINGS ----
  observeEvent(input$cache,{
      deleteCache()
    }
  )

} # END SERVER

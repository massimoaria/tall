##  Server ####
source("tallFunctions.R", local=TRUE)

### da rimuovere alla fine
library(tall)
###

server <- function(input, output, session){
  session$onSessionEnded(stopApp)

  ### Initial values ----
  values <- reactiveValues()
  values$path <- NULL
  values$menu <- -1
  values$custom_lists <- NULL
  values$txt <- data.frame()
  values$list_file <- data.frame(sheet=NULL,file=NULL,n=NULL)
  values$POSTagSelected <- ""
  values$wb <-  openxlsx::createWorkbook()
  values$dfLabel <- dfLabel()
  values$posMwSel <- c("ADJ", "NOUN", "PROPN") # POS selected by default for multiword creation
  values$myChoices <- "Empty Report"
  #values$token <- token
  values$df <- df
  label_lang <- languages$repo
  names(label_lang) <- languages$short
  values$label_lang <- label_lang



  ### SIDEBARMENU ----
  output$rest_of_sidebar <- renderMenu({
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


### DATA ----

  ### Import directory ----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        values$path = choose.dir(default = readDirectoryInput(session, 'directory'),
                          caption="Choose a directory...")
        updateDirectoryInput(session, 'directory', value = values$path)
      }
    }
  )

  output$directory = renderText({
    readDirectoryInput(session, 'directory')
  })

  ### dataImported ----
  DATAloading<- eventReactive(input$runImport,{
    switch(input$load,
           import={
             if (!is.null(values$path)){

               txt <- read_files(values$path,ext=input$ext, subfolder=input$include_subfolder)
               values$menu <- 0
               values$custom_lists <- NULL
               values$txt <- txt
             }
           },
           load_tall={
             file_tall <- input$file1
              load(file_tall$datapath)
              values$menu <- menu
              values$dfTag <- dfTag
              values$custom_lists <- custom_lists
              if (values$menu==1) updateTabItems(session, "sidebarmenu", "custTermList")
              if (values$menu==2) updateTabItems(session, "sidebarmenu", "posTagSelect")
           }
    )
  })

  output$dataImported <- DT::renderDT({
    DATAloading()
    if (values$menu==0){
      DTformat(values$txt, nrow=5)
    }
  })
### shortpath for folder path ----
  output$folder <-  renderUI({
    path <- shortpath(values$path)
    if (is.null(path)) path <- " --- "
    HTML(paste("<pre class='tab'>",path, sep = ''))
  })

  ### Convert Raw Data in Excel functions ----
  output$collection.save <- downloadHandler(
    filename = function() {
      paste("Tall-Export-File-", Sys.Date(), ".xlsx", sep="")
    },
    content <- function(file) {
      suppressWarnings(openxlsx::write.xlsx(values$txt, file=file))
    }, contentType = "xlsx"
  )


  ### PRE-PROCESSING ----

  ## Tokenization & PoS Tagging ----

  output$optionsTokenization <- renderUI({
      selectInput(
        inputId = 'language_model', label="Select language", choices = values$label_lang,
        multiple=FALSE,
        width = "100%"
    )
  })

    posTagging <- eventReactive({
      input$tokPosRun
    },{
        ## download and load model language
        udmodel_lang <- loadLanguageModel(language = input$language_model)

        ## set cores for parallel computing
        ncores <- max(1,parallel::detectCores()-1)

        ## set cores for windows machines
        if (Sys.info()[["sysname"]]=="Windows") {
          cl <- parallel::makeCluster(ncores)
          registerDoParallel(cl)
        }

        #Lemmatization and POS Tagging
        values$dfTag <- udpipe(object=udmodel_lang, x = values$txt , parallel.cores=ncores)

        # Merge metadata from the original txt object
        values$dfTag <- values$dfTag %>%
          left_join(values$txt %>% select(-text), by = "doc_id") %>%
          posSel(., c("ADJ","NOUN","PROPN", "VERB"))
          #mutate(POSSelected = ifelse(upos %in% c("ADJ","NOUN","PROPN", "VERB"), TRUE, FALSE))

        values$menu <- 1

    }
    )

    output$tokPosTagData<- DT::renderDT({
      posTagging()

      if(!is.null(values$dfTag)){
      DTformat(values$dfTag%>% select(doc_id, sentence_id,sentence,token,lemma, upos) %>%
                 rename(D_id=doc_id,
                        S_id=sentence_id,
                        Sentence=sentence,
                        Token=token,
                        Lemma=lemma,
                        POSTag=upos)
      )
      }
    })

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
        print(values$custom_lists)
      }
    )

    customListMerging <- eventReactive({
      input$custTermListRun
    },{
      if (!is.null(values$custom_lists)){
        values$dfTag <- mergeCustomLists(values$dfTag,values$custom_lists)
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
                          POSTag=upos)
        )
      }
    })

    output$customListData<- DT::renderDT({
      customListMerging()

      if (is.null(values$custom_lists)){
        DTformat(data.frame(Lemma=NULL,POSTag=NULL), n=1)
      } else {
        DTformat(values$custom_lists %>% rename(
          Lemma = lemma,
          POSTag = upos))
      }
    })

    ## Multi-Word Creation ----

    output$multiwordPosSel <- renderUI({
      checkboxGroupInput("multiwordPosSel", label="Multi-Words created by:",
                         choices = posTagAll(values$dfTag %>% dplyr::filter(!upos %in% c("MULTIWORD", "NGRAM_MERGED", "PUNCT", "SYM", "X", "NUM")))$description,
                         selected = posTagAll(values$dfTag %>% dplyr::filter(upos %in%  values$posMwSel))$description

      )
    })

    multiword <- eventReactive({
      input$multiwordCreatRun
    },{

      ### REKA Algorithm ####

      # to replace with input values
      term = input$term
      group = input$group
      ngram_max = input$ngram_max
      rake.min <- input$rake.min

      values$posMwSel <- gsub(":","",gsub(":.*","",input$multiwordPosSel))

      obj <- rake(values$dfTag, group = group, ngram_max=ngram_max, relevant = values$posMwSel, rake.min=rake.min)

      values$dfTag <- obj$dfTag
      values$multiwords <- obj$multiwords
    })

    output$multiwordData <- renderDT({
      multiword()
      DTformat(values$dfTag %>% dplyr::filter(POSSelected) %>%
                 group_by(doc_id,sentence_id) %>%
                 select(doc_id, sentence_id,sentence,token,lemma, upos) %>%
                 rename(D_id=doc_id,
                        S_id=sentence_id,
                        Sentence=sentence,
                        Token=token,
                        Lemma=lemma,
                        POSTag=upos)
      )
    })

    output$multiwordList <- renderDT({
      multiword()
      DTformat(values$multiwords)
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
      #selected <- (posTagAll(values$dfTag) %>% dplyr::filter(selected==TRUE))$pos
      selected <- (posTagAll(values$dfTag) %>% dplyr::filter(description %in% (input$posTagLists)))$pos
      values$dfTag <- posSel(values$dfTag, pos=selected)#ifelse(values$dfTag$upos %in% selected, TRUE, FALSE)
      values$menu <- 2
    })

    output$posTagSelectData <- renderDT({
      PosFilterData()
      DTformat(values$dfTag %>% dplyr::filter(POSSelected) %>%
                 group_by(doc_id,sentence_id) %>%
                 mutate(SentenceByPos = paste(lemma, collapse=" ")) %>%
                 select(doc_id, sentence_id,sentence,SentenceByPos,token,lemma, upos) %>%
                 rename(D_id=doc_id,
                        S_id=sentence_id,
                        Sentence=sentence,
                        Token=token,
                        Lemma=lemma,
                        POSTag=upos)
      )
    })




  ## OVERVIEW ----

    ## BOX ----


    #### box1 ---------------
    output$nDoc <- renderValueBox({
      values$vb <- valueBoxesIndices(values$dfTag)
      valueBox(value = p("Documents", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$nDoc), style = 'font-size:36px;color:white;', align="center"),
               icon = fa_i(name="layer-group"), color = "olive",
               width = NULL)
    })

    #### box2 ---------------
    output$avgDocLengthChar <- renderValueBox({
      valueBox(value = p("Doc Avg Length in Chars", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$avgDocLengthChars), style = 'font-size:36px;color:white;', align="center"),
               icon = fa_i(name="layer-group"), color = "olive",
               width = NULL)
    })

    #### box3 ------------
    output$avgDocLengthTokens <- renderValueBox({
      valueBox(value = p("Doc Avg Length in Tokens", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$avgDocLengthTokens), style = 'font-size:36px;color:white;', align="center"),
               icon = fa_i(name="layer-group"), color = "olive",
               width = NULL)
    })

    #### box4 ---------------
    output$nSentences <- renderValueBox({
      valueBox(value = p("Sentences", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$nSentences), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="align-left", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box5 --------------------
    output$avgSentLengthChar <- renderValueBox({
      valueBox(value = p("Sent Avg Length in Chars", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$avgSentLengthChars), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="align-left", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box6 -------------
    output$avgSentLengthTokens <- renderValueBox({
      valueBox(value = p("Sent Avg Length in Tokens", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$avgSentLengthTokens), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="align-left", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box7 ----------------
    output$nDictionary <- renderValueBox({
      valueBox(value = p("Dictionary", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$nDictionary), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="font", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box8 ---------------
    output$nTokens <- renderValueBox({
      valueBox(value = p("Tokens", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$nTokens), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="font", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box9 ---------------
    output$nLemmas <- renderValueBox({
      valueBox(value = p("Lemmas", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$nLemmas), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="font", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box10 ------------------
    output$TTR <- renderValueBox({
      valueBox(value = p("TTR", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$TTR), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="stats", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box11 ------
    output$hapax <- renderValueBox({
      valueBox(value = p("Hapax (%)", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$hapax), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="stats", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box12 -------
    output$guiraud <- renderValueBox({
      valueBox(value = p("Guiraud Index", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$guiraud), style = 'font-size:36px;color:white;', align="center"),
               icon = icon(name="stats", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    ## Overview Table ----

    output$overviewData <- renderDT(server = FALSE,{
      vb <- data.frame(Description=c("Documents", "Tokens", "Dictionary", "Lemmas", "Sentences",
                                     "Docs Avg Length in Chars", "Doc Avg Length in Tokens",
                                     "Sent Avg Length in Tokens", "Sent Avg Length in Chars",
                                     "TTR", "Hapax (%)", "Guiraud Index"),
                       Values=unlist(values$vb))
      DTformat(vb,n=12, left=1, right=2, numeric=2, pagelength=FALSE, dom=FALSE, size='110%', filename="Overview")
    })



    observeEvent(input$reportMI,{
      if(!is.null(values$TABvb)){
        sheetname <- "MainInfo"
        list_df <- list(values$TABvb)
        res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
        values$wb <- res$wb
        popUp(title="Main Information", type="success")
        values$myChoices <- sheets(values$wb)
      } else {
        popUp(type="error")
      }
    })


  ### WORDS ----

  ## Frequency List ----

  # wordcloud <- eventReactive(input$overviewApply,{
  #   # INPUT DA AGGIUNGERE
  #   # n <-  input$wcN numer of words
  #   # size <-  input$wcSize
  #   # scale <- input$wcScale scale transformation (log, etc.)
  #   n <- 100
  #   size <- 2
  #   scale <- "identity"
  #
  #   values$wcDf <- distrib(values$token, scale=scale) %>%
  #     slice_head(n=n) %>%
  #     rename(word = text,
  #            freq = n)
  #   wordcloud2(values$wcDf,
  #              size = size,
  #              color = "random-light",
  #              backgroundColor = "transparent")
  # })
  #
  # output$overviewPlot <- renderWordcloud2({
  #   wordcloud()
  # })
  #
  # output$overviewTable <- renderDT({
  #   wordcloud()
  #   values$wcDf
  # })

  ## Clustering ----

  ## Correspondence Analysis ----

  ## Network ----


  ## DOCUMENTS ----

  ## Topic Modeling ----

  ## Clustering ----

  ## Network ----

  ## Summarization ----

  ## Polarity detection ----


  ## GROUPS ----

  ## Topic Modeling ----

  ## Clustering ----

  ## Network ----

  ## Summarization ----

  ## Polarity detection ----



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



} # END SERVER

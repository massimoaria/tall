##  Server ####
source("tallFunctions.R", local=TRUE)

### da rimuovere alla fine
library(tall)
###

## suppress warnings
options(warn = -1)

## file upload max size
maxUploadSize <- 200 # default value
maxUploadSize <- getShinyOption("maxUploadSize", maxUploadSize)
options(shiny.maxRequestSize=maxUploadSize*1024^2)


server <- function(input, output, session){
  session$onSessionEnded(stopApp)

  ## suppress summarise message
  options(dplyr.summarise.inform = FALSE)

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
  values$TMplotIndex <- 1

  ## Setting plot values
  values$h <- 7
  dpi <- 300



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

  output$file_raw <- renderUI({
    fileInput(
      "file_raw",
      "Select file(s) containing text",
      multiple = TRUE,
      accept = c(input$ext)
    )
  })


  ### dataImported ----
  DATAloading<- eventReactive(input$runImport,{
    switch(input$load,
           import={
             if (!is.null(req(input$file_raw))){
               file <- input$file_raw
               txt <- read_files(file,ext=input$ext, subfolder=FALSE, line_sep=input$line_sep=="yes")
               values$menu <- 0
               values$custom_lists <- NULL
               values$txt <- txt %>% arrange(doc_id)
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
              values$D <- D
              values$where <- where
              if (values$menu==1) updateTabItems(session, "sidebarmenu", "custTermList")
              if (values$menu==2) updateTabItems(session, "sidebarmenu", "posTagSelect")
              if (ncol(values$dfTag)>1){showModal(loadTallgModal(session))}
           }
    )
  })

  output$dataImported <- DT::renderDT({
    DATAloading()
    if (values$menu==0){
      DTformat(values$txt %>% mutate(text = paste0(substr(text,1,500),"...")),left=2, nrow=5, filter="none")
    }
  })
### shortpath for folder path ----
  output$folder <-  renderUI({
    path <- shortpath(values$path)
    if (is.null(path)) path <- " --- "
    HTML(paste("<pre class='tab'>",path, sep = ''))
  })

  ### Pop-up modal for Tall file loading ----
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
    if(!is.null(dim(values$custom_lists))){
      ncust <- nrow(values$custom_lists)
      txt3 <- (paste0("Tall file includes a custom list of: ",strong(ncust), strong(" words")))
    } else{
      txt3 <- (paste0("Tall file does not include a custom word list"))
    }
    txt4 <- (paste0("The last pre-processing step performed is: ",strong(values$where)))
    text <- paste0(txt1,"<br><br>",txt2,"<br><br>",txt3,"<br><br>",txt4)
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
        values$dfTag <- highlight(values$dfTag)
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
                        "Part of Speech"=upos)
      )
      }
    })

    output$tokPosSave <- downloadHandler(
      filename = function() {
        paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
      },
      content <- function(file) {
        saveTall(values$dfTag, values$custom_lists, values$menu, "Custom Term Lists", file)
        # D <- date()
        # save(dfTag,menu,D,where, file=file)
      }, contentType = "tall"
    )

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

    output$customListData<- DT::renderDT({
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
        saveTall(values$dfTag, values$custom_lists, values$menu, "Custom Term Lists", file)
        # D <- date()
        # save(dfTag,menu,D,where, file=file)
      }, contentType = "tall"
    )

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
      values$dfTag <- highlight(values$dfTag)
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
                        "Part of Speech"=upos)
      )
    })

    output$multiwordList <- renderDT({
      multiword()
      DTformat(values$multiwords %>%  rename("Multi-Words" = keyword,
                                             "Lenght" = ngram, "Freq"=freq,
                                             "Rake value" = rake),
               numeric=4)
    })

    output$multiwordCreatSave <- downloadHandler(
      filename = function() {
        paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
      },
      content <- function(file) {
        saveTall(values$dfTag, values$custom_lists, values$menu, "Multi-Word Creation", file)
        # D <- date()
        # save(dfTag,menu,D,where, file=file)
      }, contentType = "tall"
    )


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
      values$dfTag <- posSel(values$dfTag, pos=selected)
      values$menu <- 2
    })

    output$posTagSelectData <- renderDT({
      PosFilterData()
      if(!"lemma_original_nomultiwords" %in% names(values$dfTag)) values$dfTag <- values$dfTag %>% mutate(lemma_original_nomultiwords=lemma)
      DTformat(values$dfTag %>% dplyr::filter(POSSelected) %>%
                 group_by(doc_id,sentence_id) %>%
                 mutate(SentenceByPos = paste(lemma_original_nomultiwords, collapse=" ")) %>%
                 select(doc_id, sentence_id,sentence,SentenceByPos,token,lemma, upos) %>%
                 rename(D_id=doc_id,
                        S_id=sentence_id,
                        Sentence=sentence,
                        Token=token,
                        Lemma=lemma,
                        "Part of Speech"=upos)
      )
    })

    output$posTagSelectSave <- downloadHandler(
      filename = function() {
        paste("Tall_Export_File_", Sys.Date(),".tall", sep="")
      },
      content <- function(file) {
        saveTall(values$dfTag, values$custom_lists, values$menu, "POS Tag Selection", file)
        # D <- date()
        # save(dfTag,menu,D,where, file=file)
      }, contentType = "tall"
    )




  ## OVERVIEW ----

    ## BOX ----


    #### box1 ---------------
    output$nDoc <- renderValueBox({
      values$vb <- valueBoxesIndices(values$dfTag)
      valueBox(value = p("Documents", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$nDoc), style = 'font-size:36px;color:white;', align="center"),
               icon = icon("duplicate", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box2 ---------------
    output$avgDocLengthChar <- renderValueBox({
      valueBox(value = p("Doc Avg Length in Chars", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$avgDocLengthChars), style = 'font-size:36px;color:white;', align="center"),
               icon = icon("duplicate", lib="glyphicon"), color = "olive",
               width = NULL)
    })

    #### box3 ------------
    output$avgDocLengthTokens <- renderValueBox({
      valueBox(value = p("Doc Avg Length in Tokens", style = 'font-size:16px;color:white;'),
               subtitle = p(strong(values$vb$avgDocLengthTokens), style = 'font-size:36px;color:white;', align="center"),
               icon = icon("duplicate", lib="glyphicon"), color = "olive",
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


    ## WORDCLOUD ----

    output$wordcloudPlot <- renderWordcloud2({

      n <- 200 #showing the first 200 lemmas
      wcDfPlot <- freqByPos(values$dfTag, term="lemma",pos=unique(values$dfTag$upos[values$dfTag$POSSelected==TRUE])) %>%
        slice_head(n=n) %>%
        #mutate(n=log(n)) %>%
        rename(text = term,
               freq = n)

      wcPlot <- wordcloud2::wordcloud2(wcDfPlot,
                                       fontFamily = "Impact", fontWeight = "normal", minSize=0,
                                       minRotation = 0, maxRotation = 0, shuffle = TRUE,
                                       rotateRatio = 0.7, shape = "circle",ellipticity = 0.65,
                                       widgetsize = NULL,
                                       figPath = NULL,
                                       size = ifelse(length(wcDfPlot$text)>100,1.5,1.8),
                                       color = "random-dark", backgroundColor = "transparent")
      return(wcPlot)
    })


    output$wcDfData <- renderDT({
      n=200
      wcDfTable <- freqByPos(values$dfTag, term="lemma",pos=unique(values$dfTag$upos[values$dfTag$POSSelected==TRUE])) %>%
        slice_head(n=n) %>%
        rename(Lemma = term,
               Freq = n)

      DTformat(wcDfTable,n=15, left=1, right=2, numeric=2, pagelength=TRUE, dom=TRUE, size='110%', filename="WordCloudData")

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

    ## DICTIONARY ----
    dictionary <- eventReactive({
      input$dictionaryApply
    },
    {
    #values$dictFreq <- freqByPos(values$dfTag, term=input$termDict, pos=c("PROPN", "NOUN", "ADJ", "VERB"))
    Term <- input$termDict
    values$dictFreq <- values$dfTag %>%
      dplyr::filter(POSSelected) %>%
      mutate(token = ifelse(upos == "MULTIWORD", lemma,token))

    if (Term=="lemma"){
      values$dictFreq <- values$dictFreq %>%
        group_by(upos, lemma) %>%
        summarize(n=n()) %>%
        arrange(desc(n)) %>%
        rename("Part of Speech"=upos,
               Lemma = lemma,
               Frequency = n)
    } else {
      values$dictFreq <- values$dictFreq %>%
        group_by(upos, token) %>%
        summarize(n=n()) %>%
        arrange(desc(n)) %>%
        rename("Part of Speech"=upos,
               Token = token,
               Frequency = n)
    }
    })


    output$dictionaryData <- renderDT({
      dictionary()
      DTformat(values$dictFreq,
               left=c(1,2), nrow=15, pagelength=TRUE, filename="Dictionary", dom=TRUE, size="110%")
    })

    ## TF-IDF ----
    output$tfidfData <- renderDT({
      DTformat(values$dfTag %>%
        dplyr::filter(POSSelected) %>%
        tfidf(term="lemma") %>%
          rename(
            Lemma = term,
            "TF-IDF" = TFIDF),
        left=1, numeric=2,round=4, size="110%"
      )
    })



  ### WORDS ----

    ## Click on Plotly graphs: WORDS IN CONTEXT ----
    observeEvent(event_data("plotly_click"), {
      #if (input$sidebarmenu=="freqList"){
      if (input$sidebarmenu %in% c("w_noun","w_propn", "w_adj", "w_verb", "w_other", "ca")){
        showModal(plotModalTerm(session))
      }
    })

    plotModalTerm <- function(session) {
      ns <- session$ns
      modalDialog(
        h3(strong(("Words in Context"))),
        DTOutput(ns("wordInContext")),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          # screenshotButton(label="Save", id = "cocPlotClust",
          #                  scale = 2,
          #                  file=paste("TMClusterGraph-", Sys.Date(), ".png", sep="")),
          modalButton("Close")),
      )
    }

    output$wordInContext <- renderDT({
      values$d <- event_data("plotly_click")
      word <- values$d$y
      if (input$sidebarmenu=="w_other"){
        word_search <- word
        sentences <- values$dfTag %>%
          filter(lemma %in% word_search) %>%
          ungroup() %>% select(lemma, token, sentence_hl)
      } else if (input$sidebarmenu=="ca"){
        X <- round(values$d$x,6)
        Y <- round(values$d$y,6)
        word <- values$CA$wordCoord %>%
          dplyr::filter(round(.[,1],6)==X,round(.[,2],6)==Y)
        word <- word$label
        word_search <- unique(c(word, values$dfTag$token[values$dfTag$lemma==word]))
        sentences <- values$dfTag %>%
          filter(token %in% word_search) %>%
          ungroup() %>% select(lemma, token, sentence_hl)
        } else {
        word_search <- unique(c(word, values$dfTag$token[values$dfTag$lemma==word]))
        sentences <- values$dfTag %>%
          filter(token %in% word_search) %>%
          ungroup() %>% select(lemma, token, sentence_hl)
      }
      # find sentences containing the tokens/lemmas
      DTformat(sentences, size='100%')
    }, escape=FALSE)

  ## Frequency List ----

    ## NOUN ----

    nounFreq <- eventReactive(
      eventExpr = {
        input$nounApply
        },
      valueExpr = {
        values$freqNoun <- freqByPos(values$dfTag, term="lemma", pos="NOUN")
          freqPlotly(values$freqNoun,x="n",y="term",n=input$nounN, xlabel="Frequency",ylabel="NOUN", scale="identity")
      }
    )

    output$nounPlot <- renderPlotly({
      nounFreq()
    })

    output$nounTable <- renderDT({
      nounFreq()
      DTformat(values$freqNoun %>%
                 rename(Term = term,
                        Frequency = n),
               left=1, right=2, numeric=2, filename="NounFreqList", dom=FALSE, size="110%")
    })

    output$nounExport <- downloadHandler(
      filename = function() {

        paste("NounFrequency-", Sys.Date(), ".png", sep="")
      },
      content <- function(file) {
        values$nounGgplot <- freqGgplot(values$freqNoun,x=2, y=1,n=input$nounN,
                                        title = "Noun Frequency")
        ggsave(filename = file, plot = values$nounGgplot, dpi = dpi, height = values$h, width = values$h*2, bg="white")
      },
      contentType = "png"
    )


    ## PROPN ----
    propnFreq <- eventReactive(
      eventExpr = {
        input$propnApply
      },
      valueExpr = {
        values$freqPropn <- freqByPos(values$dfTag, term="lemma", pos="PROPN")
        freqPlotly(values$freqPropn,x="n",y="term",n=input$propnN, xlabel="Frequency",ylabel="PROPN", scale="identity")

      }
    )

    output$propnPlot <- renderPlotly({
      propnFreq()
    })

    output$propnTable <- renderDT({
      propnFreq()
      DTformat(values$freqPropn %>%
                 rename(Term = term,
                        Frequency = n),
               left=1, right=2, numeric=2, filename="PropnFreqList", dom=FALSE, size="110%")
    })

    output$propnExport <- downloadHandler(
      filename = function() {

        paste("ProperNounFrequency-", Sys.Date(), ".png", sep="")
      },
      content <- function(file) {
        values$propnGgplot <- freqGgplot(values$freqPropn,x=2, y=1,n=input$propnN,
                                        title = "Proper Noun Frequency")
        ggsave(filename = file, plot = values$propnGgplot, dpi = dpi, height = values$h, width = values$h*2, bg="white")
      },
      contentType = "png"
    )

    ## ADJ ----
    adjFreq <- eventReactive(
      eventExpr = {
        input$adjApply
      },
      valueExpr = {
        values$freqAdj <- freqByPos(values$dfTag, term="lemma", pos="ADJ")
        freqPlotly(values$freqAdj,x="n",y="term",n=input$adjN, xlabel="Frequency",ylabel="ADJ", scale="identity")
      }
    )

    output$adjPlot <- renderPlotly({
      adjFreq()
    })

    output$adjTable <- renderDT({
      adjFreq()
      DTformat(values$freqAdj %>%
                 rename(Term = term,
                        Frequency = n),
               left=1, right=2, numeric=2, filename="AdjFreqList", dom=FALSE, size="110%")
    })

    output$adjExport <- downloadHandler(
      filename = function() {

        paste("AdjectiveFrequency-", Sys.Date(), ".png", sep="")
      },
      content <- function(file) {
        values$adjGgplot <- freqGgplot(values$freqAdj,x=2, y=1,n=input$adjN,
                                         title = "Adjective Frequency")
        ggsave(filename = file, plot = values$adjGgplot, dpi = dpi, height = values$h, width = values$h*2, bg="white")
      },
      contentType = "png"
    )

    ## VERB ----
    verbFreq <- eventReactive(
      eventExpr = {
        input$verbApply
      },
      valueExpr = {
        values$freqVerb <- freqByPos(values$dfTag, term="lemma", pos="VERB")
        freqPlotly(values$freqVerb,x="n",y="term",n=input$verbN, xlabel="Frequency",ylabel="VERB", scale="identity")
      }
    )

    output$verbPlot <- renderPlotly({
      verbFreq()
    })

    output$verbTable <- renderDT({
      verbFreq()
      DTformat(values$freqVerb %>%
                 rename(Term = term,
                        Frequency = n),
               left=1, right=2, numeric=2, filename="VerbFreqList", dom=FALSE, size="110%")
    })

    output$verbExport <- downloadHandler(
      filename = function() {

        paste("VerbFrequency-", Sys.Date(), ".png", sep="")
      },
      content <- function(file) {
        values$verbGgplot <- freqGgplot(values$freqVerb,x=2, y=1,n=input$verbN,
                                       title = "Verb Frequency")
        ggsave(filename = file, plot = values$verbGgplot, dpi = dpi, height = values$h, width = values$h*2, bg="white")
      },
      contentType = "png"
    )

    ## OTHER ----

    otherFreq <- eventReactive(
      eventExpr = {
        input$otherApply
      },
      valueExpr = {
        values$freqOther <- freqByPos(values$dfTag, term="lemma", pos="MULTIWORD")
        freqPlotly(values$freqOther,x="n",y="term",n=input$otherN, xlabel="Frequency",ylabel="Multi-Words", scale="identity")
      }
    )

    output$otherPlot <- renderPlotly({
      otherFreq()
    })

    output$otherTable <- renderDT({
      otherFreq()
      DTformat(values$freqOther %>%
                 rename(Term = term,
                        Frequency = n),
               left=1, right=2, numeric=2, filename="MultiWordFreqList", dom=FALSE, size="110%")
    })

    output$otherExport <- downloadHandler(
      filename = function() {

        paste("MultiWordFrequency-", Sys.Date(), ".png", sep="")
      },
      content <- function(file) {
        values$otherGgplot <- freqGgplot(values$freqOther,x=2, y=1,n=input$otherN,
                                       title = "Multi-Word Frequency")
        ggsave(filename = file, plot = values$otherGgplot, dpi = dpi, height = values$h, width = values$h*2, bg="white")
      },
      contentType = "png"
    )

    ## PART OF SPEECH ----

    posFreq <- eventReactive(
      eventExpr = {
        input$posApply
      },
      valueExpr = {
        values$freqPOS <- values$dfTag %>%
          dplyr::filter(!upos %in% c("PUNCT", "SYM", "NUM")) %>%
          group_by(upos) %>%
          count() %>%
          arrange(desc(n)) %>%
          rename(PoS = upos)

        freqPlotly(values$freqPOS,x="n",y="PoS",n=nrow(values$freqPOS), xlabel="Frequency",ylabel="Part of Speech", scale="identity")
      }
    )

    output$posPlot <- renderPlotly({
      posFreq()
    })

    output$posTable <- renderDT({
      posFreq()
      DTformat(values$freqPOS %>%
                 rename(Frequency = n),
               left=1, right=2, numeric=2, filename="POSFreqList", dom=FALSE, pagelength=FALSE, size="110%")
    })

    output$posExport <- downloadHandler(
      filename = function() {

        paste("PoSFrequency-", Sys.Date(), ".png", sep="")
      },
      content <- function(file) {
        values$posGgplot <- freqGgplot(data.frame(values$freqPOS),x=2, y=1,n=length(values$freqPOS$PoS),
                                         title = "PoS Frequency")
        ggsave(filename = file, plot = values$posGgplot, dpi = dpi, height = values$h, width = values$h*2, bg="white")
      },
      contentType = "png"
    )

    ## Words in Context ----

    wordsInContextMenu <- eventReactive(
      ignoreNULL = TRUE,
      eventExpr = {input$wordsContSearch},
      valueExpr = {
        word_search <- req(tolower(input$wordsContSearch))
        values$wordInContext <- values$dfTag %>%
          filter(tolower(lemma) %in% word_search | tolower(token) %in% word_search) %>%
          ungroup() %>% select(lemma, token, sentence_hl) %>%
          rename(Lemma=lemma, Token=token, Sentence=sentence_hl)
      })

    output$wordsContData <- renderDT({
      wordsInContextMenu()
      DTformat(values$wordInContext, size='100%')
    }, escape=FALSE)


  ## Clustering ----

    ## Dendrogramm ----
    dendFunction <- eventReactive(
      ignoreNULL = TRUE,
      eventExpr = {input$w_clusteringApply},
      valueExpr ={
        results <- clustering(values$dfTag, n=input$w_clusteringNMax,
                   group="doc_id", minEdges=25, term="lemma",
                   normalization=input$w_clusteringSimilarity)
        values$wordCluster <- results$cluster
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

    output$w_clusteringTable <- renderDT({
      dendFunction()
      DTformat(values$wordCluster, size='100%',filename="ClusterWordsTable", pagelength=TRUE, left=1, right=NULL,
               numeric=NULL, dom=TRUE, filter="top")
    })


  ## Correspondence Analysis ----

    # CA plot
    caPlotFunction <- eventReactive(
      ignoreNULL = TRUE,
      eventExpr = {input$caApply},
      valueExpr ={
        values$CA <- wordCA(values$dfTag, n=input$nCA, term=input$termCA)
        values$CA <- caClustering(values$CA, nclusters = input$nClustersCA, nDim=input$nDimsCA)
        values$CAdimY <- as.numeric(input$dimPlotCA)*2
        values$CAdimX <- values$CAdimY-1
        values$plotCA <- ca2plotly(values$CA, dimX = values$CAdimX, dimY = values$CAdimY,
                                   topWordPlot = input$nCA, threshold=0.03, labelsize = input$labelsizeCA, size=input$sizeCA)
        values$CADendrogram <- dend2vis(values$CA$clustering$h, labelsize=input$labelsizeCA, nclusters=input$nClustersCA, community=FALSE)
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
    output$caCoordTable <- renderDT({
      caPlotFunction()
      DTformat(values$CA$wordCoord %>%
                 select(label, everything()) %>%
                 left_join(
                   data.frame(label=names(values$CA$clustering$groups), Group=values$CA$clustering$groups), by = "label"
                 ) %>%
                 rename(Label = label)
               ,size='100%',filename="CAWordCoordinatesTable", pagelength=TRUE, left=1, right=2:ncol(values$CA$wordCoord),
               numeric=2:ncol(values$CA$wordCoord), dom=TRUE, filter="top", round=3)
    })

    output$caContribTable <- renderDT({
      caPlotFunction()
      DTformat(values$CA$contrib %>%
                 rownames_to_column() %>%
                 rename(Label = rowname),
               size='100%',filename="CAWordContributesTable", pagelength=TRUE, left=1, #right=2:(ncol(values$CA$contrib)+1),
               numeric=2:(ncol(values$CA$contrib)+1), dom=TRUE, filter="top", round=3)
    })

    output$caCosineTable <- renderDT({
      caPlotFunction()
      DTformat(values$CA$cosine %>%
                 rownames_to_column() %>%
                 rename(Label = rowname),
               size='100%',filename="CAWordCosinesTable", pagelength=TRUE, left=1, #right=2:(ncol(values$CA$cosine)+1),
               numeric=2:(ncol(values$CA$cosine)+1), dom=TRUE, filter="top", round=3)
    })

    output$caSingularValueTable <- renderDT({
      caPlotFunction()
      df <- data.frame(dim=paste0("Dim ",1:10),sv=(values$CA$ca$sv/sum(values$CA$ca$sv)*100)[1:10], svcorr=values$CA$ca$eigCorrectedNorm[1:10])
      DTformat(df %>%
                 rename("Factorial Dimension" = dim,
                   "Singular Values" = sv,
                        "Corrected Singular Values" = svcorr),
               size='100%',filename="CAWordSingualValueTable", pagelength=TRUE, left=1, #right=2:3,
               numeric=2:3, dom=TRUE, filter="top", round=2)
    })

  ## Network ----
    ## WORD CO-OCCURENCE ----
    netFunction <- eventReactive(
      ignoreNULL = TRUE,
      eventExpr = {input$w_networkCoocApply},
      valueExpr ={
        values$network <- network(values$dfTag, group=input$groupNet, n=input$nMax, minEdges=input$minEdges,
                                  labelsize=input$labelSize, opacity=input$opacity,
                                  interLinks=input$interLinks, normalization=input$normalizationCooc)
      }
    )

    output$w_networkCoocPlot <- renderVisNetwork({
      netFunction()
      net2vis(nodes=values$network$nodes, edges=values$network$edges)
    })

    output$w_networkCoocNodesTable <- renderDT({
      netFunction()
      DTformat(values$network$nodes %>%
                 select(upos, label, value, group, color) %>%
                 rename(PoS=upos,
                        Word=label,
                        Frequency=value,
                        Group=group,
                        "Color Group"=color), size='100%',filename="NetworkWordsTable", pagelength=TRUE, left=NULL, right=NULL,
               numeric=NULL, dom=TRUE, filter="top")
    })

    output$w_networkCoocEdgesTable <- renderDT({
      netFunction()
      DTformat(values$network$edges %>%
                 select(term_from, term_to,group_from, group_to, s,sA, sC, sJ) %>%
                 rename(From=term_from,
                        To=term_to,
                        "Co-occurence"=s,
                        "Association Index"=sA,
                        "Cosine Similarity"=sC,
                        "Jaccard Index"=sJ,
                        "Group From"=group_from,
                        "Group To"=group_to),
               size='100%',filename="NetworkLinksTable", numeric=6:8, round=4)
    })


    ## Click on visNetwork: WORDS IN CONTEXT ----
    observeEvent(ignoreNULL = TRUE,
                 eventExpr={input$click},
                 handlerExpr = {
                   showModal(plotModalTermNet(session))
                 })

    plotModalTermNet <- function(session) {
      ns <- session$ns
      modalDialog(
        h3(strong(("Words in Context"))),
        DTOutput(ns("wordInContextNet")),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          # screenshotButton(label="Save", id = "cocPlotClust",
          #                  scale = 2,
          #                  file=paste("TMClusterGraph-", Sys.Date(), ".png", sep="")),
          modalButton("Close")),
      )
    }

    output$wordInContextNet <- renderDT({
      id <- input$click
      if (input$sidebarmenu=="w_networkGrako") {
        word_search<- values$grako$nodes$title[values$grako$nodes$id==id]
        sentences <- values$grako$multiwords %>%
          filter(lemma %in% word_search) %>%
          ungroup() %>% select(lemma, token, sentence_hl)
      } else {
        word_search<- values$network$nodes$label[values$network$nodes$id==id]
        sentences <- values$dfTag %>%
          filter(lemma %in% word_search) %>%
          ungroup() %>% select(lemma, token, sentence_hl)
      }

      # find sentences containing the tokens/lemmas
      DTformat(sentences, size='100%')
    }, escape=FALSE)


    ## Click on Dendrogram: WORDS IN CONTEXT ----
    observeEvent(ignoreNULL = TRUE,
                 eventExpr={input$click_dend},
                 handlerExpr = {
                   showModal(plotModalTermDend(session))
                 })

    plotModalTermDend <- function(session) {
      ns <- session$ns
      modalDialog(
        h3(strong(("Words in Context"))),
        DTOutput(ns("wordInContextDend")),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          # screenshotButton(label="Save", id = "cocPlotClust",
          #                  scale = 2,
          #                  file=paste("TMClusterGraph-", Sys.Date(), ".png", sep="")),
          modalButton("Close")),
      )
    }

    output$wordInContextDend <- renderDT({
      id <- unlist(input$click_dend)
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
        filter(lemma %in% word_search) %>%
        ungroup() %>% select(lemma, token, sentence_hl)

      # find sentences containing the tokens/lemmas
      DTformat(sentences, size='100%')
    }, escape=FALSE)

    ## GRAKO ----
    grakoFunction <- eventReactive(
      ignoreNULL = TRUE,
      eventExpr = {input$w_networkGrakoApply},
      valueExpr ={
        values$grako <- grako(values$dfTag, n=input$grakoNMax, minEdges=input$grakoMinEdges,
                                  labelsize=input$grakoLabelSize, opacity=input$grakoOpacity,term="lemma",
                                  normalization=input$grakoNormalization, singleWords=input$grakoUnigram)
      }
    )

    output$w_networkGrakoPlot <- renderVisNetwork({
      grakoFunction()
      grako2vis(nodes=values$grako$nodes, edges=values$grako$edges)
    })

    output$w_networkGrakoNodesTable <- renderDT({
      grakoFunction()
      DTformat(values$grako$nodes %>%
                 select(upos, label, value) %>%
                 rename(PoS=upos,
                        Word=label,
                        Frequency=value), size='100%',filename="GrakoWordsTable", pagelength=TRUE, left=NULL, right=NULL,
               numeric=NULL, dom=TRUE, filter="top")
    })

    output$w_networkGrakoEdgesTable <- renderDT({
      grakoFunction()
      DTformat(values$grako$edges %>%
                 select(term_from, term_to,upos_from, upos_to, role, s,sA, sC, sJ) %>%
                 rename(From=term_from,
                        To=term_to,
                        "Co-occurence"=s,
                        "Association Index"=sA,
                        "Cosine Similarity"=sC,
                        "Jaccard Index"=sJ,
                        "PoS From"=upos_from,
                        "PoS To"=upos_to,
                        "Action"=role),
               size='100%',filename="GrakoLinksTable", numeric=7:9, round=4)
    })


  ## DOCUMENTS ----

  ## Topic Modeling ----

    ## K choice ----

    netTMKselect <- eventReactive(
      ignoreNULL = TRUE,
      eventExpr = {input$d_tm_selectApply},
      valueExpr ={
        values$TMKresult <- tmTuning(values$dfTag, group=input$groupTm, term=input$termTm,
                                     metric=input$metric, n=input$nTm, top_by=input$top_by, minK=input$minK, maxK=input$maxK, Kby=input$Kby)
        values$TMKplot <- tmTuningPlot(values$TMKresult, metric=input$metric)
      }
    )

    output$d_tm_selectPlot <- renderPlotly({
      netTMKselect()
      values$TMKplot
    })

    output$d_tm_selectTable <- renderDataTable({
      df <- values$TMKresult %>% arrange(topics) %>%
        rename("N. of Topics" = topics)
      df$Normalized <- (df[,2]-min(df[,2]))/diff(range(df[,2]))

      DTformat(df, numeric=c(2,3), round=2, nrow=nrow(df), size="110%")
    })


    ## Model estimation ----
    netTMestim <- eventReactive(
      ignoreNULL = TRUE,
      eventExpr = {input$d_tm_estimApply},
      valueExpr ={
        values$TMplotList <- split(1:input$KEstim, ceiling(seq_along(1:input$KEstim)/3))
        values$TMestim_result <- tmEstimate(values$dfTag, K=req(input$KEstim), group=input$groupTmEstim,
                                            term=input$termTmEstim, n=input$nTmEstim, top_by=input$top_byEstim)
      }
    )

    observeEvent(input$TMplotRight,{
      if (values$TMplotIndex<ceiling(req(input$KEstim)/3)){
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

    output$d_tm_estimBpTable <- renderDataTable({

      ### BETA PROBABILITY

      #DTformat(df, numeric=c(2,3), round=2, nrow=nrow(df), size="110%")
    })

    # output$d_tm_estimDPlot <- renderPlotly({
    #   netTMestim()

    # ## DOCUMENT PLOT

    #   values$TMestim_plot
    #   })

    output$d_tm_estimTpTable <- renderDataTable({

      ### THETA PROBABILITY

      #DTformat(df, numeric=c(2,3), round=2, nrow=nrow(df), size="110%")
    })


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

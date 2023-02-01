##  Server ####
source("tallFunctions.R", local=TRUE)

### da rimuovere alla fine
library(tall)
###

server <- function(input, output, session){
  session$onSessionEnded(stopApp)

  ### Initial values ----
  load("data/sampleData.rdata")
  values <- reactiveValues()
  values$path <- NULL
  values$menu <- 0
  values$custom_lists <- NULL
  values$txt <- data.frame()
  values$list_file <- data.frame(sheet=NULL,file=NULL,n=NULL)
  values$wb <-  openxlsx::createWorkbook()
  values$dfLabel <- dfLabel()
  values$myChoices <- "Empty Report"
  values$token <- token
  values$df <- df

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
    if (!is.null(values$path)){

      txt <- read_files(values$path,ext=input$ext, subfolder=input$include_subfolder)

      values$txt <- txt
    }

  })

  output$dataImported <- DT::renderDT({
    DATAloading()
    # txt <- values$txt %>%
    #   mutate(docvar1 = gsub(.data$doc_id,"",paste0(.data$docvar1,.data$docvar2)))
    values$txt

    DT::datatable(values$txt,escape = FALSE,rownames = FALSE, extensions = c("Buttons"),
                  options = list(
                    pageLength = 5,
                    autoWidth = FALSE, scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = list(list(extend = 'pageLength'),
                                   list(extend = 'print')),
                    lengthMenu = list(c(10, 25, 50, -1),
                                      c('10 rows', '25 rows', '50 rows', 'Show all')),
                    columnDefs = list(list(
                      className = 'dt-center', targets = 0:(length(names(values$txt)) - 1)
                    ))
                  ),
                  class = 'cell-border compact stripe'
    )  %>%
      DT::formatStyle(
        names(values$txt),
        backgroundColor = 'white',
        textAlign = 'center',
        fontSize = '70%'
      )

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

  output$language_model <- renderUI({
    label_lang <- languages$repo
    names(label_lang) <- languages$short
    selectizeInput(
      'language', label="Language Model", choices = label_lang,
      multiple=FALSE,
      # options = list(
      #   onInitialize = I('function() { this.setValue(""); }')
      # ),
      tags$style("height: 50px")
    )
  })

  #custom_lists <-
    observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$custom_lists
    },
    handlerExpr = {
      file <- input$custom_lists
        req(file$datapath[1])
        custom_lists <- lapply(file$datapath,function(x){
          read_excel(x)
        })
      custom_lists <- do.call(rbind,custom_lists)
        values$custom_lists <- custom_lists
    }
  )

    output$customListsData<- DT::renderDT({
      if (is.null(values$custom_lists)){
        DTformat(data.frame(term=NULL,pos=NULL))
      } else {
        DTformat(values$custom_lists)
      }
    })

    posTagging <- eventReactive(input$tokPosRun,{
        ## download and load model language
        udmodel_lang <- loadLanguageModel(language = input$language)

        ## set cores for parallel computing
        ncores <- max(1,parallel::detectCores()-1)

        ## set cores for windows machines
        if (Sys.info()[["sysname"]]=="Windows") {
          cl <- parallel::makeCluster(ncores)
          registerDoParallel(cl)
        }

        values$dfTag <- udpipe(object=udmodel_lang, x = values$txt , parallel.cores=ncores)

        ### aggiungere controllo se operazione va a buon fine

      }
    )

    output$tokPosTagData<- DT::renderDT({
      posTagging()
      #if (!is.null(values$dfTag)){
      DTformat(values$dfTag%>% select(doc_id, sentence_id,sentence,token,lemma, upos) %>%
                 rename(D_id=doc_id,
                        S_id=sentence_id,
                        Sentence=sentence,
                        Token=token,
                        Lemma=lemma,
                        POSTag=upos)
      )
      #}
    })

  ## PoS Tag Selection ----

  ###Export Tall analysis in Rdata

  ## Pre-processing - export function ----

  output$preProSave <- downloadHandler(
    filename = function() {
      paste("TallAnalysis-Export-File-", Sys.Date(), ".rdata" , sep="")
    },
    content <- function(file) {

      tall_analysis <- list(df=values$txt)

      save(tall_analysis, file=file)
    }, contentType = "rdata"
  )


  ## OVERVIEW ----


  ### WORDS ----
  ## Frequency List ----

  wordcloud <- eventReactive(input$overviewApply,{
    # INPUT DA AGGIUNGERE
    # n <-  input$wcN numer of words
    # size <-  input$wcSize
    # scale <- input$wcScale scale transformation (log, etc.)
    n <- 100
    size <- 2
    scale <- "identity"

    values$wcDf <- distrib(values$token, scale=scale) %>%
      slice_head(n=n) %>%
      rename(word = text,
             freq = n)
    wordcloud2(values$wcDf,
               size = size,
               color = "random-light",
               backgroundColor = "transparent")
  })

  output$overviewPlot <- renderWordcloud2({
    wordcloud()
  })

  output$overviewTable <- renderDT({
    wordcloud()
    values$wcDf
  })

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

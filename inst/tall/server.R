##  Server ####
source("tallFunctions.R", local=TRUE)

server <- function(input, output, session){
  session$onSessionEnded(stopApp)

  ### Initial values ----

  values <- reactiveValues()
  values$path <- NULL
  values$txt <- data.frame()
  values$list_file <- data.frame(sheet=NULL,file=NULL,n=NULL)
  values$wb <-  openxlsx::createWorkbook()
  values$dfLabel <- dfLabel()
  values$myChoices <- "Empty Report"

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

  ###Export Tall analysis in Rdata

  ## Tokenization & Cleaning - export functions ----
  output$tok_clSave <- downloadHandler(
    filename = function() {
      paste("TallAnalysis-Export-File-", Sys.Date(), ".rdata" , sep="")
    },
    content <- function(file) {

      tall_analysis <- list(df=values$txt)

      save(tall_analysis, file=file)
    }, contentType = "rdata"
  )

  ## FILTERING


  ## TOKENIZATION & CLEANING ---

  #tok_cl <- eventReactive(input$tok_clApply){}

  ## NORMALIZATION ---

  #normalization <- eventReactive(input$normalizationApply){}

  ## FILTERING

  #filtering <- eventReactive(input$applyFiltering){}

  ## MORPHOLOGICAL ---

  #morphological <- eventReactive(input$morphologicalApply){}

  ## LEXICAL ----

  #morphological <- eventReactive(input$lexicalApply){}

  ## OVERVIEW ----

  #overview <- eventReactive(input$overviewApply){}

  ## POLARITY DETECTION ----

  #polarity_det <- eventReactive(input$polarity_detApply){}

  ## KEYWORDS CONTEXT ----

  #keywords_context <- eventReactive(input$keywords_contextApply)

  ## CLUSTERING ----

  #clustering <- eventReactive(input$clusteringApply)

  ## FACTORIAL ----

  #factorial <- eventReactive(input$factorialApply)

  ## NETWORK ----

  #network <- eventReactive(input$networkApply)

  ## TOPIC MODEL ----

  #topic_model <- eventReactive(input$topic_modelApply)

  ## CLASSIFICATION ----

  #classification <- eventReactive(input$classificationApply)

  ## EXTRACTIVE SUMMARIZATION ----

  #extractive <- eventReactive(input$extractiveApply)

  ## ABSTRACTIVE SUMMARIZATION ----

  #abstractive <- eventReactive(input$abstractiveApply)

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

} # END SERVER

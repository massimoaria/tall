editUI <- function() {
  ## EDIT ----

  ### Split ----

  split <- tabItem(
    tabName = "split_tx",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(strong("Split Corpus"), align = "center"),
          br(),
        )
      )
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Split Results",
        fluidPage(
          fluidRow(
            column(
              9,
              shinycssloaders::withSpinner(
                DT::DTOutput("splitTextData"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            column(
              3,
              fluidRow(
                box(
                  width = 12,
                  div(
                    h3(strong(em("Split texts"))),
                    style = "margin-top:-57px"
                  ),
                  hr(),
                  textInput(
                    inputId = "txSplitWord",
                    label = "Split texts by a sequence of characters (e.g. **H1**)",
                    value = NULL
                  ),
                  hr(),
                  helpText(em(
                    "The minimum sequence of characters required to split the text must consist of at least three characters.",
                    br(),
                    br(),
                    "It's important to note that the text used as a delimiter for splitting is case sensitive (e.g., 'CHAPTER' is different from 'chapter')."
                  )),
                  div(
                    fluidRow(
                      column(
                        4,
                        div(
                          align = "center",
                          title = t_run,
                          do.call(
                            "actionButton",
                            c(
                              run_bttn,
                              list(
                                inputId = "splitTextRun"
                              )
                            )
                          )
                        )
                      ),
                      column(
                        4,
                        div(
                          align = "center",
                          title = t_back,
                          do.call(
                            "actionButton",
                            c(
                              back_bttn,
                              list(
                                inputId = "splitTextBack"
                              )
                            )
                          )
                        )
                      ),
                      column(
                        4,
                        div(
                          align = "center",
                          title = t_save,
                          do.call(
                            "actionButton",
                            c(list(
                              label = NULL,
                              style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                              icon = icon("floppy-disk"),
                              inputId = "splitTextSave"
                            ))
                          )
                        )
                      )
                    ),
                    style = "margin-top:-15px"
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Info & References",
        fluidPage(
          fluidRow(
            column(1),
            column(
              10,
              br(),
              HTML(infoTexts$split)
            ),
            column(1)
          )
        )
      )
    )
  )

  ### Random selection ----

  randomText <- tabItem(
    tabName = "randomText",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(strong("Random Selection"), align = "center"),
          br(),
        )
      )
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Random Selection",
        fluidPage(
          fluidRow(
            column(
              9,
              shinycssloaders::withSpinner(
                DT::DTOutput("randomTextData"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            column(
              3,
              fluidRow(
                box(
                  width = 12,
                  div(
                    h3(strong(em("Random Text Selection"))),
                    style = "margin-top:-57px"
                  ),
                  hr(),
                  uiOutput("randomDescription"),
                  br(),
                  "Extract a random sample of texts to analyze",
                  hr(),
                  div(
                    numericInput(
                      "sampleSize",
                      "Sample Size (%)",
                      value = 30,
                      min = 1,
                      max = 100,
                      step = 1
                    ),
                    style = "margin-top:-9px"
                  ),
                  uiOutput("sampleSizeUI"),
                  fluidRow(
                    column(
                      4,
                      title = t_run,
                      do.call(
                        "actionButton",
                        c(
                          run_bttn,
                          list(
                            inputId = "randomTextRun"
                          )
                        )
                      )
                    ),
                    column(
                      4,
                      title = t_back,
                      do.call(
                        "actionButton",
                        c(
                          back_bttn,
                          list(
                            inputId = "randomTextBack"
                          )
                        )
                      )
                    ),
                    column(
                      4,
                      div(
                        align = "center",
                        title = t_save,
                        do.call(
                          "actionButton",
                          c(list(
                            label = NULL,
                            style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                                          border: 1px; margin-top: 16px;",
                            icon = icon("floppy-disk"),
                            inputId = "randomTextSave"
                          ))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Info & References",
        fluidPage(
          fluidRow(
            column(1),
            column(
              10,
              br(),
              HTML(infoTexts$random)
            ),
            column(1)
          )
        )
      )
    )
  )

  ### EXTERNAL INFORMATION ----

  extInfo <- tabItem(
    tabName = "extInfo",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(strong("External Information"), align = "center"),
          br(),
        )
      )
    ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Corpus with External Information",
        fluidPage(
          fluidRow(
            column(
              9,
              shinycssloaders::withSpinner(
                DT::DTOutput("extInfoData"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            column(
              3,
              fluidRow(
                box(
                  width = 12,
                  div(
                    h3(strong(em("Add from a file"))),
                    style = "margin-top:-57px"
                  ),
                  helpText(h5(
                    "To import external information, please make sure that the file
                           to be uploaded is in Excel format and contains a column labeled
                           'doc_id' to identify documents associated to the text(s) imported."
                  )),
                  helpText(h5(
                    "You can download the list of doc_id associated with the imported text files below."
                  )),
                  fluidRow(
                    column(
                      12,
                      div(
                        align = "center",
                        actionButton(
                          inputId = "doc_idExport",
                          label = strong("Export doc_id list"),
                          icon = NULL,
                          style = "border-radius: 15px; border-width: 1px; font-size: 15px;
                                                                    text-align: center; color: #ffff; "
                        )
                      )
                    )
                  ),
                  hr(),
                  fileInput(
                    inputId = "extInfoFile",
                    label = "Import external information",
                    multiple = FALSE,
                    accept = c(
                      ".xls",
                      ".xlsx"
                    ),
                    placeholder = "No file(s) selected"
                  ),
                  hr(),
                  div(
                    fluidRow(
                      column(
                        4,
                        div(
                          title = t_run,
                          do.call(
                            "actionButton",
                            c(
                              run_bttn,
                              list(
                                inputId = "extInfoRun"
                              )
                            )
                          ),
                          align = "center"
                        )
                      ),
                      column(
                        4,
                        div(
                          align = "center",
                          title = "Back to imported text(s) ",
                          do.call(
                            "actionButton",
                            c(
                              back_bttn,
                              list(
                                inputId = "extInfoTextBack"
                              )
                            )
                          )
                        )
                      ),
                      column(
                        4,
                        div(
                          title = t_save,
                          do.call(
                            "actionButton",
                            c(
                              list(
                                label = NULL,
                                style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                icon = icon("floppy-disk")
                              ),
                              list(
                                inputId = "extInfoSave"
                              )
                            )
                          ),
                          align = "center"
                        )
                      )
                    ),
                    style = "margin-top: -15px"
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Info & References",
        fluidPage(
          fluidRow(
            column(1),
            column(
              10,
              br(),
              HTML(infoTexts$externalinfo)
            ),
            column(1)
          )
        )
      )
    )
  )
  return(
    list(
      split = split,
      randomText = randomText,
      extInfo = extInfo
    )
  )
}

editServer <- function(input, output, session, values, statsValues) {
  ## EDIT ----

  ### SPLIT ----

  splitDocFunc <- eventReactive(input$splitTextRun, {
    if (nchar(input$txSplitWord) < 3) {
      popUpGeneric(
        title = "Error",
        type = "error",
        color = c("#913333"),
        subtitle = "Sequence must be at least 3 characters long",
        btn_labels = "OK"
      )
    } else {
      values$txt <- splitDoc(values$txt, word = input$txSplitWord)
      popUpGeneric(
        title = paste0("Split by: '", input$txSplitWord, "'"),
        type = "success",
        color = c("#1d8fe1"),
        subtitle = paste0("Now you have ", nrow(values$txt), " documents"),
        btn_labels = "OK"
      )
    }
  })

  ## back to the original txt
  observeEvent(input$splitTextBack, {
    values$txt <- unsplitDoc(values$txt)
    popUpGeneric(
      title = "Restored",
      type = "waiting",
      color = c("#FFA800"),
      subtitle = paste0("Now you have ", nrow(values$txt), " documents"),
      btn_labels = "OK"
    )
  })

  output$splitTextData <- DT::renderDT({
    splitDocFunc()
    DTformat(
      values$txt %>%
        mutate(text = paste0(substr(text, 1, 500), "...")) %>%
        mutate(across(
          c(where(is.character), -text),
          ~ paste0(substr(., 1, 150), "...")
        )) %>%
        select(-c("text_original", ends_with("id_old"))) %>%
        filter(doc_selected) %>%
        select(-"doc_selected"),
      left = 2,
      nrow = 5,
      filter = "none",
      button = TRUE,
      delete = TRUE
    )
  })

  observeEvent(
    eventExpr = {
      input$splitTextSave
    },
    handlerExpr = {
      file_path <- destFolder(
        paste("Tall-Export-File-", sys.time(), ".csv", sep = ""),
        values$wdTall
      )
      readr::write_csv(
        x = values$txt %>%
          filter(doc_selected) %>%
          select(-c("text_original", "doc_selected", ends_with("id_old"))),
        file = file_path,
        na = "NA",
        append = FALSE,
        col_names = TRUE
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
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

  randomTextFunc <- eventReactive(input$randomTextRun, {
    values$txt <- samplingText(
      values$txt,
      n = as.numeric(round((input$sampleSize / 100) * nrow(values$txt)), 0)
    )
  })

  output$sampleSizeUI <- renderUI({
    req(input$sampleSize)
    HTML(paste0(
      "<br><h5><em>Number of randomly selected texts: </em><b>",
      as.numeric(round((input$sampleSize / 100) * nrow(values$txt)), 0),
      "</b></h5>"
    ))
  })

  output$randomTextData <- DT::renderDT({
    randomTextFunc()
    DTformat(
      values$txt %>%
        filter(doc_selected) %>%
        select(-c("doc_selected", "text_original")) %>%
        mutate(text = paste0(substr(text, 1, 500), "...")),
      left = 2,
      nrow = 5,
      filter = "none",
      button = TRUE
    )
  })

  ## back to the original txt
  observeEvent(input$randomTextBack, {
    values$txt <- values$txt %>%
      mutate(doc_selected = TRUE)
    updateNumericInput(
      inputId = "sampleSize",
      value = 100
    )
  })

  observeEvent(
    eventExpr = {
      input$randomTextSave
    },
    handlerExpr = {
      file_path <- destFolder(
        paste("Tall-Export-File-", sys.time(), ".csv", sep = ""),
        values$wdTall
      )
      readr::write_csv(
        x = values$txt %>%
          filter(doc_selected) %>%
          select(-c("text_original", "doc_selected", ends_with("id_old"))),
        file = file_path,
        na = "NA",
        append = FALSE,
        col_names = TRUE
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ### EXTERNAL INFORMATION ----

  EXTINFOloading <- eventReactive(input$extInfoRun, {
    req(input$extInfoFile)
    file_extinfo <- input$extInfoFile$datapath
    values$txt <- loadExtInfo(file_extinfo, values$txt)
  })

  output$extInfoData <- DT::renderDT({
    EXTINFOloading()
    DTformat(
      values$txt %>%
        filter(doc_selected) %>%
        select(-c("text_original", "doc_selected")) %>%
        mutate(text = paste0(substr(text, 1, 250), "...")),
      left = 4,
      nrow = 3,
      filter = "top",
      button = TRUE,
      delete = TRUE
    )
  })

  observeEvent(
    eventExpr = {
      input$doc_idExport
    },
    handlerExpr = {
      file <- paste("DocID_List-", sys.time(), ".xlsx", sep = "")
      file <- destFolder(file, values$wdTall)
      suppressWarnings(openxlsx::write.xlsx(
        values$txt %>%
          filter(doc_selected) %>%
          select(doc_id),
        file = file
      ))
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  observeEvent(
    eventExpr = {
      input$extInfoSave
    },
    handlerExpr = {
      file_path <- destFolder(
        paste("Tall-Export-File-", sys.time(), ".csv", sep = ""),
        values$wdTall
      )
      readr::write_csv(
        x = values$txt %>%
          filter(doc_selected) %>%
          select(-c("text_original", "doc_selected")),
        file = file_path,
        na = "NA",
        append = FALSE,
        col_names = TRUE
        # quote = c("needed"),
        # escape = c("backslash"),
        # eol = "\n"
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  # Back to the original import text ----
  observeEvent(input$extInfoTextBack, {
    values$txt <- values$txt %>%
      mutate(doc_selected = TRUE)
  })
}

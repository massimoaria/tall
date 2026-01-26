documentsUI <- function() {
  ### DOCUMENTS ----

  ### Topic Modeling ----
  ### K choice ----

  tm_k <- tabItem(
    tabName = "d_tm_select",
    fluidPage(
      fluidRow(
        column(
          9,
          h3(
            strong("Topic Modeling: Optimal selection of topic number"),
            align = "center"
          )
        ),
        div(
          title = t_run,
          column(
            1,
            do.call(
              "actionButton",
              c(
                run_bttn,
                list(
                  inputId = "d_tm_selectApply"
                )
              )
            )
          )
        ),
        div(
          title = t_export,
          column(
            1,
            do.call(
              "actionButton",
              c(
                export_bttn,
                list(
                  inputId = "d_tm_selectExport"
                )
              )
            )
          )
        ),
        div(
          title = t_report,
          column(
            1,
            do.call(
              "actionButton",
              c(
                report_bttn,
                list(
                  inputId = "d_tm_selectReport"
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          9,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Tuning Plot",
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "d_tm_selectPlot",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(
                DT::DTOutput("d_tm_selectTable"),
                color = getOption("spinner.color", default = "#4F7942")
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
                    HTML(infoTexts$tmkchoice)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          3,
          div(
            box(
              width = 12,
              div(
                h3(strong(em("Find optimal K"))),
                style = "margin-top:-57px"
              ),
              tags$hr(),
              style = "text-align: left; text-color: #989898",
              selectInput(
                inputId = "groupTm",
                label = "Topics",
                choices = c(
                  "Groups" = "Groups",
                  "Docs" = "doc_id",
                  "Sentences" = "sentence_id"
                ),
                selected = "doc_id"
              ),
              selectInput(
                "metric",
                "Metric for model tuning",
                choices = c(
                  "CaoJuan-2009" = "CaoJuan2009",
                  "Deveaud-2014" = "Deveaud2014",
                  "Arun-2010" = "Arun2010",
                  "Perplexity" = "Perplexity"
                ),
                selected = "CaoJuan2009"
              ),
              fluidRow(
                column(6),
                column(
                  6,
                  numericInput(
                    "nTm",
                    label = "N. of terms",
                    value = 100,
                    min = 1,
                    step = 1
                  )
                )
              ),
              selectInput(
                "top_by",
                "Terms selection by:",
                choices = c(
                  "Frequency" = "freq",
                  "TF-IDF" = "tfidf"
                ),
                selected = "freq"
              ),
              fluidRow(
                column(
                  4,
                  numericInput(
                    "minK",
                    label = "K min",
                    value = 2,
                    min = 2,
                    step = 1
                  )
                ),
                column(
                  4,
                  numericInput(
                    "maxK",
                    label = "K max",
                    value = 20,
                    min = 3,
                    step = 1
                  )
                ),
                column(
                  4,
                  numericInput(
                    "Kby",
                    label = "K by:",
                    value = 1,
                    min = 1,
                    step = 1
                  )
                )
              )
            ),
            style = "margin-top:40px"
          )
        )
      )
    )
  )

  ### Model estimation ----

  tm_analysis <- tabItem(
    tabName = "d_tm_estim",
    fluidPage(
      fluidRow(
        column(
          8,
          h3(strong("Topic Modeling: Model estimation"), align = "center")
        ),
        div(
          title = t_run,
          column(
            1,
            do.call(
              "actionButton",
              c(
                run_bttn,
                list(
                  inputId = "d_tm_estimApply"
                )
              )
            )
          )
        ),
        div(
          title = t_export,
          column(
            1,
            do.call(
              "actionButton",
              c(
                export_bttn,
                list(
                  inputId = "d_tm_estimExport"
                )
              )
            )
          )
        ),
        div(
          title = t_report,
          column(
            1,
            do.call(
              "actionButton",
              c(
                report_bttn,
                list(
                  inputId = "d_tm_estimReport"
                )
              )
            )
          )
        ),
        div(
          column(
            1,
            dropdown(
              h4(strong("Options: ")),
              br(),

              # Main Configuration
              div(
                class = "config-section",
                div(
                  class = "config-section-header",
                  icon("cog", lib = "glyphicon"),
                  "Main Configuration"
                ),
                materialSwitch(
                  inputId = "tmKauto",
                  label = "Automatic Topic Selection",
                  value = TRUE,
                  status = "success"
                ),
                conditionalPanel(
                  "!input.tmKauto",
                  numericInput(
                    "KEstim",
                    label = "N. of Topics (K)",
                    value = 2,
                    min = 2,
                    step = 1
                  )
                ),
                selectInput(
                  inputId = "groupTmEstim",
                  label = "Topics",
                  choices = c(
                    "Groups" = "Groups",
                    "Docs" = "doc_id",
                    "Sentences" = "sentence_id"
                  ),
                  selected = "doc_id"
                )
              ),

              # Terms Selection Section
              tags$details(
                class = "filter-section",
                tags$summary(
                  div(
                    class = "filter-section-header",
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    div(
                      icon("filter", lib = "glyphicon"),
                      " Terms Selection"
                    ),
                    icon(
                      "chevron-down",
                      lib = "glyphicon",
                      style = "font-size: 12px;"
                    )
                  )
                ),
                div(
                  style = "margin-top: 10px;",
                  numericInput(
                    "nTmEstim",
                    label = "N. of terms",
                    value = 100,
                    min = 1,
                    step = 1
                  ),
                  selectInput(
                    "top_byEstim",
                    "Terms selection by:",
                    choices = c(
                      "Frequency" = "freq",
                      "TF-IDF" = "tfidf"
                    ),
                    selected = "freq"
                  )
                )
              ),

              # Display Options Section
              tags$details(
                class = "params-section",
                tags$summary(
                  div(
                    class = "params-section-header",
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    div(
                      icon("eye-open", lib = "glyphicon"),
                      " Display Options"
                    ),
                    icon(
                      "chevron-down",
                      lib = "glyphicon",
                      style = "font-size: 12px;"
                    )
                  )
                ),
                div(
                  style = "margin-top: 10px;",
                  numericInput(
                    "nTopicPlot",
                    label = "Word/Docs per Topic",
                    value = 10,
                    min = 2,
                    step = 1
                  )
                )
              ),

              style = "gradient",
              right = TRUE,
              animate = TRUE,
              circle = TRUE,
              tooltip = tooltipOptions(title = "Options"),
              icon = icon("sliders", lib = "font-awesome"),
              width = "320px"
            )
          ),
          style = style_opt
        )
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Topic by Words Plot",
          fluidRow(
            column(
              4,
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "d_tm_estimTPlot1",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            column(
              4,
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "d_tm_estimTPlot2",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            column(
              4,
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "d_tm_estimTPlot3",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            )
          ),
          fluidRow(
            actionButton("TMplotLeft", icon("menu-left", lib = "glyphicon")),
            actionButton(
              "TMplotRight",
              icon("menu-right", lib = "glyphicon")
            ),
            align = "center"
          )
        ),
        tabPanel(
          "Topic by Docs Plot",
          fluidRow(
            column(
              4,
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "d_tm_DocPlot1",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            column(
              4,
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "d_tm_DocPlot2",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            column(
              4,
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "d_tm_DocPlot3",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            )
          ),
          fluidRow(
            actionButton("TMdocLeft", icon("menu-left", lib = "glyphicon")),
            actionButton("TMdocRight", icon("menu-right", lib = "glyphicon")),
            align = "center"
          )
        ),
        tabPanel(
          "Beta Probability",
          shinycssloaders::withSpinner(
            DT::DTOutput("d_tm_estimBpTable"),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        tabPanel(
          "Theta Probability",
          shinycssloaders::withSpinner(
            DT::DTOutput("d_tm_estimTpTable"),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        tabPanel(
          "Topic Correlation",
          shinycssloaders::withSpinner(
            plotlyOutput(
              outputId = "d_tm_networkPlot",
              height = "75vh",
              width = "98.9%"
            ),
            # visNetworkOutput("d_tm_networkPlot", width="auto", height = "75vh"),
            color = getOption("spinner.color", default = "#4F7942")
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
                HTML(infoTexts$tmmodelestimation)
              ),
              column(1)
            )
          )
        ),
        tabPanel(
          "TALL AI",
          fluidPage(
            fluidRow(
              column(
                12,
                br(),
                shinycssloaders::withSpinner(
                  htmlOutput("d_tm_GeminiUI"),
                  caption = HTML("<br><strong>Thinking...</strong>"),
                  image = "ai_small2.gif",
                  color = "#4F7942"
                )
              )
            )
          )
        )
      )
    )
  )

  ### Polarity detection ----

  polarity <- tabItem(
    tabName = "d_polDet",
    fluidPage(
      fluidRow(
        column(
          8,
          h3(strong("Polarity Detection"), align = "center")
        ),
        div(
          title = t_run,
          column(
            1,
            do.call(
              "actionButton",
              c(
                run_bttn,
                list(
                  inputId = "d_polDetApply"
                )
              )
            )
          )
        ),
        div(
          # style=style_bttn,
          title = t_export,
          column(
            1,
            do.call(
              "actionButton",
              c(
                export_bttn,
                list(
                  inputId = "d_polDetExport"
                )
              )
            )
          )
        ),
        div(
          title = t_report,
          column(
            1,
            do.call(
              "actionButton",
              c(
                report_bttn,
                list(
                  inputId = "d_polDetReport"
                )
              )
            )
          )
        ),
        div(
          column(
            1,
            dropdown(
              h4(strong("Options: ")),
              br(),

              # Main Configuration
              div(
                class = "config-section",
                div(
                  class = "config-section-header",
                  icon("cog", lib = "glyphicon"),
                  "Main Configuration"
                ),
                selectInput(
                  inputId = "groupPolarity",
                  label = "Polarity of",
                  choices = c(
                    "Groups" = "Groups",
                    "Docs" = "doc_id"
                  ),
                  selected = "doc_id"
                ),
                uiOutput("lexiconD_polarity")
              ),

              style = "gradient",
              right = TRUE,
              animate = TRUE,
              circle = TRUE,
              tooltip = tooltipOptions(title = "Options"),
              icon = icon("sliders", lib = "font-awesome"),
              width = "300px"
            )
          ),
          style = style_opt
        )
      ),
      fluidRow(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Document Polarity Distribution",
            fluidRow(
              column(
                6,
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_polPiePlot",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                6,
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_polDensPlot",
                    height = "37vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                ), # ),
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_polBoxPlot",
                    height = "37vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                ) # )
              )
            )
          ),
          tabPanel(
            "Top Words",
            fluidRow(
              column(
                6,
                align = "center",
                h4("Top Positive Words by Document Polarity Distribution"),
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_polDetPlotPos",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                6,
                align = "center",
                h4("Top Negative Words by Document Polarity Distribution"),
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_polDetPlotNeg",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              )
            )
          ),
          tabPanel(
            "Table",
            shinycssloaders::withSpinner(
              DT::DTOutput("d_polDetTable"),
              color = getOption("spinner.color", default = "#4F7942")
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
                  HTML(infoTexts$polaritydetection)
                ),
                column(1)
              )
            )
          ),
          tabPanel(
            "TALL AI",
            fluidPage(
              fluidRow(
                column(
                  12,
                  br(),
                  shinycssloaders::withSpinner(
                    htmlOutput("d_polDet_GeminiUI"),
                    caption = HTML("<br><strong>Thinking...</strong>"),
                    image = "ai_small2.gif",
                    color = "#4F7942"
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  ### Abstractive Summarization ----

  abs_summ <- tabItem(
    tabName = "d_astractive",
    fluidPage(
      fluidRow(
        column(
          11,
          h3(strong("Abstractive Summarization"), align = "center")
        ),
        div(
          title = t_report,
          column(
            1,
            do.call(
              "actionButton",
              c(
                report_bttn,
                list(
                  inputId = "d_astractiveReport"
                )
              )
            )
          )
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          9,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Abstract",
              br(),
              shinycssloaders::withSpinner(
                uiOutput("summaryData"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Full Document",
              br(),
              shinycssloaders::withSpinner(
                uiOutput("documentData2"),
                color = getOption("spinner.color", default = "#4F7942")
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
                    HTML(infoTexts$summarization)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          3,
          div(
            box(
              width = 12,
              div(
                h3(strong(em("Abstractive Summarization"))),
                style = "margin-top:-57px"
              ),
              h5(
                ("TALL generates a coherent and concise summary by interpreting and paraphrasing the main ideas from the original text"),
                style = "text-align: left; text-color: #989898"
              ),
              tags$hr(),
              fluidRow(column(
                12,
                uiOutput("optionsUnitAbstractive"),
                uiOutput("optionsAbstractive"),
                numericInput(
                  "summaryLength",
                  label = "Summary Length (in words)",
                  value = 250,
                  min = 10,
                  step = 10,
                  max = 16384
                ),
                uiOutput("abstractivePromptUI"),
              )),
              hr(),
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
                            inputId = "d_abstractiveApply"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    div(
                      align = "center",
                      title = t_view,
                      do.call(
                        "actionButton",
                        c(
                          view_bttn,
                          list(
                            inputId = "d_abstractiveView"
                          )
                        )
                      )
                    )
                  ),
                  column(4)
                ),
                style = "margin-top:-15px"
              )
            ),
            style = "margin-top:40px"
          )
        )
      )
    )
  )

  ### Extractive Summarization ----

  ext_summ <- tabItem(
    tabName = "d_summarization",
    fluidPage(
      fluidRow(
        column(
          11,
          h3(strong("Extractive Summarization"), align = "center")
        ),
        div(
          title = t_report,
          column(
            1,
            do.call(
              "actionButton",
              c(
                report_bttn,
                list(
                  inputId = "d_summarizationReport"
                )
              )
            )
          )
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          9,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Abstract",
              br(),
              fluidRow(
                #style = "height:65vh",
                shinycssloaders::withSpinner(
                  uiOutput("abstractData"),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              fluidRow(
                br(),
                align = "center",
                shinycssloaders::withSpinner(
                  uiOutput("sliderAbstractData"),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              )
            ),
            tabPanel(
              "Full Document",
              br(),
              shinycssloaders::withSpinner(
                # DT::DTOutput("documentData"),
                uiOutput("documentData"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Sentence Ranking",
              shinycssloaders::withSpinner(
                DT::DTOutput("RelSentData"),
                color = getOption("spinner.color", default = "#4F7942")
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
                    HTML(infoTexts$summarization)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          3,
          div(
            box(
              width = 12,
              div(
                h3(strong(em("Extractive Summarization"))),
                style = "margin-top:-57px"
              ),
              h5(
                ("TALL selects and reorders the most relevant sentences from the original text to generate a coherent and concise summary"),
                style = "text-align: left; text-color: #989898"
              ),
              tags$hr(),
              fluidRow(column(
                12,
                uiOutput("optionsUnitSummarization"),
                uiOutput("optionsSummarization")
              )),
              hr(),
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
                            inputId = "d_summarizationApply"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    div(
                      align = "center",
                      title = t_view,
                      do.call(
                        "actionButton",
                        c(
                          view_bttn,
                          list(
                            inputId = "d_summarizationView"
                          )
                        )
                      )
                    )
                  ),
                  column(4)
                ),
                style = "margin-top:-15px"
              )
            ),
            style = "margin-top:40px"
          )
        )
      )
    )
  )

  return(list(
    tm_k = tm_k,
    tm_analysis = tm_analysis,
    polarity = polarity,
    abs_summ = abs_summ,
    ext_summ = ext_summ
  ))
}

documentsServer <- function(input, output, session, values, statsValues) {
  ## DOCUMENTS ----

  ## Topic Modeling ----
  ## K choice ----

  netTMKselect <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$d_tm_selectApply
    },
    valueExpr = {
      switch(
        input$groupTm,
        Groups = {
          groupTm <- "doc_id"
        },
        {
          groupTm <- input$groupTm
        }
      )
      ## check to verify if groups exist or not
      if (
        input$groupTm == "doc_id" & "ungroupDoc_id" %in% names(values$dfTag)
      ) {
        values$TMKresult <- tmTuning(
          backToOriginalGroups(LemmaSelection(values$dfTag)) %>%
            filter(docSelected),
          group = groupTm,
          term = values$generalTerm,
          metric = input$metric,
          n = input$nTm,
          top_by = input$top_by,
          minK = input$minK,
          maxK = input$maxK,
          Kby = input$Kby
        )
      } else {
        values$TMKresult <- tmTuning(
          LemmaSelection(values$dfTag) %>% filter(docSelected),
          group = groupTm,
          term = values$generalTerm,
          metric = input$metric,
          n = input$nTm,
          top_by = input$top_by,
          minK = input$minK,
          maxK = input$maxK,
          Kby = input$Kby
        )
      }

      values$df <- values$TMKresult$metrics %>%
        arrange(k) %>%
        rename(topics = k)

      values$df <- values$df %>%
        mutate(across(
          .cols = -topics,
          .fns = ~ (. - min(.)) / (max(.) - min(.)),
          .names = "{.col}_Normalized"
        ))
      #values$df$Normalized <- (values$df[, 2] - min(values$df[, 2])) / diff(range(values$df[, 2]))
    }
  )

  output$d_tm_selectPlot <- renderPlotly({
    netTMKselect()
    values$TMKplot <- tmTuningPlot(values$TMKresult, metric = input$metric)
    values$TMKplot
  })

  output$d_tm_selectTable <- renderDataTable({
    netTMKselect()
    DTformat(
      values$df,
      numeric = c(2, 3),
      round = 2,
      nrow = nrow(df),
      size = "110%"
    )
  })

  observeEvent(
    eventExpr = {
      input$d_tm_selectExport
    },
    handlerExpr = {
      file <- paste("TMTopicSelection-", sys.time(), ".png", sep = "")
      file <- destFolder(file, values$wdTall)
      plot2png(values$TMKplot, filename = file, zoom = values$zoom)
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$d_tm_selectReport, {
    if (!is.null(values$df)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "KChoice"
      list_df <- list(values$df)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileKchoice <- plot2png(
        values$TMKplot,
        filename = "kchoiche.png",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileKchoice, res$col)
      )
      popUp(title = "K choice Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Model estimation ----

  netTMestim <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$d_tm_estimApply
    },
    valueExpr = {
      values$TMplotIndex <- 1
      values$TMdocIndex <- 1

      switch(
        input$groupTmEstim,
        Groups = {
          groupTmEstim <- "doc_id"
        },
        {
          groupTmEstim <- input$groupTmEstim
        }
      )

      ## check to verify if groups exist or not
      if (
        input$groupTmEstim == "doc_id" &
          "ungroupDoc_id" %in% names(values$dfTag)
      ) {
        if (isTRUE(input$tmKauto)) {
          values$TMKresult <- tmTuning(
            backToOriginalGroups(LemmaSelection(values$dfTag)) %>%
              filter(docSelected),
            group = groupTmEstim,
            term = values$generalTerm,
            metric = "CaoJuan2009",
            n = input$nTmEstim,
            top_by = input$top_byEstim,
            minK = 2,
            maxK = 20,
            Kby = 1
          )
          values$tmK <- find_elbow(
            values$TMKresult$metrics$k,
            values$TMKresult$metrics$CaoJuan2009,
            decreasing = TRUE,
            plot = FALSE
          )
        } else {
          values$tmK <- input$KEstim
        }
        values$TMplotList <- split(
          1:values$tmK,
          ceiling(seq_along(1:values$tmK) / 3)
        )
        values$TMestim_result <- tmEstimate(
          backToOriginalGroups(LemmaSelection(values$dfTag)) %>%
            filter(docSelected),
          K = values$tmK,
          group = groupTmEstim,
          term = values$generalTerm,
          n = input$nTmEstim,
          top_by = input$top_byEstim
        )
      } else {
        if (isTRUE(input$tmKauto)) {
          values$TMKresult <- tmTuning(
            LemmaSelection(values$dfTag) %>% filter(docSelected),
            group = groupTmEstim,
            term = values$generalTerm,
            metric = "CaoJuan2009",
            n = input$nTmEstim,
            top_by = input$top_byEstim,
            minK = 2,
            maxK = 20,
            Kby = 1
          )
          values$tmK <- find_elbow(
            values$TMKresult$metrics$k,
            values$TMKresult$metrics$CaoJuan2009,
            decreasing = TRUE,
            plot = FALSE
          )
        } else {
          values$tmK <- input$KEstim
        }
        values$TMplotList <- split(
          1:values$tmK,
          ceiling(seq_along(1:values$tmK) / 3)
        )
        values$TMestim_result <- tmEstimate(
          LemmaSelection(values$dfTag) %>% filter(docSelected),
          K = values$tmK,
          group = groupTmEstim,
          term = values$generalTerm,
          n = input$nTmEstim,
          top_by = input$top_byEstim
        )
      }
      ## End check ###

      ### BETA PROBABILITY
      values$beta <- values$TMestim_result$beta

      names(values$beta)[2:ncol(values$beta)] <- paste0(
        "Topic ",
        1:(ncol(values$beta) - 1)
      )
      values$tmHeatmap <- tmHeatmap(values$beta)

      ### THETA PROBABILITY
      values$theta <- values$TMestim_result$theta
      names(values$theta)[2:ncol(values$theta)] <- paste0(
        "Topic ",
        1:(ncol(values$theta) - 1)
      )
    }
  )

  output$d_tm_networkPlot <- renderPlotly({
    netTMestim()
    values$tmHeatmap$Hplot
  })

  observeEvent(input$TMplotRight, {
    if (values$TMplotIndex < ceiling(req(values$tmK) / 3)) {
      values$TMplotIndex <- values$TMplotIndex + 1
    }
  })

  observeEvent(input$TMplotLeft, {
    if (req(values$TMplotIndex) > 1) {
      values$TMplotIndex <- values$TMplotIndex - 1
    }
  })

  output$d_tm_estimTPlot1 <- renderPlotly({
    netTMestim()
    if (!values$TMplotIndex %in% 1:length(values$TMplotList)) {
      values$TMplotIndex <- 1
    }
    topic1 <- values$TMplotList[[values$TMplotIndex]]
    values$TMestim_plot1 <- tmTopicPlot(
      values$TMestim_result$beta,
      topic = topic1[[1]],
      nPlot = input$nTopicPlot
    )
    values$TMestim_plot1
  })

  output$d_tm_estimTPlot2 <- renderPlotly({
    topic2 <- values$TMplotList[[values$TMplotIndex]]
    if (length(topic2) >= 2) {
      values$TMestim_plot2 <- tmTopicPlot(
        values$TMestim_result$beta,
        topic = topic2[[2]],
        nPlot = input$nTopicPlot
      )
      values$TMestim_plot2
    }
  })

  output$d_tm_estimTPlot3 <- renderPlotly({
    topic3 <- values$TMplotList[[values$TMplotIndex]]
    if (length(topic3) == 3) {
      values$TMestim_plot3 <- tmTopicPlot(
        values$TMestim_result$beta,
        topic = topic3[[3]],
        nPlot = input$nTopicPlot
      )
      values$TMestim_plot3
    }
  })

  output$d_tm_estimBpTable <- renderDataTable(server = FALSE, {
    netTMestim()
    DTformat(
      values$beta,
      left = 1,
      numeric = c(2:ncol(values$TMestim_result$beta)),
      round = 4,
      nrow = 10,
      size = "85%",
      filename = "TopicModel_BetaTable"
    )
  })

  observeEvent(input$TMdocRight, {
    if (values$TMdocIndex < ceiling(req(values$tmK) / 3)) {
      values$TMdocIndex <- values$TMdocIndex + 1
    }
  })

  observeEvent(input$TMdocLeft, {
    if (req(values$TMdocIndex) > 1) {
      values$TMdocIndex <- values$TMdocIndex - 1
    }
  })

  output$d_tm_DocPlot1 <- renderPlotly({
    netTMestim()
    if (!values$TMdocIndex %in% 1:length(values$TMplotList)) {
      values$TMdocIndex <- 1
    }
    topic1 <- values$TMplotList[[values$TMdocIndex]]
    values$TMdoc_plot1 <- tmDocPlot(
      values$TMestim_result$theta,
      topic = topic1[[1]],
      nPlot = input$nTopicPlot
    )
    values$TMdoc_plot1
  })

  output$d_tm_DocPlot2 <- renderPlotly({
    topic2 <- values$TMplotList[[values$TMdocIndex]]
    if (length(topic2) >= 2) {
      values$TMdoc_plot2 <- tmDocPlot(
        values$TMestim_result$theta,
        topic = topic2[[2]],
        nPlot = input$nTopicPlot
      )
      values$TMdoc_plot2
    }
  })

  output$d_tm_DocPlot3 <- renderPlotly({
    topic3 <- values$TMplotList[[values$TMdocIndex]]
    if (length(topic3) == 3) {
      values$TMdoc_plot3 <- tmDocPlot(
        values$TMestim_result$theta,
        topic = topic3[[3]],
        nPlot = input$nTopicPlot
      )
      values$TMdoc_plot3
    }
  })

  output$d_tm_estimTpTable <- renderDataTable(server = FALSE, {
    netTMestim()
    DTformat(
      values$theta,
      left = 1,
      numeric = c(2:ncol(values$TMestim_result$theta)),
      round = 4,
      nrow = 10,
      size = "85%",
      filename = "TopicModel_ThetaTable"
    )
  })

  output$d_tm_GeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "Gemini AI", content = values$tmGemini, values)
  })

  observeEvent(
    eventExpr = {
      input$d_tm_estimExport
    },
    handlerExpr = {
      file1 <- paste("TMCorrPlots-", sys.time(), ".png", sep = "")
      file1 <- destFolder(file1, values$wdTall)
      file2 <- paste("TMTermPlots-", sys.time(), ".png", sep = "")
      file2 <- destFolder(file2, values$wdTall)
      file3 <- paste("TMDocPlots-", sys.time(), ".png", sep = "")
      file3 <- destFolder(file3, values$wdTall)
      values$tmGplotBeta <- topicGplot(
        values$TMestim_result$beta,
        nPlot = input$nTopicPlot,
        type = "beta"
      )
      values$tmGplotTheta <- topicGplot(
        values$TMestim_result$theta,
        nPlot = input$nTopicPlot,
        type = "theta"
      )
      ggsave(
        filename = file1,
        plot = values$tmHeatmap$HplotStatic,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "transparent"
      )
      ggsave(
        filename = file2,
        plot = values$tmGplotBeta,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "transparent"
      )
      ggsave(
        filename = file3,
        plot = values$tmGplotTheta,
        dpi = values$dpi,
        height = values$h,
        width = values$h * 2,
        bg = "transparent"
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$d_tm_estimReport, {
    if (!is.null(values$TMestim_result$beta)) {
      popUp(title = NULL, type = "waiting")
      values$tmGplotBeta <- topicGplot(
        values$TMestim_result$beta,
        nPlot = input$nTopicPlot,
        type = "beta"
      )
      values$tmGplotTheta <- topicGplot(
        values$TMestim_result$theta,
        nPlot = input$nTopicPlot,
        type = "theta"
      )
      Gem <- values$tmGemini %>% string_to_sentence_df()
      list_df <- list(
        values$tmGemini %>% string_to_sentence_df(),
        values$beta,
        values$theta
      )
      list_plot <- list(
        values$tmGplotBeta,
        values$tmGplotTheta,
        values$tmHeatmap$HplotStatic
      )
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "ModelEstim",
        wb = values$wb,
        startRow = nrow(Gem) + 1
      )
      values$wb <- wb
      popUp(title = "Model Estimation Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Polarity detection ----

  output$lexiconD_polarity <- renderUI({
    if (values$language == "english") {
      selectInput(
        inputId = "lexiconD_polarity",
        label = "Select lexicon",
        choices = c(
          "huliu",
          "loughran_mcdonald",
          "nrc"
        ),
        selected = "huliu"
      )
    }
    # )
  })

  ## Model estimation ----
  docPolarityEstim <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$d_polDetApply
    },
    valueExpr = {
      choices <- c(
        "english",
        "italian",
        "french",
        "german",
        "spanish",
        "afrikaans",
        "arabic",
        "armenian",
        "basque",
        "belarusian",
        "bulgarian",
        "catalan",
        "chinese",
        "croatian",
        "czech",
        "danish",
        "dutch",
        "estonian",
        "finnish",
        "galician",
        "greek",
        "hebrew",
        "hindi",
        "hungarian",
        "indonesian",
        "irish",
        "japanese",
        "korean",
        "latin",
        "latvian",
        "lithuanian",
        "maltese",
        "marathi",
        "norwegian",
        "persian",
        "polish",
        "portuguese",
        "romanian",
        "russian",
        "serbian",
        "slovak",
        "slovenian",
        "swedish",
        "tamil",
        "telugu",
        "turkish",
        "ukrainian",
        "urdu",
        "uyghur",
        "vietnamese"
      )
      if (values$language %in% choices) {
        if (is.null(input$lexiconD_polarity)) {
          lexiconD_polarity <- "huliu"
        } else {
          lexiconD_polarity <- input$lexiconD_polarity
        }

        ## check to verify if groups exist or not
        if (
          input$groupPolarity == "doc_id" &
            "ungroupDoc_id" %in% names(values$dfTag)
        ) {
          values$docPolarity <- sentimentAnalysis(
            backToOriginalGroups(values$dfTag) %>% filter(docSelected),
            language = values$language,
            lexicon_model = lexiconD_polarity
          )
        } else {
          values$docPolarity <- sentimentAnalysis(
            values$dfTag %>% filter(docSelected),
            language = values$language,
            lexicon_model = lexiconD_polarity
          )
        }
        values$docPolPlots <- sentimentWordPlot(
          values$docPolarity$sent_data,
          n = 10
        )
      }

      values$docPolarityOverallData <- values$docPolarity$sent_overall %>%
        select(
          doc_id,
          sentiment_polarity,
          doc_pol_clas,
          terms_positive,
          terms_negative
        ) %>%
        rename(
          Polarity = sentiment_polarity,
          "Polarity Category" = doc_pol_clas,
          "Positive Words" = terms_positive,
          "Negative Words" = terms_negative
        )
      # Pie chart
      values$sentimentPieChart <- sentimentPieChart(
        values$docPolarity$sent_overall %>%
          count(doc_pol_clas) %>%
          rename("Polarity" = doc_pol_clas)
      )
      # Density plot
      values$sentimentDensityPlot <- sentimentDensityPlot(
        values$docPolarity$sent_overall$sentiment_polarity,
        from = -1,
        to = 1
      )
      # Box plot
      values$sentimentBoxPlot <- sentimentBoxPlot(
        values$docPolarity$sent_overall
      )
    }
  )

  output$d_polPiePlot <- renderPlotly({
    docPolarityEstim()
    values$sentimentPieChart
  })

  output$d_polDensPlot <- renderPlotly({
    docPolarityEstim()
    values$sentimentDensityPlot
  })

  output$d_polBoxPlot <- renderPlotly({
    docPolarityEstim()
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

  output$d_polDet_GeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "Gemini AI", content = values$d_polDet_Gemini, values)
  })

  output$d_polDetTable <- renderDT(server = FALSE, {
    docPolarityEstim()
    DTformat(
      values$docPolarityOverallData,
      filename = "DocPolarity",
      left = c(2, 4, 5, 6),
      numeric = 3,
      round = 4,
      button = TRUE
    )
  })

  observeEvent(
    eventExpr = {
      input$d_polDetExport
    },
    handlerExpr = {
      file1 <- paste("PieChart-", sys.time(), ".png", sep = "")
      file1 <- destFolder(file1, values$wdTall)
      file2 <- paste("DensDensity-", sys.time(), ".png", sep = "")
      file2 <- destFolder(file2, values$wdTall)
      file3 <- paste("BoxPlot-", sys.time(), ".png", sep = "")
      file3 <- destFolder(file3, values$wdTall)
      file4 <- paste("Positive-", sys.time(), ".png", sep = "")
      file4 <- destFolder(file4, values$wdTall)
      file5 <- paste("Negative-", sys.time(), ".png", sep = "")
      file5 <- destFolder(file5, values$wdTall)

      plot2png(values$sentimentPieChart, filename = file1, zoom = values$zoom)
      plot2png(
        values$sentimentDensityPlot,
        filename = file2,
        zoom = values$zoom
      )
      plot2png(values$sentimentBoxPlot, filename = file3, zoom = values$zoom)
      plot2png(
        values$docPolPlots$positive,
        filename = file4,
        zoom = values$zoom
      )
      plot2png(
        values$docPolPlots$negative,
        filename = file5,
        zoom = values$zoom
      )

      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$d_polDetReport, {
    if (!is.null(values$docPolarityOverallData)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "PolarityDetection"

      Gem <- values$d_polDet_Gemini %>% string_to_sentence_df()

      list_df <- list(Gem, values$docPolarityOverallData)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- c(
        "PieChart.png",
        "DensDensity.png",
        "BoxPlot.png",
        "Positive.png",
        "Negative.png"
      )
      values$filePieChart <- plot2png(
        values$sentimentPieChart,
        filename = files[1],
        zoom = values$zoom
      )
      values$fileDensityPlot <- plot2png(
        values$sentimentDensityPlot,
        filename = files[2],
        zoom = values$zoom
      )
      values$fileBoxPlot <- plot2png(
        values$sentimentBoxPlot,
        filename = files[3],
        zoom = values$zoom
      )
      values$filedocPolPos <- plot2png(
        values$docPolPlots$positive,
        filename = files[4],
        zoom = values$zoom
      )
      values$filedocPolNeg <- plot2png(
        values$docPolPlots$negative,
        filename = files[5],
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$filePieChart, res$col),
        c(sheetname = res$sheetname, values$fileDensityPlot, res$col),
        c(sheetname = res$sheetname, values$fileBoxPlot, res$col),
        c(sheetname = res$sheetname, values$filedocPolPos, res$col),
        c(sheetname = res$sheetname, values$filedocPolNeg, res$col)
      )
      popUp(title = "Polarity Detection Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Abstractive Summarization ----

  output$optionsAbstractive <- renderUI({
    selectizeInput(
      inputId = "Abst_document_selection",
      label = "Select Document",
      choices = values$dfTag %>%
        dplyr::filter(docSelected) %>%
        select(doc_id) %>%
        unique() %>%
        pull(doc_id),
      multiple = FALSE,
      width = "100%"
    )
  })

  output$abstractivePromptUI <- renderUI({
    textAreaInput(
      inputId = "abstractivePrompt",
      label = "Provide any additional prompts for the AI Assistant:",
      placeholder = "Example: Provide the summary in bullet points; or Provide the summary in Italian language; etc.",
      value = values$abstractivePrompt,
      rows = 8,
      width = "100%"
    )
  })

  observeEvent(input$d_abstractiveApply, {
    values$abstractivePrompt <- input$abstractivePrompt
  })

  abstractiveSummarization <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$d_abstractiveApply
    },
    valueExpr = {
      values$abstractiveSumm <- abstractive_summary(
        values,
        input = input,
        id = input$Abst_document_selection,
        nL = input$summaryLength,
        api_key = NULL,
        model = values$gemini_api_model
      )
    }
  )

  output$summaryData <- renderUI({
    abstractiveSummarization()
    HTML(
      create_abstract_box(
        gemini_to_html(values$abstractiveSumm, type = "summary")
      )
    )
  })

  output$documentData2 <- renderUI({
    abstractiveSummarization()
    ## collapse sentences into paragraphs
    df_paragraphs <- values$dfTag %>%
      filter(doc_id == !!input$Abst_document_selection) %>%
      select(doc_id, paragraph_id, sentence_id, sentence) %>%
      distinct() %>%
      group_by(doc_id, paragraph_id) %>%
      summarise(Paragraph = paste(sentence, collapse = "\n ")) %>%
      ungroup()

    HTML(create_document_box(
      df_paragraphs,
      input$Abst_document_selection,
      summarization_type = "abstractive"
    ))
  })

  ## Report

  observeEvent(input$d_astractiveReport, {
    if (!is.null(values$abstractiveSumm)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "AbstractiveSummarization"

      list_df <- list(
        data.frame(
          Abstract = paste0(
            "Document: ",
            input$Abst_document_selection,
            "\n\n Abstract:\n\n",
            values$abstractiveSumm
          )
        )
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      popUp(title = "Summarization Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Extractive Summarization ----

  output$optionsUnitSummarization <- renderUI({
    selectInput(
      inputId = "unit_selection",
      label = "Summarize ",
      choices = c("Groups", "Documents"),
      selected = "Documents",
      multiple = FALSE,
      width = "100%"
    )
  })

  output$optionsSummarization <- renderUI({
    selectizeInput(
      inputId = "document_selection",
      label = ifelse(
        input$unit_selection == "Documents",
        "Select Document",
        "Select Group"
      ),
      choices = ids(
        values$dfTag %>%
          group_by(doc_id) %>%
          mutate(n_sentences = max(sentence_id)) %>%
          ungroup() %>%
          dplyr::filter(docSelected, n_sentences > 1),
        type = input$unit_selection
      ),
      multiple = FALSE,
      width = "100%"
    )
  })

  docExtraction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$d_summarizationApply
    },
    valueExpr = {
      values$docExtracted <- textrankDocument(
        values$dfTag,
        id = input$document_selection
      )
      values$docExtraction <- abstractingDocument(
        values$docExtracted$s,
        n = "5%",
        id = input$document_selection
      )
      values$docExtraction$sentences <- values$docExtracted$sentences %>%
        rename(S_id = textrank_id, Ranking = textrank)
    }
  )

  output$sliderAbstractData <- renderUI({
    docExtraction()
    choices <- c(
      "More Concise",
      paste0(seq(from = 10, to = 95, by = 5), "%"),
      "Less Concise"
    )
    sliderTextInput(
      inputId = "sliderAbstractData",
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
    eventExpr = {
      input$sliderAbstractData
    },
    valueExpr = {
      req(values$docExtracted)
      values$docExtraction <- abstractingDocument(
        values$docExtracted$s,
        n = input$sliderAbstractData,
        id = input$document_selection
      )
      values$docExtraction$sentences <- values$docExtracted$sentences %>%
        rename(S_id = textrank_id, Ranking = textrank)
    }
  )

  # Styled output for abstract
  output$abstractData <- renderUI({
    docExtractionVisualize()
    HTML(create_abstract_box(values$docExtraction$abstract))
  })

  output$RelSentData <- renderDT(server = FALSE, {
    docExtraction()
    DTformat(
      values$docExtraction$sentences,
      nrow = 10,
      size = "85%",
      title = paste0("Doc_id: ", input$document_selection),
      left = 1:2,
      numeric = 3,
      round = 4
    )
  })

  # Styled output for full document with highlighting
  output$documentData <- renderUI({
    docExtraction()
    HTML(create_document_box(
      values$docExtraction$document,
      input$document_selection,
      summarization_type = "extractive"
    ))
  })

  ## Report

  observeEvent(input$d_summarizationReport, {
    if (!is.null(values$docExtraction$sentences)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "ExtractiveSummarization"

      values$docExtraction$abstractData <- data.frame(
        "Abstract" = values$docExtraction$abstract
      )
      values$docExtraction$abstractData <- values$docExtraction$abstractData %>%
        mutate(Abstract = gsub("<.*?>", "", Abstract))

      list_df <- list(
        as.data.frame(values$docExtraction$abstractData),
        values$docExtraction$sentences,
        values$docExtraction$document
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      popUp(title = "Summarization Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })
}

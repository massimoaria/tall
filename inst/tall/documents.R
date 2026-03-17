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
              "Multi-Metric Comparison",
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "d_tm_multiMetricPlot",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "K Recommendation",
              br(),
              shinycssloaders::withSpinner(
                uiOutput("d_tm_kRecommendationUI"),
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
                inputId = "tmMethodK",
                label = "Model",
                choices = c(
                  "LDA" = "LDA",
                  "CTM (Correlated)" = "CTM",
                  "STM (Structural)" = "STM"
                ),
                selected = "LDA"
              ),
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
                  icon("gear"),
                  "Main Configuration"
                ),
                selectInput(
                  inputId = "tmMethod",
                  label = "Model",
                  choices = c(
                    "LDA" = "LDA",
                    "CTM (Correlated)" = "CTM",
                    "STM (Structural)" = "STM"
                  ),
                  selected = "LDA"
                ),
                conditionalPanel(
                  "input.tmMethod == 'STM'",
                  uiOutput("stmPrevalenceSelect"),
                  helpText(
                    "Prevalence covariates affect topic proportions across documents.",
                    style = "font-size: 11px; color: #888; margin-top: -5px;"
                  )
                ),
                uiOutput("tmKSelectionUI"),
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
                      icon("filter"),
                      " Terms Selection"
                    ),
                    icon(
                      "chevron-down",
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
                      icon("eye"),
                      " Display Options"
                    ),
                    icon(
                      "chevron-down",
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
            actionButton("TMplotLeft", icon("chevron-left")),
            actionButton(
              "TMplotRight",
              icon("chevron-right")
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
            actionButton("TMdocLeft", icon("chevron-left")),
            actionButton("TMdocRight", icon("chevron-right")),
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
          "Model Diagnostics",
          br(),
          shinycssloaders::withSpinner(
            uiOutput("d_tm_diagnosticsUI"),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        tabPanel(
          "Covariate Effects",
          br(),
          shinycssloaders::withSpinner(
            uiOutput("d_tm_covariateUI"),
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

  ### Syntactic Complexity ----

  syntactic_complexity <- tabItem(
    tabName = "d_syntactic",
    fluidPage(
      fluidRow(
        column(
          9,
          h3(strong("Syntactic Complexity"), align = "center")
        ),
        div(
          title = t_run,
          column(
            1,
            do.call(
              "actionButton",
              c(
                run_bttn,
                list(inputId = "d_syntacticApply")
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
                list(inputId = "d_syntacticExport")
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
                list(inputId = "d_syntacticReport")
              )
            )
          )
        )
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Document Metrics",
          br(),
          shinycssloaders::withSpinner(
            DT::DTOutput("d_syntacticTable"),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        tabPanel(
          "Corpus Summary",
          br(),
          shinycssloaders::withSpinner(
            uiOutput("d_syntacticSummaryUI"),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        tabPanel(
          "Distributions",
          shinycssloaders::withSpinner(
            plotlyOutput(
              outputId = "d_syntacticDistPlot",
              height = "80vh",
              width = "98.9%"
            ),
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
                HTML(infoTexts$syntacticcomplexity)
              ),
              column(1)
            )
          )
        )
      )
    )
  )

  ### SVO Triplets ----

  svo_analysis <- tabItem(
    tabName = "d_svo",
    fluidPage(
      fluidRow(
        column(
          8,
          h3(strong("SVO Triplet Extraction"), align = "center")
        ),
        div(
          title = t_run,
          column(
            1,
            do.call(
              "actionButton",
              c(
                run_bttn,
                list(inputId = "d_svoApply")
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
                list(inputId = "d_svoExport")
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
                list(inputId = "d_svoReport")
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
              div(
                class = "config-section",
                div(
                  class = "config-section-header",
                  icon("gear"),
                  "Configuration"
                ),
                numericInput(
                  "svoFreqMin",
                  label = "Min. Frequency",
                  value = 2,
                  min = 1,
                  step = 1
                ),
                numericInput(
                  "svoTopN",
                  label = "Top N Triplets",
                  value = 50,
                  min = 10,
                  step = 10
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
          "SVO Table",
          br(),
          shinycssloaders::withSpinner(
            DT::DTOutput("d_svoTable"),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        tabPanel(
          "SVO Network",
          shinycssloaders::withSpinner(
            plotlyOutput(
              outputId = "d_svoNetwork",
              height = "80vh",
              width = "98.9%"
            ),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        tabPanel(
          "Verb Frequency",
          shinycssloaders::withSpinner(
            plotlyOutput(
              outputId = "d_svoVerbPlot",
              height = "75vh",
              width = "98.9%"
            ),
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
                HTML(infoTexts$svo)
              ),
              column(1)
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
                  icon("gear"),
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

  ### Emotion Analysis ----

  emotion <- tabItem(
    tabName = "d_emo",
    fluidPage(
      fluidRow(
        column(
          8,
          h3(strong("Emotion Analysis"), align = "center")
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
                  inputId = "d_emoApply"
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
                  inputId = "d_emoExport"
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
                  inputId = "d_emoReport"
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
                  icon("gear"),
                  "Main Configuration"
                ),
                selectInput(
                  inputId = "groupEmotion",
                  label = "Emotion Analysis of",
                  choices = c(
                    "Groups" = "Groups",
                    "Docs" = "doc_id"
                  ),
                  selected = "doc_id"
                )
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
            "Emotion Distribution",
            shinycssloaders::withSpinner(
              plotlyOutput(
                outputId = "d_emoBarPlot",
                height = "75vh",
                width = "98.9%"
              ),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          tabPanel(
            "Top Words by Emotion",
            fluidRow(
              column(
                4,
                selectInput(
                  inputId = "d_emoSelectEmotion",
                  label = "Select Emotion",
                  choices = c(
                    "anger", "anticipation", "disgust", "fear",
                    "joy", "sadness", "surprise", "trust"
                  ),
                  selected = "joy"
                )
              )
            ),
            shinycssloaders::withSpinner(
              plotlyOutput(
                outputId = "d_emoWordPlot",
                height = "75vh",
                width = "98.9%"
              ),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          tabPanel(
            "Document Heatmap",
            shinycssloaders::withSpinner(
              plotlyOutput(
                outputId = "d_emoHeatmap",
                height = "75vh",
                width = "98.9%"
              ),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          tabPanel(
            "Table",
            shinycssloaders::withSpinner(
              DT::DTOutput("d_emoTable"),
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
                  HTML(infoTexts$emotionanalysis)
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
                    htmlOutput("d_emo_GeminiUI"),
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
    syntactic_complexity = syntactic_complexity,
    svo_analysis = svo_analysis,
    polarity = polarity,
    emotion = emotion,
    abs_summ = abs_summ,
    ext_summ = ext_summ
  ))
}

documentsServer <- function(input, output, session, values, statsValues) {
  ## DOCUMENTS ----

  ## STM prevalence covariate selector ----
  output$stmPrevalenceSelect <- renderUI({
    req(values$dfTag)
    all_cols <- noGroupLabels(names(values$dfTag))
    # Filter to numeric, date, or factor/character types suitable as covariates
    cov_cols <- sapply(all_cols, function(col) {
      col_data <- values$dfTag[[col]]
      is.numeric(col_data) ||
        inherits(col_data, c("Date", "POSIXct", "POSIXlt")) ||
        is.character(col_data) || is.factor(col_data)
    })
    cov_choices <- all_cols[cov_cols]

    selectInput(
      inputId = "stmPrevalence",
      label = "Prevalence Covariates:",
      choices = cov_choices,
      selected = NULL,
      multiple = TRUE,
      width = "100%"
    )
  })

  ## K selection UI for Model Estimation panel ----
  output$tmKSelectionUI <- renderUI({
    # Check if K estimation was already performed
    k_estimated <- !is.null(values$TMKresult)

    if (k_estimated) {
      # Compute consensus K from previous estimation
      consensus_k <- tmConsensusK(values$TMKresult$metrics, method = input$tmMethod)

      tagList(
        div(
          style = "padding: 10px; background-color: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 4px; margin-bottom: 10px;",
          icon("check-circle", style = "color: #4caf50;"),
          strong(" K estimation available"),
          p(
            paste0("Recommended K = ", consensus_k),
            style = "margin: 5px 0 0 0; color: #333; font-size: 13px;"
          )
        ),
        numericInput(
          "KEstim",
          label = "N. of Topics (K)",
          value = consensus_k,
          min = 2,
          step = 1
        )
      )
    } else {
      tagList(
        div(
          style = "padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px; margin-bottom: 10px;",
          icon("info-circle", style = "color: #856404;"),
          span(
            " No K estimation found. Run the K selection analysis first, or set K manually.",
            style = "color: #856404; font-size: 12px;"
          )
        ),
        numericInput(
          "KEstim",
          label = "N. of Topics (K)",
          value = 5,
          min = 2,
          step = 1
        )
      )
    }
  })

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
      filtered <- LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected)
      if (
        input$groupTm == "doc_id" & "ungroupDoc_id" %in% names(values$dfTag)
      ) {
        filtered <- backToOriginalGroups(filtered)
      }
      values$TMKresult <- tmTuning(
        filtered,
        group = groupTm,
        term = values$generalTerm,
        metric = input$metric,
        n = input$nTm,
        top_by = input$top_by,
        minK = input$minK,
        maxK = input$maxK,
        Kby = input$Kby,
        method = input$tmMethodK,
        prevalence = NULL,
        seed = values$random_seed
      )

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

  ## Multi-Metric Comparison Plot ----
  output$d_tm_multiMetricPlot <- renderPlotly({
    netTMKselect()
    req(values$TMKresult)
    tmMultiMetricPlot(values$TMKresult, method = input$tmMethodK)
  })

  ## K Recommendation Panel ----
  output$d_tm_kRecommendationUI <- renderUI({
    netTMKselect()
    req(values$TMKresult)

    metrics_df <- values$TMKresult$metrics
    is_stm <- input$tmMethodK == "STM"

    # Compute optimal K per metric
    if (is_stm) {
      metric_list <- list(
        list(name = "Exclusivity", col = "CaoJuan2009", decreasing = TRUE,
             desc = "Measures how exclusive top words are to each topic. Higher = more distinct topics."),
        list(name = "Semantic Coherence", col = "Arun2010", decreasing = FALSE,
             desc = "Measures co-occurrence of top words within topics. Less negative = more coherent."),
        list(name = "Excl. + Coherence", col = "Deveaud2014", decreasing = TRUE,
             desc = "Combined score balancing exclusivity and coherence. Higher = better trade-off."),
        list(name = "Lower Bound", col = "logLik", decreasing = FALSE,
             desc = "Variational lower bound on log-likelihood. Higher (less negative) = better fit.")
      )
    } else {
      metric_list <- list(
        list(name = "CaoJuan 2009", col = "CaoJuan2009", decreasing = TRUE,
             desc = "Average cosine similarity between topics. Lower = more distinct topics."),
        list(name = "Arun 2010", col = "Arun2010", decreasing = FALSE,
             desc = "KL divergence between word-topic and doc-topic distributions. Lower = better fit."),
        list(name = "Deveaud 2014", col = "Deveaud2014", decreasing = TRUE,
             desc = "Jensen-Shannon divergence between topic pairs. Lower = more separated topics."),
        list(name = "Perplexity", col = "Perplexity", decreasing = TRUE,
             desc = "Predictive performance on held-out data. Lower = better generalization.")
      )
    }

    # Find optimal K for each metric
    recommendations <- lapply(metric_list, function(m) {
      k_opt <- find_elbow(
        metrics_df$k,
        metrics_df[[m$col]],
        decreasing = m$decreasing,
        plot = FALSE
      )
      list(name = m$name, k = k_opt, desc = m$desc)
    })

    # Consensus K (mode of recommendations)
    k_votes <- sapply(recommendations, function(r) r$k)
    k_table <- sort(table(k_votes), decreasing = TRUE)
    consensus_k <- as.integer(names(k_table)[1])
    vote_count <- k_table[1]
    total_metrics <- length(recommendations)

    # Build UI cards
    rec_cards <- lapply(recommendations, function(r) {
      is_consensus <- (r$k == consensus_k)
      div(
        style = paste0(
          "padding: 15px; border-radius: 8px; margin-bottom: 10px; border-left: 5px solid ",
          if (is_consensus) "#4F7942" else "#667eea", "; background-color: #f8f9fa;"
        ),
        fluidRow(
          column(
            8,
            strong(r$name, style = "font-size: 15px;"),
            if (is_consensus) span(" (consensus)", style = "color: #4F7942; font-weight: 600; font-size: 12px;"),
            p(r$desc, style = "color: #666; font-size: 12px; margin: 5px 0 0 0;")
          ),
          column(
            4,
            div(
              style = "text-align: center; padding: 8px;",
              span(
                paste0("K = ", r$k),
                style = paste0(
                  "font-size: 24px; font-weight: 700; color: ",
                  if (is_consensus) "#4F7942" else "#667eea", ";"
                )
              )
            )
          )
        )
      )
    })

    tagList(
      # Consensus header
      div(
        style = "padding: 20px; background: linear-gradient(135deg, #4F7942 0%, #6CC283 100%); color: white; border-radius: 10px; margin-bottom: 20px; text-align: center;",
        h3(
          icon("bullseye"),
          paste0("Recommended K = ", consensus_k),
          style = "margin: 0; font-weight: 700;"
        ),
        p(
          paste0(
            "Consensus from ", vote_count, " out of ", total_metrics,
            " metrics | Method: ", input$tmMethodK
          ),
          style = "margin: 8px 0 0 0; opacity: 0.9;"
        ),
        if (as.integer(vote_count) < total_metrics) {
          p(
            paste0("All recommendations: K = ", paste(sort(unique(k_votes)), collapse = ", ")),
            style = "margin: 5px 0 0 0; opacity: 0.8; font-size: 13px;"
          )
        }
      ),

      # Per-metric recommendations
      rec_cards
    )
  })

  output$d_tm_selectTable <- renderDataTable({
    netTMKselect()
    DTformat(
      values$df,
      numeric = c(2, 3),
      round = 2,
      nrow = nrow(values$df),
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
      plot2png(values$TMKplot, filename = file, type = "plotly", dpi = values$dpi, height = values$h)
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
        type = "plotly",
        dpi = values$report_dpi, height = values$h
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
      filtered <- LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected)
      if (
        input$groupTmEstim == "doc_id" &
          "ungroupDoc_id" %in% names(values$dfTag)
      ) {
        tm_data <- backToOriginalGroups(filtered)
      } else {
        tm_data <- filtered
      }

      # Get prevalence covariates for STM
      stm_prevalence <- NULL
      if (input$tmMethod == "STM" && !is.null(input$stmPrevalence) && length(input$stmPrevalence) > 0) {
        stm_prevalence <- input$stmPrevalence
      }

      values$tmK <- input$KEstim
      values$TMplotList <- split(
        1:values$tmK,
        ceiling(seq_along(1:values$tmK) / 3)
      )
      values$TMestim_result <- tmEstimate(
        tm_data,
        K = values$tmK,
        group = groupTmEstim,
        term = values$generalTerm,
        n = input$nTmEstim,
        top_by = input$top_byEstim,
        method = input$tmMethod,
        prevalence = stm_prevalence,
        seed = values$random_seed
      )
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

  ## Model Diagnostics output ----
  output$d_tm_diagnosticsUI <- renderUI({
    netTMestim()
    req(values$TMestim_result)

    model <- values$TMestim_result$topicModel
    K <- values$tmK
    method <- input$tmMethod

    # Common metrics
    if (method == "STM") {
      # STM diagnostics
      semcoh <- stm::semanticCoherence(model, documents = values$TMestim_result$stm_documents)
      excl <- stm::exclusivity(model)
      diag_df <- data.frame(
        Topic = paste("Topic", 1:K),
        `Semantic Coherence` = round(semcoh, 3),
        Exclusivity = round(excl, 3),
        check.names = FALSE
      )
      bound <- round(max(model$convergence$bound), 2)
      its <- model$convergence$its
      convergence_text <- paste0(
        "Approximate variational lower bound: <strong>", bound, "</strong><br>",
        "Iterations to convergence: <strong>", its, "</strong>"
      )
    } else {
      # LDA / CTM diagnostics
      ll <- round(as.numeric(logLik(model)), 2)
      n_terms <- model@Dim[2]
      n_docs <- model@Dim[1]

      # Per-topic word entropy
      beta_mat <- exp(model@beta)
      entropy <- apply(beta_mat, 1, function(row) {
        p <- row[row > 0]
        -sum(p * log(p))
      })
      # Topic size (mean theta per topic)
      topic_prop <- colMeans(model@gamma)
      diag_df <- data.frame(
        Topic = paste("Topic", 1:K),
        `Topic Share` = paste0(round(topic_prop * 100, 1), "%"),
        `Word Entropy` = round(entropy, 3),
        `Top Word Prob.` = round(apply(beta_mat, 1, max), 4),
        check.names = FALSE
      )
      convergence_text <- paste0(
        "Log-Likelihood: <strong>", format(ll, big.mark = ","), "</strong><br>",
        "Documents: <strong>", n_docs, "</strong> | ",
        "Vocabulary: <strong>", n_terms, "</strong>"
      )
    }

    tagList(
      fluidRow(
        column(
          6,
          div(
            style = "padding: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 10px; margin-bottom: 20px;",
            h4(icon("chart-line"), strong(paste0("Model Diagnostics — ", method)),
              style = "margin-top: 0; color: white;"),
            h5(paste0("K = ", K, " topics"), style = "opacity: 0.9; color: white;")
          ),
          div(
            style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px; border-left: 4px solid #667eea; margin-bottom: 20px;",
            h5(strong("Global Metrics"), style = "margin-top: 0;"),
            HTML(convergence_text)
          )
        ),
        column(
          6,
          div(
            style = "padding: 15px; background-color: #fff; border: 1px solid #e0e0e0; border-radius: 8px;",
            h5(strong("Per-Topic Quality"), style = "margin-top: 0;"),
            DT::renderDT(
              DT::datatable(
                diag_df,
                options = list(dom = "t", pageLength = 50, scrollY = "350px"),
                rownames = FALSE,
                class = "compact stripe"
              )
            )
          )
        )
      ),
      if (method == "STM") {
        fluidRow(
          column(
            12,
            br(),
            div(
              style = "padding: 15px; background-color: #fff; border: 1px solid #e0e0e0; border-radius: 8px;",
              h5(strong("Semantic Coherence vs Exclusivity"), style = "margin-top: 0;"),
              renderPlotly({
                plot_ly(
                  diag_df,
                  x = ~`Semantic Coherence`,
                  y = ~Exclusivity,
                  text = ~Topic,
                  type = "scatter",
                  mode = "markers+text",
                  textposition = "top center",
                  marker = list(size = 12, color = "#667eea"),
                  textfont = list(size = 10, color = "gray40")
                ) %>%
                  layout(
                    xaxis = list(title = "Semantic Coherence"),
                    yaxis = list(title = "Exclusivity"),
                    paper_bgcolor = "white",
                    plot_bgcolor = "#fafafa"
                  ) %>%
                  config(displaylogo = FALSE)
              })
            )
          )
        )
      }
    )
  })

  ## Covariate Effects output (STM only) ----
  output$d_tm_covariateUI <- renderUI({
    netTMestim()
    req(values$TMestim_result)

    if (input$tmMethod != "STM") {
      return(
        div(
          style = "text-align: center; padding: 60px; color: #999;",
          icon("info-circle", style = "font-size: 48px; margin-bottom: 15px;"),
          h4("Covariate effects are available only for STM models."),
          p("Select STM (Structural) as the model type to use prevalence covariates.")
        )
      )
    }

    model <- values$TMestim_result$topicModel
    stm_meta <- values$TMestim_result$stm_meta
    K <- values$tmK
    prevalence_vars <- input$stmPrevalence

    if (is.null(prevalence_vars) || length(prevalence_vars) == 0 || is.null(stm_meta)) {
      return(
        div(
          style = "text-align: center; padding: 60px; color: #999;",
          icon("info-circle", style = "font-size: 48px; margin-bottom: 15px;"),
          h4("No prevalence covariates were specified."),
          p("Select covariates in the Options panel and re-run the analysis.")
        )
      )
    }

    # Rename metadata columns to safe names for estimateEffect
    safe_names <- gsub("[^a-zA-Z0-9_]", "_", prevalence_vars)
    meta_safe <- stm_meta
    for (i in seq_along(prevalence_vars)) {
      if (prevalence_vars[i] %in% names(meta_safe)) {
        names(meta_safe)[names(meta_safe) == prevalence_vars[i]] <- safe_names[i]
      }
    }

    # Build covariate effect plots
    cov_panels <- lapply(seq_along(prevalence_vars), function(idx) {
      cov_var <- prevalence_vars[idx]
      safe_var <- safe_names[idx]

      prep <- tryCatch({
        stm::estimateEffect(
          as.formula(paste("1:", K, "~", safe_var)),
          stmobj = model,
          metadata = meta_safe
        )
      }, error = function(e) NULL)

      if (is.null(prep)) {
        return(div(
          style = "padding: 20px; background-color: #fff3cd; border-radius: 8px; margin-bottom: 20px;",
          icon("exclamation-triangle", style = "color: #856404;"),
          strong(cov_var), ": unable to estimate covariate effects."
        ))
      }

      # Get covariate data to determine type
      cov_data <- meta_safe[[safe_var]]
      is_continuous <- is.numeric(cov_data)

      plot_output_id <- paste0("stm_cov_plot_", gsub("[^a-zA-Z0-9]", "_", cov_var))

      # Render the plot
      output[[plot_output_id]] <- renderPlot({
        if (is_continuous) {
          plot(
            prep,
            covariate = safe_var,
            topics = 1:K,
            model = model,
            method = "continuous",
            xlab = cov_var,
            main = paste0("Effect of ", cov_var, " on Topic Prevalence"),
            ci.level = 0.95,
            labeltype = "custom",
            custom.labels = paste("Topic", 1:K)
          )
        } else {
          plot(
            prep,
            covariate = safe_var,
            topics = 1:K,
            model = model,
            method = "pointestimate",
            xlab = "Expected Topic Proportion",
            main = paste0("Effect of ", cov_var, " on Topic Prevalence"),
            ci.level = 0.95,
            labeltype = "custom",
            custom.labels = paste("Topic", 1:K)
          )
        }
      }, height = max(400, K * 40))

      # Summary table
      summary_prep <- summary(prep)
      coef_list <- lapply(seq_len(K), function(t) {
        tbl <- summary_prep$tables[[t]]
        data.frame(
          Topic = paste("Topic", t),
          Coefficient = rownames(tbl),
          Estimate = round(tbl[, "Estimate"], 4),
          `Std. Error` = round(tbl[, "Std. Error"], 4),
          `t value` = round(tbl[, "t value"], 3),
          `Pr(>|t|)` = format.pval(tbl[, "Pr(>|t|)"], digits = 3),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      })
      coef_df <- do.call(rbind, coef_list)
      # Keep only rows related to this covariate (not intercept)
      coef_df <- coef_df[!grepl("^\\(Intercept\\)$", coef_df$Coefficient), ]

      coef_output_id <- paste0("stm_cov_table_", gsub("[^a-zA-Z0-9]", "_", cov_var))
      output[[coef_output_id]] <- DT::renderDT({
        DT::datatable(
          coef_df,
          options = list(dom = "t", pageLength = 100, scrollY = "300px"),
          rownames = FALSE,
          class = "compact stripe"
        )
      })

      # UI element for this covariate
      div(
        style = "margin-bottom: 30px;",
        div(
          style = "padding: 12px; background: linear-gradient(135deg, #4F7942 0%, #6CC283 100%); color: white; border-radius: 8px 8px 0 0;",
          h4(icon("chart-area"), strong(cov_var),
            if (is_continuous) span(" (continuous)", style = "opacity: 0.8; font-size: 13px; color: white;")
            else span(" (categorical)", style = "opacity: 0.8; font-size: 13px; color: white;"),
            style = "margin: 0; color: white;")
        ),
        fluidRow(
          column(
            7,
            div(
              style = "padding: 15px; background-color: #fff; border: 1px solid #e0e0e0; border-radius: 0 0 0 8px;",
              plotOutput(plot_output_id, height = paste0(max(400, K * 40), "px"))
            )
          ),
          column(
            5,
            div(
              style = "padding: 15px; background-color: #f8f9fa; border: 1px solid #e0e0e0; border-radius: 0 0 8px 0;",
              h5(strong("Regression Coefficients"), style = "margin-top: 0;"),
              DT::DTOutput(coef_output_id)
            )
          )
        )
      )
    })

    tagList(
      div(
        style = "padding: 20px; background: linear-gradient(135deg, #4F7942 0%, #6CC283 100%); color: white; border-radius: 10px; margin-bottom: 20px;",
        h4(icon("project-diagram"), strong("STM Covariate Effects"),
          style = "margin-top: 0; color: white;"),
        h5(paste0("Prevalence covariates: ", paste(prevalence_vars, collapse = ", ")),
          style = "opacity: 0.9; color: white;")
      ),
      cov_panels
    )
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
        width = values$h * values$aspect,
        bg = "transparent"
      )
      ggsave(
        filename = file2,
        plot = values$tmGplotBeta,
        dpi = values$dpi,
        height = values$h,
        width = values$h * values$aspect,
        bg = "transparent"
      )
      ggsave(
        filename = file3,
        plot = values$tmGplotTheta,
        dpi = values$dpi,
        height = values$h,
        width = values$h * values$aspect,
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
        dpi = values$report_dpi,
        startRow = nrow(Gem) + 1
      )
      values$wb <- wb
      popUp(title = "Model Estimation Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Syntactic Complexity ----

  syntacticFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$d_syntacticApply
    },
    valueExpr = {
      filtered <- values$dfTag %>% dplyr::filter(docSelected)
      values$syntacticResults <- computeSyntacticComplexity(filtered)
    }
  )

  output$d_syntacticTable <- renderDT(server = FALSE, {
    syntacticFunction()
    req(values$syntacticResults)
    df <- values$syntacticResults %>%
      rename(
        "Document" = doc_id,
        "Sentences" = n_sentences,
        "Mean Sent. Length" = mean_sent_length,
        "Mean Tree Depth" = mean_tree_depth,
        "Max Tree Depth" = max_tree_depth,
        "Mean Dep. Distance" = mean_dep_distance,
        "Clauses/Sent." = mean_clauses_per_sent,
        "Subordinate/Sent." = mean_subordinate,
        "Coordinate/Sent." = mean_coordinate,
        "Subordination Ratio" = subordination_ratio,
        "Branching Factor" = mean_branching_factor
      )
    DTformat(
      df,
      size = "100%",
      filename = "SyntacticComplexity",
      pagelength = TRUE,
      dom = TRUE,
      filter = "top"
    )
  })

  output$d_syntacticSummaryUI <- renderUI({
    syntacticFunction()
    req(values$syntacticResults)
    df <- values$syntacticResults
    n_docs <- nrow(df)

    metrics <- list(
      list(name = "Mean Sentence Length", val = mean(df$mean_sent_length), desc = "words per sentence (excl. punctuation)"),
      list(name = "Mean Tree Depth", val = mean(df$mean_tree_depth), desc = "levels of syntactic nesting"),
      list(name = "Mean Dependency Distance", val = mean(df$mean_dep_distance), desc = "tokens between head and dependent"),
      list(name = "Mean Clauses per Sentence", val = mean(df$mean_clauses_per_sent), desc = "main + subordinate + coordinate"),
      list(name = "Subordination Ratio", val = mean(df$subordination_ratio), desc = "proportion of subordinate clauses"),
      list(name = "Mean Branching Factor", val = mean(df$mean_branching_factor), desc = "children per non-leaf node")
    )

    cards <- lapply(metrics, function(m) {
      column(
        4,
        div(
          style = "padding: 20px; background-color: #f8f9fa; border-radius: 10px; border-left: 4px solid #4F7942; margin-bottom: 15px; text-align: center;",
          h3(strong(round(m$val, 2)), style = "color: #4F7942; margin: 0;"),
          h5(strong(m$name), style = "margin: 5px 0;"),
          p(m$desc, style = "color: #888; font-size: 12px; margin: 0;")
        )
      )
    })

    tagList(
      div(
        style = "padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 10px; margin-bottom: 20px;",
        h4(icon("chart-bar"), strong("Corpus-Level Syntactic Profile"),
          style = "margin-top: 0; color: white;"),
        h5(paste0(n_docs, " documents analyzed"), style = "opacity: 0.9; color: white;")
      ),
      fluidRow(cards[1:3]),
      fluidRow(cards[4:6])
    )
  })

  output$d_syntacticDistPlot <- renderPlotly({
    syntacticFunction()
    req(values$syntacticResults)
    df <- values$syntacticResults

    # Multi-panel distribution plot
    metrics_to_plot <- list(
      list(col = "mean_sent_length", title = "Sentence Length"),
      list(col = "mean_tree_depth", title = "Tree Depth"),
      list(col = "mean_dep_distance", title = "Dependency Distance"),
      list(col = "mean_clauses_per_sent", title = "Clauses per Sentence"),
      list(col = "subordination_ratio", title = "Subordination Ratio"),
      list(col = "mean_branching_factor", title = "Branching Factor")
    )

    colors <- c("#4575B4", "#4F7942", "#D73027", "#FF7F00", "#984EA3", "#E41A1C")

    fig <- plot_ly()
    for (i in seq_along(metrics_to_plot)) {
      m <- metrics_to_plot[[i]]
      fig <- fig %>%
        add_trace(
          x = df[[m$col]],
          type = "histogram",
          name = m$title,
          marker = list(color = colors[i], line = list(color = "white", width = 1)),
          opacity = 0.8,
          visible = if (i == 1) TRUE else "legendonly"
        )
    }

    values$syntacticDistPlot <- fig %>%
      layout(
        title = list(
          text = "<b>Distribution of Syntactic Complexity Metrics</b>",
          font = list(size = 16, color = "gray30"),
          x = 0.5
        ),
        xaxis = list(title = "Value"),
        yaxis = list(title = "Number of Documents"),
        barmode = "overlay",
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.12)
      ) %>%
      config(displaylogo = FALSE)

    values$syntacticDistPlot
  })

  ## Syntactic Complexity Export (PNG)
  observeEvent(input$d_syntacticExport, {
    req(values$syntacticDistPlot)
    file <- paste("SyntacticComplexity-", sys.time(), ".png", sep = "")
    file <- destFolder(file, values$wdTall)
    plot2png(values$syntacticDistPlot, filename = file, type = "plotly", dpi = values$dpi, height = values$h)
    popUp(title = "Saved in your working folder", type = "saved")
  })

  ## Syntactic Complexity Report
  observeEvent(input$d_syntacticReport, {
    if (!is.null(values$syntacticResults) && nrow(values$syntacticResults) > 0) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "SyntacticComplexity"
      list_df <- list(values$syntacticResults)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileSyntacticDist <- plot2png(
        values$syntacticDistPlot,
        filename = "SyntacticDist.png",
        type = "plotly",
        dpi = values$report_dpi, height = values$h
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileSyntacticDist, res$col)
      )
      popUp(title = "Syntactic Complexity Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## SVO Triplet Extraction ----

  svoFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$d_svoApply
    },
    valueExpr = {
      filtered <- LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected)

      values$svoResults <- extractSVO(
        filtered,
        term = values$generalTerm,
        freq.min = input$svoFreqMin
      )

      if (nrow(values$svoResults) > 0) {
        # Top N for display
        values$svoTop <- values$svoResults %>%
          slice_head(n = input$svoTopN)
      }
    }
  )

  output$d_svoTable <- renderDT(server = FALSE, {
    svoFunction()
    req(values$svoResults)
    DTformat(
      values$svoResults %>%
        rename(
          Subject = subject,
          Verb = verb,
          Object = object,
          Frequency = freq,
          "Relation Type" = rel_type
        ),
      size = "100%",
      filename = "SVO_Triplets",
      pagelength = TRUE,
      dom = TRUE,
      filter = "top"
    )
  })

  output$d_svoNetwork <- renderPlotly({
    svoFunction()
    req(values$svoTop)
    df <- values$svoTop %>% filter(object != "")

    if (nrow(df) == 0) return(plotly_empty())

    # Build a Sankey-like network: Subject -> Verb -> Object
    subjects <- unique(df$subject)
    verbs <- unique(df$verb)
    objects <- unique(df$object[df$object != ""])
    all_nodes <- c(subjects, verbs, objects)

    # Source: subject -> verb
    sv_source <- match(df$subject, all_nodes) - 1
    sv_target <- match(df$verb, all_nodes) - 1
    sv_value <- df$freq

    # Target: verb -> object
    vo_source <- match(df$verb, all_nodes) - 1
    vo_target <- match(df$object, all_nodes) - 1
    vo_value <- df$freq

    # Node colors
    node_colors <- c(
      rep("#4575B4", length(subjects)),
      rep("#4F7942", length(verbs)),
      rep("#D73027", length(objects))
    )

    values$svoSankeyPlot <- plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = all_nodes,
        color = node_colors,
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5)
      ),
      link = list(
        source = c(sv_source, vo_source),
        target = c(sv_target, vo_target),
        value = c(sv_value, vo_value),
        color = "rgba(200,200,200,0.4)"
      )
    ) %>%
      layout(
        title = list(
          text = "<b>SVO Triplet Flow</b><br><sub>Subject (blue) -> Verb (green) -> Object (red)</sub>",
          font = list(size = 16, color = "gray30"),
          x = 0.5
        ),
        font = list(size = 11),
        paper_bgcolor = "white"
      ) %>%
      config(displaylogo = FALSE)

    values$svoSankeyPlot
  })

  output$d_svoVerbPlot <- renderPlotly({
    svoFunction()
    req(values$svoResults)

    verb_freq <- values$svoResults %>%
      group_by(verb) %>%
      summarise(freq = sum(freq), .groups = "drop") %>%
      arrange(desc(freq)) %>%
      slice_head(n = 30) %>%
      mutate(verb = factor(verb, levels = rev(verb)))

    values$svoVerbPlot <- plot_ly(
      verb_freq,
      x = ~freq,
      y = ~verb,
      type = "bar",
      orientation = "h",
      marker = list(color = "#4F7942")
    ) %>%
      layout(
        title = list(
          text = "<b>Most Frequent Verbs in SVO Triplets</b>",
          font = list(size = 16, color = "gray30"),
          x = 0.5
        ),
        xaxis = list(title = "Frequency"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        margin = list(l = 120)
      ) %>%
      config(displaylogo = FALSE)

    values$svoVerbPlot
  })

  ## SVO Export (PNG images)
  observeEvent(input$d_svoExport, {
    req(values$svoResults)
    file1 <- paste("SVO-Network-", sys.time(), ".png", sep = "")
    file1 <- destFolder(file1, values$wdTall)
    file2 <- paste("SVO-VerbFreq-", sys.time(), ".png", sep = "")
    file2 <- destFolder(file2, values$wdTall)
    plot2png(values$svoSankeyPlot, filename = file1, type = "plotly", dpi = values$dpi, height = values$h)
    plot2png(values$svoVerbPlot, filename = file2, type = "plotly", dpi = values$dpi, height = values$h)
    popUp(title = "Saved in your working folder", type = "saved")
  })

  ## SVO Report (tables + images)
  observeEvent(input$d_svoReport, {
    if (!is.null(values$svoResults) && nrow(values$svoResults) > 0) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "SVOTriplets"
      list_df <- list(values$svoResults)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileSVOSankey <- plot2png(
        values$svoSankeyPlot,
        filename = "SVONetwork.png",
        type = "plotly",
        dpi = values$report_dpi, height = values$h
      )
      values$fileSVOVerb <- plot2png(
        values$svoVerbPlot,
        filename = "SVOVerbFreq.png",
        type = "plotly",
        dpi = values$report_dpi, height = values$h
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileSVOSankey, res$col),
        c(sheetname = res$sheetname, values$fileSVOVerb, res$col)
      )
      popUp(title = "SVO Triplet Results", type = "success")
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

      plot2png(values$sentimentPieChart, filename = file1, type = "plotly", dpi = values$dpi, height = values$h)
      plot2png(
        values$sentimentDensityPlot,
        filename = file2,
        type = "plotly", dpi = values$dpi, height = values$h
      )
      plot2png(values$sentimentBoxPlot, filename = file3, type = "plotly", dpi = values$dpi, height = values$h)
      plot2png(
        values$docPolPlots$positive,
        filename = file4,
        type = "plotly", dpi = values$dpi, height = values$h
      )
      plot2png(
        values$docPolPlots$negative,
        filename = file5,
        type = "plotly", dpi = values$dpi, height = values$h
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
        type = "plotly", dpi = values$report_dpi, height = values$h
      )
      values$fileDensityPlot <- plot2png(
        values$sentimentDensityPlot,
        filename = files[2],
        type = "plotly", dpi = values$report_dpi, height = values$h
      )
      values$fileBoxPlot <- plot2png(
        values$sentimentBoxPlot,
        filename = files[3],
        type = "plotly", dpi = values$report_dpi, height = values$h
      )
      values$filedocPolPos <- plot2png(
        values$docPolPlots$positive,
        filename = files[4],
        type = "plotly", dpi = values$report_dpi, height = values$h
      )
      values$filedocPolNeg <- plot2png(
        values$docPolPlots$negative,
        filename = files[5],
        type = "plotly", dpi = values$report_dpi, height = values$h
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

  ## Emotion Analysis ----

  docEmotionEstim <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$d_emoApply
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
        ## check to verify if groups exist or not
        if (
          input$groupEmotion == "doc_id" &
            "ungroupDoc_id" %in% names(values$dfTag)
        ) {
          values$docEmotionData <- emotionAnalysis(
            backToOriginalGroups(values$dfTag) %>% filter(docSelected),
            language = values$language
          )
        } else {
          values$docEmotionData <- emotionAnalysis(
            values$dfTag %>% filter(docSelected),
            language = values$language
          )
        }

        if (!is.list(values$docEmotionData)) return(NULL)

        values$emotionBarChart <- emotionBarChart(
          values$docEmotionData$corpus_emotions
        )
        values$emotionHeatmap <- emotionHeatmap(
          values$docEmotionData$doc_emotions_long
        )

        # Prepare table data
        emotion_names <- c(
          "anger", "anticipation", "disgust", "fear",
          "joy", "sadness", "surprise", "trust"
        )
        values$docEmotionTableData <- values$docEmotionData$doc_emotions %>%
          select(doc_id, all_of(emotion_names), total) %>%
          rename(Total = total)
      }
    }
  )

  output$d_emoBarPlot <- renderPlotly({
    docEmotionEstim()
    values$emotionBarChart
  })

  output$d_emoWordPlot <- renderPlotly({
    docEmotionEstim()
    req(values$docEmotionData)
    emotion_sel <- input$d_emoSelectEmotion
    if (is.null(emotion_sel)) emotion_sel <- "joy"
    emotionWordPlot(values$docEmotionData$word_emotions, emotion_sel, n = 10)
  })

  output$d_emoHeatmap <- renderPlotly({
    docEmotionEstim()
    values$emotionHeatmap
  })

  output$d_emo_GeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "Gemini AI", content = values$d_emo_Gemini, values)
  })

  output$d_emoTable <- renderDT(server = FALSE, {
    docEmotionEstim()
    DTformat(
      values$docEmotionTableData,
      filename = "DocEmotion",
      numeric = 2:9,
      round = 0
    )
  })

  observeEvent(
    eventExpr = {
      input$d_emoExport
    },
    handlerExpr = {
      file1 <- paste("EmotionDistribution-", sys.time(), ".png", sep = "")
      file1 <- destFolder(file1, values$wdTall)
      file2 <- paste("EmotionHeatmap-", sys.time(), ".png", sep = "")
      file2 <- destFolder(file2, values$wdTall)

      plot2png(values$emotionBarChart, filename = file1, type = "plotly", dpi = values$dpi, height = values$h)
      plot2png(values$emotionHeatmap, filename = file2, type = "plotly", dpi = values$dpi, height = values$h)

      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Emotion Report
  observeEvent(input$d_emoReport, {
    if (!is.null(values$docEmotionTableData)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "EmotionAnalysis"

      Gem <- values$d_emo_Gemini %>% string_to_sentence_df()

      list_df <- list(Gem, values$docEmotionTableData)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- c(
        "EmotionDistribution.png",
        "EmotionHeatmap.png"
      )
      values$fileEmotionBar <- plot2png(
        values$emotionBarChart,
        filename = files[1],
        type = "plotly", dpi = values$report_dpi, height = values$h
      )
      values$fileEmotionHeatmap <- plot2png(
        values$emotionHeatmap,
        filename = files[2],
        type = "plotly", dpi = values$report_dpi, height = values$h
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileEmotionBar, res$col),
        c(sheetname = res$sheetname, values$fileEmotionHeatmap, res$col)
      )
      popUp(title = "Emotion Analysis Results", type = "success")
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

overviewUI <- function() {
  ### OVERVIEW ----
  tabItem(
    tabName = "overview",
    fluidPage(
      fluidRow(
        tabsetPanel(
          type = "tabs",
          id = "maininfo",
          # OVERVIEW TAB - NEW LAYOUT
          tabPanel(
            title = "Overview", icon = icon("chart-column"),
            fluidRow(
              column(
                11,
                h2(icon("chart-pie"), strong("Overview"), style = "color: #4F7942; text-align: center; margin-bottom: 20px;")
              ),
              div(
                title = t_report,
                style = style_opt,
                column(
                  1,
                  do.call(
                    "actionButton",
                    c(
                      report_bttn,
                      list(
                        inputId = "overviewReport"
                      )
                    )
                  )
                )
              )
            ),

            # GROUP 1: CORPUS SIZE & STRUCTURE
            fluidRow(
              column(
                12,
                div(
                  class = "group-header group-header-green",
                  icon("folder-open", class = "fa-lg"),
                  " Corpus Size & Structure"
                )
              )
            ),
            fluidRow(
              class = "overview-row",
              tagList(
                useShinyjs(),
                column(
                  2,
                  offset = 0,
                  div(
                    id = "clickbox1",
                    title = "Number of Documents",
                    valueBoxOutput("nDoc", width = NULL)
                  )
                ),
                column(
                  2,
                  offset = 0,
                  div(
                    id = "clickbox4",
                    title = "Number of Sentences",
                    valueBoxOutput("nSentences", width = NULL)
                  )
                ),
                column(
                  3,
                  offset = 0,
                  div(
                    id = "clickbox8",
                    title = "Number of Tokens",
                    valueBoxOutput("nTokens", width = NULL)
                  )
                ),
                column(
                  3,
                  offset = 0,
                  div(
                    id = "clickbox7",
                    title = "Number of Types",
                    valueBoxOutput("nDictionary", width = NULL)
                  )
                ),
                column(
                  2,
                  offset = 0,
                  div(
                    id = "clickbox9",
                    title = "Number of Lemma",
                    valueBoxOutput("nLemmas", width = NULL)
                  )
                )
              )
            ),

            # GROUP 2: AVERAGE LENGTH METRICS
            fluidRow(
              column(
                12,
                div(
                  class = "group-header group-header-blue",
                  style = "margin-top: 20px;",
                  icon("text-height", class = "fa-lg"),
                  " Average Length Metrics"
                )
              )
            ),
            fluidRow(
              class = "overview-row",
              tagList(
                column(
                  3,
                  div(
                    id = "clickbox2",
                    title = "Average Document's Length by Characters",
                    valueBoxOutput("avgDocLengthChar", width = NULL)
                  )
                ),
                column(
                  3,
                  div(
                    id = "clickbox3",
                    title = "Average Document's Length by Tokens",
                    valueBoxOutput("avgDocLengthTokens", width = NULL)
                  )
                ),
                column(
                  3,
                  div(
                    id = "clickbox5",
                    title = "Average Sentence's Length by Characters",
                    valueBoxOutput("avgSentLengthChar", width = NULL)
                  )
                ),
                column(
                  3,
                  div(
                    id = "clickbox6",
                    title = "Average Sentence's Length by Tokens",
                    valueBoxOutput("avgSentLengthTokens", width = NULL)
                  )
                )
              )
            ),

            # GROUP 3: LEXICAL METRICS (TUTTE E 7 SULLA STESSA RIGA)
            fluidRow(
              column(
                12,
                div(
                  class = "group-header group-header-orange",
                  style = "margin-top: 20px;",
                  icon("bar-chart", class = "fa-lg"),
                  " Lexical Metrics"
                )
              )
            ),
            div(
              class = "lexical-row",
              tagList(
                div(
                  id = "clickbox10",
                  title = "Type-Token Ratio",
                  valueBoxOutput("TTR", width = NULL)
                ),
                div(
                  id = "clickbox11",
                  title = "Percentage of Hapax Legomena",
                  valueBoxOutput("hapax", width = NULL)
                ),
                div(
                  id = "clickbox12",
                  title = "Guiraud Index",
                  valueBoxOutput("guiraud", width = NULL)
                ),
                div(
                  id = "clickbox13",
                  title = "Lexical Density",
                  valueBoxOutput("lexicalDensity", width = NULL)
                ),
                div(
                  id = "clickbox14",
                  title = "Nominal Ratio",
                  valueBoxOutput("nominalRatio", width = NULL)
                ),
                div(
                  id = "clickbox15",
                  title = "Gini Index",
                  valueBoxOutput("giniIndex", width = NULL)
                ),
                div(
                  id = "clickbox16",
                  title = "Yule's K Index",
                  valueBoxOutput("yuleK", width = NULL)
                )
              )
            )
          ),
          tabPanel(
            title = "Table", icon = icon("table"),
            div(
              shinycssloaders::withSpinner(
                DT::DTOutput(outputId = "overviewData", width = 700),
                color = getOption("spinner.color", default = "#4F7942")
              ),
              align = "center"
            )
          ),
          tabPanel(
            title = "Vocabulary", icon = icon("book"),
            column(
              12,
              div(
                shinycssloaders::withSpinner(
                  DT::DTOutput(outputId = "dictionaryData", width = 700),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              align = "center"
            )
            # ,column(1)
          ),
          tabPanel(
            title = "TF-IDF", icon = icon("table"),
            column(
              12,
              div(
                shinycssloaders::withSpinner(
                  DT::DTOutput(outputId = "tfidfData", width = 700),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              align = "center"
            )
          ),
          tabPanel(
            title = "WordCloud", icon = icon("cloud"),
            fluidPage(
              fluidRow(
                column(
                  8,
                  h2(icon("cloud"), strong("WordCloud"), style = "color: #4F7942; text-align: center; margin-bottom: 20px;")
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
                          inputId = "wcApply"
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
                          inputId = "wcSave"
                        )
                      )
                    ),
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
                          inputId = "wcReport"
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
                        numericInput(
                          "nWC",
                          label = "Number of words",
                          value = 100,
                          min = 10,
                          max = 500,
                          step = 1
                        ),
                        numericInput(
                          "labelsizeWC",
                          label = "Text Size",
                          value = 5,
                          min = 1,
                          max = 20,
                          step = 1
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
                br(),
                shinycssloaders::withSpinner(
                  plotOutput(
                    outputId = "wordcloudPlot",
                    height = "700px",
                    width = "100%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              )
            )
          ),
          tabPanel(
            title = "Frequency", icon = icon("chart-bar"),
            fluidPage(
              fluidRow(
                column(
                  8,
                  h2(icon("chart-bar"), strong("Word Frequency by PoS"), style = "color: #4F7942; text-align: center; margin-bottom: 20px;")
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
                          inputId = "wFreqApply"
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
                          inputId = "wFreqExport"
                        )
                      )
                    ),
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
                          inputId = "wFreqReport"
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
                        numericInput(
                          "wFreqN",
                          label = "Number of words",
                          value = 20,
                          min = 1,
                          step = 1
                        ),
                        uiOutput("posSelectionFreq"),
                        helpText("Select a PoS category (upos) from the options before running the analysis.")
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
                column(
                  6,
                  shinycssloaders::withSpinner(
                    plotlyOutput(
                      outputId = "wFreqPlot",
                      height = "75vh",
                      width = "98.9%"
                    ),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                ),
                column(
                  6,
                  br(),
                  shinycssloaders::withSpinner(
                    DT::DTOutput("wFreqTable", width = "90.0%"),
                    color = getOption("spinner.color", default = "#4F7942")
                  ),
                  align = "center"
                )
              )
            )
          ),
          tabPanel(
            title = "Morphological Features", icon = icon("language"),
            fluidPage(
              fluidRow(
                column(
                  12,
                  h2(icon("puzzle-piece"), strong("Morphological Features"), style = "color: #4F7942; text-align: center; margin-bottom: 20px;"),
                  p(
                    "Distribution of morphological features extracted from the Universal Dependencies annotation.",
                    style = "text-align: center; color: #666; margin-bottom: 20px;"
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  uiOutput("morphFeatureDesc")
                )
              ),
              fluidRow(
                column(
                  3,
                  selectInput(
                    "morphFeature",
                    "Select Feature:",
                    choices = c(
                      "Tense" = "Tense",
                      "Mood" = "Mood",
                      "Number" = "Number",
                      "Person" = "Person",
                      "VerbForm" = "VerbForm",
                      "Degree" = "Degree",
                      "Gender" = "Gender",
                      "Case" = "Case",
                      "Voice" = "Voice",
                      "Definite" = "Definite",
                      "PronType" = "PronType"
                    ),
                    selected = "Tense"
                  )
                ),
                column(
                  9,
                  shinycssloaders::withSpinner(
                    plotlyOutput(
                      outputId = "morphFeaturePlot",
                      height = "55vh",
                      width = "98.9%"
                    ),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  br(),
                  h5(strong("Cross-tabulation: Feature Values by Part of Speech"),
                    style = "color: #555; margin-bottom: 10px;"),
                  shinycssloaders::withSpinner(
                    DT::DTOutput("morphFeatureTable"),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                )
              )
            )
          ),
          tabPanel(
            title = "Dependency Tree", icon = icon("sitemap"),
            fluidPage(
              fluidRow(
                column(
                  9,
                  h2(icon("diagram-project"), strong("Dependency Tree Viewer"), style = "color: #4F7942; text-align: center; margin-bottom: 20px;")
                ),
                div(
                  title = t_export,
                  column(
                    1,
                    do.call(
                      "actionButton",
                      c(
                        export_bttn,
                        list(inputId = "depTreeExport")
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
                        list(inputId = "depTreeReport")
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  3,
                  textInput(
                    "depTreeSearch",
                    "Search word:",
                    value = "",
                    placeholder = "Type a word to filter sentences..."
                  )
                ),
                column(
                  3,
                  uiOutput("depTreeDocSelect")
                ),
                column(
                  3,
                  uiOutput("depTreeSentSelect")
                ),
                column(
                  3,
                  div(
                    style = "margin-top: 25px;",
                    actionBttn(
                      inputId = "depTreeApply",
                      label = "Show Tree",
                      style = "bordered",
                      color = "success",
                      size = "sm",
                      icon = icon("sitemap")
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  uiOutput("depTreeSentenceText")
                )
              ),
              fluidRow(
                column(
                  12,
                  br(),
                  shinycssloaders::withSpinner(
                    uiOutput("depTreePlotUI"),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  br(),
                  h5(strong("Token Details"), style = "color: #555; margin-bottom: 10px;"),
                  shinycssloaders::withSpinner(
                    DT::DTOutput("depTreeTable"),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                )
              )
            )
          ),
          tabPanel(
            title = "TALL AI", icon = icon("robot"),
            fluidPage(
              fluidRow(
                column(
                  12,
                  br(),
                  shinycssloaders::withSpinner(
                    htmlOutput("OverviewGeminiUI"),
                    caption = HTML("<br><strong>Thinking...</strong>"),
                    image = "ai_small2.gif",
                    color = "#4F7942"
                  )
                )
              )
            )
          ),
          tabPanel(
            title = "Info & References", icon = icon("circle-info"),
            fluidPage(
              fluidRow(
                column(1),
                column(
                  10,
                  br(),
                  HTML(infoTexts$overview)
                ),
                column(1)
              )
            )
          )
        ) # , align="center"
      )
    )
  )
}

overviewServer <- function(input, output, session, values, statsValues) {
  ## BOX ----

  # Eagerly recompute value box indices whenever dfTag changes (e.g. after filtering)
  observe({
    req(values$dfTag)
    req("docSelected" %in% names(values$dfTag))
    values$vb <- valueBoxesIndices(values$dfTag %>% filter(docSelected))
    values$VbData <- data.frame(
      Description = c(
        "Documents",
        "Tokens",
        "Types",
        "Lemma",
        "Sentences",
        "Docs Avg Length in Chars",
        "Docs SD Length in Chars",
        "Doc Avg Length in Tokens",
        "Doc SD Length in Tokens",
        "Sent Avg Length in Tokens",
        "Sent SD Length in Tokens",
        "Sent Avg Length in Chars",
        "Sent SD Length in Chars",
        "TTR (%)",
        "Hapax (%)",
        "Guiraud Index",
        "Lexical Density",
        "Nominal Ratio",
        "Gini Index",
        "Yule's K"
      ),
      Values = unlist(values$vb)
    )
  })

  # ============================================
  # CORPUS SIZE & STRUCTURE (GREEN)
  # ============================================

  #### box1 - Documents ---------------
  output$nDoc <- renderValueBox({
    valueBox(
      value = strong(values$vb$nDoc),
      subtitle = "Documents",
      icon = icon("file-text", class = "fa-2x", lib = "font-awesome"),
      color = "green",
      width = NULL
    )
  })

  #### box4 - Sentences ---------------
  output$nSentences <- renderValueBox({
    valueBox(
      value = strong(values$vb$nSentences),
      subtitle = "Sentences",
      icon = icon("align-left", class = "fa-2x"),
      color = "green",
      width = NULL
    )
  })

  #### box8 - Tokens ---------------
  output$nTokens <- renderValueBox({
    valueBox(
      value = strong(values$vb$nTokens),
      subtitle = "Tokens",
      icon = icon("font", class = "fa-2x"),
      color = "green",
      width = NULL
    )
  })

  #### box7 - Types ----------------
  output$nDictionary <- renderValueBox({
    valueBox(
      value = strong(values$vb$nDictionary),
      subtitle = "Types",
      icon = icon("rectangle-list", class = "fa-2x"),
      color = "green",
      width = NULL
    )
  })

  #### box9 - Lemma ---------------
  output$nLemmas <- renderValueBox({
    valueBox(
      value = strong(values$vb$nLemmas),
      subtitle = "Lemma",
      icon = icon("book", class = "fa-2x", lib = "font-awesome"),
      color = "green",
      width = NULL
    )
  })

  # ============================================
  # AVERAGE LENGTH METRICS (BLUE)
  # ============================================

  #### box2 - Doc Avg Length in Chars ---------------
  output$avgDocLengthChar <- renderValueBox({
    valueBox(
      value = strong(paste0(
        values$vb$avgDocLengthChars,
        " ± ",
        values$vb$avgDocLengthCharsSD
      )),
      subtitle = "Doc Avg Length in Chars",
      icon = icon("rectangle-list", class = "fa-2x"),
      color = "blue",
      width = NULL
    )
  })

  #### box3 - Doc Avg Length in Tokens ------------
  output$avgDocLengthTokens <- renderValueBox({
    valueBox(
      value = strong(paste0(
        values$vb$avgDocLengthTokens,
        " ± ",
        values$vb$avgDocLengthTokensSD
      )),
      subtitle = "Doc Avg Length in Tokens",
      icon = icon("text-height", class = "fa-2x", lib = "font-awesome"),
      color = "blue",
      width = NULL
    )
  })

  #### box5 - Sent Avg Length in Chars --------------------
  output$avgSentLengthChar <- renderValueBox({
    valueBox(
      value = strong(paste0(
        values$vb$avgSentLengthChars,
        " ± ",
        values$vb$avgSentLengthCharsSD
      )),
      subtitle = "Sent Avg Length in Chars",
      icon = icon("text-width", class = "fa-2x", lib = "font-awesome"),
      color = "blue",
      width = NULL
    )
  })

  #### box6 - Sent Avg Length in Tokens -------------
  output$avgSentLengthTokens <- renderValueBox({
    valueBox(
      value = strong(paste0(
        values$vb$avgSentLengthTokens,
        " ± ",
        values$vb$avgSentLengthTokensSD
      )),
      subtitle = "Sent Avg Length in Tokens",
      icon = icon("align-justify", class = "fa-2x"),
      color = "blue",
      width = NULL
    )
  })

  # ============================================
  # LEXICAL METRICS - TUTTE ARANCIONE
  # ============================================

  #### box10 - TTR ------------------
  output$TTR <- renderValueBox({
    valueBox(
      value = strong(values$vb$TTR),
      subtitle = "TTR (%)",
      icon = icon("percent", class = "fa-2x", lib = "font-awesome"),
      color = "orange",
      width = NULL
    )
  })

  #### box11 - Hapax ------
  output$hapax <- renderValueBox({
    valueBox(
      value = strong(round(values$vb$hapax, 0)),
      subtitle = "Hapax (%)",
      icon = icon("star", class = "fa-2x", lib = "font-awesome"),
      color = "orange",
      width = NULL
    )
  })

  #### box12 - Guiraud -------
  output$guiraud <- renderValueBox({
    valueBox(
      value = strong(values$vb$guiraud),
      subtitle = "Guiraud Index",
      icon = icon("bar-chart", class = "fa-2x", lib = "font-awesome"),
      color = "orange",
      width = NULL
    )
  })

  #### box13 - Lexical Density -------
  output$lexicalDensity <- renderValueBox({
    valueBox(
      value = strong(round(values$vb$lexical_density, 1)),
      subtitle = "Lexical Density",
      icon = icon("tachometer", class = "fa-2x", lib = "font-awesome"),
      color = "orange",
      width = NULL
    )
  })

  #### box14 - Nominal Ratio -------
  output$nominalRatio <- renderValueBox({
    valueBox(
      value = strong(round(values$vb$nominal_ratio, 2)),
      subtitle = "Nominal Ratio",
      icon = icon("balance-scale", class = "fa-2x", lib = "font-awesome"),
      color = "orange",
      width = NULL
    )
  })

  #### box15 - Gini Index -------
  output$giniIndex <- renderValueBox({
    valueBox(
      value = strong(round(values$vb$gini_index, 2)),
      subtitle = "Gini Index",
      icon = icon("signal", class = "fa-2x", lib = "font-awesome"),
      color = "orange",
      width = NULL
    )
  })

  #### box16 - Yule's K -------
  output$yuleK <- renderValueBox({
    valueBox(
      value = strong(round(values$vb$yule_k, 1)),
      subtitle = "Yule's K",
      icon = icon("line-chart", class = "fa-2x", lib = "font-awesome"),
      color = "orange",
      width = NULL
    )
  })
  ## Overview Table ----

  output$overviewData <- renderDT(server = FALSE, {
    DTformat(
      values$VbData,
      nrow = nrow(values$VbData),
      left = 1,
      right = 2,
      numeric = 2,
      pagelength = FALSE,
      dom = FALSE,
      size = "110%",
      filename = "Overview",
      col_to_remove = values$generalTerm
    )
  })

  output$OverviewGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "Gemini AI", content = values$overviewGemini, values)
  })

  ## Report

  observeEvent(input$overviewReport, {
    if (!is.null(values$VbData)) {
      sheetname <- "Overview"
      list_df <- list(values$VbData)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      values$wb <- res$wb
      popUp(title = "Overview Table", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }

    if (!is.null(values$dfTag) && "feats" %in% names(values$dfTag)) {
      morphFeatures <- c("Tense", "Mood", "Number", "Person", "VerbForm",
                          "Degree", "Gender", "Case", "Voice", "Definite", "PronType")
      morphData <- lapply(morphFeatures, function(feat) {
        vals <- parseMorphFeatures(values$dfTag$feats, feat)
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) > 0) {
          tab <- sort(table(vals), decreasing = TRUE)
          data.frame(Feature = feat, Value = names(tab), Count = as.integer(tab), stringsAsFactors = FALSE)
        }
      })
      morphData <- do.call(rbind, morphData)
      if (!is.null(morphData) && nrow(morphData) > 0) {
        list_df <- list(morphData)
        res <- addDataScreenWb(list_df, wb = values$wb, sheetname = "Morphological Features")
        values$wb <- res$wb
        values$myChoices <- sheets(values$wb)
      }
    }
  })

  ## WORDCLOUD ----

  wcData <- eventReactive(
    {
      input$wcApply
    },
    {
      N <- input$nWC # showing the first 100 lemma
      filtered <- LemmaSelection(values$dfTag) %>%
        dplyr::filter(docSelected)
      pos <- unique(filtered$upos)

      values$wcDfPlot <- freqByPos(
        filtered,
        term = values$generalTerm,
        pos = pos
      ) %>%
        slice_head(n = N) %>%
        rename(
          label = term,
          value = n
        )

      set.seed(values$random_seed)
      values$WCplot <- wordcloud(
        values$wcDfPlot,
        shape = "circle",
        rot_per = 0.2,
        eccentricity = 1.5,
        colors = sample(colorlist(), nrow(values$wcDfPlot), replace = T),
        seed = values$random_seed,
        max_size = input$labelsizeWC * 10
      )
    }
  )

  output$wordcloudPlot <- renderPlot({
    wcData()
    values$WCplot
  })

  ## export WordCloud button
  observeEvent(
    eventExpr = {
      input$wcSave
    },
    handlerExpr = {
      file <- paste("Wordcloud-", sys.time(), ".png", sep = "")
      file_path <- destFolder(file, values$wdTall)
      ggsave(
        filename = file_path,
        plot = values$WCplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * values$aspect,
        bg = "transparent"
      )

      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report
  observeEvent(input$wcReport, {
    if (!is.null(values$wcDfPlot)) {
      list_df <- list(values$wcDfPlot)
      list_plot <- list(values$WCplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "WordCloud",
        wb = values$wb,
        dpi = values$report_dpi
      )
      values$wb <- wb
      popUp(
        title = paste0("WordCloud"),
        type = "success"
      )
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## VOCABULARY ----
  output$dictionaryData <- renderDT(server = FALSE, {
    # dictionary()
    values$dictFreq <- vocabulary(values$dfTag, term = values$generalTerm)
    DTformat(
      values$dictFreq,
      left = c(1, 2),
      nrow = 15,
      pagelength = TRUE,
      filename = "Dictionary",
      dom = TRUE,
      size = "110%",
      col_to_remove = values$generalTerm
    )
  })

  ## TF-IDF Table----
  output$tfidfData <- renderDT(server = FALSE, {
    values$tfidfDATA <- tfidfTable(values$dfTag, term = values$generalTerm)
    DTformat(
      values$tfidfDATA,
      left = 1,
      numeric = 2,
      round = 4,
      size = "110%",
      col_to_remove = values$generalTerm
    )
  })

  ## Frequency List ----

  ## Words Frequency by PoS ----

  output$posSelectionFreq <- renderUI({
    selectInput(
      inputId = "posSelectionFreq",
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
      req(input$posSelectionFreq)
      values$wFreq <- freqByPos(
        values$dfTag %>% filter(docSelected),
        term = values$generalTerm,
        pos = input$posSelectionFreq
      )
      values$wFreqPlotly <- freqPlotly(
        values$wFreq,
        x = "n",
        y = "term",
        n = input$wFreqN,
        xlabel = "Frequency",
        ylabel = input$posSelectionFreq,
        scale = "identity"
      )

      values$wFreqData <- values$wFreq %>%
        rename(
          Word = term,
          Frequency = n
        )
    }
  )

  output$wFreqPlot <- renderPlotly({
    wFreq()
    values$wFreqPlotly
  })

  output$wFreqTable <- renderDT(server = FALSE, {
    wFreq()
    DTformat(
      values$wFreqData,
      nrow = 15,
      left = 1,
      right = 2,
      round = 0,
      numeric = 2,
      filename = "WordsFreqList",
      dom = FALSE,
      size = "90%",
      filter = "none",
      col_to_remove = values$generalTerm
    )
  })

  observeEvent(
    eventExpr = {
      input$wFreqExport
    },
    handlerExpr = {
      file <- paste(
        "WordsFrequency-",
        input$posSelectionFreq,
        "-",
        sys.time(),
        ".png",
        sep = ""
      )
      file_path <- destFolder(file, values$wdTall)
      g = values$wFreq
      save(g, file = "progaggplot.rdata")
      values$wFreqGgplot <- freqGgplot(
        values$wFreq,
        x = 2,
        y = 1,
        n = input$wFreqN,
        title = paste0("Words Frequency by ", input$posSelectionFreq)
      )
      ggsave(
        filename = file_path,
        plot = values$wFreqGgplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * values$aspect,
        bg = "transparent"
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report
  observeEvent(input$wFreqReport, {
    if (!is.null(values$wFreq)) {
      values$wFreqGgplot <- freqGgplot(
        values$wFreq,
        x = 2,
        y = 1,
        n = input$wFreqN,
        title = paste0("Words Frequency by ", input$posSelectionFreq)
      )
      list_df <- list(values$wFreqData)
      list_plot <- list(values$wFreqGgplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "WordsFreq",
        wb = values$wb,
        dpi = values$report_dpi
      )
      values$wb <- wb
      popUp(
        title = paste0("Words Frequency by-", input$posSelectionFreq),
        type = "success"
      )
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
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
        dplyr::filter(!upos %in% c("PUNCT", "SYM", "NUM", "NGRAM_MERGED")) %>%
        group_by(upos) %>%
        count() %>%
        arrange(desc(n)) %>%
        rename(PoS = upos)

      values$posPlotly <- freqPlotly(
        values$freqPOS,
        x = "n",
        y = "PoS",
        n = nrow(values$freqPOS),
        xlabel = "Frequency",
        ylabel = "Part of Speech",
        scale = "identity"
      )

      values$freqPOSData <- values$freqPOS %>%
        rename(Frequency = n)
    }
  )

  output$posPlot <- renderPlotly({
    posFreq()
    values$posPlotly
  })

  output$posTable <- renderDT(server = FALSE, {
    posFreq()
    DTformat(
      values$freqPOSData,
      left = 1,
      right = 2,
      round = 0,
      numeric = 2,
      filename = "POSFreqList",
      dom = FALSE,
      size = "110%",
      col_to_remove = values$generalTerm
    )
  })

  observeEvent(
    eventExpr = {
      input$posExport
    },
    handlerExpr = {
      file <- paste("PoSFrequency-", sys.time(), ".png", sep = "")
      file <- destFolder(file, values$wdTall)
      values$posGgplot <- freqGgplot(
        data.frame(values$freqPOS),
        x = 2,
        y = 1,
        n = length(values$freqPOS$PoS),
        title = "PoS Frequency"
      )
      ggsave(
        filename = file,
        plot = values$posGgplot,
        dpi = values$dpi,
        height = values$h,
        width = values$h * values$aspect,
        bg = "transparent"
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$posReport, {
    values$posGgplot <- freqGgplot(
      data.frame(values$freqPOS),
      x = 2,
      y = 1,
      n = length(values$freqPOS$PoS),
      title = "PoS Tag Frequency"
    )
    if (!is.null(values$freqPOS)) {
      list_df <- list(values$freqPOSData)
      list_plot <- list(values$posGgplot)
      wb <- addSheetToReport(
        list_df,
        list_plot,
        sheetname = "PoSFreq",
        wb = values$wb,
        dpi = values$report_dpi
      )
      values$wb <- wb
      popUp(title = "PoS Tag Frequency", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Dependency Tree Viewer ----

  # Reactive: filtered data based on search word
  depTreeFiltered <- reactive({
    req(values$dfTag)
    df <- values$dfTag %>% filter(docSelected)
    search <- trimws(tolower(input$depTreeSearch))
    if (!is.null(search) && nchar(search) > 0) {
      # Find doc_id + sentence_id pairs containing the search word
      matching <- df %>%
        filter(tolower(token) == search | tolower(lemma) == search) %>%
        distinct(doc_id, sentence_id)
      df <- df %>% semi_join(matching, by = c("doc_id", "sentence_id"))
    }
    df
  })

  output$depTreeDocSelect <- renderUI({
    df <- depTreeFiltered()
    docs <- unique(df$doc_id)
    if (length(docs) == 0) {
      return(div(
        style = "margin-top: 25px; color: #856404; font-size: 13px;",
        icon("exclamation-triangle"), " No matching sentences found."
      ))
    }
    selectInput(
      "depTreeDoc",
      "Document:",
      choices = docs,
      selected = docs[1],
      width = "100%"
    )
  })

  output$depTreeSentSelect <- renderUI({
    req(input$depTreeDoc)
    df <- depTreeFiltered()
    sents <- df %>%
      filter(doc_id == input$depTreeDoc) %>%
      pull(sentence_id) %>%
      unique() %>%
      sort()
    if (length(sents) == 0) return(NULL)

    # Build preview labels: sentence_id + first words
    sent_labels <- sapply(sents, function(s) {
      tokens <- df %>%
        filter(doc_id == input$depTreeDoc, sentence_id == s) %>%
        pull(token)
      preview <- paste(head(tokens, 8), collapse = " ")
      if (length(tokens) > 8) preview <- paste0(preview, "...")
      paste0("S", s, ": ", preview)
    })
    names(sents) <- sent_labels

    selectInput(
      "depTreeSent",
      "Sentence:",
      choices = sents,
      selected = sents[1],
      width = "100%"
    )
  })

  depTreeData <- eventReactive(input$depTreeApply, {
    req(input$depTreeDoc, input$depTreeSent)
    values$dfTag %>%
      filter(
        doc_id == input$depTreeDoc,
        sentence_id == as.integer(input$depTreeSent),
        !upos %in% c("NGRAM_MERGED")
      ) %>%
      mutate(
        tid = as.integer(token_id),
        hid = as.integer(head_token_id)
      ) %>%
      select(tid, hid, token, lemma, upos, dep_rel)
  })

  output$depTreeSentenceText <- renderUI({
    df <- depTreeData()
    req(nrow(df) > 0)
    sentence <- paste(df$token, collapse = " ")
    div(
      style = "padding: 12px 15px; background-color: #f8f9fa; border-left: 4px solid #4F7942; border-radius: 4px; margin-top: 10px; font-size: 15px; line-height: 1.6;",
      icon("quote-left", style = "color: #4F7942; margin-right: 5px;"),
      sentence
    )
  })

  output$depTreePlotUI <- renderUI({
    df <- depTreeData()
    req(nrow(df) > 0)
    n_tokens <- nrow(df)
    # Min 80px per token, min total 800px
    plot_width <- max(800, n_tokens * 80)
    plot_height <- max(400, min(700, n_tokens * 12 + 200))
    div(
      style = paste0("overflow-x: auto; overflow-y: hidden; border: 1px solid #eee; border-radius: 8px; background: white; padding: 10px;"),
      plotOutput(
        outputId = "depTreePlot",
        height = paste0(plot_height, "px"),
        width = paste0(plot_width, "px")
      )
    )
  })

  output$depTreePlot <- renderPlot({
    df <- depTreeData()
    req(nrow(df) > 0)

    n <- nrow(df)
    # x positions = token order
    df$x <- seq_len(n)

    # Build arc data: from dependent to head
    arcs <- df %>%
      filter(hid > 0) %>%
      mutate(
        x_from = x,
        x_to = match(hid, df$tid)
      ) %>%
      filter(!is.na(x_to))

    if (nrow(arcs) == 0) return(NULL)

    # Arc height proportional to distance
    arcs$height <- abs(arcs$x_from - arcs$x_to) * 0.5
    arcs$xmid <- (arcs$x_from + arcs$x_to) / 2

    # Color by relation category
    rel_colors <- c(
      nsubj = "#E41A1C", `nsubj:pass` = "#E41A1C", csubj = "#E41A1C",
      obj = "#377EB8", dobj = "#377EB8", iobj = "#377EB8",
      amod = "#4DAF4A", nmod = "#4DAF4A", `nmod:poss` = "#4DAF4A",
      advmod = "#FF7F00",
      det = "#999999", case = "#999999",
      compound = "#984EA3", flat = "#984EA3", `flat:name` = "#984EA3",
      conj = "#A65628", cc = "#A65628",
      punct = "#CCCCCC",
      root = "#000000"
    )
    arcs$color <- ifelse(
      arcs$dep_rel %in% names(rel_colors),
      rel_colors[arcs$dep_rel],
      "#666666"
    )

    # PoS colors for tokens
    pos_colors <- c(
      NOUN = "#4F7942", PROPN = "#2E5A1E",
      VERB = "#E41A1C", AUX = "#FB9A99",
      ADJ = "#377EB8", ADV = "#FF7F00",
      DET = "#999999", ADP = "#999999",
      PRON = "#984EA3", CCONJ = "#A65628", SCONJ = "#A65628",
      PUNCT = "#CCCCCC", NUM = "#666666"
    )
    df$pos_color <- ifelse(
      df$upos %in% names(pos_colors),
      pos_colors[df$upos],
      "#666666"
    )

    # Build arc curves manually
    arc_curves <- do.call(rbind, lapply(seq_len(nrow(arcs)), function(i) {
      x1 <- arcs$x_from[i]
      x2 <- arcs$x_to[i]
      h <- arcs$height[i]
      t_seq <- seq(0, pi, length.out = 50)
      data.frame(
        arc_id = i,
        x = x1 + (x2 - x1) * (1 - cos(t_seq)) / 2,
        y = h * sin(t_seq),
        dep_rel = arcs$dep_rel[i],
        color = arcs$color[i],
        stringsAsFactors = FALSE
      )
    }))

    # Build plot
    p <- ggplot() +
      # Arcs
      geom_path(
        data = arc_curves,
        aes(x = x, y = y, group = arc_id, color = color),
        linewidth = 0.7,
        show.legend = FALSE
      ) +
      scale_color_identity() +
      # Arc labels (dep_rel)
      geom_text(
        data = arcs,
        aes(x = xmid, y = height + 0.15, label = dep_rel),
        size = 2.8,
        color = arcs$color,
        fontface = "bold"
      ) +
      # Token boxes
      geom_label(
        data = df,
        aes(x = x, y = -0.3, label = token),
        fill = "white",
        color = df$pos_color,
        label.size = 0.5,
        size = 3.5,
        fontface = "bold",
        label.padding = unit(0.25, "lines")
      ) +
      # PoS labels
      geom_text(
        data = df,
        aes(x = x, y = -0.7, label = upos),
        size = 2.5,
        color = df$pos_color
      ) +
      # Styling
      theme_void() +
      theme(
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "white", color = NA)
      ) +
      coord_cartesian(
        ylim = c(-1, max(arcs$height) + 0.8),
        clip = "off"
      )

    values$depTreeGgplot <- p
    p
  })

  ## Dep Tree Export (PNG)
  observeEvent(input$depTreeExport, {
    req(values$depTreeGgplot)
    df <- depTreeData()
    n_tokens <- nrow(df)
    w <- min(49, max(10, n_tokens * 1.2))
    h <- max(5, min(10, n_tokens * 0.15 + 3))
    file <- paste("DependencyTree-", sys.time(), ".png", sep = "")
    file <- destFolder(file, values$wdTall)
    ggsave(
      filename = file,
      plot = values$depTreeGgplot,
      dpi = values$dpi,
      width = w,
      height = h,
      bg = "white",
      limitsize = FALSE
    )
    popUp(title = "Saved in your working folder", type = "saved")
  })

  ## Dep Tree Report
  observeEvent(input$depTreeReport, {
    req(values$depTreeGgplot)
    df <- depTreeData()
    req(nrow(df) > 0)
    popUp(title = NULL, type = "waiting")

    display_df <- df %>%
      select(tid, token, lemma, upos, dep_rel, hid) %>%
      left_join(
        df %>% select(tid, token) %>% rename(head_token = token),
        by = c("hid" = "tid")
      ) %>%
      mutate(head_token = ifelse(hid == 0, "ROOT", head_token))

    sheetname <- "DependencyTree"
    n_tokens <- nrow(df)
    w <- max(10, n_tokens * 1.2)
    h <- max(5, min(10, n_tokens * 0.15 + 3))
    list_df <- list(display_df)
    list_plot <- list(values$depTreeGgplot)
    wb <- addSheetToReport(
      list_df,
      list_plot,
      sheetname = sheetname,
      wb = values$wb,
      dpi = values$report_dpi
    )
    values$wb <- wb
    popUp(title = "Dependency Tree Results", type = "success")
    values$myChoices <- sheets(values$wb)
  })

  output$depTreeTable <- renderDT(server = FALSE, {
    df <- depTreeData()
    req(nrow(df) > 0)

    display_df <- df %>%
      select(tid, token, lemma, upos, dep_rel, hid) %>%
      left_join(
        df %>% select(tid, token) %>% rename(head_token = token),
        by = c("hid" = "tid")
      ) %>%
      mutate(head_token = ifelse(hid == 0, "ROOT", head_token)) %>%
      rename(
        "ID" = tid,
        "Token" = token,
        "Lemma" = lemma,
        "PoS" = upos,
        "Relation" = dep_rel,
        "Head ID" = hid,
        "Head Token" = head_token
      )

    DTformat(
      display_df,
      size = "100%",
      filename = "DependencyTree",
      dom = FALSE
    )
  })

  ## Morphological Features ----

  # Human-readable labels for morphological feature values
  morphLabels <- list(
    Tense = c(Past = "Past", Pres = "Present", Fut = "Future", Imp = "Imperfect", Pqp = "Pluperfect"),
    Mood = c(Ind = "Indicative", Sub = "Subjunctive", Imp = "Imperative", Cnd = "Conditional"),
    Number = c(Sing = "Singular", Plur = "Plural", Dual = "Dual"),
    Person = c("1" = "1st Person", "2" = "2nd Person", "3" = "3rd Person"),
    VerbForm = c(Fin = "Finite", "Inf" = "Infinitive", Part = "Participle", Ger = "Gerund", Conv = "Converb", Sup = "Supine"),
    Degree = c(Pos = "Positive", Cmp = "Comparative", Sup = "Superlative", Abs = "Absolute Superlative"),
    Gender = c(Masc = "Masculine", Fem = "Feminine", Neut = "Neuter", Com = "Common"),
    Case = c(Nom = "Nominative", Acc = "Accusative", Gen = "Genitive", Dat = "Dative", Ins = "Instrumental", Loc = "Locative", Voc = "Vocative", Abl = "Ablative"),
    Voice = c(Act = "Active", Pass = "Passive", Mid = "Middle"),
    Definite = c(Def = "Definite", Ind = "Indefinite", Com = "Complex", Cons = "Construct"),
    PronType = c(Prs = "Personal", Dem = "Demonstrative", Rel = "Relative", Int = "Interrogative", Ind = "Indefinite", Neg = "Negative", Tot = "Total", Art = "Article", Exc = "Exclamative")
  )

  morphDescriptions <- c(
    Tense = "Verb tense distribution. Reveals whether the corpus is predominantly narrative (Past), descriptive/argumentative (Present), or forward-looking (Future).",
    Mood = "Verbal mood distribution. Indicative conveys facts; Subjunctive signals uncertainty, doubt, or desire; Imperative expresses commands; Conditional marks hypothetical situations.",
    Number = "Grammatical number distribution across nouns, pronouns, adjectives, and verbs.",
    Person = "Verb person distribution. 1st person signals personal/subjective style; 3rd person indicates impersonal/objective style; 2nd person suggests direct address.",
    VerbForm = "Verb form distribution. The ratio of finite to non-finite forms (infinitives, participles, gerunds) reflects syntactic complexity.",
    Degree = "Adjective/adverb degree distribution. High use of comparatives and superlatives may indicate evaluative or argumentative discourse.",
    Gender = "Grammatical gender distribution (language-dependent). Available for languages with grammatical gender marking.",
    Case = "Grammatical case distribution (language-dependent). Available for languages with case systems (e.g., German, Russian, Latin).",
    Voice = "Active vs passive voice distribution. High passive usage may indicate formal, scientific, or bureaucratic writing style.",
    Definite = "Definiteness marking. The ratio of definite to indefinite articles/determiners reveals information structure patterns.",
    PronType = "Pronoun and determiner type distribution. Shows the balance between personal, demonstrative, relative, and other pronoun types."
  )

  parseMorphFeatures <- function(feats_col, feature_name) {
    # Parse feats column (format: "Number=Plur|Tense=Past|Mood=Ind")
    # Extract values for the requested feature
    pattern <- paste0("(?:^|\\|)", feature_name, "=([^|]+)")
    matches <- regmatches(feats_col, regexpr(pattern, feats_col, perl = TRUE))
    values <- sub(paste0(".*", feature_name, "="), "", matches)
    values[matches == ""] <- NA
    # Handle non-matching rows
    result <- rep(NA_character_, length(feats_col))
    has_match <- nchar(matches) > 0
    result[has_match] <- values[has_match]
    return(result)
  }

  output$morphFeatureDesc <- renderUI({
    feature <- input$morphFeature
    desc <- morphDescriptions[feature]
    if (is.na(desc)) desc <- ""
    div(
      style = "padding: 10px; background-color: #f0f7ee; border-left: 4px solid #4F7942; border-radius: 4px; margin-bottom: 15px;",
      icon("info-circle", style = "color: #4F7942;"),
      span(desc, style = "color: #333; font-size: 13px;")
    )
  })

  output$morphFeaturePlot <- renderPlotly({
    req(values$dfTag)
    feature <- input$morphFeature

    df <- values$dfTag %>%
      dplyr::filter(docSelected) %>%
      mutate(feat_value = parseMorphFeatures(feats, feature)) %>%
      filter(!is.na(feat_value))

    if (nrow(df) == 0) return(plotly_empty())

    # Apply human-readable labels
    labels <- morphLabels[[feature]]
    if (!is.null(labels)) {
      df$feat_label <- ifelse(
        df$feat_value %in% names(labels),
        labels[df$feat_value],
        df$feat_value
      )
    } else {
      df$feat_label <- df$feat_value
    }

    freq_table <- df %>%
      count(feat_label) %>%
      arrange(desc(n)) %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        feat_label = factor(feat_label, levels = rev(feat_label))
      )

    values$morphPlot <- plot_ly(
      freq_table,
      x = ~n,
      y = ~feat_label,
      type = "bar",
      orientation = "h",
      marker = list(color = "#4F7942"),
      text = ~paste0(n, " (", pct, "%)"),
      textposition = "auto",
      hovertemplate = "<b>%{y}</b><br>Count: %{x}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = paste0("<b>", feature, " Distribution</b>"),
          font = list(size = 16, color = "gray30"),
          x = 0.5
        ),
        xaxis = list(title = "Count"),
        yaxis = list(title = ""),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        margin = list(l = 150)
      ) %>%
      config(displaylogo = FALSE)

    values$morphPlot
  })

  output$morphFeatureTable <- renderDT(server = FALSE, {
    req(values$dfTag)
    feature <- input$morphFeature

    df <- values$dfTag %>%
      dplyr::filter(docSelected) %>%
      mutate(feat_value = parseMorphFeatures(feats, feature)) %>%
      filter(!is.na(feat_value))

    if (nrow(df) == 0) return(NULL)

    # Apply human-readable labels
    labels <- morphLabels[[feature]]
    if (!is.null(labels)) {
      df$feat_label <- ifelse(
        df$feat_value %in% names(labels),
        paste0(labels[df$feat_value], " (", df$feat_value, ")"),
        df$feat_value
      )
    } else {
      df$feat_label <- df$feat_value
    }

    # Cross-tabulation: feature value x PoS
    cross_tab <- df %>%
      count(feat_label, upos) %>%
      pivot_wider(names_from = upos, values_from = n, values_fill = 0) %>%
      mutate(Total = rowSums(across(where(is.numeric)))) %>%
      arrange(desc(Total)) %>%
      rename(!!feature := feat_label)

    DTformat(
      cross_tab,
      size = "100%",
      filename = paste0("MorphFeature_", feature)
    )
  })
}

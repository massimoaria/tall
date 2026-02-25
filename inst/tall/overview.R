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
            "Overview",
            fluidRow(
              column(
                11,
                h3(strong("Overview"), align = "center")
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
            "Table",
            div(
              shinycssloaders::withSpinner(
                DT::DTOutput(outputId = "overviewData", width = 700),
                color = getOption("spinner.color", default = "#4F7942")
              ),
              align = "center"
            )
          ),
          tabPanel(
            "Vocabulary",
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
            "TF-IDF",
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
            "WordCloud",
            fluidPage(
              fluidRow(
                column(
                  8,
                  h3(strong("WordCloud"), align = "center")
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
            "Frequency",
            fluidPage(
              fluidRow(
                column(
                  8,
                  h3(strong("Word Frequency by PoS"), align = "center")
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
                        uiOutput("posSelectionFreq")
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
            "TALL AI",
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
            "Info & References",
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
        width = values$h * 2,
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
        wb = values$wb
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
        width = values$h * 2,
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
        wb = values$wb
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
        width = values$h * 2,
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
        wb = values$wb
      )
      values$wb <- wb
      popUp(title = "PoS Tag Frequency", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })
}

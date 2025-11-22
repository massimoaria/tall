## Keyness Analysis UI ----

keynessUI <- function() {
  tabItem(
    tabName = "keyness",
    fluidPage(
      fluidRow(
        column(
          8,
          h3(strong("Keyness Analysis"), align = "center")
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
                  inputId = "run_keyness"
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
                  inputId = "keynessExport"
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
                  inputId = "keynessReport"
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
                  inputId = "keyness_approach",
                  label = "Analysis Approach:",
                  choices = c(
                    "Reference Corpus" = "reference_corpus",
                    "Two Corpus Comparison" = "two_corpus"
                  ),
                  selected = "reference_corpus"
                ),
                conditionalPanel(
                  condition = "input.keyness_approach == 'two_corpus'",
                  uiOutput("keyness_group_warning")
                ),
                numericInput(
                  inputId = "keyness_n",
                  label = "Max Number of Terms:",
                  value = 1000,
                  min = 100,
                  max = 10000,
                  step = 100
                ),
                numericInput(
                  inputId = "keyness_minchar",
                  label = "Min Character Length:",
                  value = 3,
                  min = 1,
                  max = 10,
                  step = 1
                ),
                selectInput(
                  inputId = "keyness_measure",
                  label = "Keyness Measure:",
                  choices = c(
                    "G2" = "G2",
                    "LogOddsRatio" = "LogOddsRatio",
                    "Phi" = "phi",
                    "Delta P" = "DeltaP"
                  ),
                  selected = "G2"
                ),
                checkboxGroupInput(
                  inputId = "keyness_upos",
                  label = "POS Tags:",
                  choices = c("NOUN", "VERB", "ADJ", "ADV"),
                  selected = c("NOUN", "VERB"),
                  inline = FALSE
                )
              ),
              # Graphical Parameters Section
              tags$details(
                class = "params-section",
                tags$summary(
                  div(
                    class = "params-section-header",
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    div(
                      icon("eye-open", lib = "glyphicon"),
                      " Graphical Parameters"
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
                  h4("Number of Words:"),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        "Keyness_Nbarplot",
                        label = "BarPlot",
                        value = 10,
                        min = 1,
                        max = 20,
                        step = 1
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        "Keyness_Nwc",
                        label = "Wordcloud",
                        value = 100,
                        min = 10,
                        step = 1,
                        max = 200
                      )
                    )
                  )
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
            br(),
            title = "Plot",
            icon = icon("chart-column"),

            shinycssloaders::withSpinner(
              plotlyOutput(
                outputId = "keyness_barplot_plotly",
                height = "75vh",
                width = "95.0%"
              ),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          tabPanel(
            br(),
            title = "WordCloud",
            icon = icon("chart-column"),
            # Use renderUI to dynamically create wordcloud containers
            shinycssloaders::withSpinner(
              uiOutput("keyness_wordcloud_ui"),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          tabPanel(
            br(),
            title = "Frequency Context Plot",
            icon = icon("chart-column"),
            conditionalPanel(
              condition = "input.keyness_approach == 'reference_corpus'",
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "keyness_frequency_plotly",
                  height = "80vh",
                  width = "95.0%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            conditionalPanel(
              condition = "input.keyness_approach == 'two_corpus'",
              div(
                style = "padding: 50px; text-align: center;",
                h4(
                  "Frequency Context Plot is only available for Reference Corpus approach"
                ),
                p(
                  "This plot compares your corpus against a general reference corpus.",
                  style = "color: #666; margin-top: 20px;"
                )
              )
            )
          ),
          tabPanel(
            title = "Table",
            icon = icon("table"),
            shinycssloaders::withSpinner(
              DT::DTOutput("keyness_table"),
              color = getOption("spinner.color", default = "#4F7942")
            ),
            align = "center"
          ),
          tabPanel(
            "Info & References",
            fluidPage(
              fluidRow(
                column(1),
                column(
                  10,
                  div(
                    style = "padding: 30px; background: white; border-radius: 8px;
                         box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-top: 20px;",
                    HTML(infoTexts$keyness)
                  )
                ),
                column(1)
              )
            )
          )
        )
      )
    )
  )
}

## Keyness Analysis Server ----

keynessServer <- function(input, output, session, values) {
  # Reactive counter to force re-rendering
  render_counter <- reactiveVal(0)

  # Check if keyness_group exists when two_corpus approach is selected
  output$keyness_group_warning <- renderUI({
    req(input$keyness_approach == "two_corpus")

    if (!"keyness_group" %in% names(values$dfTag)) {
      div(
        style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107;
                 border-radius: 4px; margin: 10px 0;",
        icon("exclamation-triangle", style = "color: #856404;"),
        span(
          style = "color: #856404; margin-left: 5px;",
          strong("Warning:"),
          " The 'keyness_group' variable is not defined. Please define it in the Feature Roles menu before running the analysis."
        )
      )
    }
  })

  # Keyness Analysis
  keyness_results <- eventReactive(
    input$run_keyness,
    {
      # Get approach
      approach <- input$keyness_approach

      # Increment counter to force re-rendering
      render_counter(render_counter() + 1)

      # Check if keyness_group exists for two_corpus approach
      if (
        approach == "two_corpus" && !"keyness_group" %in% names(values$dfTag)
      ) {
        showNotification(
          "Error: 'keyness_group' variable not found. Please define it in Feature Roles menu.",
          type = "error",
          duration = 10
        )
        return(NULL)
      }

      withProgress(message = 'Running Keyness Analysis...', value = 0, {
        incProgress(0.3, detail = "Calculating frequencies...")

        # Run keyness analysis with selected approach
        results <- tall_keyness_analysis(
          dfTag = values$dfTag,
          approach = approach,
          language = values$language,
          N = input$keyness_n,
          min.char = input$keyness_minchar,
          upos_list = input$keyness_upos
        )

        incProgress(0.7, detail = "Generating plots...")
        if (is.null(input$keyness_measure)) {
          measure = "G2"
        } else {
          measure = input$keyness_measure
        }
        if (is.null(input$Keyness_Nbarplot)) {
          Nbarplot = 10
        } else {
          Nbarplot = input$Keyness_Nbarplot
        }

        # Generate bar plot with approach parameter
        results <- c(
          results,
          plot_tall_keyness(
            results$results,
            measure = measure,
            N = Nbarplot,
            approach = approach
          )
        )

        # Generate frequency context plot only for reference_corpus approach
        if (approach == "reference_corpus") {
          plot_frequency <- frequency_context_analysis(
            results$results,
            top_n = 20,
            g2_threshold = quantile(results$results$G2, 0.95),
            label_spacing = 0.15
          )

          results <- c(
            results,
            plot_frequency = list(plot_frequency)
          )
        }

        if (is.null(input$Keyness_Nwc)) {
          Nwc = 100
        } else {
          Nwc = input$Keyness_Nwc
        }

        # Generate wordcloud(s) based on approach
        if (approach == "reference_corpus") {
          # Single wordcloud for reference corpus approach
          data_target <- results$results %>%
            ungroup() %>%
            select(token, all_of(measure)) %>%
            arrange(desc(.data[[measure]])) %>%
            slice_head(n = Nwc) %>%
            rename(word = token, freq = all_of(measure))
          results <- c(
            results,
            wc_data = list(data_target)
          )
        } else if (approach == "two_corpus") {
          # For two corpus: create static wordcloud data

          # Try to split by measure sign first
          data_positive <- results$results %>%
            ungroup() %>%
            filter(.data[[measure]] > 0) %>%
            select(token, all_of(measure)) %>%
            arrange(desc(.data[[measure]])) %>%
            slice_head(n = Nwc)

          data_negative <- results$results %>%
            ungroup() %>%
            filter(.data[[measure]] < 0) %>%
            select(token, all_of(measure)) %>%
            arrange(.data[[measure]]) %>%
            slice_head(n = Nwc) %>%
            mutate(across(all_of(measure), abs))

          # Fallback: if one corpus is empty, split top/bottom
          if (nrow(data_positive) == 0 || nrow(data_negative) == 0) {
            all_data <- results$results %>%
              ungroup() %>%
              select(token, all_of(measure)) %>%
              arrange(desc(.data[[measure]]))

            half_n <- min(Nwc, ceiling(nrow(all_data) / 2))

            data_positive <- all_data %>%
              slice_head(n = half_n)

            data_negative <- all_data %>%
              slice_tail(n = half_n) %>%
              mutate(across(all_of(measure), abs))
          }

          # Prepare final data frames
          data_corpus1 <- data_positive %>%
            rename(word = token, freq = all_of(measure))

          data_corpus2 <- data_negative %>%
            rename(word = token, freq = all_of(measure))

          results <- c(
            results,
            wc1_data = list(data_corpus1),
            wc2_data = list(data_corpus2)
          )
        }

        # Format results table
        results$results <- results$results %>%
          rename(Word = token, Obs_Freq = O11, Exp_Freq = O12) %>%
          select(
            Word,
            Sig_corrected,
            Obs_Freq,
            Exp_Freq,
            G2,
            RDF,
            RateRatio,
            OddsRatio,
            LogOddsRatio,
            phi,
            MI,
            PMI,
            DeltaP
          ) %>%
          mutate(
            G2 = round(G2, 3),
            RDF = round(RDF, 3),
            RateRatio = round(RateRatio, 3),
            OddsRatio = round(OddsRatio, 3),
            LogOddsRatio = round(LogOddsRatio, 3),
            phi = round(phi, 3),
            MI = round(MI, 3),
            PMI = round(PMI, 3),
            DeltaP = round(DeltaP, 3)
          )
      })
      values$keyness_results <- results
    },
    ignoreNULL = TRUE
  )

  # Keyness Table Output
  output$keyness_table <- DT::renderDT({
    req(keyness_results())
    DTformat(values$keyness_results$results)
  })

  # Keyness Bar Plot plotly
  output$keyness_barplot_plotly <- plotly::renderPlotly({
    req(keyness_results())
    values$keyness_results$plot_plotly_bar
  })

  output$keyness_frequency_plotly <- plotly::renderPlotly({
    req(keyness_results())
    req(input$keyness_approach == "reference_corpus")
    values$keyness_results$plot_frequency
  })

  # Dynamic UI for wordcloud based on approach
  output$keyness_wordcloud_ui <- renderUI({
    req(keyness_results())
    approach <- input$keyness_approach
    counter <- render_counter() # Track counter for invalidation

    if (approach == "reference_corpus") {
      # Single interactive wordcloud for reference corpus
      fluidRow(
        h4(
          "Target Corpus:High Keyness Words",
          align = "center",
          style = "color: #4F7942; margin-bottom: 20px;"
        ),
        plotOutput(
          outputId = "keyness_wordcloud_plot",
          height = "600px",
          width = "100%"
        )
      )
    } else if (approach == "two_corpus") {
      # Two static wordcloud plots side by side
      fluidRow(
        column(
          6,
          h4(
            "Corpus 1",
            align = "center",
            style = "color: #4575B4; margin-bottom: 20px;"
          ),
          plotOutput(
            outputId = "keyness_wordcloud_plot1",
            height = "600px",
            width = "100%"
          )
        ),
        column(
          6,
          h4(
            "Corpus 2",
            align = "center",
            style = "color: #D73027; margin-bottom: 20px;"
          ),
          plotOutput(
            outputId = "keyness_wordcloud_plot2",
            height = "600px",
            width = "100%"
          )
        )
      )
    }
  })

  output$keyness_wordcloud_plot <- renderPlot(
    {
      req(keyness_results())
      req(input$keyness_approach == "reference_corpus")

      # Make counter a direct dependency to force re-render
      counter <- render_counter()

      req(values$keyness_results$wc_data)
      data <- values$keyness_results$wc_data

      # Debug message
      message(sprintf(
        "Rendering Target Corpus wordcloud: %d words, counter: %d",
        nrow(data),
        counter
      ))

      # Load wordcloud package for static plots
      if (!requireNamespace("wordcloud", quietly = TRUE)) {
        plot.new()
        text(
          0.5,
          0.5,
          "Please install 'wordcloud' package:\ninstall.packages('wordcloud')",
          cex = 1.2,
          col = "red"
        )
        return()
      }

      if (nrow(data) == 0) {
        plot.new()
        text(
          0.5,
          0.5,
          "No keywords found for Target Corpus",
          cex = 1.5,
          col = "gray"
        )
        return()
      }

      # Use wordcloud package for static plot
      tryCatch(
        {
          #par(mar = c(0, 0, 0, 0), bg = "white")
          set.seed(123 + counter)
          wordcloud::wordcloud(
            words = data$word,
            freq = data$freq,
            min.freq = 1,
            max.words = nrow(data),
            random.order = FALSE,
            rot.per = 0.35,
            colors = colorlist(),
            scale = c(15, 0.5),
            family = "sans"
          )
        },
        error = function(e) {
          plot.new()
          text(
            0.5,
            0.5,
            paste("Error rendering Corpus 1:\n", e$message),
            cex = 1,
            col = "red"
          )
        }
      )
    },
    bg = "white"
  )

  # Keyness WordCloud Plot 1 - static plot for two_corpus
  output$keyness_wordcloud_plot1 <- renderPlot(
    {
      req(keyness_results())
      req(input$keyness_approach == "two_corpus")

      # Make counter a direct dependency to force re-render
      counter <- render_counter()

      req(values$keyness_results$wc1_data)
      data <- values$keyness_results$wc1_data

      # Debug message
      message(sprintf(
        "Rendering Corpus 1 wordcloud: %d words, counter: %d",
        nrow(data),
        counter
      ))

      # Load wordcloud package for static plots
      if (!requireNamespace("wordcloud", quietly = TRUE)) {
        plot.new()
        text(
          0.5,
          0.5,
          "Please install 'wordcloud' package:\ninstall.packages('wordcloud')",
          cex = 1.2,
          col = "red"
        )
        return()
      }

      if (nrow(data) == 0) {
        plot.new()
        text(
          0.5,
          0.5,
          "No keywords found for Corpus 1",
          cex = 1.5,
          col = "gray"
        )
        return()
      }

      # Use wordcloud package for static plot
      tryCatch(
        {
          par(mar = c(0, 0, 0, 0), bg = "white")
          set.seed(123 + counter)
          wordcloud::wordcloud(
            words = data$word,
            freq = data$freq,
            min.freq = 1,
            max.words = nrow(data),
            random.order = FALSE,
            rot.per = 0.35,
            colors = "#4575B4",
            scale = c(6, 0.5),
            family = "sans"
          )
        },
        error = function(e) {
          plot.new()
          text(
            0.5,
            0.5,
            paste("Error rendering Corpus 1:\n", e$message),
            cex = 1,
            col = "red"
          )
        }
      )
    },
    bg = "white"
  )

  # Keyness WordCloud Plot 2 - static plot for two_corpus
  output$keyness_wordcloud_plot2 <- renderPlot(
    {
      req(keyness_results())
      req(input$keyness_approach == "two_corpus")

      # Make counter a direct dependency to force re-render
      counter <- render_counter()

      req(values$keyness_results$wc2_data)
      data <- values$keyness_results$wc2_data

      # Debug message
      message(sprintf(
        "Rendering Corpus 2 wordcloud: %d words, counter: %d",
        nrow(data),
        counter
      ))

      # Load wordcloud package for static plots
      if (!requireNamespace("wordcloud", quietly = TRUE)) {
        plot.new()
        text(
          0.5,
          0.5,
          "Please install 'wordcloud' package:\ninstall.packages('wordcloud')",
          cex = 1.2,
          col = "red"
        )
        return()
      }

      if (nrow(data) == 0) {
        plot.new()
        text(
          0.5,
          0.5,
          "No keywords found for Corpus 2",
          cex = 1.5,
          col = "gray"
        )
        return()
      }

      # Use wordcloud package for static plot
      tryCatch(
        {
          par(mar = c(0, 0, 0, 0), bg = "white")
          set.seed(456 + counter)
          wordcloud::wordcloud(
            words = data$word,
            freq = data$freq,
            min.freq = 1,
            max.words = nrow(data),
            random.order = FALSE,
            rot.per = 0.35,
            colors = "#D73027",
            scale = c(6 * values$keyness_results$normalization_ratio, 0.5),
            family = "sans"
          )
        },
        error = function(e) {
          plot.new()
          text(
            0.5,
            0.5,
            paste("Error rendering Corpus 2:\n", e$message),
            cex = 1,
            col = "red"
          )
        }
      )
    },
    bg = "white"
  )
}


## Keyness Analysis Function ----

tall_keyness_analysis <- function(
  dfTag,
  approach = c("reference_corpus", "two_corpus"),
  language = "english",
  N = 2000,
  min.char = 3,
  upos_list = c("NOUN", "VERB")
) {
  # Match the approach argument
  approach <- match.arg(approach)

  # ============================================================================
  # APPROACH 1: REFERENCE CORPUS (original implementation)
  # Compare target corpus against a reference word frequency list
  # ============================================================================
  if (approach == "reference_corpus") {
    # Load word frequency list for the specified language
    word_frequency <- tall_load_wordlist(language = language)

    # Calculate observed frequencies by filtering and aggregating tokens
    x <- dfTag %>%
      dplyr::filter(upos %in% upos_list) %>%
      mutate(token = tolower(token)) %>%
      dplyr::group_by(token) %>%
      dplyr::summarise(n = n()) %>%
      ungroup() %>%
      dplyr::filter(nchar(token) > min.char) %>%
      slice_max(order_by = n, n = N) %>%
      rename(obsFreq = n) %>%
      as_tibble()

    # Calculate total number of words (excluding numbers and punctuation)
    total_words <- nrow(
      dfTag %>%
        dplyr::filter(!upos %in% c("NUM", "PUNCT"))
    )

    # Calculate expected frequencies based on reference corpus
    df <- word_frequency %>%
      as_tibble() %>%
      mutate(token = tolower(token)) %>%
      group_by(token) %>%
      summarise(rel_freq = sum(rel_freq, na.rm = T)) %>%
      ungroup() %>%
      mutate(expFreq = round(rel_freq * total_words, 0))

    # Create frequency table by joining observed and expected frequencies
    freq_table <- x %>%
      left_join(df, by = c("token")) %>%
      select(token, obsFreq, expFreq) %>%
      distinct() %>%
      ungroup() %>%
      # replace NA in expFreq with 1
      dplyr::mutate(
        expFreq = ifelse(is.na(expFreq), 1, expFreq)
      )
  } else if (approach == "two_corpus") {
    # ============================================================================
    # APPROACH 2: TWO CORPUS COMPARISON
    # Compare corpus 1 against corpus 2 to identify distinctive features
    # ============================================================================
    # Check if keyness_group column exists
    if (!"keyness_group" %in% names(dfTag)) {
      stop(
        "Error: dfTag must contain a 'keyness_group' column with values 1 and 2 for two-corpus comparison."
      )
    }

    # Check if keyness_group has exactly two groups (1 and 2) and remove NAs
    groups <- unique(dfTag %>% drop_na(keyness_group) %>% pull(keyness_group))
    if (!all(c(1, 2) %in% groups) || length(groups) != 2) {
      stop(
        "Error: 'keyness_group' must contain exactly two groups with values 1 and 2."
      )
    }

    # Calculate total number of tokens in each corpus (filtered by upos)
    n_tokens_corpus1 <- dfTag %>%
      dplyr::filter(keyness_group == 1 & upos %in% upos_list) %>%
      nrow()

    n_tokens_corpus2 <- dfTag %>%
      dplyr::filter(keyness_group == 2 & upos %in% upos_list) %>%
      nrow()

    # Calculate normalization ratio to scale corpus 2 to corpus 1 size
    normalization_ratio <- n_tokens_corpus1 / n_tokens_corpus2

    # Print corpus sizes for user information
    message(sprintf("Corpus 1 size: %d tokens", n_tokens_corpus1))
    message(sprintf("Corpus 2 size: %d tokens", n_tokens_corpus2))
    message(sprintf("Normalization ratio: %.4f", normalization_ratio))

    # Calculate frequencies for corpus 1
    freq_corpus1 <- dfTag %>%
      dplyr::filter(keyness_group == 1 & upos %in% upos_list) %>%
      mutate(token = tolower(token)) %>%
      dplyr::group_by(token) %>%
      dplyr::summarise(obsFreq = n()) %>%
      ungroup() %>%
      dplyr::filter(nchar(token) > min.char)

    # Calculate frequencies for corpus 2
    freq_corpus2 <- dfTag %>%
      dplyr::filter(keyness_group == 2 & upos %in% upos_list) %>%
      mutate(token = tolower(token)) %>%
      dplyr::group_by(token) %>%
      dplyr::summarise(expFreq_raw = n()) %>%
      ungroup() %>%
      dplyr::filter(nchar(token) > min.char) %>%
      # Normalize frequencies of corpus 2 by scaling to corpus 1 size
      dplyr::mutate(expFreq = expFreq_raw * normalization_ratio)

    # Join the two frequency tables
    # Keep all tokens from both corpora
    freq_table <- freq_corpus1 %>%
      full_join(freq_corpus2 %>% select(token, expFreq), by = "token") %>%
      # Replace NA values with 1 to avoid calculation errors
      dplyr::mutate(
        obsFreq = ifelse(is.na(obsFreq), 1, obsFreq),
        expFreq = ifelse(is.na(expFreq), 1, expFreq)
      ) %>%
      # Select top N tokens by sum of frequencies
      dplyr::mutate(total_freq = obsFreq + expFreq) %>%
      slice_max(order_by = total_freq, n = N) %>%
      select(token, obsFreq, expFreq)
  }

  # ============================================================================
  # COMMON CALCULATIONS FOR BOTH APPROACHES
  # Calculate contingency table statistics and keyness measures
  # ============================================================================

  # Calculate contingency table statistics
  stats_tb2 <- freq_table %>%
    dplyr::mutate(
      C1 = sum(obsFreq),
      C2 = sum(expFreq),
      N = C1 + C2
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      R1 = obsFreq + expFreq,
      R2 = N - R1,
      O11 = obsFreq,
      O11 = ifelse(O11 == 0, O11 + 0.1, O11),
      O12 = R1 - O11,
      O21 = C1 - O11,
      O22 = C2 - O12
    ) %>%
    dplyr::mutate(
      E11 = (R1 * C1) / N,
      E12 = (R1 * C2) / N,
      E21 = (R2 * C1) / N,
      E22 = (R2 * C2) / N
    ) %>%
    dplyr::select(-obsFreq, -expFreq)

  # Calculate association measures and keyness statistics
  assoc_tb3 <- stats_tb2 %>%
    dplyr::mutate(Rws = nrow(.)) %>%
    dplyr::rowwise() %>%
    # Calculate Fisher's exact test
    dplyr::mutate(
      p = as.vector(unlist(fisher.test(matrix(
        c(O11, O12, O21, O22),
        ncol = 2,
        byrow = T
      ))[1]))
    ) %>%
    # Calculate per thousand word frequencies
    dplyr::mutate(
      ptw_target = O11 / C1 * 1000,
      ptw_ref = O12 / C2 * 1000
    ) %>%
    # Calculate chi-square statistic
    dplyr::mutate(
      X2 = (O11 - E11)^2 /
        E11 +
        (O12 - E12)^2 / E12 +
        (O21 - E21)^2 / E21 +
        (O22 - E22)^2 / E22
    ) %>%
    # Calculate various keyness measures
    dplyr::mutate(
      phi = sqrt((X2 / N)),
      MI = log2(O11 / E11),
      t.score = (O11 - E11) / sqrt(O11),
      PMI = log2((O11 / N) / ((O11 + O12) / N) * ((O11 + O21) / N)),
      DeltaP = (O11 / R1) - (O21 / R2),
      LogOddsRatio = log(
        ((O11 + 0.5) * (O22 + 0.5)) / ((O12 + 0.5) * (O21 + 0.5))
      ),
      G2 = 2 *
        ((O11 + 0.001) *
          log((O11 + 0.001) / E11) +
          (O12 + 0.001) * log((O12 + 0.001) / E12) +
          O21 * log(O21 / E21) +
          O22 * log(O22 / E22)),
      # Traditional keyness measures
      RateRatio = ((O11 + 0.001) / (C1 * 1000)) / ((O12 + 0.001) / (C2 * 1000)),
      RateDifference = (O11 / (C1 * 1000)) - (O12 / (C2 * 1000)),
      DifferenceCoefficient = RateDifference /
        sum((O11 / (C1 * 1000)), (O12 / (C2 * 1000))),
      OddsRatio = ((O11 + 0.5) * (O22 + 0.5)) / ((O12 + 0.5) * (O21 + 0.5)),
      LLR = 2 * (O11 * (log((O11 / E11)))),
      RDF = abs((O11 / C1) - (O12 / C2)),
      PDiff = abs(ptw_target - ptw_ref) / ((ptw_target + ptw_ref) / 2) * 100,
      SignedDKL = sum(
        ifelse(O11 > 0, O11 * log(O11 / ((O11 + O12) / 2)), 0) -
          ifelse(O12 > 0, O12 * log(O12 / ((O11 + O12) / 2)), 0)
      )
    ) %>%
    # Determine Bonferroni corrected significance
    dplyr::mutate(
      Sig_corrected = dplyr::case_when(
        p / Rws > .05 ~ "n.s.",
        p / Rws > .01 ~ "p < .05*",
        p / Rws > .001 ~ "p < .01**",
        p / Rws <= .001 ~ "p < .001***",
        T ~ "N.A."
      )
    ) %>%
    # Round p-value and determine type/antitype
    dplyr::mutate(
      p = round(p, 5),
      type = ifelse(E11 > O11, "antitype", "type"),
      phi = ifelse(E11 > O11, -phi, phi),
      G2 = ifelse(E11 > O11, -G2, G2)
    ) %>%
    # Filter out non-significant results
    dplyr::filter(Sig_corrected != "n.s.") %>%
    # Arrange by G2 statistic
    dplyr::arrange(-G2) %>%
    # Remove superfluous columns
    dplyr::select(
      -any_of(c(
        "TermCoocFreq",
        "AllFreq",
        "NRows",
        "R1",
        "R2",
        "C1",
        "C2",
        "E12",
        "E21",
        "E22",
        "upp",
        "low",
        "op",
        "t.score",
        "z.score",
        "Rws"
      ))
    ) %>%
    # Relocate important columns to the front
    dplyr::relocate(any_of(c(
      "token",
      "type",
      "Sig_corrected",
      "O11",
      "O12",
      "ptw_target",
      "ptw_ref",
      "G2",
      "RDF",
      "RateRatio",
      "RateDifference",
      "DifferenceCoefficient",
      "LLR",
      "SignedDKL",
      "PDiff",
      "LogOddsRatio",
      "MI",
      "PMI",
      "phi",
      "X2",
      "OddsRatio",
      "DeltaP",
      "p",
      "E11",
      "O21",
      "O22"
    )))

  # Return results with approach information
  return(list(
    results = assoc_tb3,
    approach = approach,
    corpus1_size = if (approach == "two_corpus") n_tokens_corpus1 else NULL,
    corpus2_size = if (approach == "two_corpus") n_tokens_corpus2 else NULL,
    normalization_ratio = if (approach == "two_corpus") {
      normalization_ratio
    } else {
      NULL
    }
  ))
}

plot_tall_keyness <- function(
  assoc_tb3,
  measure = "G2",
  N = 10,
  approach = "reference_corpus"
) {
  # Get top N and bottom N keywords
  top <- assoc_tb3 %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(.data[[measure]])) %>%
    dplyr::slice_head(n = N)

  bot <- assoc_tb3 %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(.data[[measure]])) %>%
    dplyr::slice_tail(n = N)

  names(top)[which(names(top) %in% measure)] <- "Measure"
  names(bot)[which(names(bot) %in% measure)] <- "Measure"

  combined_data <- rbind(top, bot)

  # Define plot title based on approach
  if (approach == "two_corpus") {
    plot_title <- paste0("Top ", N, " keywords for Corpus 1 vs Corpus 2")
  } else {
    plot_title <- paste0("Top ", N, " keywords for Target vs Reference Corpus")
  }

  # Create ggplot bar plot for top/bottom keywords
  plot_gg_bar <- combined_data %>%
    ggplot(aes(
      x = reorder(token, Measure, mean),
      y = Measure,
      label = Measure,
      fill = type
    )) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(
        y = ifelse(Measure > 0, Measure - 50, Measure + 50),
        label = round(Measure, 1)
      ),
      color = "white",
      size = 3
    ) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("antitype" = "#D73027", "type" = "#4575B4")) +
    labs(
      title = plot_title,
      x = "Keyword",
      y = paste0("Keyness (", measure, ")")
    )

  # Create plotly bar plot
  combined_data <- combined_data %>%
    dplyr::mutate(
      token = reorder(token, Measure, mean),
      color = ifelse(type == "antitype", "#D73027", "#4575B4")
    )

  plot_plotly_bar <- plotly::plot_ly(
    data = combined_data,
    y = ~token,
    x = ~Measure,
    type = "bar",
    orientation = "h",
    marker = list(color = ~color),
    text = ~ round(Measure, 1),
    textposition = "inside",
    textfont = list(color = "white", size = 12)
  ) %>%
    plotly::layout(
      title = plot_title,
      xaxis = list(title = paste0("Keyness (", measure, ")")),
      yaxis = list(
        title = "Keyword",
        tickmode = "linear",
        categoryorder = "trace",
        autorange = "reversed"
      ),
      showlegend = FALSE,
      margin = list(l = 100)
    )

  # Return results
  return(list(
    plot_ggplot_bar = plot_gg_bar,
    plot_plotly_bar = plot_plotly_bar
  ))
}

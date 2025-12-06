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
                )
                # ,
                # checkboxGroupInput(
                #   inputId = "keyness_upos",
                #   label = "POS Tags:",
                #   choices = c("NOUN", "VERB", "ADJ", "ADV"),
                #   selected = c("NOUN", "VERB"),
                #   inline = FALSE
                # )
              ),
              # Graphical Parameters Section
              tags$details(
                class = "advanced-section",
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
                  h4("Bar Plot:"),
                  numericInput(
                    "Keyness_Nbarplot",
                    label = "N. of Words",
                    value = 10,
                    min = 1,
                    max = 20,
                    step = 1
                  )
                ),
                div(
                  style = "margin-top: 10px;",
                  h4("Wordcloud:"),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        "Keyness_Nwc",
                        label = "N. of Words",
                        value = 50,
                        min = 10,
                        step = 1,
                        max = 200
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        "Keyness_size",
                        label = "Label Size",
                        value = 100,
                        min = 10,
                        step = 1,
                        max = 200
                      )
                    )
                  )
                ),
                div(
                  style = "margin-top: 10px;",
                  h4("Frequency Context:"),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        "Keyness_freq_words",
                        label = "N. of Words",
                        value = 20,
                        min = 1,
                        step = 1,
                        max = 100
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        "Keyness_label_spacing",
                        label = "Label spacing",
                        value = 0.15,
                        min = 0,
                        step = 0.01,
                        max = 1
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
            fluidRow(
              conditionalPanel(
                condition = "input.keyness_approach == 'reference_corpus'",
                h4(
                  "Target Corpus: High Keyness Words",
                  align = "center",
                  style = "color: #4F7942; margin-bottom: 20px;"
                )
              ),
              shinycssloaders::withSpinner(
                plotOutput(
                  outputId = "keyness_wordcloud_plot",
                  height = "600px",
                  width = "100%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
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

      if (approach == "refrence_corpus") {
        upos <- intersect(
          values$dfTag %>%
            LemmaSelection() %>%
            dplyr::filter(docSelected) %>%
            distinct(upos) %>%
            pull(upos),
          c("NOUN", "VERB", "ADJ", "ADV")
        )
      } else {
        upos <- intersect(
          values$dfTag %>%
            LemmaSelection() %>%
            dplyr::filter(docSelected) %>%
            distinct(upos) %>%
            pull(upos),
          c(
            "ADJ",
            "ADP",
            "ADV",
            "AUX",
            "CCONJ",
            "DET",
            "EMAIL",
            "EMOJI",
            "HASH",
            "INTJ",
            "IP_ADDRESS",
            "MENTION",
            "MULTIWORD",
            "NOUN",
            "NUM",
            "PRON",
            "PROPN",
            "PUNCT",
            "SCONJ",
            "SYM",
            "TO_REMOVE",
            "URL",
            "VERB"
          )
        )
      }

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

      if (length(upos) == 0) {
        showNotification(
          "Error: No valid PoS Tag selected.",
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
          upos_list = upos,
          term = ifelse(
            approach == "reference_corpus",
            "token",
            values$generalTerm
          )
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
            top_n = input$Keyness_freq_words,
            g2_threshold = quantile(results$results$G2, 0.95),
            label_spacing = input$Keyness_label_spacing
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
          values$keyness_wordcloud_plot <- wordcloud(
            data_target,
            shape = "circle",
            rot_per = 0.2,
            eccentricity = 1.3,
            colors = sample(colorlist(), nrow(data_target), replace = T),
            seed = values$random_seed,
            max_size = input$Keyness_size
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
            rename(word = token, freq = all_of(measure)) %>%
            mutate(Corpus = "Corpus 1")

          data_corpus2 <- data_negative %>%
            rename(word = token, freq = all_of(measure)) %>%
            mutate(Corpus = "Corpus 2")

          results <- c(
            results,
            wc1_data = list(data_corpus1),
            wc2_data = list(data_corpus2)
          )

          values$keyness_wordcloud_plot <- wordcloud(
            bind_rows(
              data_corpus1,
              data_corpus2
            ),
            shape = "circle",
            rot_per = 0.2,
            eccentricity = 1.3,
            colors = c("#4575B4", "#D73027"),
            seed = values$random_seed,
            max_size = input$Keyness_size * 0.7,
            facet_by = "Corpus",
            facet_ncol = 2
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

  output$keyness_wordcloud_plot <- renderPlot(
    {
      req(keyness_results())
      req(input$keyness_approach %in% c("reference_corpus", "two_corpus"))

      if (input$keyness_approach == "reference_corpus") {
        req(values$keyness_results$wc_data)
        nrowdata <- nrow(values$keyness_results$wc_data)
      } else if (input$keyness_approach == "two_corpus") {
        req(values$keyness_results$wc1_data)
        nrowdata <- nrow(values$keyness_results$wc1_data) +
          nrow(values$keyness_results$wc2_data)
      }

      if (nrowdata == 0) {
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
          values$keyness_wordcloud_plot
        },
        error = function(e) {
          plot.new()
          text(
            0.5,
            0.5,
            paste("Error rendering Corpus :\n", e$message),
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
  upos_list = c("NOUN", "VERB"),
  term = "token"
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
      dplyr::rename(term_col = all_of(term)) %>%
      mutate(term_col = tolower(term_col)) %>%
      dplyr::group_by(term_col) %>%
      dplyr::summarise(obsFreq = n()) %>%
      ungroup() %>%
      dplyr::filter(nchar(term_col) > min.char) %>%
      dplyr::rename(token = term_col)

    # freq_corpus1 <- dfTag %>%
    #   dplyr::filter(keyness_group == 1 & upos %in% upos_list) %>%
    #   mutate(token = tolower(token)) %>%
    #   dplyr::group_by(token) %>%
    #   dplyr::summarise(obsFreq = n()) %>%
    #   ungroup() %>%
    #   dplyr::filter(nchar(token) > min.char)

    # Calculate frequencies for corpus 2
    freq_corpus2 <- dfTag %>%
      dplyr::filter(keyness_group == 2 & upos %in% upos_list) %>%
      dplyr::rename(term_col = all_of(term)) %>%
      mutate(term_col = tolower(term_col)) %>%
      dplyr::group_by(term_col) %>%
      dplyr::summarise(expFreq_raw = n()) %>%
      ungroup() %>%
      dplyr::filter(nchar(term_col) > min.char) %>%
      dplyr::rename(token = term_col) %>%
      dplyr::mutate(expFreq = expFreq_raw * normalization_ratio)

    # freq_corpus2 <- dfTag %>%
    #   dplyr::filter(keyness_group == 2 & upos %in% upos_list) %>%
    #   mutate(token = tolower(token)) %>%
    #   dplyr::group_by(token) %>%
    #   dplyr::summarise(expFreq_raw = n()) %>%
    #   ungroup() %>%
    #   dplyr::filter(nchar(token) > min.char) %>%
    #   # Normalize frequencies of corpus 2 by scaling to corpus 1 size
    #   dplyr::mutate(expFreq = expFreq_raw * normalization_ratio)

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

### Keyness Wordlist Functions ----
tall_download_wordlist <- function(
  language,
  file_dir = NULL,
  overwrite = TRUE
) {
  filename <- paste0(language, "_word_frequency.keyness")

  if (is.null(file_dir)) {
    file_dir <- paste0(homeFolder(), "/tall/language_models")
  }

  url <- file.path(
    "https://raw.githubusercontent.com/massimoaria/tall.language.models/main/word.frequency.data",
    filename
  )
  to <- file.path(file_dir, filename)
  download_failed <- FALSE
  download_message <- "OK"
  dl <- suppressWarnings(try(
    utils::download.file(url = url, destfile = to, mode = "wb"),
    silent = TRUE
  ))
  if (inherits(dl, "try-error")) {
    download_failed <- TRUE
    download_message <- as.character(dl)
  } else if (inherits(dl, "integer") && dl != 0) {
    download_failed <- TRUE
    download_message <- "Download failed. Please check internet connectivity"
  }
  if (download_failed) {
    message("Something went wrong")
    message(download_message)
  } else {
    message(sprintf("Downloading finished, model stored at '%s'", to))
  }

  return(list(
    download_failed = download_failed,
    download_message = download_message,
    file_wordlist = to
  ))
}

tall_load_wordlist <- function(
  language,
  file_dir = NULL
) {
  wordlist_path <- paste0(homeFolder(), "/tall/language_models/")
  if (!dir.exists(wordlist_path)) {
    dir.create(wordlist_path, recursive = TRUE)
  }

  filename <- paste0(language, "_word_frequency.keyness")

  file_path <- file.path(wordlist_path, filename)

  if (!file.exists(file_path)) {
    message(sprintf(
      "Wordlist for language '%s' not found locally. Downloading...",
      language
    ))
    info <- tall_download_wordlist(
      language = language,
      file_dir = file_dir,
      overwrite = FALSE
    )
    if (info$download_failed) {
      return(NULL)
    }
  }

  load(file_path)

  return(word_frequency)
}


### Keyness Plotting Function ----
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

### Frequency Plotting Function ----

#' Identify and Visualize Frequency Context in Keyness Results
#'
#' This function analyzes keyness results to identify specialized terminology
#' (high keyness, low frequency) versus fundamental stylistic/thematic differences
#' (high keyness, high frequency).
#'
#' @param keyness_results A data frame containing keyness analysis results with
#'        columns: Word, G2, Obs_Freq (observed frequency in target corpus)
#' @param top_n Number of top high-frequency and low-frequency words to select (default: 15)
#' @param g2_threshold Minimum G2 score to consider (default: 10.83, p < 0.001)
#' @param title Plot title (default: "Frequency Context Analysis")
#' @param label_spacing Spacing factor for labels to avoid overlap (default: 0.08)
#' @param freq_threshold Frequency threshold to separate low/high frequency zones (default: NULL, uses median)
#' @return A plotly scatter plot object
#'
frequency_context_analysis <- function(
  keyness_results,
  top_n = 15,
  g2_threshold = 10.83,
  title = "Frequency Context Analysis",
  label_spacing = 0.08,
  freq_threshold = NULL
) {
  # Load required libraries
  require(plotly)
  require(dplyr)

  # Filter words with high keyness scores (significant keywords)
  high_keyness <- keyness_results %>%
    dplyr::filter(G2 >= g2_threshold) %>%
    arrange(desc(G2)) %>%
    rename(
      Word = token,
      Obs_Freq = O11
    )

  # Identify top N high-frequency words (fundamental differences)
  high_freq_words <- high_keyness %>%
    arrange(desc(Obs_Freq)) %>%
    head(top_n) %>%
    mutate(Category = "High Frequency\n(Fundamental Differences)")

  # Identify top N low-frequency words (specialized terminology)
  low_freq_words <- high_keyness %>%
    arrange(Obs_Freq) %>%
    head(top_n) %>%
    anti_join(high_freq_words, by = "Word") %>%
    mutate(Category = "Low Frequency\n(Specialized Terminology)")

  # Combine selected words
  selected_words <- bind_rows(high_freq_words, low_freq_words)

  # Calculate frequency threshold if not provided
  if (is.null(freq_threshold)) {
    freq_threshold <- mean(
      min(high_freq_words$Obs_Freq),
      max(low_freq_words$Obs_Freq)
    ) #median(selected_words$Obs_Freq)
  }

  # Transform coordinates to log scale for calculations
  selected_words <- selected_words %>%
    mutate(
      log_freq = log10(Obs_Freq),
      log_g2 = log10(G2)
    )

  # Calculate axis ranges for background zones (in original scale)
  x_range_orig <- range(selected_words$Obs_Freq)
  y_range_orig <- range(selected_words$G2)

  # Extend ranges for better visualization
  x_min <- x_range_orig[1] * 0.5
  x_max <- x_range_orig[2] * 2
  y_min <- y_range_orig[1] * 0.5
  y_max <- y_range_orig[2] * 2

  # Algorithm to adjust label positions alternating above/below for nearby points
  # with increased spacing for low frequency words
  adjust_labels_alternating <- function(df, spacing = label_spacing) {
    df <- df %>% arrange(log_freq, log_g2)

    # Initialize adjusted positions and anchor positions
    df$label_x <- df$log_freq
    df$label_y <- df$log_g2
    df$yanchor <- "bottom" # Default: label above point

    # Set spacing multiplier based on category (more space for low frequency)
    df$spacing_mult <- ifelse(
      df$Category == "Low Frequency\n(Specialized Terminology)",
      2.5,
      1.0
    )

    # Identify clusters of nearby points
    clusters <- list()
    current_cluster <- c(1)

    for (i in 2:nrow(df)) {
      # Check if point i is close to any point in current cluster
      is_close <- FALSE
      for (j in current_cluster) {
        dx <- df$log_freq[i] - df$log_freq[j]
        dy <- df$log_g2[i] - df$log_g2[j]
        dist <- sqrt(dx^2 + dy^2)
        if (dist < spacing * 2) {
          is_close <- TRUE
          break
        }
      }

      if (is_close) {
        current_cluster <- c(current_cluster, i)
      } else {
        if (length(current_cluster) > 1) {
          clusters[[length(clusters) + 1]] <- current_cluster
        }
        current_cluster <- c(i)
      }
    }
    # Add last cluster
    if (length(current_cluster) > 1) {
      clusters[[length(clusters) + 1]] <- current_cluster
    }

    # For each cluster, alternate labels above and below with increased distance for low freq
    for (cluster in clusters) {
      # Sort cluster by G2 value (vertical position)
      cluster_sorted <- cluster[order(df$log_g2[cluster])]

      # Alternate anchor positions
      for (idx in seq_along(cluster_sorted)) {
        i <- cluster_sorted[idx]
        mult <- df$spacing_mult[i]

        if (idx %% 2 == 0) {
          df$yanchor[i] <- "top" # Label below point
          df$label_y[i] <- df$log_g2[i] - spacing * 0.5 * mult
        } else {
          df$yanchor[i] <- "bottom" # Label above point
          df$label_y[i] <- df$log_g2[i] + spacing * 0.5 * mult
        }
      }
    }

    # Additional refinement: push labels apart if still overlapping
    for (iter in 1:30) {
      moved <- FALSE
      for (i in 1:nrow(df)) {
        for (j in 1:nrow(df)) {
          if (i >= j) {
            next
          }

          dx <- df$label_x[i] - df$label_x[j]
          dy <- df$label_y[i] - df$label_y[j]
          dist <- sqrt(dx^2 + dy^2)

          # Use max spacing multiplier for the pair
          max_mult <- max(df$spacing_mult[i], df$spacing_mult[j])
          min_dist <- spacing * 0.8 * max_mult

          # If labels are still too close, push them apart
          if (dist < min_dist && dist > 0) {
            push_x <- dx / dist * (min_dist - dist) / 2
            push_y <- dy / dist * (min_dist - dist) / 2

            df$label_x[i] <- df$label_x[i] + push_x
            df$label_x[j] <- df$label_x[j] - push_x
            df$label_y[i] <- df$label_y[i] + push_y
            df$label_y[j] <- df$label_y[j] - push_y
            moved <- TRUE
          }
        }
      }
      if (!moved) break
    }

    return(df)
  }

  # Adjust label positions with alternating strategy
  selected_words <- adjust_labels_alternating(
    selected_words,
    spacing = label_spacing
  )

  # Create base scatter plot
  p <- plot_ly() %>%
    # Add data points for high frequency words
    add_trace(
      data = selected_words %>%
        filter(Category == "High Frequency\n(Fundamental Differences)"),
      x = ~Obs_Freq,
      y = ~G2,
      type = "scatter",
      mode = "markers",
      name = "High Frequency<br>(Fundamental Differences)",
      marker = list(
        size = 12,
        color = "#FF8C00",
        line = list(color = "white", width = 1.5),
        opacity = 0.8
      ),
      text = ~Word,
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "Frequency: %{x}<br>",
        "G² Score: %{y:.2f}<br>",
        "<extra></extra>"
      )
    ) %>%
    # Add data points for low frequency words
    add_trace(
      data = selected_words %>%
        filter(Category == "Low Frequency\n(Specialized Terminology)"),
      x = ~Obs_Freq,
      y = ~G2,
      type = "scatter",
      mode = "markers",
      name = "Low Frequency<br>(Specialized Terminology)",
      marker = list(
        size = 12,
        color = "#8B4789",
        line = list(color = "white", width = 1.5),
        opacity = 0.8
      ),
      text = ~Word,
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "Frequency: %{x}<br>",
        "G² Score: %{y:.2f}<br>",
        "<extra></extra>"
      )
    )

  # Add annotations for labels with adjusted positions and alternating anchors
  annotations_list <- lapply(1:nrow(selected_words), function(i) {
    row <- selected_words[i, ]

    list(
      x = log10(row$Obs_Freq),
      y = log10(row$G2),
      xref = "x",
      yref = "y",
      text = row$Word,
      xanchor = "center",
      yanchor = row$yanchor, # Alternating between "top" and "bottom"
      showarrow = TRUE,
      arrowhead = 0,
      arrowsize = 0.5,
      arrowwidth = 1,
      arrowcolor = "rgba(128,128,128,0.5)",
      ax = (row$label_x - log10(row$Obs_Freq)) * 100,
      ay = (row$label_y - log10(row$G2)) * 100,
      font = list(size = 10, color = "black"),
      bgcolor = "rgba(255,255,255,0.5)",
      bordercolor = "rgba(128,128,128,0.6)",
      borderwidth = 0.5,
      borderpad = 2
    )
  })

  # Finalize layout with background shapes and legend
  p <- p %>%
    layout(
      xaxis = list(
        title = "Observed Frequency (Target Corpus)",
        type = "log",
        gridcolor = "#E0E0E0",
        showline = TRUE,
        linecolor = "#CCCCCC"
      ),
      yaxis = list(
        title = "G² Keyness Score",
        type = "log",
        gridcolor = "#E0E0E0",
        showline = TRUE,
        linecolor = "#CCCCCC"
      ),
      plot_bgcolor = "#F8F9FA",
      paper_bgcolor = "white",
      hovermode = "closest",
      showlegend = TRUE,
      legend = list(
        x = 0.02,
        y = 0.98,
        xanchor = "left",
        yanchor = "top",
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "#CCCCCC",
        borderwidth = 1
      ),
      # Add background rectangles using shapes (coordinates in original scale for log axes)
      shapes = list(
        # Low frequency zone (specialized terminology) - light purple
        list(
          type = "rect",
          xref = "x",
          yref = "y",
          x0 = x_min,
          y0 = y_min,
          x1 = freq_threshold,
          y1 = y_max,
          fillcolor = "rgba(139, 71, 137, 0.08)", # Purple shade matching the points
          line = list(width = 0),
          layer = "below"
        ),
        # High frequency zone (fundamental differences) - light orange
        list(
          type = "rect",
          xref = "x",
          yref = "y",
          x0 = freq_threshold,
          y0 = y_min,
          x1 = x_max,
          y1 = y_max,
          fillcolor = "rgba(255, 140, 0, 0.08)", # Orange shade matching the points
          line = list(width = 0),
          layer = "below"
        )
      ),
      margin = list(r = 80, b = 100, l = 80, t = 80),
      annotations = c(
        annotations_list,
        list(
          list(
            text = paste(
              "High keyness threshold: G² ≥",
              round(g2_threshold, 1)
            ),
            xref = "paper",
            yref = "paper",
            x = 0.01,
            y = -0.15,
            xanchor = "left",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "gray")
          )
        )
      )
    )

  return(p)
}

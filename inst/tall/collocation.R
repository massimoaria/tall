################################################################################
# COLLOCATION ANALYSIS MODULE
# Implements AntConc-like collocation analysis features:
# - Plot: Distribution of target word across documents with document viewer
# - Collocate: Words that co-occur with target word (with all statistics: MI, LogLik, T-Score)
################################################################################

# ============================================================================
# UI COMPONENT
# ============================================================================

collocationUI <- function() {
  tabItem(
    tabName = "kwic",
    fluidRow(
      # Usa fluidRow direttamente
      column(
        12,
        box(
          title = tagList(
            icon("project-diagram"),
            tags$span(
              "KWIC Analysis",
              style = "margin-left: 10px; vertical-align: middle;"
            )
          ),
          width = 12,
          status = "success",
          solidHeader = TRUE,
          collapsible = FALSE,
          tabsetPanel(
            id = "collocationTabs",

            # ==========================================
            # TAB 1: PLOT - Word Distribution
            # ==========================================
            tabPanel(
              "In-Document Plot",
              fluidPage(
                br(),
                fluidRow(
                  column(
                    12,
                    wellPanel(
                      style = "background: white; border: 1px solid #ddd; padding: 15px;",
                      fluidRow(
                        column(
                          3,
                          h4(
                            strong("Search Settings"),
                            style = "color: #4F7942; margin-top: 0;"
                          ),

                          # Search query
                          textInput(
                            "collocPlotQuery",
                            "Target Word:",
                            placeholder = "Enter word to search..."
                          ),

                          # Case sensitive option
                          checkboxInput(
                            "collocPlotCase",
                            "Case Sensitive",
                            value = FALSE
                          )
                        ),
                        column(
                          3,
                          h4(
                            strong("Display Options"),
                            style = "color: #4F7942; margin-top: 0;"
                          ),

                          # Sort options
                          selectInput(
                            "collocPlotSort",
                            "Sort Documents By:",
                            choices = c(
                              #"Row Number" = "row",
                              "Document ID" = "doc_id",
                              "Frequency" = "freq",
                              "Dispersion" = "dispersion"
                            ),
                            selected = "freq"
                          )
                        ),
                        column(
                          6,
                          # Results summary
                          htmlOutput("collocPlotSummary")
                        )
                      ),
                      fluidRow(
                        column(
                          12,
                          # Search button
                          actionButton(
                            "collocPlotSearch",
                            "Search",
                            icon = icon("search"),
                            class = "btn-success",
                            style = "width: 200px; margin-top: 10px;"
                          )
                        )
                      )
                    )
                  )
                ),

                fluidRow(
                  column(
                    12,
                    # Documents table with inline plots
                    h4(strong("Documents"), style = "color: #4F7942;"),
                    shinycssloaders::withSpinner(
                      DT::DTOutput("collocPlotTable"),
                      color = "#4F7942"
                    )
                  )
                )
              )
            ),

            # ==========================================
            # TAB 2: COLLOCATE
            # ==========================================
            tabPanel(
              "Collocation",
              fluidPage(
                br(),
                fluidRow(
                  column(
                    12,
                    wellPanel(
                      style = "background: white; border: 1px solid #ddd; padding: 15px;",
                      fluidRow(
                        column(
                          3,
                          h4(
                            strong("Search Settings"),
                            style = "color: #4F7942; margin-top: 0;"
                          ),

                          # Search query
                          textInput(
                            "collocCollocQuery",
                            "Target Word:",
                            placeholder = "Enter word to search..."
                          ),

                          # Case sensitive
                          checkboxInput(
                            "collocCollocCase",
                            "Case Sensitive",
                            value = FALSE
                          )
                        ),
                        column(
                          3,
                          h4(
                            strong("Window Settings"),
                            style = "color: #4F7942; margin-top: 0;"
                          ),

                          # Window span
                          fluidRow(
                            column(
                              6,
                              numericInput(
                                "collocCollocLeft",
                                "Left Span:",
                                value = 2,
                                min = 0,
                                max = 10,
                                step = 1
                              )
                            ),
                            column(
                              6,
                              numericInput(
                                "collocCollocRight",
                                "Right Span:",
                                value = 2,
                                min = 0,
                                max = 10,
                                step = 1
                              )
                            )
                          )
                        ),
                        column(
                          3,
                          h4(
                            strong("Filter Settings"),
                            style = "color: #4F7942; margin-top: 0;"
                          ),

                          # Minimum frequency
                          numericInput(
                            "collocCollocMinFreq",
                            "Minimum Frequency:",
                            value = 2,
                            min = 1,
                            max = 100,
                            step = 1
                          )
                        ),
                        column(
                          3,
                          # Results summary
                          htmlOutput("collocCollocSummary")
                        )
                      ),
                      fluidRow(
                        column(
                          12,
                          # Search button
                          actionButton(
                            "collocCollocSearch",
                            "Search",
                            icon = icon("search"),
                            class = "btn-success",
                            style = "width: 200px; margin-top: 10px;"
                          )
                        )
                      )
                    )
                  )
                ),

                fluidRow(
                  column(
                    12,
                    # Collocates table
                    h4(strong("Collocates"), style = "color: #4F7942;"),
                    shinycssloaders::withSpinner(
                      DT::DTOutput("collocCollocTable"),
                      color = "#4F7942"
                    )
                  )
                )
              )
            ),

            # ==========================================
            # TAB 3: KWIC Network
            # ==========================================
            tabPanel(
              "Network",
              fluidPage(
                br(),
                fluidRow(
                  column(
                    12,
                    wellPanel(
                      style = "background: white; border: 1px solid #ddd; padding: 15px;",
                      fluidRow(
                        column(
                          3,
                          h4(
                            strong("Search Settings"),
                            style = "color: #4F7942; margin-top: 0;"
                          ),

                          # Search query
                          selectizeInput(
                            inputId = "wordsContSearch",
                            label = "Search word(s) in text",
                            choices = NULL
                          )
                        ),
                        column(
                          3,
                          h4(
                            strong("Window Settings"),
                            style = "color: #4F7942; margin-top: 0;"
                          ),

                          # Window span
                          fluidRow(
                            column(
                              6,
                              numericInput(
                                "wordsContBefore",
                                "Left Span:",
                                value = 5,
                                min = 0,
                                max = 20,
                                step = 1
                              )
                            ),
                            column(
                              6,
                              numericInput(
                                "wordsContAfter",
                                "Right Span:",
                                value = 5,
                                min = 0,
                                max = 20,
                                step = 1
                              )
                            )
                          )
                        ),
                        column(
                          2,
                          h4(
                            strong("Network Settings"),
                            style = "color: #4F7942; margin-top: 0;"
                          ),

                          # Window span
                          fluidRow(
                            column(
                              9,
                              numericInput(
                                "wordsContN",
                                "Words",
                                value = 50,
                                min = 10,
                                max = 200,
                                step = 1
                              )
                            )
                          )
                        ),
                        column(
                          3,
                          # Results summary
                          htmlOutput("collocNetworkSummary")
                        )
                      ),
                      fluidRow(
                        column(
                          2,
                          # Search button
                          actionButton(
                            "wordsContApply",
                            "Search",
                            icon = icon("search"),
                            class = "btn-success",
                            style = "width: 200px; margin-top: 10px;"
                          )
                        ),
                        column(
                          2,
                          # Search button
                          actionButton(
                            "wordsContReset",
                            "Reset",
                            icon = icon("xmark"),
                            class = "btn-error",
                            style = "width: 200px; margin-top: 10px;"
                          )
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      "Words in Context",
                      fluidRow(
                        column(
                          12,
                          fluidRow(
                            column(11),
                            div(
                              title = "Export Table as Excel",
                              column(
                                1,
                                do.call(
                                  "actionButton",
                                  c(
                                    export_bttn,
                                    list(
                                      inputId = "wordsContSave"
                                    )
                                  )
                                ),
                                br()
                              )
                            )
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          12,
                          div(
                            style = "height: 450px; overflow-y: scroll; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;",
                            shinycssloaders::withSpinner(
                              uiOutput("wordsContHtml"),
                              color = getOption(
                                "spinner.color",
                                default = "#4F7942"
                              )
                            )
                          )
                        )
                      )
                    ),
                    tabPanel(
                      "Network Plot",
                      fluidRow(
                        column(10),
                        div(
                          title = t_export,
                          column(
                            1,
                            do.call(
                              "actionButton",
                              c(
                                export_bttn,
                                list(
                                  inputId = "wordContNetExport"
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
                                  inputId = "wordContNetReport"
                                )
                              )
                            )
                          )
                        )
                      ),
                      fluidRow(
                        shinycssloaders::withSpinner(
                          visNetworkOutput(
                            "wordsContNetwork",
                            width = "auto",
                            height = "58vh"
                          ),
                          color = getOption(
                            "spinner.color",
                            default = "#4F7942"
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
                              htmlOutput("ContextGeminiUI"),
                              caption = HTML(
                                "<br><strong>Thinking...</strong>"
                              ),
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
            ),

            # ==========================================
            # TAB 4: INFO & REFERENCES
            # ==========================================
            tabPanel(
              "Info & References",
              fluidPage(
                fluidRow(
                  column(1),
                  column(
                    10,
                    br(),
                    HTML(
                      "<h3 style='color: #4F7942;'>Collocation Analysis</h3>
                        <p>This module implements collocation analysis features inspired by AntConc:</p>
                        <ul>
                          <li><b>Plot:</b> Visualizes the distribution of a target word across documents
                          in a 'barcode' format, showing where hits appear in each document. Click 'View' button
                          to see the full document with highlighted target words.</li>
                          <li><b>Collocate:</b> Identifies words that frequently appear near the target
                          word, using multiple statistical measures (MI score, Log-Likelihood, and T-Score)
                          displayed simultaneously in the results table.</li>
                        </ul>

                        <h4 style='color: #4F7942;'>Statistical Measures</h4>
                        <ul>
                          <li><b>MI Score (Mutual Information):</b> Measures the strength of association
                          between words. Higher values indicate stronger collocations. Values > 3 are typically
                          considered significant.</li>
                          <li><b>Log-Likelihood:</b> Statistical test for significance of co-occurrence.
                          Values > 3.84 are significant (p < 0.05), values > 15.13 are highly significant (p < 0.0001).</li>
                          <li><b>T-Score:</b> Measures the confidence in the collocation. Values > 2
                          are generally considered significant. Unlike MI, T-Score is less sensitive to low frequencies.</li>
                        </ul>

                        <h4 style='color: #4F7942;'>References</h4>
                        <ul>
                          <li>Anthony, L. (2024). AntConc (Version 4.3.1) [Computer Software].
                          Tokyo, Japan: Waseda University.</li>
                          <li>Evert, S. (2005). The Statistics of Word Cooccurrences:
                          Word Pairs and Collocations. University of Stuttgart.</li>
                          <li>McEnery, T., & Hardie, A. (2012). Corpus Linguistics:
                          Method, Theory and Practice. Cambridge University Press.</li>
                        </ul>"
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
  )
  # )
}

# ============================================================================
# SERVER LOGIC
# ============================================================================

collocationServer <- function(input, output, session, values, statsValues) {
  # Reactive values to store results
  collocResults <- reactiveValues(
    plot_data = NULL,
    collocate_data = NULL
  )

  # ============================================================================
  # TAB 1: PLOT - Word Distribution
  # ============================================================================

  # Perform plot search
  observeEvent(input$collocPlotSearch, {
    req(values$dfTag)
    req(input$collocPlotQuery)
    req(values$generalTerm)

    # Get search parameters
    query <- input$collocPlotQuery
    case_sensitive <- input$collocPlotCase

    # Calculate plot data
    collocResults$plot_data <- calculatePlotDistribution(
      dfTag = values$dfTag,
      term_col = values$generalTerm,
      query = query,
      case_sensitive = case_sensitive
    )
  })

  # Display plot summary
  output$collocPlotSummary <- renderUI({
    req(collocResults$plot_data)

    data <- collocResults$plot_data
    total_hits <- sum(data$freq, na.rm = TRUE)
    total_docs <- nrow(data)
    docs_with_hits <- sum(data$freq > 0, na.rm = TRUE)

    HTML(paste0(
      "<div style='background: #e8f5e9; padding: 15px; border-radius: 5px; border-left: 4px solid #4F7942; margin-top: 20px;'>",
      "<h4 style='margin-top: 0; color: #4F7942;'>Search Results</h4>",
      "<p style='margin: 5px 0;'><b>Target Word:</b> ",
      input$collocPlotQuery,
      "</p>",
      "<p style='margin: 5px 0;'><b>Total Hits:</b> ",
      format(total_hits, big.mark = ","),
      "</p>",
      "<p style='margin: 5px 0;'><b>Documents with Hits:</b> ",
      docs_with_hits,
      " / ",
      total_docs,
      " (",
      round(docs_with_hits / total_docs * 100, 1),
      "%)</p>",
      "</div>"
    ))
  })

  # Display plot table with inline plots
  output$collocPlotTable <- DT::renderDT(
    {
      req(collocResults$plot_data)

      # Sort data
      data <- collocResults$plot_data
      sort_by <- input$collocPlotSort

      if (sort_by == "freq") {
        data <- data %>% arrange(desc(freq))
      } else if (sort_by == "dispersion") {
        data <- data %>% arrange(desc(dispersion))
      } else if (sort_by == "doc_id") {
        data <- data %>% arrange(doc_id)
      }

      # Add row numbers
      data <- data %>%
        mutate(
          Row = row_number(),
          # Calculate normalized frequency (per million tokens)
          norm_freq = round((freq / doc_length) * 1000000, 2),
          # Create inline plot HTML
          Plot = createInlinePlot(
            hit_positions,
            doc_length,
            300
            # input$collocPlotZoom
          ),
          # Add View button
          View = sprintf(
            '<button class="btn btn-sm btn-primary" onclick="Shiny.setInputValue(\'collocPlotViewDoc\', \'%s\', {priority: \'event\'});" style="padding: 2px 8px; font-size: 11px;">
            <i class="fa fa-eye"></i> View
          </button>',
            doc_id
          )
        )

      # Prepare display data
      display_data <- data %>%
        select(
          Row,
          doc_id,
          doc_length,
          freq,
          norm_freq,
          dispersion,
          Plot,
          View
        ) %>%
        rename(
          "Doc ID" = doc_id,
          "Doc Tokens" = doc_length,
          "Freq" = freq,
          "Norm Freq" = norm_freq,
          "Dispersion" = dispersion
        ) %>%
        dplyr::filter(Freq > 0) # Show only documents with hits

      # Create DT table
      DT::datatable(
        display_data,
        selection = "none",
        extensions = "Buttons",
        escape = FALSE, # Important: allow HTML in Plot and View columns
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 20, 50),
          scrollX = TRUE,
          dom = "Blfrtip",
          buttons = list(
            list(
              extend = 'excel',
              text = '<i class="fa fa-file-excel"></i> Excel',
              className = 'btn btn-success',
              exportOptions = list(
                modifier = list(page = "all") # Esporta tutte le pagine, non solo quella corrente
              ),
              filename = paste0("In_Documents_Table_", Sys.Date())
            )
          ),
          columnDefs = list(
            list(width = paste0(input$collocPlotZoom, "px"), targets = 6), # Plot column
            list(width = "80px", targets = 7), # View column
            list(className = "dt-center", targets = 0:7)
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#4F7942', 'color': '#fff'});",
            "}"
          )
        ),
        class = "display compact"
      ) %>%
        DT::formatStyle(
          columns = 1:8,
          fontSize = "12px"
        )
    },
    server = FALSE
  )

  # Handle View button click - show document in modal
  observeEvent(input$collocPlotViewDoc, {
    req(collocResults$plot_data)
    req(input$collocPlotViewDoc)
    req(values$dfTag)

    doc_id <- input$collocPlotViewDoc

    # Get document data
    doc_df <- values$dfTag %>%
      dplyr::filter(docSelected, doc_id == !!doc_id) %>%
      arrange(sentence_id, term_id)

    if (nrow(doc_df) == 0) {
      showNotification("Document not found", type = "error")
      return()
    }

    # Get search parameters
    term_col <- values$generalTerm
    query <- input$collocPlotQuery
    case_sensitive <- input$collocPlotCase

    # Prepare search term
    search_col <- doc_df[[term_col]]
    if (!case_sensitive) {
      search_col <- tolower(search_col)
      query_lower <- tolower(query)
    } else {
      query_lower <- query
    }

    # Mark matches
    doc_df <- doc_df %>%
      mutate(
        is_match = search_col == query_lower
      )

    # Build HTML with highlighted terms
    html_content <- buildDocumentHTML(doc_df, term_col)

    # Show modal
    showModal(
      modalDialog(
        title = tags$div(
          style = "color: #4F7942;",
          tags$h4(
            tags$i(class = "fa fa-file-text-o"),
            " Document View: ",
            doc_id,
            style = "color: #FFFFFF;"
          )
        ),
        tags$div(
          style = "max-height: 70vh; overflow-y: auto; padding: 20px;
                   background: #fff; border: 1px solid #ddd; border-radius: 5px;",
          HTML(html_content)
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })

  # ============================================================================
  # TAB 2: DOCUMENT VIEW
  # ============================================================================

  # ============================================================================
  # TAB 2: COLLOCATE
  # ============================================================================

  # Perform collocate search
  observeEvent(input$collocCollocSearch, {
    req(values$dfTag)
    req(input$collocCollocQuery)
    req(values$generalTerm)

    # Get search parameters
    term_col <- values$generalTerm
    query <- input$collocCollocQuery
    case_sensitive <- input$collocCollocCase
    left_span <- input$collocCollocLeft
    right_span <- input$collocCollocRight
    min_freq <- input$collocCollocMinFreq

    # Calculate collocates
    collocResults$collocate_data <- calculateCollocates(
      dfTag = values$dfTag,
      term_col = term_col,
      query = query,
      case_sensitive = case_sensitive,
      left_span = left_span,
      right_span = right_span,
      min_freq = min_freq
    )
  })

  # Display collocate summary
  output$collocCollocSummary <- renderUI({
    req(collocResults$collocate_data)

    data <- collocResults$collocate_data
    n_collocates <- nrow(data)

    HTML(paste0(
      "<div style='background: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #4F7942;'>",
      "<h4 style='margin-top: 0; color: #4F7942;'>Collocate Results</h4>",
      "<p style='margin: 5px 0;'><b>Target Word:</b> ",
      input$collocCollocQuery,
      "</p>",
      "<p style='margin: 5px 0;'><b>Collocates Found:</b> ",
      format(n_collocates, big.mark = ","),
      "</p>",
      "<p style='margin: 5px 0;'><b>Window:</b> ",
      input$collocCollocLeft,
      "L - ",
      input$collocCollocRight,
      "R</p>",
      "<hr style='margin: 10px 0; border: none; border-top: 1px solid #ddd;'>",
      "<p style='margin: 5px 0; font-size: 11px;'><b>Legend:</b> ",
      "<br>",
      "<span style='background: #d4edda; padding: 2px 6px; border-radius: 3px; font-weight: bold;'Significant</span> ",
      "MI > 3 | LogLik > 3.84 | T-Score > 2 &nbsp;&nbsp; ",
      "<br>",
      "<span style='background: #a8d5ba; padding: 2px 6px; border-radius: 3px; font-weight: bold;'Highly Significant</span> ",
      "LogLik > 15.13",
      "</p>",
      "</div>"
    ))
  })

  # Display collocates table
  output$collocCollocTable <- DT::renderDT(
    {
      req(collocResults$collocate_data)

      data <- collocResults$collocate_data

      # Create DT table with custom formatting
      dt <- DT::datatable(
        data,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          lengthMenu = c(10, 20, 50, 100),
          scrollX = TRUE,
          dom = "Blfrtip",
          buttons = list(
            list(
              extend = 'excel',
              text = '<i class="fa fa-file-excel"></i> Excel',
              className = 'btn btn-success',
              exportOptions = list(
                modifier = list(page = "all") # Esporta tutte le pagine, non solo quella corrente
              ),
              filename = paste0("Collocation_Table_", Sys.Date())
            )
          ),
          columnDefs = list(
            list(className = "dt-center", targets = 1:7)
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#4F7942', 'color': '#fff'});",
            "}"
          )
        ),
        class = "display compact"
      ) %>%
        # Format MI column: > 3 is significant
        DT::formatStyle(
          "MI",
          backgroundColor = styleInterval(
            cuts = c(3),
            values = c("white", "#d4edda") # Light green for significant
          ),
          fontWeight = styleInterval(
            cuts = c(3),
            values = c("normal", "bold")
          )
        ) %>%
        # Format LogLik column: > 3.84 significant, > 15.13 highly significant
        DT::formatStyle(
          "LogLik",
          backgroundColor = styleInterval(
            cuts = c(3.84, 15.13),
            values = c("white", "#d4edda", "#a8d5ba") # Light green -> darker green
          ),
          fontWeight = styleInterval(
            cuts = c(3.84),
            values = c("normal", "bold")
          )
        ) %>%
        # Format TScore column: > 2 is significant
        DT::formatStyle(
          "TScore",
          backgroundColor = styleInterval(
            cuts = c(2),
            values = c("white", "#d4edda") # Light green for significant
          ),
          fontWeight = styleInterval(
            cuts = c(2),
            values = c("normal", "bold")
          )
        ) %>%
        DT::formatStyle(
          columns = 0:7,
          fontSize = "12px"
        )

      return(dt)
    },
    server = FALSE
  )

  # ============================================================================
  # TAB 3: KWIC NETWORK
  # ============================================================================

  ## KWIC Network ----
  observe({
    req(values$dfTag)

    term <- values$generalTerm
    words <- values$dfTag %>%
      LemmaSelection() %>%
      select(all_of(term)) %>%
      pull() %>%
      unique() %>%
      sort() %>%
      tolower()
    updateSelectizeInput(
      session,
      "wordsContSearch",
      choices = c("", words),
      selected = "",
      server = TRUE
    )
  })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$wordsContReset
    },
    handlerExpr = {
      req(values$dfTag)
      values$wordInContest <- data.frame()
      term <- values$generalTerm
      words <- values$dfTag %>%
        LemmaSelection() %>%
        select(all_of(term)) %>%
        pull() %>%
        unique() %>%
        sort() %>%
        tolower()
      updateSelectizeInput(
        session,
        "wordsContSearch",
        choices = c("", words),
        selected = "",
        server = TRUE
      )
    }
  )

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$wordsContSave
    },
    handlerExpr = {
      req(values$wordInContext)
      file_path <- destFolder(
        paste("Tall-WordsInContext-", sys.time(), ".xlsx", sep = ""),
        values$wdTall
      )
      openxlsx::write.xlsx(
        x = values$wordInContext,
        file = file_path,
        colNames = TRUE
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  wordsInContextMenu <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$wordsContApply
    },
    valueExpr = {
      if (input$wordsContSearch != "") {
        word_search <- req(tolower(trimws(input$wordsContSearch)))
        values$wordInContext <- get_context_window(
          values$dfTag,
          target_word = word_search,
          n_left = input$wordsContBefore,
          n_right = input$wordsContAfter,
          term = values$generalTerm
        )
        if (nrow(values$wordInContext) > 1) {
          values$contextNetwork <- contextNetwork(
            df = values$wordInContext,
            dfTag = values$dfTag,
            target_word = word_search,
            n = input$wordsContN
          )
        } else {
          values$contextNetwork <- NULL
        }
      } else {
        values$wordInContest <- NULL
      }
    }
  )

  # Display collocate summary
  output$collocNetworkSummary <- renderUI({
    req(values$wordInContext)

    n_collocates <- nrow(values$wordInContext)

    HTML(paste0(
      "<div style='background: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #4F7942;'>",
      "<h4 style='margin-top: 0; color: #4F7942;'>KWIC Network Results</h4>",
      "<p style='margin: 5px 0;'><b>Target Word:</b> ",
      tolower(trimws(input$wordsContSearch)),
      "</p>",
      "<p style='margin: 5px 0;'><b>Collocates Found:</b> ",
      format(n_collocates, big.mark = ","),
      "</p>",
      "<p style='margin: 5px 0;'><b>Window:</b> ",
      input$wordsContBefore,
      "L - ",
      input$wordsContAfter,
      "R</p>",
      "</div>"
    ))
  })

  output$wordsContHtml <- renderUI({
    wordsInContextMenu()
    req(values$wordInContext)
    if (nrow(values$wordInContext) == 0) {
      return(HTML("<p>No results found.</p>"))
    }

    content <- lapply(1:nrow(values$wordInContext), function(i) {
      row <- values$wordInContext[i, ]

      div(
        style = "display: flex; align-items: center; margin-bottom: 5px; border-bottom: 1px solid #eee; padding: 5px 0; width: 100%;",
        # Document ID
        span(
          style = "color: darkblue; width: 150px; font-weight: bold; flex-shrink: 0; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
          row$doc_id
        ),
        # Context Before
        span(
          style = "color: gray; text-align: right; flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding-right: 10px;",
          paste0(unlist(row$context_before), collapse = " ")
        ),
        # Target Word
        span(
          style = "color: #4F7942; font-weight: bold; flex-shrink: 0; padding: 0 10px; min-width: 80px; text-align: center;",
          row$target_word
        ),
        # Context After
        span(
          style = "color: gray; text-align: left; flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding-left: 10px;",
          paste0(unlist(row$context_after), collapse = " ")
        )
      )
    })

    do.call(tagList, content)
  })

  # output$wordsContHtml <- renderUI({
  #   wordsInContextMenu()
  #   req(values$wordInContext)
  #   if (nrow(values$wordInContext) == 0) {
  #     return(HTML("<p>No results found.</p>"))
  #   }
  #
  #   content <- lapply(1:nrow(values$wordInContext), function(i) {
  #     row <- values$wordInContext[i, ]
  #     before <- paste(unlist(row$context_before), collapse = " ")
  #     div(
  #       style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
  #       span(
  #         style = "color: darkblue; text-align: left; width: 150px; font-weight: bold;",
  #         row$doc_id
  #       ),
  #       span(
  #         style = "color: gray; text-align: right; flex: 1;",
  #         paste0(unlist(row$context_before), collapse = " ")
  #       ),
  #       span(
  #         style = "color: #4F7942; font-weight: bold; padding: 0 10px;",
  #         row$target_word
  #       ),
  #       span(
  #         style = "color: gray; text-align: left; flex: 1;",
  #         paste0(unlist(row$context_after), collapse = " ")
  #       )
  #     )
  #   })
  #
  #   do.call(tagList, content)
  # })

  output$wordsContNetwork <- renderVisNetwork({
    wordsInContextMenu()
    req(values$contextNetwork)
    values$contextNetwork
  })

  output$ContextGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "Gemini AI", content = values$contextGemini, values)
  })

  ## export Network button
  observeEvent(
    eventExpr = {
      input$wordContNetExport
    },
    handlerExpr = {
      file <- paste("KWICNetwork-", sys.time(), ".png", sep = "")
      file <- destFolder(file, values$wdTall)
      plot2png(values$contextNetwork, filename = file, zoom = values$zoom)
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report
  observeEvent(input$wordContNetReport, {
    if (!is.null(values$wordInContext)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "KWICNetwork"
      list_df <- list(
        values$wordInContext
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$filenetVis <- plot2png(
        values$contextNetwork,
        filename = "KWIC-Network.png",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$filenetVis, res$col)
      )
      popUp(title = "KWIC Network Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })
}

# ----------------------------------------------------------------------------
# Build HTML for document view with highlighted target words
# ----------------------------------------------------------------------------
buildDocumentHTML <- function(doc_df, term_col) {
  # Group by paragraph and sentence
  sentences <- doc_df %>%
    group_by(paragraph_id, sentence_id) %>%
    summarise(
      sentence_tokens = list(token),
      is_match_list = list(is_match),
      .groups = "drop"
    ) %>%
    arrange(paragraph_id, sentence_id)

  # Build HTML for each sentence
  sentences <- sentences %>%
    rowwise() %>%
    mutate(
      sentence_html = {
        tokens <- sentence_tokens
        matches <- is_match_list

        # Create HTML for each token
        token_html <- sapply(1:length(tokens), function(j) {
          token <- tokens[j]
          is_match <- matches[j]

          if (is_match) {
            # Highlight matched token
            paste0(
              '<span style="background-color: #ffeb3b; font-weight: bold; padding: 2px 4px; border-radius: 3px;">',
              htmltools::htmlEscape(token),
              '</span>'
            )
          } else {
            htmltools::htmlEscape(token)
          }
        })

        # Join tokens with spaces
        paste(token_html, collapse = " ")
      }
    ) %>%
    ungroup()

  # Group by paragraph and combine sentences
  paragraphs <- sentences %>%
    group_by(paragraph_id) %>%
    summarise(
      paragraph_html = paste(sentence_html, collapse = " "),
      .groups = "drop"
    ) %>%
    arrange(paragraph_id)

  # Build final HTML with proper paragraph spacing
  html_paragraphs <- sapply(1:nrow(paragraphs), function(i) {
    paste0(
      '<p style="line-height: 1.8; margin-bottom: 20px; text-align: justify;">',
      paragraphs$paragraph_html[i],
      '</p>'
    )
  })

  # Combine all paragraphs
  full_html <- paste0(
    '<div style="font-family: \'Georgia\', serif; font-size: 14px; color: #333;">',
    paste(html_paragraphs, collapse = "\n"),
    '</div>'
  )

  return(full_html)
}

# ============================================================================
# ANALYSIS FUNCTIONS
# ============================================================================

# ----------------------------------------------------------------------------
# Calculate plot distribution of target word across documents
# ----------------------------------------------------------------------------
calculatePlotDistribution <- function(dfTag, term_col, query, case_sensitive) {
  # Filter selected documents
  df <- dfTag %>% dplyr::filter(docSelected)

  # Prepare search term
  if (!case_sensitive) {
    df[[term_col]] <- tolower(df[[term_col]])
    query <- tolower(query)
  }

  # Find matches
  df <- df %>%
    mutate(is_match = .data[[term_col]] == query)

  # Calculate document-level statistics
  # Exclude PUNCT, NUM, SYM, X from document length count
  doc_stats <- df %>%
    group_by(doc_id) %>%
    summarise(
      freq = sum(is_match),
      # Count only content words (exclude punctuation, numbers, symbols)
      doc_length = sum(!upos %in% c("PUNCT", "NUM", "SYM", "X")),
      # Get actual token positions (not just indices)
      hit_positions = list(which(is_match)),
      first_pos = ifelse(any(is_match), min(which(is_match)), NA),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      # Calculate dispersion (within-document distribution evenness)
      # Using Juilland's D-like measure: how evenly distributed hits are in the document
      dispersion = {
        pos <- hit_positions
        n_hits <- freq
        doc_len <- doc_length

        # If no hits or only one hit, dispersion is 0
        if (is.na(n_hits) || n_hits <= 1) {
          0
        } else {
          # Divide document into segments (e.g., 10 parts)
          n_segments <- min(10, doc_len)
          segment_size <- doc_len / n_segments

          # Count hits in each segment
          segment_counts <- sapply(1:n_segments, function(seg) {
            seg_start <- (seg - 1) * segment_size
            seg_end <- seg * segment_size
            sum(pos > seg_start & pos <= seg_end)
          })

          # Expected count per segment if perfectly distributed
          expected_count <- n_hits / n_segments

          # Calculate coefficient of variation
          if (expected_count > 0) {
            observed_variance <- sum((segment_counts - expected_count)^2) /
              n_segments
            cv <- sqrt(observed_variance) / expected_count

            # Normalize to 0-1 scale (lower CV = higher dispersion)
            # Perfect dispersion (CV=0) -> D=1, High concentration (high CV) -> D=0
            dispersion_value <- 1 / (1 + cv)
          } else {
            dispersion_value <- 0
          }

          round(dispersion_value, 3)
        }
      }
    ) %>%
    ungroup()

  return(doc_stats)
}

# ----------------------------------------------------------------------------
# Create inline plot HTML for table display (mini barcode plot)
# ----------------------------------------------------------------------------
createInlinePlot <- function(hit_positions_list, doc_length, width = 150) {
  sapply(seq_along(hit_positions_list), function(i) {
    positions <- hit_positions_list[[i]]
    length <- doc_length[i]

    if (length(positions) == 0 || length == 0) {
      # No hits - show empty bar
      return(paste0(
        '<svg width="',
        width,
        '" height="20" style="background: #f0f0f0; border: 1px solid #ddd;">',
        '</svg>'
      ))
    }

    # Normalize positions to 0-width scale (0-1 first, then to pixel width)
    normalized_pos <- (positions / length) * width

    # Ensure positions are within bounds
    normalized_pos <- pmin(pmax(normalized_pos, 0), width)

    # Create SVG with hit markers
    svg <- paste0(
      '<svg width="',
      width,
      '" height="20" style="background: #f0f0f0; border: 1px solid #ccc;">',
      # Add a light gray background rectangle
      '<rect x="0" y="0" width="',
      width,
      '" height="20" fill="#f0f0f0"/>',
      # Add border
      '<rect x="0" y="0" width="',
      width,
      '" height="20" fill="none" stroke="#ccc" stroke-width="1"/>',
      # Add vertical lines for each hit
      paste(
        sapply(normalized_pos, function(pos) {
          paste0(
            '<line x1="',
            pos,
            '" y1="2" x2="',
            pos,
            '" y2="18" ',
            'stroke="#4F7942" stroke-width="2"/>'
          )
        }),
        collapse = ""
      ),
      '</svg>'
    )

    return(svg)
  })
}

# ----------------------------------------------------------------------------
# Calculate collocates
# ----------------------------------------------------------------------------
calculateCollocates <- function(
  dfTag,
  term_col,
  query,
  case_sensitive,
  left_span,
  right_span,
  min_freq
) {
  # Filter selected documents and exclude punctuation, numbers, symbols
  df <- dfTag %>%
    dplyr::filter(docSelected) %>%
    filter(!upos %in% c("PUNCT", "NUM", "SYM", "X")) %>%
    arrange(doc_id, sentence_id, token_id)

  # Prepare search term
  if (!case_sensitive) {
    df[[term_col]] <- tolower(df[[term_col]])
    query <- tolower(query)
  }

  # Find target word positions
  target_positions <- which(df[[term_col]] == query)

  if (length(target_positions) == 0) {
    return(data.frame(
      collocate = character(),
      upos = character(),
      freq = numeric(),
      left_freq = numeric(),
      right_freq = numeric(),
      MI = numeric(),
      LogLik = numeric(),
      TScore = numeric()
    ))
  }

  # Extract collocates within window
  collocates_list <- lapply(target_positions, function(pos) {
    # Get window boundaries
    left_start <- max(1, pos - left_span)
    left_end <- pos - 1
    right_start <- pos + 1
    right_end <- min(nrow(df), pos + right_span)

    # Extract collocates
    left_collocates <- if (left_start <= left_end) {
      data.frame(
        collocate = df[[term_col]][left_start:left_end],
        upos = df[["upos"]][left_start:left_end],
        position = "left",
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(collocate = character(), position = character())
    }

    right_collocates <- if (right_start <= right_end) {
      data.frame(
        collocate = df[[term_col]][right_start:right_end],
        upos = df[["upos"]][right_start:right_end],
        position = "right",
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(collocate = character(), position = character())
    }

    rbind(left_collocates, right_collocates)
  })

  # upos selected
  pos_selected <- df %>%
    dplyr::filter(POSSelected) %>%
    dplyr::pull(upos) %>%
    unique()

  # Combine all collocates
  collocates <- bind_rows(collocates_list) %>%
    dplyr::filter(upos %in% pos_selected)

  # Calculate frequencies
  colloc_stats <- collocates %>%
    dplyr::filter(!is.na(collocate), collocate != query) %>%
    group_by(collocate, upos) %>%
    summarise(
      freq = n(),
      left_freq = sum(position == "left"),
      right_freq = sum(position == "right"),
      .groups = "drop"
    ) %>%
    dplyr::filter(freq >= min_freq)

  # Calculate ALL statistical measures
  if (nrow(colloc_stats) > 0) {
    # Corpus statistics for calculations
    N <- nrow(df) # Total tokens
    f_target <- length(target_positions) # Target frequency

    # Pre-compute corpus frequencies via left_join (avoids O(n*m) full scans)
    term_freq <- df %>%
      dplyr::count(.data[[term_col]], name = "f_colloc") %>%
      dplyr::rename(collocate = 1)

    colloc_stats <- colloc_stats %>%
      left_join(term_freq, by = "collocate") %>%
      mutate(
        f_colloc = ifelse(is.na(f_colloc), 0L, f_colloc),
        # Calculate contingency table values
        O11 = freq, # Observed co-occurrence
        O12 = pmax(0, f_target - freq), # Target without collocate
        O21 = pmax(0, f_colloc - freq), # Collocate without target
        O22 = pmax(0, N - f_target - f_colloc + freq), # Neither
        # Expected frequencies
        E11 = (O11 + O12) * (O11 + O21) / N,
        E12 = (O11 + O12) * (O12 + O22) / N,
        E21 = (O21 + O22) * (O11 + O21) / N,
        E22 = (O21 + O22) * (O12 + O22) / N,
        # MI Score
        MI = ifelse(is.na(E11) | E11 == 0, 0, log2(O11 / E11)),
        # Log-Likelihood (vectorized with safe log)
        LogLik = 2 * (
          ifelse(!is.na(O11) & !is.na(E11) & O11 > 0 & E11 > 0, O11 * log(O11 / E11), 0) +
          ifelse(!is.na(O12) & !is.na(E12) & O12 > 0 & E12 > 0, O12 * log(O12 / E12), 0) +
          ifelse(!is.na(O21) & !is.na(E21) & O21 > 0 & E21 > 0, O21 * log(O21 / E21), 0) +
          ifelse(!is.na(O22) & !is.na(E22) & O22 > 0 & E22 > 0, O22 * log(O22 / E22), 0)
        ),
        # T-Score
        TScore = ifelse(is.na(E11) | E11 == 0 | is.na(O11) | O11 == 0,
          0, (O11 - E11) / sqrt(O11))
      ) %>%
      select(
        collocate,
        upos,
        freq,
        left_freq,
        right_freq,
        MI,
        LogLik,
        TScore
      ) %>%
      mutate(
        MI = round(MI, 3),
        LogLik = round(LogLik, 3),
        TScore = round(TScore, 3)
      ) %>%
      arrange(desc(MI))
  } else {
    # Ensure expected columns exist even when empty
    colloc_stats$MI <- numeric(0)
    colloc_stats$LogLik <- numeric(0)
    colloc_stats$TScore <- numeric(0)
  }

  return(colloc_stats)
}

# ----------------------------------------------------------------------------
# Calculate statistical measures for collocates

# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# DTformat helper function (if not already defined in tallFunctions.R)
# ----------------------------------------------------------------------------
# This function should already exist in the TALL project
# Included here for completeness
if (!exists("DTformat")) {
  DTformat <- function(data, nrow = 20, filename = "data", ...) {
    DT::datatable(
      data,
      options = list(
        pageLength = nrow,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "csv", filename = filename),
          list(extend = "excel", filename = filename)
        )
      ),
      extensions = "Buttons",
      ...
    )
  }
}

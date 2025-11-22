### FEATURE ROLES MODULE ####
# UI and Server logic for Feature Roles functionality

#### UI Function ####
featureRolesUI <- function() {
  tabItem(
    tabName = "feature_roles",
    fluidPage(
      # Page Header
      fluidRow(
        column(
          12,
          div(
            h2(
              icon("tags", lib = "glyphicon"),
              strong("Feature Roles"),
              style = "color: #4F7942; text-align: center; margin-bottom: 20px;"
            ),
            p(
              "Assign roles to features for different types of text analysis.",
              style = "text-align: center; font-size: 16px; color: #666; margin-bottom: 30px;"
            )
          )
        )
      ),

      fluidRow(
        tabsetPanel(
          type = "tabs",

          # Main Panel - Feature Role Assignment
          tabPanel(
            "Role Assignment",
            fluidRow(
              # Main Content Area
              column(
                9,
                div(
                  style = "margin-top: 20px;",

                  # Time Variable Box
                  box(
                    title = strong(
                      icon("time", lib = "glyphicon"),
                      " Time Variable",
                      style = "font-size: 18px;"
                    ),
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,

                    p(
                      "Select a numeric or date variable to represent time for diachronic text analysis (e.g., longitudinal topic models).",
                      style = "color: #666; margin-bottom: 15px;"
                    ),

                    fluidRow(
                      column(
                        6,
                        uiOutput("timeVarSelect")
                      ),
                      column(
                        3,
                        div(
                          style = "margin-top: 25px;",
                          actionBttn(
                            inputId = "previewTimeVar",
                            label = "Preview Values",
                            style = "bordered",
                            color = "primary",
                            size = "sm",
                            icon = icon("eye-open", lib = "glyphicon")
                          )
                        )
                      ),
                      column(
                        3,
                        div(
                          style = "margin-top: 25px;",
                          actionBttn(
                            inputId = "clearTimeVar",
                            label = "Clear",
                            style = "bordered",
                            color = "danger",
                            size = "sm",
                            icon = icon("remove", lib = "glyphicon")
                          )
                        )
                      )
                    ),

                    # Display current selection
                    conditionalPanel(
                      condition = "output.timeVarSelected",
                      div(
                        style = "margin-top: 15px; padding: 10px; background-color: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 4px;",
                        icon(
                          "ok",
                          lib = "glyphicon",
                          style = "color: #4caf50;"
                        ),
                        strong(" Current selection: "),
                        textOutput("timeVarDisplay", inline = TRUE)
                      )
                    )
                  ),

                  # Label Variable Box
                  box(
                    title = strong(
                      icon("tag", lib = "glyphicon"),
                      " Label Variable",
                      style = "font-size: 18px;"
                    ),
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,

                    p(
                      "Select a variable that represents the response in supervised text classification models (e.g., Random Forest, SVM).",
                      style = "color: #666; margin-bottom: 15px;"
                    ),

                    fluidRow(
                      column(
                        6,
                        uiOutput("labelVarSelect")
                      ),
                      column(
                        3,
                        div(
                          style = "margin-top: 25px;",
                          actionBttn(
                            inputId = "previewLabelVar",
                            label = "Preview Values",
                            style = "bordered",
                            color = "primary",
                            size = "sm",
                            icon = icon("eye-open", lib = "glyphicon")
                          )
                        )
                      ),
                      column(
                        3,
                        div(
                          style = "margin-top: 25px;",
                          actionBttn(
                            inputId = "clearLabelVar",
                            label = "Clear",
                            style = "bordered",
                            color = "danger",
                            size = "sm",
                            icon = icon("remove", lib = "glyphicon")
                          )
                        )
                      )
                    ),

                    # Display current selection
                    conditionalPanel(
                      condition = "output.labelVarSelected",
                      div(
                        style = "margin-top: 15px; padding: 10px; background-color: #e3f2fd; border-left: 4px solid #2196f3; border-radius: 4px;",
                        icon(
                          "ok",
                          lib = "glyphicon",
                          style = "color: #2196f3;"
                        ),
                        strong(" Current selection: "),
                        textOutput("labelVarDisplay", inline = TRUE)
                      )
                    )
                  ),

                  # Keyness Group Variable Box
                  box(
                    title = strong(
                      icon("transfer", lib = "glyphicon"),
                      " Keyness Group Variable",
                      style = "font-size: 18px;"
                    ),
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,

                    p(
                      "Select a variable that divides the corpus into two subcorpora for keyness analysis and comparison.",
                      style = "color: #666; margin-bottom: 15px;"
                    ),

                    fluidRow(
                      column(
                        6,
                        uiOutput("keynessVarSelect")
                      ),
                      column(
                        3,
                        div(
                          style = "margin-top: 25px;",
                          actionBttn(
                            inputId = "previewKeynessVar",
                            label = "Preview Values",
                            style = "bordered",
                            color = "primary",
                            size = "sm",
                            icon = icon("eye-open", lib = "glyphicon")
                          )
                        )
                      ),
                      column(
                        3,
                        div(
                          style = "margin-top: 25px;",
                          actionBttn(
                            inputId = "clearKeynessVar",
                            label = "Clear",
                            style = "bordered",
                            color = "danger",
                            size = "sm",
                            icon = icon("remove", lib = "glyphicon")
                          )
                        )
                      )
                    ),

                    # Display current selection
                    conditionalPanel(
                      condition = "output.keynessVarSelected",
                      div(
                        style = "margin-top: 15px; padding: 10px; background-color: #fff3e0; border-left: 4px solid #ff9800; border-radius: 4px;",
                        icon(
                          "ok",
                          lib = "glyphicon",
                          style = "color: #ff9800;"
                        ),
                        strong(" Current selection: "),
                        textOutput("keynessVarDisplay", inline = TRUE)
                      )
                    ),

                    # Group assignment UI (shown only if variable has more than 2 categories)
                    uiOutput("keynessGroupAssignment")
                  )
                )
              ),

              # Sidebar - Summary Panel
              column(
                3,
                div(
                  style = "margin-top: 20px;",
                  box(
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",

                    div(
                      class = "box-header with-border",
                      h4(
                        icon("list-alt", lib = "glyphicon"),
                        strong("Summary"),
                        style = "margin: 0; color: #4F7942;"
                      )
                    ),

                    div(
                      class = "box-body",

                      # Available variables count
                      div(
                        style = "margin-bottom: 20px; padding: 15px; background-color: #f5f5f5; border-radius: 5px;",
                        h5(
                          strong("Available Features"),
                          style = "margin-top: 0; color: #333;"
                        ),
                        h3(
                          textOutput("availableVarsCount"),
                          style = "margin: 10px 0; color: #4F7942; text-align: center;"
                        )
                      ),

                      hr(style = "border-color: #e0e0e0; margin: 20px 0;"),

                      # Quick summary
                      div(
                        h5(
                          strong("Current Assignments"),
                          style = "color: #666; margin-bottom: 15px;"
                        ),

                        div(
                          style = "padding: 10px; background-color: #fff; border: 1px solid #e0e0e0; border-radius: 5px;",

                          div(
                            style = "margin-bottom: 10px;",
                            icon(
                              "time",
                              lib = "glyphicon",
                              style = "color: #3f51b5;"
                            ),
                            strong(" Time: "),
                            br(),
                            span(
                              uiOutput("summaryTimeVar", inline = TRUE),
                              style = "color: #666; font-size: 13px;"
                            )
                          ),

                          div(
                            style = "margin-bottom: 10px;",
                            icon(
                              "tag",
                              lib = "glyphicon",
                              style = "color: #00bcd4;"
                            ),
                            strong(" Label: "),
                            br(),
                            span(
                              uiOutput("summaryLabelVar", inline = TRUE),
                              style = "color: #666; font-size: 13px;"
                            )
                          ),

                          div(
                            icon(
                              "transfer",
                              lib = "glyphicon",
                              style = "color: #ff9800;"
                            ),
                            strong(" Keyness: "),
                            br(),
                            span(
                              uiOutput("summaryKeynessVar", inline = TRUE),
                              style = "color: #666; font-size: 13px;"
                            )
                          )
                        )
                      ),

                      hr(style = "border-color: #e0e0e0; margin: 20px 0;"),

                      # Action buttons
                      div(
                        actionBttn(
                          inputId = "applyVarRoles",
                          label = strong("Apply Roles"),
                          style = "pill",
                          color = "success",
                          size = "md",
                          block = TRUE,
                          icon = icon("ok", lib = "glyphicon")
                        ),
                        br(),
                        actionBttn(
                          inputId = "resetVarRoles",
                          label = strong("Reset All"),
                          style = "pill",
                          color = "danger",
                          size = "sm",
                          block = TRUE,
                          icon = icon("refresh", lib = "glyphicon")
                        )
                      )
                    )
                  )
                )
              )
            )
          ),

          # Info & References Panel
          tabPanel(
            "Info & References",
            fluidPage(
              fluidRow(
                column(
                  12,
                  div(
                    style = "padding: 30px; background: white; border-radius: 8px;
                       box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-top: 20px;",
                    HTML(infoTexts$featureroles)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}


#### Server Function ####
featureRolesServer <- function(input, output, session, values) {
  # Get available columns (excluding technical columns)
  availableColumns <- reactive({
    req(values$dfTag)
    noGroupLabels(names(values$dfTag))
  })

  # Get available TIME columns (only numeric or date types)
  availableTimeColumns <- reactive({
    req(values$dfTag)
    all_cols <- availableColumns()

    # Filter for numeric or date types
    time_cols <- sapply(all_cols, function(col) {
      col_data <- values$dfTag[[col]]
      is.numeric(col_data) ||
        inherits(col_data, c("Date", "POSIXct", "POSIXlt"))
    })

    all_cols[time_cols]
  })

  # Render available features count
  output$availableVarsCount <- renderText({
    length(availableColumns())
  })

  # Time Variable Selection UI - FILTERED
  output$timeVarSelect <- renderUI({
    choices <- availableTimeColumns()

    if (length(choices) == 0) {
      div(
        style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px;",
        icon("warning-sign", lib = "glyphicon", style = "color: #856404;"),
        span(
          " No numeric or date variables available",
          style = "color: #856404; margin-left: 5px;"
        )
      )
    } else {
      selectInput(
        inputId = "timeVarInput",
        label = "Select time variable:",
        choices = c("None" = "", choices),
        selected = values$timeVariable,
        width = "100%"
      )
    }
  })

  # Label Variable Selection UI
  output$labelVarSelect <- renderUI({
    choices <- availableColumns()

    selectInput(
      inputId = "labelVarInput",
      label = "Select label variable:",
      choices = c("None" = "", choices),
      selected = values$labelVariable,
      width = "100%"
    )
  })

  # Keyness Variable Selection UI
  output$keynessVarSelect <- renderUI({
    choices <- availableColumns()

    selectInput(
      inputId = "keynessVarInput",
      label = "Select keyness group variable:",
      choices = c("None" = "", choices),
      selected = values$keynessVariable,
      width = "100%"
    )
  })

  # Display outputs for conditional panels
  output$timeVarSelected <- reactive({
    !is.null(values$timeVariable) && values$timeVariable != ""
  })
  outputOptions(output, "timeVarSelected", suspendWhenHidden = FALSE)

  output$labelVarSelected <- reactive({
    !is.null(values$labelVariable) && values$labelVariable != ""
  })
  outputOptions(output, "labelVarSelected", suspendWhenHidden = FALSE)

  output$keynessVarSelected <- reactive({
    !is.null(values$keynessVariable) && values$keynessVariable != ""
  })
  outputOptions(output, "keynessVarSelected", suspendWhenHidden = FALSE)

  # Display current selections
  output$timeVarDisplay <- renderText({
    if (!is.null(values$timeVariable) && values$timeVariable != "") {
      values$timeVariable
    } else {
      "None"
    }
  })

  output$labelVarDisplay <- renderText({
    if (!is.null(values$labelVariable) && values$labelVariable != "") {
      values$labelVariable
    } else {
      "None"
    }
  })

  output$keynessVarDisplay <- renderText({
    if (!is.null(values$keynessVariable) && values$keynessVariable != "") {
      values$keynessVariable
    } else {
      "None"
    }
  })

  # Summary outputs - Changed to renderUI for dynamic updates
  output$summaryTimeVar <- renderUI({
    if (!is.null(values$timeVariable) && values$timeVariable != "") {
      HTML(paste0(
        "<span style='color: #4F7942; font-weight: bold;'>",
        values$timeVariable,
        "</span>"
      ))
    } else {
      HTML("<span style='color: #999;'>Not assigned</span>")
    }
  })

  output$summaryLabelVar <- renderUI({
    if (!is.null(values$labelVariable) && values$labelVariable != "") {
      HTML(paste0(
        "<span style='color: #4F7942; font-weight: bold;'>",
        values$labelVariable,
        "</span>"
      ))
    } else {
      HTML("<span style='color: #999;'>Not assigned</span>")
    }
  })

  output$summaryKeynessVar <- renderUI({
    if (!is.null(values$keynessVariable) && values$keynessVariable != "") {
      keyness_text <- values$keynessVariable

      # Add group information if groups are defined
      if (!is.null(values$keynessGroup1) && !is.null(values$keynessGroup2)) {
        group1_text <- paste(values$keynessGroup1, collapse = ", ")
        group2_text <- paste(values$keynessGroup2, collapse = ", ")

        keyness_text <- paste0(
          "<span style='color: #4F7942; font-weight: bold;'>",
          values$keynessVariable,
          "</span><br>",
          "<small style='color: #666;'>",
          "<strong>Group 1:</strong> ",
          group1_text,
          "<br>",
          "<strong>Group 2:</strong> ",
          group2_text,
          "</small>"
        )
      } else {
        keyness_text <- paste0(
          "<span style='color: #4F7942; font-weight: bold;'>",
          keyness_text,
          "</span>"
        )
      }

      HTML(keyness_text)
    } else {
      HTML("<span style='color: #999;'>Not assigned</span>")
    }
  })

  # Preview Time Variable - IMPROVED LAYOUT
  observeEvent(input$previewTimeVar, {
    req(input$timeVarInput, input$timeVarInput != "")

    # Aggregate by document (doc_id) to get one value per document
    doc_level_data <- values$dfTag %>%
      group_by(doc_id) %>%
      summarise(
        var_value = first(.data[[input$timeVarInput]]),
        .groups = "drop"
      )

    var_data <- doc_level_data$var_value
    total_docs <- nrow(doc_level_data)

    # Create summary statistics with improved layout
    if (is.numeric(var_data)) {
      summary_stats <- summary(var_data)
      unique_vals <- length(unique(var_data[!is.na(var_data)]))

      preview_text <- paste0(
        "<div style='text-align: left; padding: 15px;'>",

        # Header section
        "<div style='background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px;'>",
        "<h3 style='margin: 0; font-size: 18px;'><i class='glyphicon glyphicon-time'></i> ",
        input$timeVarInput,
        "</h3>",
        "<p style='margin: 5px 0 0 0; opacity: 0.9; font-size: 13px;'>Time Variable Preview</p>",
        "</div>",

        # Basic info
        "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 15px;'>",
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<tr><td style='padding: 6px; color: #666; font-weight: 600;'>Variable Type:</td><td style='padding: 6px; text-align: right;'><span style='background-color: #667eea; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>Numeric</span></td></tr>",
        "<tr style='background-color: white;'><td style='padding: 6px; color: #666; font-weight: 600;'>Total Documents:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #4F7942;'>",
        format(total_docs, big.mark = ","),
        "</td></tr>",
        "<tr><td style='padding: 6px; color: #666; font-weight: 600;'>Unique Values:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #667eea;'>",
        format(unique_vals, big.mark = ","),
        "</td></tr>",
        "</table>",
        "</div>",

        # Statistics
        "<div style='background-color: #fff; border: 2px solid #e0e0e0; padding: 12px; border-radius: 6px;'>",
        "<h4 style='margin: 0 0 10px 0; color: #333; font-size: 14px; border-bottom: 2px solid #667eea; padding-bottom: 5px;'><i class='glyphicon glyphicon-stats'></i> Descriptive Statistics</h4>",
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<tr><td style='padding: 6px; color: #666;'>Minimum:</td><td style='padding: 6px; text-align: right; font-weight: bold;'>",
        format(round(summary_stats["Min."], 2), big.mark = ","),
        "</td></tr>",
        "<tr style='background-color: #f8f9fa;'><td style='padding: 6px; color: #666;'>1st Quartile:</td><td style='padding: 6px; text-align: right; font-weight: bold;'>",
        format(round(summary_stats["1st Qu."], 2), big.mark = ","),
        "</td></tr>",
        "<tr><td style='padding: 6px; color: #666;'>Median:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #667eea;'>",
        format(round(summary_stats["Median"], 2), big.mark = ","),
        "</td></tr>",
        "<tr style='background-color: #f8f9fa;'><td style='padding: 6px; color: #666;'>Mean:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #4F7942;'>",
        format(round(summary_stats["Mean"], 2), big.mark = ","),
        "</td></tr>",
        "<tr><td style='padding: 6px; color: #666;'>3rd Quartile:</td><td style='padding: 6px; text-align: right; font-weight: bold;'>",
        format(round(summary_stats["3rd Qu."], 2), big.mark = ","),
        "</td></tr>",
        "<tr style='background-color: #f8f9fa;'><td style='padding: 6px; color: #666;'>Maximum:</td><td style='padding: 6px; text-align: right; font-weight: bold;'>",
        format(round(summary_stats["Max."], 2), big.mark = ","),
        "</td></tr>",
        "</table>",
        "</div>",

        "</div>"
      )
    } else {
      freq_table <- table(var_data, useNA = "ifany")
      top_10 <- head(sort(freq_table, decreasing = TRUE), 10)

      # Create table rows for categories
      category_rows <- paste(
        sapply(seq_along(top_10), function(i) {
          bg_color <- if (i %% 2 == 0) "#f8f9fa" else "white"
          paste0(
            "<tr style='background-color: ",
            bg_color,
            ";'>",
            "<td style='padding: 8px; color: #333; font-weight: 500;'>",
            names(top_10)[i],
            "</td>",
            "<td style='padding: 8px; text-align: right; font-weight: bold; color: #4F7942;'>",
            format(top_10[i], big.mark = ","),
            "</td>",
            "<td style='padding: 8px; text-align: right;'>",
            "<span style='background-color: #667eea; color: white; padding: 2px 8px; border-radius: 10px; font-size: 11px;'>",
            round(100 * top_10[i] / sum(freq_table), 1),
            "%",
            "</span></td>",
            "</tr>"
          )
        }),
        collapse = ""
      )

      preview_text <- paste0(
        "<div style='text-align: left; padding: 15px;'>",

        # Header section
        "<div style='background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px;'>",
        "<h3 style='margin: 0; font-size: 18px;'><i class='glyphicon glyphicon-tag'></i> ",
        input$timeVarInput,
        "</h3>",
        "<p style='margin: 5px 0 0 0; opacity: 0.9; font-size: 13px;'>Categorical Variable Preview</p>",
        "</div>",

        # Basic info
        "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 15px;'>",
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<tr><td style='padding: 6px; color: #666; font-weight: 600;'>Variable Type:</td><td style='padding: 6px; text-align: right;'><span style='background-color: #f5576c; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>",
        class(var_data)[1],
        "</span></td></tr>",
        "<tr style='background-color: white;'><td style='padding: 6px; color: #666; font-weight: 600;'>Total Documents:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #4F7942;'>",
        format(total_docs, big.mark = ","),
        "</td></tr>",
        "<tr><td style='padding: 6px; color: #666; font-weight: 600;'>Total Categories:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #f5576c;'>",
        length(unique(var_data[!is.na(var_data)])),
        "</td></tr>",
        "</table>",
        "</div>",

        # Category distribution
        "<div style='background-color: #fff; border: 2px solid #e0e0e0; padding: 12px; border-radius: 6px; max-height: 400px; overflow-y: auto;'>",
        "<h4 style='margin: 0 0 10px 0; color: #333; font-size: 14px; border-bottom: 2px solid #f5576c; padding-bottom: 5px;'><i class='glyphicon glyphicon-list'></i> Top 10 Categories</h4>",
        "<table style='width: 100%; border-collapse: collapse;'>",
        "<thead>",
        "<tr style='background-color: #f5f5f5; border-bottom: 2px solid #ddd;'>",
        "<th style='padding: 8px; text-align: left; color: #666; font-weight: 600;'>Category</th>",
        "<th style='padding: 8px; text-align: right; color: #666; font-weight: 600;'>Documents</th>",
        "<th style='padding: 8px; text-align: right; color: #666; font-weight: 600;'>Percentage</th>",
        "</tr>",
        "</thead>",
        "<tbody>",
        category_rows,
        "</tbody>",
        "</table>",
        "</div>",

        "</div>"
      )
    }

    sendSweetAlert(
      session = session,
      title = NULL,
      text = HTML(preview_text),
      html = TRUE,
      type = NULL,
      width = "600px"
    )
  })

  # Preview Label Variable - IMPROVED LAYOUT
  observeEvent(input$previewLabelVar, {
    req(input$labelVarInput, input$labelVarInput != "")

    # Aggregate by document (doc_id) to get one value per document
    doc_level_data <- values$dfTag %>%
      group_by(doc_id) %>%
      summarise(
        var_value = first(.data[[input$labelVarInput]]),
        .groups = "drop"
      )

    var_data <- doc_level_data$var_value
    total_docs <- nrow(doc_level_data)
    freq_table <- table(var_data, useNA = "ifany")

    # Create table rows for categories
    category_rows <- paste(
      sapply(seq_along(freq_table), function(i) {
        bg_color <- if (i %% 2 == 0) "#f8f9fa" else "white"
        paste0(
          "<tr style='background-color: ",
          bg_color,
          ";'>",
          "<td style='padding: 8px; color: #333; font-weight: 500;'>",
          names(freq_table)[i],
          "</td>",
          "<td style='padding: 8px; text-align: right; font-weight: bold; color: #4F7942;'>",
          format(freq_table[i], big.mark = ","),
          "</td>",
          "<td style='padding: 8px; text-align: right;'>",
          "<span style='background-color: #00bcd4; color: white; padding: 2px 8px; border-radius: 10px; font-size: 11px;'>",
          round(100 * freq_table[i] / sum(freq_table), 1),
          "%",
          "</span></td>",
          "</tr>"
        )
      }),
      collapse = ""
    )

    preview_text <- paste0(
      "<div style='text-align: left; padding: 15px;'>",

      # Header section
      "<div style='background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px;'>",
      "<h3 style='margin: 0; font-size: 18px;'><i class='glyphicon glyphicon-tag'></i> ",
      input$labelVarInput,
      "</h3>",
      "<p style='margin: 5px 0 0 0; opacity: 0.9; font-size: 13px;'>Label Variable Preview</p>",
      "</div>",

      # Basic info
      "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 15px;'>",
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='padding: 6px; color: #666; font-weight: 600;'>Variable Type:</td><td style='padding: 6px; text-align: right;'><span style='background-color: #00bcd4; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>",
      class(var_data)[1],
      "</span></td></tr>",
      "<tr style='background-color: white;'><td style='padding: 6px; color: #666; font-weight: 600;'>Total Documents:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #4F7942;'>",
      format(total_docs, big.mark = ","),
      "</td></tr>",
      "<tr><td style='padding: 6px; color: #666; font-weight: 600;'>Total Categories:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #00bcd4;'>",
      length(unique(var_data[!is.na(var_data)])),
      "</td></tr>",
      "</table>",
      "</div>",

      # Category distribution
      "<div style='background-color: #fff; border: 2px solid #e0e0e0; padding: 12px; border-radius: 6px; max-height: 400px; overflow-y: auto;'>",
      "<h4 style='margin: 0 0 10px 0; color: #333; font-size: 14px; border-bottom: 2px solid #00bcd4; padding-bottom: 5px;'><i class='glyphicon glyphicon-list'></i> Category Distribution</h4>",
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<thead>",
      "<tr style='background-color: #f5f5f5; border-bottom: 2px solid #ddd;'>",
      "<th style='padding: 8px; text-align: left; color: #666; font-weight: 600;'>Category</th>",
      "<th style='padding: 8px; text-align: right; color: #666; font-weight: 600;'>Documents</th>",
      "<th style='padding: 8px; text-align: right; color: #666; font-weight: 600;'>Percentage</th>",
      "</tr>",
      "</thead>",
      "<tbody>",
      category_rows,
      "</tbody>",
      "</table>",
      "</div>",

      "</div>"
    )

    sendSweetAlert(
      session = session,
      title = NULL,
      text = HTML(preview_text),
      html = TRUE,
      type = NULL,
      width = "600px"
    )
  })

  # Preview Keyness Variable - IMPROVED LAYOUT
  observeEvent(input$previewKeynessVar, {
    req(input$keynessVarInput, input$keynessVarInput != "")

    # Aggregate by document (doc_id) to get one value per document
    doc_level_data <- values$dfTag %>%
      group_by(doc_id) %>%
      summarise(
        var_value = first(.data[[input$keynessVarInput]]),
        .groups = "drop"
      )

    var_data <- doc_level_data$var_value
    total_docs <- nrow(doc_level_data)
    freq_table <- table(var_data, useNA = "ifany")

    # Create table rows for categories
    category_rows <- paste(
      sapply(seq_along(freq_table), function(i) {
        bg_color <- if (i %% 2 == 0) "#f8f9fa" else "white"
        paste0(
          "<tr style='background-color: ",
          bg_color,
          ";'>",
          "<td style='padding: 8px; color: #333; font-weight: 500;'>",
          names(freq_table)[i],
          "</td>",
          "<td style='padding: 8px; text-align: right; font-weight: bold; color: #4F7942;'>",
          format(freq_table[i], big.mark = ","),
          "</td>",
          "<td style='padding: 8px; text-align: right;'>",
          "<span style='background-color: #ff9800; color: white; padding: 2px 8px; border-radius: 10px; font-size: 11px;'>",
          round(100 * freq_table[i] / sum(freq_table), 1),
          "%",
          "</span></td>",
          "</tr>"
        )
      }),
      collapse = ""
    )

    preview_text <- paste0(
      "<div style='text-align: left; padding: 15px;'>",

      # Header section
      "<div style='background: linear-gradient(135deg, #fa709a 0%, #fee140 100%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px;'>",
      "<h3 style='margin: 0; font-size: 18px;'><i class='glyphicon glyphicon-transfer'></i> ",
      input$keynessVarInput,
      "</h3>",
      "<p style='margin: 5px 0 0 0; opacity: 0.9; font-size: 13px;'>Keyness Group Variable Preview</p>",
      "</div>",

      # Basic info
      "<div style='background-color: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 15px;'>",
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='padding: 6px; color: #666; font-weight: 600;'>Variable Type:</td><td style='padding: 6px; text-align: right;'><span style='background-color: #ff9800; color: white; padding: 3px 10px; border-radius: 12px; font-size: 12px;'>",
      class(var_data)[1],
      "</span></td></tr>",
      "<tr style='background-color: white;'><td style='padding: 6px; color: #666; font-weight: 600;'>Total Documents:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #4F7942;'>",
      format(total_docs, big.mark = ","),
      "</td></tr>",
      "<tr><td style='padding: 6px; color: #666; font-weight: 600;'>Total Categories:</td><td style='padding: 6px; text-align: right; font-weight: bold; color: #ff9800;'>",
      length(unique(var_data[!is.na(var_data)])),
      "</td></tr>",
      "</table>",
      "</div>",

      # Category distribution
      "<div style='background-color: #fff; border: 2px solid #e0e0e0; padding: 12px; border-radius: 6px; max-height: 400px; overflow-y: auto;'>",
      "<h4 style='margin: 0 0 10px 0; color: #333; font-size: 14px; border-bottom: 2px solid #ff9800; padding-bottom: 5px;'><i class='glyphicon glyphicon-list'></i> Category Distribution</h4>",
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<thead>",
      "<tr style='background-color: #f5f5f5; border-bottom: 2px solid #ddd;'>",
      "<th style='padding: 8px; text-align: left; color: #666; font-weight: 600;'>Category</th>",
      "<th style='padding: 8px; text-align: right; color: #666; font-weight: 600;'>Documents</th>",
      "<th style='padding: 8px; text-align: right; color: #666; font-weight: 600;'>Percentage</th>",
      "</tr>",
      "</thead>",
      "<tbody>",
      category_rows,
      "</tbody>",
      "</table>",
      "</div>",

      "</div>"
    )

    sendSweetAlert(
      session = session,
      title = NULL,
      text = HTML(preview_text),
      html = TRUE,
      type = NULL,
      width = "600px"
    )
  })

  # Keyness Group Assignment UI (for variables with more than 2 categories)
  output$keynessGroupAssignment <- renderUI({
    req(input$keynessVarInput, input$keynessVarInput != "")

    # Get unique values at document level
    doc_level_data <- values$dfTag %>%
      group_by(doc_id) %>%
      summarise(
        var_value = first(.data[[input$keynessVarInput]]),
        .groups = "drop"
      )

    categories <- sort(unique(doc_level_data$var_value[
      !is.na(doc_level_data$var_value)
    ]))

    if (length(categories) > 2) {
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #fff8e1; border: 1px solid #ffeb3b; border-radius: 5px;",

        h5(
          icon("info-sign", lib = "glyphicon", style = "color: #f57c00;"),
          strong(" Group Assignment Required"),
          style = "color: #f57c00; margin-top: 0;"
        ),

        p(
          "The selected variable has more than 2 categories. Please assign categories to Group 1 and Group 2, then click 'Apply Roles' to create the keyness groups.",
          style = "color: #666; margin-bottom: 15px;"
        ),

        fluidRow(
          column(
            6,
            div(
              style = "padding: 10px; background-color: #e3f2fd; border-radius: 5px;",
              h5(strong("Group 1"), style = "color: #1976d2; margin-top: 0;"),
              pickerInput(
                inputId = "keynessGroup1Categories",
                label = NULL,
                choices = as.character(categories),
                selected = values$keynessGroup1,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `selected-text-format` = "count > 2",
                  `count-selected-text` = "{0} categories selected"
                )
              )
            )
          ),
          column(
            6,
            div(
              style = "padding: 10px; background-color: #ffebee; border-radius: 5px;",
              h5(strong("Group 2"), style = "color: #c62828; margin-top: 0;"),
              pickerInput(
                inputId = "keynessGroup2Categories",
                label = NULL,
                choices = as.character(categories),
                selected = values$keynessGroup2,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `selected-text-format` = "count > 2",
                  `count-selected-text` = "{0} categories selected"
                )
              )
            )
          )
        )
      )
    } else {
      NULL
    }
  })

  # Clear individual selections
  observeEvent(input$clearTimeVar, {
    values$timeVariable <- NULL
    updateSelectInput(session, "timeVarInput", selected = "")
  })

  observeEvent(input$clearLabelVar, {
    values$labelVariable <- NULL
    updateSelectInput(session, "labelVarInput", selected = "")
  })

  observeEvent(input$clearKeynessVar, {
    values$keynessVariable <- NULL
    values$keynessGroup1 <- NULL
    values$keynessGroup2 <- NULL

    # Remove keyness_group variable if it exists
    if ("keyness_group" %in% names(values$dfTag)) {
      values$dfTag$keyness_group <- NULL
    }

    updateSelectInput(session, "keynessVarInput", selected = "")
  })

  # Apply feature roles - UNIFIED WORKFLOW
  observeEvent(input$applyVarRoles, {
    success_message <- ""

    # Apply Time Variable
    if (!is.null(input$timeVarInput) && input$timeVarInput != "") {
      values$timeVariable <- input$timeVarInput
      success_message <- paste0(success_message, "Time variable assigned.<br>")
    }

    # Apply Label Variable
    if (!is.null(input$labelVarInput) && input$labelVarInput != "") {
      values$labelVariable <- input$labelVarInput
      success_message <- paste0(success_message, "Label variable assigned.<br>")
    }

    # Apply Keyness Variable and create groups
    if (!is.null(input$keynessVarInput) && input$keynessVarInput != "") {
      values$keynessVariable <- input$keynessVarInput

      var_data <- values$dfTag[[input$keynessVarInput]]
      categories <- unique(var_data[!is.na(var_data)])

      if (length(categories) == 2) {
        # Automatically create keyness_group for binary variables
        keyness_group <- as.numeric(factor(var_data, levels = sort(categories)))
        values$dfTag$keyness_group <- keyness_group

        # Count documents per group
        doc_groups <- values$dfTag %>%
          group_by(doc_id, keyness_group) %>%
          summarise(.groups = "drop") %>%
          count(keyness_group)

        success_message <- paste0(
          success_message,
          "Keyness variable assigned and groups created:<br>",
          "<small>Group 1: ",
          sort(categories)[1],
          " (",
          doc_groups$n[doc_groups$keyness_group == 1],
          " documents)<br>",
          "Group 2: ",
          sort(categories)[2],
          " (",
          doc_groups$n[doc_groups$keyness_group == 2],
          " documents)</small><br>"
        )
      } else if (length(categories) > 2) {
        # Check if groups have been assigned
        if (
          is.null(input$keynessGroup1Categories) ||
            length(input$keynessGroup1Categories) == 0 ||
            is.null(input$keynessGroup2Categories) ||
            length(input$keynessGroup2Categories) == 0
        ) {
          sendSweetAlert(
            session = session,
            title = "Group Assignment Required",
            text = "Please assign categories to both Group 1 and Group 2 before applying roles.",
            type = "warning"
          )
          return()
        }

        # Check for overlap
        overlap <- intersect(
          input$keynessGroup1Categories,
          input$keynessGroup2Categories
        )

        if (length(overlap) > 0) {
          sendSweetAlert(
            session = session,
            title = "Error",
            text = paste(
              "Categories cannot belong to both groups. Overlapping categories:",
              paste(overlap, collapse = ", ")
            ),
            type = "error"
          )
          return()
        }

        # Check if all categories are assigned
        assigned_categories <- c(
          input$keynessGroup1Categories,
          input$keynessGroup2Categories
        )
        unassigned <- setdiff(as.character(categories), assigned_categories)

        # Create keyness_group variable
        keyness_group <- rep(NA, nrow(values$dfTag))
        keyness_group[
          values$dfTag[[input$keynessVarInput]] %in%
            input$keynessGroup1Categories
        ] <- 1
        keyness_group[
          values$dfTag[[input$keynessVarInput]] %in%
            input$keynessGroup2Categories
        ] <- 2

        # Add to dfTag
        values$dfTag$keyness_group <- keyness_group

        # Store group assignments in values
        values$keynessGroup1 <- input$keynessGroup1Categories
        values$keynessGroup2 <- input$keynessGroup2Categories

        # Count documents per group
        doc_groups <- values$dfTag %>%
          filter(!is.na(keyness_group)) %>%
          group_by(doc_id, keyness_group) %>%
          summarise(.groups = "drop") %>%
          count(keyness_group)

        n_group1 <- ifelse(
          1 %in% doc_groups$keyness_group,
          doc_groups$n[doc_groups$keyness_group == 1],
          0
        )
        n_group2 <- ifelse(
          2 %in% doc_groups$keyness_group,
          doc_groups$n[doc_groups$keyness_group == 2],
          0
        )

        success_message <- paste0(
          success_message,
          "Keyness variable assigned and groups created:<br>",
          "<small>Group 1: ",
          n_group1,
          " documents<br>",
          "Group 2: ",
          n_group2,
          " documents"
        )

        if (length(unassigned) > 0) {
          success_message <- paste0(
            success_message,
            "<br>Unassigned categories (excluded): ",
            paste(unassigned, collapse = ", ")
          )
        }

        success_message <- paste0(success_message, "</small><br>")
      }
    }
    # Show confirmation message
    if (success_message == "") {
      sendSweetAlert(
        session = session,
        title = "No Changes",
        text = "Please select at least one variable role to apply.",
        type = "info"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "Success!",
        text = HTML(success_message),
        html = TRUE,
        type = "success"
      )
    }
  })

  # Reset all feature roles
  observeEvent(input$resetVarRoles, {
    ask_confirmation(
      inputId = "confirmResetRoles",
      title = "Reset Feature Roles",
      text = "Are you sure you want to reset all feature role assignments? This will also remove the keyness_group variable if it exists.",
      type = "warning",
      btn_labels = c("Cancel", "Reset")
    )
  })

  observeEvent(input$confirmResetRoles, {
    if (isTRUE(input$confirmResetRoles)) {
      values$timeVariable <- NULL
      values$labelVariable <- NULL
      values$keynessVariable <- NULL
      values$keynessGroup1 <- NULL
      values$keynessGroup2 <- NULL

      # Remove keyness_group variable if it exists
      if ("keyness_group" %in% names(values$dfTag)) {
        values$dfTag$keyness_group <- NULL
      }

      # Update UI
      updateSelectInput(session, "timeVarInput", selected = "")
      updateSelectInput(session, "labelVarInput", selected = "")
      updateSelectInput(session, "keynessVarInput", selected = "")

      sendSweetAlert(
        session = session,
        title = "Reset Complete",
        text = "All feature roles have been cleared.",
        type = "info"
      )
    }
  })
}

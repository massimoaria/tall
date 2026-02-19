filters_groupsUI <- function() {
  ### FILTER ----
  filters <- tabItem(
    tabName = "filter_text",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(
            strong("Filter docs by available external information"),
            align = "center"
          )
        )
      ),
      fluidRow(
        column(
          9,
          shinycssloaders::withSpinner(
            DT::DTOutput("filterData"),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        column(
          3,
          fluidRow(
            box(
              width = 12,
              div(h3(strong(em("Filter by"))), style = "margin-top:-57px"),
              hr(),
              helpText(h5("Select an external information to filter docs:")),
              uiOutput("filterListUI"),
              uiOutput("filterValue"),
              hr(),
              div(
                fluidRow(
                  column(
                    6,
                    div(
                      align = "center",
                      title = t_run,
                      do.call(
                        "actionButton",
                        c(
                          run_bttn,
                          list(
                            inputId = "filterRun"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    6,
                    div(
                      align = "center",
                      title = t_back,
                      do.call(
                        "actionButton",
                        c(
                          back_bttn,
                          list(
                            inputId = "filterBack"
                          )
                        )
                      )
                    )
                  )
                ),
                style = "margin-top:-15px"
              ),
              br(),
              htmlOutput("filterSummary")
            )
          )
        )
      )
    )
  )

  ### GROUPS ----
  groups <- tabItem(
    tabName = "defineGroups",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(
            strong("Define groups by available external information"),
            align = "center"
          )
        )
      ),
      fluidRow(
        column(
          9,
          shinycssloaders::withSpinner(
            DT::DTOutput("defineGroupsData"),
            color = getOption("spinner.color", default = "#4F7942")
          )
        ),
        column(
          3,
          fluidRow(
            box(
              width = 12,
              div(
                h3(strong(em("Select external information"))),
                style = "margin-top:-57px"
              ),
              hr(),
              helpText(h5(
                "Select an external information to define new document groups:"
              )),
              uiOutput("defineGroupsListUI"),
              uiOutput(outputId = "infoGroups"),
              hr(),
              div(
                fluidRow(
                  div(
                    align = "center",
                    title = t_run,
                    do.call(
                      "actionButton",
                      c(
                        run_bttn,
                        list(
                          inputId = "defineGroupsRun"
                        )
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
  )

  return(list(filters = filters, groups = groups))
}

filters_groupsServer <- function(input, output, session, values, statsValues) {
  ## FILTER ----
  output$filterListUI <- renderUI({
    label <- c("", sort(noGroupLabels(names(values$dfTag))))
    selectInput(
      inputId = "filterList",
      label = NULL,
      choices = label,
      selected = values$selectedFilter,
      multiple = TRUE,
      width = "100%"
    )
  })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$filterList
    },
    handlerExpr = {
      if (length(input$filterList) > 0) {
        filtervalues <- LemmaSelection(values$dfTag) %>%
          select(all_of(input$filterList)) %>%
          distinct()
        values$filtervalues <- sort(filtervalues[[1]])
        values$selectedFilter <- input$filterList
      }
    }
  )

  output$filterValue <- renderUI({
    req(input$filterList)

    lapply(input$filterList, function(var) {
      if (!is.null(values[[paste0("filter_", var)]])) {
        sel_value <- values[[paste0("filter_", var)]]
      } else {
        sel_value <- ""
      }
      if (is.factor(values$dfTag[[var]])) {
        selectInput(
          inputId = paste0("filter_", var),
          label = paste("Filter", var),
          choices = levels(values$dfTag[[var]]),
          selected = sel_value,
          multiple = TRUE
        )
      } else if (is.numeric(values$dfTag[[var]])) {
        if (sel_value[1] == "") {
          sel_value <- range(values$dfTag[[var]], na.rm = TRUE)
        }
        sliderInput(
          inputId = paste0("filter_", var),
          label = paste("Filter", var),
          min = min(values$dfTag[[var]], na.rm = TRUE),
          max = max(values$dfTag[[var]], na.rm = TRUE),
          value = sel_value
        )
      } else if (is.character(values$dfTag[[var]])) {
        selectInput(
          inputId = paste0("filter_", var),
          label = paste("Filter", var),
          choices = sort(unique(values$dfTag[[var]])),
          selected = sel_value,
          multiple = TRUE
        )
      }
    })
  })

  filterDATA <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$filterRun
    },
    valueExpr = {
      if (!is.null(input$filterList)) {
        docSelected <- data.frame(matrix(ncol = 0, nrow = nrow(values$dfTag)))
        for (var in input$filterList) {
          input_id <- paste0("filter_", var)
          filter_value <- input[[input_id]]
          values[[input_id]] <- filter_value

          if (is.factor(values$dfTag[[var]])) {
            docSelected[[var]] <- ifelse(
              values$dfTag[[var]] %in% filter_value,
              TRUE,
              FALSE
            )
          } else if (is.character(values$dfTag[[var]])) {
            docSelected[[var]] <- ifelse(
              values$dfTag[[var]] %in% filter_value,
              TRUE,
              FALSE
            )
          } else if (is.numeric(values$dfTag[[var]])) {
            docSelected[[var]] <- ifelse(
              values$dfTag[[var]] >= filter_value[1] &
                values$dfTag[[var]] <= filter_value[2],
              TRUE,
              FALSE
            )
          } else {
            docSelected[[var]] <- TRUE
          }
        }
        values$dfTag$docSelected <- ifelse(
          rowSums(docSelected) >= ncol(docSelected),
          TRUE,
          FALSE
        )
      } else {
        values$dfTag$docSelected <- TRUE
      }
      values$dfTag
    }
  )

  observeEvent(
    eventExpr = input$filterRun,
    handlerExpr = {
      output$filterSummary <- renderUI({
        filtered <- LemmaSelection(values$dfTag) %>% filter(docSelected)
        num_docs <- n_distinct(filtered$doc_id)
        num_paragraphs <- sum(
          filtered %>%
            group_by(doc_id) %>%
            summarize(sent = max(paragraph_id)) %>%
            pull()
        )
        num_sentences <- sum(
          filtered %>%
            group_by(doc_id) %>%
            summarize(sent = max(sentence_id)) %>%
            pull()
        )
        num_tokens <- nrow(filtered)

        HTML(paste(
          "<div style='border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9;'>",
          "<h4><strong>Filter Summary</strong></h4>",
          "<br>",
          "<p>Number of Documents: <strong>",
          num_docs,
          "</strong></p>",
          "<p>Number of Paragraphs: <strong>",
          num_paragraphs,
          "</strong></p>",
          "<p>Number of Sentences: <strong>",
          num_sentences,
          "</strong></p>",
          "<p>Number of Tokens: <strong>",
          num_tokens,
          "</strong></p>",
          "</div>"
        ))
      })
    }
  )

  output$filterData <- renderDT({
    filterDATA()
    DTformat(
      LemmaSelection(values$dfTag) %>%
        dplyr::filter(docSelected) %>%
        select("doc_id", "sentence", "token", "lemma", "upos"),
      nrow = 3,
      size = "100%",
      title = paste0(
        "Filtered Data by ",
        paste0(input$filterList, collapse = ", ")
      ),
      col_to_remove = values$generalTerm
    )
  })

  ## Data filtered by dynamic text on dashboardHeader

  output$dataFilteredBy <- renderText({
    if (!is.null(input$filterList)) {
      req(input$filterRun)
      HTML(paste(
        "Documents filtered by: <b>",
        paste0(input$filterList, collapse = ", "),
        "</b>"
      ))
    } else {
      HTML("")
    }
  })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$filterBack
    },
    handlerExpr = {
      values$dfTag$docSelected <- TRUE
      lapply(input$filterList, function(var) {
        removeUI(paste0("filter_", var))
        if (is.factor(values$dfTag[[var]])) {
          values[[paste0("filter_", var)]] <- NULL
        } else if (is.numeric(values$dfTag[[var]])) {
          values[[paste0("filter_", var)]] <- range(
            values$dfTag[[var]],
            na.rm = TRUE
          )
        } else if (is.character(values$dfTag[[var]])) {
          values[[paste0("filter_", var)]] <- NULL
        }
      })
      updateSelectInput(
        inputId = "filterList",
        session = session,
        selected = ""
      )
    }
  )

  ## GROUPS ----

  ### Define groups ----

  output$defineGroupsListUI <- renderUI({
    label <- noGroupLabels(names(values$dfTag))
    multiInput(
      inputId = "defineGroupsList",
      label = NULL,
      choices = label,
      selected = values$selectedGroups,
      width = "100%"
    )
  })

  output$infoGroups <- renderUI({
    if (length(input$defineGroupsList) > 1) {
      shinyWidgets::alert(
        icon("info"),
        " You need to select only one field",
        status = "danger"
      )
    }
  })

  groupMetadata <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$defineGroupsRun
    },
    valueExpr = {
      values$selectedGroups <- input$defineGroupsList
      values$dfTag <- groupByMetadata(
        values$dfTag,
        metadata = input$defineGroupsList
      )
      if (length(input$defineGroupsList) == 1) {
        showModal(groupModal(session))
      } else {
        showModal(ungroupModal(session))
      }
    }
  )

  ## Data grouped by dynamic text on dashboardHeader
  output$dataGroupedBy <- renderText({
    if (length(input$defineGroupsList) == 1) {
      req(input$defineGroupsRun)
      HTML(paste("Documents grouped by: <b>", input$defineGroupsList, "</b>"))
    } else {
      HTML("")
    }
  })

  output$defineGroupsData <- renderDT({
    groupMetadata()
    DTformat(
      values$dfTag %>% filter(docSelected),
      nrow = 3,
      size = "100%",
      title = "Data Grouped By External Information",
      col_to_remove = values$generalTerm
    )
  })

  groupModal <- function(session) {
    ns <- session$ns
    values$newGr <- values$dfTag %>%
      filter(docSelected) %>%
      count(doc_id, ungroupDoc_id) %>%
      group_by(doc_id) %>%
      count()
    names(values$newGr) <- c(input$defineGroupsList, "N. of Docs")
    txt <- paste0(
      "<hr><br><br>The original <b>",
      sum(values$newGr[, 2]),
      "</b> documents have been partitioned into <b>",
      nrow(values$newGr),
      "</b> groups <br><br>"
    )
    modalDialog(
      h3(strong(paste0("Documents grouped by ", input$defineGroupsList))),
      h4(HTML(txt)),
      br(),
      DTOutput(ns("groupData")),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      ),
    )
  }

  ungroupModal <- function(session) {
    ns <- session$ns
    txt <- paste0(
      "<hr><br><br>The original partitioning<br>into documents has been correctly restored.<br><br><hr>"
    )
    modalDialog(
      h3(HTML(txt)),
      size = "m",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      ),
    )
  }

  output$groupData <- renderDT(server = FALSE, {
    DTformat(
      values$newGr,
      nrow = 10,
      size = "100%",
      title = "Groups By External Information",
      left = 1,
      col_to_remove = values$generalTerm
    )
  })
}

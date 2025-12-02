settingsUI <- function() {
  ### SETTINGS ----
  tabItem(
    tabName = "settings",
    fluidPage(
      # Header
      fluidRow(
        column(
          12,
          h3(icon("cog"), strong("Settings"), align = "center"),
          h5(
            "Configure global settings for plots, analysis reproducibility, and AI features.",
            align = "center",
            style = "color: #666;"
          ),
          br()
        )
      ),

      fluidRow(
        # LEFT COLUMN - Working Folder & Language Models
        column(
          6,
          wellPanel(
            style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
            h4(icon("folder"), strong("Working Folder")),
            h5("Select a folder where the analysis outputs will be saved"),
            br(),
            shinyDirButton(
              "workingfolder",
              "Select a Working Folder",
              "Select",
              style = "color:white;"
            ),
            br(),
            br(),
            textOutput("wdFolder"),
            hr(),
            h4(icon("database"), strong("Language Models")),
            actionButton(
              inputId = "cache",
              style = "color:white;",
              label = "Clean model folder"
            )
          )
        ),

        # RIGHT COLUMN - Reproducibility Settings
        column(
          6,
          wellPanel(
            style = "background-color: #e8f5e9; border: 1px solid #c8e6c9;",
            h4(icon("random"), strong("Reproducibility Settings")),
            br(),
            fluidRow(
              column(
                9,
                numericInput(
                  "random_seed",
                  "Random Seed",
                  value = 1234,
                  min = 1,
                  max = 99999,
                  step = 1
                )
              ),
              column(
                3,
                br(),
                actionButton(
                  "randomize_seed",
                  "Randomize",
                  icon = icon("random"),
                  style = "color:white; margin-top: 5px;"
                )
              )
            ),
            div(
              style = "background-color: #d1f2d5; padding: 10px; border-radius: 5px; border-left: 4px solid #4caf50;",
              icon("info-circle", style = "color: #2e7d32;"),
              span(
                " Using the same seed value ensures that analyses involving randomization will produce identical results when re-run.",
                style = "color: #2e7d32;"
              )
            )
          )
        )
      ),

      hr(),

      # TALL AI Section
      fluidRow(
        column(
          12,
          wellPanel(
            style = "background-color: #fff3e0; border: 1px solid #ffe0b2;",
            h3(icon("robot"), strong("Tall AI - Google Gemini Integration")),
            h5(
              "Enable advanced AI-powered features by providing your Google Gemini API Key. If you don't have one, you can generate it at ",
              tags$a(
                "AI Studio",
                href = "https://aistudio.google.com/app/apikey",
                target = "_blank"
              ),
              "."
            ),
            br(),

            fluidRow(
              column(
                4,
                h5(strong("API Key")),
                passwordInput(
                  "api_key",
                  "Enter your Gemini API Key:",
                  "",
                  width = "100%"
                ),
                uiOutput("apiStatus"),
                br(),
                fluidRow(
                  column(
                    6,
                    actionButton(
                      "set_key",
                      "Set API Key",
                      icon = icon("check"),
                      style = "color:white;",
                      width = "100%"
                    )
                  ),
                  column(
                    6,
                    actionButton(
                      "remove_key",
                      "Remove Key",
                      icon = icon("trash"),
                      style = "color:white;",
                      width = "100%"
                    )
                  )
                )
              ),

              column(
                4,
                h5(strong("Model Selection")),
                uiOutput("geminiModelChoice")
              ),

              column(
                4,
                h5(strong("Output Size")),
                uiOutput("geminiOutputSize")
              )
            )
          )
        )
      )
    )
  )
}


settingsServer <- function(input, output, session, values, statsValues) {
  ## UTILITY ----

  observeEvent(input$d_abstractiveView, {
    showModal(showDocumentModal(
      session,
      input$Abst_document_selection,
      input
    ))
  })

  observeEvent(input$d_summarizationView, {
    showModal(showDocumentModal(
      session,
      input$document_selection,
      input
    ))
  })

  showDocumentModal <- function(session, docID, input) {
    ns <- session$ns
    if (
      input$sidebarmenu %in%
        c("import_tx", "split_tx", "extInfo", "randomText")
    ) {
      text <- create_document_box(
        values$txt %>% filter(doc_id == docID) %>% pull(text),
        docID,
        summarization_type = "original_text"
      )
    } else {
      text <- create_document_box(
        values$dfTag %>%
          filter(doc_id == !!docID) %>%
          select(doc_id, paragraph_id, sentence_id, sentence) %>%
          distinct() %>%
          group_by(doc_id, paragraph_id) %>%
          summarise(Paragraph = paste(sentence, collapse = "\n ")) %>%
          ungroup(),
        docID,
        summarization_type = "abstractive"
      )
    }

    modalDialog(
      tags$style(HTML(
        "
      .modal-dialog {
        width: 70vw !important;
        max-width: 70vw !important;
      }
      .modal-content {
        height: 85vh !important;
      }
      .modal-body {
        overflow-y: auto !important;
        max-height: calc(85vh - 120px) !important;
      }
    "
      )),
      div(
        h3(strong("Document corpus")),
        br(),
        HTML(text)
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  }

  ## table click button ----
  observeEvent(input$button_id, {
    if (input$button_id != "null") {
      showModal(showDocumentModal(session, input$button_id, input))
    }
  })

  observeEvent(input$button_id_del, {
    if (
      input$sidebarmenu %in% c("import_tx", "split_tx", "extInfo", "randomText")
    ) {
      values$txt <- values$txt %>%
        mutate(
          doc_selected = ifelse(
            doc_id == input$button_id_del,
            FALSE,
            doc_selected
          )
        )
    }
    output$dataImported <- DT::renderDT({
      # DATAloading()
      if (values$menu == 0) {
        DTformat(
          values$txt %>%
            filter(doc_selected) %>%
            mutate(text = paste0(substr(text, 1, 500), "...")) %>%
            select(doc_id, text, everything()) %>%
            select(-doc_selected, -text_original),
          left = 2,
          nrow = 5,
          filter = "none",
          button = TRUE,
          delete = TRUE
        )
      }
    })
  })

  ### SETTINGS ----
  observeEvent(input$cache, {
    deleteCache()
  })

  ## Choose Working folder in Setting Menu
  roots <- c(home = homeFolder())

  observe({
    shinyDirChoose(input, "workingfolder", roots = roots, filetypes = c(""))
  })

  observeEvent(
    eventExpr = input$workingfolder,
    handlerExpr = {
      wdTall <- parseDirPath(roots = roots, input$workingfolder)

      if (length(wdTall) == 0 || is.null(wdTall)) {
        if (is.null(wdFolder())) {
          values$menu <- -2
        }
      } else {
        # setting up the main directory
        home <- homeFolder()
        path_tall <- file.path(home, "tall")
        # check if sub directory exists
        if (!file.exists(path_tall)) {
          dir.create(path_tall)
        }
        writeLines(wdTall, con = paste0(path_tall, "/tallWD.tall"))
        if (values$menu == -2) {
          values$menu <- -1
        }
        values$wdTall <- wdTall
      }
    },
    ignoreNULL = TRUE
  )

  output$wdFolder <- renderText({
    values$wdTall
  })

  # Random Seed Management
  observeEvent(input$random_seed, {
    if (!is.null(input$random_seed)) {
      set.seed(input$random_seed)
      values$random_seed <- input$random_seed
    }
  })

  observeEvent(input$randomize_seed, {
    new_seed <- sample(1:99999, 1)
    updateNumericInput(session, "random_seed", value = new_seed)
  })

  output$apiStatus <- renderUI({
    if (values$geminiAPI) {
      last <- showGeminiAPI()
      output$status <- renderText(paste0("✅ API key has been set: ", last))
    }
  })

  output$geminiOutputSize <- renderUI({
    list(
      selectInput(
        inputId = "gemini_output_size",
        label = "Max Output (in tokens)",
        selected = ifelse(
          is.null(values$gemini_output_size),
          "medium",
          values$gemini_output_size
        ),
        choices = c("Medium" = "medium", "Large" = "large")
      ),
      conditionalPanel(
        condition = "input.gemini_output_size == 'medium'",
        helpText(strong("Free Tier Output:")),
        helpText(em("Medium -> 16384 Tokens"))
      ),
      conditionalPanel(
        condition = "input.gemini_output_size == 'large'",
        helpText(strong("Free Tier Output:")),
        helpText(em("Large -> 32768 Tokens"))
      )
    )
  })

  output$geminiModelChoice <- renderUI({
    list(
      selectInput(
        inputId = "gemini_api_model",
        label = "Select the Gemini Model",
        choices = c(
          "Gemini 2.5 Flash" = "2.5-flash",
          "Gemini 2.5 Flash Lite" = "2.5-flash-lite",
          "Gemini 2.0 Flash" = "2.0-flash",
          "Gemini 2.0 Flash Lite" = "2.0-flash-lite",
          "Gemini 1.5 Flash" = "1.5-flash",
          "Gemini 1.5 Flash Lite" = "1.5-flash-8b"
        ),
        selected = ifelse(
          is.null(values$gemini_api_model),
          "2.0-flash",
          values$gemini_api_model
        )
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '2.5-flash'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 10",
          tags$br(),
          "Requests per Day: 500",
          tags$br(),
          "Latency time: High"
        ))
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '2.5-flash-lite'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 15",
          tags$br(),
          "Requests per Day: 500",
          tags$br(),
          "Latency time: Low"
        ))
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '2.0-flash-lite'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 30",
          tags$br(),
          "Requests per Day: 1500",
          tags$br(),
          "Latency time: Low"
        ))
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '2.0-flash'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 15",
          tags$br(),
          "Requests per Day: 1500",
          tags$br(),
          "Latency time: Medium"
        ))
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '1.5-flash'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 15",
          tags$br(),
          "Requests per Day: 1500",
          tags$br(),
          "Latency time: Medium"
        ))
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '1.5-flash-8b'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 15",
          tags$br(),
          "Requests per Day: 1500",
          tags$br(),
          "Latency time: Low"
        ))
      )
    )
  })

  observeEvent(input$gemini_api_model, {
    if (!is.null(input$gemini_api_model)) {
      saveGeminiModel(
        model = c(input$gemini_api_model, input$gemini_output_model),
        file = paste0(homeFolder(), "/.tall_gemini_model.txt", collapse = "")
      )
      values$gemini_api_model <- input$gemini_api_model
      values$gemini_output_size <- input$gemini_output_size
    }
  })

  observeEvent(input$gemini_output_size, {
    if (!is.null(input$gemini_output_size)) {
      saveGeminiModel(
        model = c(input$gemini_api_model, input$gemini_output_size),
        file = paste0(homeFolder(), "/.tall_gemini_model.txt", collapse = "")
      )
      values$gemini_api_model <- input$gemini_api_model
      values$gemini_output_size <- input$gemini_output_size
    }
  })

  observeEvent(input$set_key, {
    key <- input$api_key
    last <- setGeminiAPI(key)

    if (!last$valid) {
      output$apiStatus <- renderUI({
        output$status <- renderText(last$message)
      })
      values$geminiAPI <- FALSE
    } else {
      output$apiStatus <- renderUI({
        output$status <- renderText(paste0(
          "✅ API key has been set: ",
          last$message
        ))
      })
      values$geminiAPI <- TRUE
      home <- homeFolder()
      path_gemini_key <- paste0(
        home,
        "/tall/.tall_gemini_key.txt",
        collapse = ""
      )
      writeLines(Sys.getenv("GEMINI_API_KEY"), path_gemini_key)
    }
  })

  observeEvent(input$remove_key, {
    if (values$geminiAPI) {
      home <- homeFolder()
      path_gemini_key <- paste0(
        home,
        "/tall/.tall_gemini_key.txt",
        collapse = ""
      )
      file.remove(path_gemini_key)
      values$geminiAPI <- FALSE
      output$apiStatus <- renderUI({
        output$status <- renderText(paste0("❌ API key has been removed"))
      })
    }
  })

  ## SOLUTION FOR DT BUG ----
  # observeEvent(
  #   input$sidebarmenu,
  #   {
  #     # Lista completa di tutti gli output DT per ogni menu
  #     dt_outputs <- list(
  #       # ===== DATA MANAGEMENT =====
  #       import_tx = c("dataImported"),
  #       split_tx = c("splitTextData"),
  #       randomText = c("randomTextData"),
  #       extInfo = c("extInfoData"),
  #
  #       # ===== PRE-PROCESSING =====
  #       tokPos = c("tokPosTagData"),
  #       posSpecial = c("posSpecialTags", "posSpecialData", "specialEntityFreq"),
  #       custTermList = c("customPosTagData", "customListData"),
  #       multiwordCreat = c("multiwordList", "multiwordData"),
  #       multiwordByList = c("multiwordList2", "multiwordData2"),
  #       posTagSelect = c("posTagSelectData"),
  #
  #       # ===== DATA FILTERING & GROUPING =====
  #       filter_text = c("filterData"),
  #       defineGroups = c("defineGroupsData", "groupData"),
  #
  #       # ===== OVERVIEW & VOCABULARY =====
  #       overview = c("overviewData", "dictionaryData", "tfidfData"),
  #
  #       # ===== WORDS ANALYSIS =====
  #       wordCont = c(
  #         "wFreqTable",
  #         "wordInContext",
  #         "docInContext",
  #         "docInContextHigh"
  #       ),
  #       keyness = c("keyness_table"),
  #       w_pos = c("posTable"),
  #       w_clustering = c("w_clusteringTable", "wordInContextDend"),
  #       w_reinclustering = c(
  #         "w_ReinSummaryTable",
  #         "w_ReinClusteringTableTerms",
  #         "w_ReinClusteringTableSegments",
  #         "wordInContextRein"
  #       ),
  #       ca = c(
  #         "caSingularValueTable",
  #         "caCoordTable",
  #         "caContribTable",
  #         "caCosineTable"
  #       ),
  #
  #       # ===== NETWORKS =====
  #       w_networkCooc = c(
  #         "w_networkCoocNodesTable",
  #         "w_networkCoocEdgesTable",
  #         "wordInContextNet"
  #       ),
  #       w_networkTM = c("w_networkTMClusterTable", "w_networkTMWordTable"),
  #       w_word2vec = c("w_word2vecTable"),
  #       w_networkGrako = c(
  #         "w_networkGrakoNodesTable",
  #         "w_networkGrakoEdgesTable"
  #       ),
  #
  #       # ===== DOCUMENTS ANALYSIS =====
  #       d_tm_select = c("d_tm_selectTable"),
  #       d_tm_estim = c("d_tm_estimBpTable", "d_tm_estimTpTable"),
  #       d_polDet = c("d_polDetTable"),
  #       d_summarization = c("RelSentData")
  #     )
  #
  #     # Ottieni il menu corrente
  #     current_menu <- input$sidebarmenu
  #
  #     # Ottieni gli output DT del menu corrente
  #     current_outputs <- dt_outputs[[current_menu]]
  #     if (is.null(current_outputs)) {
  #       current_outputs <- character(0)
  #     }
  #
  #     # Ottieni tutti gli output DT
  #     all_outputs <- unique(unlist(dt_outputs, use.names = FALSE))
  #
  #     # Identifica gli output da sospendere (tutti tranne quelli del menu corrente)
  #     outputs_to_suspend <- setdiff(all_outputs, current_outputs)
  #
  #     # Sospendi il rendering degli output che non appartengono al menu corrente
  #     # Questo libera risorse senza distruggere gli output
  #     lapply(outputs_to_suspend, function(x) {
  #       tryCatch(
  #         {
  #           outputOptions(output, x, suspendWhenHidden = TRUE)
  #         },
  #         error = function(e) {
  #           # Ignora errori per output che potrebbero non essere ancora stati creati
  #         }
  #       )
  #     })
  #
  #     # Riattiva gli output del menu corrente
  #     lapply(current_outputs, function(x) {
  #       tryCatch(
  #         {
  #           outputOptions(output, x, suspendWhenHidden = FALSE)
  #         },
  #         error = function(e) {
  #           # Ignora errori per output che potrebbero non essere ancora stati creati
  #         }
  #       )
  #     })
  #   },
  #   priority = 100
  # )
}

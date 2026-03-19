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

      # ROW 1: Working Folder + Reproducibility + Language Models
      fluidRow(
        column(
          6,
          wellPanel(
            style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
            h4(icon("folder-open"), strong("Working Folder")),
            p(
              "All outputs (plots, reports, exported files) will be saved here.",
              style = "color: #666; font-size: 13px; margin-bottom: 15px;"
            ),
            uiOutput("wdFolderDisplay"),
            br(),
            div(
              style = "display: flex; gap: 10px; align-items: center;",
              shinyDirButton(
                "workingfolder",
                label = "Change Folder",
                title = "Select a Working Folder",
                icon = icon("folder-open"),
                style = "color: white; border-radius: 20px; padding: 8px 20px;"
              ),
              actionButton(
                inputId = "cache",
                style = "color:white; border-radius: 20px; padding: 8px 20px;",
                label = "Clean Model Cache",
                icon = icon("database")
              )
            )
          )
        ),
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
                " The seed ensures reproducible results for topic models, clustering, and all randomized analyses.",
                style = "color: #2e7d32;"
              )
            )
          )
        )
      ),

      # ROW 2: Graph Export Settings + TALL AI
      fluidRow(
        column(
          6,
          wellPanel(
            style = "background-color: #e3f2fd; border: 1px solid #bbdefb;",
            h4(icon("image"), strong("Graph Export Settings")),
            br(),
            div(
              style = "margin-bottom: 25px;",
              tags$label(
                "Export Resolution (DPI)",
                style = "font-weight: 600; color: #2E86AB; margin-bottom: 8px; display: block;"
              ),
              uiOutput("dpi_slider"),
              helpText(
                "Resolution for exported images (download button). Higher DPI = better quality but larger files.",
                style = "margin-top: 5px; color: #666; font-size: 12px;"
              )
            ),
            div(
              style = "margin-bottom: 25px;",
              tags$label(
                "Report Resolution (DPI)",
                style = "font-weight: 600; color: #2E86AB; margin-bottom: 8px; display: block;"
              ),
              uiOutput("report_dpi_slider"),
              helpText(
                "Resolution for images added to the Excel report. Lower DPI keeps the report file size small.",
                style = "margin-top: 5px; color: #666; font-size: 12px;"
              )
            ),
            div(
              style = "margin-bottom: 25px;",
              tags$label(
                "Plot Height (inches)",
                style = "font-weight: 600; color: #2E86AB; margin-bottom: 8px; display: block;"
              ),
              uiOutput("h_slider"),
              helpText(
                "Adjust the height of exported plots. Width is automatically calculated to maintain aspect ratio.",
                style = "margin-top: 5px; color: #666; font-size: 12px;"
              )
            ),
            div(
              style = "margin-bottom: 10px;",
              tags$label(
                "Plot Aspect Ratio",
                style = "font-weight: 600; color: #2E86AB; margin-bottom: 8px; display: block;"
              ),
              uiOutput("aspect_radio"),
              helpText(
                "Publication (3:2) is ideal for papers and books. Wide (2:1) is better for presentations and dashboards.",
                style = "margin-top: 5px; color: #666; font-size: 12px;"
              )
            )
          )
        ),
        column(
          6,
          wellPanel(
            style = "background-color: #fff3e0; border: 1px solid #ffe0b2;",
            h4(icon("robot"), strong("Tall AI - Google Gemini Integration")),
            p(
              "Enable AI-powered features by providing your Google Gemini API Key. Generate one at ",
              tags$a(
                "AI Studio",
                href = "https://aistudio.google.com/app/apikey",
                target = "_blank"
              ),
              ".",
              style = "color: #666; font-size: 13px; margin-bottom: 15px;"
            ),

            h5(strong("API Key")),
            passwordInput(
              "api_key",
              label = NULL,
              value = "",
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
            ),
            hr(),
            fluidRow(
              column(
                6,
                h5(strong("Model")),
                uiOutput("geminiModelChoice")
              ),
              column(
                6,
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
    if (!is.null(values$dfTag)) {
      values$dfTag <- values$dfTag %>%
        mutate(docSelected = ifelse(doc_id == input$button_id_del, FALSE, docSelected))
    }
  })

  ### SETTINGS ----

  ## Graph Settings ----
  saveGraphSettings <- function() {
    home <- homeFolder()
    path_tall <- file.path(home, "tall")
    if (!file.exists(path_tall)) {
      dir.create(path_tall)
    }
    writeLines(
      c(
        as.character(values$dpi),
        as.character(values$report_dpi),
        as.character(values$h),
        as.character(values$aspect)
      ),
      file.path(path_tall, ".tall_graph_settings.txt")
    )
  }

  observeEvent(input$dpi, {
    values$dpi <- as.numeric(input$dpi)
    saveGraphSettings()
  }, ignoreInit = TRUE)

  observeEvent(input$report_dpi, {
    values$report_dpi <- as.numeric(input$report_dpi)
    saveGraphSettings()
  }, ignoreInit = TRUE)

  observeEvent(input$h, {
    values$h <- as.numeric(input$h)
    saveGraphSettings()
  }, ignoreInit = TRUE)

  observeEvent(input$aspect, {
    values$aspect <- as.numeric(input$aspect)
    saveGraphSettings()
  }, ignoreInit = TRUE)

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

  output$wdFolderDisplay <- renderUI({
    folder <- values$wdTall
    if (is.null(folder) || length(folder) == 0 || folder == "") {
      div(
        style = "padding: 12px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
        icon("exclamation-triangle", style = "color: #856404;"),
        span(
          " No working folder selected. Please select a folder to save your outputs.",
          style = "color: #856404; font-size: 13px;"
        )
      )
    } else {
      div(
        style = "padding: 12px; background-color: #e8f5e9; border-left: 4px solid #4caf50; border-radius: 4px;",
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          icon("check-circle", style = "color: #4caf50; font-size: 18px;"),
          div(
            span("Current folder:", style = "color: #666; font-size: 12px;"),
            br(),
            strong(folder, style = "font-size: 14px; color: #2e7d32; word-break: break-all;")
          )
        )
      )
    }
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
          "Gemini 3.0 Flash" = "3-flash-preview",
          "Gemini 3.1 Pro" = "3.1-pro-preview",
          "Gemini Pro Latest" = "pro-latest"
        ),
        selected = ifelse(
          is.null(values$gemini_api_model),
          "2.5-flash-lite",
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
        condition = "input.gemini_api_model == '3-flash-preview'",
        helpText(strong("Free Tier Rate Limits:")),
        helpText(em(
          "Request per Minutes: 10",
          tags$br(),
          "Requests per Day: 500",
          tags$br(),
          "Latency time: Medium"
        ))
      ),
      conditionalPanel(
        condition = "input.gemini_api_model == '3.1-pro-preview' || input.gemini_api_model == 'pro-latest'",
        helpText(strong(style = "color: #d9534f;", "Paid API Key Required")),
        helpText(em(
          "Pro models require a paid (non-free tier) API key.",
          tags$br(),
          "Free tier API keys will not work with these models."
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

      # Alert for Pro models requiring paid API key
      if (input$gemini_api_model %in% c("3.1-pro-preview", "pro-latest")) {
        showModal(modalDialog(
          title = "Paid API Key Required",
          tags$p("The selected Pro model requires a ", tags$strong("paid (non-free tier)"), " Google AI API key."),
          tags$p("Free tier API keys do not support Pro models. If you are using a free tier key, the API calls will fail."),
          tags$p("You can upgrade your API key at: ", tags$a(href = "https://aistudio.google.com/apikey", target = "_blank", "Google AI Studio")),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
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

    # Quick sync validations
    if (is.null(key) || !is.character(key) || nchar(key) == 0) {
      output$apiStatus <- renderUI({
        output$status <- renderText("\u274c API key must be a non-empty string.")
      })
      values$geminiAPI <- FALSE
      return()
    }
    if (nchar(key) < 10) {
      output$apiStatus <- renderUI({
        output$status <- renderText("\u274c API key seems too short. Please verify your key.")
      })
      values$geminiAPI <- FALSE
      return()
    }

    output$apiStatus <- renderUI({
      output$status <- renderText("\u231b Validating API key...")
    })

    # Async: validate key via API call in background
    promises::future_promise({
      apiCheck <- gemini_ai(
        image = NULL, prompt = "Hello", model = "2.5-flash",
        type = "png", retry_503 = 5, api_key = key
      )
      contains_error <- grepl("HTTP\\s*[1-5][0-9]{2}", apiCheck)
      list(valid = !contains_error, key = key)
    }, seed = TRUE) %...>%
      (function(result) {
        if (!result$valid) {
          output$apiStatus <- renderUI({
            output$status <- renderText(
              "\u274c API key seems be not valid! Please, check it or your connection."
            )
          })
          values$geminiAPI <- FALSE
        } else {
          Sys.setenv(GEMINI_API_KEY = result$key)
          last4 <- substr(
            result$key,
            max(1, nchar(result$key) - 3),
            nchar(result$key)
          )
          masked <- paste0(
            paste0(rep("*", nchar(result$key) - 4), collapse = ""),
            last4
          )
          output$apiStatus <- renderUI({
            output$status <- renderText(
              paste0("\u2705 API key has been set: ", masked)
            )
          })
          values$geminiAPI <- TRUE
          home <- homeFolder()
          path_gemini_key <- paste0(
            home, "/tall/.tall_gemini_key.txt", collapse = ""
          )
          writeLines(result$key, path_gemini_key)
        }
      }) %...!%
      (function(err) {
        output$apiStatus <- renderUI({
          output$status <- renderText(
            paste("Error validating key:", conditionMessage(err))
          )
        })
        values$geminiAPI <- FALSE
      })

    NULL
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
  observeEvent(
    input$sidebarmenu,
    {
      # Lista completa di tutti gli output DT per ogni menu
      dt_outputs <- list(
        # ===== DATA MANAGEMENT =====
        import_tx = c("dataImported"),
        split_tx = c("splitTextData"),
        randomText = c("randomTextData"),
        extInfo = c("extInfoData"),

        # ===== PRE-PROCESSING =====
        tokPos = c("tokPosTagData"),
        posSpecial = c("posSpecialTags", "posSpecialData", "specialEntityFreq"),
        custTermList = c("customPosTagData", "customListData"),
        multiwordCreat = c("multiwordList", "multiwordData"),
        multiwordByList = c("multiwordList2", "multiwordData2"),
        posTagSelect = c("posTagSelectData"),

        # ===== DATA FILTERING & GROUPING =====
        filter_text = c("filterData"),
        defineGroups = c("defineGroupsData", "groupData"),

        # ===== OVERVIEW & VOCABULARY =====
        overview = c("overviewData", "dictionaryData", "tfidfData"),

        # ===== WORDS ANALYSIS =====
        wordCont = c(
          "wFreqTable",
          "wordInContext",
          "docInContext",
          "docInContextHigh"
        ),
        keyness = c("keyness_table"),
        w_pos = c("posTable"),
        w_clustering = c("w_clusteringTable", "wordInContextDend"),
        w_reinclustering = c(
          "w_ReinSummaryTable",
          "w_ReinClusteringTableTerms",
          "w_ReinClusteringTableSegments",
          "wordInContextRein"
        ),
        ca = c(
          "caSingularValueTable",
          "caCoordTable",
          "caContribTable",
          "caCosineTable"
        ),

        # ===== NETWORKS =====
        w_networkCooc = c(
          "w_networkCoocNodesTable",
          "w_networkCoocEdgesTable",
          "wordInContextNet"
        ),
        w_networkTM = c("w_networkTMClusterTable", "w_networkTMWordTable"),
        w_word2vec = c("w_word2vecTable"),
        # ===== DOCUMENTS ANALYSIS =====
        d_tm_select = c("d_tm_selectTable"),
        d_tm_estim = c("d_tm_estimBpTable", "d_tm_estimTpTable"),
        d_syntactic = c("d_syntacticTable"),
        d_svo = c("d_svoTable"),
        d_polDet = c("d_polDetTable"),
        d_summarization = c("RelSentData")
      )

      # Ottieni il menu corrente
      current_menu <- input$sidebarmenu

      # Ottieni gli output DT del menu corrente
      current_outputs <- dt_outputs[[current_menu]]
      if (is.null(current_outputs)) {
        current_outputs <- character(0)
      }

      # Ottieni tutti gli output DT
      all_outputs <- unique(unlist(dt_outputs, use.names = FALSE))

      # Identifica gli output da sospendere (tutti tranne quelli del menu corrente)
      outputs_to_suspend <- setdiff(all_outputs, current_outputs)

      # Sospendi il rendering degli output che non appartengono al menu corrente
      # Questo libera risorse senza distruggere gli output
      lapply(outputs_to_suspend, function(x) {
        tryCatch(
          {
            outputOptions(output, x, suspendWhenHidden = TRUE)
          },
          error = function(e) {
            # Ignora errori per output che potrebbero non essere ancora stati creati
          }
        )
      })

      # Riattiva gli output del menu corrente
      lapply(current_outputs, function(x) {
        tryCatch(
          {
            outputOptions(output, x, suspendWhenHidden = FALSE)
          },
          error = function(e) {
            # Ignora errori per output che potrebbero non essere ancora stati creati
          }
        )
      })
    },
    priority = 100
  )
}

importUI <- function() {
  tabItem(
    tabName = "import_tx",
    fluidPage(
      # Page Header
      fluidRow(
        column(
          12,
          div(
            h2(
              icon("file-text", lib = "glyphicon"),
              strong("Import Texts"),
              style = "color: #4F7942; text-align: center; margin-bottom: 30px;"
            )
          )
        )
      ),

      fluidRow(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Corpus",
            fluidRow(
              # Main Content Area - Data Table
              column(
                9,
                div(
                  style = "margin-top: 20px;",
                  shinycssloaders::withSpinner(
                    DT::DTOutput("dataImported"),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                )
              ),

              # Sidebar - Control Panel
              column(
                3,
                div(
                  style = "margin-top: 20px;",
                  box(
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",

                    # Box Header
                    div(
                      class = "box-header with-border",
                      h4(
                        icon("upload", lib = "glyphicon"),
                        strong("Import Controls"),
                        style = "margin: 0; color: #4F7942;"
                      )
                    ),

                    # Box Body
                    div(
                      class = "box-body",

                      # Import Button Area
                      div(
                        style = "margin-bottom: 20px;",
                        uiOutput("runButton")
                      ),

                      # Action Buttons (shown after import)
                      conditionalPanel(
                        condition = "input.runImport > 0",
                        tags$hr(
                          style = "border-color: #e0e0e0; margin: 20px 0;"
                        ),

                        div(
                          h5(
                            strong("Actions"),
                            style = "color: #666; margin-bottom: 15px;"
                          ),

                          fluidRow(
                            column(
                              6,
                              div(
                                align = "center",
                                div(
                                  title = "Export raw text(s) in Excel",
                                  actionButton(
                                    inputId = "collection.save",
                                    label = NULL,
                                    icon = icon(
                                      "download-alt",
                                      lib = "glyphicon"
                                    ),
                                    style = "display: block; height: 45px; width: 45px;
                                         border-radius: 50%; border: 2px solid #4F7942;
                                         background: linear-gradient(135deg, #6CC283, #4F7942);
                                         color: white; transition: all 0.3s ease;
                                         box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
                                  )
                                )
                              )
                            ),

                            column(
                              6,
                              div(
                                align = "center",
                                div(
                                  title = "Back to imported text(s)",
                                  do.call(
                                    "actionButton",
                                    c(
                                      back_bttn,
                                      list(
                                        inputId = "importTextBack",
                                        style = "display: block; height: 45px; width: 45px;
                                             border-radius: 50%; border: 2px solid #4F7942;
                                             background: linear-gradient(135deg, #6CC283, #4F7942);
                                             color: white; transition: all 0.3s ease;
                                             box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "Info & References",
            fluidPage(
              fluidRow(
                column(
                  12,
                  div(
                    style = "padding: 30px; background: white; border-radius: 8px;
                         box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-top: 20px;",
                    HTML(infoTexts$importmenu)
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


importServer <- function(input, output, session, values, statsValues) {
  ## Code to reset shiny app
  reset_rv <- reactiveVal(value = 0L)
  session$onSessionEnded(function() {
    # x <- Inf
    x <- isolate(reset_rv())

    if (!is.null(x)) {
      if (x == 0) {
        stopApp()
      }
    }
  })
  ###

  output$resetButton <- renderUI({
    reset_bttn <- list(
      label = NULL,
      style = "margin-top: -8px; font-size: 8px; border-radius:2%",
      # style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
      icon = icon(name = "refresh", lib = "glyphicon")
    )
    do.call(
      "actionButton",
      c(
        reset_bttn,
        list(
          inputId = "resetApp"
        )
      )
    )
  })

  observeEvent(input$resetApp, {
    ask_confirmation(
      inputId = "reset_confirmation",
      title = "Restart TALL",
      text = HTML(
        "Restarting TAll will result in the loss of all analyses currently in progress<br><br>
                  <b>Do you want to confirm?</b>"
      ),
      html = TRUE,
      type = "warning",
      btn_labels = c("CANCEL", "CONFIRM")
    )
  })

  observeEvent(input$reset_confirmation, {
    if (isTRUE(input$reset_confirmation)) {
      reset_rv(input$resApp)
      session$reload()
      param_stay_page <<- TRUE
    }
  })

  if (param_stay_page) {
    updateTabItems(session, "sidebarmenu", "import_tx")
    param_stay_page_newPT <<- FALSE
  }

  ## observe Gemini copy2clipboard button
  observeEvent(input$copy_btn, {
    content <- geminiSave(values, input$sidebarmenu)
    copy_to_clipboard(content)
  })

  # ## observe Gemini Save button
  observeEvent(input$save_btn, {
    filename <- paste0(values$wdTall, "/TallAI_", input$sidebarmenu, ".txt")
    txtOutput <- geminiSave(values, input$sidebarmenu)
    readr::write_lines(txtOutput, file = filename)
  })

  ## observe gemini generate button
  observeEvent(input$gemini_btn, {
    values$gemini_additional <- input$gemini_additional ## additional info to Gemini prompt
    values <- geminiWaitingMessage(values, input$sidebarmenu)
    values <- geminiGenerate(
      values,
      input$sidebarmenu,
      values$gemini_additional,
      values$gemini_model_parameters,
      input
    )
  })

  ### IMPORT ----
  output$runButton <- renderUI({
    if (!isTRUE(values$resetNeed)) {
      list(
        selectInput(
          "load",
          "Please, choose what to do",
          choices = c(
            " " = "null",
            "Load text files" = "import",
            "Load file from Biblioshiny" = "biblioshiny",
            "Load Tall structured files" = "load_tall",
            "Wikipedia pages" = "wiki",
            "Use a sample collection" = "demo"
          ),
          selected = "null"
        ),
        conditionalPanel(
          condition = "input.load == 'wiki'",
          textInput(
            inputId = "wikiWord",
            label = "Search Wikipedia",
            value = NULL
          ),
          sliderTextInput(
            inputId = "wikiN",
            label = "Pages",
            choices = seq(1, 20),
            selected = 1,
            animate = TRUE
          ),
          helpText(
            em(
              "By specifying a search phrase in 'Search Wikipedia',
                                         the content of up to 20 Wikipedia pages can be downloaded."
            ),
            br(),
            br(),
            em(
              "The content of each wiki page will be stored in the 'text' column.
                                         In addition, the page title, abstract and url will also be stored."
            ),
            br(),
            br(),
            em("The page title will be used as the 'doc_id'.")
          )
        ),
        conditionalPanel(
          condition = "input.load == 'import'",
          fluidRow(
            column(
              6,
              selectizeInput(
                "ext",
                label = "File format",
                choices = c(
                  "txt" = "txt",
                  "csv" = "csv",
                  "excel" = "xlsx",
                  "pdf" = "pdf"
                ),
                tags$style("height: 50px")
              )
            ),
            conditionalPanel(
              condition = "input.load == 'import' & input.ext=='csv'",
              column(
                6,
                selectizeInput(
                  "line_sep",
                  label = "CSV Separator",
                  choices = c(
                    " , " = ",",
                    " ; " = ";"
                  ),
                  tags$style("height: 50px")
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.load == 'import' | input.load == 'biblioshiny'",
          uiOutput("file_rawUI"),
          uiOutput("biblioChoiceUI"), # select text field in file from bibliometrix
          uiOutput("biblioRunUI"), # apply
          uiOutput(outputId = "infoImport"),
          conditionalPanel(
            condition = "input.ext == 'xlsx' ||  input.ext =='csv'",
            uiOutput(outputId = "infoTextLabel")
          )
        ),
        conditionalPanel(
          condition = "input.load=='demo'",
          selectInput(
            "demo_file",
            label = "Select sample texts",
            choices = c(
              "BBC news" = "bbc",
              "Bibliometrix" = "bibliometrix",
              "US Airlines Tweets" = "usairlines"
            ),
            selected = "bibliometrix"
          ),
          conditionalPanel(
            condition = "input.demo_file=='bibliometrix'",
            helpText(
              em(
                "The dataset is composed of a collection of 444 scientific articles written in English
                                           in which the authors used the Bibliometrix R package to perform systematic literature reviews."
              ),
              br(),
              br(),
              em(
                "The textual data consists of the article abstracts, while the additional information includes
                                           metadata such as the list of co-authors, first author, year of publication, and journal name."
              ),
              br(),
              br(),
              em("The abstracts have already been tokenized and POS tagged.")
            )
          ),
          conditionalPanel(
            condition = "input.demo_file=='bbc'",
            helpText(
              em(
                "A collection of 386 short news stories published in the entertainment section of the BBC News website."
              ),
              br(),
              br(),
              em("The texts are in English.")
            )
          ),
          conditionalPanel(
            condition = "input.demo_file=='usairlines'",
            helpText(
              em(
                "The dataset is the 'Twitter US Airline Sentiment' collection, a publicly available and widely used dataset originally hosted on Kaggle. It contains tweets collected during February 2015 that pertain to major U.S.-based airlines."
              ),
              br(),
              br(),
              em("The texts are in English.")
            )
          )
        ),
        conditionalPanel(
          condition = "input.load == 'load_tall'",
          helpText(em("Load a collection previously exported from Tall")),
          fileInput(
            "file1",
            "Choose a file",
            multiple = FALSE,
            accept = c(
              ".tall"
            )
          )
        ),
        conditionalPanel(
          condition = "input.load != 'null'",
          div(
            align = "center",
            width = 12,
            actionButton(
              inputId = "runImport",
              label = div(
                icon(name = "play", lib = "glyphicon"),
                strong("START")
              ),
              icon = NULL,
              style = "border-radius: 20px; border-width: 1px;
                                                                    font-size: 17px; color: #ffff;"
            )
          )
        )
      )
    } else {
      list(
        textAreaInput(
          inputId = "corpus_description",
          label = "Please provide a brief description of your corpus (e.g., source, type of content, domain) to improve prompts for the TALL AI Assistant:",
          placeholder = "Example: The corpus consists of 150 academic articles from biomedical journals published between 2015 and 2020...",
          value = values$corpus_description,
          rows = 8,
          width = "100%"
        ),
        div(
          align = "center",
          width = 12,
          actionButton(
            inputId = "applyCorpusDescription",
            label = div(
              icon(name = "check", lib = "glyphicon"),
              strong("Update")
            ),
            icon = NULL,
            style = "border-radius: 20px; border-width: 1px;
                                                                    font-size: 17px; color: #ffff;"
          )
        ),
        br(),
        helpText(
          em(
            "To load a new text collection, it is necessary to reset the app."
          )
        ),
        br(),
        div(
          align = "center",
          width = 12,
          actionButton(
            inputId = "runReset2",
            label = div(
              icon(name = "refresh", lib = "glyphicon"),
              strong("RESET")
            ),
            icon = NULL,
            style = "border-radius: 20px; border-width: 1px;
                                                                    font-size: 17px; color: #ffff;"
          )
        )
      )
    }
  })

  observeEvent(input$runImport, {
    values$corpus_description <- input$corpus_description
    updateTextAreaInput(
      session,
      "corpus_description",
      value = values$corpus_description
    )
  })

  observeEvent(input$applyCorpusDescription, {
    values$corpus_description <- input$corpus_description
    show_alert(
      title = "Corpus description updated",
      text = "The corpus description has been successfully updated.",
      type = "success"
    )
  })

  observeEvent(input$runReset2, {
    ask_confirmation(
      inputId = "reset_confirmation2",
      title = "Restart TALL",
      text = HTML(
        "Restarting TAll will result in the loss of all analyses currently in progress<br><br>
                  <b>Do you want to confirm?</b>"
      ),
      html = TRUE,
      type = "warning",
      btn_labels = c("CANCEL", "CONFIRM")
    )
  })

  observeEvent(input$reset_confirmation2, {
    if (isTRUE(input$reset_confirmation2)) {
      reset_rv(input$runReset2)
      session$reload()
      param_stay_page <<- TRUE
    }
  })

  output$file_rawUI <- renderUI({
    switch(
      input$ext,
      txt = {
        ext <- c("text/plain", ".txt", ".zip")
      },
      csv = {
        ext <- c("text/csv", ".csv", ".zip")
      },
      xlsx = {
        ext <- c("excel", ".xlsx", ".xls", ".zip")
      },
      pdf = {
        ext <- c(".pdf", ".zip")
      }
    )

    if (input$load == "biblioshiny") {
      ext <- c("text/csv", ".csv", ".zip")
    }

    fileInput(
      "file_raw",
      "Select file(s) containing text",
      multiple = TRUE,
      accept = ext,
      placeholder = "No file(s) selected"
    )
  })

  output$infoTextLabel <- renderUI({
    shinyWidgets::alert(
      icon("warning"),
      tags$b("Warning!"),
      br(),
      HTML(
        "The column including text(s) in your CSV or EXCEL file must be named <b>text</b>"
      ),
      status = "warning"
    )
    # }
  })

  ### dataImported ----
  DATAloading <- eventReactive(input$runImport, {
    switch(
      input$load,
      import = {
        if (!is.null(req(input$file_raw))) {
          file <- input$file_raw
          txt <- read_files(
            file,
            ext = input$ext,
            subfolder = FALSE,
            line_sep = input$line_sep
          )
          txt <- txt %>% clean_text() %>% trim_text_columns() ## clean text before tokenization
          values$menu <- 0
          values$custom_lists <- NULL
          values$txt <- txt %>%
            mutate(
              text = removeHTMLTags(text),
              text_original = text
            ) %>%
            arrange(doc_id)
          values$resetNeed <- TRUE
        }
      },
      biblioshiny = {
        if (!is.null(req(input$file_raw))) {
          file <- input$file_raw
          values$txt <- read_files(
            file,
            ext = "csv",
            subfolder = FALSE,
            line_sep = ","
          )
          values$biblioshiny <- TRUE
        }
      },
      load_tall = {
        req(input$file1)
        file_tall <- input$file1$datapath
        load(file_tall)
        values$menu <- menu
        values$dfTag <- dfTag
        values$txt <- rebuild_documents(dfTag)
        values$custom_lists <- custom_lists
        values$language <- language
        values$treebank <- treebank
        values$D <- D
        values$where <- where
        values$corpus_description <- corpus_description
        statsValues <- updateStats(
          values$dfTag,
          term = values$generalTerm,
          statsValues
        )
        if (exists("generalTerm")) {
          values$generalTerm <- generalTerm
        }
        values$resetNeed <- TRUE
        # values$metadata <- metadata
        if (values$menu == 1) {
          updateTabItems(session, "sidebarmenu", "custTermList")
        }
        if (values$menu > 1) {
          updateTabItems(session, "sidebarmenu", "posTagSelect")
        }
        if (ncol(values$dfTag) > 1) {
          showModal(loadTallgModal(session))
        }
      },
      demo = {
        switch(
          input$demo_file,
          bibliometrix = {
            file_tall <- loadSampleCollection("bibliometrix")
            load(file_tall)
            values$menu <- menu
            values$dfTag <- dfTag
            values$txt <- rebuild_documents(dfTag)
            values$custom_lists <- custom_lists
            values$language <- language
            values$D <- D
            values$where <- where
            values$corpus_description <- "The dataset is composed of a collection of 444 scientific articles written in English in which the authors used the Bibliometrix R package to perform systematic literature reviews.\n The textual data consists of the article abstracts, while the additional information includes metadata such as the list of co-authors, the first author, the year of publication, and the journal name."
            values$resetNeed <- TRUE
            statsValues <- updateStats(
              values$dfTag,
              term = values$generalTerm,
              statsValues
            )

            if (values$menu == 1) {
              updateTabItems(session, "sidebarmenu", "custTermList")
            }
            if (values$menu > 1) {
              updateTabItems(session, "sidebarmenu", "posTagSelect")
            }
            if (ncol(values$dfTag) > 1) {
              showModal(loadTallgModal(session))
            }
          },
          bbc = {
            file_tall <- loadSampleCollection("bbc")
            files <- list(name = "bbc.zip", datapath = file_tall)
            txt <- read_files(files, ext = "txt", subfolder = FALSE)
            values$menu <- 0
            values$custom_lists <- NULL
            values$corpus_description <- "A collection of 386 short news published in the entertainment section of the BBC News website."
            values$resetNeed <- TRUE
            values$txt <- txt %>%
              mutate(text_original = text) %>%
              arrange(doc_id)
          },
          usairlines = {
            file_tall <- loadSampleCollection("usairlines")
            files <- list(name = "usairlines.zip", datapath = file_tall)
            txt <- read_files(files, ext = "csv", subfolder = FALSE)
            values$menu <- 0
            values$custom_lists <- NULL
            values$corpus_description <- paste0(
              "The dataset under analysis is the 'Twitter US Airline Sentiment' collection, a publicly available",
              " and widely used dataset originally hosted on Kaggle. It contains tweets collected during February 2015",
              " that pertain to major U.S.-based airlines. The primary objective of this dataset is to capture and analyze",
              " the sentiment expressed by travelers regarding their airline experiences. Each tweet is annotated with ",
              "a sentiment labelâ€”positive, neutral, or negativeâ€”based on the emotional tone conveyed in the text. ",
              "This collection is frequently employed in natural language processing tasks, particularly for training ",
              "and evaluating sentiment classification models. In this context, the dataset will be used to explore how ",
              "airline passengers articulated their opinions and emotions on Twitter during the specified period."
            )
            values$resetNeed <- TRUE
            values$txt <- txt %>%
              mutate(text_original = text) %>%
              arrange(doc_id)
          }
        )
      },
      wiki = {
        df <- wikiSearch(input$wikiWord, n = as.numeric(input$wikiN))
        if (is.null(df)) {
          show_alert(
            title = "No results found!",
            text = "It seems there are no Wikipedia pages matching your search.",
            type = "error"
          )
          values$resetNeed <- FALSE
        } else {
          values$menu <- 0
          values$custom_lists <- NULL
          values$txt <- wikiExtract(df) %>%
            mutate(text_original = text) %>%
            rename(
              doc_id = title,
              doc_selected = selected
            )

          values$resetNeed <- TRUE
        }
      }
    )
  })

  observeEvent(
    eventExpr = {
      values$biblioshiny
    },
    handlerExpr = {
      tallFields <- intersect(c("AB", "TI", "DE"), names(values$txt))
      output$biblioChoiceUI <- renderUI({
        selectizeInput(
          inputId = "biblioChoice",
          label = "Choose the text column",
          choices = tallFields,
          selected = NULL,
          multiple = FALSE
        )
      })
      hide("runImport")
      output$biblioRunUI <- renderUI({
        div(
          align = "center",
          width = 12,
          br(),
          actionButton(
            inputId = "biblioRun",
            label = div(
              icon(name = "play", lib = "glyphicon"),
              strong("Apply")
            ),
            icon = NULL,
            style = "border-radius: 20px; border-width: 1px;
                                                                    font-size: 17px; color: #ffff;"
          )
        )
      })
    }
  )

  observeEvent(
    eventExpr = {
      input$biblioRun
    },
    handlerExpr = {
      req(input$biblioChoice)
      values$txt <- values$txt %>%
        mutate(
          text = removeHTMLTags(!!sym(input$biblioChoice)),
          text_original = text
        ) %>%
        clean_text() %>%
        trim_text_columns() %>%
        mutate(doc_selected = TRUE) %>%
        arrange(doc_id)
      values$menu <- 0
      values$custom_lists <- NULL
      values$resetNeed <- TRUE

      output$dataImported <- DT::renderDT({
        req(values$txt)
        if (nrow(values$txt) > 0) {
          col_names <- c("doc_selected", "text_original")
          # check if text exists in values$txt and then apply or not the mutate

          DTformat(
            values$txt %>%
              dplyr::filter(doc_selected) %>%
              mutate(across(
                any_of("text"),
                ~ paste0(substr(., 1, 500), "...")
              )) %>%
              select(doc_id, any_of("text"), everything()) %>%
              select(!any_of(col_names)),
            left = 3,
            nrow = 5,
            filter = "none",
            button = TRUE,
            delete = TRUE
          )
        }
      })
    }
  )

  # Trigger DATAloading when runImport is clicked
  observeEvent(input$runImport, {
    DATAloading()
  })

  output$dataImported <- DT::renderDT({
    req(values$txt)
    if (nrow(values$txt) > 0) {
      col_names <- c("doc_selected", "text_original")
      # check if text exists in values$txt and then apply or not the mutate

      DTformat(
        values$txt %>%
          dplyr::filter(doc_selected) %>%
          mutate(across(any_of("text"), ~ paste0(substr(., 1, 500), "..."))) %>%
          select(doc_id, any_of("text"), everything()) %>%
          select(!any_of(col_names)),
        left = 3,
        nrow = 5,
        filter = "none",
        button = TRUE,
        delete = TRUE
      )
    }
  })

  ### shortpath for folder path ----
  output$folder <- renderUI({
    path <- shortpath(values$path)
    if (is.null(path)) {
      path <- " --- "
    }
    HTML(paste("<pre class='tab'>", path, sep = ""))
  })

  loadTallgModal <- function(session) {
    ns <- session$ns
    modalDialog(
      title = div(
        style = "background: linear-gradient(135deg, #5cb85c 0%, #4F7942 100%);
               color: white;
               padding: 20px;
               margin: -15px -15px 20px -15px;
               border-radius: 5px 5px 0 0;",
        h3(
          icon("info-circle", lib = "font-awesome"),
          strong(" TALL Data Overview"),
          style = "margin: 0; color: white;"
        )
      ),
      div(
        style = "padding: 10px;",
        uiOutput("loadSynthesis")
      ),
      size = "l",
      easyClose = TRUE,
      footer = div(
        style = "background-color: #f8f9fa;
               padding: 15px;
               margin: 20px -15px -15px -15px;
               border-top: 2px solid #dee2e6;",
        tagList(
          actionButton(
            label = tagList(
              icon("th-list", lib = "glyphicon"),
              " Custom Lists"
            ),
            inputId = "modalCustomLists",
            style = "background-color: #5cb85c;
                   color: white;
                   border: none;
                   padding: 10px 20px;
                   border-radius: 5px;
                   font-weight: bold;
                   margin-right: 10px;"
          ),
          actionButton(
            label = tagList(icon("remove", lib = "glyphicon"), " Close"),
            inputId = "closeModalCustomLists",
            style = "background-color: #6c757d;
                   color: white;
                   border: none;
                   padding: 10px 20px;
                   border-radius: 5px;
                   font-weight: bold;"
          )
        )
      )
    )
  }

  observeEvent(input$closeModalCustomLists, {
    removeModal(session = getDefaultReactiveDomain())
  })

  output$loadSynthesis <- renderUI({
    ndocs <- length(unique(values$dfTag$doc_id))

    # Prepare data
    txt1 <- paste0(ndocs, " documents")
    txt2 <- values$D
    txt2b_lang <- tools::toTitleCase(values$language)
    txt2b_tree <- values$treebank
    txt2c <- values$corpus_description

    if (!is.null(dim(values$custom_lists))) {
      ncust <- nrow(values$custom_lists)
      txt3 <- paste0(ncust, " words")
      custom_status <- "included"
      custom_badge_color <- "#5cb85c"
    } else {
      txt3 <- "Not included"
      custom_status <- "not-included"
      custom_badge_color <- "#6c757d"
    }

    upos <- values$dfTag %>%
      select(upos) %>%
      pull() %>%
      unique()

    if ("MULTIWORD" %in% upos) {
      txt3bis <- "Included"
      multi_badge_color <- "#5cb85c"
    } else {
      txt3bis <- "Not included"
      multi_badge_color <- "#6c757d"
    }

    items <- toupper(c(
      "email",
      "url",
      "hash",
      "emoji",
      "ip_address",
      "mention"
    ))

    if (length(intersect(items, upos)) > 0) {
      txt3ter <- paste0(
        tools::toTitleCase(tolower(intersect(items, upos))),
        collapse = ", "
      )
      entity_badge_color <- "#5cb85c"
    } else {
      txt3ter <- "Not included"
      entity_badge_color <- "#6c757d"
    }

    txt4 <- values$where

    # Create modern styled layout
    tagList(
      # Main Info Card
      div(
        style = "background: white;
               border-left: 4px solid #5cb85c;
               padding: 15px 20px;
               margin-bottom: 15px;
               box-shadow: 0 2px 4px rgba(0,0,0,0.1);
               border-radius: 5px;",
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          icon(
            "file-text",
            lib = "font-awesome",
            style = "font-size: 24px; color: #5cb85c; margin-right: 10px;"
          ),
          h4(strong("Corpus Information"), style = "margin: 0; color: #2c3e50;")
        ),
        hr(style = "margin: 10px 0; border-color: #e0e0e0;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          # Documents
          div(
            style = "padding: 10px;",
            div(
              style = "color: #6c757d; font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 5px;",
              "Documents"
            ),
            div(
              style = "font-size: 24px; font-weight: bold; color: #2c3e50;",
              txt1
            )
          ),
          # Last Modified
          div(
            style = "padding: 10px;",
            div(
              style = "color: #6c757d; font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 5px;",
              "Last Modified"
            ),
            div(
              style = "font-size: 16px; color: #2c3e50;",
              txt2
            )
          )
        )
      ),

      # Language & Treebank Card
      div(
        style = "background: white;
               border-left: 4px solid #17a2b8;
               padding: 15px 20px;
               margin-bottom: 15px;
               box-shadow: 0 2px 4px rgba(0,0,0,0.1);
               border-radius: 5px;",
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          icon(
            "globe",
            lib = "glyphicon",
            style = "font-size: 20px; color: #17a2b8; margin-right: 10px;"
          ),
          h4(strong("Language Settings"), style = "margin: 0; color: #2c3e50;")
        ),
        hr(style = "margin: 10px 0; border-color: #e0e0e0;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          div(
            style = "padding: 10px;",
            div(
              style = "color: #6c757d; font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 5px;",
              "Language"
            ),
            span(
              txt2b_lang,
              style = "background-color: #17a2b8;
                     color: white;
                     padding: 5px 12px;
                     border-radius: 15px;
                     font-weight: 600;
                     font-size: 14px;"
            )
          ),
          div(
            style = "padding: 10px;",
            div(
              style = "color: #6c757d; font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 5px;",
              "Treebank"
            ),
            span(
              txt2b_tree,
              style = "background-color: #17a2b8;
                     color: white;
                     padding: 5px 12px;
                     border-radius: 15px;
                     font-weight: 600;
                     font-size: 14px;"
            )
          )
        )
      ),

      # Description Card
      div(
        style = "background: #f8f9fa;
               border-left: 4px solid #6c757d;
               padding: 15px 20px;
               margin-bottom: 15px;
               box-shadow: 0 2px 4px rgba(0,0,0,0.1);
               border-radius: 5px;",
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          icon(
            "info-sign",
            lib = "glyphicon",
            style = "font-size: 20px; color: #6c757d; margin-right: 10px;"
          ),
          h4(strong("Corpus Description"), style = "margin: 0; color: #2c3e50;")
        ),
        hr(style = "margin: 10px 0; border-color: #dee2e6;"),
        p(txt2c, style = "margin: 0; color: #495057; line-height: 1.6;")
      ),

      # Features Card
      div(
        style = "background: white;
               border-left: 4px solid #f39c12;
               padding: 15px 20px;
               margin-bottom: 15px;
               box-shadow: 0 2px 4px rgba(0,0,0,0.1);
               border-radius: 5px;",
        div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          icon(
            "tags",
            lib = "glyphicon",
            style = "font-size: 20px; color: #f39c12; margin-right: 10px;"
          ),
          h4(
            strong("Preprocessing Features"),
            style = "margin: 0; color: #2c3e50;"
          )
        ),
        hr(style = "margin: 10px 0; border-color: #e0e0e0;"),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
          # Special Entities
          div(
            style = "padding: 10px;",
            div(
              style = "color: #6c757d; font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 8px;",
              icon("star", lib = "glyphicon", style = "margin-right: 5px;"),
              "Special Entities"
            ),
            span(
              txt3ter,
              style = paste0(
                "background-color: ",
                entity_badge_color,
                ";
                           color: white;
                           padding: 5px 12px;
                           border-radius: 15px;
                           font-weight: 600;
                           font-size: 13px;
                           display: inline-block;"
              )
            )
          ),
          # Multi-Words
          div(
            style = "padding: 10px;",
            div(
              style = "color: #6c757d; font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 8px;",
              icon("link", lib = "glyphicon", style = "margin-right: 5px;"),
              "Multi-Words"
            ),
            span(
              txt3bis,
              style = paste0(
                "background-color: ",
                multi_badge_color,
                ";
                           color: white;
                           padding: 5px 12px;
                           border-radius: 15px;
                           font-weight: 600;
                           font-size: 13px;
                           display: inline-block;"
              )
            )
          )
        ),
        div(
          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 15px;",
          # Custom Word List
          div(
            style = "padding: 10px;",
            div(
              style = "color: #6c757d; font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 8px;",
              icon("list", lib = "glyphicon", style = "margin-right: 5px;"),
              "Custom Word List"
            ),
            span(
              txt3,
              style = paste0(
                "background-color: ",
                custom_badge_color,
                ";
                           color: white;
                           padding: 5px 12px;
                           border-radius: 15px;
                           font-weight: 600;
                           font-size: 13px;
                           display: inline-block;"
              )
            )
          ),
          # Last Step
          div(
            style = "padding: 10px;",
            div(
              style = "color: #6c757d; font-size: 12px; font-weight: 600; text-transform: uppercase; margin-bottom: 8px;",
              icon("flag", lib = "glyphicon", style = "margin-right: 5px;"),
              "Last Preprocessing Step"
            ),
            span(
              txt4,
              style = "background-color: #5cb85c;
                     color: white;
                     padding: 5px 12px;
                     border-radius: 15px;
                     font-weight: 600;
                     font-size: 13px;
                     display: inline-block;"
            )
          )
        )
      )
    )
  })

  observeEvent(input$modalCustomLists, {
    if (!is.null(values$custom_lists)) {
      text <- tagList(
        DTformat(
          values$custom_lists,
          left = c(1, 2),
          filename = "Custom_lists_table"
        )
      )
    } else {
      text <- tagList(
        div(
          h4(HTML("No custom lists to show.")),
          style = "text-align:left"
        )
      )
    }

    show_alert(
      title = "Custom Word Lists",
      text = text,
      type = NULL,
      size = "m",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      btn_labels = "OK",
      btn_colors = "#6CC283",
      timer = NULL,
      imageUrl = "",
      animation = TRUE
    )
  })

  ### Convert Raw Data in Excel functions ----

  observeEvent(
    eventExpr = {
      input$collection.save
    },
    handlerExpr = {
      file_path <- destFolder(
        paste("Tall-Export-File-", sys.time(), ".csv", sep = ""),
        values$wdTall
      )
      readr::write_csv(
        x = values$txt,
        file = file_path,
        na = "NA",
        append = FALSE,
        col_names = TRUE
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  # Back to the original import text ----
  observeEvent(input$importTextBack, {
    values$txt <- values$txt %>%
      mutate(doc_selected = TRUE)
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
}

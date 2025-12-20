preprocessingUI <- function() {
  ## Tokenization & PoS Tagging -----

  tokpos <- tabItem(
    tabName = "tokPos",
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Annotated Text Table",
        fluidRow(
          column(
            8,
            h3(strong("Tokenization & PoS Tagging"), align = "center"),
            br(),
            shinycssloaders::withSpinner(
              DT::DTOutput("tokPosTagData"),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          column(
            4,
            div(
              box(
                width = 12,
                title = tags$div(
                  style = "display: flex; align-items: center;", # Allineamento orizzontale
                  tags$h4(
                    "Language Model   ",
                    style = "margin-top:-20px; margin-right: 40px; font-weight: bold"
                  ), # Testo con margine a destra
                  uiOutput("flagUI") # Immagine SVG
                ),
                br(),
                div(
                  fluidRow(
                    column(
                      6,
                      div(
                        uiOutput("optionsTokenization")
                        # selectInput(
                        #   inputId = 'language_model', label="Language", choices = label_lang,
                        #   multiple=FALSE,
                        #   width = "100%"
                        # )
                      ),
                      style = "margin-top:-9px"
                    ),
                    column(
                      6,
                      div(
                        uiOutput("treebankSelect")
                        # selectInput("treebank", "Treebank", choices = "GUM")
                      ),
                      style = "margin-top:-9px"
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      div(
                        align = "center",
                        style = "margin-top:-15px",
                        width = 12,
                        title = t_run,
                        do.call(
                          "actionButton",
                          c(
                            run_bttn,
                            list(
                              inputId = "tokPosRun"
                            )
                          )
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        align = "center",
                        style = "margin-top:-15px",
                        width = 12,
                        title = t_save,
                        do.call(
                          "actionButton",
                          c(
                            list(
                              label = NULL,
                              style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 15px;",
                              icon = icon(
                                name = "floppy-save",
                                lib = "glyphicon"
                              )
                            ),
                            list(
                              inputId = "tokPosSave"
                            )
                          )
                        )
                      )
                    )
                  ),
                  style = "margin-top:-5px"
                ),
                tags$hr(),
                uiOutput("info_treebank"),
                tags$hr(),
                div(
                  style = "margin-top: 12px; margin-bottom: 12px; padding: 15px; border-radius: 6px;",

                  # Token Normalization section (Teal theme)
                  div(
                    style = "display: flex; align-items: center; gap: 15px; padding: 12px; background: linear-gradient(135deg, #e0f7f4 0%, #e8f6f3 100%); border-radius: 6px; border-left: 4px solid #1ABC9C; margin-bottom: 10px;",
                    div(
                      style = "display: flex; align-items: center; flex-grow: 1;",
                      icon(
                        "sort-alpha-down",
                        style = "color: #1ABC9C; margin-right: 10px; font-size: 22px;"
                      ),
                      span(
                        "Token Normalization (Lowercase)",
                        style = "font-weight: bold; color: #16A085; font-size: 15px;"
                      )
                    ),
                    div(
                      style = "flex-shrink: 0;",
                      materialSwitch(
                        inputId = "token_lowercase",
                        label = NULL,
                        value = TRUE,
                        status = "success"
                      )
                    )
                  ),

                  # Lemma Normalization section (Blue theme)
                  div(
                    style = "display: flex; align-items: center; gap: 15px; padding: 12px; background: linear-gradient(135deg, #e3f2fd 0%, #e8eaf6 100%); border-radius: 6px; border-left: 4px solid #3498DB;",
                    div(
                      style = "display: flex; align-items: center; flex-grow: 1;",
                      icon(
                        "exchange-alt",
                        style = "color: #3498DB; margin-right: 10px; font-size: 22px;"
                      ),
                      span(
                        "Lemma Normalization (Lowercase)",
                        style = "font-weight: bold; color: #2980B9; font-size: 15px;"
                      )
                    ),
                    div(
                      style = "flex-shrink: 0;",
                      materialSwitch(
                        inputId = "lemma_lowercase",
                        label = NULL,
                        value = TRUE,
                        status = "primary"
                      )
                    )
                  )
                ),
                uiOutput("unitAnalysis")
              )
            ),
            style = "margin-top:40px"
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
              HTML(infoTexts$tokenization)
            ),
            column(1)
          )
        )
      )
    )
  )

  ### POS Special Tagging ----
  specialentities <- tabItem(
    tabName = "posSpecial",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(strong("Tagging Special Entities"), align = "center")
        )
      )
    ),
    br(),
    fluidRow(
      column(
        9,
        tabsetPanel(
          type = "tabs",
          # Main tab with HTML-based entity cards
          tabPanel(
            "Special Entities Overview",
            div(
              style = "padding: 20px;",
              # Custom HTML output for special entities cards
              shinycssloaders::withSpinner(
                uiOutput("posSpecialCardsUI"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            )
          ),
          # Annotated text table tab (unchanged)
          tabPanel(
            "Annotated Text Table",
            shinycssloaders::withSpinner(
              DT::DTOutput("posSpecialData"),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          # Info tab (unchanged)
          tabPanel(
            "Info & References",
            fluidPage(
              fluidRow(
                column(1),
                column(
                  10,
                  br(),
                  HTML(infoTexts$specialentities)
                ),
                column(1)
              )
            )
          )
        )
      ),
      column(
        3,
        div(
          box(
            width = 12,
            div(
              h3(strong(em("Special Entities"))),
              style = "margin-top:-57px"
            ),
            tags$hr(),
            helpText(h5(
              "When processing text, special tags will be assigned to certain detected entities."
            )),
            helpText(h5("These include:")),
            helpText(h5("•    Email addresses: example@domain.com")),
            helpText(h5("•    URLs: https://www.example.com/path")),
            helpText(h5("•   Emojis: 😊, 🚀, ❤️")),
            helpText(h5("•    Hashtags: #ExampleTag")),
            helpText(h5("•    IP addresses: 192.168.1.1")),
            helpText(h5("•    Mentions: @username")),
            helpText(h5(
              "This ensures that these elements are identified and marked for further analysis within the text."
            )),
            style = "text-align: left; text-color: #989898",
            br(),
            div(
              hr(),
              div(
                fluidRow(
                  column(
                    4,
                    div(
                      align = "center",
                      style = "margin-top:-15px",
                      width = 12,
                      do.call(
                        "actionButton",
                        c(
                          run_bttn,
                          list(
                            inputId = "posSpecialRun"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    div(
                      align = "center",
                      style = "margin-top:-15px",
                      width = 12,
                      title = t_back,
                      do.call(
                        "actionButton",
                        c(
                          back_bttn,
                          list(
                            inputId = "posSpecialBack"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    div(
                      title = t_save,
                      div(
                        align = "center",
                        do.call(
                          "actionButton",
                          c(
                            save_bttn,
                            list(
                              inputId = "posSpecialSave"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                style = "margin-top: -8px"
              ),
              style = "margin-top:-15px"
            )
          )
        ),
        style = "margin-top:40px"
      )
    )
  )

  ## Custom PoS Lists -----

  custompos <- tabItem(
    tabName = "custTermList",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(strong("Custom PoS List Loading and Merging"), align = "center")
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          9,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Pos Tagging with Custom List",
              shinycssloaders::withSpinner(
                DT::DTOutput("customPosTagData"),
                color = getOption("spinner.color", default = "#4F7942")
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
                    HTML(infoTexts$customterm)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          3,
          div(
            box(
              width = 12,
              div(
                h3(strong(em("Import Custom PoS List"))),
                style = "margin-top:-57px"
              ),
              hr(),
              # selectInput("CTLterm",
              #             "Terms:",
              #             choices = c("Tokens" = "token",
              #                         "Lemma" = "lemma"),
              #             selected = "lemma"),
              helpText(h5(
                "Please ensure that the Custom PoS List is formatted as an Excel file with two columns.
                                       In the first column include the list of terms.
                                       In the second column provide the corresponding list of PoS associated with each term."
              )),
              fileInput(
                "custom_lists",
                label = NULL,
                multiple = TRUE,
                accept = c(
                  ".csv",
                  ".xls",
                  ".xlsx"
                )
              ),
              hr(),
              fluidRow(
                column(
                  4,
                  div(
                    align = "center",
                    style = "margin-top:-15px",
                    width = 12,
                    do.call(
                      "actionButton",
                      c(
                        run_bttn,
                        list(
                          inputId = "custTermListRun"
                        )
                      )
                    )
                  )
                ),
                column(
                  4,
                  div(
                    align = "center",
                    style = "margin-top:-15px",
                    width = 12,
                    title = t_back,
                    do.call(
                      "actionButton",
                      c(
                        back_bttn,
                        list(
                          inputId = "custTermListBack"
                        )
                      )
                    )
                  )
                ),
                column(
                  4,
                  div(
                    title = t_save,
                    div(
                      align = "center",
                      do.call(
                        "actionButton",
                        c(
                          save_bttn,
                          list(
                            inputId = "custTermSave"
                          )
                        )
                      )
                    )
                  )
                )
              ),
              hr(),
              div(
                helpText(
                  "Pressing Run Button will delete previous custom PoS"
                ),
                style = "text-align: center"
              )
            ),
            style = "margin-top:40px"
          )
        )
      )
    )
  )

  # Synonyms Management -----

  synonyms <- tabItem(
    tabName = "synonymsMgmt",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(strong("Synonyms Merging"), align = "center")
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          9,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Processed Data with Synonyms",
              shinycssloaders::withSpinner(
                DT::DTOutput("synonymsProcessedData"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Synonyms List Preview",
              shinycssloaders::withSpinner(
                DT::DTOutput("synonymsListPreview"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Info & Instructions",
              fluidPage(
                fluidRow(
                  column(1),
                  column(
                    10,
                    br(),
                    HTML(helpContent()$synonyms)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          3,
          div(
            box(
              width = 12,
              div(
                h3(strong(em("Import Synonyms File"))),
                style = "margin-top:-57px"
              ),
              hr(),
              helpText(h5(
                "Upload a CSV or Excel file with your synonyms list. Column 2 must contain the PoS tag (upos) to assign."
              )),
              fileInput(
                "synonyms_file",
                label = NULL,
                multiple = FALSE,
                accept = c(
                  ".csv",
                  ".xls",
                  ".xlsx"
                )
              ),
              hr(),
              helpText(
                uiOutput("synonymsStats")
              ),
              hr(),
              # Download buttons section
              fluidRow(
                column(
                  6,
                  downloadButton(
                    "downloadSynonymsTemplate",
                    "Template",
                    style = "width: 100%; margin-bottom: 10px; background-color: #6CC283; border-color: #4F7942;color: #ffffff;"
                  )
                ),
                column(
                  6,
                  downloadButton(
                    "downloadVocabulary",
                    "Vocabulary",
                    style = "width: 100%; margin-bottom: 10px; background-color: #6CC283; border-color: #4F7942;color: #ffffff;"
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  4,
                  div(
                    align = "center",
                    style = "margin-top:-15px",
                    width = 12,
                    do.call(
                      "actionButton",
                      c(
                        run_bttn,
                        list(
                          inputId = "synonymsRun"
                        )
                      )
                    )
                  )
                ),
                column(
                  4,
                  div(
                    align = "center",
                    style = "margin-top:-15px",
                    width = 12,
                    title = t_back,
                    do.call(
                      "actionButton",
                      c(
                        back_bttn,
                        list(
                          inputId = "synonymsBack"
                        )
                      )
                    )
                  )
                ),
                column(
                  4,
                  div(
                    title = t_save,
                    div(
                      align = "center",
                      do.call(
                        "actionButton",
                        c(
                          save_bttn,
                          list(
                            inputId = "synonymsSave"
                          )
                        )
                      )
                    )
                  )
                )
              ),
              hr(),
              div(
                helpText(
                  "Pressing Run will replace synonyms and update their PoS tags"
                ),
                style = "text-align: center"
              )
            ),
            style = "margin-top:40px"
          )
        )
      )
    )
  )

  ## Multi-Word Creation -----

  mwcreation <- tabItem(
    tabName = "multiwordCreat",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(strong("Multi-Word Creation"), align = "center")
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          7,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Multi-Word List",
              shinycssloaders::withSpinner(
                DT::DTOutput("multiwordList"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Annotated Text including Multi-Word",
              shinycssloaders::withSpinner(
                DT::DTOutput("multiwordData"),
                color = getOption("spinner.color", default = "#4F7942")
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
                    HTML(infoTexts$multiwordcreation)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          5,
          div(
            box(
              width = 12,
              div(
                h3(strong(em("Automatic Multi-Words"))),
                style = "margin-top:-57px"
              ),
              helpText(h5(
                "Multi-word creation extracts keywords (sequence of terms) from the text."
              )),
              helpText(h5(
                "After keywords are generated, select those you wish to include in your data from the list."
              )),
              hr(),
              style = "text-align: left; text-color: #989898",
              fluidRow(
                column(
                  12,
                  selectInput(
                    "MWmethod",
                    "Relevant Collocation Algorithm",
                    choices = c(
                      "Rake" = "rake",
                      "Pointwise Mutual Information" = "pmi",
                      "Mutual Dependency" = "md",
                      "Log-Frequency Biased Mutual Dependency" = "lfmd",
                      "Absorption Index (IS)" = "is"
                    ),
                    selected = "rake"
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  numericInput(
                    inputId = "ngram_max",
                    label = "Ngrams",
                    min = 2,
                    max = 10,
                    value = 4,
                    step = 1
                  )
                ),
                column(
                  6,
                  numericInput(
                    inputId = "freq_minMW",
                    label = "Freq Min",
                    min = 1,
                    max = Inf,
                    value = 3,
                    step = 1
                  )
                )
              ),
              conditionalPanel(
                'input.MWmethod != "is"',
                fluidRow(
                  column(
                    12,
                    h5(em(strong("Multi-Words created by:")))
                  ),
                )
              ),
              conditionalPanel(
                'input.MWmethod == "is"',
                fluidRow(
                  column(
                    12,
                    h5(em(strong("Lexical Words to use:")))
                  ),
                )
              ),
              fluidRow(
                column(
                  12,
                  div(
                    class = "multicol",
                    uiOutput("multiwordPosSel")
                  )
                )
              ),
              hr(),
              div(
                fluidRow(
                  column(
                    4,
                    div(
                      align = "center",
                      style = "margin-top:-15px",
                      width = 12,
                      do.call(
                        "actionButton",
                        c(
                          run_bttn,
                          list(
                            inputId = "multiwordCreatRun"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    div(
                      align = "center",
                      style = "margin-top:-15px",
                      width = 12,
                      title = t_back,
                      do.call(
                        "actionButton",
                        c(
                          back_bttn,
                          list(
                            inputId = "multiwordCreatBack"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    div(
                      title = t_save,
                      div(
                        align = "center",
                        do.call(
                          "actionButton",
                          c(
                            save_bttn,
                            list(
                              inputId = "multiwordCreatSave"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                style = "margin-top:-15px"
              ),
              hr(),
              # prova pulsante apply multiword
              uiOutput("multiwordCreatApplyUI")
            ),
            style = "margin-top:40px; width:100%;"
          )
        )
      )
    )
  )

  ## Multi-word By a List ----
  mwlist <- tabItem(
    tabName = "multiwordByList",
    fluidPage(
      fluidRow(
        column(
          12,
          h3(strong("Multi-Word Creation by a List"), align = "center")
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          9,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Multi-Word List",
              shinycssloaders::withSpinner(
                DT::DTOutput("multiwordList2"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Annotated Text including Multi-Word",
              shinycssloaders::withSpinner(
                DT::DTOutput("multiwordData2"),
                color = getOption("spinner.color", default = "#4F7942")
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
                    HTML(infoTexts$multiwordlist)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          3,
          div(
            box(
              width = 12,
              div(
                h3(strong(em("Import a Multi-Word List"))),
                style = "margin-top:-57px"
              ),
              helpText(h5(
                "Please ensure that the Multi-Word List is formatted as an Excel/CSV file with one column.
                                       Each cell of that column include a multi-word. Each term have to be separated by a single whitespace."
              )),
              hr(),
              style = "text-align: left; text-color: #989898",
              fluidRow(column(
                12,
                fileInput(
                  "multiword_lists",
                  label = NULL,
                  multiple = TRUE,
                  accept = c(
                    ".csv",
                    ".xls",
                    ".xlsx"
                  )
                )
              )),
              hr(),
              div(
                fluidRow(
                  column(
                    4,
                    div(
                      align = "center",
                      style = "margin-top:-15px",
                      width = 12,
                      do.call(
                        "actionButton",
                        c(
                          run_bttn,
                          list(
                            inputId = "multiwordListRun"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    div(
                      align = "center",
                      style = "margin-top:-15px",
                      width = 12,
                      title = t_back,
                      do.call(
                        "actionButton",
                        c(
                          back_bttn,
                          list(
                            inputId = "multiwordListBack"
                          )
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    div(
                      title = t_save,
                      div(
                        align = "center",
                        do.call(
                          "actionButton",
                          c(
                            save_bttn,
                            list(
                              inputId = "multiwordListSave"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                style = "margin-top:-15px"
              )
            ),
            style = "margin-top:40px; width:100%;"
          )
        )
      )
    )
  )

  ## PoS Tag Selection -----
  postagselection <- tabItem(
    tabName = "posTagSelect",
    fluidPage(
      fluidRow(
        column(
          8,
          h3(strong("PoS Tag Selection"), align = "center"),
          br(),
          br(),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Annotated Text",
              shinycssloaders::withSpinner(
                DT::DTOutput("posTagSelectData"),
                color = getOption("spinner.color", default = "#4F7942")
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
                    HTML(infoTexts$posselection)
                  ),
                  column(1)
                )
              )
            )
          )
        ),
        column(
          4,
          div(
            box(
              width = 12,
              style = "padding-top: 0px; margin-top: 0px;", # Azzera completamente

              fluidRow(
                style = "margin-top: 0px;", # Azzera anche la fluidRow
                column(
                  12,
                  style = "padding-top: 0px;", # Azzera anche la column

                  uiOutput("posStatsUI"),

                  # PoS Tags UI
                  div(
                    uiOutput("posTagListsUI")
                  ),

                  # Filters Box (Hapax e Single Character)
                  div(
                    style = "background: white; border: 2px solid #95a5a6; border-radius: 8px; padding: 15px; margin-top: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); width: 100%;",

                    # Header
                    div(
                      style = "display: flex; align-items: center; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 2px solid #95a5a6;",
                      div(
                        style = "width: 35px; height: 35px; border-radius: 50%; background: #95a5a6; display: flex; align-items: center; justify-content: center; margin-right: 10px; flex-shrink: 0;",
                        icon(
                          "filter",
                          style = "color: white; font-size: 16px;"
                        )
                      ),
                      div(
                        strong(
                          "Additional Filters",
                          style = "color: #2c3e50; font-size: 15px;"
                        )
                      )
                    ),

                    # Checkboxes affiancati con column-count
                    div(
                      style = "column-count: 2; column-gap: 20px;",
                      checkboxGroupInput(
                        "posTagHapax",
                        label = NULL,
                        choices = c("Hapax"),
                        selected = "Hapax"
                      ),
                      checkboxGroupInput(
                        "posTagSingleChar",
                        label = NULL,
                        choices = c("Single Character"),
                        selected = NULL
                      )
                    )
                  )
                )
              ),
              div(
                hr(),
                div(
                  fluidRow(
                    column(
                      6,
                      div(
                        align = "center",
                        style = "margin-top:-15px",
                        width = 12,
                        do.call(
                          "actionButton",
                          c(
                            run_bttn,
                            list(
                              inputId = "posTagSelectRun"
                            )
                          )
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        title = t_save,
                        div(
                          align = "center",
                          do.call(
                            "actionButton",
                            c(
                              save_bttn,
                              list(
                                inputId = "posTagSelectSave"
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  style = "margin-top:-1px"
                ),
                style = "margin-top:-1px"
              )
            ),
            style = "margin-top:0px"
          )
        )
      )
    )
  )

  return(list(
    tokpos = tokpos,
    specialentities = specialentities,
    custompos = custompos,
    synonyms = synonyms,
    mwcreation = mwcreation,
    mwlist = mwlist,
    postagselection = postagselection
  ))
}

preprocessingServer <- function(input, output, session, values, statsValues) {
  ### PRE-PROCESSING ----

  ## Tokenization & PoS Tagging ----

  output$flagUI <- renderUI({
    tags$img(
      src = values$flag,
      height = "25px",
      width = "40px",
      style = "margin-top:-30px;"
    )
  })

  output$optionsTokenization <- renderUI({
    selectInput(
      inputId = "language_model",
      label = "Language",
      choices = values$label_lang,
      multiple = FALSE,
      width = "100%",
      selected = values$language
    )
  })

  output$treebankSelect <- renderUI({
    selected_treebanks <- values$languages$treebank[
      values$languages$language_name == values$language
    ]
    selectInput(
      "treebank",
      "Treebank",
      choices = selected_treebanks,
      selected = values$treebank
    )
  })

  output$info_treebank <- renderUI({
    ud_description <- values$languages %>%
      filter(language_name == values$language, treebank == values$treebank) %>%
      select(description) %>%
      as.character()
    ud_info <- values$languages %>%
      filter(language_name == values$language, treebank == values$treebank) %>%
      select(tokens, words, sentences)
    ud_info <- paste0(
      "Tokens: ",
      format(as.numeric(ud_info$tokens), big.mark = ",", scientific = FALSE),
      " - Words: ",
      format(as.numeric(ud_info$words), big.mark = ",", scientific = FALSE),
      " - Sentences: ",
      format(as.numeric(ud_info$sentences), big.mark = ",", scientific = FALSE)
    )
    accuracy <- values$accuracy %>%
      filter(language_name == values$language, treebank == values$treebank) %>%
      select(Words, Lemma, Sentences, UPOS)
    ud_accuracy1 <- paste0(
      "Words: ",
      accuracy$Words,
      "% ---  Lemma: ",
      accuracy$Lemma,
      "%"
    )
    ud_accuracy2 <- paste0(
      "Sentences: ",
      accuracy$Sentences,
      "%  ---  PoS:   ",
      accuracy$UPOS,
      "%"
    )
    ud_contributors <- values$languages %>%
      filter(language_name == values$language, treebank == values$treebank) %>%
      select(contributors) %>%
      as.character()
    ud_hub_page_link <- values$languages %>%
      filter(language_name == values$language, treebank == values$treebank) %>%
      select(hub_page_link) %>%
      as.character()
    values$flag <- values$languages %>%
      filter(language_name == values$language, treebank == values$treebank) %>%
      select(flag) %>%
      as.character()
    #  HTML
    tagList(
      tags$div(
        class = "card",
        # style = "border: 1px solid #ddd; border-radius: 5px; padding: 15px; margin: 10px;",
        tags$h4(strong(em("Description")), style = "font-size: 12px;"),
        tags$p(ud_description, style = "font-size: 11px;"),
        tags$h4(strong(em("Treebank Data")), style = "font-size: 12px;"),
        tags$p(ud_info, style = "font-size: 11px;"),
        tags$h4(strong(em("Model Accuracy")), style = "font-size: 12px;"),
        tags$p(ud_accuracy1, style = "font-size: 11px;"),
        tags$p(ud_accuracy2, style = "font-size: 11px;"),
        tags$h4(strong(em("Contributors")), style = "font-size: 12px;"),
        tags$p(ud_contributors, style = "font-size: 10px;"),
        # tags$h4("Hub Page"),
        tags$a(
          href = ud_hub_page_link,
          target = "_blank",
          "UD Treebank link",
          style = "font-size: 10px; color: blue; text-decoration: underline;"
        )
      )
    )
  })

  observeEvent(input$language_model, {
    values$language <- input$language_model
    selected_treebanks <- values$languages$treebank[
      values$languages$language_name == values$language
    ]
    updateSelectInput(
      session,
      "treebank",
      choices = selected_treebanks,
      selected = values$treebank
    )
  })

  observeEvent(input$treebank, {
    values$treebank <- input$treebank
  })

  posTagging <- eventReactive(
    {
      input$tokPosRun
    },
    {
      # values$language <- input$language_model
      # values$treebank <- input$treebank
      values$language_file <- values$languages %>%
        dplyr::filter(
          language_name == values$language,
          treebank == values$treebank
        ) %>%
        select(file) %>%
        as.character()
      ## download and load model language
      udmodel_lang <- loadLanguageModel(file = values$language_file)

      # ## set cores for parallel computing
      ncores <- coresCPU()

      # Lemmatization and POS Tagging
      values$dfTag <- udpipe(
        object = udmodel_lang,
        x = values$txt %>%
          dplyr::filter(doc_selected),
        parallel.cores = ncores
      )

      # Merge metadata from the original txt object
      values$dfTag <- values$dfTag %>%
        left_join(
          values$txt %>% select(-text, -text_original),
          by = "doc_id"
        ) %>%
        filter(!is.na(upos)) %>% ##
        posSel(., c("ADJ", "NOUN", "PROPN", "VERB"))

      if (input$token_lowercase) {
        values$dfTag <- values$dfTag %>%
          mutate(
            token = tolower(token)
          )
      }
      if (input$lemma_lowercase) {
        values$dfTag <- values$dfTag %>%
          mutate(
            lemma = tolower(lemma)
          )
      }

      values$dfTag$docSelected <- TRUE
      values$menu <- 1
      statsValues <- updateStats(
        values$dfTag,
        term = values$generalTerm,
        statsValues
      )
    }
  )

  output$unitAnalysis <- renderUI({
    if (!is.null(values$dfTag)) {
      list(
        # h5(strong("Select Analysis Term: Lemma or Token")),
        # br(),
        radioGroupButtons(
          inputId = "generalTerm",
          label = "Select Analysis Term:",
          choices = c("Lemma" = "lemma", "Token" = "token"),
          selected = values$generalTerm,
          status = "primary",
          justified = TRUE
        )
      )
    }
  })

  ## Term selected on the dashboard
  output$termSelected <- renderText({
    if (!is.null(input$generalTerm)) {
      values$generalTerm <- input$generalTerm
      statsValues <- updateStats(
        values$dfTag,
        term = values$generalTerm,
        statsValues
      )
    }
    if (values$menu >= 1) {
      HTML(paste(
        "Analysis by: <b>",
        tools::toTitleCase(values$generalTerm),
        "</b>"
      ))
    }
  })

  output$tokPosTagData <- DT::renderDT({
    posTagging()

    if (!is.null(values$dfTag)) {
      DTformat(
        values$dfTag %>%
          filter(docSelected) %>%
          select(
            doc_id,
            paragraph_id,
            sentence_id,
            sentence,
            token,
            lemma,
            upos
          ) %>%
          rename(
            D_id = doc_id,
            P_id = paragraph_id,
            S_id = sentence_id,
            Sentence = sentence,
            Token = token,
            Lemma = lemma,
            "Part of Speech" = upos
          )
      )
    }
  })

  observeEvent(
    eventExpr = {
      input$tokPosSave
    },
    handlerExpr = {
      file <- paste("Tall-Export-File-", sys.time(), ".tall", sep = "")
      file_path <- destFolder(file, values$wdTall)
      saveTall(
        values$dfTag,
        values$custom_lists,
        values$language,
        values$treebank,
        values$menu,
        "Custom Term Lists",
        file_path,
        values$generalTerm,
        values$corpus_description
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Tagging Special Entities ----
  posSpecialTagging <- eventReactive(
    {
      input$posSpecialRun
    },
    {
      res <- TaggingCorpusElements(values$dfTag)

      values$dfTag <- res$x %>% filter(!token %in% c("#", "@")) # remove empty hashs and tags
      values$posSpecialData <- res$resList %>% filter(!item %in% c("#", "@"))
      statsValues <- updateStats(
        values$dfTag,
        term = values$generalTerm,
        statsValues
      )
      rm(res)

      # Store summary data for cards
      values$posSpecialSummary <- values$posSpecialData %>%
        summarySpecialEntities(type = "all")
    },
    ignoreNULL = TRUE
  )

  ## Render HTML Cards for Special Entities (COMPACT VERSION) ----
  output$posSpecialCardsUI <- renderUI({
    posSpecialTagging()

    # Get summary data
    summary_data <- values$posSpecialSummary

    # Define entity metadata with colors, icons, and labels
    entity_metadata <- list(
      EMAIL = list(
        color = "#3498db",
        icon = "envelope",
        label = "Email Addresses",
        example = "example@domain.com"
      ),
      EMOJI = list(
        color = "#f39c12",
        icon = "smile",
        label = "Emojis",
        example = "😊 🚀 ❤️"
      ),
      HASH = list(
        color = "#9b59b6",
        icon = "hashtag",
        label = "Hashtags",
        example = "#ExampleTag"
      ),
      IP_ADDRESS = list(
        color = "#e74c3c",
        icon = "network-wired",
        label = "IP Addresses",
        example = "192.168.1.1"
      ),
      MENTION = list(
        color = "#1abc9c",
        icon = "at",
        label = "Mentions",
        example = "@username"
      ),
      URL = list(
        color = "#34495e",
        icon = "link",
        label = "URLs",
        example = "https://example.com"
      )
    )

    # Create card HTML for each entity type
    cards <- lapply(1:nrow(summary_data), function(i) {
      entity_type <- summary_data$UPOS[i]
      frequency <- summary_data$Frequency[i]
      metadata <- entity_metadata[[entity_type]]

      # Skip if metadata not found
      if (is.null(metadata)) {
        return(NULL)
      }

      # Generate unique button ID for modal
      button_id <- paste0("viewEntity_", entity_type)

      # Create compact card HTML
      div(
        style = sprintf(
          "background: white;
         border-left: 4px solid %s;
         border-radius: 8px;
         padding: 12px 16px;
         margin-bottom: 10px;
         box-shadow: 0 1px 4px rgba(0,0,0,0.08);
         transition: all 0.3s ease;
         cursor: pointer;",
          metadata$color
        ),
        class = "entity-card",

        # Card content with horizontal layout
        fluidRow(
          style = "display: flex; align-items: center; margin: 0;",

          # Icon column (reduced size)
          column(
            2,
            style = "padding: 0; display: flex; align-items: center; justify-content: center;",
            div(
              style = sprintf(
                "width: 45px;
               height: 45px;
               border-radius: 50%%;
               background: linear-gradient(135deg, %s, %s);
               display: flex;
               align-items: center;
               justify-content: center;
               box-shadow: 0 2px 6px rgba(0,0,0,0.12);",
                metadata$color,
                adjustcolor(metadata$color, alpha.f = 0.7)
              ),
              icon(metadata$icon, style = "color: white; font-size: 20px;")
            )
          ),

          # Entity info column
          column(
            6,
            style = "padding: 0 10px; display: flex; flex-direction: column; justify-content: center;",
            div(
              h5(
                strong(metadata$label),
                style = sprintf(
                  "color: %s; margin: 0 0 3px 0; font-size: 15px;",
                  metadata$color
                )
              ),
              p(
                style = "color: #95a5a6; margin: 0; font-size: 11px; line-height: 1.3;",
                sprintf("Example: %s", metadata$example)
              )
            )
          ),

          # Frequency badge and view button column (horizontal layout)
          column(
            4,
            style = "padding: 0; display: flex; align-items: center; justify-content: flex-end; gap: 8px;",

            # Frequency badge
            div(
              style = sprintf(
                "background: %s;
               color: white;
               padding: 6px 14px;
               border-radius: 16px;
               font-weight: bold;
               font-size: 14px;
               min-width: 45px;
               text-align: center;",
                metadata$color
              ),
              format(frequency, big.mark = ",")
            ),

            # View button (only if frequency > 0)
            if (frequency > 0) {
              actionButton(
                inputId = button_id,
                label = NULL,
                icon = icon("eye"),
                style = sprintf(
                  "background: %s;
                 color: white;
                 border: none;
                 padding: 6px 12px;
                 border-radius: 5px;
                 font-size: 12px;
                 cursor: pointer;
                 height: 32px;
                 min-width: 32px;",
                  metadata$color
                ),
                onclick = sprintf(
                  "Shiny.setInputValue('viewEntityModal', '%s', {priority: 'event'})",
                  entity_type
                )
              )
            }
          )
        )
      )
    })

    # Add custom CSS for hover effects
    tagList(
      tags$style(HTML(
        "
      .entity-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(0,0,0,0.15) !important;
      }
    "
      )),
      cards
    )
  })

  ## Modal for Frequency Distribution ----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$viewEntityModal
    },
    handlerExpr = {
      if (!is.null(input$viewEntityModal) && input$viewEntityModal != "") {
        entity_type <- input$viewEntityModal

        # Get entity metadata for modal styling
        entity_metadata <- list(
          EMAIL = list(color = "#3498db", label = "Email", icon = "envelope"),
          EMOJI = list(color = "#f39c12", label = "Emoji", icon = "smile"),
          HASH = list(color = "#9b59b6", label = "Hashtag", icon = "hashtag"),
          IP_ADDRESS = list(
            color = "#e74c3c",
            label = "IP Address",
            icon = "network-wired"
          ),
          MENTION = list(color = "#1abc9c", label = "Mention", icon = "at"),
          URL = list(color = "#34495e", label = "URL", icon = "link")
        )

        metadata <- entity_metadata[[entity_type]]

        # Create and show modal with white title text
        showModal(
          modalDialog(
            title = div(
              style = "color: white; font-weight: bold; font-size: 20px; display: flex; align-items: center; gap: 10px;",
              icon(metadata$icon, style = "font-size: 22px;"),
              sprintf("Frequency Distribution of %s Entities", metadata$label)
            ),
            # Modal content
            div(
              style = "padding: 15px;",
              shinycssloaders::withSpinner(
                htmlOutput("specialEntityFreqHTML"),
                color = metadata$color
              )
            ),
            size = "l",
            easyClose = TRUE,
            footer = tagList(
              actionButton(
                inputId = "closeModalSpecialEntities",
                label = "Close",
                icon = icon("times"),
                style = sprintf(
                  "background: %s; color: white; border: none; padding: 8px 20px; border-radius: 5px;",
                  metadata$color
                )
              )
            )
          )
        )
      }
    }
  )

  ## Close modal observer ----
  observeEvent(input$closeModalSpecialEntities, {
    removeModal()
  })

  ## Render Frequency Distribution in Modal (WITH LARGE EMOJIS) ----
  output$specialEntityFreqHTML <- renderUI({
    req(input$viewEntityModal)

    entity_type <- input$viewEntityModal

    # Get frequency data
    freq_data <- values$posSpecialData %>%
      summarySpecialEntities(type = entity_type) %>%
      rename("Frequency" = "n")

    # Get entity color
    colors <- list(
      EMAIL = "#3498db",
      EMOJI = "#f39c12",
      HASH = "#9b59b6",
      IP_ADDRESS = "#e74c3c",
      MENTION = "#1abc9c",
      URL = "#34495e"
    )
    entity_color <- colors[[entity_type]]

    # Check if we're displaying emojis (for larger font size)
    is_emoji <- (entity_type == "EMOJI")

    # Create HTML table with modern styling
    table_html <- "<div style='max-height: 500px; overflow-y: auto;'>"
    table_html <- paste0(
      table_html,
      "
    <table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>
      <thead>
        <tr style='background: linear-gradient(135deg, ",
      entity_color,
      ", ",
      adjustcolor(entity_color, alpha.f = 0.8),
      "); color: white; position: sticky; top: 0; z-index: 10;'>
          <th style='padding: 12px; text-align: left; font-weight: bold; border-bottom: 2px solid white;'>Item</th>
          <th style='padding: 12px; text-align: center; font-weight: bold; border-bottom: 2px solid white; width: 120px;'>Frequency</th>
        </tr>
      </thead>
      <tbody>
  "
    )

    # Add rows
    for (i in 1:nrow(freq_data)) {
      item <- freq_data$item[i]
      frequency <- freq_data$Frequency[i]

      # Special handling for different entity types
      if (entity_type == "URL") {
        # URLs - make them clickable
        item_display <- sprintf(
          '<a href="%s" target="_blank" style="color: %s; text-decoration: none;">%s</a>',
          item,
          entity_color,
          item
        )
      } else if (is_emoji) {
        # EMOJIS - increase font size for better visibility
        item_display <- sprintf(
          '<span style="font-size: 28px; line-height: 1.5;">%s</span>',
          htmltools::htmlEscape(item)
        )
      } else {
        # Other types - normal display
        item_display <- htmltools::htmlEscape(item)
      }

      # Alternate row colors
      bg_color <- if (i %% 2 == 0) "#f8f9fa" else "white"

      # Adjust padding for emoji rows (more vertical space needed)
      row_padding <- if (is_emoji) "14px" else "10px"

      table_html <- paste0(
        table_html,
        sprintf(
          "
      <tr style='background: %s; transition: background 0.2s;' onmouseover='this.style.background=\"#e8f4f8\"' onmouseout='this.style.background=\"%s\"'>
        <td style='padding: %s; border-bottom: 1px solid #dee2e6; vertical-align: middle;'>%s</td>
        <td style='padding: %s; text-align: center; border-bottom: 1px solid #dee2e6; vertical-align: middle;'>
          <span style='background: %s; color: white; padding: 4px 12px; border-radius: 12px; font-weight: bold; font-size: 13px;'>%s</span>
        </td>
      </tr>
    ",
          bg_color,
          bg_color,
          row_padding,
          item_display,
          row_padding,
          entity_color,
          format(frequency, big.mark = ",")
        )
      )
    }

    table_html <- paste0(
      table_html,
      "
      </tbody>
    </table>
  </div>"
    )

    # Add summary info
    summary_html <- sprintf(
      "
    <div style='margin-top: 15px; padding: 12px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid %s;'>
      <strong style='color: %s;'>Total Items:</strong> %s |
      <strong style='color: %s;'>Total Occurrences:</strong> %s
    </div>
  ",
      entity_color,
      entity_color,
      format(nrow(freq_data), big.mark = ","),
      entity_color,
      format(sum(freq_data$Frequency), big.mark = ",")
    )

    HTML(paste0(table_html, summary_html))
  })

  ## Annotated Text Table (unchanged) ----
  output$posSpecialData <- DT::renderDT({
    posSpecialTagging()

    if (!is.null(values$dfTag)) {
      DTformat(
        values$dfTag %>%
          filter(docSelected) %>%
          select(
            doc_id,
            paragraph_id,
            sentence_id,
            sentence,
            token,
            lemma,
            upos
          ) %>%
          rename(
            D_id = doc_id,
            P_id = paragraph_id,
            S_id = sentence_id,
            Sentence = sentence,
            Token = token,
            Lemma = lemma,
            "Part of Speech" = upos
          ),
        col_to_remove = values$generalTerm
      )
    }
  })

  ## Back button - Reset special entities ----
  observeEvent(input$posSpecialBack, {
    values$dfTag <- resetSpecialEntities(values$dfTag)
    statsValues <- updateStats(
      values$dfTag,
      term = values$generalTerm,
      statsValues
    )

    # Reset summary data
    values$posSpecialSummary <- data.frame(
      UPOS = toupper(c(
        "email",
        "url",
        "hash",
        "emoji",
        "ip_address",
        "mention"
      )),
      Frequency = rep(0, 6)
    )

    popUpGeneric(
      title = "Special Entities Tags Removed",
      type = "waiting",
      color = c("#FFA800"),
      subtitle = paste0(
        "Now all Special Entities Tags have been removed from your documents"
      ),
      btn_labels = "OK"
    )
  })

  ## Save button ----
  observeEvent(
    eventExpr = {
      input$posSpecialSave
    },
    handlerExpr = {
      file <- paste("Tall-Export-File-", sys.time(), ".tall", sep = "")
      file_path <- destFolder(file, values$wdTall)
      saveTall(
        values$dfTag,
        values$custom_lists,
        values$language,
        values$treebank,
        values$menu,
        "POS Tag Selection",
        file_path,
        values$generalTerm,
        values$corpus_description
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  # =============================================================================
  # ADDITIONAL CSS FOR ENHANCED STYLING
  # =============================================================================
  tags$style(HTML(
    "
  /* Smooth transitions for all entity cards */
  .entity-card {
    transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  }

  /* Hover effect for entity cards */
  .entity-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(0,0,0,0.15) !important;
  }

  /* Button hover effects */
  .entity-card button:hover {
    opacity: 0.9;
    transform: scale(1.05);
  }

  /* Modal backdrop styling */
  .modal-backdrop {
    background-color: rgba(0, 0, 0, 0.5);
  }

  /* Modal content styling */
  .modal-content {
    border-radius: 10px;
    box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
  }

  /* Scrollbar styling for modal tables */
  .modal-body ::-webkit-scrollbar {
    width: 8px;
  }

  .modal-body ::-webkit-scrollbar-track {
    background: #f1f1f1;
    border-radius: 10px;
  }

  .modal-body ::-webkit-scrollbar-thumb {
    background: #888;
    border-radius: 10px;
  }

  .modal-body ::-webkit-scrollbar-thumb:hover {
    background: #555;
  }
"
  ))

  ## Custom Term List Merging ----

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$custom_lists
    },
    handlerExpr = {
      file <- input$custom_lists
      req(file$datapath[1])
      custom_lists <- lapply(file$datapath, function(x) {
        x <- read_excel(x) %>% select(c(1, 2))
        names(x) <- c(values$generalTerm, "upos")
        return(x)
      })
      custom_lists <- do.call(rbind, custom_lists)
      values$custom_lists <- custom_lists

      show_alert(
        title = "Custom List",
        text = DTOutput("customListData"),
        type = NULL,
        width = "80%",
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
    }
  )

  proxy1 <- dataTableProxy("posTagSelectData")

  customListMerging <- eventReactive(
    {
      input$custTermListRun
    },
    {
      # req(input$custom_lists)
      values$dfTag <- mergeCustomLists(
        values$dfTag,
        values$custom_lists,
        values$generalTerm
      )
      statsValues <- updateStats(
        values$dfTag,
        term = values$generalTerm,
        statsValues
      )
      # Update the DT proxy

      replaceData(proxy1, values$dfTag, resetPaging = FALSE)
    }
  )

  output$customPosTagData <- DT::renderDT({
    customListMerging()

    if (!is.null(values$dfTag)) {
      DTformat(
        values$dfTag %>%
          select(doc_id, sentence_id, sentence, token, lemma, upos) %>%
          rename(
            D_id = doc_id,
            S_id = sentence_id,
            Sentence = sentence,
            Token = token,
            Lemma = lemma,
            "Part of Speech" = upos
          ),
        col_to_remove = values$generalTerm
      )
    }
  })

  output$customListData <- DT::renderDT(server = FALSE, {
    # customListMerging()

    switch(
      values$generalTerm,
      lemma = {
        if (is.null(values$custom_lists)) {
          DTdf <- DTformat(data.frame(Lemma = NULL, POSTag = NULL))
        } else {
          DTdf <- DTformat(
            values$custom_lists %>%
              rename(
                Lemma = lemma,
                "Part of Speech" = upos
              ),
            col_to_remove = values$generalTerm
          )
        }
      },
      token = {
        if (is.null(values$custom_lists)) {
          DTdf <- DTformat(data.frame(Token = NULL, POSTag = NULL))
        } else {
          DTdf <- DTformat(
            values$custom_lists %>%
              rename(
                Token = token,
                "Part of Speech" = upos
              ),
            col_to_remove = values$generalTerm
          )
        }
      }
    )
    DTdf
  })

  observeEvent(
    eventExpr = {
      input$custTermSave
    },
    handlerExpr = {
      file <- paste("Tall-Export-File-", sys.time(), ".tall", sep = "")
      file_path <- destFolder(file, values$wdTall)
      saveTall(
        values$dfTag,
        values$custom_lists,
        values$language,
        values$treebank,
        values$menu,
        "Custom PoS Lists",
        file_path,
        values$generalTerm,
        values$corpus_description
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## back to the original txt
  observeEvent(input$custTermListBack, {
    values$custom_list <- NULL
    values$dfTag <- customListsReset(values$dfTag)
    statsValues <- updateStats(
      values$dfTag,
      term = values$generalTerm,
      statsValues
    )

    popUpGeneric(
      title = "Custom List Removed",
      type = "waiting",
      color = c("#FFA800"),
      subtitle = paste0(
        "Now all Custom PoS have been remove from your documents"
      ),
      btn_labels = "OK"
    )
    # Update the DT proxy
    replaceData(proxy1, values$dfTag, resetPaging = FALSE)
  })

  ## Synonyms Merging -----

  observeEvent(input$synonymsRun, {
    updateTabItems(session, "sidebarmenu", "synonymsMgmt")
  })

  observeEvent(input$synonymsBack, {
    # Restore previous state if backup exists
    if (!is.null(values$dfTag_backup_synonyms)) {
      values$dfTag <- values$dfTag_backup_synonyms
      show_alert(
        title = "Restored",
        text = "Previous state has been restored",
        type = "info",
        btn_labels = "OK",
        btn_colors = "#6CC283"
      )
    }
  })

  observeEvent(
    eventExpr = {
      input$synonymsSave
    },
    handlerExpr = {
      file <- paste("Tall-Export-File-", sys.time(), ".tall", sep = "")
      file_path <- destFolder(file, values$wdTall)
      saveTall(
        values$dfTag,
        values$custom_lists,
        values$language,
        values$treebank,
        values$menu,
        "Synonyms Merging",
        file_path,
        values$generalTerm,
        values$corpus_description
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  synonyms_data <- reactive({
    req(input$synonyms_file)

    file_path <- input$synonyms_file$datapath
    file_ext <- tools::file_ext(input$synonyms_file$name)

    tryCatch(
      {
        if (file_ext %in% c("xlsx", "xls")) {
          data <- readxl::read_excel(file_path)
        } else if (file_ext == "csv") {
          data <- read.csv(file_path, stringsAsFactors = FALSE)
        } else {
          show_alert(
            title = "Error",
            text = "Unsupported file format. Please use CSV or Excel.",
            type = "error"
          )
          return(NULL)
        }

        # Validate file structure
        if (ncol(data) < 3) {
          show_alert(
            title = "Error",
            text = "File must have at least 3 columns: target_term, upos, and at least one synonym.",
            type = "error"
          )
          return(NULL)
        }

        # Check if second column looks like upos
        valid_upos <- values$dfTag %>% distinct(upos) %>% pull(upos)

        if (!any(toupper(data[[2]]) %in% valid_upos)) {
          show_alert(
            title = "Warning",
            text = paste(
              "Column 2 should contain PoS tags (upos) like NOUN, VERB, ADJ, etc.",
              "Please verify your file structure."
            ),
            type = "warning"
          )
        }

        return(data)
      },
      error = function(e) {
        show_alert(
          title = "Error",
          text = paste("Error reading file:", e$message),
          type = "error"
        )
        return(NULL)
      }
    )
  })

  output$synonymsListPreview <- DT::renderDT({
    req(synonyms_data())

    DTformat(
      synonyms_data(),
      nrow = 20,
      filename = "synonyms_list"
    )
  })

  output$synonymsStats <- renderUI({
    if (is.null(synonyms_data())) {
      return("No file loaded")
    }

    data <- synonyms_data()
    n_targets <- nrow(data)
    n_synonyms <- sum(!is.na(data[, -(1:2)]))

    tagList(
      h5(strong("Statistics"), style = "color: #4F7942;"),
      HTML(paste0(
        "Target terms: ",
        n_targets,
        "<br/>",
        "Total synonyms: ",
        n_synonyms
      ))
    )
  })

  synonymsProcessing <- eventReactive(
    input$synonymsRun,
    {
      req(synonyms_data())

      # Create backup before processing
      values$dfTag_backup_synonyms <- values$dfTag

      show_toast(
        title = "Processing...",
        text = "Applying synonym replacements and updating PoS tags",
        type = "info",
        timer = 2000
      )

      # Apply synonyms replacement
      values$dfTag <- applySynonymsReplacement(
        dfTag = values$dfTag,
        synonyms_df = synonyms_data(),
        term_type = values$generalTerm
      )

      values$menu <- 2
      statsValues <- updateStats(
        values$dfTag,
        term = values$generalTerm,
        statsValues
      )

      show_alert(
        title = "Success",
        text = paste(
          "Synonyms replacement completed!",
          "Modified column:",
          input$synonymsTermType,
          "\\nPoS tags (upos) have been updated for replaced terms."
        ),
        type = "success",
        btn_labels = "OK",
        btn_colors = "#6CC283"
      )

      return(values$dfTag)
    }
  )

  output$synonymsProcessedData <- DT::renderDT({
    synonymsProcessing()

    if (!is.null(values$dfTag)) {
      DTformat(
        values$dfTag %>%
          select(doc_id, sentence_id, sentence, token, lemma, upos) %>%
          rename(
            D_id = doc_id,
            S_id = sentence_id,
            Sentence = sentence,
            Token = token,
            Lemma = lemma,
            PoS = upos
          ),
        nrow = 20,
        filename = "processed_with_synonyms"
      )
    }
  })

  ## Download Template Excel -----
  output$downloadSynonymsTemplate <- downloadHandler(
    filename = function() {
      paste0("synonyms_template_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Create template dataframe with example structure
      template_df <- data.frame(
        target_term = c(
          "machine_learning",
          "artificial_intelligence",
          "statistical_analysis",
          "",
          ""
        ),
        upos = c("NOUN", "NOUN", "NOUN", "", ""),
        synonym1 = c("ml", "ai", "statistics", "", ""),
        synonym2 = c("ML", "AI", "stats", "", ""),
        synonym3 = c("machine learning", "A.I.", "statistical method", "", ""),
        synonym4 = c("", "", "", "", ""),
        stringsAsFactors = FALSE
      )

      # Create instructions dataframe
      instructions_df <- data.frame(
        Instructions = c(
          "HOW TO USE THIS TEMPLATE:",
          "",
          "1. Column 'target_term': Enter the standardized term to use",
          "2. Column 'upos': Enter the Part-of-Speech tag (NOUN, VERB, ADJ, ADV, PROPN, etc.)",
          "3. Columns 'synonym1', 'synonym2', etc.: List all synonyms to replace",
          "",
          "IMPORTANT:",
          "- The upos value will be assigned to all occurrences of the target term",
          "- Matching is case-insensitive",
          "- Empty cells are ignored",
          "- You can add more synonym columns if needed",
          "",
          "EXAMPLE:",
          "target_term: machine_learning | upos: NOUN | synonym1: ml | synonym2: ML",
          "",
          "Valid PoS tags: NOUN, VERB, ADJ, ADV, PRON, DET, ADP, NUM, CONJ, PROPN, etc."
        )
      )

      # Write to Excel file with two sheets
      openxlsx::write.xlsx(
        list(
          Template = template_df,
          Instructions = instructions_df
        ),
        file = file
      )
    }
  )

  ## Download Vocabulary (Frequency Distribution) -----
  output$downloadVocabulary <- downloadHandler(
    filename = function() {
      paste0("vocabulary_", values$generalTerm, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(values$dfTag)

      # Get the term column based on generalTerm (token or lemma)
      term_col <- values$generalTerm

      # Calculate frequency distribution
      vocabulary <- values$dfTag %>%
        group_by(!!sym(term_col), upos) %>%
        summarise(frequency = n(), .groups = "drop") %>%
        arrange(desc(frequency)) %>%
        rename(
          term = !!sym(term_col),
          pos_tag = upos
        )

      # Create summary statistics
      summary_df <- data.frame(
        Metric = c(
          "Total unique terms",
          "Total occurrences",
          "Most frequent term",
          "Most frequent PoS tag",
          "Date generated",
          "Term type"
        ),
        Value = c(
          nrow(vocabulary),
          sum(vocabulary$frequency),
          vocabulary$term[1],
          names(sort(table(vocabulary$pos_tag), decreasing = TRUE))[1],
          as.character(Sys.Date()),
          values$generalTerm
        )
      )

      # Write to Excel file with two sheets
      writexl::write_xlsx(
        list(
          Vocabulary = vocabulary,
          Summary = summary_df
        ),
        path = file
      )
    }
  )

  ## Multi-Word Creation ----

  output$multiwordPosSel <- renderUI({
    if (input$MWmethod != "is") {
      poslist <- posTagAll(
        values$dfTag %>%
          dplyr::filter(
            !upos %in%
              c("MULTIWORD", "NGRAM_MERGED", "PUNCT", "SYM", "X", "NUM")
          )
      )$description

      posSelected <- posTagAll(
        values$dfTag %>% dplyr::filter(upos %in% values$posMwSel)
      )$description
    } else {
      poslist <- posTagAll(
        values$dfTag %>%
          dplyr::filter(
            upos %in%
              c("NOUN", "PROPN", "ADJ", "VERB", "ADV")
          )
      )$description
      posSelected <- posTagAll(
        values$dfTag %>% dplyr::filter(upos %in% values$posMwSel)
      )$description
    }

    checkboxGroupInput(
      "multiwordPosSelGroup",
      label = NULL,
      choices = poslist,
      selected = posSelected
    )
  })

  proxy4 <- dataTableProxy("multiwordData")

  multiword <- eventReactive(
    {
      input$multiwordCreatRun
    },
    {
      ### REKA Algorithm

      values$dfTag <- rakeReset(values$dfTag) ## reset previous multiword creation steps

      values$posMwSel <- gsub(
        ":",
        "",
        gsub(":.*", "", input$multiwordPosSelGroup)
      )

      values$rakeResults <- rake(
        values$dfTag,
        group = "doc_id",
        ngram_max = input$ngram_max,
        relevant = values$posMwSel,
        freq.min = input$freq_minMW,
        term = values$generalTerm,
        method = input$MWmethod
      )

      values$stats <- values$rakeResults$stats

      names(values$stats) <- c(
        "Multi-Words",
        "Freq",
        "Length",
        toupper(input$MWmethod)
      )
    }
  )

  observeEvent(
    ignoreNULL = FALSE,
    eventExpr = {
      input$multiwordList_rows_selected
    },
    handlerExpr = {
      if (length(input$multiwordList_rows_selected) > 0) {
        output$multiwordCreatApplyUI <- renderUI({
          run_bttn <- list(
            label = strong("Apply List"),
            style = "border-radius: 15px; border-width: 1px; font-size: 15px; text-align: center; color: #ffff; ",
            icon = NULL
          )
          div(
            align = "center",
            style = "margin-top:-5px",
            width = 12,
            helpText(
              "Please note",
              br(),
              "pressing 'Apply List' will delete previous multiword and custom list entries"
            ),
            do.call(
              "actionButton",
              c(
                run_bttn,
                list(
                  inputId = "multiwordCreatApply"
                )
              )
            )
          )
        })
      } else {
        output$multiwordCreatApplyUI <- renderUI({})
      }
    }
  )
  output$multiwordList <- renderDT(server = FALSE, {
    multiword()
    DTformat(
      values$stats %>%
        arrange(desc(Freq), .by_group = FALSE),
      numeric = 4,
      selection = TRUE,
      nrow = 20
    )
  })

  output$multiwordData <- renderDT(server = TRUE, {
    DTformat(
      values$dfTag %>%
        dplyr::filter(docSelected) %>%
        select(doc_id, sentence, token, lemma, upos) %>%
        rename(
          D_id = doc_id,
          Sentence = sentence,
          Token = token,
          Lemma = lemma,
          "Part of Speech" = upos
        ),
      size = "80%",
      col_to_remove = values$generalTerm
    )
  })
  observeEvent(input$multiwordCreatApply, {
    row_sel <- input$multiwordList_rows_selected

    if (length(row_sel) > 0) {
      showNotification(
        ui = paste0(
          "Integrating ",
          length(row_sel),
          " Multi-Words in your corpus"
        ),
        type = "warning",
        duration = 3,
        closeButton = TRUE
      )
      values$dfTag <- applyRake(
        values$dfTag,
        rakeResults = values$rakeResults,
        row_sel = row_sel,
        term = values$generalTerm
      )

      statsValues <- updateStats(
        values$dfTag,
        term = values$generalTerm,
        statsValues
      )

      replaceData(proxy4, values$dfTag, resetPaging = FALSE)

      showNotification(
        ui = paste0(
          "Multi-Words Applied: ",
          length(row_sel),
          " multi-words have been added to your documents"
        ),
        type = "message",
        duration = 5,
        closeButton = TRUE
      )

      # show_alert(
      #   title = "Annotated Data with Multi-Words",
      #   paste0("Now Multi-Words have been added to your documents"),
      #   type = NULL,
      #   width = "50%",
      #   closeOnEsc = TRUE,
      #   closeOnClickOutside = TRUE,
      #   html = TRUE,
      #   showConfirmButton = TRUE,
      #   showCancelButton = FALSE,
      #   btn_labels = "OK",
      #   btn_colors = "#6CC283",
      #   timer = NULL,
      #   imageUrl = "",
      #   animation = TRUE
      # )
    }
  })

  observeEvent(
    eventExpr = {
      input$multiwordCreatSave
    },
    handlerExpr = {
      file <- paste("Tall-Export-File-", sys.time(), ".tall", sep = "")
      file_path <- destFolder(file, values$wdTall)
      saveTall(
        values$dfTag,
        values$stats,
        values$language,
        values$treebank,
        values$menu,
        "Multi-Word Creation",
        file_path,
        values$generalTerm,
        values$corpus_description
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## back to the original txt
  observeEvent(input$multiwordCreatBack, {
    values$dfTag <- rakeReset(values$dfTag)
    values$multiwords <- data.frame(
      keyword = "",
      ngram = NA,
      freq = NA,
      rake = NA
    )
    popUpGeneric(
      title = "Multiword Removed",
      type = "waiting",
      color = c("#FFA800"),
      subtitle = paste0(
        "Now all multiwords have been remove from your documents"
      ),
      btn_labels = "OK"
    )
  })

  ## Multi-Word by a list ----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$multiword_lists
    },
    handlerExpr = {
      file <- input$multiword_lists
      req(file$datapath[1])
      keyword_lists <- lapply(file$datapath, function(x) {
        x <- read_excel(x) %>% select(1)
        names(x) <- "keyword"
        return(x)
      })
      keywordList <- do.call(rbind, keyword_lists)
      values$keywordList <- keywordList
    }
  )

  multiword2 <- eventReactive(
    {
      input$multiwordListRun
    },
    {
      req(input$multiword_lists)
      # to replace with input values
      term <- values$generalTerm

      relevant <- unique(values$dfTag$upos)

      values$rakeResults <- rake(
        values$dfTag,
        relevant = relevant,
        term = term,
        freq.min = 1,
        type = "bylist",
        keywordList = values$keywordList
      )

      row_sel <- 1:nrow(values$rakeResults$stats)

      values$dfTag <- applyRake(
        values$dfTag,
        rakeResults = values$rakeResults,
        row_sel = row_sel,
        term = term
      )

      showNotification(
        ui = paste0(
          "Multi-Words Applied: ",
          length(row_sel),
          " multi-words have been added to your documents"
        ),
        type = "message",
        duration = 5,
        closeButton = TRUE
      )

      statsValues <- updateStats(
        values$dfTag,
        term = values$generalTerm,
        statsValues
      )
    }
  )

  output$multiwordData2 <- renderDT({
    multiword2()
    DTformat(
      values$dfTag %>%
        dplyr::filter(POSSelected) %>%
        group_by(doc_id, sentence_id) %>%
        select(doc_id, sentence_id, sentence, token, lemma, upos) %>%
        rename(
          D_id = doc_id,
          S_id = sentence_id,
          Sentence = sentence,
          Token = token,
          Lemma = lemma,
          "Part of Speech" = upos
        ),
      col_to_remove = values$generalTerm
    )
  })

  output$multiwordList2 <- renderDT(server = FALSE, {
    multiword2()
    DTformat(
      values$rakeResults$stats %>%
        rename(
          "Multi-Words" = keyword,
          "Lenght" = ngram,
          "Freq" = freq
        ) %>%
        arrange(desc(Freq), .by_group = FALSE)
    )
  })

  observeEvent(
    eventExpr = {
      input$multiwordListSave
    },
    handlerExpr = {
      file <- paste("Tall-Export-File-", sys.time(), ".tall", sep = "")
      file_path <- destFolder(file, values$wdTall)
      saveTall(
        values$dfTag,
        values$custom_lists,
        values$language,
        values$treebank,
        values$menu,
        "Multi-Word by a List",
        file_path,
        values$generalTerm,
        values$corpus_description
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## back to the original txt
  observeEvent(input$multiwordListBack, {
    values$dfTag <- rakeReset(values$dfTag)
    values$multiwords <- data.frame(keyword = "", ngram = NA, freq = NA)

    popUpGeneric(
      title = "Multiword Removed",
      type = "waiting",
      color = c("#FFA800"),
      subtitle = paste0(
        "Now all multiwords have been remove from your documents"
      ),
      btn_labels = "OK"
    )
  })

  ## PoS Tag Selection ----
  observe({
    output$posTagListsUI <- renderUI({
      # Ottieni tutti i PoS tags
      all_pos <- posTagAll(values$dfTag)
      selected_pos <- (posTagAll(
        values$dfTag %>% dplyr::filter(POSSelected)
      ))$description

      # Identifica dinamicamente le categorie
      classic_available <- all_pos$description[
        !grepl("Special Entity$|Custom PoS$", all_pos$description)
      ]

      special_available <- all_pos$description[
        grepl("Special Entity$", all_pos$description)
      ]

      custom_available <- all_pos$description[
        grepl("Custom PoS$", all_pos$description)
      ]

      # Funzione per creare etichette pulite
      clean_labels <- function(items, pattern) {
        labels <- gsub(pattern, "", items)
        labels <- trimws(labels)
        return(list(
          names = unname(as.list(labels)),
          values = unname(as.list(items))
        ))
      }

      special_clean <- if (length(special_available) > 0) {
        clean_labels(special_available, ": Special Entity$")
      } else {
        NULL
      }

      custom_clean <- if (length(custom_available) > 0) {
        clean_labels(custom_available, ": Custom PoS$")
      } else {
        NULL
      }

      tagList(
        # Classic PoS Card
        if (length(classic_available) > 0) {
          div(
            style = "background: white; border: 2px solid #3498db; border-radius: 8px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); width: 100%;",

            # Header
            div(
              style = "display: flex; align-items: center; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 2px solid #3498db;",
              div(
                style = "width: 35px; height: 35px; border-radius: 50%; background: #3498db; display: flex; align-items: center; justify-content: center; margin-right: 10px; flex-shrink: 0;",
                icon("tag", style = "color: white; font-size: 16px;")
              ),
              div(
                strong(
                  "Classic PoS Tags",
                  style = "color: #2c3e50; font-size: 15px;"
                ),
                div(
                  sprintf("%d tags", length(classic_available)),
                  style = "color: #7f8c8d; font-size: 11px; margin-top: 2px;"
                )
              )
            ),

            # Checkboxes in due colonne
            div(
              style = "column-count: 2; column-gap: 20px;",
              checkboxGroupInput(
                "posTagLists_classic",
                label = NULL,
                choices = classic_available,
                selected = intersect(selected_pos, classic_available)
              )
            )
          )
        },

        # Special Entities Card
        if (length(special_available) > 0) {
          div(
            style = "background: white; border: 2px solid #f39c12; border-radius: 8px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); width: 100%;",

            # Header
            div(
              style = "display: flex; align-items: center; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 2px solid #f39c12;",
              div(
                style = "width: 35px; height: 35px; border-radius: 50%; background: #f39c12; display: flex; align-items: center; justify-content: center; margin-right: 10px; flex-shrink: 0;",
                icon("star", style = "color: white; font-size: 16px;")
              ),
              div(
                strong(
                  "Special Entities",
                  style = "color: #2c3e50; font-size: 15px;"
                ),
                div(
                  sprintf("%d entities", length(special_available)),
                  style = "color: #7f8c8d; font-size: 11px; margin-top: 2px;"
                )
              )
            ),

            # Checkboxes con etichette pulite
            div(
              style = "column-count: 2; column-gap: 20px;",
              checkboxGroupInput(
                "posTagLists_special",
                label = NULL,
                choiceNames = special_clean$names,
                choiceValues = special_clean$values,
                selected = intersect(selected_pos, special_available)
              )
            )
          )
        },

        # Custom PoS Card
        if (length(custom_available) > 0) {
          div(
            style = "background: white; border: 2px solid #1abc9c; border-radius: 8px; padding: 15px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); width: 100%;",

            # Header
            div(
              style = "display: flex; align-items: center; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 2px solid #1abc9c;",
              div(
                style = "width: 35px; height: 35px; border-radius: 50%; background: #1abc9c; display: flex; align-items: center; justify-content: center; margin-right: 10px; flex-shrink: 0;",
                icon("wrench", style = "color: white; font-size: 16px;")
              ),
              div(
                strong(
                  "Custom PoS",
                  style = "color: #2c3e50; font-size: 15px;"
                ),
                div(
                  sprintf("%d custom tags", length(custom_available)),
                  style = "color: #7f8c8d; font-size: 11px; margin-top: 2px;"
                )
              )
            ),

            # Checkboxes con etichette pulite
            div(
              style = "column-count: 2; column-gap: 20px;",
              checkboxGroupInput(
                "posTagLists_custom",
                label = NULL,
                choiceNames = custom_clean$names,
                choiceValues = custom_clean$values,
                selected = intersect(selected_pos, custom_available)
              )
            )
          )
        }
      )
    })
  })

  # Renderizza la box delle statistiche
  output$posStatsUI <- renderUI({
    # Inizializza con i valori totali se non ancora aggiornato
    n_docs <- if (statsValues$n_docs == 0) {
      length(unique(values$df$D_id))
    } else {
      statsValues$n_docs
    }

    n_tokens <- if (statsValues$n_tokens == 0) {
      nrow(values$df)
    } else {
      statsValues$n_tokens
    }

    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 8px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);",

      # Titolo
      div(
        style = "color: white; font-size: 14px; font-weight: 600; margin-bottom: 15px; text-align: center; opacity: 0.95;",
        icon("chart-bar", style = "margin-right: 8px;"),
        "Selection Statistics"
      ),

      # Statistiche in griglia
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",

        # Documenti
        div(
          style = "background: rgba(255,255,255,0.15); border-radius: 6px; padding: 12px; text-align: center;",
          div(
            style = "color: rgba(255,255,255,0.9); font-size: 11px; margin-bottom: 5px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Documents"
          ),
          div(
            style = "color: white; font-size: 24px; font-weight: bold;",
            format(n_docs, big.mark = ",")
          )
        ),

        # Token/Lemma
        div(
          style = "background: rgba(255,255,255,0.15); border-radius: 6px; padding: 12px; text-align: center;",
          div(
            style = "color: rgba(255,255,255,0.9); font-size: 11px; margin-bottom: 5px; text-transform: uppercase; letter-spacing: 0.5px;",
            ifelse(values$generalTerm == "token", "Tokens", "Lemma")
          ),
          div(
            style = "color: white; font-size: 24px; font-weight: bold;",
            format(n_tokens, big.mark = ",")
          )
        )
      ),

      # Timestamp ultimo aggiornamento (opzionale)
      if (!is.null(statsValues$last_update)) {
        div(
          style = "margin-top: 12px; padding-top: 12px; border-top: 1px solid rgba(255,255,255,0.2); text-align: center; color: rgba(255,255,255,0.8); font-size: 10px;",
          icon("sync-alt", style = "margin-right: 5px;"),
          "Updated: ",
          format(statsValues$last_update, "%H:%M:%S")
        )
      }
    )
  })

  PosFilterData <- eventReactive(
    {
      input$posTagSelectRun
    },
    {
      posTagLists_combined <- c(
        input$posTagLists_classic,
        input$posTagLists_special,
        input$posTagLists_custom
      )
      selected <- (posTagAll(values$dfTag) %>%
        dplyr::filter(description %in% (posTagLists_combined)))$pos
      values$dfTag <- removeHapaxFreq(
        values$dfTag,
        input$posTagHapax,
        input$posTagSingleChar
      )
      values$dfTag <- posSel(values$dfTag, pos = selected)
      values$menu <- 2

      statsValues <- updateStats(
        values$dfTag,
        term = values$generalTerm,
        statsValues
      )

      # Update the DT proxy
      proxy <- dataTableProxy("posTagSelectData")
      replaceData(proxy, values$dfTag, resetPaging = FALSE)
    }
  )

  output$posTagSelectData <- DT::renderDT({
    PosFilterData()

    if (!"lemma_original_nomultiwords" %in% names(values$dfTag)) {
      values$dfTag <- values$dfTag %>%
        mutate(lemma_original_nomultiwords = lemma)
    }

    DTformat(
      LemmaSelection(values$dfTag) %>%
        group_by(doc_id, sentence_id) %>%
        mutate(
          SentenceByPos = paste(lemma_original_nomultiwords, collapse = " ")
        ) %>%
        select(
          doc_id,
          sentence_id,
          sentence,
          SentenceByPos,
          token,
          lemma,
          upos
        ) %>%
        rename(
          D_id = doc_id,
          S_id = sentence_id,
          Sentence = sentence,
          Token = token,
          Lemma = lemma,
          "Part of Speech" = upos
        ) %>%
        ungroup(),
      col_to_remove = values$generalTerm
    )
  })

  observeEvent(
    eventExpr = {
      input$posTagSelectSave
    },
    handlerExpr = {
      file <- paste("Tall-Export-File-", sys.time(), ".tall", sep = "")
      file_path <- destFolder(file, values$wdTall)
      saveTall(
        values$dfTag,
        values$custom_lists,
        values$language,
        values$treebank,
        values$menu,
        "POS Tag Selection",
        file_path,
        values$generalTerm,
        values$corpus_description
      )
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )
}

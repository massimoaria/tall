# UI
#  Function source ----
source("libraries.R", local = TRUE)
source("tallFunctions.R", local = TRUE)
source("helpContent.R", local = TRUE)
source("cssTags.R", local = TRUE)
libraries()

htmltools::findDependencies(selectizeInput(
  "dummy",
  label = NULL,
  choices = NULL
))

## Language model list
languages <- langrepo()
label_lang <- unique(languages$language_name)
names(label_lang) <- gsub("_", " ", label_lang)

### input scale choices
choices <- paste0(seq(from = 0, to = 100, by = 1), "%")

## button style and contents

style_bttn <- "border-radius: 20px; border-width: 1px; font-size: 15px; text-align: center; color: #ffff; padding-left: 7px; padding-right: 20px"
style_opt <- "border-radius: 20px; border-width: 1px; font-size: 15px; margin-top: 15px" # (option button)
style_start <- "border-radius: 15px; border-width: 3px; font-size: 15px; width:100% " # (start button)
# style_bttn <- "border-radius: 15px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (action buttons)
t_report <- "Add Results to the Report"
t_export <- "Export Plot as PNG"
t_run <- "Run the Analysis"
t_view <- "View document"
t_save <- "Save the Analysis"
t_back <- "Back to the original text(s)"

run_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name = "play", lib = "glyphicon")
)

view_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = fa_i(name = "magnifying-glass", prefer_type = "solid")
  # icon = icon("search", lib="glyphicon")
)

export_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name = "download-alt", lib = "glyphicon")
)
report_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name = "plus", lib = "glyphicon")
)
save_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 1px;", # margin-top: 15px",
  icon = icon(name = "floppy-save", lib = "glyphicon")
)
back_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name = "repeat", lib = "glyphicon")
)
x_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name = "remove", lib = "glyphicon")
)

## HEADER ----

title_tall <- tags$link(
  tags$a(
    href = "https://github.com/massimoaria/tall",
    target = "_blank",
    tags$img(src = "logo_white.jpg", height = "30", width = "30")
  ),
  strong(" TALL", style = "font-size:17px;")
)

donation <- "https://www.bibliometrix.org/home/index.php/donation"
# tallWeb <- 'https://github.com/massimoaria/tall'
k_synth <- "https://www.k-synth.unina.it"
github_aria <- "https://github.com/massimoaria/tall"

infoTexts <- helpContent()

header <- shinydashboardPlus::dashboardHeader(
  title = title_tall,
  titleWidth = 250,
  controlbarIcon = NULL,
  tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("dataGroupedBy"))))),
  tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("dataFilteredBy"))))),
  tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("termSelected"))))),
  # tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("resetButton"))))),
  dropdownMenu(
    type = "messages",
    icon = icon("comment-dollar", lib = "font-awesome"),
    badgeStatus = NULL,
    headerText = strong("Donate"),
    messageItem2(
      from = "Donation",
      message = "",
      href = donation,
      icon = icon("share-alt", lib = "glyphicon")
    )
  ),
  dropdownMenu(
    type = "messages",
    icon = fa_i(name = "users"),
    badgeStatus = NULL,
    headerText = "",
    shiny::tags$li(strong("Creators")),
    messageItem2(
      from = "Massimo Aria",
      message = "",
      href = "https://www.massimoaria.com",
      icon = fa_i(name = "user-tie")
    ),
    messageItem2(
      from = "Corrado Cuccurullo",
      message = "",
      href = "https://www.corradocuccurullo.com/",
      icon = fa_i(name = "user-tie")
    ),
    messageItemCustom(
      from = "Maria Spano",
      message = "",
      href = "https://scholar.google.com/citations?user=kh_hGT0AAAAJ&hl=it&oi=ao",
      icon = "businesswoman"
    ),
    messageItem2(
      from = "Luca D'Aniello",
      message = "",
      href = "https://scholar.google.com/citations?user=IXJxh0MAAAAJ&hl=it&oi=ao",
      icon = fa_i(name = "user-tie")
    ),
    shiny::tags$li(strong("Contributors")),
    messageItem2(
      from = "Michelangelo Misuraca",
      message = "",
      href = "https://scholar.google.com/citations?user=WdivjAUAAAAJ&hl=it",
      icon = fa_i(name = "user-tie")
    )
  ),
  dropdownMenu(
    type = "messages",
    icon = fa_i(name = "cube"),
    badgeStatus = NULL,
    headerText = strong("Credits"),
    messageItem2(
      from = "K-Synth",
      message = "",
      href = k_synth,
      icon = fa_i(name = "watchman-monitoring")
    ),
    messageItem2(
      from = "Github",
      message = "",
      href = github_aria,
      icon = fa_i(name = "github")
    )
  ),
  # Settings Button - uses actionLink to trigger server-side tab change
  tags$li(
    class = "dropdown",
    actionButton(
      inputId = "go_to_settings",
      label = NULL,
      icon = icon("gear", lib = "font-awesome"),
      style = "background: transparent;
               border: none;
               color: #fff;
               font-size: 20px;  /* Increased from 16px to 20px */
               margin-top: 7px;
               cursor: pointer;",
      title = "Settings"
    )
  ),
  tags$li(
    class = "dropdown",
    tags$style(".main-header .logo {height: 53px}")
  )
)

## SIDEBAR ----
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = "sidebarmenu",
    # shinyjs::useShinyjs(),
    # style = "position: relative; overflow: visible;",
    menuItem(
      "TALL",
      tabName = "tall",
      icon = icon("text-size", lib = "glyphicon")
    ),
    menuItemOutput("rest_of_sidebar")
  )
)

## BODY ----

body <- dashboardBody(
  customTheme(),
  cssTags(), ## CSS tags
  tabItems(
    ### TALL PAGE ----
    tabItem(
      tabName = "tall",
      fluidRow(
        br(),
        div(
          img(src = "tall_logo.jpg", height = "10%", width = "33%"),
          style = "text-align: center;"
        ),
        br(),
        div(p(
          "Powered by ",
          em(a("K-Synth", href = "https://k-synth.com/", target = "_blank")),
          style = "text-align:center; font-size:17px;"
        ))
      )
    ),

    ### IMPORT ----

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
    ),

    ## EDIT ----
    ### Split ----

    tabItem(
      tabName = "split_tx",
      fluidPage(
        fluidRow(
          column(
            12,
            h3(strong("Split Corpus"), align = "center"),
            br(),
          )
        )
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Split Results",
          fluidPage(
            fluidRow(
              column(
                9,
                shinycssloaders::withSpinner(
                  DT::DTOutput("splitTextData"),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                3,
                fluidRow(
                  box(
                    width = 12,
                    div(
                      h3(strong(em("Split texts"))),
                      style = "margin-top:-57px"
                    ),
                    hr(),
                    textInput(
                      inputId = "txSplitWord",
                      label = "Split texts by a sequence of characters (e.g. **H1**)",
                      value = NULL
                    ),
                    hr(),
                    helpText(em(
                      "The minimum sequence of characters required to split the text must consist of at least three characters.",
                      br(),
                      br(),
                      "It's important to note that the text used as a delimiter for splitting is case sensitive (e.g., 'CHAPTER' is different from 'chapter')."
                    )),
                    div(
                      fluidRow(
                        column(
                          4,
                          div(
                            align = "center",
                            title = t_run,
                            do.call(
                              "actionButton",
                              c(
                                run_bttn,
                                list(
                                  inputId = "splitTextRun"
                                )
                              )
                            )
                          )
                        ),
                        column(
                          4,
                          div(
                            align = "center",
                            title = t_back,
                            do.call(
                              "actionButton",
                              c(
                                back_bttn,
                                list(
                                  inputId = "splitTextBack"
                                )
                              )
                            )
                          )
                        ),
                        column(
                          4,
                          div(
                            align = "center",
                            title = t_save,
                            do.call(
                              "actionButton",
                              c(list(
                                label = NULL,
                                style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                icon = icon(
                                  name = "floppy-save",
                                  lib = "glyphicon"
                                ),
                                inputId = "splitTextSave"
                              ))
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
        ),
        tabPanel(
          "Info & References",
          fluidPage(
            fluidRow(
              column(1),
              column(
                10,
                br(),
                HTML(infoTexts$split)
              ),
              column(1)
            )
          )
        )
      )
    ),

    ### Random selection ----

    tabItem(
      tabName = "randomText",
      fluidPage(
        fluidRow(
          column(
            12,
            h3(strong("Random Selection"), align = "center"),
            br(),
          )
        )
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Random Selection",
          fluidPage(
            fluidRow(
              column(
                9,
                shinycssloaders::withSpinner(
                  DT::DTOutput("randomTextData"),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                3,
                fluidRow(
                  box(
                    width = 12,
                    div(
                      h3(strong(em("Random Text Selection"))),
                      style = "margin-top:-57px"
                    ),
                    hr(),
                    uiOutput("randomDescription"),
                    br(),
                    "Extract a random sample of texts to analyze",
                    hr(),
                    div(
                      numericInput(
                        "sampleSize",
                        "Sample Size (%)",
                        value = 30,
                        min = 1,
                        max = 100,
                        step = 1
                      ),
                      style = "margin-top:-9px"
                    ),
                    uiOutput("sampleSizeUI"),
                    fluidRow(
                      column(
                        4,
                        title = t_run,
                        do.call(
                          "actionButton",
                          c(
                            run_bttn,
                            list(
                              inputId = "randomTextRun"
                            )
                          )
                        )
                      ),
                      column(
                        4,
                        title = t_back,
                        do.call(
                          "actionButton",
                          c(
                            back_bttn,
                            list(
                              inputId = "randomTextBack"
                            )
                          )
                        )
                      ),
                      column(
                        4,
                        div(
                          align = "center",
                          title = t_save,
                          do.call(
                            "actionButton",
                            c(list(
                              label = NULL,
                              style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                                          border: 1px; margin-top: 16px;",
                              icon = icon(
                                name = "floppy-save",
                                lib = "glyphicon"
                              ),
                              inputId = "randomTextSave"
                            ))
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
              column(1),
              column(
                10,
                br(),
                HTML(infoTexts$random)
              ),
              column(1)
            )
          )
        )
      )
    ),

    ### EXTERNAL INFORMATION ----

    tabItem(
      tabName = "extInfo",
      fluidPage(
        fluidRow(
          column(
            12,
            h3(strong("External Information"), align = "center"),
            br(),
          )
        )
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Corpus with External Information",
          fluidPage(
            fluidRow(
              column(
                9,
                shinycssloaders::withSpinner(
                  DT::DTOutput("extInfoData"),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                3,
                fluidRow(
                  box(
                    width = 12,
                    div(
                      h3(strong(em("Add from a file"))),
                      style = "margin-top:-57px"
                    ),
                    helpText(h5(
                      "To import external information, please make sure that the file
                           to be uploaded is in Excel format and contains a column labeled
                           'doc_id' to identify documents associated to the text(s) imported."
                    )),
                    helpText(h5(
                      "You can download the list of doc_id associated with the imported text files below."
                    )),
                    fluidRow(
                      column(
                        12,
                        div(
                          align = "center",
                          actionButton(
                            inputId = "doc_idExport",
                            label = strong("Export doc_id list"),
                            icon = NULL,
                            style = "border-radius: 15px; border-width: 1px; font-size: 15px;
                                                                    text-align: center; color: #ffff; "
                          )
                        )
                      )
                    ),
                    hr(),
                    fileInput(
                      inputId = "extInfoFile",
                      label = "Import external information",
                      multiple = FALSE,
                      accept = c(
                        ".xls",
                        ".xlsx"
                      ),
                      placeholder = "No file(s) selected"
                    ),
                    hr(),
                    div(
                      fluidRow(
                        column(
                          4,
                          div(
                            title = t_run,
                            do.call(
                              "actionButton",
                              c(
                                run_bttn,
                                list(
                                  inputId = "extInfoRun"
                                )
                              )
                            ),
                            align = "center"
                          )
                        ),
                        column(
                          4,
                          div(
                            align = "center",
                            title = "Back to imported text(s) ",
                            do.call(
                              "actionButton",
                              c(
                                back_bttn,
                                list(
                                  inputId = "extInfoTextBack"
                                )
                              )
                            )
                          )
                        ),
                        column(
                          4,
                          div(
                            title = t_save,
                            do.call(
                              "actionButton",
                              c(
                                list(
                                  label = NULL,
                                  style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                  icon = icon(
                                    name = "floppy-save",
                                    lib = "glyphicon"
                                  )
                                ),
                                list(
                                  inputId = "extInfoSave"
                                )
                              )
                            ),
                            align = "center"
                          )
                        )
                      ),
                      style = "margin-top: -15px"
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
              column(1),
              column(
                10,
                br(),
                HTML(infoTexts$externalinfo)
              ),
              column(1)
            )
          )
        )
      )
    ),

    ### PRE-PROCESSING ----

    ## Tokenization & PoS Tagging -----

    tabItem(
      tabName = "tokPos",
      fluidPage(
        fluidRow(
          column(
            12,
            h3(strong("Tokenization & PoS Tagging"), align = "center"),
            br(),
          )
        )
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Annotated Text Table",
          fluidRow(
            column(
              8,
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
    ),

    ### POS Special Tagging ----
    tabItem(
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
            tabPanel(
              "Special Entities Overview",
              shinycssloaders::withSpinner(
                DT::DTOutput("posSpecialTags"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Annotated Text Table",
              shinycssloaders::withSpinner(
                DT::DTOutput("posSpecialData"),
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
              helpText(h5("•⁠  ⁠Email addresses: example@domain.com")),
              helpText(h5("•⁠  ⁠URLs: https://www.example.com/path")),
              helpText(h5("•  ⁠Emojis: 😊, 🚀, ❤️")),
              helpText(h5("•⁠  ⁠Hashtags: #ExampleTag")),
              helpText(h5("•⁠  ⁠IP addresses: 192.168.1.1")),
              helpText(h5("•⁠  ⁠Mentions: @username")),
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
    ),

    ## Custom PoS Lists -----

    tabItem(
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
    ),

    ## Multi-Word Creation -----

    tabItem(
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
                        "Log-Frequency Biased Mutual Dependency" = "lfmd"
                      ),
                      selected = "rake"
                    )
                  ),
                  column(
                    6
                    # ,selectInput("term",
                    #             "Terms:",
                    #             choices = c("Tokens" = "token",
                    #                         "Lemma" = "lemma"),
                    #             selected = "lemma")
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
                fluidRow(
                  column(
                    12,
                    h5(em(strong("Multi-Words created by:")))
                  ),
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
    ),

    ## Multi-word By a List ----
    tabItem(
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
                # fluidRow(column(12,
                # selectInput("termMWL",
                #             "Terms:",
                #             choices = c("Tokens" = "token",
                #                         "Lemma" = "lemma"),
                #             selected = "lemma"))
                # ),
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
    ),

    ## PoS Tag Selection -----
    tabItem(
      tabName = "posTagSelect",
      fluidPage(
        fluidRow(
          column(
            12,
            h3(strong("PoS Tag Selection"), align = "center")
          )
        ),
        br(),
        br(),
        fluidRow(
          column(
            8,
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
                div(h3(strong(em("Select:"))), style = "margin-top:-57px"),
                tags$hr(),
                fluidRow(column(
                  12,
                  div(
                    class = "multicol",
                    uiOutput("posTagListsUI")
                  ),
                  # h3(strong(em("Select Hapax:"))),
                  hr(),
                  div(
                    class = "multicol",
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
                )),
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
                    style = "margin-top:-15px"
                  ),
                  style = "margin-top:-15px"
                )
              ),
              style = "margin-top:40px"
            )
          )
        )
      )
    ),
    ### FILTER ----
    tabItem(
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
    ),
    ### GROUPS ----
    tabItem(
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
    ),

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
              column(
                9,
                shinycssloaders::withSpinner(
                  visNetworkOutput(
                    "wordcloudPlot",
                    width = "auto",
                    height = "75vh"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
                # wordcloud2::wordcloud2Output("wordcloudPlot", height = "75vh")
              ),
              column(
                3,
                div(
                  box(
                    width = 12,
                    fluidRow(
                      column(
                        12,
                        div(
                          numericInput(
                            "nWC",
                            label = "Words",
                            value = 100,
                            min = 10,
                            max = 500,
                            step = 1
                          ),
                          numericInput(
                            "labelsizeWC",
                            label = "Text Size",
                            value = 10,
                            min = 1,
                            max = 20,
                            step = 1
                          ),
                          style = "margin-top:-3px"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        div(
                          align = "center",
                          style = "margin-top:15px",
                          width = 12,
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
                      column(
                        6,
                        div(
                          align = "center",
                          style = "margin-top:15px",
                          width = 12,
                          # do.call("actionButton", c(export_bttn, list(
                          #   inputId = "wcSave")
                          # ))
                          do.call(
                            "actionButton",
                            c(
                              export_bttn,
                              list(
                                inputId = "wcSave"
                              )
                            )
                          )
                        )
                      )
                    ),
                    align = "left"
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
                            icon("cog", lib = "glyphicon"),
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
    ),

    ### KEYNESS ----

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
                  numericInput(
                    inputId = "keyness_n",
                    label = "Max Number of Terms:",
                    value = 2000,
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
                          value = 50,
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
              shinycssloaders::withSpinner(
                wordcloud2::wordcloud2Output(
                  outputId = "keyness_wordcloud",
                  height = "85vh", # Increased height to use more vertical space
                  width = "100%" # Changed to 100% to fill horizontal space
                ),
                color = getOption("spinner.color", default = "#4F7942")
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
            )
          )
        )
      )
    ),

    ### WORDS ----

    ## Frequency List----

    ## Words Frequency by PoS----

    ### Part of Speech ----
    tabItem(
      tabName = "w_pos",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Part of Speech Frequency List"), align = "center")
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
                    inputId = "posApply"
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
                    inputId = "posExport"
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
                    inputId = "posReport"
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
                  numericInput(
                    "posN",
                    label = "Number of PoS",
                    value = 20,
                    min = 1,
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
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "posPlot",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(
                DT::DTOutput("posTable", width = 700),
                color = getOption("spinner.color", default = "#4F7942")
              ),
              align = "center"
            )
          )
        )
      )
    ),

    ## Words in Context -----

    tabItem(
      tabName = "wordCont",
      fluidPage(
        fluidRow(
          column(
            12,
            h3(strong("Words in Context"), align = "center")
          )
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
              "Words in Context",
              div(
                style = "height: 550px; overflow-y: scroll; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;",
                shinycssloaders::withSpinner(
                  uiOutput("wordsContHtml"),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              )
            ),
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(
                visNetworkOutput(
                  "wordsContNetwork",
                  width = "auto",
                  height = "75vh"
                ),
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
                    HTML(infoTexts$wordincontext)
                  ),
                  column(1)
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
                      caption = HTML("<br><strong>Thinking...</strong>"),
                      image = "ai_small2.gif",
                      color = "#4F7942"
                    )
                  )
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
                h3(strong(em("Words in Context"))),
                style = "margin-top:-57px"
              ),
              tags$hr(),
              style = "text-align: left; text-color: #989898",
              selectizeInput(
                inputId = "wordsContSearch",
                label = "Search word(s) in text",
                choices = NULL
              ),
              h4("Window Length:"),
              fluidRow(
                column(
                  6,
                  numericInput(
                    inputId = "wordsContBefore",
                    label = "Before",
                    value = 5,
                    min = 1,
                    max = 20
                  )
                ),
                column(
                  6,
                  numericInput(
                    inputId = "wordsContAfter",
                    label = "After",
                    value = 5,
                    min = 1,
                    max = 20
                  )
                )
              ),
              fluidRow(
                column(
                  4,
                  div(
                    align = "center",
                    title = "Apply",
                    do.call(
                      "actionButton",
                      c(list(
                        label = NULL,
                        style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                        icon = icon(name = "play", lib = "glyphicon"),
                        inputId = "wordsContApply"
                      ))
                    )
                  )
                ),
                column(
                  4,
                  div(
                    align = "center",
                    title = "Reset",
                    do.call(
                      "actionButton",
                      c(list(
                        label = NULL,
                        style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                        icon = icon(name = "remove", lib = "glyphicon"),
                        inputId = "wordsContReset"
                      ))
                    )
                  )
                ),
                column(
                  4,
                  div(
                    align = "center",
                    title = "Export raw text(s) in Excel",
                    do.call(
                      "actionButton",
                      c(list(
                        label = NULL,
                        style = "display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                        icon = icon(name = "download-alt", lib = "glyphicon"),
                        inputId = "wordsContSave"
                      ))
                    )
                  )
                )
              )
            ),
            style = "margin-top:40px"
          )
        )
      )
    ),

    ### Clustering ----
    tabItem(
      tabName = "w_clustering",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Clustering"), align = "center")
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
                    inputId = "w_clusteringApply"
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
                    inputId = "w_clusteringExport"
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
                    inputId = "w_clusteringReport"
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
                    "w_clusteringSimilarity",
                    label = "Words Similarity by:",
                    choices = c(
                      "None" = "none",
                      "Association Index" = "association",
                      "Cosine Similarity" = "cosine",
                      "Jaccard Index" = "jaccard"
                    ),
                    selected = "association"
                  ),
                  selectInput(
                    "w_clusteringMode",
                    label = "Cluster selection:",
                    choices = c(
                      "Auto" = "auto",
                      "Manual" = "manual"
                    ),
                    selected = "auto"
                  ),
                  conditionalPanel(
                    condition = "input.w_clusteringMode == 'manual'",
                    numericInput(
                      "w_nclusters",
                      label = "N. of Clusters",
                      value = 1,
                      min = 1,
                      step = 1
                    )
                  )
                ),

                # Parameters Section
                tags$details(
                  class = "params-section",
                  tags$summary(
                    div(
                      class = "params-section-header",
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      div(
                        icon("list", lib = "glyphicon"),
                        " Parameters"
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
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "w_clusteringNMax",
                          label = "Words",
                          value = 50,
                          min = 2,
                          step = 1
                        )
                      ),
                      column(
                        6,
                        numericInput(
                          "w_clusteringLabelSize",
                          label = "Label Size",
                          value = 4,
                          min = 1,
                          step = 0.5
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
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Dendrogram",
              shinycssloaders::withSpinner(
                visNetworkOutput(
                  "w_clusteringPlot",
                  width = "auto",
                  height = "75vh"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_clusteringTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            )
          )
        )
      )
    ),

    ### Reinert Clustering ----
    tabItem(
      tabName = "w_reinclustering",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Reinert Clustering"), align = "center")
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
                    inputId = "w_reinclusteringApply"
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
                    inputId = "w_reinclusteringExport"
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
                    inputId = "w_reinclusteringReport"
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
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        "w_rein_k",
                        label = "Max N. of Clusters",
                        value = 10,
                        min = 1,
                        step = 1
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        "w_rein_min_split_members",
                        label = "Min. Segments per Cluster",
                        value = 10,
                        min = 1,
                        step = 1
                      )
                    )
                  )
                ),

                # Segment Parameters Section
                tags$details(
                  class = "params-section",
                  tags$summary(
                    div(
                      class = "params-section-header",
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      div(
                        icon("scissors", lib = "glyphicon"),
                        " Segment Parameters"
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
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "w_rein_segments_size",
                          label = "Segment Length",
                          value = 40,
                          min = 3,
                          step = 1
                        )
                      ),
                      column(
                        6,
                        numericInput(
                          "w_rein_min_segments",
                          label = "Min. Segment Length",
                          value = 5,
                          min = 3,
                          step = 1
                        )
                      )
                    )
                  )
                ),

                # Feature Selection Section
                tags$details(
                  class = "advanced-section",
                  tags$summary(
                    div(
                      class = "advanced-section-header",
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      div(
                        icon("ok-sign", lib = "glyphicon"),
                        " Feature Selection Parameters"
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
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "w_rein_cc_test",
                          label = "Contingency Coefficient Value",
                          value = 0.3,
                          min = 0.1,
                          step = 0.1,
                          max = 0.8
                        )
                      ),
                      column(
                        6,
                        numericInput(
                          "w_rein_tsj",
                          label = "Min freq",
                          value = 3,
                          min = 1,
                          step = 1
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
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Dendrogram",
              br(),
              uiOutput("ReinCutree"),
              shinycssloaders::withSpinner(
                visNetworkOutput(
                  "w_ReinClusteringPlot",
                  width = "auto",
                  height = "75vh"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Summary",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_ReinSummaryTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Terms by Cluster",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_ReinClusteringTableTerms"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Segments by Cluster",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_ReinClusteringTableSegments"),
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
                    HTML(infoTexts$reinert)
                  ),
                  column(1)
                )
              )
            )
          )
        )
      )
    ),

    ### Correspondence Analysis ----
    tabItem(
      tabName = "ca",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Correspondence Analysis"), align = "center")
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
                    inputId = "caApply"
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
                    inputId = "caExport"
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
                    inputId = "caReport"
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
                    inputId = "groupCA",
                    label = "Unit of Analysis",
                    choices = c(
                      "Groups",
                      "Documents",
                      "Paragraphs",
                      "Sentences"
                    ),
                    selected = "Documents"
                  ),
                  numericInput(
                    "nCA",
                    label = "Words",
                    value = 50,
                    min = 2,
                    step = 1
                  )
                ),

                # Clustering Parameters Section
                tags$details(
                  class = "params-section",
                  tags$summary(
                    div(
                      class = "params-section-header",
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      div(
                        icon("stats", lib = "glyphicon"),
                        " Clustering Parameters"
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
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "nClustersCA",
                          label = "Clusters",
                          value = 1,
                          min = 1,
                          step = 1
                        )
                      ),
                      column(
                        6,
                        numericInput(
                          "nDimsCA",
                          label = "Dims for Clustering",
                          value = 2,
                          min = 1,
                          max = 10,
                          step = 1
                        )
                      )
                    )
                  )
                ),

                # Graphical Options Section
                tags$details(
                  class = "advanced-section",
                  tags$summary(
                    div(
                      class = "advanced-section-header",
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      div(
                        icon("picture", lib = "glyphicon"),
                        " Graphical Options"
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
                    selectInput(
                      "dimPlotCA",
                      "Select plane to plot:",
                      choices = c(
                        "1° Factorial Plane" = "1",
                        "2° Factorial Plane" = "2",
                        "3° Factorial Plane" = "3",
                        "4° Factorial Plane" = "4",
                        "5° Factorial Plane" = "5"
                      ),
                      selected = "1"
                    ),
                    numericInput(
                      "nDocCA",
                      label = "Top Contributing Docs/Groups",
                      value = 0,
                      min = 0,
                      step = 1
                    ),
                    numericInput(
                      "lim.contribCA",
                      label = "Max Contribution",
                      value = 2,
                      min = 0.01,
                      max = 10,
                      step = 0.01
                    ),
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "labelsizeCA",
                          label = "Label Size",
                          value = 16,
                          min = 2,
                          step = 1
                        )
                      ),
                      column(
                        6,
                        numericInput(
                          "sizeCA",
                          label = "Min. Dot Size",
                          value = 2,
                          min = 0,
                          max = 20,
                          step = 1
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
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Factorial Plane",
              shinycssloaders::withSpinner(
                plotlyOutput(
                  outputId = "caPlot",
                  height = "75vh",
                  width = "98.9%"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Dendrogram",
              shinycssloaders::withSpinner(
                visNetworkOutput(
                  "caDendrogram",
                  width = "auto",
                  height = "75vh"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Singular Values",
              shinycssloaders::withSpinner(
                DT::DTOutput("caSingularValueTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Coordinates",
              shinycssloaders::withSpinner(
                DT::DTOutput("caCoordTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Contributions",
              shinycssloaders::withSpinner(
                DT::DTOutput("caContribTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Cosines Squared",
              shinycssloaders::withSpinner(
                DT::DTOutput("caCosineTable"),
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
                    HTML(infoTexts$correspondenceanalysis)
                  ),
                  column(1)
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
                      htmlOutput("caGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
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

    ### Network ----

    ## Co-word analysis ----

    tabItem(
      tabName = "w_networkCooc",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Co-word analysis"), align = "center")
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
                    inputId = "w_networkCoocApply"
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
                    inputId = "w_networkCoocExport"
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
                    inputId = "w_networkCoocReport"
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
                    inputId = "w_groupNet",
                    label = "Co-occurrences in",
                    choices = c(
                      "Groups",
                      "Documents",
                      "Paragraphs",
                      "Sentences"
                    ),
                    selected = "Sentences"
                  ),
                  selectInput(
                    "normalizationCooc",
                    label = "Normalization by:",
                    choices = c(
                      "None" = "none",
                      "Association Index" = "association",
                      "Cosine Similarity" = "cosine",
                      "Jaccard Index" = "jaccard"
                    ),
                    selected = "association"
                  )
                ),

                # Network Options Section
                tags$details(
                  class = "advanced-section",
                  tags$summary(
                    div(
                      class = "advanced-section-header",
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      div(
                        icon("link", lib = "glyphicon"),
                        " Network Options"
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
                    materialSwitch(
                      inputId = "noOverlap",
                      label = "Avoid label overlap",
                      value = TRUE,
                      status = "success"
                    ),
                    materialSwitch(
                      inputId = "interLinks",
                      label = "Inter-group links",
                      value = TRUE,
                      status = "success"
                    ),
                    materialSwitch(
                      inputId = "removeIsolated",
                      label = "Delete isolated nodes",
                      value = FALSE,
                      status = "success"
                    )
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
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "nMax",
                          label = "Words",
                          value = 100,
                          min = 2,
                          step = 1
                        ),
                        numericInput(
                          "labelSize",
                          label = "Label Size",
                          value = 4,
                          min = 1,
                          step = 0.5
                        )
                      ),
                      column(
                        6,
                        selectInput(
                          "minEdges",
                          label = "Top Link (%)",
                          choices = c("Auto", paste0(seq(10, 100, 10), "%")),
                          selected = "Auto"
                        ),
                        numericInput(
                          "opacity",
                          label = "Opacity",
                          value = 0.6,
                          min = 0,
                          max = 1,
                          step = 0.1
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
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(
                visNetworkOutput(
                  "w_networkCoocPlot",
                  width = "auto",
                  height = "75vh"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Words",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_networkCoocNodesTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Links",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_networkCoocEdgesTable"),
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
                    HTML(infoTexts$cowordanalysis)
                  ),
                  column(1)
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
                      htmlOutput("w_networkGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
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

    ## Thematic Map ----

    tabItem(
      tabName = "w_networkTM",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Thematic Map"), align = "center")
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
                    inputId = "w_networkTMApply"
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
                    inputId = "w_networkTMExport"
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
                    inputId = "w_networkTMReport"
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
                    inputId = "w_groupTM",
                    label = "Co-occurrences in",
                    choices = c(
                      "Groups",
                      "Documents",
                      "Paragraphs",
                      "Sentences"
                    ),
                    selected = "Sentences"
                  ),
                  materialSwitch(
                    inputId = "noOverlapTM",
                    label = "Avoid label overlap",
                    value = TRUE,
                    status = "success"
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
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "nMaxTM",
                          label = "Words",
                          value = 250,
                          min = 2,
                          step = 1
                        ),
                        numericInput(
                          "labelSizeTM",
                          label = "Label Size",
                          value = 4,
                          min = 1,
                          step = 0.5
                        )
                      ),
                      column(
                        6,
                        numericInput(
                          "n.labelsTM",
                          label = "Labels",
                          value = 3,
                          min = 0,
                          step = 1
                        ),
                        numericInput(
                          "opacityTM",
                          label = "Opacity",
                          value = 0.6,
                          min = 0,
                          max = 1,
                          step = 0.1
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
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Thematic Map",
              shinycssloaders::withSpinner(
                plotlyOutput(
                  "w_networkTMMapPlot",
                  width = "auto",
                  height = "75vh"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(
                visNetworkOutput(
                  "w_networkTMNetPlot",
                  width = "auto",
                  height = "75vh"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Clusters",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_networkTMClusterTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Words",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_networkTMWordTable"),
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
                    HTML(infoTexts$thematicmap)
                  ),
                  column(1)
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
                      htmlOutput("w_networkTMGeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
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

    ## WORD EMBEDDINGS TRAINING----

    tabItem(
      tabName = "w_word2vec",
      fluidPage(
        fluidRow(
          column(
            9,
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Embedding Distributions",
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    "w_word2vecBoxplot",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              tabPanel(
                "Embedding Dimensions",
                shinycssloaders::withSpinner(
                  DT::DTOutput("w_word2vecTable"),
                  color = getOption("spinner.color", default = "#4F7942")
                ),
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    "w_word2vecPCA",
                    width = "auto",
                    height = "50vh"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              # ,tabPanel("Links",
              #          shinycssloaders::withSpinner(DT::DTOutput("w_networkGrakoEdgesTable"),
              #                                       color = getOption("spinner.color", default = "#4F7942"))
              # )
              tabPanel(
                "Info & References",
                fluidPage(
                  fluidRow(
                    column(1),
                    column(
                      10,
                      br(),
                      HTML(infoTexts$embeddingtrain)
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
                  h3(strong(em("Model Training"))),
                  style = "margin-top:-57px"
                ),
                tags$hr(),
                style = "text-align: left; text-color: #989898",
                h4("Options:"),
                selectizeInput(
                  inputId = "w2vMethod",
                  label = "Embedding Method",
                  choices = c(
                    "Continuous Bag of Words" = "cbow",
                    "Skip-Gram" = "skip-gram"
                  ),
                  selected = "cbow"
                ),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      inputId = "w2vDim",
                      label = "Dimensions",
                      value = 100,
                      min = 10,
                      step = 1
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = "w2vIter",
                      label = "Iterations",
                      value = 20,
                      min = 5,
                      max = 100
                    )
                  )
                ),
                fluidRow(
                  column(
                    4,
                    align = "center",
                    do.call(
                      "actionButton",
                      c(
                        run_bttn,
                        list(
                          inputId = "w2vApply"
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    align = "center",
                    do.call(
                      "actionButton",
                      c(
                        export_bttn,
                        list(
                          inputId = "w2vSave"
                        )
                      )
                    )
                  ),
                  column(
                    4,
                    align = "center",
                    do.call(
                      "actionButton",
                      c(
                        report_bttn,
                        list(
                          inputId = "w2vReport"
                        )
                      )
                    )
                  )
                )
              ),
              style = "margin-top:40px"
            )
          )
        )
      )
    ),

    ## WORD EMBEDDING SIMILARITY ----
    tabItem(
      tabName = "w_w2v_similarity",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Embedding Similarity"), align = "center")
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
                    inputId = "w_w2v_similarityApply"
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
                    inputId = "w_w2v_similarityExport"
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
                    inputId = "w_w2v_similarityReport"
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
                  numericInput(
                    "w_w2v_similarityN",
                    label = "Top Words",
                    min = 1,
                    value = 100,
                    step = 1
                  ),
                  sliderInput(
                    "w_w2v_font_size",
                    "Font Size",
                    min = 8,
                    max = 30,
                    value = 14,
                    step = 1
                  ),
                  selectInput(
                    "w_w2v_overlap",
                    "Avoid Label Overlap",
                    choices = c(
                      "No" = "none",
                      "Hide" = "hide",
                      "Transparency" = "transparency"
                    ),
                    selected = "transparency"
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
        )
      ),
      fluidRow(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Similarity Network",
            fluidRow(
              column(11, align = "right", uiOutput("w_w2v_Selected")),
              column(
                1,
                title = "Back to the full network",
                do.call(
                  "actionButton",
                  c(
                    x_bttn,
                    list(
                      inputId = "w_w2v_Back"
                    )
                  )
                )
              )
            ),
            shinycssloaders::withSpinner(
              visNetworkOutput(
                "w_w2vNetworkplot",
                width = "auto",
                height = "75vh"
              ),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          tabPanel(
            "UMAP",
            shinycssloaders::withSpinner(
              plotlyOutput("w_w2vUMAPplot", width = "auto", height = "75vh"),
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
                  HTML(infoTexts$embeddingsimilarity)
                ),
                column(1)
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
                    htmlOutput("w_w2vGeminiUI"),
                    caption = HTML("<br><strong>Thinking...</strong>"),
                    image = "ai_small2.gif",
                    color = "#4F7942"
                  )
                )
              )
            )
          )
        )
      )
    ),

    ## GRAKO ----

    tabItem(
      tabName = "w_networkGrako",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Grako"), align = "center")
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
                    inputId = "w_networkGrakoApply"
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
                    inputId = "w_networkGrakoExport"
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
                    inputId = "w_networkGrakoReport"
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
                    "grakoNormalization",
                    label = "Normalization by:",
                    choices = c(
                      "None" = "none",
                      "Association Index" = "association",
                      "Cosine Similarity" = "cosine",
                      "Jaccard Index" = "jaccard"
                    ),
                    selected = "association"
                  ),
                  materialSwitch(
                    inputId = "grakoUnigram",
                    label = "Include Single words",
                    value = TRUE,
                    status = "success"
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
                    fluidRow(
                      column(
                        6,
                        numericInput(
                          "grakoNMax",
                          label = "Links",
                          value = 30,
                          min = 2,
                          step = 1
                        ),
                        numericInput(
                          "grakoMinEdges",
                          label = "Top Link (%)",
                          value = 100,
                          min = 0,
                          max = 100,
                          step = 1
                        )
                      ),
                      column(
                        6,
                        numericInput(
                          "grakoLabelSize",
                          label = "Label Size",
                          value = 4,
                          min = 0.0,
                          step = 0.5
                        ),
                        numericInput(
                          "grakoOpacity",
                          label = "Opacity",
                          value = 0.6,
                          min = 0,
                          max = 1,
                          step = 0.1
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
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        fluidRow(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Network",
              shinycssloaders::withSpinner(
                visNetworkOutput(
                  "w_networkGrakoPlot",
                  width = "auto",
                  height = "75vh"
                ),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Words",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_networkGrakoNodesTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            ),
            tabPanel(
              "Links",
              shinycssloaders::withSpinner(
                DT::DTOutput("w_networkGrakoEdgesTable"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            )
          )
        )
      )
    ),

    ### DOCUMENTS ----

    ### Topic Modeling ----
    ### K choice ----

    tabItem(
      tabName = "d_tm_select",
      fluidPage(
        fluidRow(
          column(
            9,
            h3(
              strong("Topic Modeling: Optimal selection of topic number"),
              align = "center"
            )
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
                    inputId = "d_tm_selectApply"
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
                    inputId = "d_tm_selectExport"
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
                    inputId = "d_tm_selectReport"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            9,
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Tuning Plot",
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_tm_selectPlot",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              tabPanel(
                "Table",
                shinycssloaders::withSpinner(
                  DT::DTOutput("d_tm_selectTable"),
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
                      HTML(infoTexts$tmkchoice)
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
                  h3(strong(em("Find optimal K"))),
                  style = "margin-top:-57px"
                ),
                tags$hr(),
                style = "text-align: left; text-color: #989898",
                selectInput(
                  inputId = "groupTm",
                  label = "Topics",
                  choices = c(
                    "Groups" = "Groups",
                    "Docs" = "doc_id",
                    "Sentences" = "sentence_id"
                  ),
                  selected = "doc_id"
                ),
                selectInput(
                  "metric",
                  "Metric for model tuning",
                  choices = c(
                    "CaoJuan-2009" = "CaoJuan2009",
                    "Deveaud-2014" = "Deveaud2014",
                    "Arun-2010" = "Arun2010",
                    "Perplexity" = "Perplexity"
                  ),
                  selected = "CaoJuan2009"
                ),
                fluidRow(
                  column(6),
                  column(
                    6,
                    numericInput(
                      "nTm",
                      label = "N. of terms",
                      value = 100,
                      min = 1,
                      step = 1
                    )
                  )
                ),
                selectInput(
                  "top_by",
                  "Terms selection by:",
                  choices = c(
                    "Frequency" = "freq",
                    "TF-IDF" = "tfidf"
                  ),
                  selected = "freq"
                ),
                fluidRow(
                  column(
                    4,
                    numericInput(
                      "minK",
                      label = "K min",
                      value = 2,
                      min = 2,
                      step = 1
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "maxK",
                      label = "K max",
                      value = 20,
                      min = 3,
                      step = 1
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "Kby",
                      label = "K by:",
                      value = 1,
                      min = 1,
                      step = 1
                    )
                  )
                )
              ),
              style = "margin-top:40px"
            )
          )
        )
      )
    ),

    ### Model estimation ----

    tabItem(
      tabName = "d_tm_estim",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Topic Modeling: Model estimation"), align = "center")
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
                    inputId = "d_tm_estimApply"
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
                    inputId = "d_tm_estimExport"
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
                    inputId = "d_tm_estimReport"
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
                  materialSwitch(
                    inputId = "tmKauto",
                    label = "Automatic Topic Selection",
                    value = TRUE,
                    status = "success"
                  ),
                  conditionalPanel(
                    "!input.tmKauto",
                    numericInput(
                      "KEstim",
                      label = "N. of Topics (K)",
                      value = 2,
                      min = 2,
                      step = 1
                    )
                  ),
                  selectInput(
                    inputId = "groupTmEstim",
                    label = "Topics",
                    choices = c(
                      "Groups" = "Groups",
                      "Docs" = "doc_id",
                      "Sentences" = "sentence_id"
                    ),
                    selected = "doc_id"
                  )
                ),

                # Terms Selection Section
                tags$details(
                  class = "filter-section",
                  tags$summary(
                    div(
                      class = "filter-section-header",
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      div(
                        icon("filter", lib = "glyphicon"),
                        " Terms Selection"
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
                    numericInput(
                      "nTmEstim",
                      label = "N. of terms",
                      value = 100,
                      min = 1,
                      step = 1
                    ),
                    selectInput(
                      "top_byEstim",
                      "Terms selection by:",
                      choices = c(
                        "Frequency" = "freq",
                        "TF-IDF" = "tfidf"
                      ),
                      selected = "freq"
                    )
                  )
                ),

                # Display Options Section
                tags$details(
                  class = "params-section",
                  tags$summary(
                    div(
                      class = "params-section-header",
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      div(
                        icon("eye-open", lib = "glyphicon"),
                        " Display Options"
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
                    numericInput(
                      "nTopicPlot",
                      label = "Word/Docs per Topic",
                      value = 10,
                      min = 2,
                      step = 1
                    )
                  )
                ),

                style = "gradient",
                right = TRUE,
                animate = TRUE,
                circle = TRUE,
                tooltip = tooltipOptions(title = "Options"),
                icon = icon("sliders", lib = "font-awesome"),
                width = "320px"
              )
            ),
            style = style_opt
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Topic by Words Plot",
            fluidRow(
              column(
                4,
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_tm_estimTPlot1",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                4,
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_tm_estimTPlot2",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                4,
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_tm_estimTPlot3",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              )
            ),
            fluidRow(
              actionButton("TMplotLeft", icon("menu-left", lib = "glyphicon")),
              actionButton(
                "TMplotRight",
                icon("menu-right", lib = "glyphicon")
              ),
              align = "center"
            )
          ),
          tabPanel(
            "Topic by Docs Plot",
            fluidRow(
              column(
                4,
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_tm_DocPlot1",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                4,
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_tm_DocPlot2",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              column(
                4,
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    outputId = "d_tm_DocPlot3",
                    height = "75vh",
                    width = "98.9%"
                  ),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              )
            ),
            fluidRow(
              actionButton("TMdocLeft", icon("menu-left", lib = "glyphicon")),
              actionButton("TMdocRight", icon("menu-right", lib = "glyphicon")),
              align = "center"
            )
          ),
          tabPanel(
            "Beta Probability",
            shinycssloaders::withSpinner(
              DT::DTOutput("d_tm_estimBpTable"),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          tabPanel(
            "Theta Probability",
            shinycssloaders::withSpinner(
              DT::DTOutput("d_tm_estimTpTable"),
              color = getOption("spinner.color", default = "#4F7942")
            )
          ),
          tabPanel(
            "Topic Correlation",
            shinycssloaders::withSpinner(
              plotlyOutput(
                outputId = "d_tm_networkPlot",
                height = "75vh",
                width = "98.9%"
              ),
              # visNetworkOutput("d_tm_networkPlot", width="auto", height = "75vh"),
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
                  HTML(infoTexts$tmmodelestimation)
                ),
                column(1)
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
                    htmlOutput("d_tm_GeminiUI"),
                    caption = HTML("<br><strong>Thinking...</strong>"),
                    image = "ai_small2.gif",
                    color = "#4F7942"
                  )
                )
              )
            )
          )
        )
      )
    ),

    ### Polarity detection ----

    tabItem(
      tabName = "d_polDet",
      fluidPage(
        fluidRow(
          column(
            8,
            h3(strong("Polarity Detection"), align = "center")
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
                    inputId = "d_polDetApply"
                  )
                )
              )
            )
          ),
          div(
            # style=style_bttn,
            title = t_export,
            column(
              1,
              do.call(
                "actionButton",
                c(
                  export_bttn,
                  list(
                    inputId = "d_polDetExport"
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
                    inputId = "d_polDetReport"
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
                    inputId = "groupPolarity",
                    label = "Polarity of",
                    choices = c(
                      "Groups" = "Groups",
                      "Docs" = "doc_id"
                    ),
                    selected = "doc_id"
                  ),
                  uiOutput("lexiconD_polarity")
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
              "Document Polarity Distribution",
              fluidRow(
                column(
                  6,
                  shinycssloaders::withSpinner(
                    plotlyOutput(
                      outputId = "d_polPiePlot",
                      height = "75vh",
                      width = "98.9%"
                    ),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                ),
                column(
                  6,
                  shinycssloaders::withSpinner(
                    plotlyOutput(
                      outputId = "d_polDensPlot",
                      height = "37vh",
                      width = "98.9%"
                    ),
                    color = getOption("spinner.color", default = "#4F7942")
                  ), # ),
                  shinycssloaders::withSpinner(
                    plotlyOutput(
                      outputId = "d_polBoxPlot",
                      height = "37vh",
                      width = "98.9%"
                    ),
                    color = getOption("spinner.color", default = "#4F7942")
                  ) # )
                )
              )
            ),
            tabPanel(
              "Top Words",
              fluidRow(
                column(
                  6,
                  align = "center",
                  h4("Top Positive Words by Document Polarity Distribution"),
                  shinycssloaders::withSpinner(
                    plotlyOutput(
                      outputId = "d_polDetPlotPos",
                      height = "75vh",
                      width = "98.9%"
                    ),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                ),
                column(
                  6,
                  align = "center",
                  h4("Top Negative Words by Document Polarity Distribution"),
                  shinycssloaders::withSpinner(
                    plotlyOutput(
                      outputId = "d_polDetPlotNeg",
                      height = "75vh",
                      width = "98.9%"
                    ),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                )
              )
            ),
            tabPanel(
              "Table",
              shinycssloaders::withSpinner(
                DT::DTOutput("d_polDetTable"),
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
                    HTML(infoTexts$polaritydetection)
                  ),
                  column(1)
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
                      htmlOutput("d_polDet_GeminiUI"),
                      caption = HTML("<br><strong>Thinking...</strong>"),
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

    ### Abstractive Summarization ----

    tabItem(
      tabName = "d_astractive",
      fluidPage(
        fluidRow(
          column(
            11,
            h3(strong("Abstractive Summarization"), align = "center")
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
                    inputId = "d_astractiveReport"
                  )
                )
              )
            )
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
                "Abstract",
                br(),
                shinycssloaders::withSpinner(
                  uiOutput("summaryData"),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              tabPanel(
                "Full Document",
                br(),
                shinycssloaders::withSpinner(
                  uiOutput("documentData2"),
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
                      HTML(infoTexts$summarization)
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
                  h3(strong(em("Abstractive Summarization"))),
                  style = "margin-top:-57px"
                ),
                h5(
                  ("TALL generates a coherent and concise summary by interpreting and paraphrasing the main ideas from the original text"),
                  style = "text-align: left; text-color: #989898"
                ),
                tags$hr(),
                fluidRow(column(
                  12,
                  uiOutput("optionsUnitAbstractive"),
                  uiOutput("optionsAbstractive"),
                  numericInput(
                    "summaryLength",
                    label = "Summary Length (in words)",
                    value = 250,
                    min = 10,
                    step = 10,
                    max = 16384
                  ),
                  uiOutput("abstractivePromptUI"),
                )),
                hr(),
                div(
                  fluidRow(
                    column(
                      4,
                      div(
                        align = "center",
                        title = t_run,
                        do.call(
                          "actionButton",
                          c(
                            run_bttn,
                            list(
                              inputId = "d_abstractiveApply"
                            )
                          )
                        )
                      )
                    ),
                    column(
                      4,
                      div(
                        align = "center",
                        title = t_view,
                        do.call(
                          "actionButton",
                          c(
                            view_bttn,
                            list(
                              inputId = "d_abstractiveView"
                            )
                          )
                        )
                      )
                    ),
                    column(4)
                  ),
                  style = "margin-top:-15px"
                )
              ),
              style = "margin-top:40px"
            )
          )
        )
      )
    ),

    ### Extractive Summarization ----

    tabItem(
      tabName = "d_summarization",
      fluidPage(
        fluidRow(
          column(
            11,
            h3(strong("Extractive Summarization"), align = "center")
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
                    inputId = "d_summarizationReport"
                  )
                )
              )
            )
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
                "Abstract",
                br(),
                fluidRow(
                  #style = "height:65vh",
                  shinycssloaders::withSpinner(
                    uiOutput("abstractData"),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                ),
                fluidRow(
                  br(),
                  align = "center",
                  shinycssloaders::withSpinner(
                    uiOutput("sliderAbstractData"),
                    color = getOption("spinner.color", default = "#4F7942")
                  )
                )
              ),
              tabPanel(
                "Full Document",
                br(),
                shinycssloaders::withSpinner(
                  # DT::DTOutput("documentData"),
                  uiOutput("documentData"),
                  color = getOption("spinner.color", default = "#4F7942")
                )
              ),
              tabPanel(
                "Sentence Ranking",
                shinycssloaders::withSpinner(
                  DT::DTOutput("RelSentData"),
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
                      HTML(infoTexts$summarization)
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
                  h3(strong(em("Extractive Summarization"))),
                  style = "margin-top:-57px"
                ),
                h5(
                  ("TALL selects and reorders the most relevant sentences from the original text to generate a coherent and concise summary"),
                  style = "text-align: left; text-color: #989898"
                ),
                tags$hr(),
                fluidRow(column(
                  12,
                  uiOutput("optionsUnitSummarization"),
                  uiOutput("optionsSummarization")
                )),
                hr(),
                div(
                  fluidRow(
                    column(
                      4,
                      div(
                        align = "center",
                        title = t_run,
                        do.call(
                          "actionButton",
                          c(
                            run_bttn,
                            list(
                              inputId = "d_summarizationApply"
                            )
                          )
                        )
                      )
                    ),
                    column(
                      4,
                      div(
                        align = "center",
                        title = t_view,
                        do.call(
                          "actionButton",
                          c(
                            view_bttn,
                            list(
                              inputId = "d_summarizationView"
                            )
                          )
                        )
                      )
                    ),
                    column(4)
                  ),
                  style = "margin-top:-15px"
                )
              ),
              style = "margin-top:40px"
            )
          )
        )
      )
    ),

    ### REPORT ----
    tabItem(
      tabName = "report",
      fluidPage(
        fluidRow(
          h3(strong("Report"), align = "center"),
          br(),
        ),
        fluidRow(
          column(
            6,
            offset = 1,
            box(
              title = strong(
                "Select results to include in the Report",
                style = "font-size:20px;color:white;"
              ),
              status = "primary",
              width = 11,
              solidHeader = TRUE,
              tags$style(HTML(
                "
                         .box.box-solid.box-primary>.box-header {
                         background:#4F7942;
                         }
                         .box.box-solid.box-primary{
                         border-bottom-color:black;
                         border-left-color:black;
                         border-right-color:black;
                         border-top-color:black;
                         border-width:2px;
                                         }"
              )),
              uiOutput("reportSheetsUI"),
              tags$style("#reportSheets {font-size:18px;}")
            )
          ), # column(1),
          column(
            2,
            div(
              style = "border-radius: 10px; border-width: 3px; font-size: 10px;",
              align = "center",
              actionBttn(
                inputId = "allSheets",
                label = strong("Select All"),
                icon = icon("ok-circle", lib = "glyphicon"),
                style = "pill",
                color = "primary",
                block = TRUE
              ),
              br(),
              actionBttn(
                inputId = "noSheets",
                label = strong("Deselect All"),
                icon = icon("remove-circle", lib = "glyphicon"),
                style = "pill",
                color = "primary",
                block = TRUE
              ),
              br(),
              hr(),
              actionBttn(
                inputId = "report.save",
                label = strong("Export Report"),
                style = "pill",
                color = "success",
                size = "md",
                block = TRUE,
                no_outline = TRUE,
                icon = icon(name = "download-alt", lib = "glyphicon")
              ),
              br(),
              hr(),
              actionBttn(
                inputId = "deleteAll",
                label = strong("Delete Report"),
                icon = icon("exclamation-sign", lib = "glyphicon"),
                style = "pill",
                color = "danger",
                block = TRUE
              )
            )
          )
        )
      )
    ),

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
  )
) # END DASHBOARDBODY


## UserInterface ####
ui <- dashboardPage(header, sidebar, body)

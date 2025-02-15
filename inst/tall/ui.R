# UI
#  Function source ----
source("libraries.R", local=TRUE)
source("tallFunctions.R", local=TRUE)
source("helpContent.R", local=TRUE)
libraries()

## Language model list
languages <- langrepo()
label_lang <- unique(languages$language_name)
names(label_lang) <- gsub("_"," ",label_lang)

### input scale choices
choices <- paste0(seq(from = 0, to = 100,by = 1),"%")

## button style and contents

style_bttn <- "border-radius: 20px; border-width: 1px; font-size: 15px; text-align: center; color: #ffff; padding-left: 7px; padding-right: 20px"
style_opt <-  "border-radius: 20px; border-width: 1px; font-size: 15px; margin-top: 15px" # (option button)
style_start <-  "border-radius: 15px; border-width: 3px; font-size: 15px; width:100% " # (start button)
#style_bttn <- "border-radius: 15px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (action buttons)
t_report  <-  "Add Results to the Report"
t_export  <-  "Export Plot as PNG"
t_run <- "Run the Analysis"
t_view <- "View document"
t_save <- "Save the Analysis"
t_back <- "Back to the original text(s)"

run_bttn <- list(
  label = NULL,
  style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name ="play", lib="glyphicon")
)

view_bttn <- list(
  label = NULL,
  style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = fa_i(name = "magnifying-glass", prefer_type="solid")
  #icon = icon("search", lib="glyphicon")
)

export_bttn <- list(
  label=NULL,
  style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name ="download-alt", lib="glyphicon")
)
report_bttn <- list(
  label = NULL,
  style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name ="plus", lib="glyphicon")
)
save_bttn <- list(
  label=NULL,
  style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 1px;",# margin-top: 15px",
  icon = icon(name ="floppy-save", lib="glyphicon")
)
back_bttn <- list(
  label = NULL,
  style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name ="repeat", lib="glyphicon")
)


## HEADER ----

title_tall <- tags$link(tags$a(href = 'https://github.com/massimoaria/tall',target="_blank",
                               tags$img(src="logo_white.jpg", height = '30',width='30')
), strong(" TALL",style="font-size:17px;"))

donation <- 'https://www.bibliometrix.org/home/index.php/donation'
#tallWeb <- 'https://github.com/massimoaria/tall'
k_synth <- 'https://www.k-synth.unina.it'
github_aria <- 'https://github.com/massimoaria/tall'

infoTexts <- helpContent()

header <- shinydashboardPlus::dashboardHeader(title = title_tall,
                                              titleWidth = 250, controlbarIcon = NULL,
                                              tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("dataGroupedBy"))))),
                                              tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("dataFilteredBy"))))),
                                              #tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("resetButton"))))),
                                              dropdownMenu(type = "messages",
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
                                                icon = fa_i(name="users"),
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
                                                  from ="Maria Spano",
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
                                                icon = fa_i(name="cube"),
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
                                              tags$li(class = "dropdown",
                                                      tags$style(".main-header .logo {height: 53px}"))
)

## SIDEBAR ----
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(id="sidebarmenu",
              #shinyjs::useShinyjs(),
              #style = "position: relative; overflow: visible;",
              menuItem("TALL", tabName = "tall", icon = icon("text-size", lib = "glyphicon")),
              menuItemOutput ("rest_of_sidebar")
  )
)

## BODY ----

body <- dashboardBody(
  customTheme(),
  ## workaround to solve visualization issues in Data Table
  tags$head(tags$style(HTML( '.has-feedback .form-control { padding-right: 0px;}' ))),
  ## script to open more times the same modal ####
  tags$script("
    Shiny.addCustomMessageHandler('button_id', function(value) {
    Shiny.setInputValue('button_id', value);
    });
  "),
  tags$script("
    Shiny.addCustomMessageHandler('button_id2', function(value) {
    Shiny.setInputValue('button_id2', value);
    });
  "),
  tags$script("
    Shiny.addCustomMessageHandler('click', function(value) {
    Shiny.setInputValue('click', value);
    });
  "),
  tags$script("
    Shiny.addCustomMessageHandler('click_dend', function(value) {
    Shiny.setInputValue('click_dend', value);
    });
  "),
  #### BUTTON STYLE ###############
  tags$style(".glyphicon-refresh {color:#ffffff; font-size: 15px; align: center;}"),
  tags$style(".fa-magnifying-glass {color:#ffffff; font-size: 15px; align: center;}"),
  tags$style(".glyphicon-download-alt {color:#ffffff; font-size: 18px; align: center; margin-left: -3.5px}"),
  tags$style(".glyphicon-play {color:#ffffff; font-size: 18px; align: center;margin-left: -0.5px}"),
  tags$style(".glyphicon-remove {color:#ffffff; font-size: 18px; align: center;margin-left: -0.5px}"),
  tags$style(".glyphicon-search {color:#ffffff; font-size: 18px; align: center;margin-left: -0.5px}"),
  tags$style(".glyphicon-repeat {color:#ffffff; font-size: 18px; align: center;margin-left: -3px; padding-left: -15px}"),
  tags$style(".glyphicon-plus {color:#ffffff; font-size: 18px;align: center; margin-left: -2px}"),
  tags$style(".glyphicon-cog {color:#4F794290; font-size: 21px; margin-top: 2.3px; margin-left: -3px}"),
  tags$style(".glyphicon-floppy-save {color:#ffffff; font-size: 18px; text-align:center; padding-right: -10px;
             margin-top: 1px;}"),#margin-top: 4px; margin-down: 22px; margin-right: 25px}"),
  tags$style(".glyphicon-download {color:#ffffff; font-size: 18px; align: center;margin-top: 3px}"),

  tags$style(".glyphicon-folder-open {color:#ffffff; font-size: 17px}"),
  tags$head(

    tags$style("mark {background-color: #6CC283;}"), ## Color for highlighted text #5a918a

    tags$style(".fa-envelope {color:#FF0000; font-size: 20px}"),
    tags$style(".fa-envelope-open {font-size: 20px}"),
    tags$style(".fa-cube {font-size: 20px}"),
    tags$style(".fa-question {font-size: 20px}"),
    tags$style(".fa-comment-dollar {font-size: 20px}"),
    tags$style(".fa-bars {font-size: 20px}"),
    tags$style(".sidebar-toggle {font-size: 15px}"),
    tags$style(".fa-users {font-size: 18px}"),
  ),
  ##checkBoxGroup

  tags$head(
    tags$style(HTML("

     .multicol {

       -webkit-column-count: 2; /* Chrome, Safari, Opera */

       -moz-column-count: 2; /* Firefox */

       column-count: 2;

     }

   "))),

  tabItems(

    ### TALL PAGE ----
    tabItem(tabName = "tall",
            fluidRow(
              #h1(HTML("TA<i>ll</i>"), align="center", style = "font-family: 'Times New Roman'; font-size: 70px;"),
              br(),
              div(img(src = "tall_logo.jpg", height = "10%",width = "33%"), style="text-align: center;"),
             # h2(HTML("Text Analysis for A<i>ll</i>"), align="center", style = "font-family: 'Times New Roman';"),
              br(),
              div(p("Powered by ",
                    em(a("K-Synth",
                         href = "https://k-synth.com/", target="_blank")),
                    style="text-align:center; font-size:17px;"))
            )
    ),


    ### IMPORT ----

    tabItem(tabName = "import_tx",
            fluidPage(
              fluidRow(
                tabsetPanel(type="tabs",
                            tabPanel("Corpus",
                                     column(9,
                                            shinycssloaders::withSpinner(DT::DTOutput("dataImported"),color = getOption("spinner.color", default = "#4F7942"))
                                     ),
                                     column(3,
                                            fluidRow(
                                              box(
                                                width = 12,
                                                div(h3(strong(em("Import texts"))), style="margin-top:-57px"),
                                                hr(),
                                                uiOutput("runButton"),
                                                conditionalPanel(condition="input.runImport > 0",
                                                                 tags$hr(),
                                                                 div(
                                                                   fluidRow(
                                                                     column(6,
                                                                            div(align="center",
                                                                                title = "Export raw text(s) in Excel",
                                                                                do.call("actionButton", c(list(
                                                                                  label=NULL,
                                                                                  style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                                                                  icon = icon(name ="download-alt", lib="glyphicon"),
                                                                                  inputId = "collection.save")
                                                                                )
                                                                                )
                                                                            )
                                                                     ),
                                                                     column(6,
                                                                            div(align="center",
                                                                                title = "Back to imported text(s) ",
                                                                                do.call("actionButton", c(back_bttn, list(
                                                                                  inputId = "importTextBack")
                                                                                )
                                                                                )
                                                                            )
                                                                     )
                                                                   )
                                                                   ,style="margin-top: -15px"
                                                                 )
                                                )
                                              )
                                            )
                                     )),
                            tabPanel("Info & References",
                                     fluidPage(
                                       fluidRow(
                                         column(1),
                                         column(10,
                                                br(),
                                                HTML(infoTexts$importmenu)
                                         ),
                                         column(1)
                                       )
                                     ))

                )
              )
            )
    ),

    ## EDIT ----
    ### Split ----

    tabItem(tabName = "split_tx",
            fluidPage(
              fluidRow(
                column(9,
                       shinycssloaders::withSpinner(DT::DTOutput("splitTextData"),color = getOption("spinner.color", default = "#4F7942"))
                ),
                column(3,
                       fluidRow(
                         box(
                           width = 12,
                           div(h3(strong(em("Split texts"))), style="margin-top:-57px"),
                           hr(),
                           # selectInput(inputId="txSplitBy",
                           #             label="Split texts by:",
                           #             choices = c("a word at the beginning of a line" = "starting",
                           #                         "a sequence of special characters" = "into"),
                           #             selected = "starting"),
                           textInput(inputId="txSplitWord",
                                     label="Split texts by a sequence of characters (e.g. **H1**)",
                                     value=NULL),
                           hr(),
                           helpText(em("The minimum sequence of characters required to split the text must consist of at least three characters.",
                                       br(),br(),
                                       "It's important to note that the text used as a delimiter for splitting is case sensitive (e.g., 'CHAPTER' is different from 'chapter').")
                           ),
                           div(
                           fluidRow(
                             column(4,
                                    div(align="center",
                                        title = t_run,
                                        do.call("actionButton", c(run_bttn, list(
                                          inputId = "splitTextRun")
                                        ))
                                    )
                             ),
                             column(4,
                                    div(align="center",
                                    title = t_back,
                                    do.call("actionButton", c(back_bttn, list(
                                      inputId = "splitTextBack")
                                    ))
                                    )
                             ),
                             column(4,
                                    div(align="center",
                                        title = t_save,
                                        do.call("actionButton", c(list(
                                          label=NULL,
                                          style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                          icon = icon(name ="floppy-save", lib="glyphicon"),
                                          inputId = "splitTextSave")
                                        )
                                        )
                                    )
                             )
                           ), style="margin-top:-15px")

                         )
                       )
                )

              )
            )
    ),

    ### Random selection ----

    tabItem(tabName = "randomText",
            fluidPage(
              fluidRow(
                column(9,
                       shinycssloaders::withSpinner(DT::DTOutput("randomTextData"),color = getOption("spinner.color", default = "#4F7942"))
                ),
                column(3,
                       fluidRow(
                         box(
                           width = 12,
                           div(h3(strong(em("Random Text Selection"))), style="margin-top:-57px"),
                           hr(),
                           uiOutput("randomDescription"),
                           br(),
                           "Extract a random sample of texts to analyze",
                           hr(),
                           div(
                            numericInput("sampleSize",
                                         "Sample Size (%)",
                                         value = 30,
                                         min = 1,
                                         max = 100,
                                         step = 1
                            )
                            ,style="margin-top:-9px"),
                            uiOutput("sampleSizeUI"),
                           fluidRow(
                             column(4,
                                    title = t_run,
                                    do.call("actionButton", c(run_bttn, list(
                                      inputId = "randomTextRun")
                                    ))
                             ),
                             column(4,
                                    title = t_back,
                                    do.call("actionButton", c(back_bttn, list(
                                      inputId = "randomTextBack")
                                    ))
                             )
                             )

                         )
                       )
                )
              )
            )
    ),

    ### EXTERNAL INFORMATION ----

    tabItem(tabName = "extInfo",
            fluidPage(
              fluidRow(
                column(9,
                       shinycssloaders::withSpinner(DT::DTOutput("extInfoData"),color = getOption("spinner.color", default = "#4F7942"))
                ),
                column(3,
                       fluidRow(
                         box(
                           width = 12,
                           div(h3(strong(em("Add from a file"))), style="margin-top:-57px"),
                           helpText(h5("To import external information, please make sure that the file
                           to be uploaded is in Excel format and contains a column labeled
                           'doc_id' to identify documents associated to the text(s) imported.")),
                           helpText(h5("You can download the list of doc_id associated with the imported text files below.")),
                           fluidRow(
                             column(12,
                                    div(align="center",
                                        actionButton(inputId="doc_idExport",
                                                       label = strong("Export doc_id list"),
                                                       icon = NULL,
                                                       style ="border-radius: 15px; border-width: 1px; font-size: 15px;
                                                                    text-align: center; color: #ffff; ")
                                    )
                             )
                           ),
                           hr(),
                           fileInput(
                             inputId="extInfoFile",
                             label="Import external information",
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
                             column(4,
                                    div(
                               title = t_run,
                               do.call("actionButton", c(run_bttn, list(
                                 inputId = "extInfoRun")
                               )), align="center"
                             )
                             ),
                             column(4,
                                    div(align="center",
                                        title = "Back to imported text(s) ",
                                        do.call("actionButton", c(back_bttn, list(
                                          inputId = "extInfoTextBack")
                                        )
                                        )
                                    )),
                             column(4,
                                    div(
                                      title = t_save,
                                      do.call("actionButton", c(list(
                                        label=NULL,
                                        style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                        icon = icon(name ="floppy-save", lib="glyphicon")
                                      ), list(
                                        inputId = "extInfoSave")
                                      )),align="center"
                                    )
                             )
                           ), style="margin-top: -15px")

                         )

                       )
                )
              )
            )
    ),

    ### PRE-PROCESSING ----

    ## Tokenization & PoS Tagging -----

    tabItem(tabName = "tokPos",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("Tokenization & PoS Tagging"), align = "center"),
                       br(),
                       helpText(
                        p(
                          "TALL uses pre-trained annotation models based solely on ",
                          tags$a("Universal Dependencies 2.15", href = "https://universaldependencies.org/", target = "_blank"),
                          " treebanks."
                        ),
                        p(
                          "When using a language model for the first time, it will be downloaded from our repository and saved on your computer. In this case, an active internet connection is required."
                        ),style="text-align: left; text-color: #6c6b6b; font-size: 14px;", align = "center"
                      )
                )
              )
            ),
            # br(),
            # br(),
            fluidRow(
              column(9,
                     tabsetPanel(type = "tabs",
                                 tabPanel("Annotated Text Table",
                                          shinycssloaders::withSpinner(DT::DTOutput("tokPosTagData"),
                                                                       color = getOption("spinner.color", default = "#4F7942"))
                                 )
                     )
              ),
              column(3,
                     div(
                       box(
                         width = 12,
                         title = tags$div(
                          style = "display: flex; align-items: center;", # Allineamento orizzontale
                          tags$h4("Language Model   ", style = "margin-top:-20px; margin-right: 40px; font-weight: bold"), # Testo con margine a destra
                          uiOutput("flagUI") # Immagine SVG
                        ),
                        br(),
                         div(
                         fluidRow(column(6,
                                         div(
                                           #uiOutput("optionsTokenization")
                                           selectInput(
                                            inputId = 'language_model', label="Language", choices = label_lang,
                                            multiple=FALSE,
                                            width = "100%"
                                          )),style="margin-top:-9px"),
                                          column(6,
                                            div(
                                            selectInput("treebank", "Treebank", choices = NULL)),style="margin-top:-9px")
                         ),
                         fluidRow(column(6,
                          div(
                            align = "center",style="margin-top:-15px",
                            width=12,
                                         title = t_run,
                                         do.call("actionButton", c(run_bttn, list(
                                           inputId = "tokPosRun")
                                         )))
                                  ),
                                  column(6,
                                    div(
                                      align = "center",style="margin-top:-15px",
                                      width=12,
                                         title = t_save,
                                         do.call("actionButton", c(list(
                                           label=NULL,
                                           style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 15px;",
                                           icon = icon(name ="floppy-save", lib="glyphicon")
                                         ), list(
                                           inputId = "tokPosSave")
                                         ))
                         ))), style="margin-top:-5px"),
                                  tags$hr(),
                         uiOutput("info_treebank")
                       )
                     ),style="margin-top:40px"
              )
            )
    ),

    ### POS Special Tagging ----
    tabItem(tabName = "posSpecial",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("Tagging Special Entities"), align = "center"))
              )
            ),
            br(),
            br(),
            fluidRow(
              column(9,
                     tabsetPanel(type = "tabs",
                                 tabPanel("Special Entities Overview",
                                          shinycssloaders::withSpinner(DT::DTOutput("posSpecialTags"),
                                                                       color = getOption("spinner.color", default = "#4F7942"))
                                 ),
                                 tabPanel("Annotated Text Table",
                                          shinycssloaders::withSpinner(DT::DTOutput("posSpecialData"),
                                                                       color = getOption("spinner.color", default = "#4F7942"))
                                 )
                     )
              ),
              column(3,
                     div(
                       box(
                         width = 12,
                         div(h3(strong(em("Special Entities"))), style="margin-top:-57px"),
                         tags$hr(),
                         helpText(h5("When processing text, special tags will be assigned to certain detected entities.")),
                         helpText(h5("These include:")),
                         helpText(h5("•⁠  ⁠Email addresses: example@domain.com")),
                         helpText(h5("•⁠  ⁠URLs: https://www.example.com/path")),
                         helpText(h5("•  ⁠Emojis: 😊, 🚀, ❤️")),
                         helpText(h5("•⁠  ⁠Hashtags: #ExampleTag")),
                         helpText(h5("•⁠  ⁠IP addresses: 192.168.1.1")),
                         helpText(h5("•⁠  ⁠Mentions: @username")),
                         helpText(h5("This ensures that these elements are identified and marked for further analysis within the text.")),
                         style="text-align: left; text-color: #989898",
                         br(),
                         div(
                           hr(),
                           div(
                             fluidRow(column(4,
                                             div(
                                               align = "center",style="margin-top:-15px",
                                               width=12,
                                               do.call("actionButton", c(run_bttn, list(
                                                 inputId = "posSpecialRun")
                                               )))
                             ),
                             column(4,
                                    div(
                                      align = "center",style="margin-top:-15px",
                                      width=12,
                                      title = t_back,
                                      do.call("actionButton", c(back_bttn, list(
                                        inputId = "posSpecialBack")
                                      ))
                                    )
                             ),
                             column(4,
                                    div(
                                      title = t_save,
                                      div(align="center",
                                          do.call("actionButton", c(save_bttn, list(
                                            inputId = "posSpecialSave")

                                          ))
                                      )
                                    )
                             )
                             ), style="margin-top: -8px"), style="margin-top:-15px")
                   )
                     ),style="margin-top:40px"
              )
            )
    ),

    ## Custom Term Lists -----

    tabItem(tabName = "custTermList",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("Custom Term List Loading and Merging"), align = "center"))),
              br(),
              br(),
              fluidRow(
                column(9,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Pos Tagging with Custom Lists",
                                            shinycssloaders::withSpinner(DT::DTOutput("customPosTagData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   )#,
                                  #  tabPanel("Custom Term List",
                                  #           shinycssloaders::withSpinner(DT::DTOutput("customListData"),
                                  #                                        color = getOption("spinner.color", default = "#4F7942"))
                                  #  )
                       )
                ),
                column(3,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Import Custom Term List"))), style="margin-top:-57px"),
                           hr(),
                           selectInput("CTLterm",
                                       "Terms:",
                                       choices = c("Tokens" = "token",
                                                   "Lemma" = "lemma"),
                                       selected = "lemma"),
                           helpText(h5("Please ensure that the Custom Term List is formatted as an Excel file with two columns.
                                       In the first column include the desired terms.
                                       In the second column provide the corresponding list of PoS associated with each term.")),
                                           fileInput("custom_lists", label=NULL,
                                                     multiple = TRUE,
                                                     accept = c(".csv",
                                                                ".xls",
                                                                ".xlsx")),
                           hr(),
                           fluidRow(column(4,
                                           div(
                                             align = "center",style="margin-top:-15px",
                                             width=12,
                                             do.call("actionButton", c(run_bttn, list(
                                               inputId = "custTermListRun")
                                             )))
                           ),
                           column(4,
                            div(
                              align = "center",style="margin-top:-15px",
                              width=12,
                              title = t_back,
                              do.call("actionButton", c(back_bttn, list(
                                inputId = "custTermListBack")
                              ))
                            )
                     ),
                           column(4,
                                  div(
                                    title = t_save,
                                    div(align="center",
                                        do.call("actionButton", c(save_bttn, list(
                                          inputId = "custTermSave")

                                        ))
                                    )
                                  )
                           )
                           ),
                           hr(),
                           div(
                            helpText("Pressing Run Button will delete previous custom PoS"),
                            style = "text-align: center"
                           )
                         ),style="margin-top:40px"
                       )
                )

              )
            )
    ),

    ## Multi-Word Creation -----

    tabItem(tabName = "multiwordCreat",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("Multi-Word Creation"), align = "center"))),
              br(),
              br(),
              fluidRow(
                column(7,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Multi-Word List",
                                            shinycssloaders::withSpinner(DT::DTOutput("multiwordList"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   )
                                   ,tabPanel("Annotated Text including Multi-Word",
                                            shinycssloaders::withSpinner(DT::DTOutput("multiwordData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   ),
                                   tabPanel("Info & References",
                                            fluidPage(
                                              fluidRow(
                                                column(1),
                                                column(10,
                                                       br(),
                                                       HTML(infoTexts$multiwordcreation)
                                                ),
                                                column(1)
                                              )
                                            ))
                       )
                ),
                column(5,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Automatic Multi-Words"))), style="margin-top:-57px"),
                           helpText(h5("Multi-word creation extracts keywords (single terms or sequences) from the text.")),
                           helpText(h5("After keywords are generated, select those you wish to include in your data from the list.")),
                           hr(),
                           style="text-align: left; text-color: #989898",
                           fluidRow(column(6,
                            selectInput("MWmethod",
                                  "Relevant Collocation Algorithm",
                                  choices = c("Rake" = "rake",
                                        "Pointwise Mutual Information" = "pmi",
                                        "Mutual Dependency" = "md",
                                        "Log-Frequency Biased Mutual Dependency" = "lfmd"
                                       ),
                                  selected = "lemma")
                                ),
                                    column(6,
                                           selectInput("term",
                                                                    "Terms:",
                                                                    choices = c("Tokens" = "token",
                                                                                "Lemma" = "lemma"),
                                                                    selected = "lemma"))
                           ),
                           fluidRow(
                            column(6,
                              numericInput(inputId = "ngram_max",
                                           label = "Ngrams",
                                           min = 2,
                                           max = 10,
                                           value = 4,
                                           step = 1)
                            ),
                            column(6,
                                    numericInput(inputId = "freq_minMW",
                                                  label = "Freq Min",
                                                  min = 1,
                                                  max = Inf,
                                                  value = 3,
                                                  step = 1)
                                  )
                           ),
                           fluidRow(column(12,
                                           h5(em(strong("Multi-Words created by:")))
                           ),
                           ),
                           fluidRow(
                             column(12,
                                    div(
                                      class="multicol",
                                      uiOutput("multiwordPosSel")
                                    )
                             )),
                           hr(),
                           div(
                             fluidRow(column(4,
                                             div(
                                               align = "center",style="margin-top:-15px",
                                               width=12,
                                               do.call("actionButton", c(run_bttn, list(
                                                 inputId = "multiwordCreatRun")
                                               ))
                                             )
                             ),
                             column(4,
                                    div(
                                      align = "center",style="margin-top:-15px",
                                      width=12,
                                      title = t_back,
                                      do.call("actionButton", c(back_bttn, list(
                                        inputId = "multiwordCreatBack")
                                      ))
                                    )
                             ),
                             column(4,
                                    div(
                                      title = t_save,
                                      div(align="center",
                                          do.call("actionButton", c(save_bttn, list(
                                            inputId = "multiwordCreatSave")

                                          ))
                                      )
                                    )
                             )
                             ), style="margin-top:-15px"),
                           hr(),
                           #prova pulsante apply multiword
                           uiOutput("multiwordCreatApplyUI")
                         ), style="margin-top:40px; width:100%;")

                )
              )
            )
    ),

    ## Multi-word By a List ----
    tabItem(tabName = "multiwordByList",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("Multi-Word Creation by a List"), align = "center"))),
              br(),
              br(),
              fluidRow(
                column(9,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Multi-Word List",
                                            shinycssloaders::withSpinner(DT::DTOutput("multiwordList2"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   ),
                                   tabPanel("Annotated Text including Multi-Word",
                                            shinycssloaders::withSpinner(DT::DTOutput("multiwordData2"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   )
                       )
                ),
                column(3,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Import a Multi-Word List"))), style="margin-top:-57px"),
                           hr(),
                           selectInput("termMWL",
                                       "Terms:",
                                       choices = c("Tokens" = "token",
                                                   "Lemma" = "lemma"),
                                       selected = "lemma"),
                           helpText(h5("Please ensure that the Multi-Word List is formatted as an Excel/CSV file with one column.
                                       Each cell of that column include a multi-word. Each term have to be separated by a single whitespace.")),
                           fluidRow(column(12,
                                           fileInput("multiword_lists", label=NULL,
                                                     multiple = TRUE,
                                                     accept = c(".csv",
                                                                ".xls",
                                                                ".xlsx"))
                           )),
                           div(
                             hr(),
                             div(
                               fluidRow(column(4,
                                               div(
                                                 align = "center",style="margin-top:-15px",
                                                 width=12,
                                                 do.call("actionButton", c(run_bttn, list(
                                                   inputId = "multiwordListRun")
                                                 )))
                               ),
                               column(4,
                                      div(
                                        align = "center",style="margin-top:-15px",
                                        width=12,
                                        title = t_back,
                                        do.call("actionButton", c(back_bttn, list(
                                          inputId = "multiwordListBack")
                                        ))
                                      )
                               ),
                               column(4,
                                      div(
                                        title = t_save,
                                        div(align="center",
                                            do.call("actionButton", c(save_bttn, list(
                                              inputId = "multiwordListSave")

                                            ))
                                        )
                                      )
                               )
                               ), style="margin-top: -8px"), style="margin-top:-15px")
                         ),style="margin-top:40px"
                       )
                )

              )
            )
    ),

    ## PoS Tag Selection -----
    tabItem(tabName = "posTagSelect",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("PoS Tag Selection"), align = "center"))),
              br(),
              br(),
              fluidRow(
                column(8,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Annotated Text",
                                            shinycssloaders::withSpinner(DT::DTOutput("posTagSelectData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   )
                       )
                ),
                column(4,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Select:"))), style="margin-top:-57px"),
                           tags$hr(),
                           fluidRow(column(12,
                                           div(
                                             class="multicol",
                                             uiOutput("posTagListsUI")
                                           ),
                                           #h3(strong(em("Select Hapax:"))),
                                           hr(),
                                           div(
                                             class="multicol",
                                             checkboxGroupInput("posTagHapax", label=NULL,
                                                                choices = c("Hapax"),
                                                                selected = "Hapax"
                                             ),
                                             checkboxGroupInput("posTagSingleChar", label=NULL,
                                                                choices = c("Single Character"),
                                                                selected = NULL
                                             )
                                           )

                           )),
                           div(
                             hr(),
                             div(
                               fluidRow(column(6,
                                               div(
                                                 align = "center",style="margin-top:-15px",
                                                 width=12,
                                                 do.call("actionButton", c(run_bttn, list(
                                                   inputId = "posTagSelectRun")
                                                 ))
                                               )
                               ),
                               column(6,
                                      div(
                                        title = t_save,
                                        div(align="center",
                                            do.call("actionButton", c(save_bttn, list(
                                              inputId = "posTagSelectSave")

                                            ))
                                        )
                                      )
                               )
                               ), style="margin-top:-15px"), style="margin-top:-15px")
                         ), style="margin-top:40px"
                       )
                )

              )
            )
    ),
    ### FILTER ----
    tabItem(tabName = "filter_text",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("Filter docs by available external information"), align = "center"))
              ),
              fluidRow(
                column(9,
                       shinycssloaders::withSpinner(DT::DTOutput("filterData"),color = getOption("spinner.color", default = "#4F7942"))
                ),
                column(3,
                       fluidRow(
                         box(
                           width = 12,
                           div(h3(strong(em("Filter by"))), style="margin-top:-57px"),
                           hr(),
                           helpText(h5("Select an external information to filter docs:")),
                           uiOutput("filterListUI"),
                           uiOutput("filterValue"),
                           hr(),
                           conditionalPanel(
                             condition = 'input.filterList !=""',
                           column(6,
                                  div(
                                    align = "center",style="margin-top:-5px",
                                    width=12,
                                    do.call("actionButton", list(
                                      label = "Select All",
                                      style ="border-radius: 15px; border-width: 1px; font-size: 15px; text-align: center; color: #ffff; ",
                                      icon = NULL,
                                      inputId = "filterAll")
                                    )
                                  )
                                  ),
                           column(6,
                                  div(
                                    align = "center",style="margin-top:-5px",
                                    width=12,
                                    do.call("actionButton", list(
                                      label = "Deselect All",
                                      style ="border-radius: 15px; border-width: 1px; font-size: 15px; text-align: center; color: #ffff; ",
                                      icon = NULL,
                                      inputId = "filterNone")
                                    )
                                  )
                                  )
                           ),
                           br(),
                           div(
                           fluidRow(
                                    div(align="center",
                                        title = t_run,
                                        do.call("actionButton", c(run_bttn, list(
                                          inputId = "filterRun")
                                        ))

                                    )

                           ), style="margin-top:-15px")

                         )
                       )
                )

              )
            )
    ),
    ### GROUPS ----
    tabItem(tabName = "defineGroups",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("Define groups by available external information"), align = "center"))
              ),
              fluidRow(
                column(9,
                       shinycssloaders::withSpinner(DT::DTOutput("defineGroupsData"),color = getOption("spinner.color", default = "#4F7942"))
                ),
                column(3,
                       fluidRow(
                         box(
                           width = 12,
                           div(h3(strong(em("Select external information"))), style="margin-top:-57px"),
                           hr(),
                           helpText(h5("Select an external information to define new document groups:")),
                           uiOutput("defineGroupsListUI"),
                           uiOutput(outputId = "infoGroups"),
                           hr(),
                           div(
                           fluidRow(
                                    div(align="center",
                                        title = t_run,
                                        do.call("actionButton", c(run_bttn, list(
                                          inputId = "defineGroupsRun")
                                        ))
                                    )

                           ), style="margin-top:-15px")

                         )
                       )
                )

              )
            )
    ),

    ### OVERVIEW ----

    tabItem(tabName = "overview",
            fluidPage(
              fluidRow(
                column(11,
                       h3(strong("Overview"), align = "center")),
                div(
                  title = t_report,style=style_opt,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "overviewReport")
                         ))
                  )
                ),

              ),
              fluidRow(
                tabsetPanel(type = "tabs", id = "maininfo",
                            tabPanel("Overview",
                                     fluidRow(
                                       br(),
                                       tagList(
                                         useShinyjs(),
                                       column(3,
                                              div(id='clickbox1',title="Numbers of Documents", valueBoxOutput("nDoc", width = "33vh")),
                                              div(id='clickbox2',title= "Average Document's Lenght by characters", valueBoxOutput("avgDocLengthChar", width = "33vh")),
                                              div(id='clickbox3',title="Average Document's Length by tokens",valueBoxOutput("avgDocLengthTokens", width = "33vh"))),
                                       column(3,
                                              div(id='clickbox4',title="Number of Sentences", valueBoxOutput("nSentences", width = "33vh")),
                                              div(id='clickbox5',title="Average Sentence's Length by characters", valueBoxOutput("avgSentLengthChar", width = "33vh")),
                                              div(id='clickbox6',title="Average Sentence's Length by tokens", valueBoxOutput("avgSentLengthTokens", width = "33vh"))),
                                       column(3,
                                              div(id='clickbox7',title="Number of Types", valueBoxOutput("nDictionary", width = "33vh")),
                                              div(id='clickbox8',title="Number of Tokens", valueBoxOutput("nTokens", width = "33vh")),
                                              div(id='clickbox9',title="Number of Lemma", valueBoxOutput("nLemmas", width = "33vh"))),
                                       column(3,
                                              div(id='clickbox10',title="Types/Tokens Ratio", valueBoxOutput("TTR", width = "33vh")),
                                              div(id='clickbox11',title="Percentage of Hapax", valueBoxOutput("hapax", width = "33vh")),
                                              div(id='clickbox12',title="Guiraud Index", valueBoxOutput("guiraud", width = "33vh"))),
                                     )
                                     )
                            ),
                            tabPanel("Table",
                                     div(
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "overviewData", width = 700),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                                     , align="center")
                            ),
                            tabPanel("WordCloud",
                                     column(9,
                                            shinycssloaders::withSpinner(visNetworkOutput("wordcloudPlot", width="auto", height = "75vh"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                            # wordcloud2::wordcloud2Output("wordcloudPlot", height = "75vh")
                                     ),
                                     column(3,
                                            div(
                                              box(
                                                width = 12,
                                                fluidRow(
                                                  column(12,
                                                         div(
                                                           selectInput("termWC",
                                                                       label = "WordCloud by:",
                                                                       choices = c("Tokens"="token",
                                                                                   "Lemma"="lemma"),
                                                                       selected = "token"),
                                                           numericInput("nWC",
                                                                        label = "Words",
                                                                        value = 100,
                                                                        min = 10,
                                                                        max = 500,
                                                                        step = 1),
                                                           numericInput("labelsizeWC",
                                                                        label = "Text Size",
                                                                        value = 10,
                                                                        min = 1,
                                                                        max = 20,
                                                                        step = 1),
                                                           style="margin-top:-3px"
                                                         )
                                                  )),
                                                fluidRow(
                                                  column(6,
                                                         div(
                                                           align = "center",style="margin-top:15px",
                                                           width=12,
                                                           do.call("actionButton", c(run_bttn, list(
                                                             inputId = "wcApply")
                                                           ))
                                                         )
                                                  ),
                                                  column(6,
                                                         div(
                                                           align = "center",style="margin-top:15px",
                                                           width=12,
                                                           # do.call("actionButton", c(export_bttn, list(
                                                           #   inputId = "wcSave")
                                                           # ))
                                                           do.call("actionButton", c(export_bttn, list(
                                                             inputId = "wcSave")
                                                           ))
                                                         )
                                                  )
                                                )
                                                ,align="left")
                                            )
                                     )
                            ),
                            tabPanel("Vocabulary",
                                     column(9,div(
                                       shinycssloaders::withSpinner(DT::DTOutput(outputId = "dictionaryData", width = 700),
                                                                           color = getOption("spinner.color", default = "#4F7942"))),
                                       align="center"),
                                     column(3,
                                            div(
                                              box(
                                                width = 12,
                                                fluidRow(
                                                  column(9,
                                                         div(
                                                           selectInput("termDict",
                                                                       label = "Vocabulary by:",
                                                                       choices = c("Tokens"="token",
                                                                                   "Lemma"="lemma"),
                                                                       selected = "token"), style="margin-top:-3px"
                                                         )
                                                  ),
                                                  column(3,
                                                         div(
                                                           align = "center",style="margin-top:15px",
                                                           width=12,
                                                           do.call("actionButton", c(run_bttn, list(
                                                             inputId = "dictionaryApply")
                                                           ))
                                                         )
                                                  )
                                                )
                                              )
                                              ,align="left")
                                     )
                            ),
                            tabPanel("TF-IDF",
                                     column(9,
                                            div(
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "tfidfData", width = 700),
                                                                  color = getOption("spinner.color", default = "#4F7942"))),
                                     align="center"),
                                     column(3,
                                            div(
                                              box(
                                                width = 12,
                                                fluidRow(
                                                  column(9,
                                                         div(
                                                           selectInput("termTfidf",
                                                                       label = "TF-IDF by:",
                                                                       choices = c("Tokens"="token",
                                                                                   "Lemma"="lemma"),
                                                                       selected = "lemma"), style="margin-top:-3px"
                                                         )
                                                  ),
                                                  column(3,
                                                         div(
                                                           align = "center",style="margin-top:15px",
                                                           width=12,
                                                           do.call("actionButton", c(run_bttn, list(
                                                             inputId = "tfidfApply")
                                                           ))
                                                         )
                                                  )
                                                )
                                              )
                                              ,align="left")
                                     )
                                     )
                )#, align="center"
              )
            )
    ),

    ### WORDS ----

    ## Frequency List----

    ## Words Frequency by PoS----

    tabItem(tabName = "w_freq",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Words Frequency by PoS"), align = "center")
                ),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "wFreqApply")
                         ))
                  )),

                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "wFreqExport")
                         )),
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "wFreqReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             numericInput("wFreqN",
                                          label=("Number of words"),
                                          value = 20),
                             uiOutput("posSelectionFreq"),
                             selectInput("wFreqTerm",
                                         "Terms:",
                                         choices = c("Tokens" = "token",
                                                     "Lemma" = "lemma"),
                                         selected = "lemma"),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             tooltip = tooltipOptions(title = "Options"),
                             style = "material-circle", size = "sm"
                           )

                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "wFreqPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("wFreqTable", width = 700),
                                                                  color = getOption("spinner.color", default = "#4F7942")),
                                     align="center"
                            )
                )
              )
            )
    ),

    ### Part of Speech ----
    tabItem(tabName = "w_pos",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Part of Speech Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "posApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "posExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "posReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             numericInput("posN",
                                          label=("Number of PoS"),
                                          value = 20),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             tooltip = tooltipOptions(title = "Options"),
                             style = "material-circle", size = "sm"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "posPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("posTable", width = 700),
                                                                  color = getOption("spinner.color", default = "#4F7942")),
                                     align="center"
                            )
                )
              )
            )
    ),

    ## Words in Context -----

    tabItem(tabName = "wordCont",
            fluidPage(
              fluidRow(
                column(12,
                       h3(strong("Words in Context"), align = "center"))
              )
            ),
            br(),
            br(),
            fluidRow(
              column(9,
                     tabsetPanel(type = "tabs",
                                 tabPanel("Words in Context",
                                 div(
                                   style = "height: 550px; overflow-y: scroll; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;",

                                            shinycssloaders::withSpinner(uiOutput("wordsContHtml"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                 )
                                 ),
                                 tabPanel("Network",
                                          shinycssloaders::withSpinner(visNetworkOutput("wordsContNetwork", width="auto", height = "75vh"),
                                                                       color = getOption("spinner.color", default = "#4F7942"))
                                 )
                     )
              ),
              column(3,
                     div(
                       box(
                         width = 12,
                         div(h3(strong(em("Words in Context"))), style="margin-top:-57px"),
                         tags$hr(),
                         style="text-align: left; text-color: #989898",
                         selectizeInput(inputId = "wordsContSearch",
                           label = "Search word(s) in text", choices = NULL),

                         # searchInput(
                         #   inputId = "wordsContSearch",
                         #   label = "Search word(s) in text",
                         #   placeholder = "",
                         #   btnSearch = icon("search"),
                         #   btnReset = icon("remove"),
                         #   resetValue = "",
                         #   width = "100%"
                         # ),
                         h4("Window Length:"),
                         fluidRow(
                           column(6,
                                  numericInput(inputId = "wordsContBefore",
                                               label = "Before",
                                               value = 5,
                                               min = 1,
                                               max = 20)
                                  ),
                           column(6,
                                  numericInput(inputId = "wordsContAfter",
                                               label = "After",
                                               value = 5,
                                               min = 1,
                                               max = 20)
                                  )
                         ),
                         fluidRow(
                           column(4,
                                  div(align="center",
                                      title = "Apply",
                                      do.call("actionButton", c(list(
                                        label=NULL,
                                        style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                        icon = icon(name ="play", lib="glyphicon"),
                                        inputId = "wordsContApply")
                                      )))
                           ),
                           column(4,
                                  div(align="center",
                                      title = "Reset",
                                      do.call("actionButton", c(list(
                                        label=NULL,
                                        style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                        icon = icon(name ="remove", lib="glyphicon"),
                                        inputId = "wordsContReset")
                                      )))
                           ),
                           column(4,
                                  div(align="center",
                                      title = "Export raw text(s) in Excel",
                                      do.call("actionButton", c(list(
                                        label=NULL,
                                        style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                        icon = icon(name ="download-alt", lib="glyphicon"),
                                        inputId = "wordsContSave")
                                      )
                                      )
                                  )
                                  ))
                       ),style="margin-top:40px"
                     )
              )
            )
    ),

    ### Clustering ----
    tabItem(tabName = "w_clustering",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Clustering"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "w_clusteringApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "w_clusteringExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "w_clusteringReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             selectInput("termClustering",
                                         "By:",
                                         choices = c("Tokens" = "token",
                                                     "Lemma" = "lemma"),
                                         selected = "lemma"),
                             selectInput("w_clusteringSimilarity",
                                         label = "Words Similarity by:",
                                         choices = c("None"="none",
                                                     "Association Index"="association",
                                                     "Cosine Similarity"="cosine",
                                                     "Jaccard Index"="jaccard"),
                                         selected = "association"),
                             selectInput("w_clusteringMode",
                                         label = "Cluster selection:",
                                         choices = c("Auto"="auto",
                                                     "Manual"="manual"),
                                         selected = "auto"),
                             conditionalPanel(
                               condition = "input.w_clusteringMode == 'manual'",
                               numericInput("w_nclusters",
                                            label = "N. of Clusters",
                                            value = 1,
                                            min = 1,
                                            step = 1
                               )
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("w_clusteringNMax",
                                                   label = "Words",
                                                   value = 50,
                                                   min = 2,
                                                   step=1)
                               ),
                               column(6,
                                      numericInput("w_clusteringLabelSize",
                                                   label = "Label Size",
                                                   value = 4,
                                                   min = 1,
                                                   step = 0.5)
                               )
                             ),
                             tooltip = tooltipOptions(title = "Options"),
                             width = "300px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             style = "material-circle", size = "sm"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Dendrogram",
                                     shinycssloaders::withSpinner(visNetworkOutput("w_clusteringPlot", width="auto", height = "75vh"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("w_clusteringTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )

                )
              )
            )
    ),

    ### Reinert Clustering ----
    tabItem(tabName = "w_reinclustering",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Reinert Clustering"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "w_reinclusteringApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "w_reinclusteringExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "w_reinclusteringReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             selectInput("termReinClustering",
                                         "By:",
                                         choices = c("Tokens" = "token",
                                                     "Lemma" = "lemma"),
                                         selected = "token"),
                             fluidRow(
                               column(6,
                                      numericInput("w_rein_k",
                                                   label = "Max N. of Clusters",
                                                   value = 10,
                                                   min = 1,
                                                   step = 1
                                      )
                               ),
                               column(6,
                                      numericInput("w_rein_min_split_members",
                                                   label = "Min. Segments per Cluster",
                                                   value = 10,
                                                   min = 1,
                                                   step = 1
                                      )
                               )
                             ),
                             hr(),
                             h4(("Segment parameters ")),
                             fluidRow(
                               column(6,
                             numericInput("w_rein_segments_size",
                                            label = "Segment Lenght",
                                            value = 40,
                                            min = 3,
                                            step = 1
                               )),
                             column(6,
                                    numericInput("w_rein_min_segments",
                                                 label = "Min. Segment Lenght",
                                                 value = 5,
                                                 min = 3,
                                                 step = 1
                                    ))
                             ),
                             hr(),
                             h4(("Feature selection parameters")),
                             fluidRow(
                               column(6,
                                      numericInput("w_rein_cc_test",
                                                   label = "Contingency Coefficient Value",
                                                   value = 0.3,
                                                   min = 0.1,
                                                   step = 0.1,
                                                   max = 0.8
                                      )),
                               column(6,
                                      numericInput("w_rein_tsj",
                                                   label = "Min freq",
                                                   value = 3,
                                                   min = 1,
                                                   step = 1
                                      ))
                             ),

                             tooltip = tooltipOptions(title = "Options"),
                             width = "300px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             style = "material-circle", size = "sm"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Dendrogram",
                                     br(),
                                     uiOutput("ReinCutree"),
                                     shinycssloaders::withSpinner(visNetworkOutput("w_ReinClusteringPlot", width="auto", height = "75vh"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                            ,tabPanel("Summary",
                                      shinycssloaders::withSpinner(DT::DTOutput("w_ReinSummaryTable"),
                                                                   color = getOption("spinner.color", default = "#4F7942"))
                            )
                            ,tabPanel("Terms by Cluster",
                                      shinycssloaders::withSpinner(DT::DTOutput("w_ReinClusteringTableTerms"),
                                                                   color = getOption("spinner.color", default = "#4F7942"))
                            )
                            ,tabPanel("Segments by Cluster",
                                     shinycssloaders::withSpinner(DT::DTOutput("w_ReinClusteringTableSegments"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                            ,tabPanel("References",
                                     fluidPage(
                                       fluidRow(
                                         column(1),
                                         column(10,
                                                br(),
                                                HTML(infoTexts$reinert)
                                         ),
                                         column(1)
                                       )
                                     ))


                )
              )
            )
    ),

    ### Correspondence Analysis ----
    tabItem(tabName = "ca",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Correspondence Analysis"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "caApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "caExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "caReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             selectInput(
                               inputId = "groupCA",
                               label = "Unit of Analysis ",
                               choices = c("Groups", "Documents", "Paragraphs", "Sentences"),
                               selected = "Documents"
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("nCA",
                                                   label = "Words",
                                                   value = 50,
                                                   min = 2,
                                                   step=1)
                               ),
                               column(6,
                                      selectInput("termCA",
                                                  "By:",
                                                  choices = c("Tokens" = "token",
                                                              "Lemma" = "lemma"),
                                                  selected = "lemma")
                               )),
                             fluidRow(
                               column(6,
                                      numericInput("nClustersCA",
                                                   label = "Clusters",
                                                   value = 1,
                                                   min = 1,
                                                   step = 1)
                               ),
                               column(6,
                                      numericInput("nDimsCA",
                                                   label = "Dims for Clustering",
                                                   value = 2,
                                                   min = 1,
                                                   max = 10,
                                                   step = 1)
                               )
                             ),
                             hr(),
                             h4(strong("Graphical options: ")),
                             br(),
                             selectInput("dimPlotCA",
                                         "Select plane to plot:",
                                         choices = c("1° Factorial Plane" = "1",
                                                     "2° Factorial Plane" = "2",
                                                     "3° Factorial Plane" = "3",
                                                     "4° Factorial Plane" = "4",
                                                     "5° Factorial Plane" = "5"),
                                         selected = "1"),
                             numericInput("nDocCA",
                                          label = "Top Contributing Docs/Groups",
                                          value = 0,
                                          min = 0,
                                          step = 1),
                             numericInput("lim.contribCA",
                                          label = "Max Contribution",
                                          value = 2,
                                          min = 0.01,
                                          max = 10,
                                          step = 0.01),
                             fluidRow(
                               column(6,
                                      numericInput("labelsizeCA",
                                                   label = "Label Size",
                                                   value = 16,
                                                   min = 2,
                                                   step=1)
                               ),
                               column(6,
                                      numericInput("sizeCA",
                                                   label = "Min. Dot Size",
                                                   value = 2,
                                                   min = 0,
                                                   max = 20,
                                                   step = 1)
                               )
                             ),

                             tooltip = tooltipOptions(title = "Options"),
                             width = "300px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             style = "material-circle", size = "sm"
                           ),
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Factorial Plane",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "caPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Dendrogram",
                                     shinycssloaders::withSpinner(visNetworkOutput("caDendrogram", width="auto", height = "75vh"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Singular Values",
                                     shinycssloaders::withSpinner(DT::DTOutput("caSingularValueTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Coordinates",
                                     shinycssloaders::withSpinner(DT::DTOutput("caCoordTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Contributions",
                                     shinycssloaders::withSpinner(DT::DTOutput("caContribTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Cosines Squared",
                                     shinycssloaders::withSpinner(DT::DTOutput("caCosineTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Info & References",
                                     fluidPage(
                                       fluidRow(
                                         column(1),
                                         column(10,
                                                br(),
                                                HTML(infoTexts$correspondenceanalysis)
                                         ),
                                         column(1)
                                       )
                                     )
                            )
                            )
              )
            )
    ),


    ### Network ----

    ## Co-word analysis ----

    tabItem(tabName = "w_networkCooc",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Co-word analysis"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "w_networkCoocApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "w_networkCoocExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "w_networkCoocReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             selectInput(inputId="w_term",
                                         label = "Terms:",
                                         choices = c(
                                           "Tokens"="token",
                                           "Lemma"="lemma"),
                                         selected = "lemma"
                             ),
                             selectInput(
                               inputId = "w_groupNet",
                               label = "Co-occurrences in ",
                               choices = c("Groups","Documents", "Paragraphs", "Sentences"),
                               selected = "Sentences"
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
                             ),
                             selectInput("normalizationCooc",
                                         label = "Normalization by:",
                                         choices = c("None"="none",
                                                     "Association Index"="association",
                                                     "Cosine Similarity"="cosine",
                                                     "Jaccard Index"="jaccard"),
                                         selected = "association"),
                             fluidRow(
                               column(6,
                                      numericInput("nMax",
                                                   label = "Words",
                                                   value = 100,
                                                   min = 2,
                                                   step=1),
                                      numericInput("labelSize",
                                                   label = "Label Size",
                                                   value = 4,
                                                   min = 1,
                                                   step = 0.5)
                               ),column(6,
                                        selectInput("minEdges",
                                                    label = "Top Link (%)",
                                                    choices = c("Auto", paste0(seq(10,100,10),"%")),
                                                    selected = "Auto"
                                                    ),
                                        numericInput("opacity",
                                                     label = "Opacity",
                                                     value = 0.6,
                                                     min = 0,
                                                     step = 0.1)
                               )),
                             tooltip = tooltipOptions(title = "Options"),
                             width = "300px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             style = "material-circle", size = "sm"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network",
                                     shinycssloaders::withSpinner(visNetworkOutput("w_networkCoocPlot", width="auto", height = "75vh"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Words",
                                     shinycssloaders::withSpinner(DT::DTOutput("w_networkCoocNodesTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Links",
                                     shinycssloaders::withSpinner(DT::DTOutput("w_networkCoocEdgesTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Info & References",
                                     fluidPage(
                                       fluidRow(
                                         column(1),
                                         column(10,
                                                br(),
                                                HTML(infoTexts$cowordanalysis)
                                         ),
                                         column(1)
                                       )
                                     )
                            )
                            )
              )
            )
    ),

    ## GRAKO ----

    tabItem(tabName = "w_networkGrako",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Grako"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "w_networkGrakoApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "w_networkGrakoExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "w_networkGrakoReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             selectInput(inputId="grako_term",
                                         label = "By:",
                                         choices = c(
                                           "Tokens"="token",
                                           "Lemma"="lemma"),
                                         selected = "lemma"
                             ),
                             selectInput("grakoNormalization",
                                         label = "Normalization by:",
                                         choices = c("None"="none",
                                                     "Association Index"="association",
                                                     "Cosine Similarity"="cosine",
                                                     "Jaccard Index"="jaccard"),
                                         selected = "association"),
                             materialSwitch(
                               inputId = "grakoUnigram",
                               label = "Include Single words",
                               value = TRUE,
                               status = "success"
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("grakoNMax",
                                                   label = "Links",
                                                   value = 30,
                                                   min = 2,
                                                   step=1),
                                      numericInput("grakoMinEdges",
                                                   label = "Top Link (%)",
                                                   value = 100,
                                                   min = 0,
                                                   max = 100,
                                                   step = 1)
                               ),column(6,
                                        numericInput("grakoLabelSize",
                                                     label = "Label Size",
                                                     value = 4,
                                                     min = 0.0,
                                                     step = 0.5),
                                        numericInput("grakoOpacity",
                                                     label = "Opacity",
                                                     value = 0.6,
                                                     min = 0,
                                                     step = 0.1)
                               )),
                             tooltip = tooltipOptions(title = "Options"),
                             width = "300px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             style = "material-circle", size = "sm"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Network",
                                     shinycssloaders::withSpinner(visNetworkOutput("w_networkGrakoPlot", width="auto", height = "75vh"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Words",
                                     shinycssloaders::withSpinner(DT::DTOutput("w_networkGrakoNodesTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Links",
                                     shinycssloaders::withSpinner(DT::DTOutput("w_networkGrakoEdgesTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### DOCUMENTS ----

    ### Topic Modeling ----
    ### K choice ----

    tabItem(tabName = "d_tm_select",
            fluidPage(
              fluidRow(
                column(9,
                       h3(strong("Topic Modeling: Optimal selection of topic number"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "d_tm_selectApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "d_tm_selectExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_tm_selectReport")
                         ))
                  ))
              ),
              fluidRow(
                column(9,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Tuning Plot",
                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "d_tm_selectPlot", height = "75vh",width ="98.9%"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   ),
                                   tabPanel("Table",
                                            shinycssloaders::withSpinner(DT::DTOutput("d_tm_selectTable"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   ),
                                   tabPanel("Info & References",
                                            fluidPage(
                                              fluidRow(
                                                column(1),
                                                column(10,
                                                       br(),
                                                       HTML(infoTexts$tmkchoice)
                                                ),
                                                column(1)
                                              )
                                            )
                                   )
                       )
                ),
                column(3,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Find optimal K"))), style="margin-top:-57px"),
                           tags$hr(),
                           style="text-align: left; text-color: #989898",
                           selectInput(
                             inputId = "groupTm",
                             label = "Topics",
                             choices = c("Groups" = "Groups",
                                         "Docs"="doc_id",
                                         "Sentences"="sentence_id"),
                             selected = "doc_id"
                           ),
                           uiOutput("TMmetric"),
                           fluidRow(column(6,
                                           selectInput("termTm", "Terms:",
                                                       choices = c(
                                                         "Tokens"="token",
                                                         "Lemma"="lemma"),
                                                       selected = "lemma"
                                           )),
                                    column(6,
                                           numericInput("nTm",
                                                        label = "N. of terms",
                                                        value = 100,
                                                        min = 1,
                                                        step=1))
                           ),
                           selectInput("top_by", "Terms selection by:",
                                       choices = c(
                                         "Frequency"="freq",
                                         "TF-IDF"="tfidf"),
                                       selected = "freq"
                           ),
                           fluidRow(column(4,
                                           numericInput("minK",
                                                        label = "K min",
                                                        value = 2,
                                                        min = 2,
                                                        step=1)
                           ),
                           column(4,
                                  numericInput("maxK",
                                               label = "K max",
                                               value = 20,
                                               min = 3,
                                               step=1)
                           ),
                           column(4,
                                  numericInput("Kby",
                                               label = "K by:",
                                               value = 1,
                                               min = 1,
                                               step=1)
                           )
                           )
                         ),style="margin-top:40px"
                       )
                )
              )
            )
    ),

    ### Model estimation ----

    tabItem(tabName = "d_tm_estim",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Topic Modeling: Model estimation"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "d_tm_estimApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "d_tm_estimExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_tm_estimReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             materialSwitch(
                               inputId = "tmKauto",
                               label = "Automatic Topic Selection",
                               value = TRUE,
                               status = "success"
                             ),
                             conditionalPanel('!input.tmKauto',
                                              numericInput("KEstim",
                                                           label = "N. of Topics (K)",
                                                           value = 2,
                                                           min = 2,
                                                           step=1)
                             ),
                             selectInput(
                               inputId = "groupTmEstim",
                               label = "Topics",
                               choices = c("Groups"="Groups",
                                           "Docs"="doc_id",
                                           "Sentences"="sentence_id"),
                               selected = "doc_id"),
                             fluidRow(
                               column(6,
                                      selectInput("termTmEstim", "By:",
                                                  choices = c(
                                                    "Tokens"="token",
                                                    "Lemma"="lemma"),
                                                  selected = "lemma"
                                      )),
                               column(6,
                                      numericInput("nTmEstim",
                                                   label = "N. of terms",
                                                   value = 100,
                                                   min = 1,
                                                   step=1))
                             ),
                             selectInput("top_byEstim", "Terms selection by:",
                                         choices = c(
                                           "Frequency"="freq",
                                           "TF-IDF"="tfidf"),
                                         selected = "freq"),
                             hr(),
                             numericInput("nTopicPlot",
                                          label = "Word/Docs per Topic",
                                          value = 10,
                                          min = 2,
                                          step=1),
                             tooltip = tooltipOptions(title = "Options"),
                             width = "300px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             style = "material-circle", size = "sm")
                ),
                style = style_opt
                )
              ),
              tabsetPanel(type = "tabs",
                          tabPanel("Topic by Words Plot",
                                   fluidRow(
                                     column(4,
                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "d_tm_estimTPlot1", height = "75vh",width ="98.9%"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))),
                                     column(4,
                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "d_tm_estimTPlot2", height = "75vh",width ="98.9%"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))),
                                     column(4,
                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "d_tm_estimTPlot3", height = "75vh",width ="98.9%"),
                                                                         color = getOption("spinner.color", default = "#4F7942")))
                                   ),
                                   fluidRow(
                                     actionButton("TMplotLeft", icon("menu-left", lib = "glyphicon")),
                                     actionButton("TMplotRight", icon("menu-right", lib = "glyphicon")),
                                     align="center"
                                   )
                          ),
                          tabPanel("Topic by Docs Plot",
                                   fluidRow(
                                     column(4,
                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "d_tm_DocPlot1", height = "75vh",width ="98.9%"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))),
                                     column(4,
                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "d_tm_DocPlot2", height = "75vh",width ="98.9%"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))),
                                     column(4,
                                            shinycssloaders::withSpinner(plotlyOutput(outputId = "d_tm_DocPlot3", height = "75vh",width ="98.9%"),
                                                                         color = getOption("spinner.color", default = "#4F7942")))
                                   ),
                                   fluidRow(
                                     actionButton("TMdocLeft", icon("menu-left", lib = "glyphicon")),
                                     actionButton("TMdocRight", icon("menu-right", lib = "glyphicon")),
                                     align="center"
                                   )
                          ),
                          tabPanel("Beta Probability",
                                   shinycssloaders::withSpinner(DT::DTOutput("d_tm_estimBpTable"),
                                                                color = getOption("spinner.color", default = "#4F7942"))
                          ),
                          tabPanel("Theta Probability",
                                   shinycssloaders::withSpinner(DT::DTOutput("d_tm_estimTpTable"),
                                                                color = getOption("spinner.color", default = "#4F7942"))
                          ),
                          tabPanel("Topic Correlation",
                                   shinycssloaders::withSpinner(plotlyOutput(outputId = "d_tm_networkPlot", height = "75vh",width ="98.9%"),
                                                                #visNetworkOutput("d_tm_networkPlot", width="auto", height = "75vh"),
                                                                color = getOption("spinner.color", default = "#4F7942"))
                          ),
                          tabPanel("Info & References",
                                   fluidPage(
                                     fluidRow(
                                       column(1),
                                       column(10,
                                              br(),
                                              HTML(infoTexts$tmmodelestimation)
                                       ),
                                       column(1)
                                     )
                                   )
                          )

              )
            )
    ),

    ### Polarity detection ----

    tabItem(tabName = "d_polDet",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Polarity Detection"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "d_polDetApply")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("actionButton", c(export_bttn, list(
                           inputId = "d_polDetExport")
                         ))
                  )),

                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_polDetReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             selectInput(
                               inputId = "groupPolarity",
                               label = "Polarity of",
                               choices = c("Groups" = "Groups",
                                           "Docs"="doc_id"),
                               selected = "doc_id"
                             ),
                             uiOutput("lexiconD_polarity"),
                             tooltip = tooltipOptions(title = "Options"),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = TRUE, animate = TRUE,
                             style = "material-circle", size = "sm"
                           )
                ),
                style = style_opt
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Document Polarity Distribution",
                                     fluidRow(
                                       column(6,
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "d_polPiePlot", height = "75vh", width ="98.9%"),
                                                                           color = getOption("spinner.color", default = "#4F7942"))),
                                       column(6,
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "d_polDensPlot", height = "37vh", width ="98.9%"),
                                                                           color = getOption("spinner.color", default = "#4F7942")),#),
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "d_polBoxPlot", height = "37vh", width ="98.9%"),
                                                                           color = getOption("spinner.color", default = "#4F7942"))#)
                                       )
                                     )

                            ),
                            tabPanel("Top Words",
                                     fluidRow(
                                       column(6, align="center",
                                              h4("Top Positive Words by Document Polarity Distribution"),
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "d_polDetPlotPos", height = "75vh",width ="98.9%"),
                                                                           color = getOption("spinner.color", default = "#4F7942"))),
                                       column(6, align="center",
                                              h4("Top Negative Words by Document Polarity Distribution"),
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "d_polDetPlotNeg", height = "75vh",width ="98.9%"),
                                                                           color = getOption("spinner.color", default = "#4F7942")))
                                     )
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("d_polDetTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Info & References",
                                     fluidPage(
                                       fluidRow(
                                         column(1),
                                         column(10,
                                                br(),
                                                HTML(infoTexts$polaritydetection)
                                         ),
                                         column(1)
                                       )
                                     )
                            )
                )
              )
            )
    ),

    ### Summarization ----

    tabItem(tabName = "d_summarization",
            fluidPage(
              fluidRow(
                column(11,
                       h3(strong("Summarization"), align = "center")),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_summarizationReport")
                         ))
                  ))
              ),
              br(),
              br(),
              fluidRow(
                column(9,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Abstract",
                                            fluidRow(style = "height:65vh",
                                            shinycssloaders::withSpinner(uiOutput("abstractData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                            ),
                                            fluidRow(align = "center",
                                            shinycssloaders::withSpinner(uiOutput("sliderAbstractData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                            )
                                   ),
                                   tabPanel("Full Document",
                                            shinycssloaders::withSpinner(DT::DTOutput("documentData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   ),
                                   tabPanel("Sentence Ranking",
                                            shinycssloaders::withSpinner(DT::DTOutput("RelSentData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   ),
                                   tabPanel("Info & References",
                                            fluidPage(
                                              fluidRow(
                                                column(1),
                                                column(10,
                                                       br(),
                                                       HTML(infoTexts$summarization)
                                                ),
                                                column(1)
                                              )
                                            )
                                   )

                       )
                ),
                column(3,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Sentences extraction"))), style="margin-top:-57px"),
                           tags$hr(),
                           fluidRow(column(12,
                                           uiOutput("optionsUnitSummarization"),
                                           uiOutput("optionsSummarization"))
                           ),
                           hr(),
                           div(
                           fluidRow(
                             column(4,
                                    div(align="center",
                                        title = t_run,
                                        do.call("actionButton", c(run_bttn, list(
                                          inputId = "d_summarizationApply")
                                        ))
                                    )
                             ),
                             column(4,
                                    div(align="center",
                                        title = t_view,
                                        do.call("actionButton", c(view_bttn, list(
                                          inputId = "d_summarizationView")
                                        ))
                                    )
                             ),
                             column(4)
                           ), style="margin-top:-15px")
                         ),style="margin-top:40px"
                       )
                )
              )
            )
    ),

    ### REPORT ----
    tabItem(tabName = "report",
            fluidPage(
              fluidRow(
                h3(strong("Report"), align="center"),
                br(),
              ),
              fluidRow(
                column(6,offset = 1,
                       box(title = strong("Select results to include in the Report",
                                          style='font-size:20px;color:white;'),
                           status = "primary", width = 11, solidHeader = TRUE,
                           tags$style(HTML("
                         .box.box-solid.box-primary>.box-header {
                         background:#4F7942;
                         }
                         .box.box-solid.box-primary{
                         border-bottom-color:black;
                         border-left-color:black;
                         border-right-color:black;
                         border-top-color:black;
                         border-width:2px;
                                         }")),
                           uiOutput('reportSheetsUI'),
                           tags$style("#reportSheets {font-size:18px;}")
                       )
                ),#column(1),
                column(2,
                       div(style ="border-radius: 10px; border-width: 3px; font-size: 10px;",
                           align = "center",
                           actionBttn(
                             inputId = 'allSheets',
                             label = strong('Select All'),
                             icon = icon("ok-circle", lib="glyphicon"),
                             style = "pill", color = "primary",
                             block = TRUE
                           ),
                           br(),
                           actionBttn(
                             inputId = 'noSheets',
                             label = strong('Deselect All'),
                             icon = icon("remove-circle", lib="glyphicon"),
                             style = "pill", color = "primary",
                             block = TRUE
                           ),
                           br(),
                           hr(),
                           actionBttn(
                             inputId="report.save",
                             label = strong("Export Report"),
                             style = "pill", color = "success",
                             size = "md",
                             block = TRUE,
                             no_outline = TRUE,
                             icon = icon(name ="download-alt", lib="glyphicon")
                           ),
                           br(),
                           hr(),
                           actionBttn(
                             inputId = 'deleteAll',
                             label = strong('Delete Report'),
                             icon = icon("exclamation-sign", lib="glyphicon"),
                             style = "pill", color = "danger",
                             block = TRUE
                           )

                       )
                )
              )


            )
    ),

    ### SETTINGS ----
    tabItem(tabName = "settings",
            fluidPage(
              fluidRow(
                h3(strong("Settings"), align="center"),
                br(),
              ),
              fluidRow(column(6,
                              h3("Settings:"),
                              br(),
                              shinyDirButton("workingfolder", "Select a Working Folder", "Select"),
                              textOutput("wdFolder"),
                              hr(),
                              actionButton(inputId="cache",
                                           label="Clean temporary folder")
                              # sliderTextInput(
                              #   inputId = "dpi",
                              #   label = "Please select the desired DPI",
                              #   grid = TRUE,
                              #   force_edges = TRUE,
                              #   choices = c("75", "150", "300", "600"),
                              #   width = "70%",
                              #   selected = "300"
                              # ),
                              # br(),
                              # sliderTextInput(
                              #   inputId = "h",
                              #   label = "Please select the desired heigth in inches",
                              #   grid = TRUE,
                              #   force_edges = TRUE,
                              #   width = "70%",
                              #   choices = seq(5,15),
                              #   selected = "7"
                              # )
              ), column(6
                        ### To insert settings for default path, etc.
              )
              )
            )
    )

  )) # END DASHBOARDBODY


## UserInterface ####
ui <- dashboardPage(header, sidebar, body)



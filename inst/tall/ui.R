# UI
#  Function source ----
source("libraries.R", local=TRUE)
source("tallFunctions.R", local=TRUE)
libraries()

## button style and contents

style_bttn <- "border-radius: 20px; border-width: 1px; font-size: 17px; text-align: center; color: #ffff; padding-left: 20px; padding-right: 20px"
style_opt <-  "border-radius: 35px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (option button)
style_start <-  "border-radius: 15px; border-width: 3px; font-size: 15px; width:100% " # (start button)
#style_bttn <- "border-radius: 15px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (action buttons)
t_report  <-  "Add Results to the Report"
t_export  <-  "Export Plot as PNG"
t_run <- "Run the Analysis"
t_save <- "Save the Analysis"

run_bttn <- list(
  label = NULL,
  style ="display:block; height: 45px; width: 45px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name ="play", lib="glyphicon")
)
report_bttn <- list(
  label = NULL,
  style ="display:block; height: 45px; width: 45px; border-radius: 50%; border: 3px; margin-top: 0px",
  icon = icon(name ="plus", lib="glyphicon")
)
export_bttn <- list(
  label=NULL,
  style ="display:block; height: 45px; width: 45px; border-radius: 50%; border: 3px; margin-top: 0px",
  icon = icon(name ="download-alt", lib="glyphicon")
)
save_bttn <- list(
  label=NULL,
  style ="display:block; height: 43px; width: 43px; border-radius: 50%; border: 1px;",# margin-top: 15px",
  icon = icon(name ="floppy-save", lib="glyphicon")
)


## HEADER ----

title_tall <- tags$link(tags$a(href = 'https://www.unina.it/',target="_blank",
                               #tags$img(src="unina_logo.png", height = '30',width='30')
),
strong("TALL"))

header <- shinydashboardPlus::dashboardHeader(title = title_tall,
                                              titleWidth = 250, controlbarIcon = NULL)



## SIDEBAR ----
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(id="sidebarmenu",
              #style = "position: relative; overflow: visible;",
              menuItem("TALL", tabName = "tall", icon = icon("text-size", lib = "glyphicon")),
              menuItemOutput ("rest_of_sidebar")
  )
)

## BODY ----

body <- dashboardBody(
  customTheme(),
  tags$style(".glyphicon-download-alt {color:#ffffff; font-size: 24px; align: center; margin-left: -2.5px}"),
  tags$style(".glyphicon-play {color:#ffffff; font-size: 24px; align: center}"),
  tags$style(".glyphicon-plus {color:#ffffff; font-size: 24px;align: center; margin-left: -0.5px}"),
  tags$style(".glyphicon-cog {color:#ffffff; font-size: 26px; margin-top: 2px; margin-left: -2px}"),
  tags$style(".glyphicon-floppy-save {color:#ffffff; font-size: 23px; text-align:center; padding-right: -10px;
             margin-top: 1px;}"),#margin-top: 4px; margin-down: 22px; margin-right: 25px}"),

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
    # tags$script(
    #   'var dimension = [0, 0];
    #           $(document).on("shiny:connected", function(e) {
    #               dimension[0] = window.innerWidth;
    #               dimension[1] = window.innerHeight;
    #               Shiny.onInputChange("dimension", dimension);
    #           });
    #           $(window).resize(function(e) {
    #               dimension[0] = window.innerWidth;
    #               dimension[1] = window.innerHeight;
    #               Shiny.onInputChange("dimension", dimension);
    #           });
    #           $(document).ready(function(){
    #               $("a[data-toggle=tab]").on("show.bs.tab", function(e){
    #                 Shiny.setInputValue("activeTab", $(this).attr("data-value"));
    #                });
    #         });
    #   '
    # )
  ),

  tabItems(

    ### TALL PAGE ----
    tabItem(tabName = "tall",
            fluidRow(
              h1(strong("TALL"), align="center"),
              br(),
              h3("Text Analysis for aLL",align = "center"),
              br(),
              div(p("Powered by ",
                    em(a("K-Synth",
                         href = "https://k-synth.com/", target="_blank")),
                    style="text-align:center; font-size:17px;"))
            )
    ),

    ### IMPORT TEXT ----

    tabItem(tabName = "import_tx",
            fluidPage(
              fluidRow(
                column(9,
                       shinycssloaders::withSpinner(DT::DTOutput("dataImported"),color = getOption("spinner.color", default = "#4F7942"))
                ),
                column(3,
                       fluidRow(
                         box(
                           width = 12,
                           div(h3(strong(em("Import texts"))), style="margin-top:-57px"),
                           hr(),
                           selectInput("load", "Please, choose what to do",
                                       choices = c(
                                         " "= "null",
                                         "Load text files"="import",
                                         "Load Tall structured files"="load_tall",
                                         "Use a sample collection"="demo"
                                       ),
                                       selected = "null"
                           ),
                           conditionalPanel(
                             condition="input.load == 'import'",
                             fluidRow(column(6,
                                             selectizeInput(
                                               'ext', label="File format",choices = c(
                                                 "txt"="txt",
                                                 "csv"="csv",
                                                 "excel"="xlsx"),
                                               tags$style("height: 50px")
                                             )
                             ),
                             column(6,
                                    selectizeInput(
                                      'line_sep', label="Line Separator",choices = c(
                                        "Dot (.)"="no",
                                        "Return (\\n)"="yes"),
                                      tags$style("height: 50px")
                                    )
                             )
                             ),
                             uiOutput("file_raw")
                           ),
                           conditionalPanel(
                             condition="input.load=='demo'",
                             helpText(h4("This is a sample collection ...."))
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
                           conditionalPanel(condition = "input.load != 'null'",
                                            fluidRow(
                                              column(12,
                                                     div(
                                                       align = "center",
                                                       width=12,
                                                       br(),
                                                       actionButton(inputId="runImport",
                                                                    label = strong("START"),
                                                                    icon = icon(name="play",lib = "font-awesome"),
                                                                    style = style_bttn
                                                       ))
                                              )
                                            )
                           ),
                           conditionalPanel(condition="input.load != 'null'",
                                            tags$hr(),
                                            fluidRow(
                                              column(12,
                                                     div(
                                                       align = "center",
                                                       width=12,
                                                       downloadButton(outputId="collection.save",
                                                                      label = strong("Export Raw Texts in Excel"),
                                                                      icon = icon(name="download", lib = "font-awesome"),
                                                                      style ="border-radius: 15px; border-width: 1px; font-size: 17px;
                                                                    text-align: center; color: #ffff; "
                                                       ))
                                              )
                                            )
                           )
                         )
                       )
                )
              )
            )
    ),

    ### ADD METADATA ----

    tabItem(tabName = "add_meta",
            fluidRow(
              column(9,DT::DTOutput("add_metaMerged")),
              column(3,
                     box(
                       width = 12,
                       h3(strong("Add metadata")),
                       br(),
                       fluidRow(column(12,
                                       div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                           align = "center",
                                           width=12,
                                           actionBttn(inputId = "applyMetadata", label = strong("Merge"),
                                                      width = 12, style = "pill", color = "primary",
                                                      icon = icon(name ="play", lib="glyphicon")))))#,
                     )
              )
            )
    ),

    ### FILTER TEXT ----

    tabItem(tabName = "filter_text",
            fluidRow(
              column(9,DT::DTOutput("FilterText")),
              column(3,
                     box(
                       width = 12,
                       h3(strong("Filter text")),
                       br(),
                       fluidRow(column(12,
                                       div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                           align = "center",
                                           width=12,
                                           actionBttn(inputId = "applyFilter", label = strong("Apply Filters"),
                                                      width = 12, style = "pill", color = "primary"))))#,
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
                       h3(strong("Tokenization & PoS Tagging"), align = "center"))
              )
            ),
            br(),
            br(),
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
                         div(h3(strong(em("Language model"))), style="margin-top:-57px"),
                         tags$hr(),
                         helpText(h5("Before beginning the annotation process (i.e., tokenization, tagging, and lemmatization), a language model must be downloaded."),
                                  h5("TALL utilizes pre-trained models provided by Universal Dependencies treebanks."),
                                  h5("When using a language model for the first time, it will be downloaded from UDT and saved on your computer. In this case, an active internet connection is required.")),
                         style="text-align: left; text-color: #989898",
                         br(),
                         uiOutput("optionsTokenization"),
                         fluidRow(column(6,
                                         div(
                                           align = "left",
                                           width=12,
                                           actionButton(inputId="tokPosRun",
                                                        label = strong("APPLY"),
                                                        icon = icon(name="play", lib = "font-awesome"),
                                                        style = style_bttn
                                           ))
                         ),
                         column(6,
                                div(
                                  title = t_save,
                                  div(align="center",
                                      do.call("downloadButton", c(save_bttn, list(
                                        outputId = "tokPosSave")
                                      ))
                                  ))
                         )
                         )
                       ),style="margin-top:40px"
                     )
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
                                   ),
                                   tabPanel("Term Custom List",
                                            shinycssloaders::withSpinner(DT::DTOutput("customListData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   )
                       )
                ),
                column(3,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Import Term Custom Lists"))), style="margin-top:-57px"),
                           tags$hr(),
                           fluidRow(column(12,
                                           fileInput("custom_lists", label=NULL,
                                                     multiple = TRUE,
                                                     accept = c(".csv",
                                                                ".xls",
                                                                ".xlsx"))
                           )),
                           fluidRow(column(6,
                                           div(
                                             align = "center",
                                             width=12,
                                             actionButton(inputId="custTermListRun",
                                                          label = strong("APPLY"),
                                                          icon = icon(name="play", lib = "font-awesome"),
                                                          style = style_bttn
                                             ))
                           ),
                           column(6,
                                  div(
                                    title = t_save,
                                    div(align="center",
                                        do.call("downloadButton", c(save_bttn, list(
                                          outputId = "custTermSave")

                                        ))
                                    )
                                  )
                           )
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
                column(9,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Multi-Word List",
                                            shinycssloaders::withSpinner(DT::DTOutput("multiwordList"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   ),
                                   tabPanel("Annotated Text including Multi-Words",
                                            shinycssloaders::withSpinner(DT::DTOutput("multiwordData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   )
                       )
                ),
                column(3,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Options: "))), style="margin-top:-57px"),
                           tags$hr(),
                           fluidRow(column(6,
                                           selectInput("term",
                                                       "Terms:",
                                                       choices = c("Tokens" = "token",
                                                                   "Lemmas" = "lemma"),
                                                       selected = "lemma")),
                                    column(6,
                                           selectInput("group",
                                                       "Group by:",
                                                       choices = c("Docs" = "doc_id",
                                                                   "Sentences" = "sentence_id"),
                                                       selected = "doc_id"))),
                           fluidRow(
                             column(6,
                                    numericInput(inputId = "ngram_max",
                                                 label = "Ngrams",
                                                 min = 2,
                                                 max = 10,
                                                 value = 4,
                                                 step = 1)),
                             column(6,
                                    numericInput(inputId = "rake.min",
                                                 label = "Rake Min",
                                                 min = 0,
                                                 max = Inf,
                                                 value = 2,
                                                 step = 0.1))
                           ),
                           uiOutput("multiwordPosSel"),
                           fluidRow(column(6,
                                           div(
                                             align = "center",
                                             width=12,
                                             actionButton(inputId="multiwordCreatRun",
                                                          label = strong("APPLY"),
                                                          icon = icon(name="play", lib = "font-awesome"),
                                                          style = style_bttn
                                             ))
                           ),
                           column(6,
                                  div(
                                    title = t_save,
                                    div(align="center",
                                        do.call("downloadButton", c(save_bttn, list(
                                          outputId = "multiwordCreatSave")

                                        ))
                                    )
                                  )
                           )
                           )
                         ), style="margin-top:40px")

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
                column(9,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Annotated Text",
                                            shinycssloaders::withSpinner(DT::DTOutput("posTagSelectData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   )
                       )
                ),
                column(3,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Select PoS Tag:"))), style="margin-top:-57px"),
                           tags$hr(),
                           fluidRow(column(12,
                                           uiOutput("posTagLists")
                           )),
                           fluidRow(column(6,
                                           div(
                                             align = "center",
                                             width=12,
                                             actionButton(inputId="posTagSelectRun",
                                                          label = strong("APPLY"),
                                                          icon = icon(name="play", lib = "font-awesome"),
                                                          style = style_bttn
                                             ))
                           ),
                           column(6,
                                  div(
                                    title = t_save,
                                    div(align="center",
                                        do.call("downloadButton", c(save_bttn, list(
                                          outputId = "posTagSelectSave")

                                        ))
                                    )
                                  )
                           )
                           )
                         ), style="margin-top:40px"
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
                                       column(3,
                                              valueBoxOutput("nDoc", width = "33vh"),
                                              valueBoxOutput("avgDocLengthChar", width = "33vh"),
                                              valueBoxOutput("avgDocLengthTokens", width = "33vh")),
                                       column(3,
                                              valueBoxOutput("nSentences", width = "33vh"),
                                              valueBoxOutput("avgSentLengthChar", width = "33vh"),
                                              valueBoxOutput("avgSentLengthTokens", width = "33vh")),
                                       column(3,
                                              valueBoxOutput("nDictionary", width = "33vh"),
                                              valueBoxOutput("nTokens", width = "33vh"),
                                              valueBoxOutput("nLemmas", width = "33vh")),
                                       column(3,
                                              valueBoxOutput("TTR", width = "33vh"),
                                              valueBoxOutput("hapax", width = "33vh"),
                                              valueBoxOutput("guiraud", width = "33vh")),
                                     )
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "overviewData", width = 700))
                                     ),
                            tabPanel("WordCloud",
                                     wordcloud2::wordcloud2Output("wordcloudPlot", height = "75vh"), width = 700)
                            ), align ="center"
              )
            )
    ),

    ### WORDS ----

    ## Frequency List----

    ### NOUN ----
    tabItem(tabName = "w_noun",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("NOUN Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "nounApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("nounN",
                                          label=("Number of NOUN"),
                                          value = 20),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = FALSE, animate = TRUE,
                             tooltip = tooltipOptions(title = "Options"),
                             #style = "material-circle"
                             )

                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "nounExport")
                         )),
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "nounReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "nounPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("nounTable", width = 700),
                                                                  color = getOption("spinner.color", default = "#4F7942")),
                                     align="center"
                            )
                )
              )
            )
    ),

    ### PROPN ----
    tabItem(tabName = "w_propn",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("PROPN Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "propnApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("propnN",
                                          label=("Number of PROPN"),
                                          value = 20),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = FALSE, animate = TRUE,
                             tooltip = tooltipOptions(title = "Options"),
                             #style = "material-circle"
                           )

                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "propnExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "propnReport")
                         ))
                  )),

              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "propnPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("propnTable", width = 700),
                                                                  color = getOption("spinner.color", default = "#4F7942")),
                                     align="center"
                            )
                )
              )
            )
    ),

    ### ADJ ----
    tabItem(tabName = "w_adj",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("ADJ Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "adjApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("adjN",
                                          label=("Number of ADJ"),
                                          value = 20),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = FALSE, animate = TRUE,
                             tooltip = tooltipOptions(title = "Options"),
                             #style = "material-circle"
                           )

                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "adjExport")
                         )),

                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "adjReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "adjPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("adjTable", width = 700),
                                                                  color = getOption("spinner.color", default = "#4F7942")),
                                     align="center"
                            )
                )
              )
            )
    ),

    ### VERB ----
    tabItem(tabName = "w_verb",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("VERB Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "verbApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("verbN",
                                          label=("Number of VERB"),
                                          value = 20),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = FALSE, animate = TRUE,
                             tooltip = tooltipOptions(title = "Options"),
                             #style = "material-circle"
                           )

                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "verbExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "verbReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "verbPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("verbTable", width = 700),
                                                                  color = getOption("spinner.color", default = "#4F7942")),
                                     align="center"
                            )
                )
              )
            )
    ),

    ### OTHER ----
    tabItem(tabName = "w_other",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("OTHER Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "otherApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("otherN",
                                          label=("Number of OTHER"),
                                          value = 20),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = FALSE, animate = TRUE,
                             tooltip = tooltipOptions(title = "Options"),
                             #style = "material-circle"
                           )

                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "otherExport")
                         )),

                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "otherReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "otherPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("otherTable", width = 700),
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
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             numericInput("posN",
                                          label=("Number of PoS"),
                                          value = 20),
                             width = "220px", icon = icon("cog", lib="glyphicon"),
                             right = FALSE, animate = TRUE,
                             tooltip = tooltipOptions(title = "Options"),
                             #style = "material-circle"
                           )

                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "posExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "posReport")
                         ))
                  ))

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

    ### Clustering----
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
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "w_clusteringExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "w_clusteringReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "w_clusteringPlot", height = "75vh",width ="98.9%"),
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
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "caExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "caReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "caPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("caTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Network ----

    tabItem(tabName = "w_network",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Network"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "w_networkApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "w_networkExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "w_networkReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "w_networkPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("w_networkTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### DOCUMENTS ----

    ### Topic Modeling ----

    tabItem(tabName = "d_topicMod",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Topic Modeling"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "d_topicModApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "d_topicModExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_topicModReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "d_topicModPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("d_topicModTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Clustering ----

    tabItem(tabName = "d_clustering",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Clustering"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "d_clusteringApply")
                         ))
                  )),

                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           )
                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "d_clusteringExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_clusteringReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "d_clusteringPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("d_clusteringTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Network ----

    tabItem(tabName = "d_network",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Network"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "d_networkApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           )
                ),
                style = style_opt
                ),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "d_networkExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_networkReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "d_networkPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("d_networkTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Summarization ----

    tabItem(tabName = "d_summarization",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Summarization"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "d_summarizationApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "d_summarizationExport")
                         ))

                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_summarizationReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "d_summarizationPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("d_summarizationTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
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
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "d_polDetExport")
                         ))
                  )),

                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "d_polDetReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "d_polDetPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("d_polDetTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### GROUPS ----

    ### Topic Modeling ----

    tabItem(tabName = "g_topicMod",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Topic Modeling"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "g_topicModApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "g_topicModExport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "g_topicModReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "g_topicModPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("g_topicModTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Clustering ----

    tabItem(tabName = "g_clustering",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Clustering"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "g_clusteringApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "g_clusteringExport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "g_clusteringReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "g_clusteringPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("g_clusteringTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Network ----

    tabItem(tabName = "g_network",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Network"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "g_networkApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "g_networkExport")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "g_networkReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "g_networkPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("g_networkTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Summarization ----

    tabItem(tabName = "g_summarization",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Summarization"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "g_summarizationApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "g_summarizationExport")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "g_summarizationReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "g_summarizationPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("g_summarizationTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Polarity detection ----

    tabItem(tabName = "g_polDet",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Polarity Detection"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "g_polDetApply")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             br(),
                             ### elenco opzioni (bottono, input, ecc)

                             right = TRUE, animate = TRUE, #circle = TRUE,
                             #style = "gradient",
                             #style = "unite",
                             tooltip = tooltipOptions(title = "Options"),
                             color = "default",
                             icon = icon("cog", lib="glyphicon")#,
                             #width = "200px"
                           ),
                ),
                style = style_opt
                ),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "g_polDetExport")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "g_polDetReport")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "g_polDetPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("g_polDetTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
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
                           uiOutput('reportSheets'),
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
                           downloadBttn(
                             outputId="report.save",
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
              )
            )
    )

  )) # END DASHBOARDBODY


## UserInterface ####
ui <- dashboardPage(header, sidebar, body)

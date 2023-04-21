# UI
#  Function source ----
source("libraries.R", local=TRUE)
source("tallFunctions.R", local=TRUE)
libraries()

## button style and contents

style_bttn <- "border-radius: 20px; border-width: 1px; font-size: 15px; text-align: center; color: #ffff; padding-left: 7px; padding-right: 20px"
style_opt <-  "border-radius: 20px; border-width: 1px; font-size: 15px; margin-top: 15px" # (option button)
style_start <-  "border-radius: 15px; border-width: 3px; font-size: 15px; width:100% " # (start button)
#style_bttn <- "border-radius: 15px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (action buttons)
t_report  <-  "Add Results to the Report"
t_export  <-  "Export Plot as PNG"
t_run <- "Run the Analysis"
t_save <- "Save the Analysis"

run_bttn <- list(
  label = NULL,
  style ="display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name ="play", lib="glyphicon")
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


## HEADER ----

title_tall <- tags$link(tags$a(href = 'https://www.unina.it/',target="_blank",
                               #tags$img(src="unina_logo.png", height = '30',width='30')
), strong("TALL"))

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
  tags$style(".glyphicon-download-alt {color:#ffffff; font-size: 18px; align: center; margin-left: -3.5px}"),
  tags$style(".glyphicon-play {color:#ffffff; font-size: 18px; align: center;margin-left: -0.5px}"),
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
  ),

  tabItems(

    ### TALL PAGE ----
    tabItem(tabName = "tall",
            fluidRow(
              h1(HTML("TA<i>ll</i>"), align="center", style = "font-family: 'Times New Roman'; font-size: 70px;"),
              br(),
              h3(("Text analysis for All"), align="center", style = "font-family: 'Times New Roman';"),
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
                                                 "excel"="xlsx",
                                                 "pdf"="pdf"),
                                               tags$style("height: 50px")
                                             )
                             ),
                             column(6,
                                    selectizeInput(
                                      'line_sep', label="CSV Separator",choices = c(
                                        " , "=",",
                                        " ; "=";"),
                                      tags$style("height: 50px")
                                    )
                             )
                             ),
                             uiOutput("file_raw"),
                             uiOutput(outputId = "infoImport"),
                           ),
                           conditionalPanel(
                             condition="input.load=='demo'",
                             selectInput("demo_file",
                                         label="Select sample texts",
                                            choices=c(
                                              "Bibliometrix publicationts" = "bibliometrix",
                                              "BBC short entertainment news" = "bbc"
                                            ),
                                            selected = "bibliometrix"
                                         ),
                             conditionalPanel(
                               condition = "input.demo_file=='bibliometrix'",
                               helpText(h5("Bibliometrix sample description...."))
                             ),
                             conditionalPanel(
                               condition = "input.demo_file=='bbc'",
                               helpText(h5("BBC sample description...."))
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
                           conditionalPanel(condition = "input.load != 'null'",
                                                     div(
                                                       align = "center",
                                                       width=12,
                                                       actionButton(inputId="runImport",
                                                                    label = div(icon(name="play",lib = "glyphicon"),strong("START")),
                                                                    icon = NULL,
                                                                    style = "border-radius: 20px; border-width: 1px;
                                                                    font-size: 17px; color: #ffff;")
                                            )
                           ),
                           conditionalPanel(condition="input.load != 'null'",
                                            tags$hr(),
                                                     div(
                                                       align = "center",
                                                       width=12,
                                                       downloadButton(outputId="collection.save",
                                                                      label = strong("Export Raw Texts in Excel"),
                                                                      icon = NULL,
                                                                      style ="border-radius: 15px; border-width: 1px; font-size: 15px;
                                                                    text-align: center; color: #ffff; "

                                              )
                                            )
                           )
                         )
                       )
                )
              )
            )
    ),

    ### Split texts ----

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
                           selectInput(inputId="txSplitBy",
                                       label="Split texts by:",
                                       choices = c("a word at the beginning of a line" = "starting",
                                                   "a sequence of special characters" = "into"),
                                       selected = "starting"),
                           textInput(inputId="txSplitWord",
                                     label="Insert a word or a sequence of special chars (e.g. H1__)",
                                     value=NULL),
                           fluidRow(
                             column(6,
                                    div(align="center",
                                    title = t_run,
                                    do.call("actionButton", c(run_bttn, list(
                                      inputId = "splitTextRun")
                                    ))
                                    )
                             ),
                             column(6,
                                    div(align="center",
                                    title = t_save,
                                    do.call("downloadButton", c(list(
                                      label=NULL,
                                      style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                      icon = icon(name ="floppy-save", lib="glyphicon"),
                                      outputId = "splitTextSave")
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

    ### Random text selection ----

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
                           fluidRow(
                             column(6,
                                    div(
                                    numericInput("sampleSize",
                                                 "Sample Size",
                                                 value = 10,
                                                 min = 1,
                                                 step = 1
                                    )
                                    ,style="margin-top:-9px")
                                    ),
                             column(3,
                                    title = t_run,
                                    do.call("actionButton", c(run_bttn, list(
                                      inputId = "randomTextRun")
                                    ))
                             ),
                             column(3,
                                    title = t_run,
                                    do.call("actionButton", c(run_bttn, list(
                                      inputId = "randomTextBack")
                                    ))
                             )
                             # column(3,
                             #        title = t_save,
                             #        do.call("downloadButton", c(list(
                             #          label=NULL,
                             #          style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                             #          border: 1px; margin-top: 16px;",
                             #          icon = icon(name ="floppy-save", lib="glyphicon")
                             #        ), list(
                             #          outputId = "randomTextSave")
                             #        ))
                             # )


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
                           'doc_id' to identify the associated documents.")),
                           fileInput(
                             inputId="extInfoFile",
                             label=NULL,
                             multiple = FALSE,
                             accept = c(
                               ".xls",
                               ".xlsx"
                             ),
                             placeholder = "No file(s) selected"
                           ),
                           helpText(h5("You can download the list of IDs associated with the imported text files below.")),
                           fluidRow(
                             column(12,
                                    div(align="center",
                                        downloadButton(outputId="doc_idExport",
                                                       label = strong("Export doc_id list"),
                                                       icon = NULL,
                                                       style ="border-radius: 15px; border-width: 1px; font-size: 15px;
                                                                    text-align: center; color: #ffff; ")
                                    )
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(6,div(
                               title = t_run,
                               do.call("actionButton", c(run_bttn, list(
                                 inputId = "extInfoRun")
                               )), align="center"
                             )
                             ),
                             column(6,
                                    div(
                                      title = t_save,
                                      do.call("downloadButton", c(list(
                                        label=NULL,
                                        style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                        icon = icon(name ="floppy-save", lib="glyphicon")
                                      ), list(
                                        outputId = "extInfoSave")
                                      )),align="center"
                                    )
                             )
                           )
                         )

                       )
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
                         fluidRow(column(6,
                                         div(
                                         uiOutput("optionsTokenization"))
                                         ,style="margin-top:-9px"),
                                  column(3,
                                         title = t_run,
                                         do.call("actionButton", c(run_bttn, list(
                                           inputId = "tokPosRun")
                                         ))
                                  ),
                                  column(3,
                                         title = t_save,
                                         do.call("downloadButton", c(list(
                                           label=NULL,
                                           style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 15px;",
                                           icon = icon(name ="floppy-save", lib="glyphicon")
                                         ), list(
                                           outputId = "tokPosSave")
                                         ))
                                  ))
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
                                             align = "center",style="margin-top:-15px",
                                             width=12,
                                             do.call("actionButton", c(run_bttn, list(
                                               inputId = "custTermListRun")
                                             )))
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
                                             align = "center",style="margin-top:-15px",
                                             width=12,
                                             do.call("actionButton", c(run_bttn, list(
                                               inputId = "multiwordCreatRun")
                                             ))
                                             )

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
                           uiOutput("defineGroupsList"),
                           uiOutput(outputId = "infoGroups"),
                           hr(),
                           fluidRow(
                             column(6,
                                    div(align="center",
                                        title = t_run,
                                        do.call("actionButton", c(run_bttn, list(
                                          inputId = "defineGroupsRun")
                                        ))
                                    )
                             ),
                             column(6,
                                    div(align="center",
                                        title = t_save,
                                        do.call("downloadButton", c(list(
                                          label=NULL,
                                          style ="display:block; height: 37px; width: 37px; border-radius: 50%;
                                      border: 1px; margin-top: 16px;",
                                          icon = icon(name ="floppy-save", lib="glyphicon"),
                                          outputId = "defineGroupsSave")
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
                                       column(9,
                                              wordcloud2::wordcloud2Output("wordcloudPlot", height = "75vh")
                                              ),
                                     column(3,
                                            div(
                                              box(
                                                width = 12,
                                                fluidRow(
                                                  column(9,
                                                         div(
                                                           selectInput("termWC",
                                                                       label = "Dictionary by:",
                                                                       choices = c("Tokens"="token",
                                                                                   "Lemmas"="lemma"),
                                                                       selected = "token"), style="margin-top:-3px"
                                                         )
                                                  ),
                                                  column(3,
                                                         div(
                                                           align = "center",style="margin-top:15px",
                                                           width=12,
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
                            tabPanel("Dictionary",
                                     column(9,shinycssloaders::withSpinner(DT::DTOutput(outputId = "dictionaryData", width = 700))),
                                     column(3,
                                            div(
                                              box(
                                                width = 12,
                                                fluidRow(
                                                  column(9,
                                                         div(
                                                selectInput("termDict",
                                                            label = "Dictionary by:",
                                                            choices = c("Tokens"="token",
                                                                        "Lemmas"="lemma"),
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
                                     shinycssloaders::withSpinner(DT::DTOutput(outputId = "tfidfData", width = 700)))
                ), align="center"
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
                       h3(strong("Noun Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "nounApply")
                         ))
                  )),

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
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             numericInput("nounN",
                                          label=("Number of Nouns"),
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
                       h3(strong("Proper Noun Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "propnApply")
                         ))
                  ),
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
              div(column(1,
                         dropdown(
                           h4(strong("Options: ")),
                           hr(),
                           numericInput("propnN",
                                        label=("Number of Proper Nouns"),
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
                       h3(strong("Adjective Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "adjApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "adjExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "adjReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             numericInput("adjN",
                                          label=("Number of Adjectives"),
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
                       h3(strong("Verb Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "verbApply")
                         ))
                  )),
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
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             numericInput("verbN",
                                          label=("Number of Verbs"),
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

    ### MULTIWORD ----
    tabItem(tabName = "w_other",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Multi-Word Frequency List"), align = "center")),
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "otherApply")
                         ))
                  )),
                div(
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "otherExport")
                         ))
                  )),
                div(
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "otherReport")
                         ))
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
                             # uiOutput("otherFreq"),
                             numericInput("otherN",
                                          label=("Number of Multi-Words"),
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
                                 tabPanel("Annotated Text by Words",
                                          shinycssloaders::withSpinner(DT::DTOutput("wordsContData"),
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
                         searchInput(
                           inputId = "wordsContSearch",
                           label = "Search word(s) in text",
                           placeholder = "",
                           btnSearch = icon("magnifying-glass"),
                           #btnReset = icon("xmark"),
                           width = "100%"
                         )
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
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
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
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
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
                                                         "Terms:",
                                                         choices = c("Tokens" = "token",
                                                                     "Lemmas" = "lemma"),
                                                         selected = "lemma"))),
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
                                          label = "Docs/Groups",
                                          value = 0,
                                          min = 0,
                                          step = 1),
                             fluidRow(
                               column(6,
                                      numericInput("labelsizeCA",
                                                   label = "Label size",
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
                            tabPanel("Coordinates",
                                     shinycssloaders::withSpinner(DT::DTOutput("caCoordTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Contributes",
                                     shinycssloaders::withSpinner(DT::DTOutput("caContribTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Cosines Squared",
                                     shinycssloaders::withSpinner(DT::DTOutput("caCosineTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Singular Values",
                                     shinycssloaders::withSpinner(DT::DTOutput("caSingularValueTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),


    ### Network ----

    ## WORD CO-OCCURENCE ----

    tabItem(tabName = "w_networkCooc",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Word co-occurence"), align = "center")),
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
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "w_networkCoocExport")
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
                             checkboxGroupInput(
                               inputId = "groupNet",
                               label = "Groups",
                               choices = c("Docs"="doc_id",
                                           "Sentences"="sentence_id"),
                               selected = c("doc_id", "sentence_id"),
                               inline = TRUE
                             ),
                             materialSwitch(
                               inputId = "interLinks",
                               label = "Inter-group links",
                               value = FALSE,
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
                                        numericInput("minEdges",
                                                     label = "Top Link (%)",
                                                     value = 10,
                                                     min = 0,
                                                     max = 100,
                                                     step = 1),
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
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "w_networkGrakoExport")
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
                               value = FALSE,
                               status = "success"
                             ),
                             fluidRow(
                               column(6,
                                      numericInput("grakoNMax",
                                                   label = "Words",
                                                   value = 30,
                                                   min = 2,
                                                   step=1),
                                      numericInput("grakoMinEdges",
                                                   label = "Top Link (%)",
                                                   value = 10,
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
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "d_tm_selectExport")
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
                             label = "Groups",
                             choices = c("Docs"="doc_id",
                                         "Sentences"="sentence_id"),
                             selected = "doc_id"
                           ),
                           selectInput("metric", "Metric for model tuning",
                                       choices = c(
                                         "CaoJuan-2009"="CaoJuan2009",
                                         "Deveaud-2014"="Deveaud2014",
                                         "Arun-2010"="Arun2010",
                                         "Griffiths-2004"="Griffiths2004"
                                       ),
                                       selected = "CaoJuan2009"
                           ),
                           fluidRow(column(6,
                           selectInput("termTm", "Terms:",
                                       choices = c(
                                         "Tokens"="token",
                                         "Lemmas"="lemma"),
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
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "d_tm_estimExport")
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
                               label = "Groups",
                               choices = c("Docs"="doc_id",
                                           "Sentences"="sentence_id"),
                               selected = "doc_id"),
                             fluidRow(
                               column(6,
                                      selectInput("termTmEstim", "Terms:",
                                                  choices = c(
                                                    "Tokens"="token",
                                                    "Lemmas"="lemma"),
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
                  )),
                div(column(1,
                           dropdown(
                             h4(strong("Options: ")),
                             hr(),
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
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "d_polDensPlot", width ="98.9%"),
                                                                           color = getOption("spinner.color", default = "#4F7942")),#),
                                              shinycssloaders::withSpinner(plotlyOutput(outputId = "d_polBoxPlot", width ="98.9%"),
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
                div(
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "d_summarizationApply")
                         ))
                  )),
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
                                   tabPanel("Document",
                                            shinycssloaders::withSpinner(DT::DTOutput("documentData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                            ),
                                   tabPanel("Sentence Ranking",
                                            shinycssloaders::withSpinner(DT::DTOutput("RelSentData"),
                                                                         color = getOption("spinner.color", default = "#4F7942"))
                                   )

                       )
                ),
                column(3,
                       div(
                         box(
                           width = 12,
                           div(h3(strong(em("Sentences extraction"))), style="margin-top:-57px"),
                           tags$hr(),
                           fluidRow(column(6,
                                           uiOutput("optionsSummarization")),
                                    column(6,
                                           numericInput("nTopSent",
                                                        label = "N of Sentences",
                                                        value = 5,
                                                        min = 1,
                                                        step = 1
                                                        ))
                                    )
                         )
                       ),style="margin-top:40px"
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

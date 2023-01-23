# UI
#  Function source ----
source("libraries.R", local=TRUE)
source("tallFunctions.R", local=TRUE)
libraries()

## button style and contents

#style_opt <- "height: 45px; width: 45px; border-radius: 50%; border: 3px; margin-top: 15px"
style_opt <-  "border-radius: 15px; border-width: 3px; font-size: 15px; margin-top: 15px;" # (option button)
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
  style ="display:block; height: 45px; width: 45px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name ="plus", lib="glyphicon")
)
export_bttn <- list(
  label=NULL,
  style ="display:block; height: 45px; width: 45px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon(name ="download-alt", lib="glyphicon")
)
save_bttn <- list(
  label=NULL,
  style ="display:block; height: 45px; width: 45px; border-radius: 50%; border: 3px; ",
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
              menuItem("Data", tabName = "data", icon = fa_i(name = "file-import"),
                       menuSubItem("Import texts", tabName = "import_tx", icon = icon("chevron-right")),
                       menuSubItem("Add metadata", tabName = "add_meta", icon = icon("chevron-right")),
                       menuSubItem("Filter text", tabName = "filter_text", icon = icon("chevron-right"))),
              #menuItem("Filters", tabName = "filters", icon = fa_i(name ="filter")),
              menuItem("Vocabulary Construction", tabName = "voc_con", icon = icon("indent-right", lib = "glyphicon"),
                       menuSubItem("Tokenization & Cleaning", tabName = "tok_cl", icon = icon("chevron-right")),
                       menuSubItem("Normalization", tabName = "normalization", icon = icon("chevron-right"))),
              menuItem("Vocabulary Reduction", tabName = "vocab_red", icon = icon("font", lib="glyphicon"),
                       menuSubItem("Filtering", tabName = "filtering", icon = icon("chevron-right")),
                       menuSubItem("Morphological", tabName = "morph", icon = icon("chevron-right")),
                       menuSubItem("Lexical", tabName = "lexical", icon = icon("chevron-right"))),
              menuItem("Explore", tabName = "explore", icon = icon("search", lib = "glyphicon"),
                       menuSubItem("Overview", tabName = "overview", icon = icon("chevron-right")),
                       menuSubItem("Peculiar language analysis", tabName = "pec_lan", icon = icon("chevron-right")),
                       menuSubItem("Polarity detection", tabName = "polarity_det", icon = icon("chevron-right")),
                       menuSubItem("Keywords in Context", tabName = "keywords_context", icon = icon("chevron-right"))),
              menuItem("Organize",tabName = "organize", icon = icon("list-alt", lib = "glyphicon"),
                       "Unsupervised approaches",
                       menuSubItem("Clustering", tabName = "clustering", icon = icon("chevron-right")),
                       menuSubItem("Factorial analysis", tabName = "factorial", icon = icon("chevron-right")),
                       menuSubItem("Network analysis", tabName = "network", icon = icon("chevron-right")),
                       menuSubItem("Topic modeling", tabName = "topic_model", icon = icon("chevron-right")),
                       "Supervised",
                       menuSubItem("Classification", tabName = "classification", icon = icon("chevron-right"))),
              menuItem("Summarize",tabName = "synth", icon = icon("comment", lib = "glyphicon"),
                       menuSubItem("Extractive Summarization", tabName = "extractive", icon = icon("chevron-right")),
                       menuSubItem("Abstractive Summarization", tabName = "abstractive", icon = icon("chevron-right"))
              ),
              menuItem("Report",tabName = "report", icon = fa_i(name ="list-alt")
              )
  )
)

## BODY ----

body <- dashboardBody(
  #shinyDashboardThemes(theme = "blue_gradient"),
  customTheme(),
  tags$style(".glyphicon-download-alt {color:#ffffff; font-size: 24px; align: center; margin-left: -2.5px}"),
  tags$style(".glyphicon-play {color:#ffffff; font-size: 24px; align: center}"),
  tags$style(".glyphicon-plus {color:#ffffff; font-size: 24px;align: center; margin-left: -0.5px}"),
  tags$style(".glyphicon-cog {color:#ffffff; font-size: 28px; margin-top: 2px; margin-left: -2px}"),
  tags$style(".glyphicon-floppy-save {color:#ffffff; font-size: 25px; margin-top: 4px; }"),

  tags$style(".glyphicon-folder-open {color:#ffffff; font-size: 17px}"),

# tags$style(
#   '.bootstrap-switch .bootstrap-switch-handle-on,
#   .bootstrap-switch .bootstrap-switch-handle-off,
#   .bootstrap-switch .bootstrap-switch-label {
#     display: inline-block;
#     vertical-align: baseline; width:100%;
#   }'),

  tags$head(
    tags$style(".fa-envelope {color:#FF0000; font-size: 20px}"),
    tags$style(".fa-envelope-open {font-size: 20px}"),
    tags$style(".fa-cube {font-size: 20px}"),
    tags$style(".fa-question {font-size: 20px}"),
    tags$style(".fa-comment-dollar {font-size: 20px}"),
    tags$style(".fa-bars {font-size: 20px}"),
    tags$style(".sidebar-toggle {font-size: 15px}"),
    tags$script(
      'var dimension = [0, 0];
              $(document).on("shiny:connected", function(e) {
                  dimension[0] = window.innerWidth;
                  dimension[1] = window.innerHeight;
                  Shiny.onInputChange("dimension", dimension);
              });
              $(window).resize(function(e) {
                  dimension[0] = window.innerWidth;
                  dimension[1] = window.innerHeight;
                  Shiny.onInputChange("dimension", dimension);
              });
              $(document).ready(function(){
                  $("a[data-toggle=tab]").on("show.bs.tab", function(e){
                    Shiny.setInputValue("activeTab", $(this).attr("data-value"));
                   });
            });
      '
    )),


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
                column(8,
                       shinycssloaders::withSpinner(DT::DTOutput("dataImported"),color = getOption("spinner.color", default = "#4F7942"))
                ),
                column(4,
                       fluidRow(
                         box(
                           width = 12,
                           h3(strong("Import texts")),
                           selectInput("load", "Please, choose what to do",
                                       choices = c(
                                         " "= "null",
                                         "Load a raw files"="import",
                                         "Load a Tall structured file"="load_ex",
                                         "Use a sample collection"="demo"
                                       ),
                                       selected = "null"
                           ),
                           conditionalPanel(
                             condition="input.load == 'import'",
                             h5(strong('Select the folder that contains the files')),
                             fluidRow(
                               column(3,
                                      div(
                                        directoryInput('directory', label = NULL, value = NULL),
                                        style="margin-top: 5px;"
                                      )),
                               column(5,
                                      div(
                                        h6((htmlOutput("folder"))),
                                        style="margin-top: -5px;"
                                      ),
                               ),
                               column(4,
                                      (switchInput(
                                        inputId = "include_subfolder",
                                        label = "Include subfolders",
                                        labelWidth = "100px",
                                        onLabel = "YES",
                                        offLabel = "NO",
                                        size = "small",
                                        onStatus = "success",
                                        offStatus = "danger",
                                        width="100%",
                                        inline = T,
                                      )#, style= "margin-top: 3px; "
                                      )
                               )
                               ),
                             fluidRow(
                                      column(6,
                                             selectizeInput(
                                               'ext', label=NULL,choices = c(
                                                 "TXT"="txt",
                                                 "CSV"="csv",
                                                 "EXCEL"="xlsx"),
                                               options = list(
                                                 placeholder = 'File format',
                                                 onInitialize = I('function() { this.setValue(""); }')
                                               ), tags$style("height: 50px")
                                             )
                                      ),
                                      column(6,
                                             selectizeInput(
                                               'language', label=NULL,choices = c(
                                                 "ITALIAN"="it",
                                                 "ENGLISH"="en",
                                                 "FRENCH"="fr"),
                                               options = list(
                                                 placeholder = 'Language text',
                                                 onInitialize = I('function() { this.setValue(""); }')
                                               ), tags$style("height: 50px")
                                             )
                                      )
                               )
                           ),
                           conditionalPanel(
                             condition="input.load=='demo'",
                             helpText(h4("This is a sample collection ...."))
                           ),
                           conditionalPanel(
                             condition = "input.load == 'load_ex'",
                             conditionalPanel(
                               condition = "input.load == 'load_ex'",
                               helpText(em("Load a collection previously exported from Tall")
                               )),
                             fileInput(
                               "file1",
                               "Choose a file",
                               multiple = FALSE,
                               accept = c(
                                 ".csv",
                                 ".txt",
                                 ".ciw",
                                 ".bib",
                                 ".xlsx",
                                 ".zip",
                                 ".xls",
                                 ".rdata",
                                 ".rda",
                                 ".rds"
                               )
                             )
                           ),
                           conditionalPanel(condition = "input.load != 'null'",
                                            fluidRow(
                                              column(12,
                                                     div(
                                                       align = "center",
                                                       width=12,
                                                       actionButton(inputId="runImport",
                                                                    label = strong("Start"),
                                                                    icon = icon(name="play", lib="glyphicon"),
                                                                    style ="border-radius: 25px; border-width: 1px; font-size: 24px;
                                                                    text-align: center; color: #ffff; padding-left: 40px; padding-right: 40px"
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
                                                                    label = strong("Convert Raw Data in Excel"),
                                                                    #icon = icon(name="play", lib="glyphicon"),
                                                                    style ="border-radius: 15px; border-width: 1px; font-size: 20px;
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
                       # h5(" "),
                       # box(h6(htmlOutput("textDim")),
                       #     width = "100%"),
                       # br(),
                       # uiOutput("selectLA"),
                       # uiOutput("sliderPY"),
                       # uiOutput("selectType"),
                       # uiOutput("sliderTCpY"),
                       # selectInput("bradfordSources",
                       #             label = "Source by Bradford Law Zones",
                       #             choices = c("Core Sources"="core",
                       #                         "Core + Zone 2 Sources"="zone2",
                       #                         "All Sources"="all"),
                       #             selected = "all")
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
                       # h5(" "),
                       # box(h6(htmlOutput("textDim")),
                       #     width = "100%"),
                       # br(),
                       # uiOutput("selectLA"),
                       # uiOutput("sliderPY"),
                       # uiOutput("selectType"),
                       # uiOutput("sliderTCpY"),
                       # selectInput("bradfordSources",
                       #             label = "Source by Bradford Law Zones",
                       #             choices = c("Core Sources"="core",
                       #                         "Core + Zone 2 Sources"="zone2",
                       #                         "All Sources"="all"),
                       #             selected = "all")
                     )
              )
            )
    ),

    ### VOCABULARY CONSTRUCTION ----

    ### Tokenization & Cleaning ----
    tabItem(tabName = "tok_cl",
            fluidPage(
              fluidRow(
                column(7,
                       h3(strong("Tokenization & Cleaning"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "tok_clApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "tok_clReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "tok_clExport")
                         )),
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
                  title = t_save,
                  column(1,
                         do.call("downloadButton", c(save_bttn, list(
                           outputId = "tok_clSave")
                         ))
                  ))
                ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "tok_clPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("tok_clTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )

    ),

    ### Normalization----
    tabItem(tabName = "normalization",
            fluidPage(
              fluidRow(
                column(7,
                       h3(strong("Normalization"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "normalizationApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "normalizationReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "normalizationExport")
                         )),
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
                  title = t_save,
                  column(1,
                         do.call("downloadButton", c(save_bttn, list(
                           outputId = "normalizationSave")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "normalizationPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("normalizationTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### VOCABULARY REDUCTION ----

    ## Filtering----
    tabItem(tabName = "filtering",
            fluidRow(
              column(9,DT::DTOutput("Filtered")),
              column(3,
                     box(
                       width = 12,
                       h3(strong("Filtering")),
                       br(),
                       fluidRow(column(12,
                                       div(style ="border-radius: 10px; border-width: 3px; font-size: 15px;",
                                           align = "center",
                                           width=12,
                                           actionBttn(inputId = "applyFiltering", label = strong("Apply Filters"),
                                                      width = 12, style = "pill", color = "primary"))))#,
                       # h5(" "),
                       # box(h6(htmlOutput("textDim")),
                       #     width = "100%"),
                       # br(),
                       # uiOutput("selectLA"),
                       # uiOutput("sliderPY"),
                       # uiOutput("selectType"),
                       # uiOutput("sliderTCpY"),
                       # selectInput("bradfordSources",
                       #             label = "Source by Bradford Law Zones",
                       #             choices = c("Core Sources"="core",
                       #                         "Core + Zone 2 Sources"="zone2",
                       #                         "All Sources"="all"),
                       #             selected = "all")
                     )
              )
            )
    ),

    ### Morphological----
    tabItem(tabName = "morph",
            fluidPage(
              fluidRow(
                column(7,
                       h3(strong("Morphological"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "morphologicalApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "morphologicalReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "morphologicalExport")
                         )),

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
                  title = t_save,
                  column(1,
                         do.call("downloadButton", c(save_bttn, list(
                           outputId = "morphologicalSave")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "morphologicalPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("morphologicalTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),
    ### Lexical ----
    tabItem(tabName = "lexical",
            fluidPage(
              fluidRow(
                column(7,
                       h3(strong("Lexical"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "lexicalApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "lexicalReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "lexicalExport")
                         )),

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
                  title = t_save,
                  column(1,
                         do.call("downloadButton", c(save_bttn, list(
                           outputId = "lexicalSave")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "lexicalPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("lexicalTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### EXPLORE ----

    ### Overview ----
    tabItem(tabName = "overview",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Overview"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "overviewApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "overviewReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "overviewExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(wordcloud2Output(outputId = "overviewPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("overviewTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )

    ),
    ### Term Frequency ----
    tabItem(tabName = "termfrequency",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Term Frequency"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "termfrequencyApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "termfrequencyReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "termfrequencyExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "termfrequencyPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("termfrequencyTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),
    ### Polarity detection ----
    tabItem(tabName = "polarity_det",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Polarity Detection"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "polarity_detApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "polarity_detReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "polarity_detExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "polarity_detPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("polarity_detTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),
    ### Keywords in context ----
    tabItem(tabName = "keywords_context",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Keywords Context"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "keywords_contextApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "keywords_contextReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "keywords_contextExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "keywords_contextPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("keywords_contextTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### ORGANIZE ----

    ## Unsupervised
    ### Clustering ----
    tabItem(tabName = "clustering",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Clustering"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "clusteringApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "clusteringReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "clusteringExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "clusteringPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("clusteringTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),
    ### Factorial analysis ----
    tabItem(tabName = "factorial",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Factorial analysis"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "factorialApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "factorialReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "factorialExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "factorialPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("factorialTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),
    ### Network analysis ----
    tabItem(tabName = "network",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Network analysis"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "networkApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "networkReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "networkExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "networkPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("networkTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),
    ### Topic Modeling ----
    tabItem(tabName = "topic_model",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Topic Model"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "topic_modelApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "topic_modelReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "topic_modelExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "topic_modelPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("topic_modelTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ##Supervised
    ### Classification ----
    tabItem(tabName = "classification",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Classification"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "classificationApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "classificationReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "classificationExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "classificationPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("classificationTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### SYNTHETIZE ----

    ### Extractive Summarization ----
    tabItem(tabName = "extractive",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Extractive Summarization"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "extractiveApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "extractiveReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "extractiveExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "extractivePlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("extractiveTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),
    ### Abstractive Summarization ----
    tabItem(tabName = "abstractive",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Abstractive Summarization"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "abstractiveApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "abstractiveReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "abstractiveExport")
                         )),

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
                )
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "abstractivePlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("abstractiveTable"),
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
    )

  )) # END DASHBOARDBODY


## UserInterface ####
ui <- dashboardPage(header, sidebar, body)

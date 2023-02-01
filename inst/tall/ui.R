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
  style ="display:block; height: 45px; width: 45px; border-radius: 50%; border: 3px; margin-top: 15px",
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
              menuItem("Pre-processing", tabName = "prePro", icon = icon("indent-right", lib = "glyphicon"),
                       menuSubItem(". Tokenization & PoS Tagging", tabName = "tokPos", icon = fa_i(name ="1")),
                       menuSubItem(". PoS Tag Selection", tabName = "posTagSelect", icon = fa_i(name ="2"))),
              menuItem("Overview", tabName = "overview", icon = icon("search", lib="glyphicon")),
              menuItem("Words", tabName = "words", icon = icon("font", lib = "glyphicon"),
                       menuSubItem("Frequency List", tabName = "freqList", icon = icon("chevron-right")),
                       menuSubItem("Clustering", tabName = "w_clustering", icon = icon("chevron-right")),
                       menuSubItem("Correspondence Analysis", tabName = "ca", icon = icon("chevron-right")),
                       menuSubItem("Network", tabName = "w_network", icon = icon("chevron-right"))),
              menuItem("Documents",tabName = "documents", icon = fa_i(name="layer-group"),
                       menuSubItem("Topic Modeling", tabName = "d_topicMod", icon = icon("chevron-right")),
                       menuSubItem("Clustering", tabName = "d_clustering", icon = icon("chevron-right")),
                       menuSubItem("Network", tabName = "d_network", icon = icon("chevron-right")),
                       menuSubItem("Summarization", tabName = "d_summarization", icon = icon("chevron-right")),
                       menuSubItem("Polarity Detection", tabName = "d_polDet", icon = icon("chevron-right"))),
              menuItem("Groups",tabName = "groups", icon = fa_i(name ="spinner"),
                       menuSubItem("Topic Modeling", tabName = "g_topicMod", icon = icon("chevron-right")),
                       menuSubItem("Clustering", tabName = "g_clustering", icon = icon("chevron-right")),
                       menuSubItem("Network", tabName = "g_network", icon = icon("chevron-right")),
                       menuSubItem("Summarization", tabName = "g_summarization", icon = icon("chevron-right")),
                       menuSubItem("Polarity Detection", tabName = "g_polDet", icon = icon("chevron-right"))),
              menuItem("Report",tabName = "report", icon = fa_i(name ="list-alt")),
              menuItem("Settings",tabName = "settings", icon = fa_i(name ="sliders")
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

    ### PRE-PROCESSING ----

    ## Tokenization & PoS Tagging -----

    tabItem(tabName = "tokPos",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("Tokenization & PoS Tagging"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "tokPosApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "tokPosReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "tokPosExport")
                         )),
                  )),
                div(#style=style_bttn,
                  title = t_save,
                  column(1,
                         do.call("downloadButton", c(save_bttn, list(
                           outputId = "tokPosSave")
                         ))
                  ))
                ),
              br(),
              br(),
              fluidRow(column(8,DT::DTOutput("tokPosTag")),
                       column(4,
                              box(
                                width = 12,
                                #h3(strong("Pre-processing")),
                                br(),
                                fluidRow(column(12,
                                                div(
                                                  align = "center",
                                                  width=12,
                                                  actionButton(inputId="tokPosRun",
                                                               label = strong("Apply"),
                                                               icon = icon(name="play", lib="glyphicon"),
                                                               style ="border-radius: 25px; border-width: 1px; font-size: 24px;
                                                                    text-align: center; color: #ffff; padding-left: 40px; padding-right: 40px"
                                                  ))
                                                ))#,
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
            )

    ),

    ## PoS Tag Selection -----

    tabItem(tabName = "posTagSelect",
            fluidPage(
              fluidRow(
                column(8,
                       h3(strong("PoS Tag Selection"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "posTagSelectApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "posTagSelectReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "posTagSelectExport")
                         )),
                  )),
                div(#style=style_bttn,
                  title = t_save,
                  column(1,
                         do.call("downloadButton", c(save_bttn, list(
                           outputId = "posTagSelectSave")
                         ))
                  ))
              ),
              br(),
              br(),
              fluidRow(column(8,DT::DTOutput("posTagSelectTab")),
                       column(4,
                              box(
                                width = 12,
                                #h3(strong("Pre-processing")),
                                br(),
                                fluidRow(column(12,
                                                div(
                                                  align = "center",
                                                  width=12,
                                                  actionButton(inputId="posTagSelectRun",
                                                               label = strong("Apply"),
                                                               icon = icon(name="play", lib="glyphicon"),
                                                               style ="border-radius: 25px; border-width: 1px; font-size: 24px;
                                                                    text-align: center; color: #ffff; padding-left: 40px; padding-right: 40px"
                                                  ))
                                ))#,
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
            )

    ),

    ### OVERVIEW ----
    tabItem(tabName = "overview",
            fluidPage(
              fluidRow(
                column(7,
                       h3(strong("Overview"), align = "center")),
                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "overviewReport")
                         ))
                  )),

                )
              ##VALUES BOXS HERE!
              )
    ),

    ### WORDS ----

    ## Frequency List----
    tabItem(tabName = "freqList",
            fluidPage(
              fluidRow(
                column(7,
                       h3(strong("Frequency List"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "freqListApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "freqListReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "freqListExport")
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
                           outputId = "freqListSave")
                         ))
                  ))
              ),
              fluidRow(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     shinycssloaders::withSpinner(plotlyOutput(outputId = "freqListPlot", height = "75vh",width ="98.9%"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            ),
                            tabPanel("Table",
                                     shinycssloaders::withSpinner(DT::DTOutput("freqListTable"),
                                                                  color = getOption("spinner.color", default = "#4F7942"))
                            )
                )
              )
            )
    ),

    ### Clustering----
    tabItem(tabName = "w_clustering",
            fluidPage(
              fluidRow(
                column(7,
                       h3(strong("Clustering"), align = "center")),
                div(#style=style_bttn,
                  title = t_run,
                  column(1,
                         do.call("actionButton", c(run_bttn, list(
                           inputId = "w_clusteringApply")
                         ))
                  )),

                div(#style=style_bttn,
                  title = t_report,
                  column(1,
                         do.call("actionButton", c(report_bttn, list(
                           inputId = "w_clusteringReport")
                         ))
                  )),
                div(#style=style_bttn,
                  title = t_export,
                  column(1,
                         do.call("downloadButton", c(export_bttn, list(
                           outputId = "w_clusteringExport")
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
                           outputId = "w_clusteringSave")
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
                 column(7,
                        h3(strong("Correspondence Analysis"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "caApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "caReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "caExport")
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
                            outputId = "caSave")
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
                 column(7,
                        h3(strong("Network"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "w_networkApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "w_networkReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "w_networkExport")
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
                            outputId = "w_networkSave")
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
                 column(7,
                        h3(strong("Topic Modeling"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "d_topicModApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "d_topicModReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "d_topicModExport")
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
                            outputId = "d_topicModSave")
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
                 column(7,
                        h3(strong("Clustering"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "d_clusteringApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "d_clusteringReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "d_clusteringExport")
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
                            outputId = "d_clusteringSave")
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
                 column(7,
                        h3(strong("Network"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "d_networkApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "d_networkReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "d_networkExport")
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
                            outputId = "d_networkSave")
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
                 column(7,
                        h3(strong("Summarization"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "d_summarizationApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "d_summarizationReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "d_summarizationExport")
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
                            outputId = "d_summarizationSave")
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
                 column(7,
                        h3(strong("Polarity Detection"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "d_polDetApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "d_polDetReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "d_polDetExport")
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
                            outputId = "d_polDetSave")
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
                 column(7,
                        h3(strong("Topic Modeling"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "g_topicModApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "g_topicModReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "g_topicModExport")
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
                            outputId = "g_topicModSave")
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
                 column(7,
                        h3(strong("Clustering"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "g_clusteringApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "g_clusteringReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "g_clusteringExport")
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
                            outputId = "g_clusteringSave")
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
                 column(7,
                        h3(strong("Network"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "g_networkApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "g_networkReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "g_networkExport")
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
                            outputId = "g_networkSave")
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
                 column(7,
                        h3(strong("Summarization"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "g_summarizationApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "g_summarizationReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "g_summarizationExport")
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
                            outputId = "g_summarizationSave")
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
                 column(7,
                        h3(strong("Polarity Detection"), align = "center")),
                 div(#style=style_bttn,
                   title = t_run,
                   column(1,
                          do.call("actionButton", c(run_bttn, list(
                            inputId = "g_polDetApply")
                          ))
                   )),

                 div(#style=style_bttn,
                   title = t_report,
                   column(1,
                          do.call("actionButton", c(report_bttn, list(
                            inputId = "g_polDetReport")
                          ))
                   )),
                 div(#style=style_bttn,
                   title = t_export,
                   column(1,
                          do.call("downloadButton", c(export_bttn, list(
                            outputId = "g_polDetExport")
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
                            outputId = "g_polDetSave")
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

# UI
#  Function source ----
source("libraries.R")
libraries()


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
                       menuSubItem("Add metadata", tabName = "add_meta", icon = icon("chevron-right"))),
              menuItem("Filters", tabName = "filters", icon = fa_i(name ="filter")),
              menuItem("Tokenization & Cleaning", tabName = "tok_cl", icon = icon("indent-right", lib = "glyphicon"),
                       menuSubItem("Parsing", tabName = "parsing", icon = icon("chevron-right")),
                       menuSubItem("Normalization", tabName = "normalization", icon = icon("chevron-right"))),
              menuItem("Vocabulary Reduction", tabName = "vocab_red", icon = icon("font", lib="glyphicon"),
                       menuSubItem("Morphological", tabName = "morph", icon = icon("chevron-right")),
                       menuSubItem("Lexical", tabName = "lexical", icon = icon("chevron-right"))),
              menuItem("Explore", tabName = "explore", icon = icon("search", lib = "glyphicon"),
                       menuSubItem("Overview", tabName = "overview", icon = icon("chevron-right")),
                       menuSubItem("WordCloud", tabName = "wordcloud", icon = icon("chevron-right")),
                       menuSubItem("Polarity detection", tabName = "polarity_det", icon = icon("chevron-right")),
                       menuSubItem("Keywords in Context", tabName = "keyword_context", icon = icon("chevron-right"))),
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
              h3("Text Analysis for ALL",align = "center"),
              br(),
              div(p("Powered by ",
                    em(a("K-Synth",
                         href = "https://k-synth.com/", target="_blank")),
                    style="text-align:center; font-size:17px;"))
            )
    ),
    ### IMPORT TEXT ----
    tabItem(tabName = "import"
            ),

    ### FILTER ----
    tabItem(tabName = "filters"
    ),

    ### TOKENIZATION & CLEANING ----

    ### Parsing ----
    tabItem(tabName = "parsing"
            ),

    ### Normalization----
    tabItem(tabName = "normalization"
            ),

    ### VOCABULARY REDUCTION ----

    ### Morphological----
    tabItem(tabName = "morph"
    ),
    ### Lexical ----
    tabItem(tabName = "lexical"
    ),

    ### EXPLORE ----

    ### Overview ----
    tabItem(tabName = "overview"
    ),
    ### WordCloud ----
    tabItem(tabName = "wordcloud"
    ),
    ### Polarity detection ----
    tabItem(tabName = "polarity_det"
    ),
    ### Keywords in context ----
    tabItem(tabName = "keyword_context"
    ),

    ### ORGANIZE ----

    ## Unsupervised
    ### Clustering ----
    tabItem(tabName = "clustering"
    ),
    ### Factorial analysis ----
    tabItem(tabName = "factorial"
    ),
    ### Network analysis ----
    tabItem(tabName = "network"
    ),
    ### Topic Modeling ----
    tabItem(tabName = "topic_model"
    ),

    ##Supervised
    ### Classification ----
    tabItem(tabName = "classification"
    ),

    ### SYNTHETIZE ----

    ### Extractive Summarization ----
    tabItem(tabName = "extractive"
    ),
    ### Abstractive Summarization ----
    tabItem(tabName = "abstractive"
    ),

    ### REPORT ----
    tabItem(tabName = "report"
    )

  )
  ### Dashboard objects style ----

  # # Research & Authors plots
  # tags$head(
  #   tags$script(
  #     'var dimension = [0, 0];
  #             $(document).on("shiny:connected", function(e) {
  #                 dimension[0] = window.innerWidth;
  #                 dimension[1] = window.innerHeight;
  #                 Shiny.onInputChange("dimension", dimension);
  #             });
  #             $(window).resize(function(e) {
  #                 dimension[0] = window.innerWidth;
  #                 dimension[1] = window.innerHeight;
  #                 Shiny.onInputChange("dimension", dimension);
  #             });'
  #   ),
  #
  #   # Icon style
  #   tags$style(HTML("
  #                               #final_text {
  #                                 text-align: center;
  #                               }
  #                               div.box-header {
  #                                 text-align: center;
  #                               }
  #                               ")
  #   ),
  #   tags$style(".fa-user {color:#FFFFFF}"),
  #   tags$style(".fa-users {color:#FFFFFF}"),
  #   tags$style(".fa-user-graduate {color:#FFFFFF}"),
  #   tags$style(".fa-book {color:#FFFFFF; font-size: 45px}"),
  #   tags$style(".fa-building {color:#FFFFFF;font-size: 45px}")
  # ),

  # ### TAB ITEMS ----
  #
  # tabItems(
  #
  #   ### WELCOME PAGE ----
  #   tabItem(tabName = "HomePage",
  #           fluidRow(
  #             div(img(src = "unina_logo.png", height = 300,width = 300), style="text-align: center;")),
  #           h1(strong("UNINA Dashboard"), align="center"),
  #           br(),
  #           h3("Knowledge Extraction from the scientific research activity of University of Naples Federico II",align = "center"),
  #           br(),
  #           div(p("Powered by ",
  #                 em(a("K-Synth",
  #                      href = "https://k-synth.com/", target="_blank")),
  #                 style="text-align:center; font-size:17px;"))
  #   ),
  #
  #   ### DEPARTMENT PAGE ----
  #   tabItem(tabName = "department",
  #           uiOutput("dept_page"),
  #           fluidRow(
  #             column(3,valueBoxOutput("strutturati", width = "33vh")),
  #             column(3,valueBoxOutput("po", width = "33vh")), #prof ordinari
  #             column(3,valueBoxOutput("pa", width = "33vh")), #prof associati
  #             column(3,valueBoxOutput("rt", width = "33vh")) # ricercatori
  #           )
  #   ),
  #
  #   ### RESEARCH PAGE ----
  #   tabItem(
  #     tabName = "research",
  #     fluidPage(
  #       fluidRow(
  #         column(width = 4,
  #                uiOutput("dip_name"),
  #                valueBoxOutput("tot_prod", width = 6), #products
  #                valueBoxOutput("prod_riv", width = 6), # prodotti su rivista
  #                valueBoxOutput("tot_Q1", width = 6), #totali prodotti
  #                valueBoxOutput("prod_rivS", width = 6), # prodotto su rivista scientifica
  #                valueBoxOutput("prod_rivA", width = 6), #prodotto su rivista fascia A
  #                valueBoxOutput("conf_pro", width = 6), # conferece proceedings
  #                valueBoxOutput("monog", width = 6), #monografie
  #                valueBoxOutput("ch_Book", width = 6),# capitolo libri
  #                valueBoxOutput("intColl", width = 6),# collab internazionali
  #                valueBoxOutput("tot_cit", width = 6)# collab internazionali
  #
  #         ),
  #
  #         # Reasearch plot: Annual scientific production ----
  #         column(width = 8,
  #                uiOutput("outcome_var"),
  #                plotlyOutput(outputId = "plot_prod", height = "37vh"),
  #                br(),
  #
  #                # Reasearch plot: Citations trend ----
  #                uiOutput("outcome_var2"),
  #                plotlyOutput(outputId = "plot_cit", height = "37vh"))
  #       )
  #     )
  #   ),
  #
  #   ### TEACHING PAGE ----
  #   tabItem(
  #     tabName = "teaching",
  #     uiOutput("teaching_page")
  #   ),
  #
  #   ### MEMBERS PAGE ----
  #   tabItem(
  #     tabName = "members",
  #     conditionalPanel("input.au_name != 'null'",
  #                      fluidPage(
  #                        fluidRow(
  #
  #                          ### Author's products ----
  #                          column(width = 4,
  #                                 ### Author name ----
  #                                 uiOutput("au_name"),
  #                                 valueBoxOutput("au_tot_prod", width = 6), #products
  #                                 valueBoxOutput("au_prod_riv", width = 6), # prodotti su rivista
  #                                 valueBoxOutput("au_tot_Q1", width = 6), #totali prodotti
  #                                 valueBoxOutput("au_prod_rivS", width = 6), # prodotto su rivista scientifica
  #                                 valueBoxOutput("au_prod_rivA", width = 6), #prodotto su rivista fascia A
  #                                 valueBoxOutput("au_conf_pro", width = 6), # conferece proceedings
  #                                 valueBoxOutput("au_monog", width = 6), #monografie
  #                                 valueBoxOutput("au_ch_Book", width = 6),
  #                                 valueBoxOutput("au_intColl", width = 6),# collab internazionali
  #                                 valueBoxOutput("au_TC", width = 6)# total citations
  #                          ),
  #
  #                          ## Author plots:Annual scientific production ----
  #                          column(width = 8,
  #                                 uiOutput("au_outcome_var"),
  #                                 plotlyOutput("au_plot_prod", height = "40vh"),
  #                                 br(),
  #                                 ## Author plots: Total citation per Year ----
  #                                 uiOutput("au_citations"),
  #                                 plotlyOutput("au_plot_cit", height = "40vh")
  #                          )
  #                        )
  #                      )
  #     )
  #   ),
  #
  #
  #   ### UPDATE FILE PAGE ----
  #   tabItem(
  #     tabName = "update",
  #     fluidRow(
  #       box(title = "Update Data",
  #           status = "info", solidHeader = TRUE, width = 12,
  #
  #           fluidRow(
  #             column(4, h4(icon("upload"), "Upload an IRIS file")),
  #           ),
  #
  #           fluidRow(
  #             column(4, fileInput('iris_file', label = "",
  #                                 multiple = FALSE,
  #                                 accept = c(".csv", ".xls", ".xlsx")))
  #           ),
  #
  #           fluidRow(
  #             column(4, h4(icon("upload"), "Upload an Info Dept file about research")),
  #           ),
  #
  #           fluidRow(
  #             column(4, fileInput('infoDept_file', label = "",
  #                                 multiple = FALSE,
  #                                 accept = c(".csv", ".xls", ".xlsx")))
  #           ),
  #
  #           fluidRow(
  #             column(4, h4(icon("upload"), "Upload an Info Dept file about teaching")),
  #           ),
  #
  #           fluidRow(
  #             column(4, fileInput('corsiDept_file', label = "",
  #                                 multiple = FALSE,
  #                                 accept = c(".csv", ".xls", ".xlsx")))
  #           )
  #       )
  #     )
  #   )
  #
  # ) # END TAB ITEMS
) # END DASHBOARDBODY


## UserInterface ####
ui <- dashboardPage(header, sidebar, body)

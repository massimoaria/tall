wordsUI <- function() {
  ## WORDS ----

  ### Clustering ----
  clustering <- tabItem(
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
  )

  ### Reinert Clustering ----
  reinert <- tabItem(
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
  )

  ### Correspondence Analysis ----
  ca <- tabItem(
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
  )

  ### Network ----

  ## Co-word analysis ----

  coword <- tabItem(
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
  )

  ## Thematic Map ----

  tm <- tabItem(
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
  )

  ## WORD EMBEDDINGS TRAINING----

  we_training <- tabItem(
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
  )

  ## WORD EMBEDDING SIMILARITY ----
  we_similarity <- tabItem(
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
  )

  ## GRAKO ----

  grako <- tabItem(
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
  )
  return(list(
    # kwic = kwic,
    clustering = clustering,
    reinert = reinert,
    ca = ca,
    coword = coword,
    tm = tm,
    we_training = we_training,
    we_similarity = we_similarity,
    grako = grako
  ))
}

wordsServer <- function(input, output, session, values, statsValues) {
  ### WORDS ----

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

  ## Click on Plotly graphs: DOC IN CONTEXT ----
  observeEvent(event_data("plotly_click"), {
    d <- event_data("plotly_click")
    elementY <- d$y[1]
    if (!is.null(elementY)) {
      if (elementY %in% unique(values$dfTag$doc_id)) {
        showModal(showDocumentModal(
          session,
          elementY,
          input
        ))
      }
    }
  })

  ## Reinert Clustering ----
  dendReinFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$w_reinclusteringApply
    },
    valueExpr = {
      values$reinert <- tall::reinert(
        x = values$dfTag,
        k = input$w_rein_k,
        term = values$generalTerm,
        segment_size = input$w_rein_segments_size,
        min_segment_size = input$w_rein_min_segments,
        min_split_members = input$w_rein_min_split_members,
        cc_test = input$w_rein_cc_test,
        tsj = input$w_rein_tsj
      )

      values$tc <- tall::term_per_cluster(
        values$reinert,
        cutree = NULL,
        k = unique(values$reinert$group)
      )
      values$reinertSummary <- tall::reinSummary(values$tc, 10)

      # groups <- tibble(uc=1:length(values$reinert$group), Cluster=values$reinert$group)
      values$tc$segments <- values$tc$segments %>%
        group_by(cluster) %>%
        arrange(uc, .by_group = TRUE) %>%
        select(-"uc")

      output$ReinCutree <- renderUI({
        req(input$w_rein_k)
        fluidRow(
          column(9),
          column(
            3,
            selectInput(
              "ReinCutree",
              label = "Dendrogram Pruning",
              choices = input$w_rein_k:1,
              selected = input$w_rein_k
            )
          )
        )
      })

      values$ReinertDendrogram <- dend2vis(
        values$reinert,
        labelsize = 10,
        nclusters = input$w_rein_k,
        community = FALSE
      )
    }
  )

  cutree_event <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$ReinCutree
    },
    valueExpr = {
      values$ReinertDendrogram <- dend2vis(
        values$reinert,
        labelsize = 10,
        nclusters = as.numeric(input$ReinCutree),
        community = FALSE
      )
    }
  )

  output$w_ReinClusteringPlot <- renderVisNetwork({
    dendReinFunction()
    cutree_event()
    values$ReinertDendrogram
  })

  output$w_ReinClusteringTableSegments <- renderDT({
    dendReinFunction()
    # find sentences containing the tokens/lemma
    DTformat(
      values$tc$segments,
      size = "100%",
      button = FALSE,
      col_to_remove = values$generalTerm
    )
  })
  output$w_ReinSummaryTable <- renderDT({
    dendReinFunction()
    # find sentences containing the tokens/lemma
    DTformat(
      values$reinertSummary,
      size = "100%",
      button = FALSE,
      filter = "none",
      col_to_remove = values$generalTerm
    )
  })

  output$w_ReinClusteringTableTerms <- renderDT({
    dendReinFunction()
    DTformat(
      values$tc$terms %>%
        mutate(
          freq = round(freq * 100, 1),
          chi_square = round(chi_square, 1),
          p_value = round(p_value, 4)
        ) %>%
        select(term, freq_true, freq, chi_square, p_value, sign, cluster) %>%
        rename(
          "Term" = term,
          "Freq. in Cluster" = freq_true,
          "% in Cluster" = freq,
          "Chi^2" = chi_square,
          "P-Value" = p_value,
          "Sign" = sign,
          "Cluster" = cluster
        ),
      size = "85%",
      button = FALSE,
      numeric = c(5),
      round = 3
    )
  })

  ## export CLustering button
  observeEvent(
    eventExpr = {
      input$w_reinclusteringExport
    },
    handlerExpr = {
      file <- paste("ReinertDendrogram-", sys.time(), ".png", sep = "")
      file <- destFolder(file, values$wdTall)
      plot2png(values$ReinertDendrogram, filename = file, type = "vis")
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$w_reinclusteringReport, {
    if (!is.null(values$reinert)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "Reinert"
      list_df <- list(
        values$reinertSummary,
        values$tc$terms %>%
          mutate(freq = round(freq * 100, 1)) %>%
          select(term, freq, chi_square, p_value, sign, cluster) %>%
          rename(
            "Term" = term,
            "% in Cluster" = freq,
            "Chi^2" = chi_square,
            "P-Value" = p_value,
            "Sign" = sign,
            "Cluster" = cluster
          ),
        values$tc$segments
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileReinertDendrogram <- plot2png(
        values$ReinertDendrogram,
        filename = "ReinertDendrogram.png",
        type = "vis",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileReinertDendrogram, res$col)
      )
      popUp(title = "Reinert Clustering Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Clustering ----

  ## Dendrogramm ----
  dendFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$w_clusteringApply
    },
    valueExpr = {
      results <- clustering(
        LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected),
        n = input$w_clusteringNMax,
        group = "doc_id",
        minEdges = 25,
        term = values$generalTerm,
        normalization = input$w_clusteringSimilarity
      )
      values$wordCluster <- results$cluster
      values$wordCluster <- values$wordCluster %>%
        rename(Word = word, Group = group, Frequency = frequency)
      values$wordComm <- results$comm
      if (input$w_clusteringMode == "auto") {
        nclusters <- max(values$wordComm$membership)
      } else {
        nclusters <- min(
          input$w_nclusters,
          length(values$wordComm$membership) - 1
        )
      }
      values$WordDendrogram <- dend2vis(
        values$wordComm,
        labelsize = input$w_clusteringLabelSize,
        nclusters = nclusters
      )
    }
  )

  output$w_clusteringPlot <- renderVisNetwork({
    dendFunction()
    values$WordDendrogram
  })

  output$w_clusteringTable <- renderDT(server = FALSE, {
    dendFunction()
    DTformat(
      values$wordCluster,
      size = "100%",
      filename = "ClusterWordsTable",
      pagelength = TRUE,
      left = 1,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      filter = "top",
      col_to_remove = values$generalTerm
    )
  })

  ## export CLustering button
  observeEvent(
    eventExpr = {
      input$w_clusteringExport
    },
    handlerExpr = {
      file <- paste("Dendrogram-", sys.time(), ".png", sep = "")
      file <- destFolder(file, values$wdTall)
      plot2png(values$WordDendrogram, filename = file, zoom = values$zoom)
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$w_clusteringReport, {
    if (!is.null(values$wordCluster)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "Clustering"
      list_df <- list(values$wordCluster)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileDend <- plot2png(
        values$WordDendrogram,
        filename = "Clustering.png",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileDend, res$col)
      )
      popUp(title = "Clustering Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Correspondence Analysis ----

  # CA plot
  caPlotFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$caApply
    },
    valueExpr = {
      ## check to verify if groups exist or not
      filtered <- LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected)
      if (
        input$groupCA == "Documents" & "ungroupDoc_id" %in% names(values$dfTag)
      ) {
        filtered <- backToOriginalGroups(filtered)
      }
      values$CA <- wordCA(
        filtered,
        n = input$nCA,
        term = values$generalTerm,
        group = input$groupCA
      )
      ##
      values$CA <- caClustering(
        values$CA,
        nclusters = input$nClustersCA,
        nDim = input$nDimsCA,
        lim.contr = input$lim.contribCA
      )
      values$CAdimY <- as.numeric(input$dimPlotCA) * 2
      values$CAdimX <- values$CAdimY - 1
      values$plotCA <- ca2plotly(
        values$CA,
        dimX = values$CAdimX,
        dimY = values$CAdimY,
        topWordPlot = input$nCA,
        topDocPlot = input$nDocCA,
        threshold = 0.03,
        labelsize = input$labelsizeCA,
        size = input$sizeCA,
        lim.contr = input$lim.contribCA
      )
      values$CADendrogram <- dend2vis(
        values$CA$clustering$h,
        labelsize = input$labelsizeCA,
        nclusters = input$nClustersCA,
        community = FALSE
      )

      # wordCoordData
      values$CA$wordCoordData <- values$CA$wordCoord %>%
        select(label, everything()) %>%
        left_join(
          data.frame(
            label = names(values$CA$clustering$groups),
            Group = values$CA$clustering$groups
          ),
          by = "label"
        ) %>%
        rename(Label = label) %>%
        select(Label, Group, everything()) %>%
        rename(Cluster = Group)

      # contribData
      values$CA$contribData <- values$CA$contrib %>%
        tibble::rownames_to_column() %>%
        rename(Label = rowname)

      # cosineData
      values$CA$cosineData <- values$CA$cosine %>%
        tibble::rownames_to_column() %>%
        rename(Label = rowname)

      # dfCA
      if (length(values$CA$ca$sv) < 10) {
        values$dfCA <- data.frame(
          dim = paste0("Dim ", 1:length(values$CA$ca$sv)),
          sv = (values$CA$ca$sv / sum(values$CA$ca$sv) * 100),
          svcorr = values$CA$ca$eigCorrectedNorm
        )
      } else {
        values$dfCA <- data.frame(
          dim = paste0("Dim ", 1:10),
          sv = (values$CA$ca$sv / sum(values$CA$ca$sv) * 100)[1:10],
          svcorr = values$CA$ca$eigCorrectedNorm[1:10]
        )
      }
      values$dfCA <- values$dfCA %>%
        rename(
          "Factorial Dimension" = dim,
          "Singular Values" = sv,
          "Corrected Explained Inertia" = svcorr
        )
    }
  )

  output$caPlot <- renderPlotly({
    caPlotFunction()
    values$plotCA
  })

  output$caDendrogram <- renderVisNetwork({
    caPlotFunction()
    values$CADendrogram
  })

  # gemini button for correspondence analysis
  output$caGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$caGemini, values)
  })

  # CA Table
  output$caCoordTable <- renderDT(server = FALSE, {
    caPlotFunction()
    DTformat(
      values$CA$wordCoordData,
      size = "100%",
      filename = "CAWordCoordinatesTable",
      pagelength = TRUE,
      left = 1,
      right = 2:ncol(values$CA$wordCoordData),
      numeric = 3:ncol(values$CA$wordCoordData),
      dom = TRUE,
      filter = "top",
      round = 3,
      col_to_remove = values$generalTerm
    )
  })

  output$caContribTable <- renderDT(server = FALSE, {
    caPlotFunction()
    DTformat(
      values$CA$contribData,
      size = "100%",
      filename = "CAWordContributesTable",
      pagelength = TRUE,
      left = 1, # right=2:(ncol(values$CA$contrib)+1),
      numeric = 2:(ncol(values$CA$contrib) + 1),
      dom = TRUE,
      filter = "top",
      round = 3,
      col_to_remove = values$generalTerm
    )
  })

  output$caCosineTable <- renderDT(server = FALSE, {
    caPlotFunction()
    DTformat(
      values$CA$cosineData,
      size = "100%",
      filename = "CAWordCosinesTable",
      pagelength = TRUE,
      left = 1, # right=2:(ncol(values$CA$cosine)+1),
      numeric = 2:(ncol(values$CA$cosine) + 1),
      dom = TRUE,
      filter = "top",
      round = 3,
      col_to_remove = values$generalTerm
    )
  })

  output$caSingularValueTable <- renderDT(server = FALSE, {
    caPlotFunction()
    DTformat(
      values$dfCA,
      size = "100%",
      filename = "CAWordSingualValueTable",
      pagelength = TRUE,
      left = 1, # right=2:3,
      numeric = 2:3,
      dom = TRUE,
      filter = "top",
      round = 2,
      col_to_remove = values$generalTerm
    )
  })

  observeEvent(
    eventExpr = {
      input$caExport
    },
    handlerExpr = {
      file1 <- paste("CAMap-", sys.time(), ".png", sep = "")
      file1 <- destFolder(file1, values$wdTall)
      file2 <- paste("CADendrogram-", sys.time(), ".png", sep = "")
      file2 <- destFolder(file2, values$wdTall)
      plot2png(values$plotCA, filename = file1, type = "plotly")
      plot2png(values$CADendrogram, filename = file2, type = "vis")
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$caReport, {
    if (!is.null(values$CA)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "CorrespondenceAnalysis"

      Gem <- values$caGemini %>% string_to_sentence_df()
      list_df <- list(
        Gem,
        values$CA$wordCoordData,
        values$CA$contribData,
        values$CA$cosineData,
        values$dfCA
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileplotCA <- plot2png(
        values$plotCA,
        filename = "CAMap.png",
        type = "plotly",
        zoom = values$zoom
      )
      values$fileCADendrogram <- plot2png(
        values$CADendrogram,
        filename = "CADendrogram.png",
        type = "vis",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileplotCA, res$col),
        c(sheetname = res$sheetname, values$fileCADendrogram, res$col)
      )
      popUp(title = "Correspondence Analysis Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Network ----

  ## Co-word analysis ----
  netFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$w_networkCoocApply
    },
    valueExpr = {
      switch(
        input$w_groupNet,
        Groups = {
          group <- "doc_id"
        },
        Documents = {
          group <- "doc_id"
        },
        Paragraphs = {
          group <- c("doc_id", "paragraph_id")
        },
        Sentences = {
          group <- c("doc_id", "sentence_id")
        }
      )
      ## check to verify if groups exist or not

      # community.repulsion <- as.numeric(gsub("%","",input$community.repulsion))/100
      #community.repulsion <- 0

      filtered <- LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected)
      if (
        input$w_groupNet == "Documents" &
          "ungroupDoc_id" %in% names(values$dfTag)
      ) {
        filtered <- backToOriginalGroups(filtered)
      }
      values$network <- network(
        filtered,
        term = values$generalTerm,
        group = group,
        n = input$nMax,
        minEdges = input$minEdges,
        labelsize = input$labelSize,
        opacity = input$opacity,
        interLinks = input$interLinks,
        normalization = input$normalizationCooc,
        remove.isolated = input$removeIsolated,
        community.repulsion = 0.5,
        seed = values$random_seed,
        cluster = "louvain"
      )
      ## end check
      # net=values$network
      # save(net, file="network.rdata")

      values$netVis <- net2vis(
        nodes = values$network$nodes,
        edges = values$network$edges,
        click = TRUE,
        noOverlap = input$noOverlap
      )

      # network$nodes
      if (is.na(values$network$nodes)[1]) {
        values$network$nodesData <- data.frame(
          Word = "",
          Frequency = NA,
          Group = NA,
          "Color Group" = ""
        )
        values$network$edgesData <- data.frame(
          From = NA,
          To = NA,
          "Co-occurence" = 0,
          "Association Index" = 0,
          "Cosine Similarity" = 0,
          "Jaccard Index" = 0,
          "Group From" = 0,
          "Group To" = 0
        )
      } else {
        values$network$nodesData <- values$network$nodes %>%
          select(label, value, group, color) %>%
          rename(
            Word = label,
            Frequency = value,
            Group = group,
            "Color Group" = color
          )

        # network$edges
        values$network$edgesData <- values$network$edges %>%
          select(term_from, term_to, group_from, group_to, s, sA, sC, sJ) %>%
          rename(
            From = term_from,
            To = term_to,
            "Co-occurence" = s,
            "Association Index" = sA,
            "Cosine Similarity" = sC,
            "Jaccard Index" = sJ,
            "Group From" = group_from,
            "Group To" = group_to
          )
        values$gemini_model_parameters <- geminiParameterPrompt(
          values,
          input$sidebarmenu,
          input
        )
      }
    }
  )

  output$w_networkCoocPlot <- renderVisNetwork({
    netFunction()
    values$netVis
  })

  # gemini button for word network
  output$w_networkGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$w_networkGemini, values)
  })

  output$w_networkCoocNodesTable <- renderDT(server = FALSE, {
    netFunction()
    DTformat(
      values$network$nodesData,
      size = "100%",
      filename = "NetworkWordsTable",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      filter = "top",
      col_to_remove = values$generalTerm
    )
  })

  output$w_networkCoocEdgesTable <- renderDT(server = FALSE, {
    netFunction()
    DTformat(
      values$network$edgesData,
      size = "100%",
      filename = "NetworkLinksTable",
      numeric = 6:8,
      round = 4,
      col_to_remove = values$generalTerm
    )
  })

  ## export Network button
  observeEvent(
    eventExpr = {
      input$w_networkCoocExport
    },
    handlerExpr = {
      file <- paste("Network-Docs-", sys.time(), ".png", sep = "")
      file <- destFolder(file, values$wdTall)
      plot2png(values$netVis, filename = file, zoom = values$zoom)
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$w_networkCoocReport, {
    if (!is.null(values$network$nodes)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "CoWord"
      Gem <- values$w_networkGemini %>% string_to_sentence_df()
      list_df <- list(
        Gem,
        values$network$nodesData,
        values$network$edgesData
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$filenetVis <- plot2png(
        values$netVis,
        filename = "CoWord.png",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$filenetVis, res$col)
      )
      popUp(title = "Co-Word Analysis Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## Click on visNetwork: WORDS IN CONTEXT ----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$click
    },
    handlerExpr = {
      if (input$click != "null") {
        showModal(plotModalTermNet(session))
      }
    }
  )

  plotModalTermNet <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Words in Context"))),
      DTOutput(ns("wordInContextNet")),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(
          label = "Close",
          inputId = "closePlotModalTermNet",
          style = "color: #ffff;",
          icon = icon("remove", lib = "glyphicon")
        )
      ),
    )
  }

  observeEvent(input$closePlotModalTermNet, {
    removeModal(session = getDefaultReactiveDomain())
    # session$sendCustomMessage("click", 'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  output$wordInContextNet <- renderDT(
    server = FALSE,
    {
      if (!is.null(input$click)) {
        id <- input$click
      } else {
        id <- NULL
      }
      switch(
        input$sidebarmenu,
        "w_networkGrako" = {
          word_search <- values$grako$nodes$title[values$grako$nodes$id == id]

          selectedEdges <- values$grako$edges %>%
            filter(term_from %in% word_search | term_to %in% word_search) %>%
            mutate(grako = paste0(term_from, " ", term_to))

          sentences <- values$grako$multiwords %>%
            filter(grako %in% selectedEdges$grako) %>%
            select(doc_id, sentence_hl) %>%
            distinct()
        },
        "overview" = {
          word_search <- values$WC2VIS$x$nodes$label[
            values$WC2VIS$x$nodes$id == id
          ]
          sentences <- values$dfTag %>%
            filter(docSelected) %>%
            filter(.data[[values$generalTerm]] %in% word_search) %>%
            ungroup() %>%
            select(doc_id, lemma, token, sentence_hl)
        },
        {
          word_search <- values$network$nodes$label[
            values$network$nodes$id %in% id # to check for warning
          ]
          sentences <- values$dfTag %>%
            filter(docSelected) %>%
            filter(lemma %in% word_search) %>%
            ungroup() %>%
            select(doc_id, lemma, token, sentence_hl)
        }
      )

      # find sentences containing the tokens/lemma
      DTformat(
        sentences,
        size = "100%",
        button = TRUE,
        col_to_remove = "lemma"
      )
    },
    escape = FALSE
  )

  ## Click on Dendrogram: WORDS IN CONTEXT ----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$click_dend
    },
    handlerExpr = {
      if (input$click_dend != "null") {
        showModal(plotModalTermDend(session))
      }
    }
  )

  plotModalTermDend <- function(session) {
    ns <- session$ns
    modalDialog(
      h3(strong(("Words in Context"))),
      DTOutput(ns("wordInContextDend")),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(
          label = "Close",
          inputId = "closeplotModalTermDend",
          style = "color: #ffff;",
          icon = icon("remove", lib = "glyphicon")
        )
      ),
    )
  }

  observeEvent(input$closeplotModalTermDend, {
    removeModal(session = getDefaultReactiveDomain())
    # session$sendCustomMessage("click_dend",'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  output$wordInContextDend <- renderDT(
    server = FALSE,
    {
      if (!is.null(input$click_dend)) {
        id <- unlist(input$click_dend)
      }
      switch(
        input$sidebarmenu,
        "w_clustering" = {
          words_id <- c(
            id,
            unlist(values$WordDendrogram$x$nodes$neib[
              values$WordDendrogram$x$nodes$id == id
            ])
          )
          words <- unlist(values$WordDendrogram$x$nodes$label[
            values$WordDendrogram$x$nodes$id %in% words_id
          ])
          word_search <- words[!is.na(words)]
        },
        "ca" = {
          words_id <- c(
            id,
            unlist(values$CADendrogram$x$nodes$neib[
              values$CADendrogram$x$nodes$id == id
            ])
          )
          words <- unlist(values$CADendrogram$x$nodes$label[
            values$CADendrogram$x$nodes$id %in% words_id
          ])
          word_search <- words[!is.na(words)]
        }
      )

      sentences <- values$dfTag %>%
        filter(docSelected) %>%
        filter(lemma %in% word_search) %>%
        ungroup() %>%
        select(doc_id, lemma, token, sentence_hl)

      # find sentences containing the tokens/lemma
      DTformat(
        sentences,
        size = "100%",
        button = TRUE,
        col_to_remove = values$generalTerm
      )
    },
    escape = FALSE
  )

  # ## Report
  #
  # observeEvent(input$w_networkCoocReport,{
  #   if(!is.null(values$network$nodes)){
  #     popUp(title=NULL, type="waiting")
  #     sheetname <- "CoWord"
  #     list_df <- list(values$network$nodesData
  #                     ,values$network$edgesData
  #     )
  #     res <- addDataScreenWb(list_df, wb=values$wb, sheetname=sheetname)
  #     #values$wb <- res$wb
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     values$filenetVis <- plot2png(values$netVis, filename="CoWord.png", zoom = values$zoom)
  #     values$list_file <- rbind(values$list_file, c(sheetname=res$sheetname,values$filenetVis,res$col))
  #     popUp(title="Co-Word Analysis Results", type="success")
  #     values$myChoices <- sheets(values$wb)
  #   } else {
  #     popUp(type="error")
  #   }
  # })

  ## Click on Reinert Dendrogram: WORDS IN CONTEXT ----
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$click_rein
    },
    handlerExpr = {
      if (input$click_rein != "null") {
        id <- unlist(input$click_rein)
        words_id <- c(
          id,
          unlist(values$ReinertDendrogram$x$nodes$neib[
            values$ReinertDendrogram$x$nodes$id == id
          ])
        )
        words <- unlist(values$ReinertDendrogram$x$nodes$label[
          values$ReinertDendrogram$x$nodes$id %in% words_id
        ])
        word_search <- as.numeric(words[!is.na(words)])
        values$word_search_rein <- word_search

        if (length(word_search) > 0) {
          values$tc_k <- values$tc

          # remove duplicated terms when two or more clusters are aggregated
          values$tc_k$terms <- values$tc_k$terms %>%
            filter(cluster %in% word_search) %>%
            group_by(term) %>%
            slice_min(order_by = p_value, n = 1) %>%
            ungroup()

          values$tc_k$segments <- values$tc_k$segments %>%
            filter(cluster %in% word_search)
          # segments <- values$tc
          values$tc_k <- highlight_segments(values$tc_k, n = 10)

          # values$tc_k$segments <- values$tc_k$segments %>%
          #   group_by(doc_id) %>%
          #   arrange(uc, .by_group = TRUE) %>%
          #   select(doc_id, uc, segment, cluster)
        }
        showModal(plotModalTermRein(session))
      }
    }
  )

  plotModalTermRein <- function(session) {
    ns <- session$ns
    modalDialog(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Terms by Cluster",
          h3(strong(
            (paste0(
              "Terms associated to Cluster(s): ",
              paste0(values$word_search_rein, collapse = ", "),
              collape = ""
            ))
          )),
          plotlyOutput(ns("plotInContextRein"))
        ),
        tabPanel(
          "Segments by Cluster",
          h3(strong(
            (paste0(
              "Segments associated to Cluster(s): ",
              paste0(values$word_search_rein, collapse = ", "),
              collape = ""
            ))
          )),
          DTOutput(ns("wordInContextRein"))
        ),
      ),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(
          label = "Close",
          inputId = "closeplotModalTermRein",
          style = "color: #ffff;",
          icon = icon("remove", lib = "glyphicon")
        )
      ),
    )
  }

  observeEvent(input$closeplotModalTermRein, {
    removeModal(session = getDefaultReactiveDomain())
    # session$sendCustomMessage("click_dend",'null') # reset input value to plot modal more times
    resetModalButtons(session = getDefaultReactiveDomain())
  })

  output$wordInContextRein <- renderDT(
    server = FALSE,
    {
      req(values$tc_k$segments)
      # find sentences containing the tokens/lemma
      DTformat(
        values$tc_k$segments,
        nrow = 5,
        size = "80%",
        button = TRUE,
        col_to_remove = values$generalTerm
      )
    },
    escape = FALSE
  )

  output$plotInContextRein <- renderPlotly({
    tall::reinPlot(values$tc_k$terms, nPlot = 10)
  })

  ## Thematic Map ----
  TMFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$w_networkTMApply
    },
    valueExpr = {
      switch(
        input$w_groupTM,
        Groups = {
          group <- "doc_id"
        },
        Documents = {
          group <- "doc_id"
        },
        Paragraphs = {
          group <- c("doc_id", "paragraph_id")
        },
        Sentences = {
          group <- c("doc_id", "sentence_id")
        }
      )
      ## check to verify if groups exist or not

      filtered <- LemmaSelection(values$dfTag) %>% dplyr::filter(docSelected)
      if (
        input$w_groupTM == "Documents" &
          "ungroupDoc_id" %in% names(values$dfTag)
      ) {
        filtered <- backToOriginalGroups(filtered)
      }
      values$TM <- tallThematicmap(
        filtered,
        term = values$generalTerm,
        group = group,
        n = input$nMaxTM,
        labelsize = input$labelSizeTM,
        n.labels = input$n.labelsTM,
        opacity = input$opacityTM,
        seed = values$random_seed
      )

      values$TMvis <- net2vis(
        nodes = values$TM$net$nodes,
        edges = values$TM$net$edges,
        click = FALSE,
        noOverlap = TRUE
      )
      values$TMmap <- plotTM(values$TM$df, size = input$labelSizeTM / 10)
      values$TM$ClusterTable <- values$TM$df %>%
        rename(
          "Label" = name,
          "Cluster Frequency" = freq,
          "Num. of Words" = n,
          "Centrality" = centrality,
          "Density" = density,
          "Cluster" = groups,
          "Color" = color
        ) %>%
        select(
          "Label",
          "Cluster",
          "Cluster Frequency",
          "Num. of Words",
          "Centrality",
          "Density",
          "Color"
        )
    }
  )

  output$w_networkTMMapPlot <- renderPlotly({
    TMFunction()
    values$TMmap
  })

  # gemini button for word in context
  output$w_networkTMGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$w_networkTMGemini, values)
  })

  output$w_networkTMNetPlot <- renderVisNetwork({
    TMFunction()
    values$TMvis
  })

  output$w_networkTMClusterTable <- renderDT(server = FALSE, {
    TMFunction()
    DTformat(
      values$TM$ClusterTable,
      size = "100%",
      filename = "TMClusterTable",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      filter = "top",
      col_to_remove = values$generalTerm
    )
  })

  output$w_networkTMWordTable <- renderDT(server = FALSE, {
    TMFunction()
    DTformat(
      values$TM$df_lab %>% select(-Cluster_Frequency),
      size = "100%",
      filename = "TMWordsTable",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      filter = "top",
      col_to_remove = values$generalTerm
    )
  })

  ## export TM button
  observeEvent(
    eventExpr = {
      input$w_networkTMExport
    },
    handlerExpr = {
      file1 <- paste("TAMap-", sys.time(), ".png", sep = "")
      file1 <- destFolder(file1, values$wdTall)
      file2 <- paste("TANetwork-", sys.time(), ".png", sep = "")
      file2 <- destFolder(file2, values$wdTall)
      plot2png(values$TMmap, filename = file1, type = "plotly")
      plot2png(values$TMvis, filename = file2, type = "vis")
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$w_networkTMReport, {
    if (!is.null(values$TM)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "ThematicMap"
      Gem <- values$w_networkTMGemini %>% string_to_sentence_df()
      list_df <- list(
        Gem,
        values$TM$ClusterTable,
        values$TM$df_lab %>% select(-Cluster_Frequency)
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileplotTM <- plot2png(
        values$TMmap,
        filename = "TMMap.png",
        type = "plotly",
        zoom = values$zoom
      )
      values$fileTMNetwork <- plot2png(
        values$TMvis,
        filename = "TMNetwork.png",
        type = "vis",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileplotTM, res$col),
        c(sheetname = res$sheetname, values$fileTMNetwork, res$col)
      )
      popUp(title = "Thematic Map Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## word2vec TRAINING ----

  w2vTrainingFunction <- eventReactive(input$w2vApply, valueExpr = {
    values$w2v_model <- w2vTraining(
      values$dfTag %>% filter(docSelected),
      term = values$generalTerm,
      dim = input$w2vDim,
      iter = input$w2vIter
    )
    values$w2v_stats <- list()
    values$w2v_stats$stats <- summary_stats_embeddings(as.matrix(
      values$w2v_model
    ))
    # values$w2v_stats$distances <- distance_similarity_stats(as.matrix(values$w2v_model))
    values$w2v_stats$pca <- pca_analysis_embeddings(as.matrix(values$w2v_model))
    values$df_EmbeddingDims <- as.matrix(values$w2v_model) %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Word") %>%
      tidyr::pivot_longer(
        cols = -Word,
        names_to = "Dimension",
        values_to = "Value"
      )
    values$df_EmbeddingDims$Dimension <- rep(
      sprintf(
        "D%03d",
        seq_len(
          nrow(values$df_EmbeddingDims) /
            length(unique(values$df_EmbeddingDims$Word))
        )
      ),
      length(unique(values$df_EmbeddingDims$Word))
    )
    values$w2vBoxplot <- plot_ly(
      data = values$df_EmbeddingDims,
      y = ~Value,
      x = ~Dimension,
      type = "box",
      boxpoints = "outliers",
      hoverinfo = "x+y"
    ) %>%
      layout(
        title = "Distribution of embedding values by dimension",
        xaxis = list(title = "Dimension", tickangle = -45),
        yaxis = list(title = "Value")
      )
    values$w2vPCA <- plot_ly(
      x = sprintf("PC%03d", seq_along(values$w2v_stats$pca)),
      y = values$w2v_stats$pca * 100,
      type = "bar"
    ) %>%
      layout(
        title = "Variance Explained by Principal Components",
        xaxis = list(title = "Principal Components"),
        yaxis = list(title = "Variance Proportion"),
        bargap = 0.2
      )
  })

  output$w_word2vecBoxplot <- renderPlotly({
    w2vTrainingFunction()
    values$w2vBoxplot
  })

  output$w_word2vecPCA <- renderPlotly({
    w2vTrainingFunction()
    values$w2vPCA
  })

  output$w_word2vecTable <- renderDT(server = FALSE, {
    w2vTrainingFunction()
    DTformat(
      values$w2v_stats$stats,
      size = "80%",
      filename = "WordEmbeddingTable",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      filter = "none"
    )
  })

  ## export Embedding button
  observeEvent(
    eventExpr = {
      input$w2vSave
    },
    handlerExpr = {
      file1 <- paste("WEmatrix-", sys.time(), ".csv", sep = "")
      file1 <- destFolder(file1, values$wdTall)
      file2 <- paste("WEboxplot-", sys.time(), ".png", sep = "")
      file2 <- destFolder(file2, values$wdTall)
      file3 <- paste("WEpca-", sys.time(), ".png", sep = "")
      file3 <- destFolder(file3, values$wdTall)
      write.csv(as.matrix(values$w2v_model), file = file1)
      plot2png(values$w2vBoxplot, filename = file2, type = "plotly")
      plot2png(values$w2vPCA, filename = file3, type = "plotly")
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report
  observeEvent(input$w2vReport, {
    if (!is.null(values$w2v_model)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "EmbeddingTraining"
      list_df <- list(values$w2v_stats$stats)
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileplotw2vBoxplot <- plot2png(
        values$w2vBoxplot,
        filename = "w2vBoxplot.png",
        type = "plotly",
        zoom = values$zoom
      )
      values$fileplotw2vPCA <- plot2png(
        values$w2vPCA,
        filename = "w2vPCA.png",
        type = "plotly",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileplotw2vBoxplot, res$col),
        c(sheetname = res$sheetname, values$fileplotw2vPCA, res$col)
      )
      popUp(title = "Word Embedding Training Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## word2vec SIMILARITY ----

  w2vSimilarity <- eventReactive(input$w_w2v_similarityApply, {
    w2vTrainingFunction()
    values$w2vNetwork <- w2vNetwork(
      values$w2v_model,
      values$dfTag,
      term = values$generalTerm,
      n = input$w_w2v_similarityN
    )
    values$umapDf <- w2vUMAP(
      values$w2v_model,
      top_words = values$w2vNetwork$top_words
    )
    df_adj <- adjust_labels_iterative_with_opacity(
      values$umapDf,
      min_dist = 0.3,
      max_iter = 50,
      shift_step = 0.05,
      alpha_low = 0.4
    )
    values$w2vUMAPplot <- plot_ly(
      data = df_adj,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "text",
      text = ~word,
      textfont = list(
        size = input$w_w2v_font_size
      ),
      textposition = "top center"
    ) %>%
      style(
        textfont = list(color = df_adj$text_color, size = input$w_w2v_font_size)
      ) %>%
      layout(
        # title = list(text = "CBOW Embeddings Visualization (UMAP)", x = 0.5),
        xaxis = list(
          title = "UMAP Dimension 1",
          zeroline = FALSE,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "UMAP Dimension 2",
          zeroline = FALSE,
          showgrid = FALSE
        ),
        hovermode = "closest"
      )
  })

  output$w_w2v_Selected <- renderUI({
    w2vTrainingFunction()
    #nodesId <- sort(values$w2vNetwork$top_words)
    nodesId <- sort(values$w2vNetwork$nodes$id)
    selectInput(
      "w2v_selected_node",
      "Select word to highlight:",
      choices = c("", nodesId),
      selected = ""
    )
  })

  output$w_w2vNetworkplot <- renderVisNetwork({
    w2vSimilarity()
    values$w2vNetworkPlot <- w2v2Vis(
      nodes = values$w2vNetwork$nodes,
      edges = values$w2vNetwork$edges,
      size = 20,
      labelsize = input$w_w2v_font_size,
      overlap = input$w_w2v_overlap
    )
    values$w2vNetworkPlot
  })

  # gemini button for word embedding similarity
  output$w_w2vGeminiUI <- renderUI({
    values$gemini_model_parameters <- geminiParameterPrompt(
      values,
      input$sidebarmenu,
      input
    )
    geminiOutput(title = "", content = values$w_w2vGemini, values)
  })

  observe({
    visNetworkProxy("w_w2vNetworkplot") %>%
      visSelectNodes(id = input$w2v_selected_node) %>%
      visFit(nodes = input$w2v_selected_node)
  })

  observeEvent(
    eventExpr = {
      input$w_w2v_Back
    },
    handlerExpr = {
      visNetworkProxy("w_w2vNetworkplot") %>%
        visSelectNodes(id = values$w2vNetwork$top_words) %>%
        visFit(nodes = NULL)
    }
  )

  output$w_w2vUMAPplot <- renderPlotly({
    w2vSimilarity()
    values$w2vUMAPplot
  })

  ## export Embedding Similarity button
  observeEvent(
    eventExpr = {
      input$w_w2v_similarityExport
    },
    handlerExpr = {
      file1 <- paste("WENetplot-", sys.time(), ".png", sep = "")
      file1 <- destFolder(file1, values$wdTall)
      file2 <- paste("WEumap-", sys.time(), ".png", sep = "")
      file2 <- destFolder(file2, values$wdTall)
      plot2png(values$w2vNetworkPlot, filename = file1, type = "vis")
      plot2png(values$w2vUMAPplot, filename = file2, type = "plotly")
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report
  observeEvent(input$w_w2v_similarityReport, {
    if (!is.null(values$umapDf)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "EmbeddingSimilarity"
      Gem <- values$w_w2vGemini %>% string_to_sentence_df()
      list_df <- list(
        Gem,
        values$w2vNetwork$edges,
        values$umapDf
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileplotw2vNet <- plot2png(
        values$w2vNetworkPlot,
        filename = "w2vNetworkPlot.png",
        type = "vis",
        zoom = values$zoom
      )
      values$fileplotw2vUMAP <- plot2png(
        values$w2vUMAPplot,
        filename = "w2vUMAPPlot.png",
        type = "plotly",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileplotw2vNet, res$col),
        c(sheetname = res$sheetname, values$fileplotw2vUMAP, res$col)
      )
      popUp(title = "Word Embedding Similarity Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })

  ## GRAKO ----
  grakoFunction <- eventReactive(
    ignoreNULL = TRUE,
    eventExpr = {
      input$w_networkGrakoApply
    },
    valueExpr = {
      values$grako <- grako(
        values$dfTag %>% filter(docSelected),
        n = input$grakoNMax,
        minEdges = input$grakoMinEdges,
        labelsize = input$grakoLabelSize,
        opacity = input$grakoOpacity,
        normalization = input$grakoNormalization,
        singleWords = input$grakoUnigram,
        term = values$generalTerm
      )

      values$grakoVis <- grako2vis(
        nodes = values$grako$nodes,
        edges = values$grako$edges
      )

      # grako$nodes
      values$grako$nodesData <- values$grako$nodes %>%
        select(upos, label, value) %>%
        mutate(label = gsub("<.*?>", "", label)) %>%
        rename(
          "Part of Speech" = upos,
          Word = label,
          Frequency = value
        ) %>%
        relocate("Part of Speech", .after = last_col())

      # grako$edges
      values$grako$edgesData <- values$grako$edges %>%
        select(term_from, term_to, upos_from, upos_to, role, s, sA, sC, sJ) %>%
        rename(
          From = term_from,
          To = term_to,
          "Co-occurence" = s,
          "Association Index" = sA,
          "Cosine Similarity" = sC,
          "Jaccard Index" = sJ,
          "PoS From" = upos_from,
          "PoS To" = upos_to,
          "Action" = role
        )
    }
  )

  output$w_networkGrakoPlot <- renderVisNetwork({
    grakoFunction()
    values$grakoVis
  })

  output$w_networkGrakoNodesTable <- renderDT(server = FALSE, {
    grakoFunction()
    DTformat(
      values$grako$nodesData,
      size = "100%",
      filename = "GrakoWordsTable",
      pagelength = TRUE,
      left = NULL,
      right = NULL,
      numeric = NULL,
      dom = TRUE,
      filter = "top"
    )
  })

  output$w_networkGrakoEdgesTable <- renderDT(server = FALSE, {
    grakoFunction()
    DTformat(
      values$grako$edgesData,
      size = "100%",
      filename = "GrakoLinksTable",
      numeric = 7:9,
      round = 4
    )
  })

  ## export Network button
  observeEvent(
    eventExpr = {
      input$w_networkGrakoExport
    },
    handlerExpr = {
      file <- paste("Grako-", sys.time(), ".png", sep = "")
      file <- destFolder(file, values$wdTall)
      plot2png(values$grakoVis, filename = file, zoom = values$zoom)
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ## Report

  observeEvent(input$w_networkGrakoReport, {
    if (!is.null(values$grako$nodes)) {
      popUp(title = NULL, type = "waiting")
      sheetname <- "Grako"
      list_df <- list(
        values$grako$nodesData,
        values$grako$edgesData
      )
      res <- addDataScreenWb(list_df, wb = values$wb, sheetname = sheetname)
      # values$wb <- res$wb
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      values$fileGrako <- plot2png(
        values$grakoVis,
        filename = "grako.png",
        zoom = values$zoom
      )
      values$list_file <- rbind(
        values$list_file,
        c(sheetname = res$sheetname, values$fileGrako, res$col)
      )
      popUp(title = "Grako Results", type = "success")
      values$myChoices <- sheets(values$wb)
    } else {
      popUp(type = "error")
    }
  })
}

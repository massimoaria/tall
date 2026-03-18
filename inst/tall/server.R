##  Server ####
files <- c(
  "libraries.R",
  "tallAI.R",
  "tallShot.R",
  "tallUtils.R",
  "tallTextIO.R",
  "tallNLP.R",
  "tallOverview.R",
  "tallNetwork.R",
  "tallTopicModel.R",
  "tallEmbeddings.R",
  "tallSentiment.R",
  "tallReport.R",
  "tallVisualization.R",
  "tallLanguages.R",
  "helpContent.R",
  "cssTags.R",
  "header.R",
  "home.R",
  "import.R",
  "edit.R",
  "preprocessing.R",
  "filters_groups.R",
  "overview.R",
  "featureroles.R",
  "keyness.R",
  "collocation.R",
  "words.R",
  "documents.R",
  "doc_classification.R",
  "report.R",
  "settings.R"
)

for (file in files) {
  source(file, local = TRUE)
}

## file upload max size
maxUploadSize <- 1000 # default value
maxUploadSize <- getShinyOption("maxUploadSize", maxUploadSize)
options(shiny.maxRequestSize = maxUploadSize * 1024^2)

param_stay_page <- FALSE

server <- function(input, output, session) {
  ## suppress summarise message
  options(dplyr.summarise.inform = FALSE)

  ## Load analysis packages after UI has rendered
  libraries_analysis()

  ## Setup async execution for AI calls
  future::plan(future::multisession, workers = 2)

  ### Initial values ----
  values <- resetValues()
  statsValues <- updateStats(NULL, "token")

  ## Setting plot values
  values$dpi <- 300
  values$report_dpi <- 72
  values$h <- 7
  values$aspect <- 1.5  # Publication (3:2) default; Wide = 2.0

  ## Load saved graph settings
  home <- homeFolder()
  path_graph_settings <- file.path(home, "tall", ".tall_graph_settings.txt")
  if (file.exists(path_graph_settings)) {
    graph_settings <- readLines(path_graph_settings, warn = FALSE)
    if (length(graph_settings) >= 3) {
      saved_dpi <- suppressWarnings(as.numeric(graph_settings[1]))
      saved_report_dpi <- suppressWarnings(as.numeric(graph_settings[2]))
      saved_h <- suppressWarnings(as.numeric(graph_settings[3]))
      if (!is.na(saved_dpi)) values$dpi <- saved_dpi
      if (!is.na(saved_report_dpi)) values$report_dpi <- saved_report_dpi
      if (!is.na(saved_h)) values$h <- saved_h
      if (length(graph_settings) >= 4) {
        saved_aspect <- suppressWarnings(as.numeric(graph_settings[4]))
        if (!is.na(saved_aspect) && saved_aspect %in% c(1.5, 2.0)) {
          values$aspect <- saved_aspect
        }
      }
    }
  }

  ## Render settings sliders with saved values
  output$dpi_slider <- renderUI({
    sliderTextInput(
      inputId = "dpi",
      label = NULL,
      grid = TRUE,
      force_edges = TRUE,
      choices = c("75", "150", "300", "600"),
      width = "100%",
      selected = as.character(as.integer(isolate(values$dpi)))
    )
  })
  output$report_dpi_slider <- renderUI({
    sliderTextInput(
      inputId = "report_dpi",
      label = NULL,
      grid = TRUE,
      force_edges = TRUE,
      choices = c("72", "150", "300"),
      width = "100%",
      selected = as.character(as.integer(isolate(values$report_dpi)))
    )
  })
  output$h_slider <- renderUI({
    sliderTextInput(
      inputId = "h",
      label = NULL,
      grid = TRUE,
      force_edges = TRUE,
      choices = seq(5, 15),
      width = "100%",
      selected = as.character(as.integer(isolate(values$h)))
    )
  })
  output$aspect_radio <- renderUI({
    radioGroupButtons(
      inputId = "aspect",
      label = NULL,
      choices = c("Publication (3:2)" = "1.5", "Wide (2:1)" = "2"),
      selected = as.character(isolate(values$aspect)),
      justified = TRUE,
      size = "sm",
      status = "primary"
    )
  })

  ## Define JS functions for visNetwork canvas capture (DPI-aware)
  session$onFlushed(
    function() {
      shinyjs::runjs(
        '
      window._captureVisCanvas = function(networkId, dpi, callback) {
        var widget = HTMLWidgets.find("#" + networkId);
        if (!widget || !widget.network) return;
        var network = widget.network;
        var canvas = network.canvas.frame.canvas;
        var targetScale = (dpi || 96) / 96;

        Object.defineProperty(window, "devicePixelRatio", {
          value: targetScale, configurable: true
        });

        network.redraw();
        var dataURL = canvas.toDataURL("image/png");

        delete window.devicePixelRatio;
        network.redraw();

        callback(dataURL);
      };

      window.captureVisExport = function(networkId, filename, dpi) {
        _captureVisCanvas(networkId, dpi, function(dataURL) {
          var link = document.createElement("a");
          link.href = dataURL;
          link.download = filename;
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
        });
      };

      window.captureVisReport = function(networkId, dpi) {
        _captureVisCanvas(networkId, dpi, function(dataURL) {
          Shiny.setInputValue("vis_canvas_report", {
            data: dataURL
          }, {priority: "event"});
        });
      };
    '
      )
    },
    once = TRUE
  )

  saved_message <- "Done!"

  ### SIDEBARMENU ----
  homeServer(input, output, session, values, statsValues)

  ### IMPORT ----
  importServer(input, output, session, values, statsValues)

  ### EDIT ----
  editServer(input, output, session, values, statsValues)

  ### PRE-PROCESSING ----
  preprocessingServer(input, output, session, values, statsValues)

  ### FILTERS & GROUPS ----
  filters_groupsServer(input, output, session, values, statsValues)

  ## FEATURE ROLES ----
  featureRolesServer(input, output, session, values)

  ## OVERVIEW ----
  overviewServer(input, output, session, values, statsValues)

  ### KEYNESS ----
  keynessServer(input, output, session, values)

  ## WORDS ----
  wordsServer(input, output, session, values, statsValues)

  ### COLLOCATION ----
  collocationServer(input, output, session, values, statsValues)

  ## DOCUMENTS ----
  documentsServer(input, output, session, values, statsValues)

  ### Doc Supervised Classification ----
  docClassificationServer(input, output, session, values)

  ## REPORT ----
  reportServer(input, output, session, values)

  ## SETTINGS & UTILITY ----
  settingsServer(input, output, session, values, statsValues)

  ## Header Object ----
  headerServer(input, output, session, values)
} # END SERVER

##  Server ####
files <- c(
  "tallAI.R",
  "tallShot.R",
  "tallFunctions.R",
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
  ## suppress warnings
  options(warn = -1)

  ## suppress summarise message
  options(dplyr.summarise.inform = FALSE)

  ### Initial values ----
  values <- resetValues()
  statsValues <- updateStats(NULL, "token")

  ## Setting plot values
  values$h <- 7
  values$zoom <- 2
  values$dpi <- 300
  #set.seed(5)
  # load("data/regex_list.tall")

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

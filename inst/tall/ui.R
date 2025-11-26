# UI

#  Function source ----
files <- c(
  "libraries.R",
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
  "words.R",
  "documents.R",
  "report.R",
  "settings.R"
)

for (file in files) {
  source(file, local = TRUE)
}

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
    href = "https://tall-app.com",
    target = "_blank",
    tags$img(src = "logo_white.jpg", height = "30", width = "30")
  ),
  strong(" TALL", style = "font-size:17px;")
)

infoTexts <- helpContent()

header <- shinydashboardPlus::dashboardHeader(
  title = title_tall,
  titleWidth = 250,
  controlbarIcon = NULL,
  .list = headerUI()
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
    ## TALL PAGE ----
    homeUI(),

    ## IMPORT ----
    importUI(),

    ## EDIT ----

    ### Split ----
    editUI()$split,

    ### Random selection ----
    editUI()$randomText,

    ### EXTERNAL INFORMATION ----
    editUI()$extInfo,

    ## PRE-PROCESSING ----

    ### Tokenization & PoS Tagging -----
    preprocessingUI()$tokpos,

    ### Special Entities Tagging ----
    preprocessingUI()$specialentities,

    ### Multi-Word Units ----
    preprocessingUI()$mwcreation,
    preprocessingUI()$mwlist,

    ### Custom Term List ----
    preprocessingUI()$custompos,

    ### Synonyms Merging ----
    preprocessingUI()$synonyms,

    ### PoS Selection ----
    preprocessingUI()$postagselection,

    ## FILTER ----
    filters_groupsUI()$filters,

    ## GROUPS ----
    filters_groupsUI()$groups,

    ## FEATURE ROLES ----
    featureRolesUI(),

    ## OVERVIEW ----
    overviewUI(),

    ## KEYNESS ----

    keynessUI(),

    ## WORDS ----

    ### KWIC ----
    wordsUI()$kwic,

    ### Reinert Clustering ----
    wordsUI()$reinert,

    ### Correspondence Analysis ----
    wordsUI()$ca,

    ### Co-Word Analysis ----
    wordsUI()$coword,

    ### Thematic Analysis ----
    wordsUI()$tm,

    ### Embedding Training ----
    wordsUI()$we_training,

    ### Embedding Similarity ----
    wordsUI()$we_similarity,

    ## DOCUMENTS ----

    ### Topic Modeling ----
    #### TM K Choices ----
    documentsUI()$tm_k,

    #### TM Analysis ----
    documentsUI()$tm_analysis,

    ### Polarity Detection ----
    documentsUI()$polarity,

    ### Abstractive Summarization ----
    documentsUI()$abs_summ,

    ### Extractive Summarization ----
    documentsUI()$ext_summ,

    ## REPORT ----
    reportUI(),

    ## SETTINGS ----
    settingsUI()
  )
) # END DASHBOARDBODY


## UserInterface ####
ui <- tagList(
  dashboardPage(header, sidebar, body),
  scrollToTopButton()
)

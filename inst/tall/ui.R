# UI

#  Function source ----
files <- c(
  "libraries.R",
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
  "words.R",
  "collocation.R",
  "documents.R",
  "doc_classification.R",
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
  icon = icon("play")
)

view_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = fa_i(name = "magnifying-glass", prefer_type = "solid")
  # icon = icon("magnifying-glass")
)

export_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon("download")
)
report_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon("plus")
)
save_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 1px;", # margin-top: 15px",
  icon = icon("floppy-disk")
)
back_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon("rotate")
)
x_bttn <- list(
  label = NULL,
  style = "display:block; height: 37px; width: 37px; border-radius: 50%; border: 3px; margin-top: 15px",
  icon = icon("xmark")
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
      icon = fa_i(name = "house-user")
    ),
    menuItemOutput("rest_of_sidebar")
  )
)

## BODY ----

# Build each composite UI once, then extract sub-elements
edit_ui <- editUI()
preprocessing_ui <- preprocessingUI()
words_ui <- wordsUI()
documents_ui <- documentsUI()
fg_ui <- filters_groupsUI()

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
    edit_ui$split,

    ### Random selection ----
    edit_ui$randomText,

    ### EXTERNAL INFORMATION ----
    edit_ui$extInfo,

    ## PRE-PROCESSING ----

    ### Tokenization & PoS Tagging -----
    preprocessing_ui$tokpos,

    ### Special Entities Tagging ----
    preprocessing_ui$specialentities,

    ### Multi-Word Units ----
    preprocessing_ui$mwcreation,
    preprocessing_ui$mwlist,

    ### Custom Term List ----
    preprocessing_ui$custompos,

    ### Synonyms Merging ----
    preprocessing_ui$synonyms,

    ### PoS Selection ----
    preprocessing_ui$postagselection,

    ## FILTER ----
    fg_ui$filters,

    ## GROUPS ----
    fg_ui$groups,

    ## FEATURE ROLES ----
    featureRolesUI(),

    ## OVERVIEW ----
    overviewUI(),

    ## KEYNESS ----

    keynessUI(),

    ## WORDS ----

    ### Collocation Analysis ----
    collocationUI(),

    ### KWIC ----
    #words_ui$kwic,

    ### Reinert Clustering ----
    words_ui$reinert,

    ### Correspondence Analysis ----
    words_ui$ca,

    ### Co-Word Analysis ----
    words_ui$coword,

    ### Thematic Analysis ----
    words_ui$tm,

    ### Embedding Training ----
    words_ui$we_training,

    ### Embedding Similarity ----
    words_ui$we_similarity,

    ## DOCUMENTS ----

    ### Topic Modeling ----
    #### TM K Choices ----
    documents_ui$tm_k,

    #### TM Analysis ----
    documents_ui$tm_analysis,

    #### Doc Classification ----
    docClassificationUI(),

    ### Polarity Detection ----
    documents_ui$polarity,

    ### Abstractive Summarization ----
    documents_ui$abs_summ,

    ### Extractive Summarization ----
    documents_ui$ext_summ,

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

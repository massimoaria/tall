# Pacchetti in Depends (caricati automaticamente all'avvio)
#' @import shiny
#' @import shinydashboardPlus

# Pacchetti in Imports - pervasive use (full namespace)
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import Rcpp
#' @import visNetwork
#' @import udpipe

# Pacchetti in Imports - selective use
#' @importFrom base64enc dataURI
#' @importFrom ca ca
#' @importFrom RSpectra svds
#' @importFrom sparkline sparkline spk_chr
#' @importFrom textrank textrank_sentences
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom ggplot2 ggplot ggsave
#' @importFrom readr write_csv
#' @importFrom strucchange breakpoints
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets alert updatePrettyCheckboxGroup prettyCheckboxGroup
#' @importFrom DT datatable formatStyle formatRound renderDT DTOutput
#' @importFrom jsonlite fromJSON
#' @importFrom later later
#' @importFrom promises promise_all
#' @importFrom rlang sym
#' @importFrom igraph cluster_walktrap graph_from_data_frame membership centr_betw
#' @importFrom plotly add_annotations add_trace add_markers add_polygons
#' @importFrom plotly config event_data event_register layout
#' @importFrom plotly plot_ly plotlyOutput renderPlotly
#' @importFrom Matrix Matrix
#'
#' @import openxlsx
#' @import topicmodels
#' @import pdftools
#' @import tidygraph
#' @import readxl
#' @import readtext
#' @import fontawesome
#' @import shinycssloaders
#' @import curl
#' @import pagedown
#' @import doParallel
#' @import shinyFiles
#' @import ggraph
#' @import ggwordcloud
#' @useDynLib tall
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp evalCpp
## usethis namespace: end
NULL

.onAttach <- function(...) {
  packageStartupMessage(
    "Please note that our software is open source and available for use, distributed under the MIT license.
                        \nFor information and bug reports:
                        - Write a post on https://github.com/massimoaria/tall/issues
                        \nTo start with the tall app, please digit:
tall()\n"
  )
}

# Pacchetti in Depends (caricati automaticamente all'avvio)
#' @import shiny
#' @import shinydashboardPlus

# Pacchetti in Imports (funzioni disponibili solo se esplicitamente chiamate)
#' @import base64enc
#' @import httr2
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import Rcpp
#' @import RSpectra
#' @import openxlsx
#' @import visNetwork
#' @import udpipe
#' @import topicmodels
#' @import pdftools
#' @import textrank
#' @import sparkline
#' @import tidygraph
#' @import readxl
#' @import readtext
#' @import fontawesome
#' @import ca
#' @import shinycssloaders
#' @import curl
#' @import pagedown
#' @import doParallel
#' @import parallel
#' @import shinyFiles
#' @import ggraph
#' @import word2vec
#' @import wordcloud2
#' @import umap
#' @importFrom readr write_csv
#' @importFrom strucchange breakpoints
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets alert updatePrettyCheckboxGroup prettyCheckboxGroup
#' @importFrom DT datatable formatStyle formatRound renderDT DTOutput
#' @importFrom jsonlite fromJSON
#' @importFrom chromote Chromote default_chromote_object
#' @importFrom graphics plot lines text legend
#' @importFrom later later
#' @importFrom promises promise_all
#' @importFrom rlang sym
#' @importFrom igraph cluster_walktrap graph_from_data_frame membership centr_betw
#' @importFrom plotly add_annotations
#' @importFrom plotly api_create as.widget as_widget attrs_selected colorbar
#' @importFrom plotly config embed_notebook event_data event_register event_unregister
#' @importFrom plotly export geom2trace get_figure gg2list ggplotly group2NA hide_colorbar
#' @importFrom plotly hide_guides hide_legend highlight highlight_key knit_print.api_grid
#' @importFrom plotly knit_print.api_plot knit_print.api_grid_local last_plot offline
#' @importFrom plotly orca partial_bundle plotlyOutput renderPlotly plotlyProxy
#' @importFrom plotly plotlyProxyInvoke plotly_build plotly_data plotly_empty
#' @importFrom plotly plotly_example plotly_IMAGE plotly_json plotly_POST
#' @importFrom plotly plot_dendro plot_geo plot_ly plot_mapbox rangeslider raster2uri
#' @importFrom plotly remove_typedarray_polyfill save_image schema showRGB
#' @importFrom plotly signup style subplot TeX toRGB toWebGL to_basic
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

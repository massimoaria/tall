## @import stats
#' @import graphics
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import udpipe
#' @import RSpectra
#' @importFrom rlang sym
#' @importFrom stats chisq.test
## usethis namespace: start
#' @useDynLib tall
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp evalCpp
## usethis namespace: end
NULL

.onAttach<-function(...){
  packageStartupMessage("Please note that our software is open source and available for use, distributed under the MIT license.
                        \nFor information and bug reports:
                        - Write a post on https://github.com/massimoaria/tall/issues
                        \nTo start with the tall app, please digit:
tall()\n")
}

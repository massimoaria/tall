#' @import stats
#' @import graphics
#' @import shiny
#' @import udpipe

.onAttach<-function(...){
  packageStartupMessage("Please note that our software is open source and available for use, distributed under the MIT license.
                        \nFor information and bug reports:
                        - Write a post on https://github.com/massimoaria/tall/issues
                        \nTo start with the tall app, please digit:
tall()\n")
}

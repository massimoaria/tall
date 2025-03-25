#' TALL UI
#'
#' \code{tall} performs text analysis for all.
#'
#' @param port is the TCP port that the application should listen on. If the port is not specified,
#' and the shiny.port option is set (with options(shiny.port = XX)), then that port will be used.
#' Otherwise, use a random port.
#'
#' @param launch.browser If true, the system's default web browser will be launched automatically
#' after the app is started. Defaults to true in interactive sessions only. This value of
#' this parameter can also be a function to call with the application's URL.
#'
#' @param host The IPv4 address that the application should listen on.
#' Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#'
#' @param maxUploadSize is a integer. The max upload file size argument. Default value is 1000 (megabyte)
#'
#' @return
#' No return value, called for side effects.
#'
#' @export

tall <- function(host = "127.0.0.1", port = NULL,
                 launch.browser = TRUE, maxUploadSize = 1000) {
  shinyOptions(maxUploadSize = maxUploadSize)
  suppressWarnings(
    runApp(system.file("tall", package = "tall"), launch.browser = launch.browser, port = port, host = getOption("shiny.host", host))
  )
}

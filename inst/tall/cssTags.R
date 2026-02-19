cssTags <- function() {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "tall-static.css")
    ),
    tags$script(src = "tall-handlers.js")
  )
}

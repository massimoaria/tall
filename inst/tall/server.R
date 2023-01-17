##  Server ####
server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  ###
  values <- reactiveValues()
  values$path <- NULL
  values$txt <- data.frame()

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        values$path = choose.dir(default = readDirectoryInput(session, 'directory'),
                          caption="Choose a directory...")
        updateDirectoryInput(session, 'directory', value = values$path)
      }
    }
  )

  output$directory = renderText({
    readDirectoryInput(session, 'directory')
  })

  DATAloading<- eventReactive(input$load,{
    if (!is.null(values$path)){
      values$txt <- txtImport(values$path)
    }
  })

  output$dataImported <- DT::renderDT({
    DATAloading()
    #values$txt
  })


} # END SERVER

##  Server ####
source("tallFunctions.R", local=TRUE)

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

  DATAloading<- eventReactive(input$runImport,{
    if (!is.null(values$path)){

      txt <- txtImport(values$path, sep="__-__")

      txt <- txt %>%
        group_by(doc_id) %>%
        mutate(docvar1 = gsub(doc_id,"",docvar1)) %>%
        rename(folder = docvar1)
      #save(txt, file="/Users/massimoaria/Rpackages/tall/txt.rdata")

      values$txt <- txt
    }

  })

  output$dataImported <- DT::renderDT({
    DATAloading()
    # txt <- values$txt %>%
    #   mutate(docvar1 = gsub(.data$doc_id,"",paste0(.data$docvar1,.data$docvar2)))
    values$txt

    DT::datatable(values$txt,escape = FALSE,rownames = FALSE, extensions = c("Buttons"),
                  options = list(
                    pageLength = 5,
                    autoWidth = FALSE, scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = list(list(extend = 'pageLength'),
                                   list(extend = 'print')),
                    lengthMenu = list(c(10, 25, 50, -1),
                                      c('10 rows', '25 rows', '50 rows', 'Show all')),
                    columnDefs = list(list(
                      className = 'dt-center', targets = 0:(length(names(values$txt)) - 1)
                    ))
                  ),
                  class = 'cell-border compact stripe'
    )  %>%
      DT::formatStyle(
        names(values$txt),
        backgroundColor = 'white',
        textAlign = 'center',
        fontSize = '70%'
      )

  })


} # END SERVER

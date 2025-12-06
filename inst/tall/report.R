reportUI <- function() {
  tabItem(
    tabName = "report",
    fluidPage(
      fluidRow(
        h3(strong("Report"), align = "center"),
        br(),
      ),
      fluidRow(
        column(
          6,
          offset = 1,
          box(
            title = strong(
              "Select results to include in the Report",
              style = "font-size:20px;color:white;"
            ),
            status = "primary",
            width = 11,
            solidHeader = TRUE,
            tags$style(HTML(
              "
                         .box.box-solid.box-primary>.box-header {
                         background:#4F7942;
                         }
                         .box.box-solid.box-primary{
                         border-bottom-color:black;
                         border-left-color:black;
                         border-right-color:black;
                         border-top-color:black;
                         border-width:2px;
                                         }"
            )),
            uiOutput("reportSheetsUI"),
            tags$style("#reportSheets {font-size:18px;}")
          )
        ), # column(1),
        column(
          2,
          div(
            style = "border-radius: 10px; border-width: 3px; font-size: 10px;",
            align = "center",
            actionBttn(
              inputId = "allSheets",
              label = strong("Select All"),
              icon = icon("ok-circle", lib = "glyphicon"),
              style = "pill",
              color = "primary",
              block = TRUE
            ),
            br(),
            actionBttn(
              inputId = "noSheets",
              label = strong("Deselect All"),
              icon = icon("remove-circle", lib = "glyphicon"),
              style = "pill",
              color = "primary",
              block = TRUE
            ),
            br(),
            hr(),
            actionBttn(
              inputId = "report.save",
              label = strong("Export Report"),
              style = "pill",
              color = "success",
              size = "md",
              block = TRUE,
              no_outline = TRUE,
              icon = icon(name = "download-alt", lib = "glyphicon")
            ),
            br(),
            hr(),
            actionBttn(
              inputId = "deleteAll",
              label = strong("Delete Report"),
              icon = icon("exclamation-sign", lib = "glyphicon"),
              style = "pill",
              color = "danger",
              block = TRUE
            )
          )
        )
      )
    )
  )
}

reportServer <- function(input, output, session, values) {
  ## REPORT ----
  ### Report Save xlsx ----
  observeEvent(
    eventExpr = {
      input$report.save
    },
    handlerExpr = {
      file <- paste("TallReport-", sys.time(), ".xlsx", sep = "")
      file <- destFolder(file, values$wdTall)

      wb_export <- copyWorkbook(values$wb)
      if (nrow(values$list_file) > 0) {
        wb_export <- addScreenWb(df = values$list_file, wb = wb_export) # , width=10, height=7, dpi=300)
      }
      sheetToRemove <- setdiff(sheets(wb_export), input$reportSheets)
      if (length(sheetToRemove) > 0) {
        for (i in sheetToRemove) {
          removeWorksheet(wb_export, i)
        }
      }
      sheetToAdd <- sheets(wb_export)
      for (i in sheetToAdd) {
        setColWidths(
          wb_export,
          sheet = i,
          cols = 1,
          widths = 30,
          hidden = FALSE
        )
      }
      openxlsx::saveWorkbook(wb_export, file = file)
      popUp(title = "Saved in your working folder", type = "saved")
    }
  )

  ### Report UI elements
  observe({
    output$reportSheetsUI <- renderUI({
      prettyCheckboxGroup(
        inputId = "reportSheets",
        label = NULL, # short2long(df=values$dfLabel, myC=values$myChoices),
        choices = short2long(df = values$dfLabel, myC = values$myChoices),
        selected = values$myChoices,
        icon = icon("check"),
        animation = "pulse",
        status = "primary",
        bigger = T,
        fill = TRUE
      )
    })
  })

  observe({
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "reportSheets",
      # label = short2long(df=values$dfLabel, myC=values$myChoices),
      choices = short2long(df = values$dfLabel, myC = values$myChoices),
      selected = if (!input$noSheets) values$myChoices,
      prettyOptions = list(
        animation = "pulse",
        status = "info",
        bigger = T
      )
    )
  })

  observe({
    updatePrettyCheckboxGroup(
      session = getDefaultReactiveDomain(),
      inputId = "reportSheets",
      choices = short2long(df = values$dfLabel, myC = values$myChoices),
      selected = if (input$allSheets) values$myChoices,
      prettyOptions = list(
        animation = "pulse",
        status = "info",
        bigger = T
      )
    )
  })

  observeEvent(input$deleteAll, {
    ask_confirmation(
      inputId = "delete_confirmation",
      title = "Want to confirm?",
      text = "All the results will be removed from the report",
      type = "warning",
      btn_labels = c("CANCEL", "CONFIRM"),
    )
  })

  observeEvent(
    input$delete_confirmation,
    {
      if (isTRUE(input$delete_confirmation)) {
        values$myChoices <- "Empty Report"
        values$list_file <- data.frame(sheet = NULL, file = NULL, n = NULL)
        values$wb <- openxlsx::createWorkbook()
      }
    },
    ignoreNULL = TRUE
  )
}

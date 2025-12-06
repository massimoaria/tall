homeUI <- function() {
  tabItem(
    tabName = "tall",
    fluidRow(
      br(),
      div(
        img(src = "tall_logo.jpg", height = "13%", width = "30%"),
        style = "text-align: center;"
      ),
      # Welcome message
      div(
        p(
          "A user-friendly interface for text analysis with advanced text mining and natural language processing methods.",
          style = "text-align: center; font-size: 16px; max-width: 800px; margin: 0 auto 30px auto; color: #666;"
        )
      ),

      # References
      div(
        style = "max-width: 900px; margin: 0 auto; padding: 20px; background-color: #f8f9fa; border-radius: 10px;",

        # License message
        p(
          "TALL is open-source and freely available for use, distributed under the MIT license.",
          br(),
          "When it is used in a publication, we ask that authors cite the following reference:",
          style = "text-align: center; font-size: 15px; margin-bottom: 20px; color: #333;"
        ),

        # Reference
        div(
          style = "padding: 15px; background-color: white; border-left: 4px solid #4F7942; border-radius: 5px; margin-bottom: 20px;",
          p(
            "Aria, M., Spano, M., D'Aniello, L., Cuccurullo, C., & Misuraca, M. (2025). ",
            strong("tall: Text Analysis for All"),
            ". ",
            em("CRAN"),
            ".",
            br(),
            tags$a(
              href = "https://doi.org/10.32614/CRAN.package.tall",
              target = "_blank",
              icon("link"),
              " https://doi.org/10.32614/CRAN.package.tall",
              style = "color: #4F7942; text-decoration: none;"
            ),
            style = "margin: 0; font-size: 14px; line-height: 1.8; text-align: center;"
          )
        ),

        # License violation warning
        p(
          strong(
            "Failure to properly cite the software is considered a violation of the license."
          ),
          style = "text-align: center; font-size: 14px; margin-bottom: 20px; color: #333;"
        ),

        # Website link
        p(
          "For an introduction and live examples, visit the ",
          tags$a(
            href = "https://www.tall-app.com",
            target = "_blank",
            "TALL website",
            style = "color: #4F7942; text-decoration: none; font-weight: bold;"
          ),
          ".",
          style = "text-align: center; font-size: 14px; color: #333;"
        )
      )
    )
  )
}

homeServer <- function(input, output, session, values, statsValues) {
  ### SIDEBARMENU ----
  output$rest_of_sidebar <- renderMenu({
    if (values$menu == -2) {
      # popup to inform user that at first run you have to set the working folder
      popUpGeneric(
        title = HTML(
          "<span style='color: #4F7942;'><strong>Welcome to TALL</strong></span> 👋"
        ),
        type = "info", # Cambiato da "warning" a "info"
        color = c("#4F7942", "#6CC283"), # Colori brand di TALL con gradiente
        subtitle = HTML(
          "
    <div style='text-align: left; line-height: 1.6; color: #333;'>
      <p style='margin-bottom: 15px;'>
        Before we begin, please select a <strong>default working folder</strong> where all your analyses,
        reports, and results will be saved.
      </p>
      <p style='margin-bottom: 0; color: #666; font-size: 14px;'>
        💡 <em>You can always change this location later in Settings.</em>
      </p>
    </div>
  "
        ),
        btn_labels = "Continue",
        html = TRUE
      )
      updateTabItems(session, "sidebarmenu", "settings")
    }
    if (values$menu == 2) {
      if (length(noGroupLabels(names(values$dfTag))) > 0) {
        values$menu <- 3
      }
    }
    sidebarMenu(.list = menuList(values$menu))
  })

  observeEvent(input$workingfolder, {
    updateTabItems(session, "sidebarmenu", "settings")
  })

  observeEvent(input$runImport, {
    updateTabItems(session, "sidebarmenu", "import_tx")
  })

  observeEvent(input$biblioRun, {
    updateTabItems(session, "sidebarmenu", "import_tx")
  })

  observeEvent(input$tokPosRun, {
    updateTabItems(session, "sidebarmenu", "tokPos")
  })

  observeEvent(input$custTermListRun, {
    updateTabItems(session, "sidebarmenu", "custTermList")
  })

  observeEvent(input$posTagSelectRun, {
    updateTabItems(session, "sidebarmenu", "posTagSelect")
  })

  observeEvent(input$multiwordCreatRun, {
    updateTabItems(session, "sidebarmenu", "multiwordCreat")
  })

  observeEvent(input$multiwordCreatApply, {
    updateTabItems(session, "sidebarmenu", "multiwordCreat")
  })

  observeEvent(input$multiwordCreatBack, {
    updateTabItems(session, "sidebarmenu", "multiwordCreat")
  })

  observeEvent(input$multiwordListRun, {
    updateTabItems(session, "sidebarmenu", "multiwordByList")
  })

  observeEvent(input$multiwordListBack, {
    updateTabItems(session, "sidebarmenu", "multiwordByList")
  })

  # Settings button handler
  observeEvent(
    input$go_to_settings,
    {
      updateTabItems(session, "sidebarmenu", "settings")
    },
    ignoreInit = TRUE
  )

  # Settings button handler
  observeEvent(
    input$go_to_postagging,
    {
      updateTabItems(session, "sidebarmenu", "tokPos")
    },
    ignoreInit = TRUE
  )
}

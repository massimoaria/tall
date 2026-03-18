homeUI <- function() {
  tabItem(
    tabName = "tall",
    fluidPage(
      tags$style(HTML(
        "
      .compact-home {
        padding: 30px;
        max-width: 1300px;
        margin: 0 auto;
      }
      .header-section {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 50px;
        margin-bottom: 40px;
      }
      .logo-container img {
        height: 380px;
        width: auto;
        border-radius: 15px;
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      }
      .title-container {
        text-align: left;
      }
      .main-title {
        font-size: 80px;
        font-weight: 700;
        color: #2c3e50;
        margin: 0 0 15px 0;
        line-height: 1;
        letter-spacing: -1px;
      }
      .subtitle {
        font-size: 28px;
        color: #7f8c8d;
        margin: 0 0 18px 0;
        font-weight: 400;
      }
      .ai-badge {
        background: linear-gradient(135deg, #4F7942 0%, #6CC283 100%);
        color: white;
        padding: 8px 20px;
        border-radius: 20px;
        font-size: 15px;
        font-weight: 700;
        display: inline-block;
        margin-bottom: 15px;
        letter-spacing: 0.5px;
      }
      .ai-description {
        font-size: 22px;
        color: #495057;
        margin-top: 10px;
        line-height: 1.5;
        font-weight: 400;
      }
      .content-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 30px;
        margin-top: 30px;
      }
      .citation-box {
        background: linear-gradient(135deg, #4F7942 0%, #6CC283 100%);
        padding: 30px;
        border-radius: 15px;
        box-shadow: 0 6px 25px rgba(79, 121, 66, 0.3);
        color: #1a1a2e;
      }
      .citation-title {
        font-size: 20px;
        font-weight: 700;
        margin-bottom: 18px;
        display: flex;
        align-items: center;
        gap: 10px;
        color: #1a1a2e;
      }
      .citation-text {
        font-size: 16px;
        line-height: 1.8;
        background: rgba(255,255,255,0.7);
        padding: 20px;
        border-radius: 10px;
        border-left: 4px solid rgba(26, 26, 46, 0.3);
        color: #1a1a2e;
        font-weight: 400;
      }
      .citation-warning {
        margin-top: 15px;
        font-size: 15px;
        display: flex;
        align-items: center;
        gap: 8px;
        color: #1a1a2e;
        font-weight: 600;
      }
      .info-panel {
        display: flex;
        flex-direction: column;
        gap: 20px;
      }
      .info-card {
        background: #f8f9fa;
        padding: 25px;
        border-radius: 12px;
        border-left: 5px solid #4F7942;
        font-size: 16px;
        color: #495057;
        line-height: 1.6;
      }
      .info-card-title {
        font-weight: 700;
        color: #2c3e50;
        margin-bottom: 10px;
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 18px;
      }
      .website-link {
        color: #4F7942;
        font-weight: 700;
        text-decoration: none;
        border-bottom: 2px solid #4F7942;
      }
      .website-link:hover {
        color: #3a5c31;
        border-bottom-color: #3a5c31;
      }
    "
      )),

      div(
        class = "compact-home",

        # Header with logo and title side by side
        div(
          class = "header-section",
          div(
            class = "logo-container",
            img(src = "tall_logo.jpg")
          ),
          div(
            class = "title-container",
            h1(
              "TALL",
              class = "main-title"
            ),
            p("Text Analysis for All", class = "subtitle"),
            br(),
            span("TALL AI POWERED", class = "ai-badge"),
            p(
              class = "ai-description",
              "A user-friendly interface for text analysis with advanced text mining",
              br(),
              "and natural language processing methods"
            )
          )
        ),

        # Grid with citation and info
        div(
          class = "content-grid",

          # Citation Box
          div(
            class = "citation-box",
            div(
              class = "citation-title",
              icon("quote-left"),
              "How to Cite"
            ),
            div(
              class = "citation-text",
              strong("Aria, M., Spano, M., D'Aniello, L., Cuccurullo, C., & Misuraca, M."),
              " (2026). TALL: Text analysis for all - an interactive R-shiny application for exploring, modeling, and visualizing textual data. ",
              em("SoftwareX"),
              ", 34, 102590.",
              br(), br(),
              tags$a(
                icon("file-alt"), " Full Paper (Open Access)",
                href = "https://www.sciencedirect.com/science/article/pii/S2352711026000841",
                target = "_blank",
                style = "color: #1a1a2e; font-weight: 600; text-decoration: none; margin-right: 20px;"
              ),
              tags$a(
                icon("paperclip"), " Supplementary Material",
                href = "https://ars.els-cdn.com/content/image/1-s2.0-S2352711026000841-mmc1.pdf",
                target = "_blank",
                style = "color: #1a1a2e; font-weight: 600; text-decoration: none;"
              )
            ),
            div(
              class = "citation-warning",
              icon("exclamation-triangle"),
              "Failure to cite is a license violation"
            )
          ),

          # Info Panel
          div(
            class = "info-panel",

            div(
              class = "info-card",
              div(
                class = "info-card-title",
                icon("certificate"),
                "Open Source & Free"
              ),
              "Distributed under the MIT license. Free for academic and commercial use."
            ),

            div(
              class = "info-card",
              div(
                class = "info-card-title",
                icon("book"),
                "Documentation"
              ),
              "Visit the ",
              tags$a(
                "TALL website",
                href = "https://www.tall-app.com",
                target = "_blank",
                class = "website-link"
              ),
              " for tutorials and examples."
            )
          )
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
          "<span style='color: #4F7942;'><strong>Welcome to TALL</strong></span>"
        ),
        type = "info",
        color = c("#4F7942", "#6CC283"),
        subtitle = HTML(
          "
    <div style='text-align: left; line-height: 1.6; color: #333;'>
      <p style='margin-bottom: 15px;'>
        Before we begin, please select a <strong>default working folder</strong> where all your analyses,
        reports, and results will be saved.
      </p>
      <p style='margin-bottom: 0; color: #666; font-size: 14px;'>
        <em>You can always change this location later in Settings.</em>
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

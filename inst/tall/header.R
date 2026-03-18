headerUI <- function() {
  list(
    tags$li(class = "dropdown", tags$a(uiOutput("dataGroupedBy"))), # Rimosso HTML/paste
    tags$li(class = "dropdown", tags$a(uiOutput("dataFilteredBy"))),
    # tags$li(class = "dropdown", tags$a(uiOutput("termSelected"))),
    tags$li(
      class = "dropdown",
      actionButton(
        inputId = "go_to_postagging",
        label = tags$a(
          HTML(
            paste(uiOutput("termSelected"))
          ),
          style = "color: #fff;"
        ),
        # icon = icon("gear", lib = "font-awesome"),
        style = "background: transparent;
               border: none;
               color: #fff;
               font-size: 16px;  /* Increased from 16px to 20px */
               margin-top: 7px;
               cursor: pointer;",
        title = "PoS Tagging"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        title = "Total downloads from CRAN",
        icon("cloud-arrow-down", lib = "font-awesome"),
        uiOutput("header_download_count", inline = TRUE)
      )
    ),
    tags$li(
      class = "dropdown",
      actionButton(
        "show_donate",
        label = NULL,
        icon = icon("heart"),
        style = "background: none; border: none; color: white; font-size: 15px; padding: 15px; margin: 0; transition: background 0.3s;",
        onmouseover = "this.style.background='rgba(0,0,0,0.1)';",
        onmouseout = "this.style.background='none';"
      )
    ),

    # Author CARD ----
    tags$li(
      class = "dropdown",
      actionButton(
        "show_team",
        label = NULL,
        icon = icon("users"),
        style = "background: none; border: none; color: white; font-size: 15px; padding: 15px; margin: 0;"
      )
    ),

    tags$li(
      class = "dropdown",
      actionButton(
        "show_credits",
        label = NULL,
        icon = icon("cube"),
        style = "background: none; border: none; color: white; font-size: 15px; padding: 15px; margin: 0; transition: background 0.3s;",
        onmouseover = "this.style.background='rgba(0,0,0,0.1)';",
        onmouseout = "this.style.background='none';"
      )
    ),

    # Settings Button - uses actionLink to trigger server-side tab change
    tags$li(
      class = "dropdown",
      actionButton(
        inputId = "go_to_settings",
        label = NULL,
        icon = icon("gear", lib = "font-awesome"),
        style = "background: transparent;
               border: none;
               color: #fff;
               font-size: 20px;  /* Increased from 16px to 20px */
               margin-top: 7px;
               cursor: pointer;",
        title = "Settings"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$style(".main-header .logo {height: 53px}")
    )
  )
}

headerServer <- function(input, output, session, values) {
  ## CRAN download count (async, non-blocking) ----
  output$header_download_count <- renderUI({
    downloads <- suppressWarnings(total_downloads("tall"))
    tags$span(
      HTML(format_abbreviated(downloads)),
      style = "margin-left: 5px; font-weight: bold;"
    )
  })

  ## Team Card ----
  observeEvent(input$show_team, {
    showModal(modalDialog(
      title = div(
        style = "text-align: center; margin-bottom: 10px;",
        icon("users", style = "color: #3c8dbc; font-size: 26px;"),
        span(
          " TALL Team",
          style = "font-size: 24px; font-weight: bold; color: #2c3e50; margin-left: 10px;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),

      # Creators Section
      div(
        style = "padding: 20px;",

        div(
          style = "margin-bottom: 35px;",
          h4(
            icon("star", style = "color: #f39c12;"),
            " Creators",
            style = "color: #2c3e50; margin-bottom: 20px; border-bottom: 2px solid #3c8dbc; padding-bottom: 10px; font-weight: bold;"
          ),

          div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 25px; margin-bottom: 10px;",

            createAuthorCard(
              name = "Massimo Aria",
              title = "Full Professor of Statistics for Social Sciences",
              affiliation = "University of Naples Federico II",
              url = "https://www.massimoaria.com",
              photo = "images/team/massimo_aria.jpg",
              scholar = FALSE
            ),

            createAuthorCard(
              name = "Maria Spano",
              title = "Associate Professor of Statistics for Social Sciences",
              affiliation = "University of Naples Federico II",
              url = "https://scholar.google.com/citations?user=kh_hGT0AAAAJ&hl=it&oi=ao",
              photo = "images/team/maria_spano.jpg",
              scholar = TRUE
            ),

            createAuthorCard(
              name = "Luca D'Aniello",
              title = "Assistant Professor of Statistics for Social Sciences",
              affiliation = "University of Naples Federico II",
              url = "https://scholar.google.com/citations?user=IXJxh0MAAAAJ&hl=it&oi=ao",
              photo = "images/team/luca_daniello.jpg",
              scholar = TRUE
            )
          )
        ),

        # Contributors Section
        div(
          h4(
            icon("hands-helping", style = "color: #95a5a6;"),
            " Contributors",
            style = "color: #2c3e50; margin-bottom: 20px; border-bottom: 2px solid #95a5a6; padding-bottom: 10px; font-weight: bold;"
          ),

          div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 25px;",

            createAuthorCard(
              name = "Corrado Cuccurullo",
              title = "Full Professor of Corporate Governance",
              affiliation = "University of Campania Luigi Vanvitelli",
              url = "https://www.corradocuccurullo.com/",
              photo = "images/team/corrado_cuccurullo.jpg",
              scholar = FALSE
            ),

            createAuthorCard(
              name = "Michelangelo Misuraca",
              title = "Associate Professor of Statistics for Social Sciences",
              affiliation = "University of Salerno",
              url = "https://scholar.google.com/citations?user=WdivjAUAAAAJ&hl=it",
              photo = "images/team/michelangelo_misuraca.jpg",
              scholar = TRUE
            )
          )
        )
      )
    ))
  })

  ## Link Card ----
  observeEvent(input$show_credits, {
    showModal(modalDialog(
      title = div(
        style = "text-align: center; margin-bottom: 10px;",
        icon("cube", style = "color: #667eea; font-size: 26px;"),
        span(
          " Credits",
          style = "font-size: 24px; font-weight: bold; color: #2c3e50; margin-left: 10px;"
        )
      ),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),

      div(
        style = "padding: 20px;",
        # K-Synth Card
        tags$a(
          href = "https://www.k-synth.com",
          target = "_blank",
          style = "text-decoration: none; display: block; margin-bottom: 20px;",
          div(
            style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 25px; border-radius: 12px; box-shadow: 0 5px 20px rgba(102, 126, 234, 0.3); transition: all 0.3s; cursor: pointer;",
            onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 8px 25px rgba(102, 126, 234, 0.4)';",
            onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 5px 20px rgba(102, 126, 234, 0.3)';",

            div(
              style = "display: flex; align-items: center; margin-bottom: 15px;",
              div(
                style = "width: 60px; height: 60px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 20px; box-shadow: 0 3px 10px rgba(0,0,0,0.1);",
                icon(
                  "watchman-monitoring",
                  style = "color: #667eea; font-size: 28px;"
                )
              ),
              div(
                div(
                  "K-Synth srl",
                  style = "color: white; font-size: 22px; font-weight: bold; margin-bottom: 5px;"
                ),
                div(
                  "Academic Spin-Off of University of Naples Federico II",
                  style = "color: rgba(255,255,255,0.9); font-size: 14px;"
                )
              )
            ),

            div(
              style = "color: rgba(255,255,255,0.95); font-size: 13px; line-height: 1.6; margin-bottom: 12px;",
              'A science-centric information & intelligence Specialist Firm, specialized in the research, consulting, and production of "knowledge", extracted and analyzed from a heterogeneous plurality of large volumes of data, through quantitative, statistical, and data science methods.'
            ),

            div(
              icon("external-link-alt", style = "margin-right: 5px;"),
              "Visit K-Synth",
              style = "color: white; font-size: 13px; font-weight: 600; display: inline-block; padding: 8px 16px; background: rgba(255,255,255,0.2); border-radius: 6px;"
            )
          )
        ),

        # GitHub Card
        tags$a(
          href = "https://github.com/massimoaria/tall",
          target = "_blank",
          style = "text-decoration: none; display: block;",
          div(
            style = "background: linear-gradient(135deg, #24292e 0%, #000000 100%); padding: 25px; border-radius: 12px; box-shadow: 0 5px 20px rgba(0, 0, 0, 0.3); transition: all 0.3s; cursor: pointer;",
            onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 8px 25px rgba(0, 0, 0, 0.4)';",
            onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 5px 20px rgba(0, 0, 0, 0.3)';",

            div(
              style = "display: flex; align-items: center; margin-bottom: 15px;",
              div(
                style = "width: 60px; height: 60px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin-right: 20px; box-shadow: 0 3px 10px rgba(0,0,0,0.1);",
                icon("github", style = "color: #24292e; font-size: 28px;")
              ),
              div(
                div(
                  "GitHub",
                  style = "color: white; font-size: 22px; font-weight: bold; margin-bottom: 5px;"
                ),
                div(
                  "Open Source Repository",
                  style = "color: rgba(255,255,255,0.9); font-size: 14px;"
                )
              )
            ),

            div(
              style = "color: rgba(255,255,255,0.95); font-size: 13px; line-height: 1.6; margin-bottom: 12px;",
              "Access the source code, report issues, contribute to development, and stay updated with the latest releases."
            ),

            div(
              icon("external-link-alt", style = "margin-right: 5px;"),
              "View Repository",
              style = "color: white; font-size: 13px; font-weight: 600; display: inline-block; padding: 8px 16px; background: rgba(255,255,255,0.2); border-radius: 6px;"
            )
          )
        )
      )
    ))
  })

  ## Donation Card ----
  observeEvent(input$show_donate, {
    showModal(modalDialog(
      title = div(
        style = "text-align: center; margin-bottom: 10px;",
        icon("heart", style = "color: #e74c3c; font-size: 26px;"),
        span(
          " Support TALL",
          style = "font-size: 24px; font-weight: bold; color: #2c3e50; margin-left: 10px;"
        )
      ),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),

      div(
        style = "padding: 20px;",

        # Messaggio principale
        div(
          style = "text-align: center; margin-bottom: 30px;",

          div(
            style = "font-size: 16px; color: #2c3e50; line-height: 1.8; margin-bottom: 20px;",
            "TALL is a free and open-source tool developed with passion by our research team.",
            tags$br(),
            "Your support helps us maintain and improve the project."
          ),

          div(
            style = "font-size: 18px; color: #e74c3c; font-weight: bold; margin-bottom: 25px;",
            icon("heart", style = "margin-right: 8px;"),
            "Help us keep TALL free for everyone"
          )
        ),

        # Card donazione principale
        tags$a(
          href = "https://www.bibliometrix.org/home/index.php/donation",
          target = "_blank",
          style = "text-decoration: none; display: block;",
          div(
            style = "background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%); padding: 30px; border-radius: 12px; box-shadow: 0 5px 20px rgba(231, 76, 60, 0.3); transition: all 0.3s; cursor: pointer;",
            onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 8px 25px rgba(231, 76, 60, 0.4)';",
            onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 5px 20px rgba(231, 76, 60, 0.3)';",

            div(
              style = "text-align: center;",

              # Icona grande
              div(
                style = "width: 80px; height: 80px; border-radius: 50%; background: white; display: flex; align-items: center; justify-content: center; margin: 0 auto 20px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
                icon(
                  "hand-holding-heart",
                  style = "color: #e74c3c; font-size: 35px;"
                )
              ),

              # Titolo
              div(
                "Make a Donation",
                style = "color: white; font-size: 24px; font-weight: bold; margin-bottom: 12px;"
              ),

              # Descrizione
              div(
                "Support the development and maintenance of TALL",
                style = "color: rgba(255,255,255,0.95); font-size: 14px; margin-bottom: 20px; line-height: 1.6;"
              ),

              # Button
              div(
                icon("external-link-alt", style = "margin-right: 8px;"),
                "Donate Now",
                style = "color: #e74c3c; font-size: 16px; font-weight: 700; display: inline-block; padding: 12px 30px; background: white; border-radius: 8px; box-shadow: 0 3px 10px rgba(0,0,0,0.1);"
              )
            )
          )
        ),

        # Info aggiuntive
        div(
          style = "margin-top: 25px; padding: 20px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #e74c3c;",

          div(
            icon("info-circle", style = "color: #3498db; margin-right: 8px;"),
            strong("Why donate?", style = "color: #2c3e50; font-size: 14px;")
          ),

          div(
            style = "margin-top: 10px; color: #7f8c8d; font-size: 13px; line-height: 1.6;",
            tags$ul(
              style = "margin: 10px 0; padding-left: 20px;",
              tags$li("Keep the tool free and accessible"),
              tags$li("Fund new features and improvements"),
              tags$li("Support open-source research software"),
              tags$li("Enable continuous maintenance and updates")
            )
          )
        )
      )
    ))
  })
}

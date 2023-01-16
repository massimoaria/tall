##  Server ####  
server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  ###
  
  #' values=reactiveValues()
  #' values$file=FALSE
  #' values$selected_member <- ""
  #' 
  #' ### DEPARTMENT PAGE ----
  #' 
  #' observeEvent(input$btn_run,
  #'              {
  #'                if((input$select_department=="null")) {
  #'                  showModal(modalDialog(
  #'                    title = "Warning!",
  #'                    tags$b("Please select a department"),
  #'                    size = 's'
  #'                  ))
  #'                }
  #'              })
  #' 
  #' observeEvent(input$btn_run,{
  #'   # carica i dati 
  #'   load_data(input,values)
  #'   if (!isTRUE(values$file)){
  #'     showModal(modalDialog(
  #'       title = "Warning!",
  #'       tags$b("Department data not available"),
  #'       size = 's'
  #'     ))
  #'   }else{
  #'     
  #'     output$dept_page <- renderUI({
  #'       
  #'       if (input$select_department!="null"){
  #'         fluidPage(
  #'           fluidRow(
  #'             
  #'             div(img(src = "dises_logo.png", height = '300px',width='400px'), style="text-align: center;")),
  #'           h1(strong("DiSES - Department of Economics and Statistics"), align="center"),
  #'           br(),
  #'           div(p("Website:",
  #'                 em(a(" http://www.dises.unina.it/", 
  #'                      href = "http://www.dises.unina.it/", target="_blank")), 
  #'                 style="text-align:center; font-size:17px;")),         
  #'           br(),
  #'           div(p("Powered by ",
  #'                 em(a("K-Synth", 
  #'                      href = "https://k-synth.com/", target="_blank")), 
  #'                 style="text-align:center; font-size:17px;")),
  #'           br(),
  #'           h2("Main information about the organization", style="text-align: center;"),
  #'           br()
  #'         )
  #'       }
  #'       
  #'     })
  #'     
  #'     # The organization ----
  #'     
  #'     load_data(input, values)
  #'     dip_research(input,values)
  #'     
  #'     output$strutturati <- renderValueBox({
  #'       valueBox(
  #'         value = p("Professors & Researchers", style = 'font-size:17px;color:white;'),
  #'         subtitle = p(strong(formatC(values$dip$ric, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'         icon = fa_i(("users")),
  #'         color = "navy")
  #'       
  #'     })
  #'     
  #'     output$po <- renderValueBox({
  #'       valueBox(
  #'         value = p("Full Professors", style = 'font-size:17px;color:white;'),
  #'         subtitle = p(strong(formatC(values$dip$ric2$ricPO, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'         icon = fa_i(("user")),
  #'         color = "navy")
  #'       
  #'     })
  #'     
  #'     output$pa <- renderValueBox({
  #'       valueBox(
  #'         value = p("Associate Professors", style = 'font-size:17px;color:white;'),
  #'         subtitle = p(strong(formatC(values$dip$ric2$ricPA, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'         icon = fa_i(("user")),
  #'         color = "navy")
  #'       
  #'     })
  #'     output$rt <- renderValueBox({
  #'       valueBox(
  #'         value = p("Researchers", style = 'font-size:17px;color:white;'),
  #'         subtitle = p(strong(formatC(values$dip$ric2$ricRT, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'         icon = fa_i(("user")),
  #'         color = "navy")
  #'       
  #'     })
  #'   }
  #' })
  #' 
  #' 
  #' ### RESEARCH PAGE ----
  #' 
  #' ### Dept name ----
  #' output$dip_name <- renderUI({
  #'   
  #'   fluidRow(
  #'     box(title=h2(toupper(values$dip_name)),
  #'         width = 12)
  #'   )
  #'   
  #' })
  #' 
  #' ## Research products ValueBoxes ----
  #' 
  #' output$tot_Q1 <- renderValueBox({
  #'   valueBox(
  #'     value = p("Articles in Q1", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$rivQ1, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'     icon = fa_i(("book")),
  #'     color = "navy")
  #'   
  #' })
  #' output$tot_prod <- renderValueBox({
  #'   valueBox(
  #'     value = p("Products", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$tot, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'     icon = fa_i(("book")),
  #'     color = "navy")
  #'   
  #' })
  #' output$prod_riv <- renderValueBox({
  #'   valueBox(
  #'     value = p("Journal articles", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$riv, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'     color = "navy")
  #'   
  #' })
  #' output$prod_rivS <- renderValueBox({
  #'   valueBox(
  #'     value = p("Art on ANVUR Journals", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$rivS, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'     color = "navy")
  #'   
  #' })
  #' output$prod_rivA <- renderValueBox({
  #'   valueBox(
  #'     value = p("Art on ANVUR 'A' Journals", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$rivA, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'     color = "navy")
  #'   
  #' })
  #' output$conf_pro <- renderValueBox({
  #'   valueBox(
  #'     value = p("Conference proceedings", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$conf, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'     color = "navy")
  #'   
  #' })
  #' output$monog <- renderValueBox({
  #'   valueBox(
  #'     value = p("Monographs", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$mon, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'     color = "navy")
  #'   
  #' })
  #' output$ch_Book <- renderValueBox({
  #'   valueBox(
  #'     value = p("Book chapters", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$ch, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'     color = "navy")
  #'   
  #' })
  #' 
  #' output$intColl <- renderValueBox({
  #'   valueBox(
  #'     value = p("International Collaboration", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(paste(formatC(values$dip$int_perc, digits = 0, format = "f"),"%", sep="")), style = 'font-size:30px;color:white;'),
  #'     color = "navy")
  #'   
  #' })
  #' output$tot_cit <- renderValueBox({
  #'   valueBox(
  #'     value = p("Total citation", style = 'font-size:17px;color:white;'),
  #'     subtitle = p(strong(formatC(values$dip$TC, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'     color = "navy")
  #'   
  #' })
  #' 
  #' 
  #' ### Research plots: Annual scientific production ----
  #' 
  #' output$outcome_var <- renderUI({
  #'   
  #'   selectInput("outcome_var", 
  #'               label=NULL,
  #'               choices= c("Publications", names(table(values$iris_df$`Tipologia (collezione)`))),selected = "Publications")
  #' })
  #' 
  #' AnnProd <- eventReactive(input$outcome_var,{
  #'   if(input$outcome_var=="Publications"){field="all"} 
  #'   else{
  #'     field <- input$outcome_var
  #'   }
  #'   production(df=values$iris_df, field= field, type="dip")
  #' })
  #' 
  #' output$plot_prod <- renderPlotly({
  #'   res_ASPplot <- AnnProd()
  #'   # values$ASPplot <- AnnProd()
  #'   # 
  #'   #'#EFEFEF'
  #'   plot.ly(res_ASPplot,flip=FALSE, side="r", aspectratio=1, size=0.15)
  #' }
  #' )
  #' 
  #' ### Research plots: Citation trends WoS & Scopus ----
  #' output$outcome_var2 <- renderUI({
  #'   
  #'   selectInput("outcome_var2", 
  #'               label=NULL,
  #'               choices =  c("Citation by Web of Science"="wos","Citation by Scopus"="scopus"))
  #' })
  #' 
  #' CitPlot <- eventReactive(input$outcome_var2,{
  #'   if(input$outcome_var2=="Citation by Web of Science"){field="wos"} 
  #'   else{
  #'     field <- input$outcome_var2
  #'   }
  #'   citation(df=values$iris_df, field= field)  
  #' })
  #' 
  #' output$plot_cit <- renderPlotly({
  #'   
  #'   res_CITplot <- CitPlot()
  #'   
  #'   plot.ly(res_CITplot,flip=FALSE, side="r", aspectratio=1.7, size=0.10)
  #'   
  #' })
  #' 
  #' 
  #' ### TEACHING PAGE ----
  #' # Info about courses ----
  #' 
  #' output$teaching_page <- renderUI({
  #'   
  #'   LT <- courses(values,"LT")
  #'   LM <- courses(values,"LM")
  #'   MS <- courses(values,"MS")
  #'   PHD <- courses(values,"PHD")
  #'   
  #'   fluidPage(
  #'     fluidRow(
  #'       
  #'       # LAUREA TRIENNALE
  #'       box(
  #'         column(12,
  #'                valueBoxOutput("cdl_lt",  width = "50vh")),
  #'         box(
  #'           title = "Info about Undergraduate courses", status = "primary", solidHeader = TRUE,
  #'           collapsible = TRUE, collapsed = TRUE, width = 12,
  #'           tabBox(
  #'             width = "100%",
  #'             tabPanel(
  #'               title="Courses", LT)
  #'           )
  #'         )
  #'       ),
  #'       
  #'       # LAUREA MAGISTRALE
  #'       box(
  #'         column(12,valueBoxOutput("cdl_lm",  width = "50vh")),
  #'         box(
  #'           title = "Info about Graduate courses", status = "primary", solidHeader = TRUE,
  #'           collapsible = TRUE, collapsed = TRUE, width = 12,
  #'           tabBox(
  #'             width = "100%",
  #'             tabPanel(
  #'               title="Courses", LM))
  #'         )
  #'       )
  #'     ),
  #'     
  #'     fluidRow(
  #'       
  #'       # MASTER
  #'       box(
  #'         column(12,valueBoxOutput("ms", width = "50vh")),
  #'         box(
  #'           title = "Info about Master courses", status = "primary", solidHeader = TRUE,
  #'           collapsible = TRUE, collapsed = TRUE, width = 12,
  #'           tabBox(
  #'             width = "100%",
  #'             tabPanel(
  #'               title="Courses", MS))
  #'         )
  #'       ),
  #'       
  #'       # DOTTORATO
  #'       box(
  #'         column(12,valueBoxOutput("phd", width = "100vh")),
  #'         box(
  #'           title = "Info about PhD courses", status = "primary", solidHeader = TRUE,
  #'           collapsible = TRUE, collapsed = TRUE, width = 12,
  #'           tabBox(
  #'             width = "100%",
  #'             tabPanel(
  #'               title="Courses", PHD))
  #'         )
  #'       )
  #'     )
  #'   )
  #'   
  #' })
  #' 
  #' ### Teaching ValueBoxes ----
  #' 
  #' output$cdl_lt <- renderValueBox({
  #'   valueBox(
  #'     value = p("Undergraduate Degree", style = 'font-size:25px;color:white;', align="center"),
  #'     subtitle = p(strong(formatC(values$dip$cdl$LT, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'     icon = fa_i(name="user-graduate"),
  #'     color = "aqua")
  #'   
  #' })
  #' output$cdl_lm <- renderValueBox({
  #'   valueBox(
  #'     value = p("Graduate Degree", style = 'font-size:25px;color:white;', align="center"),
  #'     subtitle = p(strong(formatC(values$dip$cdl$LM, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'     icon = fa_i(("user-graduate")),
  #'     color = "aqua")
  #'   
  #' })
  #' output$ms <- renderValueBox({
  #'   valueBox(
  #'     value = p("Master's Degree", style = 'font-size:25px;color:white;', align="center"),
  #'     subtitle = p(strong(formatC(values$dip$cdl$MS, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'     icon = fa_i(("user-graduate")),
  #'     color = "light-blue")
  #'   
  #' })
  #' output$phd <- renderValueBox({
  #'   valueBox(
  #'     value = p("PhD", style = 'font-size:25px;color:white;', align="center"),
  #'     subtitle = p(strong(formatC(values$dip$cdl$PHD, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'     icon = fa_i(("user-graduate")),
  #'     color = "light-blue")
  #'   
  #' })
  #' 
  #' ### MEMBERS PAGE ----
  #' 
  #' ### Select a members ----
  #' 
  #' output$select_members <- renderUI({
  #'   
  #'   au_list <- sort(unique(values$iris_df$Ricercatore))
  #'   
  #'   selectInput(inputId = "select_members",
  #'               label=tags$em(style="color: black;","Please, select a member"), 
  #'               choices = c(" "="null", au_list),
  #'               selected = "null",  width = 250,
  #'               multiple = F)
  #' })
  #' 
  #' observeEvent(input$btn_build_report,{
  #'   au_research(input, values)
  #'   
  #'   # Author name ----
  #'   
  #'   output$au_name <- renderUI({
  #'     
  #'     # userBox(
  #'     #   title = "",
  #'     #   label = values$selected_member,
  #'     #   subtitle = "",
  #'     #   width = 12,
  #'     #   type = 2,
  #'     #   #src = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
  #'     #   color = "yellow",
  #'     #   "Some text here!",
  #'     #   footer = "The footer here!"
  #'     # )
  #'     fluidRow(
  #'       box(title=h2(values$selected_member),
  #'           footer = values$role_member,
  #'           width = 12)
  #'     )
  #'     
  #'   })
  #'   
  #'   ### Author's products ----
  #'   
  #'   ## Author Products ValueBoxes ----
  #'   
  #'   output$au_tot_Q1 <- renderValueBox({
  #'     valueBox(
  #'       value = p("Articles in Q1", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au$rivQ1, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'       icon = fa_i(("book")),
  #'       color = "navy"
  #'     )
  #'     
  #'   })
  #'   output$au_tot_prod <- renderValueBox({
  #'     valueBox(
  #'       value = p("Products", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au$tot, digits = 0, format = "f")), style = 'font-size:30px;color:white;',align="center"),
  #'       icon = fa_i(("book")),
  #'       color = "navy"
  #'     )
  #'     
  #'   })
  #'   output$au_prod_riv <- renderValueBox({
  #'     valueBox(
  #'       value = p("Journal articles", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au$riv, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'       color = "navy"
  #'     )
  #'     
  #'   })
  #'   output$au_prod_rivS <- renderValueBox({
  #'     valueBox(
  #'       value = p("Art on ANVUR Journals", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au$rivS, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'       color = "navy"
  #'     )
  #'     
  #'   })
  #'   output$au_prod_rivA <- renderValueBox({
  #'     valueBox(
  #'       value = p("Art on ANVUR 'A' Journals", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au$rivA, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'       color = "navy"
  #'     )
  #'     
  #'   })
  #'   output$au_conf_pro <- renderValueBox({
  #'     valueBox(
  #'       value = p("Conference proceedings", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au$conf, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'       color = "navy"
  #'     )
  #'     
  #'   })
  #'   output$au_monog <- renderValueBox({
  #'     valueBox(
  #'       value = p("Monographs", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au$mon, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'       color = "navy"
  #'     )
  #'     
  #'   })
  #'   output$au_ch_Book <- renderValueBox({
  #'     valueBox(
  #'       value = p("Book chapters", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au$ch, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'       color = "navy"
  #'     )
  #'     
  #'   })
  #'   output$au_intColl <- renderValueBox({
  #'     valueBox(
  #'       value = p("International Collaboration", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(paste(formatC(values$au$int_perc, digits = 0, format = "f"),"%", sep="")), style = 'font-size:30px;color:white;'),
  #'       color = "navy")
  #'     
  #'   })
  #'   output$au_TC <- renderValueBox({
  #'     valueBox(
  #'       value = p("Total citations", style = 'font-size:17px;color:white;'),
  #'       subtitle = p(strong(formatC(values$au_openalex_full_stat$tot_TC, digits = 0, format = "f")), style = 'font-size:30px;color:white;'),
  #'       color = "navy")
  #'     
  #'   })
  #'   
  #'   ### Author's plot: Annual Scientific Production ----
  #'   
  #'   output$au_outcome_var <- renderUI({
  #'     list(
  #'       selectInput("au_outcome_var",
  #'                   label=NULL,#em("Select a publication type"),
  #'                   choices= c("Publications", names(table(values$iris_au$`Tipologia (collezione)`))),selected = "Publications")
  #'     )
  #'   })
  #'   
  #'   AU_AnnProd <- eventReactive(input$au_outcome_var,{
  #'     if(input$au_outcome_var=="Publications"){field="all"} 
  #'     else{
  #'       field <- input$au_outcome_var
  #'     }
  #'     production(df=values$iris_au, 
  #'                field= field, type="author")
  #'   })
  #'   
  #'   output$au_plot_prod <- renderPlotly({
  #'     res_au_ASPplot <- AU_AnnProd()
  #'     plot.ly(res_au_ASPplot,flip=FALSE, side="r", aspectratio=1, size=0.15)
  #'     
  #'   })
  #'   
  #'   ### Author's plot: Total citation per Year ----
  #'   
  #'   output$au_plot_cit <- renderPlotly({
  #'     res_au_CITplot <- au_citation(values)
  #'     plot.ly(res_au_CITplot,flip=FALSE, side="r", aspectratio=1, size=0.15)
  #'     
  #'   })
  #'   
  #'   
  #' })
  
  
} # END SERVER

#### Google GEMINI API ####
gemini_ai <- function(image = NULL,
                      prompt = "Explain these images",
                      model = "2.0-flash",
                      type = "png",
                      retry_503 = 3) {

  # Default config
  generation_config <- list(
    temperature = 1,
    maxOutputTokens = 8192,
    topP = 0.95,
    topK = 40,
    seed = 1234
  )

  # Build URL
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  # Base structure of parts
  parts <- list(list(text = prompt))

  # Handle images if provided
  if (!is.null(image)) {
    if (!is.vector(image)) image <- as.vector(image)
    mime_type <- paste0("image/", type)

    for (img_path in image) {
      if (!file.exists(img_path)) {
        return(paste0("❌ Error: Image file does not exist: ", img_path))
      }

      image_data <- tryCatch(
        base64enc::base64encode(img_path),
        error = function(e) {
          return(NULL)
        }
      )

      if (is.null(image_data)) {
        return(paste0("❌ Failed to encode image: ", img_path))
      }

      parts <- append(parts, list(
        list(inline_data = list(
          mime_type = mime_type,
          data = image_data
        ))
      ))
    }
  }

  # Assemble request body
  request_body <- list(
    contents = list(
      parts = parts
    ),
    generationConfig = generation_config
  )

  # Retry loop
  for (attempt in seq_len(retry_503)) {

    # Build and send request
    req <- request(url) |>
      req_url_query(key = api_key) |>
      req_headers("Content-Type" = "application/json") |>
      req_body_json(request_body)

    resp <- tryCatch(
      req_perform(req),
      error = function(e) {
        return(list(error = TRUE, message = paste("❌ Request failed with error:", e$message)))
      }
    )

    # Handle connection-level error
    # if (is.list(resp) && isTRUE(resp$error)) {
    #   return(resp$message)
    # }

    # Retry on HTTP 503
    if (resp$status_code == 503) {
      if (attempt < retry_503) {
        message(paste0("⚠️ HTTP 503 (Service Unavailable) - retrying in 2 seconds (attempt ", attempt, "/", retry_503, ")..."))
        Sys.sleep(2)
        next
      } else {
        return(
          paste0(
            "❌ HTTP 503: Service Unavailable.\n",
            "The Google Gemini servers are currently overloaded or under maintenance.\n",
            "All retry attempts failed (", retry_503, "). Please try again later."
          )
        )
      }
    }

    # Other HTTP errors
    if (resp$status_code != 200) {
      msg <- tryCatch({
        parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
        parsed$error$message
      }, error = function(e) {
        "Service unavailable or unexpected error."
      })

      return(paste0("❌ HTTP ", resp$status_code, ": ", msg))
    }

    # Successful response
    candidates <- httr2::resp_body_json(resp)$candidates
    outputs <- unlist(lapply(candidates, \(c) c$content$parts))
    return(outputs)
  }
}

setGeminiAPI <- function(api_key) {
  # 1. Controllo validità dell'API key
  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    message("❌ API key must be a non-empty string.")
    return(NA)
  }

  if (nchar(api_key) < 10) {
    message("❌ API key seems too short. Please verify your key.")
    return(NA)
  }

  # 2. Mostra solo gli ultimi 4 caratteri per feedback
  last_chars <- 4
  last <- substr(api_key, max(1, nchar(api_key) - last_chars + 1), nchar(api_key))

  # 3. Imposta la variabile d'ambiente
  Sys.setenv(GEMINI_API_KEY = api_key)

  return(paste0(paste0(rep("*",nchar(api_key)-4), collapse=""),last,collapse=""))
}

showGeminiAPI <- function(){
  api_key <- Sys.getenv("GEMINI_API_KEY")
  last_chars <- 4
  last <- substr(api_key, max(1, nchar(api_key) - last_chars + 1), nchar(api_key))
  last <- paste0(paste0(rep("*",nchar(api_key)-4), collapse=""),last,collapse="")
  return(last)
}

load_api_key <- function(path = path_tall) {
  if (file.exists(path)) {
    key <- readLines(path, warn = FALSE)
    if (nchar(key) >= 10) {
      Sys.setenv(GEMINI_API_KEY = key)
      return(TRUE)
    }
  }
  return(FALSE)
}

## gemini prompt for images
geminiPromptImage <- function(obj, type="vis", prompt="Explain the topics in this map", key, desc = NULL){
  if (key){
    if (!is.null(desc)) prompt <- paste0(prompt,desc,collapse=". ")
    tmpdir <- tempdir()
    owd <- setwd(tmpdir)
    on.exit(setwd(owd))
    file_path <- paste0(tempfile(),".png",collapse="")
    switch(type,
           "vis"={
             suppressWarnings(plot2png(obj, filename = file_path, zoom = 2, type="vis"))
           },
           "plotly"={
             suppressWarnings(plot2png(obj, filename = file_path, zoom = 2, type="plotly"))
           },
           "text"={
             file_path <- NULL
           },
           "multi"={
             file_path <- obj
           },
           "html"={
             html_name <- tempfile(fileext = ".html")
             htmltools::save_html(obj, html_name)
             tallShot(html_name, zoom = 2, file = file_path)
           },
           "ggplot2"={
             ggsave(filename = file_path, plot = obj, dpi = 72, height = 7, width = 14, bg = "transparent")
           })

    res <- gemini_ai(image = file_path,
                     prompt = prompt)
  } else {
    res <- 'To access this feature, please provide a valid Gemini AI API key. You can obtain your API key by visiting the official <a href="https://aistudio.google.com/" target="_blank">Google AI Studio website</a>.'
  }

  return(res)
}

geminiOutput <- function(title = "", content = "", values){
  if (is.null(content)){
    content <- "Click the 'Ask TALL AI' button to analyze the visual outputs and provide an automatic interpretation of your results based on the graphs.\n
This AI-powered feature leverages Google Gemini to help you understand patterns and insights emerging from your contextual analysis.\n\n\n\n\n\n\n"
  }
  box(
    title = title,
    width = 12,
    status = "info",
    solidHeader = TRUE,
    div(
      style = "white-space: pre-wrap; background-color:#f9f9f9; padding:15px; border:1px solid #ccc; border-radius:5px; max-height:400px; overflow-y: auto;",
      HTML(content)
    ),
    br(),
    em("You can modify or enrich the proposed prompt with additional context or details about your analysis to help TALL AI generate a more accurate and meaningful interpretation."),
    textAreaInput(
      inputId = "gemini_additional",
      label = NULL,
      value = values$gemini_model_parameters,
      placeholder = "You can provide additional context or details about your analysis to help TALL AI generate a more accurate and meaningful interpretation.",
      rows = 3,
      width = "100%"
    ),
    fluidRow(
      column(4, align = "center",
             actionButton("gemini_btn", "Ask TALL AI", style = "color: white;",
                          icon(name = "microchip", lib = "font-awesome"),
                          width = "80%")
      ),
      column(4, align = "center",
             actionButton("copy_btn", "Copy", style = "color: white;", icon = icon("clipboard"),
                          width = "80%")
      ),
      column(4, align = "center",
             actionButton("save_btn", "Save", style = "color: white;", icon = icon("download"),
                          width = "80%")
      )
    )
  )
}

# gemini2clip <- function(values, activeTab){
#   switch(activeTab,
#          "wordCont" = {values$contextGemini},
#          "w_reinclustering" = {"Not yet implemented"},
#          "ca" = {values$caGemini},
#          "w_networkCooc" = {values$w_networkGemini},
#          "w_networkTM" = {values$w_networkTMGemini},
#          "w_w2v_similarity" = {values$w_w2vGemini},
#          "d_tm_estim" = {values$tmGemini},
#          "d_polDet" = {values$d_polDet_Gemini}
#   )
# }

geminiGenerate <- function(values, activeTab, gemini_additional, gemini_model_parameters){
  if (gemini_additional!="") {
    desc <- paste0(values$corpus_description, gemini_additional, gemini_model_parameters, collapse=". ")
  } else {
    desc <- values$corpus_description
  }
  switch(activeTab,
         "wordCont" = {
           req(values$contextNetwork)
           values$contextGemini <- geminiPromptImage(obj=values$contextNetwork, type="vis",
                                                     prompt="Explain the topics in this 'word in context' network",
                                                     key=values$geminiAPI, desc=desc)
         },
         "w_reinclustering" = {"Not yet implemented"},
         "ca" = {
           req(values$plotCA)
           values$caGemini <- geminiPromptImage(obj=values$plotCA, type="plotly",
                                                prompt="Provide an interpretation of this 'correspondence analysis' map",
                                                key=values$geminiAPI, desc=desc)
         },
         "w_networkCooc" = {
           req(values$netVis)
           values$w_networkGemini <- geminiPromptImage(obj=values$netVis, type="vis",
                                                       prompt="Provide an interpretation of this 'word co-occurrence' network",
                                                       key=values$geminiAPI, desc=desc)
         },
         "w_networkTM" = {
           req(values$TMmap)
           values$w_networkTMGemini <- geminiPromptImage(obj=plotTM(values$TM$df, size = 0.5, gemini = TRUE),
                                                         type="plotly",
                                                         prompt="Provide an interpretation of this 'strategic map'",
                                                         key=values$geminiAPI, desc=desc)
         },
         "w_w2v_similarity" = {
           req(values$w2vNetworkPlot)
           values$w_w2vGemini <- geminiPromptImage(obj=values$w2vNetworkPlot, type="vis",
                                                   prompt="Provide an interpretation of this 'cosine similarity' map.
                                           The map has been created on a word embedding matrix by word2vec model.",
                                                   key=values$geminiAPI, desc=desc)
         },
         "d_tm_estim" = {
           req(values$TMestim_result)
           tmPlot <- topicGplot(values$TMestim_result$beta, nPlot = 10, type = "beta")
           values$tmGemini <- geminiPromptImage(obj=tmPlot, type="ggplot2",
                                                prompt="Interpret this graph showing the beta probabilities P(word|topic) from a topic modeling analysis.
                                                Each value represents how strongly a word is associated with a specific topic.
                                                Focus on identifying which words best characterize each topic and whether topics appear well differentiated.",
                                                key=values$geminiAPI, desc=desc)
         },
         "d_polDet" = {
           req(values$docPolPlots)
           p1 <- values$sentimentPieChart
           p2 <- values$docPolPlots$positive %>% layout(showlegend = FALSE)
           p3 <- values$docPolPlots$negative %>% layout(showlegend = FALSE)

           files <- unlist(lapply(c("topic1","topic2","topic3"), function(x){
             paste0(tempdir(),"/",x,".png")
           }))

           suppressWarnings(plot2png(p1, filename = files[1], zoom = 2, type="plotly"))
           suppressWarnings(plot2png(p2, filename = files[2], zoom = 2, type="plotly"))
           suppressWarnings(plot2png(p3, filename = files[3], zoom = 2, type="plotly"))

           values$d_polDet_Gemini <- geminiPromptImage(obj=files,
                                                       type="multi",
                                                       prompt="Provide an interpretation of these three plots generated from a Polarity Detection Analysis.
                                                       The first plot is a pie chart showing the distribution of documents by polarity label (Very positive, positive, neutral, negative, and very negative).
                                                       The second and third plots display the frequency distributions of the top words found in positive and negative documents, respectively. Focus on identifying any notable differences in word usage and the overall sentiment trends.",
                                                       key=values$geminiAPI, desc=desc)
         }
  )
  return(values)
}

geminiParameterPrompt <- function(values, activeTab, input){

  txt <- paste0("The analysis was perfomed on ", values$generalTerm, " extracted from the original corpus. ")

  switch(activeTab,
         "wordCont" = {
           req(values$contextNetwork)
           txt <- paste0(txt, "The context windows consists of of the ",
                         input$wordsContBefore, " words preceding and the ",
                         input$wordsContAfter, " words following the target word.")
         },
         "w_reinclustering" = {"Not yet implemented"},
         "ca" = {
           req(values$plotCA)
           txt <- paste0(txt, "The occurrences of the most ", input$nCA, " frequent words were measured across ",
                         input$groupCA, ". The words were then grouped into ",
                         input$nClustersCA, " clusters using hierarchical clustering.")
         },
         "w_networkCooc" = {
           req(values$netVis)
           txt <- paste0(txt, "The co-occurrences of the most ", input$nMax, " frequent words were measured across ",
                         input$w_groupNet, ". The co-occurrences were normalized using the ",input$normalizationCooc ,
                         " index. The words were then grouped using Walktrap community detection algorithm.")
         },
         "w_networkTM" = {
           req(values$TMmap)
           txt <- paste0(txt, "The co-occurrences of the most ", input$nMaxTM , " frequent words were measured across ",
                         input$w_groupTM, ". The co-occurrences were normalized using the association index. The words were then grouped using Walktrap community detection algorithm.")
         },
         "w_w2v_similarity" = {
           req(values$w2vNetworkPlot)
           txt <- paste0(txt, "The ", input$w_w2v_similarityN ," most frequent target words were selected, and for each of them, the 10 words with the highest cosine similarity were identified.")
         },
         "d_tm_estim" = {
           req(values$TMestim_result)
           txt <- paste0(txt, "The Topic Model was estimated using the ",input$nTmEstim," frequent words identified by ", input$top_byEstim," measure.")
         },
         "d_polDet" = {
           req(values$docPolPlots)
           txt
         },
         {""}
  )
  return(txt)
}

geminiWaitingMessage <- function(values, activeTab){

  messageTxt <- "\n\nPlease Wait\n\nThinking.....\n\n"

  switch(activeTab,
         "wordCont" = {
           req(values$contextNetwork)
           values$contextGemini <- messageTxt
         },
         "w_reinclustering" = {"Not yet implemented"},
         "ca" = {
           req(values$plotCA)
           values$caGemini <- messageTxt
         },
         "w_networkCooc" = {
           req(values$netVis)
           values$w_networkGemini <- messageTxt
         },
         "w_networkTM" = {
           req(values$TMmap)
           values$w_networkTMGemini <- messageTxt
         },
         "w_w2v_similarity" = {
           req(values$w2vNetworkPlot)
           values$w_w2vGemini <- messageTxt
         },
         "d_tm_estim" = {
           req(values$TMestim_result)
           values$tmGemini <- messageTxt
         },
         "d_polDet" = {
           req(values$docPolPlots)
           values$d_polDet_Gemini <- messageTxt
         }
  )
  return(values)
}

geminiSave <- function(values, activeTab, type=c("clip","save")){

  switch(activeTab,
         "wordCont" = {
           req(values$contextNetwork)
           gemini <- values$contextGemini
         },
         "w_reinclustering" = {"Not yet implemented"},
         "ca" = {
           req(values$plotCA)
           gemini <- values$caGemini
         },
         "w_networkCooc" = {
           req(values$netVis)
           gemini <- values$w_networkGemini
         },
         "w_networkTM" = {
           req(values$TMmap)
           gemini <- values$w_networkTMGemini
         },
         "w_w2v_similarity" = {
           req(values$w2vNetworkPlot)
           gemini <- values$w_w2vGemini
         },
         "d_tm_estim" = {
           req(values$TMestim_result)
           gemini <- values$tmGemini
         },
         "d_polDet" = {
           req(values$docPolPlots)
           gemini <- values$d_polDet_Gemini
         }
  )
  if (type=="save") {
    cat(gemini, file=paste0(values$wdTall,"/TallAI_",activeTab,".txt"))
  } else {
      return(gemini)
    }
}

copy_to_clipboard <- function(x) {
  # Check the operating system
  sys_info <- Sys.info()
  os_type <- tolower(sys_info["sysname"])

  # Convert the object to a string if it is not already
  if (!is.character(x)) {
    x <- capture.output(print(x))
  }

  # Copy to clipboard based on the operating system
  if (os_type == "windows") {
    writeClipboard(x)
  } else if (os_type == "darwin") {  # macOS
    con <- pipe("pbcopy", "w")
    writeLines(x, con)
    close(con)
  } else if (os_type == "linux") {
    # Use xclip or xsel, if available
    if (nzchar(Sys.which("xclip"))) {
      con <- pipe("xclip -selection clipboard", "w")
      writeLines(x, con)
      close(con)
    } else if (nzchar(Sys.which("xsel"))) {
      con <- pipe("xsel --clipboard --input", "w")
      writeLines(x, con)
      close(con)
    } else {
      stop("Neither 'xclip' nor 'xsel' are available. Please install one of them to use the clipboard on Linux.")
    }
  } else {
    stop("Unrecognized or unsupported operating system.")
  }
}

string_to_sentence_df <- function(input_string) {
  # Validate that the input is a character string of length 1
  if (!is.character(input_string) || nchar(input_string) < 2) {
    return(data.frame(TALL_AI = "TALL AI was not performed", stringsAsFactors = FALSE, row.names = NULL))
  }

  # Split the string by newline character "\n"
  split_sentences <- unlist(strsplit(input_string, split = "\n", fixed = TRUE))

  # Trim whitespace from each sentence
  cleaned_sentences <- trimws(split_sentences)

  # Remove empty entries
  cleaned_sentences <- cleaned_sentences[cleaned_sentences != ""]

  # Convert to data frame
  sentence_df <- data.frame(TALL_AI = cleaned_sentences, stringsAsFactors = FALSE, row.names = NULL)

  return(sentence_df)
}

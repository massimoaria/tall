#### Google GEMINI API ####
gemini_ai <- function(image = NULL,
                      prompt = "Explain these images",
                      model = "2.0-flash",
                      type = "png",
                      retry_503 = 5,
                      api_key=NULL,
                      outputSize = "medium"){

  switch(outputSize,
         "small" = {
           generation_config <- list(
             temperature = 1,
             maxOutputTokens = 8192,
             topP = 0.95,
             topK = 40,
             seed = 1234
           )
         },
         "medium" = {
           generation_config <- list(
             temperature = 1,
             maxOutputTokens = 16384, #8192,
             topP = 0.95,
             topK = 40,
             seed = 1234
           )
         },
         "large" = {
           generation_config <- list(
             temperature = 1,
             maxOutputTokens = 32768, #8192,
             topP = 0.95,
             topK = 40,
             seed = 1234
           )
         }
  )

  # # Default config
  # generation_config <- list(
  #   temperature = 1,
  #   maxOutputTokens = 16384,#8192,
  #   topP = 0.95,
  #   topK = 40,
  #   seed = 1234
  # )

  # Build URL
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  if (is.null(api_key)) api_key <- Sys.getenv("GEMINI_API_KEY")

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
        return(list(status_code=stringr::str_extract(e$message, "(?<=HTTP )\\d+")|> as.numeric(),
                    error = TRUE, message = paste("❌ Request failed with error:", e$message)))
      }
    )

    # # Handle connection-level error
    # if (is.list(resp) && isTRUE(resp$error)) {
    #   return(resp$message)
    # }

    # Retry on HTTP 503 or 429
    if (resp$status_code %in% c(429,503)) {
      if (attempt < retry_503) {
        message(paste0("⚠️ HTTP 503 (Service Unavailable) - retrying in 2 seconds (attempt ", attempt, "/", retry_503, ")..."))
        Sys.sleep(2)
        next
      } else {
        return(
          paste0(
            "❌ HTTP 503: Service Unavailable.\n",
            "The Google Gemini servers are currently overloaded or under maintenance.\n",
            "All retry attempts failed (", retry_503, "). Please try again in a few minutes. Alternatively, consider using a different AI model with lower latency."
          )
        )
      }
    }

    # HTTP errors
    # 400 api key not valid
    if (resp$status_code == 400) {
      msg <- tryCatch({
        parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
        parsed$error$message
      }, error = function(e) {
        "Please check your API key. It seems to be not valid!"
      })
      return(paste0("❌ HTTP ", resp$status_code, ": ", msg))
    }
    # Other HTTP errors
    if (resp$status_code != 200) {
      msg <- tryCatch({
        parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp))
        parsed$error$message
      }, error = function(e) {
        "Service unavailable or unexpected error. Please check your API key and usage limit."
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
  apiCheck <- gemini_ai(image = NULL,
                        prompt = "Hello",
                        model = "2.0-flash",
                        type = "png",
                        retry_503 = 5, api_key=api_key)

  contains_http_error <- grepl("HTTP\\s*[1-5][0-9]{2}", apiCheck)

  if (contains_http_error) {
    return(list(valid=FALSE, message="❌ API key seems be not valid! Please, check it or your connection."))
  }

  if (is.null(api_key) || !is.character(api_key) || nchar(api_key) == 0) {
    return(list(valid=FALSE, message="❌ API key must be a non-empty string."))
  }

  if (nchar(api_key) < 10) {
    return(list(valid=FALSE, message="❌ API key seems too short. Please verify your key."))
  }

  # 2. Mostra solo gli ultimi 4 caratteri per feedback
  last_chars <- 4
  last <- substr(api_key, max(1, nchar(api_key) - last_chars + 1), nchar(api_key))

  # 3. Imposta la variabile d'ambiente
  Sys.setenv(GEMINI_API_KEY = api_key)

  return(list(valid=TRUE, message=paste0(paste0(rep("*",nchar(api_key)-4), collapse=""),last,collapse="")))
}

showGeminiAPI <- function(){
  api_key <- Sys.getenv("GEMINI_API_KEY")
  last_chars <- 4
  last <- substr(api_key, max(1, nchar(api_key) - last_chars + 1), nchar(api_key))
  last <- paste0(paste0(rep("*",nchar(api_key)-4), collapse=""),last,collapse="")
  return(last)
}

load_api_key <- function(path = path_gemini_key) {
  if (file.exists(path)) {
    key <- readLines(path, warn = FALSE)
    if (nchar(key) >= 10) {
      Sys.setenv(GEMINI_API_KEY = key)
      return(TRUE)
    }
  }
  return(FALSE)
}

loadGeminiModel = function(file){
  # load info about model type and output size
  if (file.exists(file)){
    model <- readLines(file, warn = FALSE)
  } else {
    model <- c("2.0-flash","medium")
  }
  if (length(model== 1)) {
    model <- c(model,"medium")
  }
  return(model)
}

saveGeminiModel = function(model, file){
  if (file.exists(file)) {
    file.remove(file)
  }
  writeLines(model, file)
}

geminiOutput <- function(title = "", content = "", values){
  if (is.null(content)){
    content <- "Click the 'Ask Biblio AI' button to analyze the visual outputs and provide an automatic interpretation of your results based on the graphs.\n
This AI-powered feature leverages Google Gemini to help you understand patterns and insights emerging from your contextual analysis.\n  \n  \n \n  \n  \n  \n"
  }
  box(
    title = title,
    width = 12,
    status = "info",
    solidHeader = TRUE,
    div(
      id = "typing-box",
      style = "white-space: pre-wrap; background-color:#f9f9f9; padding:15px; border:1px solid #ccc; border-radius:5px; max-height:400px; overflow-y: auto;",
      #HTML(text_to_html(content))
      HTML(gemini_to_html(content))
      #HTML(content)
    ),
    br(),
    em("You can modify or enrich the proposed prompt with additional context or details about your analysis to help 'TALL AI' generate a more accurate and meaningful interpretation."),
    textAreaInput(
      inputId = "gemini_additional",
      label = NULL,
      value = values$gemini_model_parameters,
      placeholder = "You can provide additional context or details about your analysis to help 'TALL AI' generate a more accurate and meaningful interpretation.",
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
             downloadButton(outputId = "save_btn", label = "Save", icon = icon("download"),
                            style = "width: 80%;")
      )
    )
  )
}

tallAiPrompts <- function(values, activeTab){

  ## Role definition for Gemini as Biblio AI assistant
  promptInitial <- paste0("You are TALL AI, an expert virtual assistant specializing in text analysis and computational linguistics, integrated into the R TALL software. ",
                          "Your task is to support researchers in interpreting and critically discussing the results of ",
                          "their quantitative textual analyses, offering insights, contextual explanations, and guidance for data-driven interpretation. ")

  ## Specific prompts for each analysis
  switch(activeTab,
         "overview"={
           prompt <- paste0("A user has analyzed a text corpus and obtained the following summary statistics. Provide a detailed interpretation of these results based on the values provided. ",
           "Corpus metrics: ", merge_df_to_string(values$VbData),
           "Structure your response in the following way: ",
           "1.  **General Summary**: Based on the data provided, start with a paragraph that summarizes the main characteristics of the corpus. ",
           "2.  **Corpus Size and Structure**: Explain the meaning of `Documents`, `Tokens`, `Types`, `Lemma`, and `Sentences` based on their provided values. Relate the `Tokens` count to the `Types` count to give a first impression of vocabulary size. ",
           "3.  **Document and Sentence Characteristics**: Interpret the provided averages (`Avg Length`) and standard deviations (`SD Length`) for documents and sentences. Explain what the specific `SD` values imply about the consistency of lengths across the corpus (e.g., a low SD suggests uniform lengths, a high SD suggests high variability). ",
           "4.  **Lexical Richness and Variety**: Analyze the provided values for the lexical richness indices: `TTR (%)`, `Hapax (%)`, `Guiraud Index`, and `Yule's K`. For each, explain what its value indicates about the richness of the vocabulary. Clarify why multiple indices are useful (e.g., correcting for corpus length). ",
           "5.  **Stylistic and Distributional Indices**: Explain the meaning of the given values for `Lexical Density`, `Nominal Ratio`, and `Gini Index`. For each metric, interpret what the provided value suggests about the text's nature (e.g., for `Lexical Density`: informational vs. conversational; for `Nominal Ratio`: descriptive vs. narrative style; for `Gini Index`: word distribution). ",
           "6.  **Conclusion and Potential Next Steps**: End with a summary conclusion and suggest to the user what subsequent analyses might be interesting based on the specific results they obtained.")
         },
         "wordCont" = {
           prompt <- paste0("You have to provide an interpratation of a Words in Context (KWIC - Keyword in Context) analysis. ",
                            "Your expertise covers concordance analysis, semantic usage patterns, contextual meaning interpretation, and discourse structure analysis. ",
                            "You help users understand: ",
                            " - Semantic shifts and meaning variations of target words ",
                            " - Collocational patterns and linguistic relationships ",
                            " - Contextual distribution and usage frequency ",
                            " - Thematic contexts and discourse patterns ",
                            " - Pragmatic and stylistic variations. ",
                            "Always provide insights that are linguistically grounded, contextually relevant, and actionable for text analysis research.")
         },
         "ca"={
           prompt <- paste0("You have to interpret Correspondence Analysis (CA) results from the TALL software package in R. ",
                            "Your expertise covers multivariate text analysis, dimensional interpretation, word-document associations, and visualization of textual relationships.",
                            "You help users understand: ",
                            "  - Dimensional structure and variance explained",
                            "  - Word positioning and semantic relationships",
                            "  - Document clustering and thematic groupings",
                            "  - Distance interpretation and proximity patterns",
                            "  - Quality of representation and contribution analysis",
                            "  - Biplot interpretation and association patterns. ",
                            "Always provide interpretations that are statistically sound, linguistically meaningful, and practically actionable for text analysis research.")
         },
         "w_networkCooc"={
           prompt <- paste0("You have to provide an interpretation of Word Co-occurrence Analysis results. ",
                            "Your expertise covers network analysis, semantic relationships, collocational patterns, and lexical association interpretation. ",
                            "You help users understand: ",
                            " - Network topology and connectivity patterns ",
                            " - Semantic clustering and thematic groupings ",
                            " - Word association strengths and significance ",
                            " - Community detection and lexical fields ",
                            " - Centrality measures and influential words ",
                            " - Network density and structural properties ",
                            " - Contextual relationships and discourse patterns. ",
                            "Always provide interpretations that are linguistically informed, statistically grounded, and actionable for text analysis research.")
         },
         "w_networkTM" ={
           prompt <- paste0("You have to provide an interpretation of Thematic Map analysis. ",
                            "Your expertise covers bibliometric-inspired thematic analysis, topic clustering, strategic diagram interpretation, and conceptual structure analysis. ",
                            "You help users understand: ",
                            " - Strategic positioning of topics (Hot, Basic, Niche, Peripheral) ",
                            " - Callon Centrality and Density measures interpretation ",
                            " - Topic development and maturity assessment ",
                            " - Conceptual structure and thematic relationships ",
                            " - Research field evolution and strategic implications ",
                            " - Community detection and topic clustering ",
                            " - Comparative thematic analysis across time periods or groups ",
                            "Always provide interpretations that are methodologically sound, theoretically grounded, and strategically actionable for research and content analysis.")
         },
         "w_w2v_similarity"={
           prompt <- paste0("You have to provide an interpretation of Word Similarity Network analysis results from the TALL software package in R. ",
                            "Your expertise covers word embedding analysis, semantic similarity interpretation, network topology analysis, and community detection in embedding spaces. ",
                            "You help users understand: ",
                            " - Word2vec embedding relationships and semantic similarity patterns ",
                            " - Network structure and community clusters based on cosine similarity ",
                            " - Semantic neighborhoods and lexical fields ",
                            " - Embedding space topology and word positioning ",
                            " - Community detection results and thematic groupings ",
                            " - Similarity thresholds and network connectivity patterns ",
                            " - Comparative analysis of different embedding models (CBOW vs Skip-gram) ",
                            "Always provide interpretations that are linguistically informed, computationally grounded, and actionable for semantic analysis research. ")
         },
         "d_tm_estim"={
           prompt <- paste0("You have to provide an interpretation of Topic Modeling analysis results from the TALL software package in R. ",
                            "Your expertise covers Latent Dirichlet Allocation (LDA), probabilistic topic modeling, beta and theta probability distributions, document-topic associations, and semantic structure discovery. ",
                            "You help users understand: ",
                            " - LDA model interpretation and topic coherence assessment ",
                            " - Beta probabilities (word-topic distributions) and topic characterization ",
                            " - Theta probabilities (document-topic distributions) and document classification ",
                            " - Topic quality evaluation and model diagnostics ",
                            " - Semantic structure discovery and thematic organization ",
                            " - Model parameter optimization and validation ",
                            "Always provide interpretations that are statistically sound, linguistically meaningful, and methodologically rigorous for probabilistic text analysis.")
         },
         "d_polDet"={
           prompt <- paste0("You have to provide an interpretation of Topic Modeling analysis results from the TALL software package in R. ",
                                   "Your expertise covers Latent Dirichlet Allocation (LDA), probabilistic topic modeling, beta and theta probability distributions, document-topic associations, and semantic structure discovery.  ",
                                   "You help users understand:   ",
                                   " - LDA model interpretation and topic coherence assessment ",
                                   " - Beta probabilities (word-topic distributions) and topic characterization ",
                                   " - Theta probabilities (document-topic distributions) and document classification ",
                                   " - Topic quality evaluation and model diagnostics ",
                                   " - Semantic structure discovery and thematic organization ",
                                   " - Model parameter optimization and validation. ",
                                   " Always provide interpretations that are statistically sound, linguistically meaningful, and methodologically rigorous for probabilistic text analysis.")
         },
         {
           prompt <- paste0("Provide an interpretation of this plot creted with 'TALL R Package'")
         })
  prompt <- paste0(promptInitial,prompt)
#if (!activeTab %in% c("mainInfo", "thematicMap", "trendTopic")) prompt <- paste0(prompt, " Provide also scientific references about the methodological description")
return(prompt)
}



gemini2clip <- function(values, activeTab){
  switch(activeTab,
         "wordCont" = {values$contextGemini},
         "w_reinclustering" = {"Not yet implemented"},
         "ca" = {values$caGemini},
         "w_networkCooc" = {values$w_networkGemini},
         "w_networkTM" = {values$w_networkTMGemini},
         "w_w2v_similarity" = {values$w_w2vGemini},
         "d_tm_estim" = {values$tmGemini},
         "d_polDet" = {values$d_polDet_Gemini}
  )
}

geminiGenerate <- function(values, activeTab, gemini_additional, gemini_model_parameters, input){
  if (gemini_additional!="") {
    desc <- paste0(values$corpus_description, gemini_additional, gemini_model_parameters, collapse=". ")
  } else {
    desc <- paste0(values$corpus_description, gemini_model_parameters, collapse=". ")
  }
  prompt <- tallAiPrompts(values, activeTab)
  switch(activeTab,
         "overview" ={
           req(values$VbData)
           values$overviewGemini <- geminiPromptImage(obj=values$VbData, type="text",
                                                     prompt=prompt,
                                                     key=values$geminiAPI, desc=desc, values=values)
         },
         "wordCont" = {
           req(values$contextNetwork)
           values$contextGemini <- geminiPromptImage(obj=values$contextNetwork, type="vis",
                                                     prompt=prompt,
                                                     key=values$geminiAPI, desc=desc, values=values)
         },
         "w_reinclustering" = {"Not yet implemented"},
         "ca" = {
           req(values$plotCA)
           values$caGemini <- geminiPromptImage(obj=values$plotCA, type="plotly",
                                                prompt=prompt,
                                                key=values$geminiAPI, desc=desc, values=values)
         },
         "w_networkCooc" = {
           req(values$netVis)
           values$w_networkGemini <- geminiPromptImage(obj=values$netVis, type="vis",
                                                       prompt=prompt,
                                                       key=values$geminiAPI, desc=desc, values=values)
         },
         "w_networkTM" = {
           req(values$TMmap)
           values$w_networkTMGemini <- geminiPromptImage(obj=plotTM(values$TM$df, size = 0.5, gemini = TRUE),
                                                         type="plotly",
                                                         prompt=prompt,
                                                         key=values$geminiAPI, desc=desc, values=values)
         },
         "w_w2v_similarity" = {
           req(values$w2vNetworkPlot)
           values$w_w2vGemini <- geminiPromptImage(obj=values$w2vNetworkPlot, type="vis",
                                                   prompt=prompt,
                                                   key=values$geminiAPI, desc=desc, values=values)
         },
         "d_tm_estim" = {
           req(values$TMestim_result)
           tmPlot <- topicGplot(values$TMestim_result$beta, nPlot = 10, type = "beta")
           values$tmGemini <- geminiPromptImage(obj=tmPlot, type="ggplot2",
                                                prompt=prompt,
                                                key=values$geminiAPI, desc=desc, values=values)
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
                                                       prompt=prompt,
                                                       key=values$geminiAPI, desc=desc, values=values)
         }
  )
  return(values)
}

geminiParameterPrompt <- function(values, activeTab, input){

  txt <- paste0("The analysis was perfomed on ", values$generalTerm, " extracted from the original corpus. ")

  switch(activeTab,
         "overview" ={
           req(values$VbData)
           txt <- ""
         },
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

## gemini prompt for images
geminiPromptImage <- function(obj, type="vis", prompt="Explain the topics in this map", key, desc = NULL, values){
  ## Check Computer configuration to work with Biblio AI
  ### Internet Connection
  if (!is_online()){
    res <- '⚠️ **Note**: TALL AI requires an active internet connection to work.'
    return(res)
  }
  ### Chromium Browser
  if (is.null(values$Chrome_url)) {
    res <- '⚠️ **Note**: TALL AI requires a **Chrome-based browser** (such as Google Chrome or Microsoft Edge) installed on your computer to work correctly.'
    return(res)
  }
  ### Gemini API key
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
                     prompt = prompt,
                     model =  values$gemini_api_model,
                     outputSize = values$gemini_output_size)
  } else {
    res <- 'To access this feature, please provide a valid Gemini AI API key. You can obtain your API key by visiting the official <a href="https://aistudio.google.com/" target="_blank">Google AI Studio website</a>.'
  }

  return(res)
}

geminiWaitingMessage <- function(values, activeTab){

  messageTxt <- "⌛ Thinking..."

  switch(activeTab,
         "overview" = {
           req(values$VbData)
           values$overviewGemini <- messageTxt
         },
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

geminiSave <- geminiSave <- function(values, activeTab){

  switch(activeTab,
         "overview" = {
           req(values$VbData)
           gemini <- values$overviewGemini
         },
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
  if (is.null(gemini)) gemini <- "Click 'Ask TALL AI' for help. "

  return(gemini)
}

merge_df_to_string <- function(df) {
  # Check if the input has at least two columns
  if (ncol(df) < 2) {
    stop("The data frame must have at least two columns.")
  }

  # Ensure the input is a data frame
  df <- as.data.frame(df)

  # Convert each row into a "param: value" format
  row_strings <- apply(df[, 1:2], 1, function(row) {
    paste0(row[1], ": ", row[2])
  })

  # Concatenate all row strings using "; " as separator
  final_string <- paste(row_strings, collapse = "; ")

  return(final_string)
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

# # convert gemini output to HTML
# text_to_html <- function(input_text) {
#   input_text <- c(input_text,"\n")
#   # Escape HTML special characters
#   escape_html <- function(text) {
#     text <- gsub("&", "&amp;", text)
#     text <- gsub("<", "&lt;", text)
#     text <- gsub(">", "&gt;", text)
#     text
#   }
#
#   # Convert markdown-style bold (**text**) to <strong>
#   convert_bold <- function(text) {
#     gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", text)
#   }
#
#   # Process each paragraph
#   paragraphs <- unlist(strsplit(input_text, "\n\n"))
#   html_paragraphs <- lapply(paragraphs, function(p) {
#     lines <- unlist(strsplit(p, "\n"))
#     lines <- sapply(lines, escape_html) # escape special characters
#     lines <- sapply(lines, convert_bold) # convert **bold**
#
#     if (all(grepl("^\\*\\s+", lines))) {
#       # Convert to unordered list
#       lines <- gsub("^\\*\\s+", "", lines)
#       items <- paste0("<li>", lines, "</li>", collapse = "\n")
#       return(paste0("<ul>\n", items, "\n</ul>"))
#     } else {
#       # Regular paragraph
#       return(paste0("<p>", paste(lines, collapse = "<br/>"), "</p>"))
#     }
#   })
#
#   # Combine all HTML parts
#   html_body <- paste(html_paragraphs, collapse = "\n\n")
#   html <- paste0("<html>\n<body>\n", html_body, "\n</body>\n</html>")
#   html <- gsub("\n","",html)
#   return(html)
# }

# # From HTML to text Blocks
# html_to_blocks <- function(raw_html){
#   html_body <- sub(".*<body[^>]*>", "", raw_html)
#   html_body <- sub("</body>.*", "", html_body)
#
#   blocks <- stringr::str_split(
#     html_body,
#     "(?=<p|<ul|<ol|<li|<h[1-6]|<blockquote|<pre|<table|<div)",
#     simplify = FALSE
#   )[[1]]
#
#   blocks <- trimws(blocks)
#   blocks <- blocks[nzchar(blocks)]
# }

## New function to convert gemini output as HTML blocks
gemini_to_html <- function(text) {
  # Remove original leading/trailing whitespace
  text <- trimws(text)

  # Divide text into lines
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))

  # Remove rows
  lines <- lines[lines != ""]

  # Initialize HTML output with CSS styles
  html_lines <- c(
    "<div style='font-family: Arial, sans-serif; line-height: 1.3; margin: 0 auto; padding: 20px;" # max-width: 800px; '>"
  )
  in_list <- FALSE
  list_type <- ""

  for (i in 1:length(lines)) {
    line <- trimws(lines[i])

    # jump empty lines
    if (line == "") {
      next
    }

    # Title management (lines enclosed in **)
    if (stringr::str_detect(line, "^\\*\\*[^*]+\\*\\*$")) {
      # Chiudi eventuali liste aperte
      if (in_list) {
        if (list_type == "ul") {
          html_lines <- c(html_lines, "</ul>")
        } else {
          html_lines <- c(html_lines, "</ol>")
        }
        in_list <- FALSE
      }

      # Convert titles
      title_text <- stringr::str_replace_all(line, "^\\*\\*(.+)\\*\\*$", "\\1")
      html_lines <- c(html_lines, paste0("<h3 style='color: #333; border-bottom: 2px solid #6CC283; padding-bottom: 5px; margin-bottom: 10px; margin-top: 20px;'>", title_text, "</h3>"))
      next
    }

    # Managing bulleted lists (starting with *)
    if (stringr::str_detect(line, "^\\s*\\*\\s+")) {
      # Se non siamo già in una lista puntata, iniziala
      if (!in_list || list_type != "ul") {
        if (in_list && list_type == "ol") {
          html_lines <- c(html_lines, "</ol>")
        }
        html_lines <- c(html_lines, "<ul style='margin-bottom: 10px; margin-top: 5px;'>")
        in_list <- TRUE
        list_type <- "ul"
      }

      # Remove the asterisk and format the content
      item_text <- stringr::str_replace(line, "^\\s*\\*\\s+", "")
      item_text <- format_inline_text(item_text)
      html_lines <- c(html_lines, paste0("<li style='margin-bottom: 3px;'>", item_text, "</li>"))
      next
    }

    # Managing numbered lists (starting with a number followed by a period)
    if (stringr::str_detect(line, "^\\s*\\d+\\.\\s+")) {
      # Se non siamo già in una lista numerata, iniziala
      if (!in_list || list_type != "ol") {
        if (in_list && list_type == "ul") {
          html_lines <- c(html_lines, "</ul>")
        }
        html_lines <- c(html_lines, "<ol style='margin-bottom: 10px; margin-top: 5px;'>")
        in_list <- TRUE
        list_type <- "ol"
      }

      # Remove the number and format the content
      item_text <- stringr::str_replace(line, "^\\s*\\d+\\.\\s+", "")
      item_text <- format_inline_text(item_text)
      html_lines <- c(html_lines, paste0("<li style='margin-bottom: 3px;'>", item_text, "</li>"))
      next
    }

    # If we get here and we're on a list, let's close it.
    if (in_list) {
      if (list_type == "ul") {
        html_lines <- c(html_lines, "</ul>")
      } else {
        html_lines <- c(html_lines, "</ol>")
      }
      in_list <- FALSE
    }

    # Normal paragraph management
    formatted_line <- format_inline_text(line)
    html_lines <- c(html_lines, paste0("<p style='margin-bottom: 8px;'>", formatted_line, "</p>"))
  }

  # Close any lists that remain open
  if (in_list) {
    if (list_type == "ul") {
      html_lines <- c(html_lines, "</ul>")
    } else {
      html_lines <- c(html_lines, "</ol>")
    }
  }

  # Close the container div
  html_lines <- c(html_lines, "</div>")

  # Merge all rows
  html_result <- paste(html_lines, collapse = "\n")

  return(html_result)
}

# Auxiliary function for formatting inline text
format_inline_text <- function(text) {
  # Bold management (**text**)
  text <- stringr::str_replace_all(text, "\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>")

  # Cursive management (*text*) - only if it is not already in bold
  text <- stringr::str_replace_all(text, "(?<!\\*)\\*([^*]+)\\*(?!\\*)", "<em>\\1</em>")

  # Handling text in quotation marks as inline code (“text”)
  text <- stringr::str_replace_all(text, '"([^"]+)"', '<code style="background-color: #f4f4f4; padding: 2px 4px; border-radius: 3px; font-family: \'Courier New\', monospace;">\\1</code>')

  # Handling parentheses with percentages or numerical values
  text <- stringr::str_replace_all(text, "\\(([^)]*%[^)]*)\\)", "<span style='color: #6CC283; font-weight: bold;'>(\\1)</span>")

  return(text)
}

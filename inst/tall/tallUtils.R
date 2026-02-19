### TALL TEAM CARDS ----

# Funzione helper aggiornata con titolo e affiliazione
createAuthorCard <- function(
  name,
  title,
  affiliation,
  url,
  photo,
  scholar = FALSE
) {
  tags$a(
    href = url,
    target = "_blank",
    style = "text-decoration: none;",
    div(
      style = "text-align: center; transition: transform 0.3s; cursor: pointer; padding: 15px; background: #f9f9f9; border-radius: 10px;",
      onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0 8px 20px rgba(0,0,0,0.15)';",
      onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='none';",

      # Foto circolare con bordo
      div(
        style = sprintf(
          "width: 110px; height: 110px; border-radius: 50%%; background-image: url('%s'); background-size: cover; background-position: center; margin: 0 auto 15px; border: 4px solid #3c8dbc; box-shadow: 0 4px 15px rgba(0,0,0,0.2);",
          photo
        )
      ),

      # Nome
      div(
        name,
        style = "font-weight: bold; color: #2c3e50; font-size: 16px; margin-bottom: 8px;"
      ),

      # Titolo
      div(
        title,
        style = "color: #7f8c8d; font-size: 13px; font-weight: 500; margin-bottom: 5px;"
      ),

      # Affiliazione
      div(
        icon("university", style = "margin-right: 5px;"),
        affiliation,
        style = "color: #95a5a6; font-size: 12px; line-height: 1.4; margin-bottom: 10px;"
      ),

      # Link icon
      div(
        if (scholar) {
          tagList(
            icon("graduation-cap", style = "margin-right: 3px;"),
            "Google Scholar"
          )
        } else {
          tagList(
            icon("link", style = "margin-right: 3px;"),
            "Website"
          )
        },
        style = "color: #3c8dbc; font-size: 12px; font-weight: 600;"
      )
    )
  )
}


### UTILS functions ----
# check Internet connection

check_online <- function(
  host = "8.8.8.8",
  timeout = 5,
  # min_success = 1,
  method = "ping" # method = c("ping", "socket", "http")
) {
  #method <- match.arg(method)

  if (method == "ping") {
    # Usa solo il codice di ritorno, non analizza l'output
    ping_cmd <- if (.Platform$OS.type == "windows") {
      sprintf("ping -n 1 -w %d %s", timeout * 1000, host)
    } else {
      sprintf("ping -c 1 -W %d %s", timeout, host)
    }
    exit_code <- suppressWarnings(system(
      ping_cmd,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    ))
    return(exit_code == 0)
  } else if (method == "socket") {
    # Connessione TCP a DNS Google (porta 53)
    tryCatch(
      {
        con <- socketConnection(
          host = host,
          port = 53,
          blocking = TRUE,
          open = "r+",
          timeout = timeout
        )
        close(con)
        return(TRUE)
      },
      error = function(e) {
        return(FALSE)
      }
    )
  } else if (method == "http") {
    # Richiesta HTTP
    tryCatch(
      {
        # check if host start with http or https and add if missing
        if (!grepl("^https?://", host)) {
          host <- paste0("https://", host)
        }

        # con <- url("https://www.google.com", open = "rb")
        con <- url(host, open = "rb")
        on.exit(close(con))
        readLines(con, n = 1, warn = FALSE)
        return(TRUE)
      },
      error = function(e) {
        return(FALSE)
      }
    )
  }
}


# Number format abbreviated
format_abbreviated <- function(x) {
  if (is.na(x)) {
    return("--")
  }
  if (x >= 1e6) {
    return(paste0(format(round(x / 1e6, 2), nsmall = 2), "M"))
  } else if (x >= 1e3) {
    return(paste0(format(round(x / 1e3, 0), nsmall = 0), "K"))
  } else {
    return(as.character(x))
  }
}

# total package download
total_downloads <- function(
  pkg_name = "tall",
  from = "2025-01-01",
  to = Sys.Date()
) {
  # Function to get total downloads of a package from CRAN logs
  # Args:
  #   pkg_name: Name of the package as a string
  # Returns:
  #   Total number of downloads as an integer

  # if (!is_Online()) {
  #   return(NA)
  # }

  #today <- Sys.Date()
  if (!is.character(pkg_name) || length(pkg_name) != 1) {
    stop("pkg_name must be a single string.")
  }

  url <- paste0(
    "https://cranlogs.r-pkg.org/downloads/total/",
    from,
    ":",
    to,
    "/",
    pkg_name
  )

  # if (!is_Online(timeout = 1, url)) {
  #   return(NA)
  # }
  if (!check_online(host = url, timeout = 1, method = "http")) {
    return(NA)
  }

  json_text <- tryCatch(
    {
      readLines(url, warn = FALSE)
    },
    error = function(e) {
      return(NA)
    }
  )

  # Se già nel tryCatch è tornato "NA", esci subito
  if (identical(json_text, "NA")) {
    return(NA)
  }

  # Extract the number manually (not robust)
  txt <- unlist(strsplit(json_text, ","))
  txt <- txt[grepl("downloads", txt)]

  if (length(txt) == 0) {
    return(NA)
  }

  downloads <- gsub("[^0-9]", "", txt)

  return(as.integer(downloads))
}

# Scroll to Top Button (Font Awesome version)

scrollToTopButton <- function() {
  tags$div(
    # CSS per il pulsante
    tags$head(
      tags$style(HTML(
        "
        #scrollToTopBtn {
          display: none;
          position: fixed;
          bottom: 20px;
          right: 20px;
          z-index: 99999;
          border: none;
          outline: none;
          background-color: rgba(68, 68, 68, 0.7);
          color: white;
          cursor: pointer;
          padding: 12px;
          border-radius: 4px;
          font-size: 16px;
          width: 45px;
          height: 45px;
          box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3);
          transition: all 0.3s ease;
          backdrop-filter: blur(4px);
        }

        #scrollToTopBtn:hover {
          background-color: rgba(79, 121, 66, 0.85);
          transform: translateY(-3px);
          box-shadow: 0 4px 10px rgba(0, 0, 0, 0.4);
        }

        #scrollToTopBtn:active {
          transform: translateY(-1px);
          box-shadow: 0 2px 6px rgba(0, 0, 0, 0.3);
        }

        #scrollToTopBtn i {
          margin: 0;
          padding: 0;
          line-height: 1;
        }
      "
      ))
    ),

    # Il pulsante HTML
    tags$button(
      id = "scrollToTopBtn",
      icon("arrow-up", lib = "glyphicon"),
      onclick = "scrollToTop()",
      title = "Back to top"
    ),

    # JavaScript per gestire lo scroll
    tags$script(HTML(
      "
      // Funzione per mostrare/nascondere il pulsante
      window.onscroll = function() {scrollFunction()};

      function scrollFunction() {
        var btn = document.getElementById('scrollToTopBtn');
        if (document.body.scrollTop > 100 || document.documentElement.scrollTop > 100) {
          btn.style.display = 'block';
        } else {
          btn.style.display = 'none';
        }
      }

      // Funzione per scrollare in alto con animazione smooth
      function scrollToTop() {
        window.scrollTo({
          top: 0,
          behavior: 'smooth'
        });
      }
    "
    ))
  )
}

## CPU cores ------
coresCPU <- function() {
  ## set cores for parallel computing
  ncores <- max(1, parallel::detectCores() - 1)

  ## set cores for windows machines
  if (Sys.info()[["sysname"]] == "Windows") {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
  }
  return(ncores)
}

## Check Internet connection ----
is_online <- function() {
  # Attempt to connect to a known online resource (e.g., Google's DNS server)
  check_online()
}

## clean raw text before apply tokenization ----

short2long <- function(df, myC) {
  z <- unlist(lapply(myC, function(x) {
    y <- gsub(r"{\s*\([^\)]+\)}", "", x)
    gsub(y, df$long[df$short == y], x)
  }))
  names(myC) <- z
  return(myC)
}


## Labels sheets Report

dfLabel <- function() {
  short <- c(
    "Empty Report",
    "Overview",
    "WordsFreq",
    "WordCloud",
    "PoSFreq",
    "Keyness",
    "KWICNetwork",
    "Reinert",
    "CorrespondenceAnalysis",
    "CoWord",
    "ThematicMap",
    "Grako",
    "EmbeddingTraining",
    "EmbeddingSimilarity",
    "KChoice",
    "ModelEstim",
    "PolarityDetection",
    "AbstractiveSummarization",
    "ExtractiveSummarization"
  )

  long <- c(
    "Empty Report",
    "Overview",
    "Words Frequency",
    "WordCloud",
    "PoS Tag Frequency",
    "Keyness Analysis",
    "KWIC Network Analysis",
    "Reinert Clustering",
    "Correspondence Analysis",
    "Co-Word Analysis",
    "Thematic Map",
    "Grako",
    "Word Embedding Training",
    "Word Embedding Similarity",
    "TM-K choice",
    "TM-Model Estimation",
    "Polarity Detection",
    "Abstractive Summarization",
    "Extractive Summarization"
  )

  data.frame(short = short, long = long)
}

## Add to Report PopUp

popUp <- function(title = NULL, type = "success", btn_labels = "OK") {
  switch(
    type,
    saved = {
      title <- title
      subtitle <- ""
      btn_colors <- "#1d8fe1"
      showButton <- TRUE
      timer <- 3000
    },
    success = {
      title <- paste(title, "\n added to report", sep = "")
      subtitle <- ""
      btn_colors <- "#1d8fe1"
      showButton <- TRUE
      timer <- 3000
    },
    error = {
      title <- "No results to add to the report "
      subtitle <- "Please Run the analysis and then Add it to the report"
      btn_colors <- "#913333"
      showButton <- TRUE
      timer <- 3000
    },
    waiting = {
      title <- "Please wait... "
      subtitle <- "Adding results to report"
      btn_colors <- "#FFA800"
      showButton <- FALSE
      btn_labels <- NA
      timer <- NA
    }
  )
  if (type == "saved") {
    type <- "success"
  }
  show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = btn_colors,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}

## generic popup ###


popUpGeneric <- function(
  title = NULL,
  type = "success",
  color = c("#1d8fe1", "#913333", "#FFA800"),
  subtitle = "",
  btn_labels = "OK",
  html = FALSE
) {
  showButton <- TRUE
  timer <- NA
  show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = html,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = color,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}


### UTILITY FUNCTIONS ----
# find home folder

homeFolder <- function() {
  switch(
    Sys.info()[["sysname"]],
    Windows = {
      home <- Sys.getenv("R_USER")
    },
    Linux = {
      home <- Sys.getenv("HOME")
    },
    Darwin = {
      home <- Sys.getenv("HOME")
    }
  )
  return(home)
}
## check working folder

wdFolder <- function() {
  home <- paste0(homeFolder(), "/tall")
  wdFile <- paste0(home, "/tallWD.tall")
  wdTall <- NULL

  if (file.exists(wdFile)) {
    wdTall <- readLines(wdFile)
    if (!file.exists(wdTall)) {
      file.remove(wdFile)
      wdTall <- NULL
    }
  }
  return(wdTall)
}

# add working folder path to a file name

destFolder <- function(file, folder) {
  paste0(folder, "/", file)
}


sys.time <- function() {
  format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
}

## Reset reactive values

resetValues <- function() {
  ### Initial values ----
  values <- list()
  values <- reactiveValues()

  if (inherits(try(pagedown::find_chrome(), silent = T), "try-error")) {
    Chrome_url <- NULL
  } else {
    Chrome_url <- pagedown::find_chrome()
  }

  #  Sys.setenv (CHROMOTE_CHROME = Chrome_url)

  ## chrome configuration for shinyapps server

  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c(
          "--disable-gpu",
          "--no-sandbox",
          "--disable-dev-shm-usage", # required bc the target easily crashes
          c("--force-color-profile", "srgb")
        )
      ))
    )
  }
  ## end configuration

  ## Check if Chrome browser is installed on the computer
  if (is.null(Chrome_url)) {
    showModal(modalDialog(
      title = strong("Warning message!"),
      HTML(
        "Chrome or a Chromium-based browser is not installed on your computer.<br>
If you do not have either of these browsers installed, TALL will be unable to export graphs.<br>
To ensure the functionality of Biblioshiny,
           please download Chrome by <a href='https://www.google.com/chrome/' target='_blank' > <b>clicking here</b></a>."
      ),
      footer = modalButton("Dismiss"),
      easyClose = TRUE
    ))
  } else {
    Sys.setenv(CHROMOTE_CHROME = Chrome_url)
  }
  values$Chrome_url <- Chrome_url
  values$posSpecialSummary <- NULL
  values$biblioshiny <- NULL
  values$resetNeed <- FALSE
  values$normButton <- FALSE
  values$path <- NULL
  values$custom_lists <- NULL
  values$txt <- data.frame()
  values$corpusElements <- data.frame()
  values$txtOriginal <- data.frame()
  values$list_file <- data.frame(sheet = NULL, file = NULL, n = NULL)
  values$POSTagSelected <- ""
  values$wb <- openxlsx::createWorkbook()
  values$dfLabel <- dfLabel()
  values$posMwSel <- c("ADJ", "NOUN", "PROPN") # POS selected by default for multiword creation
  values$myChoices <- "Empty Report"
  values$generalTerm <- "lemma"

  accuracy <- model_accuracy()
  values$accuracy <- accuracy
  languages_df <- langrepo()
  values$language <- "english"
  values$languages <- languages_df
  label_lang <- unique(languages_df$language_name)
  names(label_lang) <- gsub("_", " ", label_lang)
  values$label_lang <- label_lang
  values$treebank <- languages_df$treebank[1]
  values$chapter <- languages_df$chapter
  values$flag <- "GB.svg"
  values$TMplotIndex <- 1
  values$TMdocIndex <- 1
  values$tmTopSentences <- FALSE
  values$selectedGroups <- NULL
  values$selectedFilter <- ""
  values$wdTall <- wdFolder()
  if (is.null(wdFolder())) {
    values$menu <- -2
  } else {
    values$menu <- -1
  }

  ## random seed
  values$random_seed <- 1234

  ## gemini api and model
  home <- homeFolder()
  path_gemini_key <- paste0(
    file.path(home, "tall"),
    "/.tall_gemini_key.txt",
    collapse = ""
  )
  # check if sub directory exists
  values$geminiAPI <- load_api_key(path_gemini_key)
  values$corpus_description <- NULL
  values$gemini_additional <- NULL

  path_gemini_model <- path_gemini_key <- paste0(
    file.path(home, "tall"),
    "/.tall_gemini_model.txt",
    collapse = ""
  )
  gemini_api_model <- loadGeminiModel(path_gemini_model)
  values$gemini_api_model <- gemini_api_model[1]
  values$gemini_output_size <- gemini_api_model[2]

  values$abstractivePrompt <- NULL

  # Initialize Feature Roles values
  values$timeVariable <- NULL
  values$labelVariable <- NULL
  values$keynessVariable <- NULL
  values$keynessGroup1 <- NULL
  values$keynessGroup2 <- NULL
  values$keynessGroupsApplied <- FALSE

  return(values)
}


firstUp <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

## Color palette for plots

colorlist <- function() {
  c(
    "#4DAF4A",
    "#E41A1C",
    "#377EB8",
    "#984EA3",
    "#FF7F00",
    "#A65628",
    "#F781BF",
    "#999999",
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854",
    "#FFD92F",
    "#B3B3B3",
    "#A6CEE3",
    "#1F78B4",
    "#B2DF8A",
    "#33A02C",
    "#FB9A99",
    "#E31A1C",
    "#FDBF6F",
    "#FF7F00",
    "#CAB2D6",
    "#6A3D9A",
    "#B15928",
    "#8DD3C7",
    "#BEBADA",
    "#FB8072",
    "#80B1D3",
    "#FDB462",
    "#B3DE69",
    "#D9D9D9",
    "#BC80BD",
    "#CCEBC5"
  )
}


## POS selection functions ----
# Set TRUE PoS selected ##

saveTall <- function(
  dfTag,
  custom_lists,
  language,
  treebank,
  menu,
  where,
  file,
  generalTerm,
  corpus_description
) {
  D <- date()
  D <- strsplit(gsub("\\s+", " ", D), " ")
  D <- paste(unlist(D)[c(1, 2, 3, 5)], collapse = " ")
  save(
    dfTag,
    custom_lists,
    language,
    treebank,
    menu,
    D,
    where,
    file = file,
    generalTerm,
    corpus_description
  )
}

### Export Tall analysis in .tall file ----

# SIDEBARMENU DYNAMIC ----

deleteCache <- function() {
  home <- homeFolder()

  # setting up the main directory
  path_tall <- file.path(home, "tall")
  result <- unlink(path_tall, recursive = TRUE)
  btn_labels <- "OK"

  if (result == 0) {
    subtitle <- paste0(
      "The folder '",
      path_tall,
      "' \nand files contained in it have been correctly deleted"
    )
    title <- ""
    btn_colors <- "#1d8fe1"
    showButton <- TRUE
    timer <- 3000
    type <- "success"
  } else {
    subtitle <- paste0(
      "The folder '",
      path_tall,
      "' \ndoes not exist or you don't have permissions to delete it"
    )
    title <- ""
    btn_colors <- "#913333"
    showButton <- TRUE
    timer <- 3000
    type <- "error"
  }
  show_alert(
    title = title,
    text = subtitle,
    type = type,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    showConfirmButton = showButton,
    showCancelButton = FALSE,
    btn_labels = btn_labels,
    btn_colors = btn_colors,
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}

## RESET MODAL DIALOG INPUTS

resetModalButtons <- function(session) {
  session$sendCustomMessage("button_id", "null")
  session$sendCustomMessage("click", "null")
  session$sendCustomMessage("click-dend", "null")
  runjs("Shiny.setInputValue('plotly_click-A', null);")
  return(session)
}

#### language models repo ----



### DOCUMENT CLASSIFICATION MODULE ####
# Supervised classification with Random Forest
# Features: DTM or Word2Vec representations + Ranger RF

#### UI Function ####
docClassificationUI <- function() {
  tabItem(
    tabName = "doc_classification",
    fluidPage(
      # Page Header
      fluidRow(
        column(
          12,
          div(
            h2(
              icon("rectangle-list"),
              strong("Supervised Document Classification"),
              style = "color: #4F7942; text-align: center; margin-bottom: 20px;"
            ),
            p(
              "Train a Random Forest classifier to predict document categories using the Label Variable defined in Feature Roles.",
              style = "text-align: center; font-size: 16px; color: #666; margin-bottom: 30px;"
            )
          )
        )
      ),

      # Main content
      fluidRow(
        # Configuration Panel
        column(
          8,
          box(
            title = strong(
              icon("gear"),
              " Configuration",
              style = "font-size: 18px;"
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            id = "rf_config_box",

            # Target Variable Status
            div(
              style = "background-color: #e8f5e9; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
              icon("tags", style = "color: #4CAF50;"),
              strong(" Target Variable: "),
              uiOutput("rf_target_var_display", inline = TRUE),
              br(),
              uiOutput("rf_target_var_info")
            ),

            # Feature Representation Selection
            div(
              style = "margin-bottom: 20px;",
              h4(
                icon("table-cells-large"),
                strong(" Feature Representation"),
                style = "color: #4F7942;"
              ),
              p(
                "Choose how documents will be represented as features for the classifier:",
                style = "color: #666; font-size: 14px;"
              ),
              radioGroupButtons(
                inputId = "rf_feature_type",
                label = NULL,
                choices = c(
                  "Document-Term Matrix (TF-IDF)" = "dtm",
                  "Word2Vec Document Embeddings" = "w2v"
                ),
                selected = "dtm",
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("check"),
                  no = icon("xmark")
                )
              )
            ),

            # DTM Options
            conditionalPanel(
              condition = "input.rf_feature_type == 'dtm'",
              div(
                style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                h5(strong("Document-Term Matrix Options")),
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "rf_dtm_weighting",
                      "Weighting Scheme:",
                      choices = c(
                        "TF-IDF" = "tfidf",
                        "Term Frequency (TF)" = "tf",
                        "Binary" = "binary"
                      ),
                      selected = "tfidf"
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "rf_dtm_min_df",
                      "Min. Document Frequency:",
                      value = 2,
                      min = 1,
                      step = 1
                    ),
                    tags$small(
                      "Terms must appear in at least this many docs",
                      style = "color: #666;"
                    )
                  ),
                  column(
                    4,
                    sliderInput(
                      "rf_dtm_max_df",
                      "Max. Document Proportion:",
                      value = 0.95,
                      min = 0.5,
                      max = 1,
                      step = 0.05
                    ),
                    tags$small(
                      "Terms appearing in more docs are removed",
                      style = "color: #666;"
                    )
                  )
                ),
                fluidRow(
                  column(
                    6,
                    numericInput(
                      "rf_dtm_max_features",
                      "Max Features (optional):",
                      value = NA,
                      min = 10,
                      step = 10
                    ),
                    tags$small(
                      "Leave empty for all features",
                      style = "color: #666;"
                    )
                  )
                )
              )
            ),

            # Word2Vec Options
            conditionalPanel(
              condition = "input.rf_feature_type == 'w2v'",
              div(
                style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                h5(strong("Word2Vec Document Embeddings Options")),
                fluidRow(
                  column(
                    6,
                    radioButtons(
                      "rf_w2v_aggregation",
                      "Aggregation Method:",
                      choices = c(
                        "Simple Mean" = "mean",
                        "TF-IDF Weighted Mean" = "tfidf_weighted"
                      ),
                      selected = "mean"
                    ),
                    tags$small(
                      "How to combine word vectors into document vector",
                      style = "color: #666;"
                    )
                  ),
                  column(
                    6,
                    div(
                      style = "margin-top: 10px; padding: 10px; background-color: #fff; border: 1px solid #ddd; border-radius: 4px;",
                      h5(strong("Model Status")),
                      uiOutput("rf_w2v_model_status")
                    )
                  )
                )
              )
            ),

            hr(),

            # Random Forest Parameters
            div(
              h4(
                icon("tree"),
                strong(" Random Forest Parameters"),
                style = "color: #4F7942;"
              ),
              fluidRow(
                column(
                  4,
                  numericInput(
                    "rf_num_trees",
                    "Number of Trees:",
                    value = 500,
                    min = 100,
                    max = 2000,
                    step = 100
                  )
                ),
                column(
                  4,
                  sliderInput(
                    "rf_test_size",
                    "Test Set Proportion:",
                    value = 0.3,
                    min = 0.1,
                    max = 0.5,
                    step = 0.05
                  )
                ),
                column(
                  4,
                  div(
                    style = "margin-top: 25px;",
                    checkboxInput(
                      "rf_class_weights",
                      "Balance Class Weights",
                      value = TRUE
                    ),
                    tags$small(
                      "Recommended for imbalanced datasets",
                      style = "color: #666;"
                    )
                  )
                )
              )
            ),

            hr(),

            # Action Button
            div(
              style = "text-align: center; margin-top: 20px;",
              actionBttn(
                inputId = "rf_run",
                label = strong("Train Random Forest Model"),
                style = "pill",
                color = "success",
                size = "lg",
                icon = icon("play")
              )
            )
          )
        ),

        # Information Sidebar
        column(
          4,
          # Information Box
          box(
            title = strong(
              icon("circle-info"),
              " Information"
            ),
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            id = "rf_info_box",

            h5(strong("Prerequisites:")),
            tags$ul(
              tags$li(
                "Define a ",
                strong("Label Variable"),
                " in the Feature Roles menu"
              ),
              tags$li("At least 5 documents per class"),
              tags$li(
                "For Word2Vec: train embeddings in Words > Embeddings > Training"
              )
            ),

            hr(),

            h5(strong("Feature Representation:")),
            tags$ul(
              tags$li(
                strong("Document-Term Matrix:"),
                br(),
                "Traditional bag-of-words approach with TF-IDF weighting. Interpretable feature importance.",
                style = "margin-bottom: 10px;"
              ),
              tags$li(
                strong("Word2Vec Embeddings:"),
                br(),
                "Semantic document representations. Better generalization, lower dimensionality.",
                style = "margin-bottom: 10px;"
              )
            ),

            hr(),

            h5(strong("Random Forest:")),
            p(
              "Ensemble learning method using the ",
              tags$code("ranger"),
              " package for fast training.",
              style = "font-size: 13px;"
            )
          ),

          # Help Box
          box(
            title = strong(
              icon("circle-question"),
              " Need Help?"
            ),
            width = 12,
            status = "warning",
            collapsible = TRUE,
            collapsed = FALSE,
            id = "rf_help_box",
            p(
              "The Random Forest will predict the ",
              strong("Label Variable"),
              " defined in Feature Roles.",
              style = "font-size: 13px;"
            ),
            p(
              "Use ",
              strong("DTM"),
              " for interpretability and ",
              strong("Word2Vec"),
              " for better semantic understanding.",
              style = "font-size: 13px;"
            )
          )
        )
      ),

      # Results Panel (conditionally displayed)
      conditionalPanel(
        condition = "output.rf_trained",
        fluidRow(
          column(
            12,
            h3(
              icon("chart-bar"),
              strong(" Classification Results"),
              style = "color: #4F7942; margin-top: 30px; margin-bottom: 20px;"
            )
          )
        ),

        # Performance Metrics Row
        fluidRow(
          column(
            6,
            box(
              title = strong("Confusion Matrix - Test Set"),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              shinycssloaders::withSpinner(
                plotOutput("rf_confusion_plot", height = "400px"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            )
          ),
          column(
            6,
            box(
              title = strong("Performance Metrics"),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              div(
                style = "margin-bottom: 15px;",
                h4(
                  "Overall Accuracy: ",
                  textOutput("rf_accuracy_display", inline = TRUE),
                  style = "color: #4CAF50;"
                )
              ),
              shinycssloaders::withSpinner(
                DT::DTOutput("rf_metrics_table"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            )
          )
        ),

        # Feature Importance
        fluidRow(
          column(
            12,
            box(
              title = strong("Feature/Dimension Importance"),
              width = 12,
              status = "info",
              solidHeader = TRUE,
              shinycssloaders::withSpinner(
                plotlyOutput("rf_importance_plot", height = "600px"),
                color = getOption("spinner.color", default = "#4F7942")
              )
            )
          )
        ),

        # Model Details
        fluidRow(
          column(
            6,
            box(
              title = strong("Training Details"),
              width = 12,
              status = "primary",
              verbatimTextOutput("rf_model_summary")
            )
          ),
          column(
            6,
            box(
              title = strong("Export Results"),
              width = 12,
              status = "primary",
              p(
                "Download classification results and model performance metrics."
              ),
              downloadButton(
                "rf_download_results",
                "Download Results (.xlsx)",
                class = "btn-info btn-block"
              ),
              hr(),
              actionBttn(
                inputId = "rf_apply_unlabeled",
                label = "Apply to Unlabeled Documents",
                style = "bordered",
                color = "primary",
                size = "md",
                block = TRUE,
                icon = icon("forward")
              )
            )
          )
        )
      )
    )
  )
}


#### SERVER FUNCTION ####
docClassificationServer <- function(input, output, session, values) {
  # Reactive values for classification results
  rf_results <- reactiveValues(
    model = NULL,
    train_data = NULL,
    test_data = NULL,
    train_pred = NULL,
    test_pred = NULL,
    feature_matrix = NULL,
    feature_type = NULL,
    term_mapping = NULL, # Mapping between original terms and sanitized names
    eval_train = NULL,
    eval_test = NULL,
    trained = FALSE
  )

  #### UI OUTPUTS ####

  # Display target variable
  output$rf_target_var_display <- renderUI({
    if (!is.null(values$labelVariable) && values$labelVariable != "") {
      tags$span(
        strong(values$labelVariable),
        style = "color: #4CAF50; font-size: 16px;"
      )
    } else {
      tags$span(
        "Not defined",
        style = "color: #d32f2f; font-size: 16px;"
      )
    }
  })

  # Target variable info
  output$rf_target_var_info <- renderUI({
    check <- checkClassificationPrereqs(values)

    if (!check$valid) {
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #ffebee; border-left: 4px solid #f44336; border-radius: 4px;",
        icon("triangle-exclamation", style = "color: #d32f2f;"),
        strong(" Issues found:", style = "color: #d32f2f;"),
        tags$ul(
          lapply(check$errors, function(err) {
            tags$li(err, style = "color: #d32f2f;")
          })
        )
      )
    } else {
      # Show class distribution
      class_dist <- table(
        values$dfTag %>%
          filter(docSelected) %>%
          distinct(doc_id, .keep_all = TRUE) %>%
          pull(!!sym(values$labelVariable))
      )

      div(
        style = "margin-top: 10px;",
        tags$small(
          icon("check", style = "color: #4CAF50;"),
          strong(" Class distribution: "),
          paste(names(class_dist), "=", class_dist, collapse = " | "),
          style = "color: #666;"
        )
      )
    }
  })

  # Word2Vec model status
  output$rf_w2v_model_status <- renderUI({
    if (!is.null(values$w2v_model)) {
      div(
        icon("check", style = "color: #4CAF50;"),
        strong(" Model Available", style = "color: #4CAF50;"),
        br(),
        tags$small(
          sprintf("Dimensions: %d", ncol(as.matrix(values$w2v_model))),
          style = "color: #666;"
        )
      )
    } else {
      div(
        icon("triangle-exclamation", style = "color: #ff9800;"),
        strong(" Model Not Trained", style = "color: #ff9800;"),
        br(),
        tags$small(
          "Will be trained automatically with default parameters",
          style = "color: #666;"
        )
      )
    }
  })

  # Reactive flag for trained model
  output$rf_trained <- reactive({
    rf_results$trained
  })
  outputOptions(output, "rf_trained", suspendWhenHidden = FALSE)

  #### CLASSIFICATION WORKFLOW ####

  # Main classification event
  observeEvent(input$rf_run, {
    # Check prerequisites
    check <- checkClassificationPrereqs(values)
    if (!check$valid) {
      sendSweetAlert(
        session = session,
        title = "Cannot Train Model",
        text = HTML(paste(check$errors, collapse = "<br>")),
        html = TRUE,
        type = "error"
      )
      return(NULL)
    }

    # Show progress
    showNotification(
      "Training Random Forest model... This may take a moment.",
      type = "message",
      duration = NULL,
      id = "rf_training_notification"
    )

    tryCatch(
      {
        # Step 1: Create feature matrix
        if (input$rf_feature_type == "dtm") {
          # DTM approach
          dtm_result <- createDTM_forRF(
            dfTag = values$dfTag,
            term = values$generalTerm,
            weighting = input$rf_dtm_weighting,
            max_features = if (is.na(input$rf_dtm_max_features)) {
              NULL
            } else {
              input$rf_dtm_max_features
            },
            min_df = input$rf_dtm_min_df,
            max_df_prop = input$rf_dtm_max_df
          )

          feature_matrix <- dtm_result$matrix
          rf_results$term_mapping <- dtm_result$term_mapping
          rf_results$feature_type <- "DTM"

          # Inform user about DTM dimensions
          showNotification(
            sprintf(
              "DTM created: %d documents × %d terms",
              nrow(feature_matrix),
              ncol(feature_matrix)
            ),
            type = "message",
            duration = 3
          )
        } else {
          # Word2Vec approach
          # Check if model exists, if not train it
          if (is.null(values$w2v_model)) {
            showNotification(
              "Word2Vec model not found. Training with default parameters...",
              type = "warning",
              duration = 5
            )

            values$w2v_model <- trainWord2Vec(
              dfTag = values$dfTag,
              term = values$generalTerm,
              dim = 100,
              iter = 20
            )
          }

          # Aggregate word vectors to document vectors
          feature_matrix <- aggregateW2V_forRF(
            dfTag = values$dfTag,
            w2v_model = values$w2v_model,
            method = input$rf_w2v_aggregation,
            term = values$generalTerm
          )

          rf_results$term_mapping <- NULL # No term mapping for embeddings
          rf_results$feature_type <- "Word2Vec"
        }

        # Step 2: Get target variable at document level
        # CRITICAL: dfTag is tokenized, so we need to get document-level metadata correctly
        # We use group_by + summarize with first() to get the first non-NA value
        target_data <- values$dfTag %>%
          filter(docSelected) %>%
          group_by(doc_id) %>%
          summarise(
            target_var = first(na.omit(!!sym(values$labelVariable))),
            .groups = 'drop'
          )

        # Remove any documents where target is NA
        target_data <- target_data %>%
          filter(!is.na(target_var))

        # Merge features with target
        if (input$rf_feature_type == "dtm") {
          # For DTM, rownames are doc_ids
          feature_df <- as.data.frame(feature_matrix)
          feature_df$doc_id <- rownames(feature_matrix)

          # Ensure both doc_id columns are character type for reliable matching
          feature_df$doc_id <- as.character(feature_df$doc_id)
          target_data$doc_id <- as.character(target_data$doc_id)

          # Merge with target
          feature_df <- feature_df %>%
            inner_join(target_data, by = "doc_id")

          # Check if we have any rows after join
          if (nrow(feature_df) == 0) {
            stop(
              "No documents remain after merging features with target variable."
            )
          }

          # Check if target_var column exists
          if (!"target_var" %in% names(feature_df)) {
            stop("target_var column not found after merge!")
          }

          # Remove doc_id column
          feature_df <- feature_df[,
            names(feature_df) != "doc_id",
            drop = FALSE
          ]

          # Rename target_var to unique name that won't conflict with any term
          col_idx <- which(names(feature_df) == "target_var")
          if (length(col_idx) != 1) {
            stop(
              "Problem finding target_var column. Found ",
              length(col_idx),
              " matches."
            )
          }
          names(feature_df)[col_idx] <- ".target_class"
        } else {
          # For Word2Vec, feature_matrix already has doc_id column
          feature_df <- as.data.frame(feature_matrix)

          # Ensure doc_id is character
          feature_df$doc_id <- as.character(feature_df$doc_id)
          target_data$doc_id <- as.character(target_data$doc_id)

          feature_df <- feature_df %>%
            inner_join(target_data, by = "doc_id")

          # Check if we have any rows after join
          if (nrow(feature_df) == 0) {
            stop(
              "No documents remain after merging features with target variable."
            )
          }

          # Remove doc_id column - use base R
          feature_df <- feature_df[,
            names(feature_df) != "doc_id",
            drop = FALSE
          ]

          # Rename target_var to unique name
          col_idx <- which(names(feature_df) == "target_var")
          names(feature_df)[col_idx] <- ".target_class"
        }

        # Ensure target is factor AFTER merging
        # Use the unique column name
        feature_df$.target_class <- as.factor(feature_df$.target_class)

        # Calculate class weights if requested
        class_weights <- NULL
        if (input$rf_class_weights) {
          class_counts <- table(feature_df$.target_class)
          class_weights <- 1 / as.numeric(class_counts)
          names(class_weights) <- names(class_counts)
          class_weights <- class_weights / sum(class_weights)
        }

        # Step 3: Train Random Forest
        # Use random seed from settings
        rf_seed <- if (!is.null(values$random_seed)) values$random_seed else 123

        rf_model <- trainRangerRF(
          feature_matrix = feature_df,
          test_size = input$rf_test_size,
          num.trees = input$rf_num_trees,
          class.weights = class_weights,
          seed = rf_seed
        )

        # Step 4: Evaluate
        eval_train <- evaluateClassifier(
          true_labels = rf_model$train_data$.target_class,
          pred_probs = rf_model$train_pred$predictions
        )

        eval_test <- evaluateClassifier(
          true_labels = rf_model$test_data$.target_class,
          pred_probs = rf_model$test_pred$predictions
        )

        # Store results
        rf_results$model <- rf_model$model
        rf_results$train_data <- rf_model$train_data
        rf_results$test_data <- rf_model$test_data
        rf_results$train_pred <- rf_model$train_pred
        rf_results$test_pred <- rf_model$test_pred
        rf_results$feature_matrix <- feature_df
        rf_results$eval_train <- eval_train
        rf_results$eval_test <- eval_test
        rf_results$trained <- TRUE

        # Remove progress notification
        removeNotification(id = "rf_training_notification")

        # Collapse all configuration boxes to show results
        # Small delay to ensure DOM is ready
        shinyjs::delay(500, {
          shinyjs::runjs(
            "
          $('#rf_config_box').closest('.box').find('[data-widget=\"collapse\"]').click();
          $('#rf_info_box').closest('.box').find('[data-widget=\"collapse\"]').click();
          $('#rf_help_box').closest('.box').find('[data-widget=\"collapse\"]').click();
        "
          )
        })

        # Success message with proper formatting
        sendSweetAlert(
          session = session,
          title = "Model Trained Successfully!",
          text = paste0(
            "Test Set Accuracy: ",
            sprintf("%.2f%%", eval_test$accuracy * 100),
            "\n",
            "OOB Error: ",
            sprintf("%.2f%%", rf_model$model$prediction.error * 100)
          ),
          type = "success"
        )
      },
      error = function(e) {
        removeNotification(id = "rf_training_notification")
        sendSweetAlert(
          session = session,
          title = "Error Training Model",
          text = as.character(e$message),
          type = "error"
        )
      }
    )
  })

  #### RESULTS OUTPUTS ####

  # Accuracy display
  output$rf_accuracy_display <- renderText({
    req(rf_results$trained)
    sprintf("%.2f%%", rf_results$eval_test$accuracy * 100)
  })

  # Confusion matrix plot
  output$rf_confusion_plot <- renderPlot({
    req(rf_results$trained)
    plotConfusionMatrix(rf_results$eval_test$confusion_matrix)
  })

  # Metrics table
  output$rf_metrics_table <- DT::renderDT({
    req(rf_results$trained)

    metrics_df <- rf_results$eval_test$metrics_per_class %>%
      mutate(
        precision = round(precision, 3),
        recall = round(recall, 3),
        f1 = round(f1, 3)
      ) %>%
      rename(
        Class = class,
        Precision = precision,
        Recall = recall,
        `F1-Score` = f1
      )

    DT::datatable(
      metrics_df,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })

  # Feature importance plot
  output$rf_importance_plot <- renderPlotly({
    req(rf_results$trained)
    plotFeatureImportance(
      rf_results$model,
      feature_type = rf_results$feature_type,
      top_n = 30,
      term_mapping = rf_results$term_mapping
    )
  })

  # Model summary
  output$rf_model_summary <- renderPrint({
    req(rf_results$trained)

    cat("Random Forest Model Summary\n")
    cat("============================\n\n")
    cat(sprintf("Feature Type: %s\n", rf_results$feature_type))
    cat(sprintf("Number of Trees: %d\n", rf_results$model$num.trees))
    cat(sprintf("Number of Features: %d\n", ncol(rf_results$train_data) - 1))
    cat(sprintf("Training Set Size: %d\n", nrow(rf_results$train_data)))
    cat(sprintf("Test Set Size: %d\n", nrow(rf_results$test_data)))
    cat(sprintf(
      "\nOOB Prediction Error: %.2f%%\n",
      rf_results$model$prediction.error * 100
    ))
    cat(sprintf(
      "Train Set Accuracy: %.2f%%\n",
      rf_results$eval_train$accuracy * 100
    ))
    cat(sprintf(
      "Test Set Accuracy: %.2f%%\n",
      rf_results$eval_test$accuracy * 100
    ))
  })

  # Download results
  output$rf_download_results <- downloadHandler(
    filename = function() {
      paste0("RF_Classification_Results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      exportClassificationResults(
        rf_results = rf_results,
        filename = file,
        label_var = values$labelVariable
      )
    }
  )

  # Apply to unlabeled documents
  observeEvent(input$rf_apply_unlabeled, {
    req(rf_results$trained)

    # TODO: Implement prediction on unlabeled documents
    sendSweetAlert(
      session = session,
      title = "Feature Coming Soon",
      text = "Prediction on unlabeled documents will be implemented in the next version.",
      type = "info"
    )
  })
}


#### HELPER FUNCTIONS ####

#' Check Classification Prerequisites
#'
#' Validates that all requirements are met before training
#'
#' @param values Shiny reactive values object
#' @return List with 'valid' (logical) and 'errors' (character vector)
checkClassificationPrereqs <- function(values) {
  errors <- c()

  # Check if dfTag exists
  if (is.null(values$dfTag)) {
    errors <- c(
      errors,
      "No data loaded. Please load and preprocess your corpus first."
    )
    return(list(valid = FALSE, errors = errors))
  }

  # Check label variable
  if (is.null(values$labelVariable) || values$labelVariable == "") {
    errors <- c(
      errors,
      "Label variable not defined. Please set it in the Feature Roles menu."
    )
  }

  # Check if label variable exists in dfTag
  if (
    !is.null(values$labelVariable) &&
      values$labelVariable != "" &&
      !values$labelVariable %in% names(values$dfTag)
  ) {
    errors <- c(
      errors,
      paste("Label variable '", values$labelVariable, "' not found in data.")
    )
  }

  # Check for selected documents
  if (
    !"docSelected" %in% names(values$dfTag) ||
      sum(values$dfTag$docSelected) == 0
  ) {
    errors <- c(
      errors,
      "No documents selected. Please select documents in the Documents menu."
    )
  }

  # Check if enough documents per class (only if label variable is valid)
  if (length(errors) == 0) {
    doc_labels <- values$dfTag %>%
      filter(docSelected) %>%
      distinct(doc_id, .keep_all = TRUE) %>%
      pull(!!sym(values$labelVariable))

    class_counts <- table(doc_labels)

    if (any(class_counts < 5)) {
      min_class <- names(class_counts)[which.min(class_counts)]
      errors <- c(
        errors,
        sprintf(
          "Class '%s' has only %d documents. Each class needs at least 5 documents for reliable training.",
          min_class,
          min(class_counts)
        )
      )
    }

    # Check for at least 2 classes
    if (length(class_counts) < 2) {
      errors <- c(errors, "At least 2 classes are required for classification.")
    }
  }

  return(list(valid = length(errors) == 0, errors = errors))
}


#' Create Document-Term Matrix for Random Forest
#'
#' Creates a DTM with various weighting schemes using udpipe
#'
#' @param dfTag Data frame with tokenized and tagged text
#' @param term Character, either "lemma" or "token"
#' @param weighting Character, one of "tf", "tfidf", "binary"
#' @param max_features Integer, maximum number of features to keep (NULL = all)
#' @param min_df Integer, minimum document frequency
#' @param max_df_prop Numeric, maximum proportion of documents a term can appear in
#' @return List with matrix and term_mapping
createDTM_forRF <- function(
  dfTag,
  term = "lemma",
  weighting = "tfidf",
  max_features = NULL,
  min_df = 2,
  max_df_prop = 0.95
) {
  # Filter selected documents and POS
  df_filtered <- dfTag %>%
    filter(docSelected, POSSelected)

  # Get unique doc_ids for later reference
  original_doc_ids <- unique(df_filtered$doc_id)

  # Select term
  if (term == "lemma") {
    df_filtered <- df_filtered %>%
      mutate(term_text = lemma)
  } else {
    df_filtered <- df_filtered %>%
      mutate(term_text = token)
  }

  # Use udpipe's document_term_frequencies
  dtf <- document_term_frequencies(
    x = df_filtered,
    document = "doc_id",
    term = "term_text"
  )

  # Create DTM using udpipe's document_term_matrix
  dtm <- document_term_matrix(dtf)

  # CRITICAL: Extract and preserve document IDs
  doc_ids <- rownames(dtm)

  # Convert to regular matrix
  dtm_matrix <- as.matrix(dtm)

  # Ensure rownames are preserved after conversion
  rownames(dtm_matrix) <- doc_ids

  # Store original number of documents
  n_docs_initial <- nrow(dtm_matrix)

  # Get document frequency (number of documents containing each term)
  doc_freq <- colSums(dtm_matrix > 0)
  n_docs <- nrow(dtm_matrix)

  # Apply document frequency filtering
  # Remove too rare terms
  keep_terms <- doc_freq >= min_df

  # Remove too frequent terms
  keep_terms <- keep_terms & (doc_freq / n_docs <= max_df_prop)

  # Filter matrix
  dtm_matrix <- dtm_matrix[, keep_terms, drop = FALSE]

  # CRITICAL: Remove documents that have no terms after filtering
  # This happens when all terms in a document were filtered out
  doc_term_counts <- rowSums(dtm_matrix)
  keep_docs <- doc_term_counts > 0

  n_docs_removed <- sum(!keep_docs)

  if (sum(keep_docs) == 0) {
    stop(
      "All documents were filtered out. Please reduce min_df or increase max_df_prop."
    )
  }

  dtm_matrix <- dtm_matrix[keep_docs, , drop = FALSE]

  # Update doc_freq after filtering documents
  doc_freq <- colSums(dtm_matrix > 0)
  n_docs <- nrow(dtm_matrix)

  # Apply weighting
  if (weighting == "tfidf") {
    # Calculate IDF
    idf <- log(n_docs / doc_freq)

    # Apply TF-IDF
    dtm_matrix <- sweep(dtm_matrix, 2, idf, "*")
  } else if (weighting == "binary") {
    dtm_matrix <- (dtm_matrix > 0) * 1
  }
  # else keep raw TF

  # Optional: keep only top features by variance
  if (!is.null(max_features) && ncol(dtm_matrix) > max_features) {
    term_variance <- apply(dtm_matrix, 2, var)
    top_terms <- order(term_variance, decreasing = TRUE)[1:max_features]
    dtm_matrix <- dtm_matrix[, top_terms, drop = FALSE]
  }

  # CRITICAL: Create mapping between original terms and sanitized column names
  # This preserves original terms for visualization
  original_terms <- colnames(dtm_matrix)
  sanitized_names <- make.names(original_terms, unique = TRUE)

  # Create term mapping data frame
  term_mapping <- data.frame(
    original_term = original_terms,
    sanitized_name = sanitized_names,
    stringsAsFactors = FALSE
  )

  # Apply sanitized names to matrix
  colnames(dtm_matrix) <- sanitized_names

  # Return both matrix and mapping
  return(list(
    matrix = dtm_matrix,
    term_mapping = term_mapping
  ))
}


#' Aggregate Word2Vec Embeddings to Document Level
#'
#' Creates document embeddings by aggregating word vectors
#'
#' @param dfTag Data frame with tokenized and tagged text
#' @param w2v_model word2vec model object
#' @param method Character, "mean" or "tfidf_weighted"
#' @param term Character, either "lemma" or "token"
#' @return Data frame with doc_id and embedding dimensions
aggregateW2V_forRF <- function(
  dfTag,
  w2v_model,
  method = "mean",
  term = "lemma"
) {
  # Get embedding matrix
  w2v_matrix <- as.matrix(w2v_model)

  # Filter and prepare data
  df_filtered <- dfTag %>%
    filter(docSelected, POSSelected) %>%
    mutate(term_text = tolower(!!sym(term)))

  # Calculate TF-IDF weights if needed
  if (method == "tfidf_weighted") {
    n_docs <- length(unique(df_filtered$doc_id))

    tfidf_weights <- df_filtered %>%
      group_by(doc_id, term_text) %>%
      summarise(tf = n(), .groups = 'drop') %>%
      group_by(term_text) %>%
      mutate(
        df = n(),
        idf = log(n_docs / df)
      ) %>%
      ungroup() %>%
      mutate(tfidf = tf * idf)
  }

  # Aggregate by document
  doc_embeddings <- df_filtered %>%
    group_by(doc_id) %>%
    group_modify(
      ~ {
        doc_terms <- .x$term_text

        # Filter terms present in w2v model
        valid_terms <- intersect(doc_terms, rownames(w2v_matrix))

        if (length(valid_terms) == 0) {
          # Return zero vector if no valid terms
          zero_vec <- rep(0, ncol(w2v_matrix))
          return(tibble(
            !!!setNames(as.list(zero_vec), paste0("dim_", 1:ncol(w2v_matrix)))
          ))
        }

        if (method == "mean") {
          # Simple mean
          doc_vec <- colMeans(w2v_matrix[valid_terms, , drop = FALSE])
        } else {
          # TF-IDF weighted mean
          doc_id_val <- .y$doc_id

          weights <- tfidf_weights %>%
            filter(doc_id == doc_id_val, term_text %in% valid_terms) %>%
            arrange(match(term_text, valid_terms)) %>%
            pull(tfidf)

          if (length(weights) == 0 || sum(weights) == 0) {
            doc_vec <- colMeans(w2v_matrix[valid_terms, , drop = FALSE])
          } else {
            # Align weights with terms
            term_weight_map <- tfidf_weights %>%
              filter(doc_id == doc_id_val, term_text %in% valid_terms) %>%
              select(term_text, tfidf)

            weights_aligned <- sapply(valid_terms, function(t) {
              w <- term_weight_map$tfidf[term_weight_map$term_text == t]
              if (length(w) == 0) 1 else w
            })

            weighted_vecs <- sweep(
              w2v_matrix[valid_terms, , drop = FALSE],
              1,
              weights_aligned,
              "*"
            )
            doc_vec <- colSums(weighted_vecs) / sum(weights_aligned)
          }
        }

        tibble(!!!setNames(as.list(doc_vec), paste0("dim_", 1:length(doc_vec))))
      }
    ) %>%
    ungroup()

  # CRITICAL: Sanitize column names to be valid R variable names
  # Ensure consistency with DTM approach
  colnames(doc_embeddings) <- make.names(
    colnames(doc_embeddings),
    unique = TRUE
  )

  return(doc_embeddings)
}


#' Train Word2Vec Model
#'
#' Wrapper function to train word2vec on the corpus
#'
#' @param dfTag Data frame with tokenized text
#' @param term Character, "lemma" or "token"
#' @param dim Integer, embedding dimension
#' @param iter Integer, number of iterations
#' @return word2vec model object
trainWord2Vec <- function(dfTag, term = "lemma", dim = 100, iter = 20) {
  # Prepare text data
  df_filtered <- dfTag %>%
    filter(docSelected, POSSelected) %>%
    select(sentence_id, !!sym(term)) %>%
    mutate(term_lower = tolower(!!sym(term)))

  # Create list of sentences
  sentence_list <- df_filtered %>%
    group_by(sentence_id) %>%
    summarise(sentence = list(term_lower), .groups = 'drop') %>%
    pull(sentence)

  # Train model
  w2v_model <- word2vec(
    x = sentence_list,
    type = "cbow",
    dim = dim,
    iter = iter,
    window = 5,
    min_count = 2,
    threads = parallel::detectCores() - 1
  )

  return(w2v_model)
}


#' Create Stratified Train/Test Split
#'
#' Creates stratified indices for train/test split
#'
#' @param y Factor or character vector of class labels
#' @param p Numeric, proportion for training set
#' @param seed Integer, random seed
#' @return Integer vector of training indices
createStratifiedSplit <- function(y, p = 0.7, seed = 123) {
  set.seed(seed)

  # Get unique classes
  classes <- unique(y)
  train_idx <- c()

  # For each class, sample proportionally
  for (cls in classes) {
    cls_idx <- which(y == cls)
    n_train <- round(length(cls_idx) * p)

    # Sample indices for this class
    cls_train_idx <- sample(cls_idx, size = n_train, replace = FALSE)
    train_idx <- c(train_idx, cls_train_idx)
  }

  return(sort(train_idx))
}


#' Train Random Forest with Ranger
#'
#' Trains a random forest classifier using ranger package
#'
#' @param feature_matrix Data frame with features and 'target' column
#' @param test_size Numeric, proportion of data for test set
#' @param num.trees Integer, number of trees
#' @param mtry Integer, number of variables to try at each split (NULL = auto)
#' @param min.node.size Integer, minimum node size (NULL = auto)
#' @param class.weights Named vector of class weights (NULL = equal weights)
#' @param seed Integer, random seed
#' @return List with model, train_data, test_data, train_pred, test_pred
trainRangerRF <- function(
  feature_matrix,
  test_size = 0.3,
  num.trees = 500,
  mtry = NULL,
  min.node.size = NULL,
  class.weights = NULL,
  seed = 123
) {
  set.seed(seed)

  # Target column is named .target_class to avoid conflicts with term columns
  if (!".target_class" %in% names(feature_matrix)) {
    stop("Target column '.target_class' not found in feature matrix")
  }

  # Target should already be a factor from the calling function
  if (!is.factor(feature_matrix$.target_class)) {
    feature_matrix$.target_class <- as.factor(feature_matrix$.target_class)
  }

  # Stratified split using custom function
  train_idx <- createStratifiedSplit(
    y = feature_matrix$.target_class,
    p = 1 - test_size,
    seed = seed
  )

  train_data <- feature_matrix[train_idx, ]
  test_data <- feature_matrix[-train_idx, ]

  # Auto-calculate mtry if not provided
  if (is.null(mtry)) {
    mtry <- floor(sqrt(ncol(train_data) - 1))
  }

  # Train model
  model <- ranger::ranger(
    formula = .target_class ~ .,
    data = train_data,
    num.trees = num.trees,
    mtry = mtry,
    min.node.size = min.node.size,
    importance = 'impurity',
    probability = TRUE,
    class.weights = class.weights,
    num.threads = parallel::detectCores() - 1,
    seed = seed
  )

  # Predictions
  train_pred <- predict(model, data = train_data)
  test_pred <- predict(model, data = test_data)

  return(list(
    model = model,
    train_data = train_data,
    test_data = test_data,
    train_pred = train_pred,
    test_pred = test_pred
  ))
}


#' Evaluate Classification Performance
#'
#' Computes confusion matrix and per-class metrics
#'
#' @param true_labels Factor, true class labels
#' @param pred_probs Matrix, predicted probabilities (rows = samples, cols = classes)
#' @return List with confusion_matrix, accuracy, metrics_per_class
evaluateClassifier <- function(true_labels, pred_probs) {
  # Get predicted classes (highest probability)
  pred_classes <- factor(
    colnames(pred_probs)[apply(pred_probs, 1, which.max)],
    levels = levels(true_labels)
  )

  # Confusion matrix
  cm <- table(Predicted = pred_classes, Actual = true_labels)

  # Overall accuracy
  accuracy <- sum(diag(cm)) / sum(cm)

  # Per-class metrics
  classes <- levels(true_labels)
  metrics_list <- lapply(classes, function(cls) {
    tp <- cm[cls, cls]
    fp <- sum(cm[cls, ]) - tp
    fn <- sum(cm[, cls]) - tp
    tn <- sum(cm) - tp - fp - fn

    precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
    f1 <- ifelse(
      precision + recall > 0,
      2 * precision * recall / (precision + recall),
      0
    )

    tibble(
      class = cls,
      precision = precision,
      recall = recall,
      f1 = f1
    )
  })

  metrics_df <- bind_rows(metrics_list)

  return(list(
    confusion_matrix = cm,
    accuracy = accuracy,
    metrics_per_class = metrics_df
  ))
}


#' Plot Confusion Matrix
#'
#' Creates a ggplot visualization of confusion matrix
#'
#' @param cm Confusion matrix (table object)
#' @return ggplot object
plotConfusionMatrix <- function(cm) {
  # Convert to data frame
  cm_df <- as.data.frame(cm)
  colnames(cm_df) <- c("Predicted", "Actual", "Count")

  # Calculate percentages by actual class
  cm_df <- cm_df %>%
    group_by(Actual) %>%
    mutate(
      Total = sum(Count),
      Percentage = Count / Total * 100
    ) %>%
    ungroup()

  # Create plot
  p <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Count)) +
    geom_tile(color = "white", size = 1) +
    geom_text(
      aes(label = sprintf("%d\n(%.1f%%)", Count, Percentage)),
      color = "black",
      size = 4
    ) +
    scale_fill_gradient(
      low = "#fff7ec",
      high = "#7F0000",
      name = "Count"
    ) +
    labs(
      title = "Confusion Matrix",
      x = "Actual Class",
      y = "Predicted Class"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid = element_blank()
    ) +
    coord_fixed()

  return(p)
}


#' Plot Feature Importance
#'
#' Creates an interactive plotly bar chart of feature importance
#'
#' @param model ranger model object
#' @param feature_type Character, "DTM" or "Word2Vec"
#' @param top_n Integer, number of top features to show
#' @param term_mapping Data frame with original_term and sanitized_name columns (NULL for Word2Vec)
#' @return plotly object
plotFeatureImportance <- function(
  model,
  feature_type = "DTM",
  top_n = 30,
  term_mapping = NULL
) {
  # Extract importance
  importance_values <- model$variable.importance

  # Sort and get top N
  importance_df <- tibble(
    feature = names(importance_values),
    importance = importance_values
  ) %>%
    arrange(desc(importance)) %>%
    head(top_n)

  # If DTM and term_mapping is available, restore original terms
  if (feature_type == "DTM" && !is.null(term_mapping)) {
    importance_df <- importance_df %>%
      left_join(term_mapping, by = c("feature" = "sanitized_name")) %>%
      mutate(
        display_name = ifelse(!is.na(original_term), original_term, feature)
      )
  } else {
    importance_df <- importance_df %>%
      mutate(display_name = feature)
  }

  # Create label based on feature type
  if (feature_type == "DTM") {
    x_label <- "Terms"
    title_text <- "Top Terms by Importance"
  } else {
    x_label <- "Embedding Dimensions"
    title_text <- "Embedding Dimensions by Importance"
  }

  # Create plot
  p <- plot_ly(
    data = importance_df,
    x = ~importance,
    y = ~ reorder(display_name, importance),
    type = "bar",
    orientation = 'h',
    marker = list(
      color = importance_df$importance,
      colorscale = 'Viridis',
      showscale = TRUE,
      colorbar = list(title = "Importance")
    ),
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "Importance: %{x:.4f}<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = list(text = title_text, x = 0.5, xanchor = "center"),
      xaxis = list(title = "Importance (Gini)"),
      yaxis = list(title = x_label),
      margin = list(l = 150),
      hovermode = "closest"
    )

  return(p)
}


#' Export Classification Results
#'
#' Exports results to Excel file with multiple sheets
#'
#' @param rf_results Reactive values with classification results
#' @param filename Character, output file path
#' @param label_var Character, name of label variable
exportClassificationResults <- function(rf_results, filename, label_var) {
  # Create workbook
  wb <- openxlsx::createWorkbook()

  # Sheet 1: Model Summary
  summary_data <- tibble(
    Metric = c(
      "Feature Type",
      "Number of Trees",
      "Number of Features",
      "Training Set Size",
      "Test Set Size",
      "OOB Error (%)",
      "Train Accuracy (%)",
      "Test Accuracy (%)"
    ),
    Value = c(
      rf_results$feature_type,
      rf_results$model$num.trees,
      ncol(rf_results$train_data) - 1,
      nrow(rf_results$train_data),
      nrow(rf_results$test_data),
      round(rf_results$model$prediction.error * 100, 2),
      round(rf_results$eval_train$accuracy * 100, 2),
      round(rf_results$eval_test$accuracy * 100, 2)
    )
  )

  openxlsx::addWorksheet(wb, "Model_Summary")
  openxlsx::writeData(wb, "Model_Summary", summary_data)

  # Sheet 2: Performance Metrics
  metrics_data <- rf_results$eval_test$metrics_per_class %>%
    mutate(
      precision = round(precision, 4),
      recall = round(recall, 4),
      f1 = round(f1, 4)
    )

  openxlsx::addWorksheet(wb, "Performance_Metrics")
  openxlsx::writeData(wb, "Performance_Metrics", metrics_data)

  # Sheet 3: Confusion Matrix
  cm_data <- as.data.frame.matrix(rf_results$eval_test$confusion_matrix)
  cm_data <- cbind(Predicted = rownames(cm_data), cm_data)

  openxlsx::addWorksheet(wb, "Confusion_Matrix")
  openxlsx::writeData(wb, "Confusion_Matrix", cm_data)

  # Sheet 4: Feature Importance
  importance_data <- tibble(
    Feature = names(rf_results$model$variable.importance),
    Importance = rf_results$model$variable.importance
  ) %>%
    arrange(desc(Importance))

  # If DTM with term mapping, add original terms
  if (!is.null(rf_results$term_mapping)) {
    importance_data <- importance_data %>%
      left_join(
        rf_results$term_mapping,
        by = c("Feature" = "sanitized_name")
      ) %>%
      mutate(
        Original_Term = ifelse(!is.na(original_term), original_term, Feature)
      ) %>%
      select(Original_Term, Feature, Importance)
  }

  openxlsx::addWorksheet(wb, "Feature_Importance")
  openxlsx::writeData(wb, "Feature_Importance", importance_data)

  # Sheet 5: Test Predictions
  test_pred_classes <- colnames(rf_results$test_pred$predictions)[
    apply(rf_results$test_pred$predictions, 1, which.max)
  ]

  predictions_data <- tibble(
    Actual = as.character(rf_results$test_data$target),
    Predicted = test_pred_classes,
    Correct = Actual == Predicted
  )

  # Add probabilities
  prob_df <- as.data.frame(rf_results$test_pred$predictions)
  colnames(prob_df) <- paste0("Prob_", colnames(prob_df))
  predictions_data <- cbind(predictions_data, prob_df)

  openxlsx::addWorksheet(wb, "Test_Predictions")
  openxlsx::writeData(wb, "Test_Predictions", predictions_data)

  # Save workbook
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

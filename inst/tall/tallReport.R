addDataWb <- function(list_df, wb, sheetname, startRow = 1) {
  l <- length(list_df)
  # startRow <- 1
  for (i in 1:l) {
    df <- list_df[[i]]
    n <- nrow(df)
    writeDataTable(
      wb,
      sheetname,
      df,
      startRow = startRow,
      startCol = 1,
      tableStyle = "TableStyleMedium20"
    )
    startRow <- startRow + n + 3
  }
  return(wb)
}

addDataScreenWb <- function(list_df, wb, sheetname) {
  ind <- which(regexpr(sheetname, wb$sheet_names) > -1)
  if (length(ind) > 0) {
    sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
  }
  addWorksheet(wb = wb, sheetName = sheetname, gridLines = FALSE)
  if (!is.null(list_df)) {
    addDataWb(list_df, wb, sheetname, startRow = 1)
    col <- max(unlist(lapply(list_df, ncol))) + 2
  } else {
    col <- 1
  }

  results <- list(wb = wb, col = col, sheetname = sheetname)
  return(results)
}

addGgplotsWb <- function(
  list_plot,
  wb,
  sheetname,
  col,
  width = 10,
  height = 7,
  dpi = 75,
  startRow = 1
) {
  l <- length(list_plot)
  # startRow <- 1
  for (i in 1:l) {
    fileName <- tempfile(
      pattern = "figureImage",
      fileext = ".png"
    )
    if (inherits(list_plot[[i]], "ggplot")) {
      ggsave(
        plot = list_plot[[i]],
        filename = fileName,
        width = width,
        height = height,
        units = "in",
        dpi = dpi
      )
    }
    if (inherits(list_plot[[i]], "igraph")) {
      igraph2PNG(
        x = list_plot[[i]],
        filename = fileName,
        width = width,
        height = height,
        dpi = dpi
      )
    }
    insertImage(
      wb = wb,
      sheet = sheetname,
      file = fileName,
      width = width,
      height = height,
      startRow = startRow,
      startCol = col,
      units = "in",
      dpi = dpi
    )
    startRow <- startRow + (height * 6) + 1
  }
  return(wb)
}

# from igraph to png file
igraph2PNG <- function(x, filename, width = 10, height = 7, dpi = 75) {
  V(x)$centr <- centr_betw(x)$res
  df <- data.frame(
    name = V(x)$label,
    cluster = V(x)$color,
    centr = V(x)$centr
  ) %>%
    group_by(cluster) %>%
    slice_head(n = 3)
  V(x)$label[!(V(x)$label %in% df$name)] <- ""
  png(
    filename = filename,
    width = width,
    height = height,
    unit = "in",
    res = dpi
  )
  grid::grid.draw(plot(x))
  dev.off()
}


addScreenWb <- function(df, wb, width = 14, height = 8, dpi = 75) {
  names(df) <- c("sheet", "file", "n")
  if (nrow(df) > 0) {
    sheet <- unique(df$sheet)
    for (i in 1:length(sheet)) {
      sh <- sheet[i]
      df_sh <- df %>% dplyr::filter(.data$sheet == sh)
      l <- nrow(df_sh)
      startRow <- 30
      for (j in 1:l) {
        fileName <- df_sh$file[j]
        insertImage(
          wb = wb,
          sheet = sh,
          file = fileName,
          width = width,
          height = height,
          startRow = startRow,
          startCol = df_sh$n[j],
          units = "in",
          dpi = dpi
        )
        startRow <- startRow + (height * 10) + 3
      }
    }
  }
  return(wb)
}

addSheetToReport <- function(
  list_df,
  list_plot,
  sheetname,
  wb,
  dpi = 75,
  startRow = 1
) {
  ind <- which(regexpr(sheetname, wb$sheet_names) > -1)
  if (length(ind) > 0) {
    sheetname <- paste(sheetname, "(", length(ind) + 1, ")", sep = "")
  }
  addWorksheet(wb, sheetname, gridLines = FALSE)

  if (!is.null(list_df)) {
    col <- max(unlist(lapply(list_df, ncol))) + 2
    wb <- addDataWb(list_df, wb = wb, sheetname = sheetname, startRow = 1)
  } else {
    col <- 1
  }

  if (!is.null(list_plot)) {
    wb <- addGgplotsWb(
      list_plot,
      wb = wb,
      sheetname = sheetname,
      col = col,
      dpi = dpi,
      startRow = startRow
    )
  }
  # values$sheet_name <- sheetname
  return(wb)
}



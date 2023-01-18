# Button for Folder Selection ----

choose.dir = function(default = NA, caption = NA, useNew=TRUE) {
  if (Sys.info()['sysname'] == 'Darwin') {
    return(choose.dir.darwin(default = default, caption = caption))
  } else if (Sys.info()['sysname'] == 'Linux') {
    return(choose.dir.linux(default = default, caption = caption))
  } else if (Sys.info()['sysname'] == 'Windows') {
    # Use batch script to circumvent issue w/ `choose.dir`/`tcltk::tk_choose.dir`
    # window popping out unnoticed in the back of the current window
    return(choose.dir.windows(default = default, caption = caption, useNew = useNew))
  }
  return(paste("Error: don't know how to show a folder dialog in", Sys.info()['sysname']) )
}

choose.dir.darwin <- function(default = NA, caption = NA) {
  command = 'osascript'
  args = '-e "POSIX path of (choose folder{{prompt}}{{default}})"'

  if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
    prompt = sprintf(' with prompt \\"%s\\"', caption)
  } else {
    prompt = ''
  }
  args = sub('{{prompt}}', prompt, args, fixed = T)

  if (!is.null(default) && !is.na(default) && nzchar(default)) {
    default = sprintf(' default location \\"%s\\"', path.expand(default))
  } else {
    default = ''
  }
  args = sub('{{default}}', default, args, fixed = T)

  suppressWarnings({
    path = system2(command, args = args, stderr = TRUE)
  })
  if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
    # user canceled
    path = NA
  } else {
    # cut any extra output lines, like "Class FIFinderSyncExtensionHost ..."
    path = tail(path, n=1)
  }

  return(path)
}

choose.dir.linux <- function(default = NA, caption = NA) {
  command = 'zenity'
  args = '--file-selection --directory'

  if (!is.null(default) && !is.na(default) && nzchar(default)) {
    args = paste(args, sprintf('--filename="%s"', default))
  }

  if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
    args = paste(args, sprintf('--title="%s"', caption))
  }

  suppressWarnings({
    path = system2(command, args = args, stderr = TRUE)
  })

  #Return NA if user hits cancel
  if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
    # user canceled
    return(NA)
  }

  #Error: Gtk-Message: GtkDialog mapped without a transient parent
  if(length(path) > 1){
    path = path[(length(path))]
  }

  return(path)
}

choose.dir.windows <- function(default = NA, caption = NA, useNew = TRUE) {
  if(useNew){
    ## uses a powershell script rather than the bat version, gives a nicer interface
    ## and allows setting of the default directory and the caption
    whereisutils <- system.file("utils", 'newFolderDialog.ps1', package = "tall")
    command = 'powershell'
    args = paste('-NoProfile -ExecutionPolicy Bypass -File',normalizePath(whereisutils))
    if (!is.null(default) && !is.na(default) && nzchar(default)) {
      args = paste(args, sprintf('-default "%s"', normalizePath(default)))
    }

    if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
      args = paste(args, sprintf('-caption "%s"', caption))
    }

    suppressWarnings({
      path = system2(command, args = args, stdout = TRUE)
    })
   } else {
    whereisutils <- system.file("utils", 'choose_dir.bat', package = "tall")
    command = normalizePath(whereisutils)
    args = if (is.na(caption)) '' else sprintf('"%s"', caption)
    suppressWarnings({
      path = system2(command, args = args, stdout = TRUE)
    })
  }
  if (path == 'NONE') path = NA
  return(path)
}

directoryInput = function(inputId, label, value = NULL) {
  if (!is.null(value) && !is.na(value)) {
    value = path.expand(value)
  }
  version <- as.character(packageVersion("tall")[[1]])
  dep <- htmltools::htmlDependency(
    name = "tall-assets", version = version,
    package = "tall",
    src = "assets",
    script = "js/directory_input_binding.js"
  )
  tagList(
    shiny::div(
      class = 'form-group directory-input-container',
      `%AND%`(label, tags$label(label)),
      shiny::div(
        # shiny::span(
        #    class = 'col-xs-9 col-md-11',
        #    style = 'padding-left: 70px; padding-right: 0px',
        #shiny::div(
        class = 'input-group shiny-input-container',
        style = 'width:100%;',
        shiny::span(
          class = 'shiny-input-container',
          tags$button(
            id = inputId,
            style = 'padding-left: 50px; padding-right: 50px',
            title="Browse", # Tips
            class = 'btn btn-default directory-input',icon('folder-open', lib="glyphicon")
            #)
          ),
          #div(class = 'input-group-addon', icon('folder-open', lib="glyphicon")),
          tags$input(
            id = sprintf('%s__chosen_dir', inputId),
            value = value,
            type = 'text',
            class = 'form-control directory-input-chosen-dir',
            style = 'font-size: 12px;', #align??
            readonly = 'readonly'
          )
        ),
        #' shiny::span(
        #'   class = 'shiny-input-container',
        #'   tags$button(
        #'     id = inputId,
        #'     style = 'padding-left: 20px; padding-right: 20px',
        #'
        #'     class = 'btn btn-default directory-input',icon('folder-open', lib="glyphicon")
        #'     #'...'
        #'   )
        #' )
      )
    ),
    dep
  )

}

updateDirectoryInput = function(session, inputId, value = NULL, ...) {
  if (is.null(value)) {
    value = choose.dir(...)
  }
  session$sendInputMessage(inputId, list(chosen_dir = value))
}

readDirectoryInput = function(session, inputId) {
  session$input[[sprintf('%s__chosen_dir', inputId)]]
}

# AND infix operator ----
## Given x and y, return y only if both x and y are set
`%AND%` <- function(x, y) {
  if (!is.null(x) && !isTRUE(is.na(x)))
    if (!is.null(y) && !isTRUE(is.na(y)))
      return(y)
  return(NULL)
}

# IMPORT TEXT FUNCTIONS ----
txtImport <- function(folder, sep="__"){
  obj <- readtext::readtext(paste0(folder,"*"),
                            docvarsfrom = "filepaths",
                            dvsep = sep)
}

shortpath <- function(path){
  if (inherits(path,"character")){
    unlist(lapply(strsplit(path,"/"), function(l){
      l[length(l)]
    }))
  } else {NULL}
}

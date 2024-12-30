
tall_download_model <- function(file,
model_dir = getwd(),
model_repo = "2.15", 
overwrite = TRUE){

  filename <- paste0(file,"-ud-",model_repo,".udpipe")

    url <- file.path("https://raw.githubusercontent.com/massimoaria/udpipe.models/main",
    model_repo, filename)
    to <- file.path(model_dir, filename)
    download_failed <- FALSE
    download_message <- "OK"
    dl <- suppressWarnings(try(
      utils::download.file(url = url, destfile = to, mode = "wb"),  
      silent = TRUE))
    if(inherits(dl, "try-error")){
      download_failed  <- TRUE
      download_message <- as.character(dl)
    }else if(inherits(dl, "integer") && dl != 0){
      download_failed  <- TRUE
      download_message <- "Download failed. Please check internet connectivity"
    }
    if(download_failed){
      message("Something went wrong")
      message(download_message)
    }else{
      message(sprintf("Downloading finished, model stored at '%s'", to))
    }
  
    data.frame(language = language,
      file_model = to,
      url = url,
      download_failed = download_failed,
      download_message = download_message,
      stringsAsFactors = FALSE)

}

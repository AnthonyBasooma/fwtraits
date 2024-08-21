#' Title
#'
#' @param type Either taxa or token
#'
#' @return
#' @export
#'
#' @examples
#'
decache <- function(type = 'token', action= 'reset'){

  match.arg(type, choices = c('token', 'taxa'))
  match.arg(action, choices = c('destroy', 'reset'))

  if(type=='token'){
    cacheddata <- cachem::cache_disk(dir = 'authkey')
  }else{
    cacheddata <- cachem::cache_disk(dir = 'taxadata')
  }

  if(action=='reset'){

    msg <- paste0("Do you want to irreversiblly remove all cached ", type," files from disk?")

    tf <- askYesNo(msg, default = TRUE,
                   prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))))
    if(isTRUE(tf)){

      warning("All ", type," have been unreversibly removed and they have to be re-downloaded.", call. = FALSE)

      cacheddata$reset()

      invisible(NULL)

    }else{
      message("No ", type, " file removed from the disk.")
    }
  }else{

  msg2 <- paste0("Do you want to irreversibly remove the ", type," cache directory from disk?")

  tf <- askYesNo(msg2, default = TRUE,
                 prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))))
  if(isTRUE(tf)){

    cacheddata$destroy()

    warning("The ", type," folder has been unreversibly removed from disk and cached data has to be re-downloaded.", call. = FALSE)

    invisible(NULL)

  }else{
    message("The ", type," folder will not be removed from the disk.")
  }
  }
}

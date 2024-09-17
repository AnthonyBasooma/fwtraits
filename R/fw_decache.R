#' @title Decache tokens and data files.
#'
#' @param type \code{string}. Either \code{taxa} or \code{token} to be removed from the data.
#' @param action \code{string}. Either \code{reset} to remove token or data files or
#'      \code{destroy} to remove all the files and cache directory.
#'
#' @return \code{message}. files or token cache token or taxa files and directory removed.
#'
#' @export
#'
#' @importFrom utils askYesNo
#'
#' @examples
#'
#' \dontrun{
#'
#' #to remove the token but not delete the folder
#'
#' fw_decache(type= 'token', action= 'reset' )
#'
#' #delete the folder
#'
#' fw_decache(type= 'token', action= 'destroy' )
#'
#' }
#'
fw_decache <- function(type = 'token', action= 'reset'){

  match.arg(type, choices = c('token', 'taxa'))

  match.arg(action, choices = c('destroy', 'reset'))

  if(type=='token'){

    cacheddata <- memoise::cache_filesystem(path = 'authtoken')
  }else{
    cacheddata <- memoise::cache_filesystem(path = 'taxadata')
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

    warning("The ", type," folder has been unreversibly removed from disk and you will restart the current R session.", call. = FALSE)

    invisible(NULL)

  }else{
    message("The ", type," folder will not be removed from the disk.")
  }
  }
}

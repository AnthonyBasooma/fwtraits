#save the API key in the user r environment


#' Function and instructions creating the API_KEY variable in the User environment.
#'
#' @importFrom utils file.edit
#' @export
#'
fw_setapikey <- function(){

  cat("  =============================================",'\n',
      " =============================================",'\n',
      "PLEASE FOLLOW THE INSTRUCTIONS CAREFULLY",'\n',
      " =============================================",'\n',
      "1. PLEASE EDIT THE .Renviron THATS HAS OPENED",'\n',
      "==                                         ",'\n',
      "2. TYPE IN: API_KEY = 36-digits alphanumeric",'\n',
      "==                                         ",'\n',
      "3. SAVE AND CLOSE. RESTART RSESSION",'\n',
      "==                                         ",'\n',
      "4. CONGS: YOUR ARE READY TO GO!!!!!!!!",'\n',
      "============================================",'\n',
      "============================================",'\n')

  file.edit("~/.Renviron")
}


#'For loading the API key from the environment.
#' @export
fw_keyload <- function() {

  # check if the API Key is saved already and possibly not the case

  key_loaded <- Sys.getenv(x = "API_KEY")

  if(key_loaded=="") stop('The API key has not been well set in the .Renviron file. Run fw_setapikey() and follow the instruction carefully.', call. = FALSE)

  if(grepl("[A-Za-z0-9]", key_loaded) !=TRUE) stop("The key provided save in .Renviron is wrong. Please repeat saving and run again.", call. = FALSE)

  if (nchar(key_loaded) != 36) stop("Please check the API Key provided it exceeds 36 characters.")

  cat(
    " ========================================================================", "\n",
    "Congratulations!!! Ready to interact with www.freshwaterecology.info database", "\n",
    "=========================================================================", "\n"
  )
  return(key_loaded)
}





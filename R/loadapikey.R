#' Title
#'
#' @return
#' @export
#'
#' @importFrom askpass askpass
#'
#' @examples
loadapikey <- function() {
  # check if the FRESHWATERAPIKEY has been set and has the API key

  apikey <- Sys.getenv("FRESHWATERAPIKEY")

  if (nchar(apikey) == 36) {
    return(apikey)
  } else {
    apikey <- askpass(prompt = "Paste API key ")

    # validate the apikey before saving in the environment

    if (nchar(apikey) != 36) stop("Wrong API key. Paste the correct API or contact FIP admin at irv@irv-software.at or ask@boku.ac.at", call. = FALSE)

    # set the system environment to save the token
    Sys.setenv("FRESHWATERAPIKEY" = apikey)

    # get the token

    getkey <- Sys.getenv("FRESHWATERAPIKEY")

    if (is.null(getkey)) {
      stop("The API key is not provided")
    } else {
      cat(
        " ===============================================================", "\n",
        "Congs!!! Ready to interact with Freshwater Information Platform.", "\n",
        "================================================================", "\n"
      )

      return(apikey)
    }
  }
}

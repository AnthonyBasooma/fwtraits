#' @title Steps to follow in using the Freshwater Information Platform
#'
#' @return \code{list} steps to follow in using the FIP
#'
#' @importFrom rstudioapi viewer
#'
#' @export
#'
#' @examples
#'
#' #b4us <- before_u_start()
#'
#' @author Anthony Basooma
#'
# before_u_start <- function() {
#   step1 <- "Step 1: Register with in the Freshwater Information Platform at https://www.freshwaterecology.info/register/index.php"
#
#   step2 <- "Step 2: Apply for the API Key by sending your details at (irv@irv-software.at) or (ask@boku.ac.at) "
#
#   step3 <- "Step 3: Copy and paste the API key by running fip_token(), which will prompt for the key"
#
#   step4 <- "Congs: Ready to interact with Freshwater Information Platform will appear."
#
#   return(list(step1, step2, step3, step4))
# }

before_u_start <- function() {
  dir <- tempfile()

  dir.create(dir)

  htmlFile <- file.path(dir, "beforeustart.html")

  writeLines("<h4>Before you start please check the steps carefully</h4>
  <hr>
<p> Step 1: Register with in the at Freshwater Information Platform <a style='color: blue'
href='https://www.freshwaterecology.info/register/index.php'> registration page</a> </p>

 <p> Step 2: Apply for the API Key by sending your details at (irv@irv-software.at) or (ask@boku.ac.at) </p>

 <p> Step 3: Copy and paste the API key by running fip_token(), which will prompt for the key</p>

 <p> Step 4: Congs: Ready to interact with Freshwater Information Platform will appear</p>

 <p>                       <b>Prepared by Anthony Basooma (anthony.basooma@boku.ac.at)   </p>

</br>", con = htmlFile)

  viewer(htmlFile)

}



#' @importFrom askpass askpass
#'
#' @noRd
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




#' @title Function to retrieve all the traits in the database.
#'
#' @return \code{list} List of traits for all the taxa groups and orders in the database.
#'
#' @export
#'
#' @examples
#'
#' x <- fip_paramlist()
#'
#' @author Anthony Basooma
#'
fip_paramlist <- function() {

  # base url with parameter list

  paramurl <- "https://www.freshwaterecology.info/fweapi2/v1/getecoparamlist"

  paramlist <- request(base_url = paramurl) |>
    req_perform() |>
    resp_body_json()

  return(paramlist)
}

#' @noRd
#'
fip_classes <- function(paramlist) {
  av <- sapply(paramlist, function(x) strsplit(x[[7]], split = ", ", fixed = TRUE)[[1]])

  txall <- unique(do.call(c, av))

  txallfinal <- txall[!txall %in% c("all classes")]

  return(txallfinal)
}

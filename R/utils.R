#' @title Steps to follow in using the Freshwater Information Platform
#'
#' @return \code{list} steps to follow in using the FWDB
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
#   step3 <- "Step 3: Copy and paste the API key by running fw_token(), which will prompt for the key"
#
#   step4 <- "Congs: Ready to interact with Freshwater Information Platform will appear."
#
#   return(list(step1, step2, step3, step4))
# }

before_u_start <- function() {
  dir <- tempfile()

  dir.create(dir)

  htmlFile <- file.path(dir, "beforeustart.html")

  writeLines("<h3>fwtraits</h3>
  <hr>
  <h4> 1. Aim of the package </h4>
  <p> The package provides robust and seamless access and interation with the
  Freshwaterecology.info database, while extracting and arranging the species ecological preferences.</p>
  <p> To start interacting with the platform, pleases follow the steps below carefully.</p>
  <p> <center>THANK YOU </center></p>
  <hr>
  <h4> 2. Step to start interacting with Freshwaterecology.info database </h4>
<p> <b>Step 1</b>: Register at the Freshwater Information Platform <a style='color: blue'
href='https://www.freshwaterecology.info/register/index.php'> registration page</a> to obtain an API key. </p>

 <p> <b>Step 2</b>: Apply for the API Key by sending your details at irv@irv-software.at or ask@boku.ac.at </p>

 <p> <b>Step 3</b>: Copy and paste the API key by running fw_token(), which will prompt for the key.</p>

 <p> <b>Step 4</b>: Congratulations: Ready to interact with Freshwater Information Platform will appear.</p>

 <p> To cite the work, please run fwcite() </p>
 <hr>

 <h4>3. Literature cited </h4>

 <ol>
  <li style = 'font-size: 12px;'>Schmidt-Kloiber, A., & Hering, D. (2015).
  Www.freshwaterecology.info - An online tool that unifies, standardises and
  codifies more than 20,000 European freshwater organisms and their ecological
  preferences. Ecological Indicators, 53, 271-282.
  https://doi.org/10.1016/j.ecolind.2015.02.007</li>
</ol>

 <p> <center><b>Prepared by Anthony Basooma (anthony.basooma@boku.ac.at)  </center> </p>

</br>", con = htmlFile)

  viewer(htmlFile)

}



#' @importFrom askpass askpass
#' @importFrom httr2 secret_decrypt
#'
#' @noRd
loadapikey <- function(test = FALSE, encrytedkey= NULL, fwtraitskey= NULL) {

  if(isFALSE(test)){

    # check if the FRESHWATERAPIKEY has been set and has the API key

    apikey <- Sys.getenv("FRESHWATERAPIKEY")

    if (nchar(apikey) == 36) {
      return(apikey)
    } else {
      apikey <- askpass(prompt = "Paste API key ")

      # validate the apikey before saving in the environment

      if (nchar(apikey) != 36) stop("Wrong API key. Paste the correct API or contact FWDB admin at irv@irv-software.at or ask@boku.ac.at", call. = FALSE)

      # set the system environment to save the token
      Sys.setenv("FRESHWATERAPIKEY" = apikey)

      # get the token

      getkey <- Sys.getenv("FRESHWATERAPIKEY")

      if (is.null(getkey)) {
        stop("The API key is not provided")
      } else {
        cat(
          " ===============================================================", "\n",
          "Congratulations!!! Ready to interact with Freshwater Information Platform", "\n",
          "================================================================", "\n"
        )

        return(apikey)
      }
    }
  }else{
    apikey <- secret_decrypt(encrytedkey, fwtraitskey)
  }
}




#' @title Function to retrieve all the traits in the database.
#'
#' @return \code{list} List of traits for all the taxa groups and orders in the database.
#'
#' @examples
#'
#' @keywords internal
#'
#' x <- fw_paramlist()
#'
#' @author Anthony Basooma
#'
fw_paramlist <- function() {

  # base url with parameter list

  paramurl <- "https://www.freshwaterecology.info/fweapi2/v1/getecoparamlist"

  paramlist <- request(base_url = paramurl) |>
    req_perform() |>
    resp_body_json()

  return(paramlist)
}

#' @noRd
#'
fw_classes <- function(paramlist) {

  av <- sapply(paramlist, function(x) strsplit(x[[7]], split = ", ", fixed = TRUE)[[1]])

  txall <- unique(do.call(c, av))

  txallfinal <- txall[!txall %in% c("all classes")]

  return(txallfinal)
}

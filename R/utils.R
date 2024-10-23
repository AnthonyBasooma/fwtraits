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
#' #b4us <- fw_be4ustart()
#'
#' @author Anthony Basooma
#'


fw_be4ustart <- function() {

  dir <- tempfile()

  dir.create(dir)

  htmlFile <- file.path(dir, "beforeustart.html")

  #user1 <- Sys.info()[["user"]]

  writeLines("
  <body style='background-color:white;'>
  <h3>fwtraits</h3>
  <hr>


  <h4> 1. Aim of the package </h4>
  <p> The package provides robust and seamless access and interaction with the
  Freshwaterecology.info database, while extracting and arranging the species
  ecological preferences, traits or indicators. The database includes traits or
  ecological preferences for five taxonomic groups including fishes,
  macrophtes, macroinvertebrates, phytoplankton, and phytobenthos including diatoms. Over
  203 traits or ecological preferences exists for about 28000 species.</p>

  <h4> 2. Step to start interacting with Freshwaterecology.info database </h4>

  <p> <b>STEP 1</b>: If you are not registered with the Freshwater Information Platform, where the Freshwaterecology.info
  database is part of, proceed to <b> STEP 2</b>, otherwise proceed to STEP 3</p>

<p> <b>Step 2</b>: Register at the Freshwaterecology.info database <a style='color: blue'
href='https://www.freshwaterecology.info/register/index.php'> registration page</a> to obtain an API key. During registration, please tick
for acquistion of the API key which will be shortly sent to the email entered. After proceed to STEP 4 </p>

 <p> <b>STEP 3</b>: Apply for the API Key by sending your request for the key to registration@freshwaterecology.info </p>

 <p> <b>STEP 4</b>: The API key is an alphanumeric 36 character string.
 Advisable to copy and paste the key in the prompt got after executing the fw_token(). For safety reasons
 do not put or save your key in the analysis codes as the fw_token will use user sessions to store your key.</p>

 <p> <b>STEP 5</b>: Congratulations: Ready to interact with Freshwaterecology.info database will appear.</p>

 <p> For issues about the package fucntionality, please contact anthony.basooma@boku.ac.at or post on
 <a style='color: blue'
href='https://github.com/AnthonyBasooma/fwtraits/issues pages'> isuues page</a>

 <h4>3. Literature cited </h4>

  Schmidt-Kloiber, A., & Hering, D. (2015).
  Www.freshwaterecology.info - An online tool that unifies, standardises and
  codifies more than 20,000 European freshwater organisms and their ecological
  preferences. Ecological Indicators, 53, 271-282.
  https://doi.org/10.1016/j.ecolind.2015.02.007

 <hr>
 <p> <center><b>Prepared by Anthony Basooma (anthony.basooma@boku.ac.at)  </center> </p>
 <p> <center>THANK YOU </center></p>

</br></body>" , con = htmlFile)

  viewer(htmlFile)
}


#' @title To load the API key
#'
#' @description
#' The function allows the user to protect the key in user system environment to avoid
#' it being included in the analysis codes.
#'
#' @param test \code{logical}. If TRUE, then function is used with \code{secret_make_key}
#'        to encrypt the API key while publicly testing shared code.
#' @param sacrambled_apikey \code{alphanumeric} An alphanumeric string generated from \code{secret_make_key} fucntion
#'        while testing the codes.
#' @param fwtraitskey \code{string} A string that indicates key in the edited Rsession.
#'
#' @return encrypted API key
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' enc_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"
#'
#' #the FWTRAITS_KEY is the unlock key saved in my local environment
#' #check https://httr2.r-lib.org/articles/wrapping-apis.html for more information
#'
#' apikey <- httr2::secret_decrypt(encrypted = enc_api, key = 'FWTRAITS_KEY')
#'
#' #download fish catchment region data
#' #setting the FWTRAITS_KEY
#'
#' #run this usethis::edit_r_environ()
#'
#' apikeydecrypted <- fw_loadapikey(test = TRUE, sacrambled_apikey = enc_api,
#'                               fwtraitskey =  'FWTRAITS_KEY')
#'
#' }
#'
#' @seealso \code{\link{fw_token}}, \code{\link{fw_fetchdata}}

fw_loadapikey <- function(test = FALSE, sacrambled_apikey = NULL, fwtraitskey= NULL) {

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
          " ========================================================================", "\n",
          "Congratulations!!! Ready to interact with Freshwaterecology.info database", "\n",
          "=========================================================================", "\n"
        )

        return(apikey)
      }
    }
  }else{
    apikey <- secret_decrypt(sacrambled_apikey, fwtraitskey)
  }
}


#' @title Function to retrieve all the traits in the database.
#'
#' @return \code{list} List of traits for all the taxa groups and orders in the database.
#'
#' @keywords internal
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

  taxagrouplists <- sapply(paramlist, function(x) strsplit(x[[7]], split = ", ", fixed = TRUE)[[1]])

  txall <- unique(do.call(c, taxagrouplists))

  txallfinal <- txall[!txall %in% c("all classes")]

  return(txallfinal)
}




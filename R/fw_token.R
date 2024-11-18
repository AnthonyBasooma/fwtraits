#' @title Access and loading the token key
#'
#' @description
#' The function and updates the authentication token that is automatically generated every after
#' six hours by the servers. The function uses the API key, which is a one time key that is provided
#' during registration or provided by database managers for already registered users. Since the authentication
#' token expires, the seed parameter is included to allow caching across user sessions. Therefore,
#' the data downloaded with a particular seed will be in stored in memory and can be retrieved from the user
#' PC than from servers and hence tremendously optimizing on the speed on data access.
#' The token is generated in two different ways depending on whether the codes will be shared
#' with others or they are personal use. If they are for personal use, then the API key is directly pasted in the
#' pop up after fw_token is executed. However, in the latter circumstance, the API key advisable to
#' be stored in the R user environment and encrypted during code execution. Check the vignettes for
#' handling the API key on fwtraits GitHub.
#'
#'
#' @param apikey \code{string}. The API key which is automatically loaded using the loadapikey() internal function.
#' @param seed \code{integer}. An integer to help track the caching of the access token generated during data collation.
#'        If a user wants to get a new token, then the seed should be changed.
#' @param inform \code{logical}. To indicate if the token is successfully generated. Default \code{TRUE}.
#' @param cachepath \code{string}. The root path were the cached data will be saved on the user PC.
#'      If the path is not provided, the cached information will be saved in the current
#'      working directly.
#'
#' @importFrom curl has_internet
#' @importFrom httr2 req_headers secret_decrypt
#' @importFrom askpass askpass
#'
#' @return \code{string} token authentication token key
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #1.Use the API key in shared R examples
#'
#' #check https://httr2.r-lib.org/articles/wrapping-apis.html for more information
#'
#' #step 1
#' #sessionkey <- secret_make_key()
#'
#' #edit this page with usethis::edit_r_environ()
#'
#' #enc_api <- secret_encrypt(x = 'apikey', key = sessionkey)
#'
#' #encrypted token for my api key
#'
#' enc_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"
#'
#' #the FWTRAITS_KEY is the unlock key saved in my local environment.
#'
#' #download fish catchment region data
#'
#' #setting the FWTRAITS_KEY
#'
#' #run this usethis::edit_r_environ()
#'
#' apikeydecrypted <- loadapikey(test = TRUE, sacrambled_apikey = enc_api,
#'                               fwtraitskey =  'FWTRAITS_KEY')
#'
#' tokendata <- fw_token(key= apikeydecrypted, seed = 1234)
#'
#' }
#'
#'

fw_token <- function(apikey = fw_loadapikey(), seed= NULL, inform = FALSE, cachepath = NULL) {

  if(is.null(seed))stop("The seed must be provided")

  if(!is(seed, 'numeric')) stop("The seed must be a numeric e.g. 11211, 3421 ...")

  cachedir <- fw_path(cachepath)

  setCacheRootPath(path= cachedir)

  cache.root = getCacheRootPath()

  key <- list(apikey, seed)

  token <- loadCache(key)

  if(!is.null(token)){

    if(isTRUE(inform)) message('The token is being loaded from memory.')

    return(token)

  }else{

    if (!curl::has_internet()) stop("No internet connection detected. Connect to access database.")

    if (is.null(key)) stop("The APIKEY is not provided. Please register at https://www.freshwaterecology.info/register/index.php")

    if(is.null(seed)) stop('Please set seed to properly cache the data while interacting with the platform.')

    turl <- request("https://www.freshwaterecology.info/fweapi2/v1/token")

    set.seed(seed)

    #set seed to return the same token at particular moment
    reqtk <- tryCatch(expr = turl |> req_headers("Content-Type" = "application/json") |>

                        req_body_raw(body = toJSON(list(apikey = apikey), auto_unbox = TRUE)) |>

                        req_perform(), error = function(e) return(NULL))

    if (!is.null(reqtk)) {

      tokenout <- reqtk |> resp_body_json()

      token <- tokenout$access_token

      saveCache(token, key=key, comment="token code generated")
      #token;
    } else {
      stop("Invalid API key and apply at  https://www.freshwaterecology.info/register/index.php or visit fw_be4ustart().")
    }

  }
}

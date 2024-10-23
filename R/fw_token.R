#' @title Access and loading the token key
#'
#' @description
#' The function and updates the authentication token that is automatically generated every after
#' six hours by the servers. The function gets the API key, which is a one time key that is provided
#' during registration or by database managers for already registered users. Since the authentication
#' token expires, the seed parameter is included to allow caching across user sessions. Therefore,
#' the data downloaded with a particular seed will be in stored in memory and can be retrieved from the user
#' PC than from servers and hence tremendously optimizing on the speed on data access.
#'
#'
#' @param key \code{string} The API key which is automatically loaded using the loadapikey() internal function.
#' @param quietly \code{logical}. To indicate if the token is successfully generated. Default \code{TRUE}.
#' @param seed \code{integer}. An integer to help track the caching of the access token generated during data collation.
#'        If a user wants to get a new token, then the seed should be changed.
#'
#'
#' @importFrom curl has_internet
#' @importFrom httr2 req_headers
#' @importFrom askpass askpass
#' @importFrom httr2 secret_decrypt
#'
#'
#' @return \code{string} token authentication token key
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
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
#' #the FWTRAITS_KEY is the unlock key saved in my local environment
#' #check https://httr2.r-lib.org/articles/wrapping-apis.html for more information
#'
#' #download fish catchment region data
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

fw_token <- function(key = fw_loadapikey(), quietly = TRUE, seed= NULL) {

  if (!curl::has_internet()) stop("No internet connection detected. Connect to access database.")

  if (is.null(key)) stop("The APIKEY is not provided. Please register at https://www.freshwaterecology.info/register/index.php")

  if(is.null(seed)) stop('Please set seed to properly cache the data while interacting with the platform.')
  turl <- request("https://www.freshwaterecology.info/fweapi2/v1/token")

  set.seed(seed)

  #set seed to return the same token at particular moment
  reqtk <- tryCatch(expr = turl |> req_headers("Content-Type" = "application/json") |>
                      req_body_raw(body = toJSON(list(apikey = key), auto_unbox = TRUE)) |>
                      req_perform(), error = function(e) e)

  if (inherits(reqtk, "error")) {

    stop("Invalid API key provided, please apply for an API key at  https://www.freshwaterecology.info/register/index.php or visit before_u_start() function to set or get API key.")

    } else if (!inherits(reqtk, "error")) {

    tkperf <- reqtk |> resp_body_json()

    tokendata <- tkperf$access_token

    if (isFALSE(quietly)) message("Token genereated successfully.")
  } else {
    stop("Unable to access the database.")
  }
  return(tokendata)
}


# fw_token <- function(key = fw_loadapikey(), quietly = TRUE, seed = NULL, verbose = FALSE){
#
#   checkcache <- memoise::has_cache(token)(key, quietly, seed)
#
#   if(isFALSE(checkcache)){
#
#     tokendata <- token(key, quietly, seed)
#
#     if(isTRUE(verbose))message("Token generated with ", seed, " and is memoised for the data downloaded within 6 hours.")
#
#     }else{
#     token
#   }
# }
#
# tk <- readRDS(file = "C:\\Users\\anthbasooma\\Documents\\Anthony\\PhD\\AuaINFRA\\authtoken\\e8dab55102a55a836c2c8fb29d5122ed.rds")
#



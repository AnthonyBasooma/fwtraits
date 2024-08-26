#' @title Access and loading the token key
#'
#' @param key \code{string} The API key which is automatically loaded using the loadapikey() internal function.
#' @param quietly \code{logical}. To indicate if the token is successfully generated. Default \code{TRUE}.
#' @param seed \code{integer}. An integer to help track the caching of the access token generated during data collation.
#'        If a user wants to get a new token, then the seed should be changed.
#'
#'
#' @importFrom curl has_internet
#' @importFrom httr2 req_headers
#'
#'
#' @return \code{string} token authentication token key
#'
#' @export
#'
#' @examples
#'

fw_token <- function(key = loadapikey(), quietly = TRUE, seed= NULL) {

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

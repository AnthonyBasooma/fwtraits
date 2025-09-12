#' @title Access and loading the token key
#'
#' @description
#' The function updates the authentication token automatically, which the
#' servers generate every six hours. The function uses the API key, a one-time
#' key provided during registration or by database managers for already
#' registered users. Since the authentication token expires, the seed parameter
#' allows caching across user sessions. Therefore, the data downloaded with a
#' particular seed will be stored in memory and can be retrieved by the user. PC
#' rather than from servers, and hence, tremendously optimize the speed of data
#' access. The token is generated in two ways, depending on whether the codes
#' will be shared with others or for personal use. If they are for personal use,
#' the API key is directly pasted into the pop-up after the fw_token is
#' executed. However, in the latter circumstance, it is advisable to store the
#' API key in the R user environment and encrypt it during code execution. Check
#' the vignettes for handling the API key on the fwtraits GitHub.
#'
#'
#' @param apikey \code{string}. The API key is automatically loaded using
#'   the loadapikey() internal function.
#' @param seed \code{integer}. An integer to help track the caching of the
#'   access token generated during data collation. If a user wants a new token,
#'   the seed should be changed.
#' @param secure \code{logical}. If \code{TRUE}, the User will be prompted to
#'   set the API key in the .Renviron file by running the
#'   \code{\link{fw_setapikey}} function. The User must strictly type in API_KEY
#'   = 'api key', save, close the file and restart the R session or RStudio for
#'   the API_KEY environment to be captured. If \code{FALSE}, then the key will
#'   be entered directly in the API_KEY directly in the fw_token() function.
#'   This method is insecure, since other users can obtain the key from the codes.
#' @param cachefolder \code{string}. The root path where the cached data will be
#'   saved on the user's PC. If the path is not provided, the cached information
#'   will be saved in the current working directly.
#' @param inform \code{logical}. This is to indicate if the token has been
#'   successfully generated. Default \code{TRUE}.
#'
#'
#' @importFrom curl has_internet
#' @importFrom httr2 req_headers
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
#' }
#'
#' @seealso \code{\link{fw_setapikey}}

fw_token <- function(apikey = NULL, seed= NULL, cachefolder = NULL, secure = TRUE, inform = FALSE ) {

  cachedir <- fw_path(cachefolder)

  setCacheRootPath(path= cachedir)

  cache.root = getCacheRootPath()

  key <- list(apikey, seed)

  token <- loadCache(key)

  if(!is.null(token)){

    return(token)

  }else{

    xx <- try(curl::nslookup( host = "r-project.org"), silent = TRUE)
    if(inherits(xx, "try-error"))stop("The internet is required to obtain the API token and activate the key.")

    if(is.null(seed)) stop('Please set seed to properly cache the data while interacting with the platform.')

    turl <- request("https://www.freshwaterecology.info/fweapi2/v1/token")

    if(is.null(apikey)&& isFALSE(secure)) stop("Since you have set secure to FALSE, provide the API key directly in the apikey parameter. NOT A SAFE OPTION")

    set.seed(seed)

    if(isTRUE(secure)) apikey <- fw_keyload()

    #set seed to return the same token at particular moment
    reqtk <- tryCatch(expr = turl |> req_headers("Content-Type" = "application/json") |>

                        req_body_raw(body = toJSON(list(apikey = apikey), auto_unbox = TRUE)) |>

                        req_perform(), error = function(e) return(NULL))

    if (!is.null(reqtk)) {

      tokenout <- reqtk |> resp_body_json()

      token <- tokenout$access_token

      saveCache(token, key=key, comment="token code generated", compress = TRUE)

      token;
    } else {
      stop("Invalid API key, check properly or apply for a key at  https://www.freshwaterecology.info/register/index.php or visit fw_be4ustart().")
    }

  }
}

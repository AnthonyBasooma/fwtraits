#' @title Access and loading the token key
#'
#' @param key \code{string} The API key which is automatically loaded using the loadapikey() internal function.
#' @param quietly \code{logical}. To indicate if the token is successfully generated. Default \code{TRUE}.
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
tokenkey <- function(key = loadapikey(), quietly = TRUE) {

  if (!curl::has_internet()) stop("No internet connection detected. Connect to access database.")

  if (is.null(key)) stop("The APIKEY is not provided. Please register at https://www.freshwaterecology.info/register/index.php")

  turl <- httr2::request("https://www.freshwaterecology.info/fweapi2/v1/token")

  #set seed to return the same token at particular moment
  reqtk <- tryCatch(expr = turl |> req_headers("Content-Type" = "application/json") |>
                      req_body_raw(body = toJSON(list(apikey = key), auto_unbox = TRUE)) |>
                      req_perform(), error = function(e) e)

  if (inherits(reqtk, "error")) {
    stop("Invalid credentials, please apply for an API key at  https://www.freshwaterecology.info/register/index.php")
  } else if (!inherits(reqtk, "error")) {
    tkperf <- reqtk |> resp_body_json()

    tkn <- tkperf$access_token

    if (isFALSE(quietly)) message("Token genereated successfully.")
  } else {
    stop("Unable to access the database.")
  }
  return(tkn)
}



#' @title Generate and decache the user authentication token
#'
#' @param seed \code{integer}. An integer to help track the caching of the access token generated during data collation.
#'        If a user wants to get a new token, then the seed should be changed.
#' @param decache \code{string}. Either \code{key} and put a particular seed to remove a particular token from the storage disk or
#'        \code{all} to remove all the user authentication token from the system and therefore the associated will also be re-downloaded.
#' @param quietly \code{logical}. To indicate if the token is successfully generated. Default \code{TRUE}.
#'
#' @return
#' @export
#'
#' @examples
fip_token <- function(seed = NULL, decache = NULL, quietly = TRUE){

  tokenfile = cachem::cache_disk(dir = 'authkey')

  if(!is.null(seed)){

    #set token key to cache

    set.seed(seed)
    key <- paste0("tok", paste0(sample(x=c("ek","a",'a_','e','g_','m','0',1,2,3,4,5,6,7,8,9)),
                                collapse = ''))
    if(is.null(decache)){

      token <- tokenkey(quietly = quietly)

      tokenfile$set(key= key, value = token)

      return(token)

    }else if(decache=='key'){

      keyread <- tryCatch(expr = readRDS(file = file.path("authkey", paste0(key, '.rds'))),
                          warning= function(w) return(NULL))

      if(!is.null(keyread)){

        tokenfile$remove(key = key)

      }else{
        warning('No authentication token file found for the the set seed of ', seed, ' .', call. = FALSE)
        return(NULL)
      }

    }else{
      stop("Set decache to 'key' to remove file associated with the set seed or set seed to NULL to remove all keys.", call. = FALSE)
    }#end remove key
  }else if (decache=='all'){

    msg <- "Do you want to remove all cached token files?"

    tf <- askYesNo(msg, default = TRUE,
                   prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))))
    if(isTRUE(tf)){

      warning("All user authentication token keys will be lost and data will be re-downloaded.", call. = FALSE)
      tokenfile$reset()
      return(NULL)

    }else{
      message("No user authentication token removed from the disk.")
    }
  }else{
    stop("Either provide seed to generate or decache to remove cached tokens.", call. = FALSE)

  }
}


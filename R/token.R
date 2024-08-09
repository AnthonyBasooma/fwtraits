#' Title
#'
#' @param key
#' @param quietly
#'
#' @importFrom curl has_internet
#' @importFrom httr2 req_headers
#'
#' @return
#' @export
#'
#' @examples
fip_token <- function(key = loadapikey(), quietly = TRUE, seed) {
  if (!curl::has_internet()) stop("No internet connection detected. Connect to access database.")

  if (is.null(key)) stop("The APIKEY is not provided. Please register at https://www.freshwaterecology.info/register/index.php")

  turl <- httr2::request("https://www.freshwaterecology.info/fweapi2/v1/token")

  #set seed to return the same token at particular moment
  set.seed(seed)

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

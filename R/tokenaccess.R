

#' Title
#'
#' @param apikey gggg
#' @param quietly hhh
#'
#' @return
#'
#' @importFrom httr2 req_headers req_body_raw req_perform request
#' @importFrom curl has_internet
#'
#' @export
#'
#' @examples
#'
token <- function(apikey = NULL, quietly = TRUE){

  if(!has_internet())stop('No internet connection detected. Connect to access database.')

  if(is.null(apikey)) stop('The APIKEY is not provided. Please register at https://www.freshwaterecology.info/register/index.php')

  turl = request("https://www.freshwaterecology.info/fweapi2/v1/token")

  reqtk <- tryCatch(expr = turl |> req_headers("Content-Type" = 'application/json') |>

                      req_body_raw(body = toJSON(list(apikey = apikey), auto_unbox = TRUE))|>

                      req_perform(), error=function(e) e)


  if(inherits(reqtk, 'error')){


    stop('Invalid credentials, please apply for an API key at  https://www.freshwaterecology.info/register/index.php')


  }else if(!inherits(reqtk, 'error')){

    tkperf <- reqtk |> resp_body_json()

    tkn = tkperf$access_token

    if(isFALSE(quietly)) message('Token genereated successfully.')

  }else{
    stop('Unable to access the database.')
  }
  return(tkn)
}

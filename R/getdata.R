#' @title Get data from the freshwater information platform
#'
#' @param taxa hhzz
#' @param token hhh
#'
#' @importFrom httr2 request req_auth_bearer_token resp_body_json
#' @importFrom jsonlite toJSON
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#' x <- 1
#' }
#'
#'
getData <- function(taxa, species = NULL, token = NULL){

  if(length(taxa)>1)stop("Only one taxonomic group such as fish, macroinvertebrates is allowed to be obtained.")

  urldata <- urltaxa()

  match.arg(taxa, choices = c('fish', 'fi', 'macroinvertebrates', 'inverts', 'mi'))

  #validate the token id

  if(is.null(token)){

    stop("The token key is not provided.")

  }else if(nchar(token)<100) {

    stop('Provide a valid API key to access the FPI database.')

  }else{

    if(taxa=='fish' || taxa=='fi'){

      tlist = urldata[[2]][[1]]

      params <- urldata[[3]]

      codes = sapply(params$ecoparamlist$fi, function(x) x$code)

    }else if(taxa=='macroinvertebrates'|| taxa=='inverts' || taxa=='mi'){

      tlist = urldata[[2]][[2]]

      codes = sapply(params$ecoparamlist$mi, function(x) x$code)

    }else{
      stop('Invalid taxa name')
    }
    #access the different taxa tables

    tdata <- lapply(tlist, function(x){

      ldata <- request(base_url = urldata[[1]])  |> req_auth_bearer_token(token = token) |>

        req_body_raw(body = toJSON(list(organismgroup= taxa, taxagroup = x,
                                        displayValues = list(ecologicalparameter=codes)),
                                   auto_unbox = TRUE), type = "application/json") |>
        req_perform()


      fxdata <- ldata |> resp_body_json()


      dfdata <- as.data.frame(do.call(rbind, fxdata$searchResult))

      return(dfdata)
    })

    taxadata <- do.call(rbind, tdata)

    return(taxadata)
  }
}
# Acipenser
# ruthenus

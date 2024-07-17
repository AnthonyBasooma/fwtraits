


#' Title
#'
#' @param data ggg
#' @param taxacol  gg
#'
#' @return
#'
#' @importFrom specleanr check_names
#' @export
#'
#' @examples
cleanednames <- function(data, taxacol='Taxonname'){

  sx <- check_names(data = data, colsp = taxacol, pct = 90, verbose = F, merge = F)

  sclean = unlist(sx$speciescheck)

  return(sclean)
}


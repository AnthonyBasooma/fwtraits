
#' Title
#'
#' @param taxagroup
#' @param traits
#'
#' @return
#' @export
#'
#' @examples
#'
traitguide <- function(taxagroup = NULL, traits = NULL){

  paramlist <- fip_paramlist()

  ecolist <- paramlist$ecologicalParameterList

  taxagroup <- sapply(names(ecolist), function(z){

    taxanames <- harmonisetaxa(tx = z)

    zdata <- ecolist[[z]]

    xgroup <- sapply(zdata, function(x1){x1[[8]]})

    xclean <- xgroup[sapply(xgroup, length) > 0]

    xnames <- sapply(zdata, function(x1){x1[[2]]})

    len <- sapply(zdata, function(x1){length(x1[[8]])})

    xnamesclean <- xnames[which(len>0)]

    extract <- function(y, traitname){

      traitname <- clean_traits(traitname)

      name <- sapply(y, function(x) if(is.null(x[[1]])==TRUE) "No names" else x[[1]])

      abbr <- sapply(y, function(x) if(is.null(x[[2]])==TRUE) "No abbr" else x[[2]])

      expl <- sapply(y, function(x) if(is.null(x[[3]])==TRUE) "Not explained" else x[[3]])

      dfguide <- data.frame(taxagroup = taxanames, traitname = traitname, name = name, abbr = abbr, explanation = expl)
    }

    groupdata <- mapply(extract, y = xclean, traitname = xnamesclean, SIMPLIFY = FALSE)

    groupfinal <- do.call(rbind, groupdata)

  }, simplify = FALSE)

  dfinal <- Reduce(rbind, taxagroup)

  if(!is.null(traits)){
    taxafinal <- dfinal[dfinal$taxagroup %in% taxagroup | dfinal$traitname %in% traits, ]
  }else{
    taxafinal <- dfinal
  }


  return(taxafinal)
}

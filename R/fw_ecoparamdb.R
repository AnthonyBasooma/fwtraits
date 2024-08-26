
#' @title Standard table with taxonomic groups and their traits explanations.
#'
#' @param taxagroup \code{string or vector}. Taxa group names to aid the users in filtering the standard
#'      table for species traits and their explanations.
#' @param traits \code{string or vector}. Traits that can be filtered from the databases, given the taxonomic group
#'      indicated in \code{taxagroup} parameter.
#'
#' @return \code{dataframe} A dataset with taxonomic groups, traits and their explanations.
#' @export
#'
#' @examples
#'
fw_ecoparamdb <- function(taxagroup = NULL, traits = NULL){

  paramlist <- fw_paramlist()

  ecolist <- paramlist$ecologicalParameterList

  groupfiles <- sapply(names(ecolist), function(z){

    taxanames <- tcheck(tx = z)

    zdata <- ecolist[[z]]

    xgroup <- sapply(zdata, function(x1){x1[[8]]})

    xclean <- xgroup[sapply(xgroup, length) > 0]

    xnames <- sapply(zdata, function(x1){x1[[2]]})

    len <- sapply(zdata, function(x1){length(x1[[8]])})

    xnamesclean <- xnames[which(len>0)]

    extract <- function(y, traitname){

      traitclean <- clean_traits(traitname)

      traitunclean <- traitname

      name <- sapply(y, function(x) if(is.null(x[[1]])==TRUE) "No names" else x[[1]])

      abbr <- sapply(y, function(x) if(is.null(x[[2]])==TRUE) "No abbr" else x[[2]])

      expl <- sapply(y, function(x) if(is.null(x[[3]])==TRUE) "Not explained" else x[[3]])

      dfguide <- data.frame(taxagroup = taxanames, traitraw = traitunclean,
                            traitname = traitclean, description = name, abbrevation = abbr,
                            explanation = expl)
    }

    groupdata <- mapply(extract, y = xclean, traitname = xnamesclean, SIMPLIFY = FALSE)

    groupfinal <- do.call(rbind, groupdata)

  }, simplify = FALSE)

  dfinal <- Reduce(rbind, groupfiles)

  if(!is.null(traits)){
    taxafinal <- dfinal[dfinal$taxagroup %in% taxagroup | dfinal$traitname %in% traits, ]
  }else{
    taxafinal <- dfinal
  }

  return(taxafinal)
}

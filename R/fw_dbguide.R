
#' @title Standard table with taxonomic groups and their traits explanations.
#'
#' @param organismgroup \code{string or vector}. Taxa group names to aid the users in filtering the standard
#'      table for species traits and their explanations.
#' @param traits \code{string or vector}. Traits that can be filtered from the databases, given the organism group
#'      indicated in \code{organismgroup} parameter.
#'
#' @return \code{dataframe} A dataset with taxonomic groups, traits and their explanations.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' dbase <- fw_dbguide()
#'
#' }
#'
fw_dbguide <- function(organismgroup = NULL, traits = NULL){

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

      dfguide <- data.frame(organismgroup = taxanames, parameters_raw = traitunclean,
                            parameters_cleaned = traitclean, traitvalue = name, parameterabbrevation = abbr,
                            parameterexplanation = expl)
    }

    groupdata <- mapply(extract, y = xclean, traitname = xnamesclean, SIMPLIFY = FALSE)

    groupfinal <- do.call(rbind, groupdata)

  }, simplify = FALSE)

  dfinal <- Reduce(rbind, groupfiles)

  if(!is.null(organismgroup)){

    standardabbr <- c('pp', 'fi', 'mi','di', 'pb', 'mp')

    if(any(organismgroup %in% standardabbr)==TRUE){

      xgroups <- unlist(sapply(organismgroup, function(x) tcheck(x),  USE.NAMES = FALSE))

    }else{
      stop('Use the stanadrd organism group abbreviations including ', standardabbr)
    }

    taxafinal <- dfinal[dfinal$organismgroup %in% xgroups, ]

  }else{
    taxafinal <- dfinal
  }

  return(taxafinal)
}

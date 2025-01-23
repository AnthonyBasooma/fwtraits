
#' @title Standard table with taxonomic groups and their traits explanations.
#'
#' @param organismgroup \code{string or vector}. Taxa group names to aid the users in filtering the standard
#'      table for species traits and their explanations.
#' @param cachefolder \code{string}. The root path were the cached data will be saved on the user PC.
#'      If the path is not provided, the cached information will be saved in the current
#'      working directly.
#'
#' @return \code{dataframe} A dataset with taxonomic groups, traits and their explanations.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' dbase <- fw_dbguide(cachefolder = 'cache')
#'
#' }
#'
fw_dbguide <- function(organismgroup = NULL,cachefolder = 'cache'){

  paramlist <- fw_paramlist(cachefolder = cachefolder)

  ecolist <- paramlist$ecologicalParameterList

  if(!is.null(organismgroup)) {

    if(all(organismgroup%in%names(ecolist))==TRUE) ecolist <- ecolist[organismgroup] else stop('Use the stanadrd organism group abbreviations including pp, fi, mi,di, pp, mp')
  }

  groupfiles <- sapply(names(ecolist), function(zz){

    zdata <- ecolist[[zz]]

    #extract the trait description

    categoryList <- sapply(zdata, function(x1)x1[['categoryList']])

    parameterNames <- sapply(zdata, function(x1){

      len <- length(x1[['categoryList']])

      #lWR has no category list
      if(len>0) name <- x1[['name']] else name <- 'length weight regression'

    })

    #extract classification system

    classificationNames <- sapply(zdata, function(x1){

      cls <- x1[['classificationSystem']]

      if(!is.null(cls) && !cls=="") cls else cls <- NA
    })

    DataType <- sapply(zdata, function(x1){

      cls <- x1[['classificationSystem']]

      if(!is.null(cls) && !cls==""){

        if(cls=='presence/absence assignment system' | cls=='single category assignment system'|
           cls == "presence/absence " | cls=='modified presence/absence assignment system'){

          datatype = 'Nominal'

        } else if(cls=='category' | cls=='single category assignment system' |
                  cls=="habitat preference parameters" | cls=="Life parameters"| cls=="score"){

          datatype = 'Factor'

        }else if(cls=="10 points" | cls == '10 point assignment system'){

          datatype = 'Fuzzy'

        }else if(cls=="metric value (0-6)" | cls=="metric value (1-3)" | cls == 'metric value (1-5)' |
                 cls == "saprobic preference parameters" | cls == "trophic preference parameters" |
                 cls == "pollution preference parameters"){

          datatype = 'Ordered'

        }
      }else{
        datatype = NA

      }
    })

    mm <- mapply(categoryList, parameterNames, classificationNames, DataType,
                 FUN = function(yx, px, cx, dx){

      parameterclean <- clean_traits(px)

      categoryNames <- sapply(yx, function(xx) if(is.null(xx[[1]])==TRUE | xx[[1]] =="") NA else xx[[1]])

      categoryAbbreviation <- sapply(yx, function(x) if(is.null(x[[2]])==TRUE| x[[2]] =="") NA else x[[2]])

      categoryExplanation <- sapply(yx, function(x) if(is.null(x[[3]])==TRUE | x[[3]] =="") NA else x[[3]])

      if(length(categoryExplanation)==0) categoryExplanation <- NA
      if(length(categoryAbbreviation)==0) categoryAbbreviation <- NA
      if(length(categoryNames)==0) categoryNames <- NA

      dfguide <- data.frame(organismgroup         = zz,
                            parameters_raw        = px,
                            parameters_cleaned    = parameterclean,
                            category_name         = categoryNames,
                            category_abbrevation  = categoryAbbreviation,
                            category_explanation  = categoryExplanation,
                            classificationSystem  = cx,
                            DataType              = dx
                            )


    }, SIMPLIFY = FALSE)

    groupfinal <- do.call(rbind, mm)

  }, simplify = FALSE)

  dfinal <- Reduce(rbind, groupfiles)

  return(dfinal)
}


#' @title Arranging and user friendly selection of traits at different taxonomic levels.
#'
#' @inheritParams fw_split
#' @param spcol \code{string}. If the data is a dataframe, the species column is required and provided in this parameter.
#'        The column should have complete species name and not genus and species provided separately.
#' @param groupcol \code{string} If the data is a dataframe, and more than one taxonomic group exists in the data, the
#'      the \code{groupcol} is required to iterate over the taxonomic groups separately.
#' @param wide \code{logical}. If \code{TRUE}, then the output is spread to a wider or spread format for each unique species and
#'      taxonomic groups.
#' @param na.rm \code{logical} If \code{TRUE}, then the traits with no data will be removed from the output dataset.
#'        Default \code{TRUE}.
#'
#'
#' @importFrom stats reshape
#'
#' @return \code{dataframe} A dataframe species traits at a selected taxonomic level.
#'
#' @export
#'
#' @author {
#' Anthony Basooma \email{anthony.basooma@boku.ac.at}
#' }
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' #download fish catchment region data
#'
#'
#' fishdata <- fw_fetchdata(data = 'Abramis brmaam', organismgroup = 'fi',
#'                              ecoparams = 'migration',
#'                              cachefolder = 'cache')#the species spelling is checked
#' }
#'
#' @seealso \code{\link{fw_token}}, \code{\link{fw_searchdata}}, \code{\link{fw_split}}

fw_fetchdata <- function(data, organismgroup,
                         spcol = NULL,
                         groupcol = NULL,
                         ecoparams = NULL,
                         apikey = NULL,
                         seed = 1135,
                         secure = TRUE,
                         cachefolder = NULL,
                         inform = FALSE,
                         wide = FALSE,
                         na.rm = TRUE,
                         warn = FALSE,
                         errorness = 27,
                         pct = 80
){

  if(is(data, "data.frame")){

    if(is.null(spcol)){
      stop("If ", deparse(substitute(data)), " is a dataframe, column with species names must be provided.")

    }else{
      #one organism group in the data

      if(is.null(groupcol) && length(organismgroup)==1){

        if(spcol%in%colnames(data)==FALSE) stop(deparse(substitute(spcol)), " not in the dataset ", deparse(substitute(data))," provided.")

        #create a species list
        specieslist <- unlist(data[, spcol])

      }else{
        #more than one taxa group in the dataset
        if(groupcol%in%colnames(data)==FALSE) stop(deparse(substitute(groupcol)), " not in the dataset ", deparse(substitute(data))," provided.")

        taxagroup_split <- split(data, f=data[, groupcol])

        specieslist <- sapply(taxagroup_split, function(x) x[,spcol], simplify = FALSE)
      }

    }

  }else if(is(data, 'vector') || is('data', 'atomic')){

    specieslist <- data

  }else if(is(data, 'list')){

    specieslist <- unlist(data)
  }else{
    stop("Data format for species not allowed.")
  }

  #get species as they exist in the taxon names of the Freshwaterecology.info

  extracteddata <- fw_extract(data = specieslist,
                              ecoparams = ecoparams,
                              organismgroup = organismgroup,
                              apikey  = apikey,
                              seed = seed,
                              secure = secure,
                              warn = warn,
                              errorness = errorness,
                              pct = pct,
                              inform = inform,
                              cachefolder = cachefolder)

  if(nrow(extracteddata)<1) stop("No data found for the species entered. Confirm right group of taxa has been selected.")

  traitwords <- unique(unlist(extracteddata$parameter))

  traitlist <- sapply(traitwords, function(twords){ #t-words are the trait words looped

    ftraits <- extracteddata[extracteddata[, "parameter"] %in% twords, ]

    #rearrange the traits
    dflist0 <- split(ftraits,seq(nrow(ftraits))) #equal to species names

    #if some traits do exist for a particular species, remove from the list

    dflists <- dflist0[sapply(dflist0, function(x) nrow(x))>=1]

    if(length(dflists)>=1) dflists else  stop("The species does not enough traits for extraction", call. = FALSE)

    specieslist <- sapply(dflists, function(x){

      taxagroup = x$taxagroup

      species <- x$species

      parameter <- x$parameter

      descvalue  <-  strsplit(x$description,split="_ ",fixed=TRUE)

      #if a species has more than on trait value for the same trait, it will be
      #split bse the standard database trait values are split
      if(grepl('_', x$value)==TRUE) traitvalue <-  strsplit(x$value,split="_ ",fixed=TRUE) else  traitvalue <-  strsplit(x$value,split=", ",fixed=TRUE)


      traits <- mapply(FUN = function(taxagroup, sp, par, desc, value) data.frame(organismgroup = taxagroup,
                                                                                  species = species, parameter = parameter,
                                                                                  description = desc, value = value),
                       taxagroup = taxagroup, sp = species, par = parameter,  desc = descvalue , value = traitvalue, SIMPLIFY = FALSE)

      spfinal <- Reduce(rbind, traits)

    }, simplify = F)

    #create a dataframe for all species considered

    spfinal <- do.call(rbind, specieslist)

  }, simplify = F, USE.NAMES = FALSE)

  traitdf_long <- do.call(rbind, traitlist)

  # initialize the row names
  rownames(traitdf_long) <- NULL


  #remove traits with no data

  if(isTRUE(na.rm)) traitdf <- traitdf_long[!traitdf_long$value=="nodata",] else traitdf <-traitdf_long

  #add the traits and description to make it wide

  traitdf$tf <- traitdf$parameter== traitdf$description

  traitdf$value2 <- ifelse(traitdf$tf==TRUE,  traitdf$value, traitdf$description)

  #handle for catchment region

  traitdf$link = paste0(traitdf$parameter, traitdf$value2)

  traitdf <- traitdf[, !names(traitdf) %in% c("tf", "value2")]

  # #get the standard database names
  dbstandard <- fw_dbguide()

  dbstandard$link <- paste0(dbstandard$parameters_cleaned, dbstandard$parameterabbrevation)

  dbstandard <- dbstandard[, c("link", "traitvalue")]

  if(isTRUE(na.rm) || as.logical(na.rm)) all_x <- FALSE else all_x <- TRUE

  sanitized <- merge(traitdf, dbstandard, by='link', all.x = all_x)

  if("catchment region" %in%ecoparams==TRUE) sanitized$traitvalue <- ifelse(sanitized$parameter=='catchment region'&sanitized$value=='n', 'native',
                                                                                ifelse(sanitized$parameter=='catchment region'&sanitized$value=='a', 'alien',
                                                                                       sanitized$traitvalue))

  sanitized <- sanitized[, !names(sanitized) %in% c('link')]

  if(isTRUE(wide)){

    sanitized$tf <- sanitized$parameter==sanitized$description

    sanitized['traitdesc'] <- ifelse(sanitized$tf == FALSE,
                                     paste(sanitized$parameter,'_',sanitized$description ),
                                     paste(sanitized$parameter,'_',sanitized$value ))

    dfsel <- sanitized[, c("organismgroup", 'species', 'traitvalue', "traitdesc")]

    spfinal <- reshape(dfsel, timevar = 'traitdesc', idvar = c('organismgroup', 'species') ,
                       direction = 'wide', sep = "_")

    #remove the traitvalue created by reshape function

    colnames(spfinal) <- sapply(colnames(spfinal), function(x) sub('traitvalue_', '', x ))

  }else{
    spfinal <- sanitized[, !names(sanitized) %in% c('value')]

    spfinal$description <- ifelse(sanitized$parameter=='catchment region', sanitized$description, NA )

    spfinal <- Filter(function(x)!all(is.na(x)), spfinal)
  }
  attr(spfinal, 'fetch') <- "dataout"

  attr(spfinal, 'format') <- wide

  attr(spfinal, 'speciescol') <- spcol

  return( spfinal)
}



#' @title Arranging and user friendly selection of traits at different taxonomic levels.
#'
#' @inheritParams fw_split
#' @param spcol \code{string}. If the data is a dataframe, the species column is required and provided in this parameter.
#'        The column should have complete species name and not genus and species provided separately.
#' @param groupcol \code{string} If the data is a dataframe, and more than one taxonomic group exists in the data, the
#'      the \code{groupcol} is required to iterate over the taxonomic groups separately.
#' @param wide \code{logical}. If \code{TRUE}, then the output is spread to a wider or spread format for each unique species and
#'      taxonomic groups.
#' @param lat,lon \code{string}. These are columns that have both the longitude and latitude
#'        values in the species dataset.
#' @param na.rm \code{logical} If \code{TRUE}, then the traits with no data will be removed from the output dataset.
#'        Default \code{TRUE}.
#' @param merge \code{logical}. If the data is a dataframe and not list or vector, merge allows to
#'        affix the species ecological parameters on the user dataframe. Default \code{FALSE} to return
#'        only the species data but not the whole user input dataset.
#' @param sanitize \code{logical} Either \code{TRUE} to return complete trait names but not abbreviations which is normally returned
#'      from the database.
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
#' library(httr2)
#'
#' sacrambled_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"
#' #scrambled_api_key
#'
#' #download fish catchment region data
#'
#' apikeydecrypted <- fw_loadapikey(test = TRUE, sacrambled_apikey = sacrambled_api,
#'                               fwtraitskey =  'FWTRAITS_KEY')
#'
#' tokendata <- fw_token(key= apikeydecrypted, seed = 1234)
#'
#' fishdata <- fw_fetchdata(data = 'Abramis brmaam', organismgroup = 'fi',
#'                              ecoparams = 'catchment region',
#'                              token = tokendata)#the species spelling is checked
#' }
#'
#' @seealso \code{\link{fw_token}}, \code{\link{fw_searchdata}}, \code{\link{fw_split}}

fw_fetchdata <- function(data, organismgroup, token,
                         spcol = NULL,
                         groupcol = NULL,
                         ecoparams = NULL,
                         lat = NULL,
                         lon = NULL,
                         cachepath = NULL,
                         inform = FALSE,
                         wide = FALSE,
                         na.rm = TRUE,
                         merge= FALSE,
                         warn = FALSE,
                         sanitize = FALSE,
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
  #generates the traits[[1]] and species details[[2]]
  extracteddata <- fw_extract(data = specieslist,
                              ecoparams = ecoparams,
                              organismgroup = organismgroup,
                              token  = token,
                              warn = warn,
                              errorness = errorness,
                              pct = pct,
                              inform = inform,
                              cachepath = cachepath)

  if(nrow(extracteddata[[1]])<1) stop("No data found for the species entered. Confirm right group of taxa has been selected.")

  traitwords <- unique(unlist(extracteddata[[1]]$parameter))

  traitlist <- sapply(traitwords, function(twords){ #t-words are the trait words looped

    ftraits <- extracteddata[[1]][extracteddata[[1]][, "parameter"] %in% twords, ]

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

      traitvalue <-  strsplit(x$value,split="_ ",fixed=TRUE)

      traits <- mapply(FUN = function(taxagroup, sp, par, desc, value) data.frame(organismgroup = taxagroup,
                                                                                  species = species, parameter = parameter,
                                                                                  abbreviation = desc, value = value),
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

  if(isTRUE(wide)){

    traitdf['traitdesc'] <- paste(traitdf$parameter,'_',traitdf$abbreviation )

    dfsel <- traitdf[, c("organismgroup", 'species', 'value', "traitdesc")]

    spfinal <- reshape(dfsel, timevar = 'traitdesc', idvar = c('organismgroup', 'species') ,
                       direction = 'wide', sep = "_")

    #remove the value_ created by reshape function

    colnames(spfinal) <- sapply(colnames(spfinal), function(x) gsub("value_", "", x))

  }else{
    spfinal <- traitdf
  }

  if(isTRUE(merge) && is(data, 'data.frame')){

    #standardize the column which one in the data
    #Two columns with species checked and unchecked names
    speciescheckenames <- extracteddata[[2]] #error in sp names addressed

    #change the first column unchecked column name to match the data
    colnames(speciescheckenames)[1] <- spcol

    dfmerge <- data |> merge(speciescheckenames, by.x = spcol)

    colnames(dfmerge)[1] <- 'speciesunchecked'

    colnames(dfmerge)[which(names(dfmerge) == "clean")] <- spcol

    colnames(spfinal)[which(names(spfinal) == "species")] <- spcol

    dfall <- dfmerge |> merge(spfinal, by.x = spcol)

    dfinal <- as.data.frame(dfall[!duplicated(dfall[c(lat,lon, spcol)]),])
  }else{

    dfinal <- spfinal
  }
  if(isTRUE(sanitize) || as.logical(sanitize)){

    dfinal$tf <- dfinal$parameter== dfinal$abbreviation

    dfinal$value2 <- ifelse(dfinal$tf==TRUE,  dfinal$value,  dfinal$abbreviation)

    dfinal$link = paste0(dfinal$parameter, dfinal$value2)

    dfinal <- dfinal[, !names(dfinal) %in% c("tf", "value2")]

    # #get the standard database names
    dbstandard <- fw_dbguide()

    dbstandard$link <- paste0(dbstandard$ecoparameters_cleaned, dbstandard$parameterabbrevation)

    dbstandard <- dbstandard[, c("link", "parametervalue")]

    if(isTRUE(na.rm) || as.logical(na.rm)) all_x <- FALSE else all_x <- TRUE

    sanitized <- merge(dfinal, dbstandard, by='link', all.x = all_x)

    sanitized <- sanitized[, !names(sanitized) %in% c('link')]
  }else{
    sanitized <- dfinal
  }
  attr(sanitized, 'fetch') <- "dataout"
  attr(sanitized, 'format') <- wide
  attr(sanitized, 'sanitize') <- sanitize
  attr(sanitized, 'merged') <- merge

  return( sanitized)
}



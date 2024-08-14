#' Title
#'
#' @param data
#' @param trait
#' @param grouptaxa
#'
#' @importFrom memoise memoise
#' @importFrom stats reshape
#'
#' @return
#' @export
#'
#' @example
#'
fip_traits <- function(data, spcol = NULL, group = 'fi', traits = 'all', taxonomy= NULL,
                       taxaorder = NULL, token  = NULL, multiple = FALSE, parallel = FALSE, cores = 2, quietly = FALSE,
                       level = NULL, wide = FALSE,
                       select = NULL,
                       na.rm = FALSE){

  if(is(data, "data.frame") && is.null(spcol)) stop("If ", deparse(substitute(data)), " is a dataframe, column with species names must be provided.")

  if(is(data, "data.frame") && !is.null(spcol)){

    if(spcol%in%colnames(data)==FALSE) stop(deparse(substitute(spcol)), " not in the dataset provided.")

    #create a species list

    specieslist <- unlist(data[, spcol])

  }else if(is(data, 'vector') || is('data', 'atomic')){

    specieslist <- data

  }else if(is(data, 'list')){

    specieslist <- unlist(data)
  }else{
    stop("Data format for species not allowed.")
  }

  #get species as they exist in the taxon names of the FIP

  extracteddata <- extract_traits(species = specieslist, group = group, taxaorder = taxaorder, token  = token,
                                  multiple = multiple, parallel = parallel, cores = cores, quietly = quietly)

  if(nrow(extracteddata)<1) stop("No data found for the species entered. Confirm right group of taxa has been selected.")

  if(traits=="all"){
    #get traits
    traitwords <- unique(unlist(extracteddata$parameter))

  }else{
    traitwords <- compare_traits(traits= traits)
  }

  traitlist <- sapply(traitwords, function(twords){ #twords are the trait words looped

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

      traitvalue <-  strsplit(x$value,split="_ ",fixed=TRUE)

      traits <- mapply(FUN = function(taxagroup, sp, par, desc, value) data.frame(taxagroup = taxagroup, species = species, trait = parameter,
                                                                                  description = desc, value = value),
                       taxagroup = taxagroup, sp = species, par = parameter,  desc = descvalue , value = traitvalue, SIMPLIFY = FALSE)

      spfinal <- Reduce(rbind, traits)

    }, simplify = F)

    #create a dataframe for all species considered

    spfinal <- do.call(rbind, specieslist)

  }, simplify = F, USE.NAMES = FALSE)

  traitdfinal_long <- do.call(rbind, traitlist)

  # initialize the row names
  rownames(traitdfinal_long) <- NULL

  if(!is.null(select)) traitdfsel <- traitdfinal_long[traitdfinal_long[, "value"] %in% select, ] else traitdfsel <- traitdfinal_long

  if(nrow(traitdfsel)<1) stop("No data found for the selected items.")

  #remove traits with no data

  if(isTRUE(na.rm)) traitdf <- traitdfsel[!traitdfsel$value=="nodata",] else traitdf <-traitdfsel

  #add the traits and description to make it wide

  if(isTRUE(wide)){

    traitdf['traitdesc'] <- paste(traitdf$trait,'_',traitdf$description )

    dfsel <- traitdf[, c("taxagroup", 'species', 'value', "traitdesc")]

    spfinal <- reshape(dfsel, timevar = 'traitdesc', idvar = c('taxagroup', 'species') , direction = 'wide', sep = "_")

    #remove the value_created by reshape function

    colnames(spfinal) <- sapply(colnames(spfinal), function(x) gsub("value_", "", x))

  }else{
    spfinal <- traitdf
  }

  return(spfinal)
}

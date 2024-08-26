#' @title Arranging and user friendly selection of traits at different taxanomic levels.
#'
#' @inheritParams collatedata
#' @inheritParams extract_traits
#' @param spcol \code{string}. If the data is a dataframe, the species column is required and provided in this parameter.
#'        The column should have complete species name and not genus and species provided separately.
#' @param taxacol \code{string} If the data is a dataframe, and more than one taxonomic group exists in the data, the
#'      the \code{taxacol} is required to iterate over the taxonomic groups separately.
#' @param traits \code{string or vector}. If \code{all} is indicated, then all the traits will be extracted. Otherwise,
#'      individual traits can be indicated in a vector format. Check the allowed traits in \code{\link{fw_ecoparamdb}} function and identify
#'      all the traits allowed for each group and their explanation.
#' @param level \code{string}. The taxonomic orders allowed for each species including \code{species, genus, order or family}.
#' @param taxaorder \code{vector}. If \code{taxa} is \code{mi}, the \code{taxaorder} must be indicated for data to be downloaded.
#'      The different macroinvertebrates orders allowed can be obtained using \code{fw_orders()} function.
#' @param token \code{string}. This is a required parameter to allow user authentication with the platform. To get the token, use
#'      \code{before_u_start()} function to get the required steps. Remember that the token is saved in memory such that
#'      the data downloaded is not re-downloaded in the next session.
#' @param wide \code{logical}. If \code{TRUE}, then the output is spread to a wider or spread format for each unique species and
#'      taxonomic groups.
#' @param selectvalue \code{vector}. To allow user selection within the traits, for example, for fishes if catchment region is considered
#'      then the species native in the Danube region can be selected and retained.
#' @param descvalue \code{vector}. To allow traits selection within the traits, for example, for fishes and catchment regions is
#'        is considered, 'Danu' can be selected.
#' @param na.rm \code{logical} If \code{TRUE}, then the traits with no data will be removed from the output dataset.
#'        Default \code{TRUE}.
#' @param merge \code{logical}. If the data is a dataframe and not list or vector, merge allows to
#'        affix the species ecological parameters on the user dataframe. Default \code{FALSE} to return
#'        only the species data but not the whole user input dataset.
#'
#'
#' @importFrom memoise memoise
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
#' @example
#' \dontrun{
#'
#' #encypted token for my api key
#' enc_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"
#'
#' #the FWTRAITS_KEY is the unlock key saved in my local environment
#' #check https://httr2.r-lib.org/articles/wrapping-apis.html for more information
#'
#' apikey <- httr2::secret_decrypt(encrypted = enc_api, key = 'FWTRAITS_KEY')
#'
#' #download fish catchment region data
#' #setting the FWTRAITS_KEY
#'
#' #run this usethis::edit_r_environ()
#'
#' apikeydecrypted <- loadapikey(test = TRUE, encrytedkey = enc_api,
#'                               fwtraitskey =  'FWTRAITS_KEY')
#'
#' tokendata <- fw_token(key= apikeydecrypted, seed = 1234)
#'
#' fishdata <- fw_ecoparameters(data = 'Abramis brmaam', taxa = 'fi',
#'                              ecotraits = 'catchment region',
#'                              token = tokendata,
#'                              errorness = 27,
#'                              pct = 70)#the species spelling is checked
#' }
#'
fw_ecoparameters <- function(data, taxa, token, spcol = NULL, taxacol = NULL,  ecotraits = NULL,
                       traits = 'all', level= NULL,
                       taxaorder = NULL, multiple = FALSE,
                       parallel = FALSE, cores = 2, quietly = FALSE,
                       wide = FALSE,
                       selectvalue = NULL,
                       descvalue = NULL,
                       na.rm = TRUE,
                       merge= FALSE,
                       warn = FALSE,
                       errorness = 20,
                       pct = 80){

  if(is(data, "data.frame")){

    if(is.null(spcol)){
      stop("If ", deparse(substitute(data)), " is a dataframe, column with species names must be provided.")

    }else{
      #one taxa group in the data

      if(is.null(taxacol) && length(taxa)==1){

        if(spcol%in%colnames(data)==FALSE) stop(deparse(substitute(spcol)), " not in the dataset ", deparse(substitute(data))," provided.")

        #create a species list
        specieslist <- unlist(data[, spcol])

      }else{
        #more than one taxa group in the dataset
        if(taxacol%in%colnames(data)==FALSE) stop(deparse(substitute(taxacol)), " not in the dataset ", deparse(substitute(data))," provided.")

        taxagroup_split <- split(data, f=data[, taxacol])

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
  extracteddata <- extract_traits(data = specieslist, ecotraits = ecotraits, taxa = taxa,
                                  taxaorder = taxaorder, token  = token,
                                  multiple = multiple, parallel = parallel,
                                  cores = cores, quietly = quietly, warn = warn,
                                  errorness = errorness,
                                  pct = pct)

  if(nrow(extracteddata[[1]])<1) stop("No data found for the species entered. Confirm right group of taxa has been selected.")

  if(traits=="all"){
    #get traits
    traitwords <- unique(unlist(extracteddata[[1]]$parameter))

  }else{
    traitwords <- compare_traits(traits= traits)
  }

  traitlist <- sapply(traitwords, function(twords){ #twords are the trait words looped

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

      traits <- mapply(FUN = function(taxagroup, sp, par, desc, value) data.frame(taxagroup = taxagroup, species = species, trait = parameter,
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

  if(!is.null(selectvalue) & !is.null(descvalue)) {

    traitdfsel <- traitdf_long[traitdf_long[, "value"] %in% selectvalue &  traitdf_long[, "description"]%in% descvalue, ]

  }else if(any(!is.null(selectvalue) | !is.null(descvalue))){

    traitdfsel <- traitdf_long[traitdf_long[, "value"] %in% selectvalue |  traitdf_long[, "description"]%in% descvalue, ]
  }else{
    traitdfsel <- traitdf_long
  }

  if(nrow(traitdfsel)<1)stop('Wrong selection has been made.check the traitguide dataset from the description and values allowed.', call. = FALSE)

  #remove traits with no data

  if(isTRUE(na.rm)) traitdf <- traitdfsel[!traitdfsel$value=="nodata",] else traitdf <-traitdfsel

  #add the traits and description to make it wide

  if(isTRUE(wide)){

    traitdf['traitdesc'] <- paste(traitdf$trait,'_',traitdf$description )

    dfsel <- traitdf[, c("taxagroup", 'species', 'value', "traitdesc")]

    spfinal <- reshape(dfsel, timevar = 'traitdesc', idvar = c('taxagroup', 'species') ,
                       direction = 'wide', sep = "_")

    #remove the value_created by reshape function

    colnames(spfinal) <- sapply(colnames(spfinal), function(x) gsub("value_", "", x))

  }else{
    spfinal <- traitdf
  }
  if(isTRUE(merge)){
    #standardize the column which one in the data
    spd <- extracteddata[[2]]

    colnames(spd)[1] <- spcol

    dfmerge <- data |> merge(spd, by.x = spcol)

    colnames(dfmerge)[1] <- 'speciesunchecked'

    colnames(dfmerge)[which(names(dfmerge) == "clean")] <- spcol

   dfinal <- dfmerge |> merge(spfinal, by.x = spcol)

  }else{

    dfinal <- spfinal
  }
  return(dfinal)
}



#' @title Extract basin ranges from Freshwater Information Platform.
#'
#
#' @param data Data frame to extract basin species records which are either native, endemic or both.
#' @param colsp A variable in the data with species names
#' @param basin Basin names as indicated in the Freshwater Information Platform (Schmidt-Kloiber et al., 2019)
#' @param range For each basin, either indicate n to return only native, a: alien (Schmidt-Kloiber et al., 2019)
#' @param pct The percentage similarity to be considered to assign a relatively similar name from FishBase if the exact match is absent.
#'     The higher the pct values, the higher the different similar are considered to replace a species name that is cheeked from FisBase.
#' @param verbose To indicate messages and the default is FALSE
#' @param sn Either \strong{FALSE} to return accepted names from FishBase rather than synonyms.Default FALSE.
#' @param verbose TRUE to return processing messages or FALSE for no messages. Default FALSE
#'
#' @importFrom stats complete.cases aggregate
#'
#' @return Species data frame with temperature ranges from FishBase
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(jdsdata)
#'
#'
#' nativedanube <- ecoranges(x = jdsdata, colsp = 'speciesname',
#' range = 'n', verbose = T, basin = 'Danu')
#'
#' #Species introduced in Danube
#'
#' aliendf <- ecoranges(x = jdsdata, colsp = 'species', basin = 'Danu', range = 'a')
#'
#' # Species check from FIP and FB
#'
#' aliennativedf <- ecoranges(x = 'Salmo trutta', basin = 'Danu', range = c('n','a')
#'
#' }
#'
#'
#' @references Schmidt-Kloiber, A., Bremerich, V., De Wever, A. et al. The Freshwater Information Platform:
#' a global online network providing data, tools and resources for science and policy support.
#' Hydrobiologia 838, 1-11 (2019).
#'
#'

fip_ranges <- function(data, colsp=NULL, basin, range, pct=90, verbose=F, sn=FALSE){

  if(missing(data)) stop('Data is missing with no default')

  if(!inherits(data, 'data.frame')) stop('Only dataframes accpeted.')

  fis <- specleanr::ndata

  if(all(basin%in%c(colnames(fis)[4:40]))==FALSE)stop('Basin should be within the stated basins in FIP. Check https://www.freshwaterecology.info/fwe_search.php?og=fish for more information')

  if(all(range%in%c('n','a'))==FALSE) stop('Range should be either a, n, or both. Check https://www.freshwaterecology.info/fwe_search.php?og=fish for more information')

  if(inherits(data, 'data.frame') && is.null(colsp)) stop('Provide species column name to continue. The species names should be checked in check_names function to proceed')

  x <- as.data.frame(data)

  x2 <- x[!is.na(x[,colsp]),]

  if('speciescheck'%in%colnames(x)){
    cdata <- x2
  }else{
    cdata <- check_names(data=x2, colsp = colsp, pct= 95, verbose = F, sn=sn, merge = T)
  }
  #If basin is provided, only species that native or alien to the database will be generated

  if(!is.null(basin) && !is.null(range)){

    nspecies <- fis$Taxon[which(fis[, basin]%in%range)]

    basinspecies <- check_names(data=as.list(nspecies), pct= 95, verbose = F, sn=sn)

    #filter out only native species
    dfinal <- cdata[which(unlist(cdata$speciescheck) %in% unlist(basinspecies$speciescheck)),]
  }

  cat('Please cite this dataset as:
      Schmidt-Kloiber, A., Bremerich, V., De Wever, A. et al. The Freshwater Information Platform:
      a global online network providing data, tools and resources for science and policy support.
      Hydrobiologia 838, 1-11', '\n')

  return(dfinal)
}

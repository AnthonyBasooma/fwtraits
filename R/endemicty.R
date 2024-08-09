#' @title Determines the species ranges, whether native or alien based on Freshwater Information Platform.
#'
#' @param basin Basin under consideration.
#' @param range Whether introduced (a) or native (n).
#'
#' @return dataframe showing species ranges
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' df <- endemicity(basin ='Danu', range =c('a','n'))
#' }
#'
endemicity <- function(basin, range){

  #check species names
  #ndata is standard database from the freshwater information platform with fish species traits
  x <- specleanr::ndata

  dfcheck <- check_names(data = x, colsp = 'Taxon', verbose = F, pct = 95, merge = T)

  df_filter <- dfcheck[dfcheck[,basin]%in%range,]

  dfselect <- df_filter[,c('speciescheck', basin)]

  colnames(dfselect) <- c('speciesname','range')

  cat('Please cite this dataset as:
      Schmidt-Kloiber, A., Bremerich, V., De Wever, A. et al. The Freshwater Information Platform:
      a global online network providing data, tools and resources for science and policy support.
      Hydrobiologia 838, 1-11 (2019)', '\n')


  return(dfselect)
}

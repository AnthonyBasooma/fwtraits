
#' Auto generation of species sites
#'
#' @param x \code{dataframe} Geo spatial data with geometry column from sf package.
#' @param dist \code{integer} The distance used to cluster points as sites. The default is 1000.
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' \dontrun{
#' geospdata <- speciesdata |>
#' sf::st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'),
#'             crs = sf::st_crs(4326))
#'  xx <- fw_geosites(geospdata)
#' }
#'
fw_geosites <- function(x, dist = 1000){

  sdist <- sf::st_is_within_distance(x, x, dist = dist)

  xgps <- sapply(1:length(sdist), function(i) min(unlist(sdist[[i]])))

  dout <- x |>dplyr::mutate(sites = paste0(99, xgps))

  return(dout)
}

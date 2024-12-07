#' @title ggplot2 visualization.
#'
#' @param data The species data either in sf format or with coordinate columns to enable plotting.
#' @param output fetchdata output only accepted.
#' @param params species ecological parameters selected if there are more than one.
#' @param lat,lon \code{string}. These are columns that have both the longitude and latitude
#'        values in the species dataset.
#' @param basin \code{string}. Geographical map to be used as a bounding box for drawing the
#'        visualization maps. The species coordinates must within the indicated bounding box
#'        or basin boundary.
#' @param group Particular organism group to filter out to allow visualization.
#' @param reduce \code{logical}. If TRUE, the coordinates are rounded off which reduces the distinct points
#'        to ease ploting. Default FALSE.
#' @param bgcolor \code{string}. The color which will be used to fill the maps.
#' @param bgtheme \code{string}. The theme to be used for the map based on the ggplot2 package
#'        themes including \code{theme_bw}, \code{theme_classic}.
#' @param epsg Coordinate reference number. Default is 4326.
#'
#'
#' @return ggplot2 display
#'
#' @export
#'
#'
fw_maps <- function(data, output, lat = NULL, lon = NULL, basin,
                    params = NULL, reduce = FALSE,
                    group = NULL, bgtheme = 'bw',
                    bgcolor = 'grey50', epsg = 4326){

  #check if the species input is a dataframe

  if(!is(data, 'data.frame')) stop('Only dataframes accepted not vector of species names.')

  #check if the latitude/longitude or geometry is in data
  if(!inherits(data, "sf") && is.null(lat)) stop('If ', deparse(substitute(data)), ' is not a sf data, then provide the latitude and longitude.')

  if(!inherits(data, "sf")) if((lat %in%colnames(data) && lon%in%colnames(data))==FALSE) stop('Either the ', lat, ' or ', lon, 'is not in ', deparse(substitute(data)), '.')

  #check if the output used is from fetch data

  if(is.null(attributes(output)$fetch)) stop("Only fetch data output accepted in the output parameter.")

  #check if its not in a wide format
  if(attributes(output)$format==TRUE) stop("Spread or wide format fetch data not acceptable for visualistion.")

  if(!is.null(params)) getdf <- output[output[,"parameter"] %in% params,] else getdf <- output

  if(length(unique(getdf$organismgroup))>1){

    if(is.null(group)) stop("For multiple organism group, please filter only one to visualize the output. Chose from ", paste(unique(getdf$organismgroup), collapse = ', '))

    getdfinal <- getdf[getdf[,"organismgroup"] %in% group,]

  }else{
    getdfinal <- getdf
  }

  #remove parameter that have only one count
  counts <- table(getdfinal$parameter)

  nameOut <- names(counts[which(counts>=2)])

  if(length(nameOut)<1){
    stop("No species ecological parameters to viusualise after removing singular ones")
  }  else if(length(nameOut)==1){
    paramname <- nameOut
    nameOut
  } else{
    nameOut
  }
  datafinal <- getdfinal[getdfinal$parameter%in%nameOut,]

  pkgs <- c('ggplot2', 'sf')

  sapply(pkgs, function(xx) if(!requireNamespace(xx, quietly = TRUE)) stop('required ', xx, ' not found, please install package.'))


  #get the species names after cleaning

  #species list from input data: not clean

  colsp <- attributes(output)$speciescol

  spinput <- unlist(data[, colsp])

  #species list from fetched data

  spfetch <- unique(output$species)

  speciesnames <- clean_names(sp = spinput, prechecks = TRUE, standardnames = spfetch, full = TRUE)

  names(speciesnames)[2] <- 'species'

  #attach uncleaned names on fetched data

  fetchdata <- merge(output, speciesnames, by='species')

  #merge fetchdata with input data using unclean species names

  colnames(fetchdata)[colnames(fetchdata)=='raw'] <- colsp

  datafinal <- merge(data, fetchdata, by = colsp, all = TRUE, allow.cartesian=TRUE)

  #select particular columns

  if(inherits(data, 'sf')) colselected <- c(colsp, "parametervalue", "geometry") else colselected <- c(lat, lon, colsp, "parametervalue")

  dfseld <- datafinal[,colselected]


  if(isTRUE(reduce) && !is(data, 'sf')){
    dfseld[, lat] <- round(dfseld[, lat], 2)

    dfseld[, lon] <- round(dfseld[, lon], 2)
  }

  duprm <- dfseld[!duplicated(dfseld),]


  if(!is(data, 'sf')) spdata <- duprm |> sf::st_as_sf(coords = c(lon, lat), crs= sf::st_crs(epsg)) else spdata <- duprm |> sf::st_as_sf()

  spdatafinal <- sf::st_filter(spdata, basin)

  spdatafinal <- spdatafinal[!is.na(spdatafinal$parametervalue),]

  #determine number of columns for facets

  if(length(unique(spdatafinal$parametervalue))<= 4) ncols <- 2 else ncols <- 3

  parametervalue <- NULL


  map <- ggplot2::ggplot()+

    ggplot2::geom_sf(data = basin, fill = bgcolor)+

    ggplot2::geom_sf(data = spdatafinal, na.rm = TRUE)+

    ggplot2::facet_wrap(~parametervalue, ncol = ncols)+

    ggplot2::theme(text = ggplot2::element_text(size = 12))+

    {if(bgtheme == 'bw'){

      ggplot2::theme_bw()

    }else if(bgtheme == 'classic'){

      ggplot2::theme_classic()

    }else{
      NULL
    }}+
    ggplot2::theme(legend.position='none')+

    ggplot2::labs(color = params, x = 'Longitude', y='Latitude')

  return(map)
}




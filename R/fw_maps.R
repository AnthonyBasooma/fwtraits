#' @title ggplot2 visualization.
#'
#' @param output fetchdata output only accepted.
#' @param params species ecological parameters selected if there are more than one.
#' @param lat,lon \code{string}. These are columns that have both the longitude and latitude
#'        values in the species dataset.
#' @param basin \code{string}. Geographical map to be used as a bounding box for drawing the
#'        visualization maps. The species coordinates must within the indicated bounding box
#'        or basin boundary.
#' @param group Particular organism group to filter out to allow visualization.
#' @param viridis \code{logical} Either to use scale_fill_viridis or scale_fill_grey.
#' @param bgcolor \code{string}. The color which will be used to fill the maps.
#' @param bgtheme \code{string}. The theme to be used for the map based on the ggplot2 package
#'        themes including \code{theme_bw}, \code{theme_classic}.
#'
#' @importFrom stats aggregate
#'
#' @return ggplot2 display
#'
#' @export
#'
#'
fw_maps <- function(output, lat = NULL, lon = NULL, basin,
                    params = NULL,
                    group = NULL, viridis = TRUE, bgtheme = 'bw',
                    bgcolor = 'grey50'){

  #check if the output is fetch data

  if(is.null(attributes(output)$fetch)) stop("Only fetch data output accepted in the output parameter.")

  if(attributes(output)$format==TRUE) stop("Spread or wide format fetch data not acceptable for visualistion.")

  #one species is not necessary
  if(length(unique(output$species))==1) stop("Not necessary to display output for one species.")

  #check if output has the coordinate columns

  if(attributes(output)$merged==FALSE) stop("Set merge to TRUE in the fetchdata function to return the coordinates columns if in species data provided.")

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

  spcoords <- datafinal |> sf::st_as_sf(coords = c(lon, lat), crs= sf::st_crs(4326))

  #filter out data
  finalspdata <- sf::st_filter(spcoords, basin)

  #initialize ggplot2 parameters

  parametervalue <- NULL

  map <- ggplot2::ggplot()+

    ggplot2::geom_sf(data = basin, fill = bgcolor)+

    ggplot2::geom_sf(data = finalspdata, ggplot2::aes(color= parametervalue), na.rm = TRUE)+

    {if(isTRUE(viridis))ggplot2::scale_color_viridis_d(na.translate = FALSE) else ggplot2::scale_fill_grey(na.translate = FALSE)}+

    {if(bgtheme == 'bw'){

      ggplot2::theme_bw()

    }else if(bgtheme == 'classic'){

      ggplot2::theme_classic()

    }else{
      NULL
    }}+
    ggplot2::labs(color = params)

  return(map)
}

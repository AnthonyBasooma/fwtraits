#' @title ggplot2 visualization.
#'
#' @param output fetchdata output only accepted.
#' @param scales indicate if the scales are free, free_x, and free_y.
#' @param ncol Number of columns to display the data in facet_wrap.
#' @param params species ecological parameters selected if there are more than one.
#' @param group Particular organism group to filter out to allow visualization.
#' @param viridis \code{logical} Either to use scale_fill_viridis or scale_fill_grey
#' @param bg \code{numeric} Either 1 for theme_bw, 2 for theme_classic and 0 the default ggplot2 theme.
#'
#' @importFrom stats aggregate
#'
#' @return ggplot2 display
#'
#' @export
#'
#'
fw_visualize <- function(output, scales = 'free', ncol = 2, params = NULL,
                         group = NULL, viridis = TRUE, bg = 1){

  #check if the output is fetch data

  if(is.null(attributes(output)$fetch)) stop("Please indicate the fetch data out only.")

  if(attributes(output)$format==TRUE) stop("Spread or wide format fetch data not acceptable for visualistion.")

  #one species is not necessary
  if(length(unique(output$species))==1) stop("Not necessary to display output for one species.")

  if(!is.null(params)){

    getdf <- output[output[,"parameter"] %in% params,]
  }else{
    getdf <- output
  }

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

  if(attributes(output)$sanitize==TRUE){
    datafinal$valuedata <- datafinal$parametervalue
    angle <- 45; hjust <- 1
  }else{
    datafinal$valuedata <- datafinal$value
    angle <- 0; hjust <- 1
  }

  dfsummary <- aggregate(datafinal$valuedata,
                         by= list(organismgroup = datafinal$organismgroup,
                                  parameter = datafinal$parameter,
                                  valuedata = datafinal$valuedata),
                         FUN = length)

  parameter <- NULL

  x <- NULL

  valuedata <- NULL

  gout <- ggplot2::ggplot(dfsummary, ggplot2::aes(x = valuedata, y = x, fill = parameter))+

    ggplot2::geom_bar(stat = 'identity')+

    {if(length(unique(dfsummary$parameter))>1){

      ggplot2::facet_wrap(~ parameter, scales = scales, ncol = ncol)
    }else{
      ggplot2::ggtitle(paramname)
    }
    }+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))+

    {if(isTRUE(viridis)) ggplot2::scale_fill_viridis_d() else ggplot2::scale_fill_grey()}+

    {if(bg==1){
      ggplot2::theme_bw()
    }else if(bg==2){
      ggplot2::theme_classic()
    }else{
      NULL
    }}+
    ggplot2::theme(text = ggplot2::element_text(size = 12),

          legend.position = 'none',

          axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust),

          strip.text = ggplot2::element_text(face = 'bold'))+

    ggplot2::labs(x=" ", y ="Number of species")

  return(gout)
}


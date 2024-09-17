#' @title ggplot2 visualization.
#'
#' @param foutput fetchdata output only accepted.
#' @param scales indicate if the scales are free, free_x, and free_y.
#' @param ncol Number of columns to display the data in facet_wrap.
#' @param params species ecological parameters selected if there are more than one.
#' @param group Particular organism group to filter out to allow visualisation.
#'
#' @importFrom stats aggregate
#' @importFrom ggplot2 ggplot aes facet_wrap geom_bar scale_y_continuous element_blank element_text labs expansion theme ggtitle scale_fill_viridis_d
#'
#' @return ggplot2 display
#'
#' @export
#'
#'
fw_visualize <- function(foutput, scales = 'free', ncol = 2, params = NULL, group = NULL){

  #check if the foutput is fetch data

  if(attributes(foutput)$fetch !="dataout") stop("Please indicate the fetch data out only.")

  if(attributes(foutput)$format==TRUE) stop("Spread or wide format fetch data not acceptable for visualistion.")

  #one species is not necessary
  if(length(unique(foutput$species))==1) stop("Not necessary to display output for one species.")

  if(!is.null(params)){

    getdf <- foutput[foutput[,"parameter"] %in% params,]
  }else{
    getdf <- foutput
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

  if(attributes(foutput)$sanitize==TRUE){
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

  gout <- ggplot(dfsummary, aes(x = valuedata, y = x, fill = parameter))+

    geom_bar(stat = 'identity')+

    {if(length(unique(dfsummary$parameter))>1){

      facet_wrap(~ parameter, scales = scales, ncol = ncol)
    }else{
      ggtitle(paramname)
    }
    }+
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+

    scale_fill_viridis_d()+

    theme(text = element_text(size = 12),

          legend.position = 'none',

          axis.text.x = element_text(angle = angle, hjust = hjust),

          strip.text = element_text(face = 'bold'))+

    labs(x=" ", y ="Number of species")

  return(gout)
}

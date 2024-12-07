#' @title ggplot2 visualization.
#'
#' @param output fetchdata output only accepted.
#' @param scales indicate if the scales are free, free_x, and free_y.
#' @param ncol Number of columns to display the data in facet_wrap.
#' @param params species ecological parameters selected if there are more than one.
#' @param group Particular organism group to filter out to allow visualization.
#' @param viridis \code{logical} Either to use scale_fill_viridis or scale_fill_grey
#'

#' @return ggplot2 display
#'
#' @export
#'
#'
fw_visualize <- function(output, scales = 'free', ncol = 2, params = NULL,
                         group = NULL, viridis = TRUE){

  #check if the output is fetch data

  if(is.null(attributes(output)$fetch)) stop("Only fetch data output accepted in the output parameter.")

  if(attributes(output)$format==TRUE) stop("Spread or wide format fetch data not acceptable for visualistion.")

  #one species is not necessary
  if(length(unique(output$species))==1) stop("Not necessary to display output for one species.")

  if(!is.null(params)) getdf <- output[output[,"parameter"] %in% params,] else   getdf <- output

  if(length(unique(getdf$organismgroup))>1){

    if(is.null(group)) stop("For multiple organism group, please filter only one to visualize the output. Chose from ", paste(unique(getdf$organismgroup), collapse = ', '))

    getdfinal <- getdf[getdf[,"organismgroup"] %in% group,]

  }else{
    getdfinal <- getdf
  }

  if(!requireNamespace('dplyr', quietly = TRUE))stop('Install dplyr package to continue.')

  species <- NULL

  datafinal <- getdfinal |> dplyr::group_by(species, parametervalue, parameter) |>

   dplyr::summarise(cts = length(species), .groups = 'drop')

  parameter <- NULL

  parametervalue <- NULL

  x <- NULL

  gout <- ggplot2::ggplot(datafinal, ggplot2::aes(x = parametervalue, fill = parameter))+

    ggplot2::geom_bar()+

    {if(length(unique(unlist(datafinal$parameter)))>1){

      ggplot2::facet_wrap(~ parameter, scales = scales, ncol = ncol)

    }
      #else{
      #ggplot2::ggtitle(paramname)
    #}
    }+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))+

    {if(isTRUE(viridis)) ggplot2::scale_fill_viridis_d() else ggplot2::scale_fill_grey()}+

    ggplot2:: theme_bw()+

    ggplot2::theme(text = ggplot2::element_text(size = 12),

          legend.position = 'none',

          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),

          strip.text = ggplot2::element_text(face = 'bold'))+

    ggplot2::labs(x=" ", y ="Number of species")

  return(gout)
}



# #remove parameter that have only one count
# counts <- table(getdfinal$parameter)
#
# nameOut <- names(counts[which(counts>=2)])
#
# if(length(nameOut)<1){
#   stop("No species ecological parameters to viusualise after removing singular ones")
# }  else if(length(nameOut)==1){
#   paramname <- nameOut
#   nameOut
# } else{
#   nameOut
# }
#
# datafinal <- getdfinal[getdfinal$parameter%in%nameOut,]
#
#
# dfsummary <- aggregate(datafinal$valuedata,
#                        by= list(organismgroup = datafinal$organismgroup,
#                                 parameter = datafinal$parameter,
#                                 valuedata = datafinal$valuedata),
#                        FUN = length)
#
#
#
#
#
#


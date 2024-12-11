#' @title ggplot2 visualization.
#'
#' @param output fetchdata output only accepted.
#' @param scales indicate if the scales are free, free_x, and free_y.
#' @param ncol Number of columns to display the data in facet_wrap.
#' @param params species ecological parameters selected if there are more than one.
#' @param group Particular organism group to filter out to allow visualization.
#' @param color \code{string} Change the color for the bar graphs being plotted.
#'

#' @return ggplot2 display
#'
#' @export
#'
#'
fw_visualize <- function(output, scales = 'free', ncol = 2, params = NULL,
                         group = NULL, color = 'purple'){

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

  if(!requireNamespace('ggplot2', quietly = TRUE))stop('Install ggplot2 package to continue.')

  if(!requireNamespace('tidytext', quietly = TRUE))stop('Install tidytext package to continue.')

  species <- NULL

  datafinal <- getdfinal |> dplyr::group_by(traitvalue, parameter) |>

   dplyr::summarise(cts = length(species), .groups = 'drop')

  parameter <- NULL

  traitvalue <- NULL

  cts <- NULL

  gout <- ggplot2::ggplot(datafinal, ggplot2::aes(x = tidytext::reorder_within(traitvalue, -cts , parameter),
                                                  y= cts))+

    ggplot2::geom_bar(stat = 'identity', fill = color)+

    {if(length(unique(unlist(datafinal$parameter)))>1){

      ggplot2::facet_wrap(~ parameter, scales = scales, ncol = ncol)

    }
    }+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1)))+

    ggplot2:: theme_bw()+

    tidytext::scale_x_reordered()+

    ggplot2::theme(text = ggplot2::element_text(size = 12),

                   panel.grid.major = ggplot2::element_blank(),

                   panel.grid.minor = ggplot2::element_blank(),

                   legend.position = 'none',

                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),

                   strip.text = ggplot2::element_text(face = 'bold'))+

    ggplot2::labs(x= "Species traits", y ="Number of species")

  return(gout)
}

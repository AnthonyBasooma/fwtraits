#' @title ggplot2 visualization.
#'
#' @param output fetchdata output only accepted.
#' @param scales indicate if the scales are free, free_x, and free_y.
#' @param ncol Number of columns to display the data in facet_wrap.
#' @param params species ecological parameters selected if there are more than one.
#' @param organism_group Particular organism group to filter out to allow visualization.
#' @param color \code{string} Change the color for the bar graphs being plotted.
#' @param na.rm \code{logical} To remove NA in the category names or species traits.
#'

#' @return ggplot2 display
#'
#' @export
#'
#'
fw_visualize <- function(output, scales = 'free', ncol = 2, params = NULL,
                         organism_group = NULL, color = 'purple', na.rm = TRUE){

  #check if the output is fetch data

  match.arg(organism_group, choices = c("mi",'fi', 'pp', 'pb', 'di', 'mp'))

  if(isFALSE(output$type=='fetch')) stop("Only fetch data output accepted in the output parameter.")

  output <- output$ecodata

  #one species is not necessary

  if(!is.null(params)) getdf <- output[output[,"Parameter"] %in% params,] else   getdf <- output

  if(length(unique(getdf$OrganismGroup))>1){

    if(is.null(organism_group)) stop("For multiple organism_group, please filter only one to visualize the output. Chose from ", paste(unique(getdf$OrganismGroup), collapse = ', '))

    getdfinal <- getdf[getdf[,"OrganismGroup"] %in% organism_group,] |> dplyr::mutate(species = paste(Genus, ' ', Species))

    if(length(unique(getdfinal$species))==1) stop("Not necessary to display graphical output for one species returned from the selected group.", call. = FALSE)

  }else{
    getdfinal <- getdf
  }

  check_packages(c('dplyr', 'ggplot2', 'tidytext'))

  Species <- NULL
  Genus <- NULL

  datafinal <-  getdfinal |> dplyr::mutate(species = paste(Genus, ' ', Species)) |>

    dplyr::group_by(CategoryName, Parameter) |>

    dplyr::summarise(cts = length(CategoryName), .groups = 'drop')

  if(isTRUE(na.rm)) datafinal <- datafinal |> dplyr::filter(!is.na(CategoryName))

  Parameter <- NULL

  CategoryName <- NULL

  cts <- NULL

  gout <- ggplot2::ggplot(datafinal, ggplot2::aes(x = tidytext::reorder_within(CategoryName, -cts , Parameter),
                                                  y= cts))+

    ggplot2::geom_bar(stat = 'identity', fill = color)+

    {if(length(unique(unlist(datafinal$Parameter)))>1){

      ggplot2::facet_wrap(~ Parameter, scales = scales, ncol = ncol)

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

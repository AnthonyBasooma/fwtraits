#' For computing the functional dendogram
#'
#' @param fwdata \code{list} A list from the \code{fw_fetchdata} function is the only data type accepted.
#' @param method \code{string} Clustering method, including ward, average, and complete
#' @param k \code{integer} Determine the number of cluster the user wants to output after data clustering. The default is 4.
#' @param plot \code{logical} Either \code{TRUE} to show the plot of functional dendogram. Defualt is \code{FALSE}.
#'
#' @return dataframe, plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   fishtraits <- fw_fetchdata(data = speciesdata,
#'                         ecoparams = c('rheophily habitat', 'spawning habitat',
#'                         'feeding diet adult'),
#'                         taxonomic_column = 'scientificName',
#'                         organismgroup = 'fi')
#'
#'
#'  head(fdendoclust, 3)
#'
#'  table(fdendoclust$cluster)
#'
#' }

fw_fdendro <- function(fwdata, method = 'average', k = 4, plot= FALSE){

  dd <- fwdata$ecodata |>

    dplyr::select(.data$Parameter, .data$Taxonname, .data$CategoryName) |>

    tidyr::pivot_wider(names_from = .data$Parameter, values_from = .data$CategoryName,
                       values_fn =  dplyr::first)|>

    tibble::column_to_rownames("Taxonname") |>
    stats::na.omit()

  colnames(dd) <- sapply(colnames(dd), function(x) gsub(" ", "_", x))

  dd[, 1:ncol(dd)] <- lapply(dd[, 1:ncol(dd)], as.factor)

  fdendo <- cluster::daisy(dd, metric = 'gower')

  clust <- stats::hclust(fdendo, method = method)

  groups <- stats::cutree(clust, k = k)

  dd$cluster <- factor(groups)

  if(isTRUE(plot)){

    graphics::plot(clust, xlab = "Group", ylab = "Functional Disimialarity", sub = "")
  }

  return(dd)
}

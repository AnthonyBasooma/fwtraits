#' @title Efiplus data used to develop ecological sensitivity parameters for riverine species in European streams and rivers.
#'
#' @description A \code{tibble}
#'
#' @docType data
#'
#' @details BQEs sensitivity to global/climate change in European rivers:
#' implications for reference conditions and pressure-impact-recovery chains (Logez et al. 2012). An extract has been made for
#' usage in this package but for more information write to ihg@boku.ac.at
#'
#' @usage data(speciesdata)
#'
#' @keywords European wide dataset
#'
#' @format A \code{tibble} 99 rows and 23 columns.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("speciesdata")
#' speciesdata
#' }
#'
#'
#' @references Logez M, Belliard J, Melcher A, Kremser H, Pletterbauer F, Schmutz S, Gorges G, Delaigue O, Pont D. 2012.
#' Deliverable D5.1-3: BQEs sensitivity to global/climate change in European rivers: implications for reference conditions
#' and pressure-impact-recovery chains.
#'
"speciesdata"


#' @title Taxonomic data for species in the freshwaterecology.info database
#'
#' @description A \code{tibble}
#'
#' @docType data
#'
#' @details
#' Standard taxonomic backbone for macroinvertebrates from www.freshwaterecology.info
#'
#' @usage data(invertbackbone)
#'
#' @keywords  Macroinvertebrates
#'
#' @format A \code{tibble} 10421 rows and 3 columns.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("invertbackbone")
#' invertbackbone
#' }
#'
#'
#' @references Schmidt-Kloiber, A., & Hering, D. (2015). www.freshwaterecology.info - An online tool that unifies,
#' standardizes and codifies more than 20,000 European freshwater organisms and their ecological preferences.
#' Ecological Indicators, 53, 271-282. https://doi.org/10.1016/j.ecolind.2015.02.007.
#'
"invertbackbone"


#' @title Taxonomic data for the phytobentho species in www.freshwaterecology.info database.
#'
#' @description A \code{tibble}
#'
#' @docType data
#'
#' @details
#' The phytobentho data forms the taxonomic backbone for the species archived in the database.
#'        This enables taxonomic name checks and faster data retrieval from the database.
#'
#' @usage data(pbenthodata)
#'
#' @keywords Phytobentho data
#'
#' @format A \code{tibble} The dataset has 1857 rows and 2 columns. The column names constains
#'
#'  \itemize{
#'  \item Taxgroup, which is large taxonomic group naming containing multiple species.
#'  \ item Taxon, is the species taxonomic names.
#'  }
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' data("pbenthodata")
#'
#' pbenthodata
#' }
#'
#' @references Schmidt-Kloiber, A., & Hering, D. (2015). Www.freshwaterecology.info - An online tool that unifies,
#' standardises and codifies more than 20,000 European freshwater organisms and their ecological preferences.
#' Ecological Indicators, 53, 271-282. https://doi.org/10.1016/j.ecolind.2015.02.007.
#'
"pbenthodata"


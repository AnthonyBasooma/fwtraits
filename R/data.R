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
#' \itemize{
#'  \item scientificName: The fish species names extracted from the EFIPLUS dataset.
#'  \item waterBody: The water body from which the species records were collected.
#'  \item decimalLatitude: Species location
#'  \item decimalLongitude: Species occurrence records.
#'  \item MRR: The locality where the species was collated or sampled from
#'  \item Date: the day, month, and year when the species record was collected.
#'  \item: year: The year when the species record was collated.
#'  \item Locality: particular locality where the species was sampled.
#'  \item country: The country where the record was made.
#'
#' }
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
#' Standard taxonomic backbone for macroinvertebrates from www.freshwaterecology.info.
#'      All macroinvetebrates species are crosschecked with this database improve quality control
#'      of the species names provided by the user.
#'
#' @usage data(invertbackbone)
#'
#' @keywords  Macroinvertebrates
#'
#' @format A \code{tibble} 10421 rows and 3 columns.
#'
#' \itemize{
#'  \item Taxgroup: Higher taxonomic grouping for the species, for example, Bivalvia.
#'  \item Family: Taxonomic classification, e.g., CARDIIDAE
#'  \item Taxon: Lower taxonomic grouping for the species.eg., Parvicardium exiguum
#'  }
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
#' @usage data(pbenthodata)
#'
#' @keywords Phytobentho data
#'
#' @format A \code{tibble} The dataset has 1857 rows and 2 columns.
#'
#'  \itemize{
#'  \item Taxgroup: Higher taxonomic grouping for the species, for example, Bacillariophyceae.
#'  \item Taxon: Lower taxonomic grouping for the species.eg., Achnanthes acus, Achnanthes brevipes,
#'           Achnanthes brevipes var. brevipes
#'  }
#' @details This dataset was extracted from the www.freshwaterecology.info database and formed a standardized
#'        reference for the species names for phytobenthos. Therefore, all user-provided species are
#'        checked across this database to identify whether or not they exist.
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


#' @title Data with ecological parameters classification.
#'
#' @description A \code{tibble}
#'
#' @docType data
#'
#' @usage data(classifydata)
#'
#' @keywords Classification data
#'
#' @format A \code{tibble} The dataset has 817 rows and 4 columns.
#'
#'  \itemize{
#'  \item Organism group: Are the taxonomic groups in the www.freshwaterecology.info database.
#'  \item parameter_cleaned: Are the ecological parameter names in the database.
#'  \item DataType: Are the data classification for each ecological parameter. The data types are still under revision to improve harmony. Therefore,
#'      the users can provide a different classification based on their expertise.
#'  }
#' @details The database will be used internally to assign data types such as nominal, ordinal, ratio, and interval to ecological parameters.
#'
#' @examples
#'
#' \dontrun{
#'
#' data("classifydata")
#'
#' classifydata
#' }
#'
#' @references Schmidt-Kloiber, A., & Hering, D. (2015). Www.freshwaterecology.info - An online tool that unifies,
#' standardises and codifies more than 20,000 European freshwater organisms and their ecological preferences.
#' Ecological Indicators, 53, 271-282. https://doi.org/10.1016/j.ecolind.2015.02.007.
#'
"classifydata"

















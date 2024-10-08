% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fw_extract.R
\name{fw_extract}
\alias{fw_extract}
\title{Extracting the traits from the downloaded data.}
\usage{
fw_extract(
  data,
  organismgroup,
  ecoparams = NULL,
  taxagroup = NULL,
  token = NULL,
  parallel = FALSE,
  cores = NULL,
  pct = 80,
  subspecies = FALSE,
  errorness = 20,
  warn = FALSE
)
}
\arguments{
\item{data}{\code{vector}. The list or vector with species names for which ecological references needs to be extracted from the
database.}

\item{organismgroup}{\code{string}. The organismgroup group to download from the platform.
The allowed group include \code{"fi", "mi", "pp", "pb", "di","mp"} for fishes, macroinvertebrates, phytoplankton,
phytobenthos, diatoms, and macrophytes. Multiple groups allowed such as \code{'pp', 'di'}.
\itemize{
\item{\code{pp}: Pytoplankton.}
\item{\code{mp}: Macrophtytes}
\item{\code{mi}: Macroinvertebrates}
\item{\code{fi}: Fishes}
\item{\code{di}: Diatoms}
\item{\code{pb}: Phytobenthos without diatoms}
}}

\item{ecoparams}{\code{vector}. Selected traits that should be downloaded for particular organismgroup group. Check \code{\link{fw_dbguide}} for the allowed
traits in the database.}

\item{taxagroup}{\code{vector}. If \code{organismgroup} is \code{mi}, the \code{taxagroup} must be indicated for data to be downloaded.
The different macroinvertebrates orders allowed can be obtained using \code{\link{fw_taxagroup}} function.}

\item{token}{\code{string}. This is a required parameter to allow user authentication with the platform. To get the token, use
\code{\link{fw_be4ustart}} function to get the required steps. Remember that the token is saved in memory such that
the data downloaded is not re-downloaded in the next session.}

\item{parallel}{\code{logical}. If \code{TRUE} then the parallel data download is enabled.}

\item{cores}{\code{integer} An integer indicating the number of cores to be be used in parallelisng the
the data download. Default is 2.}

\item{pct}{\code{numeric}. The number used as a cutoff to infer similarity of the user provided name and what is found in the database.
The higher the percentage, the higher the similarity the species name provided by the user and the one in the database. \code{pct}
ranges from 0 to 100 but the default is 80 to ensure that wrong names are not selected at low similarity percentage cutoff.}

\item{subspecies}{To consider subspecies while carrying taxonomic names checks. For example, Salmo trutta fario
will not be truncated to Salmo trutta if \code{subspecies is TRUE}:}

\item{errorness}{\code{numeric} Similar to \code{pct}, \code{errorness} parameter uses the distance differences between the user-provided
names and all the taxa group species standard names. The lower the percentage error, the higher the similarity in the species
names provided. Default is 20 and beyond 30, a warning is showed to avoid wrong species replace the user provided name, which leads
to extracting wrong traits.}

\item{warn}{\code{logical} To show species name warning checks and traits cleaning. Default \code{FALSE}.}
}
\value{
\code{dataframe} A dataframe species traits for all orders.
}
\description{
Extracting the traits from the downloaded data.
}
\examples{

\dontrun{
#' #' #encrypted token for my api key

enc_api <- "p6-9gAvYXveyC_B_0hTzyYl5FwLRwZEPD-ZE9Y4KrIBstgNc8K2Mr4u6t39LGZ2E1m9SOw"

apikey <- httr2::secret_decrypt(encrypted = enc_api, key = 'FWTRAITS_KEY')

#download fish catchment region data
#setting the FWTRAITS_KEY

#run this usethis::edit_r_environ()

apikeydecrypted <- fw_loadapikey(test = TRUE, encrytedkey = enc_api,
                              fwtraitskey =  'FWTRAITS_KEY')

tokendata <- fw_token(key= apikeydecrypted, seed = 1234)

#extract is for specific species or multiple number of species

dfextract <- fw_extract(data = "Abramis brama", organismgroup = 'fi',
ecoparams = 'migration', token = tokendata)

}

}

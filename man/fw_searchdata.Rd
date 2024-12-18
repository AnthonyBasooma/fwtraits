% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fw_searchdata.R
\name{fw_searchdata}
\alias{fw_searchdata}
\title{To download data from the Freshwaterecology.info database.}
\usage{
fw_searchdata(
  organismgroup,
  refdata = NULL,
  ecoparams = NULL,
  apikey = NULL,
  warn = TRUE,
  seed = 1135,
  secure = TRUE,
  inform = FALSE,
  cachefolder = NULL
)
}
\arguments{
\item{organismgroup}{\code{string}. The organism group to download from the platform.
The allowed group include \code{"fi", "mi", "pp", "pb", "di","mp"} for fishes,
macroinvertebrates, phytoplankton,
phytobenthos, diatoms, and macrophytes, respectively. Multiple groups allowed such as \code{'pp', 'di'}.
\itemize{
\item{\code{pp}: Pytoplankton.}
\item{\code{mp}: Macrophtytes}
\item{\code{mi}: Macroinvertebrates}
\item{\code{fi}: Fishes}
\item{\code{di}: Diatoms}
\item{\code{pb}: Phytobenthos without diatoms}
}}

\item{refdata}{\code{string} An internal placeholder to accommodate the standard taxonomic names for
invertebrates and phytobenthos from the database.}

\item{ecoparams}{\code{vector}. Selected traits that should be downloaded for particular organismgroup group. Check \code{\link{fw_dbguide}} for the allowed
traits in the database.}

\item{apikey}{\code{string}. The API key which is automatically loaded using the loadapikey() internal function.}

\item{warn}{\code{logical} To show species name warning checks and traits cleaning. Default \code{FALSE}.}

\item{seed}{\code{integer}. An integer to help track the caching of the access token generated during data collation.
If a user wants to get a new token, then the seed should be changed.}

\item{secure}{\code{logical}. If \code{TRUE}, the user will be prompted to set the API key in the
.Renviron file by running the \code{\link{fw_setapikey}} function. The User must strictly
type in API_KEY = 'api key', save, close the file and restart the R session or RStudio
for the API_KEY environment to be captured.
If \code{FALSE}, then the key will be entered directly in the API_KEY directly in the
fw_token() function. This method is insecure, since the key can be obtained from the codes
by other users.}

\item{inform}{\code{logical}. To indicate if the token is successfully generated. Default \code{TRUE}.}

\item{cachefolder}{\code{string}. The root path were the cached data will be saved on the user PC.
If the path is not provided, the cached information will be saved in the current
working directly.}
}
\value{
List of download species traits
}
\description{
The function provides a seamless access and download of species ecological parameters, traits,
or indicators from the Freshwaterecology.info database. The function allows multiple inclusion
of organism groups, which include macroinvertebrates, fishes, phytoplankton, phytobenthos,
macrophytes, and diatoms.
}
\examples{

\dontrun{
dfsearch <- fw_searchdata(organismgroup = 'fi', ecoparams = 'migration', cachefolder = 'cache')
}
}

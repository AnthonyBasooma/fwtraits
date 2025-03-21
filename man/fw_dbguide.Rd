% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fw_dbguide.R
\name{fw_dbguide}
\alias{fw_dbguide}
\title{Standard table with taxonomic groups and their traits explanations.}
\usage{
fw_dbguide(organismgroup = NULL, cachefolder = "cache")
}
\arguments{
\item{organismgroup}{\code{string or vector}. Taxa group names to aid the users in filtering the standard
table for species traits and their explanations.}

\item{cachefolder}{\code{string}. The root path were the cached data will be saved on the user PC.
If the path is not provided, the cached information will be saved in the current
working directly.}
}
\value{
\code{dataframe} A dataset with taxonomic groups, traits and their explanations.
}
\description{
Standard table with taxonomic groups and their traits explanations.
}
\examples{

\dontrun{

dbase <- fw_dbguide(cachefolder = 'cache')

}

}

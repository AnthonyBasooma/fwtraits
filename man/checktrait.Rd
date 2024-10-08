% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fw_dbchecks.R
\name{checktrait}
\alias{checktrait}
\title{Checks the traits spelling compared to user input.}
\usage{
checktrait(x, std, mindist = 0.3, error = 0.8, grp = NULL, warn = TRUE)
}
\arguments{
\item{x}{\code{string or vector}. The traits to be checked for spelling errors and matching database entries.}

\item{std}{\code{lits}. A list with standard traits names from the the database to compare with user entries.}

\item{mindist}{\code{numeric}. Set a threshold for trait similarity between the user provided and that found in the database.
The lower the percentage, the higher the similarity between the user provided
and standard trait names.}

\item{error}{\code{numeric}. Also percentage to improve the distance based checked implemented or set in
mindist parameter}

\item{grp}{\code{grp}. The taxa names checked for. see \code{\link{fw_searchdata}}.}

\item{warn}{\code{logical} To show species name warning checks and traits cleaning. Default \code{FALSE}.}
}
\value{
\code{list or string}. A list, vector or string of cleaned traits names based on
the user provided and standard database traits for downloading.
}
\description{
Checks the traits spelling compared to user input.
}

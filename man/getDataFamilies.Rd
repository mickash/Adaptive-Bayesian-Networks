% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/LocationActions.R
\name{getDataFamilies}
\alias{getDataFamilies}
\title{List data families in database}
\usage{
getDataFamilies(cn, metadata = "metadata")
}
\arguments{
\item{cn}{An open RODBC connection}

\item{metadata}{The metadata table in the database}
}
\value{
A vector specifying names of available networks.
}
\description{
List data families in database
}


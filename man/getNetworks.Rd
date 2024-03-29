% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/LocationActions.R
\name{getNetworks}
\alias{getNetworks}
\title{List networks in database}
\usage{
getNetworks(cn, networks = "networks")
}
\arguments{
\item{cn}{An open RODBC connection}

\item{networks}{The networks table in the database}
}
\value{
A vector specifying names of available networks.
}
\description{
List networks in database
}


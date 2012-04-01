\name{WDIcache}
\alias{WDIcache}
\title{Update the list of available WDI indicators...}
\description{Update the list of available WDI indicators}
\details{Download an updated list of available WDI indicators from the World Bank website. Returns a data frame for use in the \code{WDIsearch} function.}
\value{Series of indicators, sources and descriptions in data frame format}
\author{Vincent Arel-Bundock \email{varel@umich.edu}}
\note{Downloading all series information from the World Bank website can take time.
The \code{WDI} package ships with a local data object with information on all the series
available on 2012-03-31. You can update this database by retrieving a new list using \code{WDIcache}, and  then
feeding the resulting object to \code{WDI} via the \code{cache} argument.}

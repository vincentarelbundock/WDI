#' World Development Indicators series and country information 
#'
#' A list of two character matrices. 
#'
#' The first character matrix includes a full list of WDI series (updated 2012-06-18):
#' 
#' \itemize{
#'   \item id WDI indicator code  
#'   \item name Short name of the series
#'   \item sourceNote Detailed description provided by the source
#'   \item source Source database
#'   \item sourceOrganization Source organization
#' }
#' 
#' The second character matrix includes extra country information (updated 2012-06-18):
#'
#' \itemize{
#'   \item iso3c ISO 3-letter country code  
#'   \item iso2c ISO 2-letter country code  
#'   \item country Country name (long English form)
#'   \item region Region
#'   \item capital Capital 
#'   \item longitude Longitude 
#'   \item latitutde Latitude
#'   \item income Income level as defined by the World Bank
#'   \item lending Lending category, as defined by the World Bank
#' }

#' @docType data
#' @keywords datasets
#' @name WDI_data 
#' @usage WDI_data
#' @format List of 2 data frames 
NULL


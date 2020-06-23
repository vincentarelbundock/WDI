# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('year', 'value', 'Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code'))

#' WDI: World Development Indicators (World Bank)
#' 
#' Downloads the requested data by using the World Bank's API, parses the
#' resulting XML file, and formats it in long country-year format. 
#' 
#' @param country Vector of countries (ISO-2 character codes, e.g. "BR", "US",
#'     "CA") for which the data is needed. Using the string "all" instead of
#'     individual iso codes pulls data for every available country.
#' @param indicator Character vector of indicators codes. See the WDIsearch()
#' function. If you supply a named vector, the indicators will be automatically
#' renamed: `c('women_private_sector' = 'BI.PWK.PRVS.FE.ZS')`
#' @param start Start date, usually a year in integer format. Must be 1960 or
#' greater.
#' @param end End date, usually a year in integer format. Must be greater than
#' the `start` argument.
#' @param extra TRUE returns extra variables such as region, iso3c code, and
#'     incomeLevel
#' @param cache NULL (optional) a list created by WDIcache() to be used with the
#'     extra=TRUE argument
#' @return Data frame with country-year observations. You can extract a
#' data.frame with indicator names and descriptive labels by inspecting the
#' `label` attribute of the resulting data.frame: `attr(dat, 'label')`
#' @author Vincent Arel-Bundock \email{vincent.arel-bundock@umontreal.ca}
#' @importFrom RJSONIO fromJSON
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' WDI(country="all", indicator=c("AG.AGR.TRAC.NO","TM.TAX.TCOM.BC.ZS"),
#'     start=1990, end=2000)
#' WDI(country=c("US","BR"), indicator="NY.GNS.ICTR.GN.ZS", start=1999, end=2000,
#'     extra=TRUE, cache=NULL)
#'
#' # Rename indicators on the fly
#' WDI(country = 'CA', indicator = c('women_private_sector' = 'BI.PWK.PRVS.FE.ZS',
#'                                   'women_public_sector' = 'BI.PWK.PUBS.FE.ZS'))
#' }
#'
WDI <- function(country = "all", 
                indicator = "NY.GDP.PCAP.KD",
                start = 1960, 
                end = 2020, 
                extra = FALSE, 
                cache = NULL){

    # Sanity: country
    if (!is.character(country)) {
        stop('The `country` argument must be a character vector')
    }

    country <- gsub('[^a-zA-Z0-9]', '', country)
    country_good <- unique(c(WDI::WDI_data$country[,'iso3c'], 
                             WDI::WDI_data$country[,'iso2c']))
    country_good <- c('all', country_good)
    country_bad <- base::setdiff(country, country_good)
    country <- base::intersect(country, country_good)

    if (length(country) == 0) {
        stop('None of the countries requested are valid. Please use ISO-2, ISO-3, or World Bank regional codes.')
    }
    if (length(country_bad) > 0) {
        warning('Please use ISO-2, ISO-3, or World Bank regional codes. Some of the country codes that you requested are invalid: ',  
                paste(country_bad, collapse = ', '))
    }

    # Sanity: start & end
    if(!(start <= end)){
        stop('`end` must be equal to or greater than `start`.')
    }
    is_integer <- function(x) is.numeric(x) && (x %% 1 == 0)
    if (is_integer(start) && (start < 1960)) {
        stop('`start` must be equal to or greater than 1960')
    }

    # Download
    dat <- list()
    failed <- NULL
    for (i in indicator) {
        tmp <- tryCatch(wdi.dl(i, country, start, end), error = function(e) e)
        if (is.null(tmp) || !inherits(tmp$data, 'data.frame') || (nrow(tmp$data) == 0)) {
            failed <- c(failed, i)
        } else {
            dat[[i]] <- tmp
        }
    }

    # Sanity: downloaded data
    if (length(dat) == 0) {
        message('None of the indicators your requested could be downloaded. Please verify the arguments of the `WDI()` function. You can also type a URL of this form in your browser to check if the World Bank web API is currently serving the indicator(s) of interest: ',
                wdi.query(indicator = failed[1])[1])
        return(NULL)
    } 

    # not downloaded and not in cache
    indicator_good <- WDI::WDI_data$series[, 'indicator']
    tmp <- base::setdiff(failed, indicator_good) 
    if (length(tmp) > 0) {
        message('These indicators could not be downloaded, and they do not appear in the cached list of available World Bank data series: ',
                paste(tmp, collapse = ', '))
    }

    # not downloaded
    tmp <- base::setdiff(failed, tmp)
    if (length(tmp) > 0) {
        message('These indicators could not be downloaded: ',
                paste(tmp, collapse = ', '))
    }

    # Extract labels
    lab = lapply(dat, function(x) data.frame('indicator' = x$indicator,
                                             'label' = x$label,
                                             stringsAsFactors = FALSE))
    lab = do.call('rbind', lab)

    # Extract data
    dat = lapply(dat, function(x) x$data)
    dat = Reduce(function(x,y) merge(x,y,all=TRUE), dat)
    row.names(dat) <- NULL

    # Extras
    if(!is.null(cache)){
        country_data = cache$country
    }else{
        country_data = WDI::WDI_data$country
    }
    if(extra==TRUE){
	    dat = merge(dat, country_data, all.x=TRUE)
    }
    countries = country[country != 'all' & !(country %in% dat$iso2c)]
    if(length(countries) > 0){
    }

    # Assign label attributes
    for (i in 1:nrow(lab)) {
        if (lab$indicator[i] %in% colnames(dat)) {
            attr(dat[[lab$indicator[i]]], 'label') = lab$label[[i]]
        }
    }

	# Rename columns based on indicator vector names
	if (!is.null(names(indicator))) {
		for (i in seq_along(indicator)) {
			idx = match(indicator[i], colnames(dat))
			if (!is.na(idx)) {
				colnames(dat)[idx] = names(indicator)[i]
			}
		}
	}

	# Output
    return(dat)
}

#' Download all the WDI indicators at once.
#' 
#' @return Data frame 
#' @author Vincent Arel-Bundock \email{vincent.arel-bundock@umontreal.ca}
#' @return a list of 6 data frames: Data, Country, Series, Country-Series,
#' Series-Time, FootNote
#' @export
WDIbulk = function() {
    if (!'tidyr' %in% utils::installed.packages()[, 1]) {
        stop('To use the `WDIbulk` function, you must install the `tidyr` package.')
    }

    # download
    temp = tempfile()
    url = 'http://databank.worldbank.org/data/download/WDI_csv.zip'
    utils::download.file(url, temp)

    # read
    zip_content = c("WDIData.csv", "WDICountry.csv", "WDISeries.csv",
                    "WDICountry-Series.csv", "WDISeries-Time.csv",
                    "WDIFootNote.csv")
    out = lapply(zip_content, function(x) utils::read.csv(unz(temp, x), stringsAsFactors = FALSE))

    # flush
    unlink(temp)

    # names
    names(out) = zip_content
    names(out) = gsub('.csv', '', names(out))
    names(out) = gsub('WDI', '', names(out))

    # clean "Data" entry
    out$Data$X = NULL
    out$Data = tidyr::gather(out$Data, year, value, -Country.Name,
                             -Country.Code, -Indicator.Name, -Indicator.Code)

    # clean year column
    out$Data$year = gsub('^X', '', out$Data$year)
    out$Data$year = as.integer(out$Data$year)

    # output
    return(out)
}

#' Internal function to build API call
#'
#' @export
#' @keywords internal
wdi.query = function(indicator = "NY.GDP.PCAP.CD", 
                     country = 'all', 
                     start = 1960, 
                     end = 2020) {

    country <- paste(country, collapse = ';')

    # WDI only allows 32500 per_page (this seems undocumented)
    out = paste0("http://api.worldbank.org/v2/country/", country, "/indicator/", indicator,
                 "?format=json",
                 "&date=",start,":",end,
                 "&per_page=32500",
                 "&page=",1:10)
    return(out)
}

#' Internal function to download data
#'
#' @export
#' @keywords internal
wdi.dl = function(indicator, country, start, end){
    get_page <- function(daturl) {
        # download
        dat_raw = RJSONIO::fromJSON(daturl, nullValue=NA)[[2]]
        # extract data 
        dat = lapply(dat_raw, function(j) cbind(j$country[[1]], j$country[[2]], j$value, j$date))
        dat = data.frame(do.call('rbind', dat), stringsAsFactors = FALSE)
        colnames(dat) = c('iso2c', 'country', as.character(indicator), 'year')
        dat$label <- dat_raw[[1]]$indicator['value']
        # output
        return(dat)
    }

    pages <- wdi.query(indicator, country, start, end)

    dat <- list()
    done <- FALSE # done when pages no longer return useable info
    for (i in seq_along(pages)) {
        if (!done) {
            tmp <- tryCatch(get_page(pages[i]), error = function(e) NULL)
            if (inherits(tmp, 'data.frame') && (nrow(tmp) > 0)) {
                dat[[i]] <- tmp
            } else {
                done <- TRUE
            }
        }
    }
    dat <- do.call('rbind', dat)

    # numeric types
    dat[[indicator]] <- as.numeric(dat[[indicator]])

    # date is character for monthly/quarterly data, numeric otherwise
    if (!any(grepl('M|Q', dat$year))) {
        dat$year <- as.integer(dat$year)
    }

    # Bad data in WDI JSON files require me to impose this constraint
    dat = dat[!is.na(dat$year) & dat$year <= end & dat$year >= start,]

    # output
    out = list('data' = dat[, 1:4],
               'indicator' = indicator,
               'label' = dat$label[1])

    return(out)
}

#' Update the list of available WDI indicators
#'
#' Download an updated list of available WDI indicators from the World Bank website. Returns a list for use in the \code{WDIsearch} function. 
#' 
#' @return Series of indicators, sources and descriptions in two lists list  
#' @note Downloading all series information from the World Bank website can take time.
#' The \code{WDI} package ships with a local data object with information on all the series
#' available on 2012-06-18. You can update this database by retrieving a new list using \code{WDIcache}, and  then
#' feeding the resulting object to \code{WDIsearch} via the \code{cache} argument. 
#' @export
WDIcache = function(){
    # Series
    series_url = 'https://api.worldbank.org/v2/indicator?per_page=25000&format=json'
    series_dat    = RJSONIO::fromJSON(series_url, nullValue=NA)[[2]]
    series_dat = lapply(series_dat, function(k) cbind(
                        'indicator'=k$id, 'name'=k$name, 'description'=k$sourceNote, 
                        'sourceDatabase'=k$source[2], 'sourceOrganization'=k$sourceOrganization)) 
    series_dat = do.call('rbind', series_dat)          
    # Countries
    country_url = 'https://api.worldbank.org/v2/countries/all?per_page=25000&format=json'
    country_dat = RJSONIO::fromJSON(country_url, nullValue=NA)[[2]]
    country_dat = lapply(country_dat, function(k) cbind(
                         'iso3c'=k$id, 'iso2c'=k$iso2Code, 'country'=k$name, 'region'=k$region[2],
                         'capital'=k$capitalCity, 'longitude'=k$longitude, 'latitude'=k$latitude, 
                         'income'=k$incomeLevel[2], 'lending'=k$lendingType[2])) 
    country_dat = do.call('rbind', country_dat)
    row.names(country_dat) = row.names(series_dat) = NULL
    out = list('series'=series_dat, 'country'=country_dat)
    out$series = iconv(out$series, to = 'utf8')
    out$country = iconv(out$country, to = 'utf8')
    # some regions have extra whitespace in wb data
    out$country[, 'region'] = base::trimws(out$country[, 'region'])
    return(out)
}

#' Search names and descriptions of available WDI series
#' 
#' Data frame with series code, name, description, and source for the WDI series
#' which match the given criteria
#' 
#' @param string Character string. Search for this string using \code{grep} with
#'     \code{ignore.case=TRUE}.
#' @param field Character string. Search this field. Admissible fields:
#'     'indicator', 'name', 'description', 'sourceDatabase', 'sourceOrganization'
#' @param short TRUE: Returns only the indicator's code and name. FALSE: Returns
#'     the indicator's code, name, description, and source.
#' @param cache Data list generated by the \code{WDIcache} function. If omitted,
#'     \code{WDIsearch} will search a local list of series.  
#' @return Data frame with code, name, source, and description of all series which
#'     match the criteria.  
#' @export
#' @examples
#' \dontrun{
#' WDIsearch(string='gdp', field='name', cache=NULL)
#' WDIsearch(string='AG.AGR.TRAC.NO', field='indicator', cache=NULL)
#' }
WDIsearch <- function(string="gdp", field="name", short=TRUE, cache=NULL){
    if(!is.null(cache)){ 
        series = cache$series    
    }else{
        series = WDI::WDI_data$series
    }
    matches = grep(string, series[,field], ignore.case=TRUE)
    if(short){
        out = series[matches, c('indicator', 'name')]
    }else{
        out = series[matches,]
    }
    return(out)
}

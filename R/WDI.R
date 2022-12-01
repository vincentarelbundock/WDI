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
#' the `start` argument. If `NULL`, the end date is set to 5 years in the future.
#' @param extra TRUE returns extra variables such as region, iso3c code, and
#'     incomeLevel. See Details.
#' @param cache NULL (optional) a list created by WDIcache() to be used with the extra=TRUE argument.
#' @param latest Integer indicating the number of most recent non-NA values to get. Default is NULL. If specified, it overrides the start and end dates.
#' @param language ISO-2 code in lower case indicating in which language the characters should be provided. List of languages available with `WDI::languages_supported()`. Default is English.
#'     
#'     
#' @details It is possible to only specify the `indicator` and the `country` arguments, in which case `WDI()` will return data from 1960 to the last year available on World Bank's website. It is also possible to get only the most recent non-NA values, with `latest`.
#' 
#' If `extra = TRUE`, additional variables are provided:
#' 
#' \itemize{
#' 
#' \item{status: observation status, e.g is the observation a forecast?}
#' \item{iso3c}
#' \item{region}
#' \item{capital: name of the capital city}
#' \item{latitude, longitude}
#' \item{income: income categories of the World Bank}
#' \item{lending}
#' 
#' }
#' 
#' @return Data frame with country-year observations. You can extract a
#' data.frame with indicator names and descriptive labels by inspecting the
#' `label` attribute of the resulting data.frame: `attr(dat, 'label')`
#' @author Vincent Arel-Bundock \email{vincent.arel-bundock@umontreal.ca}
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
#'                                   
#' # Get the 5 latest non-NA values
#' WDI(country=c("US","BR"), indicator="NY.GNS.ICTR.GN.ZS", latest = 5)
#' }
#'
WDI <- function(country = "all",
                indicator = "NY.GDP.PCAP.KD",
                start = 1960,
                end = NULL,
                extra = FALSE,
                cache = NULL,
                latest = NULL,
                language = "en") {



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
    is_integer <- function(x) is.numeric(x) && (x %% 1 == 0)
    
    if (is.null(end)) {
        end <- as.integer(format(Sys.Date(), format = "%Y")) + 5
    }

    # If latest is specified then the good order of dates doesn't matter
    if (!is.null(start) && !is.null(end) && is.null(latest)) {
        if (!(start <= end)) {
            stop("`end` must be equal to or greater than `start`.")
        }
        if (is_integer(start) && (start < 1960)) {
            stop("`start` must be equal to or greater than 1960")
        }
    }
    
    # Sanity: needs dates or number of most recent values (but not both)
    if (is.null(start) && is.null(end) && is.null(latest)) {
      stop("Need to specify dates or number of latest values.")
    }
    
    
    # "Language" option (placed here and not in wdi.query because
    # otherwise tryCatch doesn't show the expected error message)
    # get the two first letters (iso code)
    supported_fully <- substr(languages_supported()$fully, 
                              start = 1, 
                              stop = 2)
    supported_locally <- substr(languages_supported()$locally, 
                                start = 1, 
                                stop = 2)
    if (is.null(language)) {
      language <- "en"
    } else {
      if (language %in% supported_locally) {
        warning("This language is only supported partially.")
      } else if (!(language %in% supported_locally ||  
                   language %in% supported_fully)) {
        stop(paste0("This language is not supported. Run ",
                    "WDI::languages_supported() to have a list of ",
                    "fully and partially supported languages."))
      }
    }
    

    # Download
    dat <- list()
    failed <- NULL

    for (i in indicator) {
        tmp <- tryCatch(wdi.dl(i, country, start, end, latest, language, extra), error = function(e) e)
        if (is.null(tmp) || !inherits(tmp$data, 'data.frame') || (nrow(tmp$data) == 0)) {
            failed <- c(failed, i)
        } else {
            dat[[i]] <- tmp
        }
    }

    # Sanity: downloaded data
    if (length(failed) > 0) {

        msg <- sprintf(
'The following indicators could not be downloaded: %s.

Please make sure that you are running the latest version of the `WDI` package, and that the arguments you are using in the `WDI()` function are valid.

Sometimes, downloads will suddenly stop working, even if nothing has changed in the R code of the WDI package. ("The same WDI package version worked yesterday!") In those cases, the problem is almost certainly related to the World Bank servers or to your internet connection.

You can check if the World Bank web API is currently serving the indicator(s) of interest by typing a URL of this form in your web browser:

%s',
paste(failed, collapse = ", "),
wdi.query(indicator = failed[1])[1])

        if (length(failed) == length(indicator)) {
            stop(msg)
        } else if (length(failed) > 0) {
            warning(msg)
        }
    }

    # Extract labels
    lab <- sapply(names(dat), function(x) attr(dat[[x]][["data"]], "label")[1])

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
    for (n in names(lab)) {
        attr(dat[[n]], "label") <- lab[[n]]
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
#' @param timeout integer maximum number of seconds to wait for download
#' @return a list of 6 data frames: Data, Country, Series, Country-Series,
#' Series-Time, FootNote
#' @export
WDIbulk = function(timeout = 600) {

    # store default option
    oo <- options(timeout = timeout)

    if (!'tidyr' %in% utils::installed.packages()[, 1]) {
        stop('To use the `WDIbulk` function, you must install the `tidyr` package.')
    }

    # download
    temp_dir = tempdir()
    temp_file = tempfile(tmpdir = temp_dir)
    url = 'https://databank.worldbank.org/data/download/WDI_csv.zip'
    utils::download.file(url, temp_file)

    # read
    unzipped <- utils::unzip(zipfile = temp_file,
                             exdir = temp_dir)

    out = lapply(unzipped, function(x){
        utils::read.csv(x, stringsAsFactors = FALSE)
    })

    # flush
    unlink(temp_file)

    # names
    names(out) = c("Data", "Country", "Series",
                   "Country-Series", "Series-Time",
                   "FootNote")

    # clean "Data" entry
    out$Data$X = NULL

    out$Data = tidyr::pivot_longer(
        data = out$Data,
        cols = tidyr::starts_with("X"),
        names_to = "year",
        names_prefix = "X",
        values_to = "value"
    )

    # clean year column
    out$Data$year = as.integer(out$Data$year)

    # restore default option
    on.exit(options(oo))

    # output
    return(out)
}

#' Internal function to build API call
#'
#' @export
#' @keywords internal
wdi.query = function(indicator = "NY.GDP.PCAP.CD", 
                     country = 'all', 
                     start = NULL, 
                     end = NULL,
                     latest = NULL,
                     language = "en") {

    country <- paste(country, collapse = ';')

    
    # If latest is specified, dates are overridden
    if (!is.null(latest)) {
      latest <- paste0("&mrnev=", latest)
      years <- NULL
    } else {
      years <- paste0("&date=", start, ":", end)
    }
    
    # WDI only allows 32500 per_page (this seems undocumented)
    out = paste0("https://api.worldbank.org/v2/",
                 language, 
                 "/country/", 
                 country, "/indicator/", indicator,
                 "?format=json",
                 years,
                 "&per_page=32500",
                 "&page=", 1:10,
                 latest)
    return(out)
}

#' Internal function to download data
#'
#' @export
#' @keywords internal
wdi.dl = function(indicator, country, start, end, latest = NULL, language = "en", extra = FALSE){
    get_page <- function(daturl) {
        dat_new <- jsonlite::fromJSON(daturl)
        meta <- dat_new[[1]]
        dat <- dat_new[[2]]
        dat2 <- data.frame(
            country = dat[["country"]][["value"]],
            iso2c = dat[["country"]][["id"]],
            iso3c = dat[["countryiso3code"]],
            year = dat[["date"]],
            indicator = dat[["value"]],
            stringsAsFactors = FALSE)
        
        # issue #54
        if (any(unique(nchar(dat2$iso2c)) == 3)) {
          dat2$iso3c <- dat2$iso2c
          dat2$iso2c <- NULL
        }
        colnames(dat2)[colnames(dat2) == "indicator"] <- indicator
        if (isTRUE(extra) && nrow(dat2) > 0 && "obs_status" %in% colnames(dat)) {
            dat2[["status"]] <- dat[["obs_status"]]
            dat2[["status"]] <- ifelse(dat2[["status"]] == "F", "forecast", dat2[["status"]])
        }
        dat <- dat2

        # output
        attr(dat, "lastupdated") <- tryCatch(meta[["lastupdated"]], error = function(e) NULL)
        attr(dat, "label") <- dat_new[[2]]$indicator$value
        return(dat)
    }

    pages <- wdi.query(indicator, country, start, end, latest, language)
    lab <- attr(pages[[1]], "label")

    dat <- list()
    done <- FALSE # done when pages no longer return useable info
    for (i in seq_along(pages)) {
            tmp <- tryCatch(get_page(pages[i]), error = function(e) NULL)
        if (!done) {
            if (inherits(tmp, 'data.frame') && (nrow(tmp) > 0)) {
                dat[[i]] <- tmp
            } else {
                done <- TRUE
            }
        }
    }
    lastupdated <- attr(dat[[1]], "lastupdated")

    dat <- do.call('rbind', dat)

    # numeric types
    dat[[indicator]] <- as.numeric(dat[[indicator]])

    # date is character for monthly/quarterly data, numeric otherwise
    if (is.factor(dat$year)) {
        dat$year <- as.character(dat$year)
    }
    if (is.character(dat$year) && !any(grepl('M|Q', dat$year))) {
        dat$year <- as.integer(dat$year)
    }

    # Bad data in WDI JSON files require me to impose this constraint
    if (!is.null(start) && !is.null(end)) {
      dat = dat[!is.na(dat$year) & dat$year <= end & dat$year >= start,] 
    }

    # output
    out <- list(
        'data' = dat,
        'indicator' = indicator,
        'label' = lab)

    out$data$label <- NULL

    # updated column
    if (isTRUE(extra) && !is.null(lastupdated)) {
      out[["data"]][["lastupdated"]] <- lastupdated
    }

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
    series_dat <- jsonlite::fromJSON(series_url)[[2]]
    series_dat <- data.frame(
        indicator = series_dat$id,
        name = series_dat$name,
        description = series_dat$sourceNote,
        sourceDatabase = series_dat$source$value,
        sourceOrganization = series_dat$sourceOrganization,
        stringsAsFactors = FALSE)

    # Countries
    country_url = 'https://api.worldbank.org/v2/countries/all?per_page=25000&format=json'
    country_dat <- jsonlite::fromJSON(country_url)[[2]]
    country_dat <- data.frame(
        iso3c = country_dat$id,
        iso2c = country_dat$iso2Code,
        country = country_dat$name,
        region = trimws(country_dat$region$value),
        capital = country_dat$capitalCity,
        longitude = country_dat$longitude,
        latitude = country_dat$latitude,
        income = country_dat$incomeLevel$value,
        lending = country_dat$lendingType$value,
        stringsAsFactors = FALSE)

    out = list('series'=series_dat, 'country'=country_dat)
    # out$series = iconv(out$series, to = 'utf8')
    # out$country = iconv(out$country, to = 'utf8')
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
    matches = grep(string, series[, field], ignore.case=TRUE)
    if(short){
        out = series[matches, c('indicator', 'name')]
    }else{
        out = series[matches,]
    }
    return(out)
}


#' List of supported languages
#' 
#' This prints two lists of languages, the fully supported ones and the locally supported ones:
#' * the languages in the category "fully" will return translated names and other info for all countries.
#' * the languages in the category "partially" will return translated names and other info only for the country they represent. 
#' 
#' For example, choosing "vi" (for Vietnamese) will translate "Vietnam" in the dataset but other country names won't be translated and will be empty.
#'
#'
#' @return A list of fully and partially supported languages.
#' @export
languages_supported <- function() {
  
    fully <- c("en (English)", "es (Spanish)", "fr (French)", 
               "ar (Arabic)", "zh (Chinese)")
    locally <- c("bg (Bulgarian)", "de (German)", "hi (Hindi)", 
                 "id (Indonesian)", "ja (Japanese)", "km (Khmer)", 
                 "ko (Korean)", "mk (Macedonian)", "mn (Mongolian)",
                 "pl (Polish)", "pt (Portuguese)", "ro (Romanian)",
                 "ru (Russian)", "sq (Albanian)", "th (Thai)", "tr (Turkish)",
                 "uk (Ukrainian)", "vi (Vietnamese)")
    
    list(
      fully = fully,
      locally = locally
    )
    
}

skip_on_cran()

library(WDI)

# breaks on travis but works locally
test_that('WDIcache', {

    cache = WDIcache()

    expect_type(cache, 'list')
    expect_gte(length(cache$series), 15000)

    # web cache = stored cache
    old <- WDIsearch('gdp.*capita.*constant')
    new <- WDIsearch('gdp.*capita.*constant', cache=cache)
    expect_identical(old, new)

})

test_that('Bad indicator' , {
    expect_message(expect_s3_class(
        WDI(indicator = c('NY.GDP.PCAP.KD', 'test'), country = 'US', start = 1990, end = 1991),
        'data.frame'))
    expect_message(expect_null(
        WDI(indicator = c('bad', 'test'), country = 'US', start = 1990, end = 1990)))
})

test_that('Bad country', {
    expect_error(WDI(country = c('bad')))
    expect_warning(WDI(country = c('CA', 'bad'), start = 1990, end = 1991))
})

test_that('Bad year', {
    expect_error(WDI(start = 1950))
    expect_error(WDI(start = 1991, end = 1990))
    expect_error(WDI(start = '1990M2', end = '1990M1'))
})

test_that('WDI', {

    # 1 country 1 indicator
    x <- WDI(country='US', indicator='NY.GDP.PCAP.KD', start = 1990, end = 1991) 
    expect_s3_class(x, 'data.frame')
    expect_equal(dim(x), c(2, 4))

    # 3 countries 1 indicator
    x <- WDI(country=c('CA', 'MX', 'US'), 
             indicator='NY.GDP.PCAP.KD', 
             start=1990, end=1991)
    expect_s3_class(x, 'data.frame')
    expect_equal(dim(x), c(6, 4))
    expect_equal(length(unique(x$iso2c)), 3)

    # multiple indicators
    ind <- c("NY.GDP.PCAP.KD", "NY.GDP.PCAP.KN", "NY.GDP.PCAP.PP.KD")
    x <- WDI(indicator = ind, start=1990, end=1991)
    expect_gte(nrow(x), 30)
    expect_equal(ncol(x), 6)

    # bad countries
    co <- c('BADCOUNTRY_one', 'BADCOUNTRY_two', 'CA', 'MX', 'US')
    x <- expect_warning(WDI(country = co, start=1990, end=1991))
    expect_equal(length(unique(x$iso2c)), 3)

    # bad indicator 
    ind <- c("blah blah", "NY.GDP.PCAP.KD", "NY.GDP.PCAP.KN", "NY.GDP.PCAP.PP.KD", "NY.GDP.PCAP.PP.KD.87")
    x <- expect_message(WDI(indicator = ind, start=1990, end=1991))
    expect_gte(nrow(x), 30)
    expect_equal(ncol(x), 6)

})

test_that('WDI(extra = TRUE)', {

    x <- WDI(country='US', indicator='NY.GDP.PCAP.KD', 
             start=1991, end=1992, extra = TRUE)
    expect_s3_class(x, 'data.frame')
    expect_equal(dim(x), c(2, 11))

})

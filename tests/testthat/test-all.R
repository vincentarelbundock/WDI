context('All tests in one file')

library(WDI)

# breaks on travis but works locally
#test_that('WDIcache', {

    #cache = WDIcache()

    #expect_type(cache, 'list')
    #expect_gte(length(cache$series), 15000)

    ## web cache = stored cache
    #old <- WDIsearch('gdp.*capita.*constant')
    #new <- WDIsearch('gdp.*capita.*constant', cache=cache)
    #expect_identical(old, new)

#})

test_that('WDI', {

    # 1 country 1 indicator
    x <- WDI(country='US', indicator='NY.GDP.PCAP.KD', 
             start=1960, end=2005)
    expect_s3_class(x, 'data.frame')
    expect_gte(nrow(x), 30)
    expect_equal(ncol(x), 4)

    # 3 countries 1 indicator
    x <- WDI(country=c('CA', 'MX', 'US'), 
             indicator='NY.GDP.PCAP.KD', 
             start=1960, end=2005)
    expect_s3_class(x, 'data.frame')
    expect_gte(nrow(x), 30)
    expect_equal(ncol(x), 4)
    expect_equal(length(unique(x$iso2c)), 3)

    # multiple indicators
    ind <- c("NY.GDP.PCAP.KD", "NY.GDP.PCAP.KN", "NY.GDP.PCAP.PP.KD")
    x <- WDI(indicator = ind)
    expect_gte(nrow(x), 30)
    expect_equal(ncol(x), 6)

    # bad countries
    co <- c('BADCOUNTRY_one', 'BADCOUNTRY_two', 'CA', 'MX', 'US')
    expect_warning(WDI(country = co))

    # bad indicator 
    ind <- c("blah blah", "NY.GDP.PCAP.KD", "NY.GDP.PCAP.KN", "NY.GDP.PCAP.PP.KD", "NY.GDP.PCAP.PP.KD.87")
    x <- expect_message(WDI(indicator = ind))
    expect_gte(nrow(x), 30)
    expect_equal(ncol(x), 6)

})

test_that('WDI(extra = TRUE)', {

    x <- WDI(country='US', indicator='NY.GDP.PCAP.KD', 
             start=1960, end=2005, extra = TRUE)
    expect_s3_class(x, 'data.frame')
    expect_gte(nrow(x), 30)
    expect_equal(ncol(x), 11)

})

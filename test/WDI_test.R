# Warning no longer works
rm(list=ls())
library(WDI)

# Country list
co = c('IN', 'US', 'BR', 'ZW')

# Cache
cache = WDIcache()
str(cache)

# Search
WDIsearch('gdp.*capita.*constant')
WDIsearch('gdp.*capita.*constant', cache=cache)

# 1 country 1 indicator
x = WDI(country='US', indicator='NY.GDP.PCAP.KD', start=1960, end=2005)
head(x)

# 3 countries 1 indicator
x = WDI(country=c('CA', 'MX', 'US'), indicator='NY.GDP.PCAP.KD', start=1960, end=2005)
head(x)

# 1 bad country (should raise warning) 
x = WDI(country=c('BADCOUNTRY_one', 'BADCOUNTRY_two', 'CA', 'MX', 'US'), indicator='NY.GDP.PCAP.KD', start=1960, end=2005)
head(x)

# Multiple indicators 1 country
x = WDIsearch('gdp.*capita')
x = WDI(country=co, indicator=x[,1], start=2002, end=2003)
head(x)

# Multiple indicators 1 country + 2 bad indicator (should raise warning)
x = WDIsearch('gdp.*capita')
x = WDI(country=c('BADCOUNTRY', 'CA', 'MX'), indicator=c('BADIND1', 'BADIND2',  x[1:4,1]))
head(x)

# All countries, multiple indicators
x = WDIsearch('gdp.*capita')
x = WDI(country='all', indicator=c('BADIND', x[1:4,1]))
head(x)

# Bad, but allowable user input
x = WDIsearch('gdp.*capita')
x = WDI(country=c('MX', 'all'), indicator=c('BADIND', x[1:4,1]))
head(x)

# Long list of countries
x = WDI(country=WDI_data[[2]][,'iso2c'])
head(x)

# Extra information
rm(x)
x = WDI(country=co, extra=TRUE)

# iso3c and iso2c
x = WDI(country=c('US', 'CAN'))

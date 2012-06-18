# World Bank Development indicators for R

The World Bank makes available a ton of great data from its World Development Indicators through its web API. The `WDI` package for `R` makes it easy to search and download data series from the WDI. 

# Installation

`WDI` is published on CRAN and so can be installed by simply typing: 

    install.packages('WDI')

# Searching for data

You can search for data by using keywords in the `WDIsearch`. For instance, if you are looking for data on Gross Domestic Product (i.e. GDP): 

```r
WDIsearch('gdp')
```

Which produces this: 

```r
> WDIsearch('gdp')[1:10,]
      indicator              name                                                                      
 [1,] "BG.GSR.NFSV.GD.ZS"    "Trade in services (% of GDP)"                                            
 [2,] "BM.KLT.DINV.GD.ZS"    "Foreign direct investment, net outflows (% of GDP)"                      
 [3,] "BN.CAB.XOKA.GD.ZS"    "Current account balance (% of GDP)"                                      
 [4,] "BN.CUR.GDPM.ZS"       "Current account balance excluding net official capital grants (% of GDP)"
 [5,] "BN.GSR.FCTY.CD.ZS"    "Net income (% of GDP)"                                                   
 [6,] "BN.KLT.DINV.CD.ZS"    "Foreign direct investment (% of GDP)"                                    
 [7,] "BN.KLT.PRVT.GD.ZS"    "Private capital flows, total (% of GDP)"                                 
 [8,] "BN.TRF.CURR.CD.ZS"    "Net current transfers (% of GDP)"                                        
 [9,] "BNCABFUNDCD_"         "Current Account Balance, %GDP"                                           
[10,] "BX.KLT.DINV.WD.GD.ZS" "Foreign direct investment, net inflows (% of GDP)" 
```

`WDIsearch` uses grep and ignores cases, so you can also use regular expressions. For instance, if you are looking for GDP per capita in constant dollars: 

```r
WDIsearch('gdp.*capita.*constant')
     indicator           name                                                 
[1,] "GDPPCKD"           "GDP per Capita, constant US$, millions"             
[2,] "NY.GDP.PCAP.KD"    "GDP per capita (constant 2000 US$)"                 
[3,] "NY.GDP.PCAP.KN"    "GDP per capita (constant LCU)"                      
[4,] "NY.GDP.PCAP.PP.KD" "GDP per capita, PPP (constant 2005 international $)"
```

# Download and use the data

Download a series you like for the countries you like:

```r
dat = WDI(indicator='NY.GDP.PCAP.KD', country=c('MX','CA','US'), start=1960, end=2012)
```

Look at the data: 

```r
head(dat)
  iso2c country NY.GDP.PCAP.KD year
1    CA  Canada       9374.883 1960
2    CA  Canada       9479.824 1961
3    CA  Canada       9967.366 1962
4    CA  Canada      10290.362 1963
5    CA  Canada      10774.653 1964
6    CA  Canada      11283.606 1965
```

Plot the data:

```r
library(ggplot2)
ggplot(dat, aes(year, NY.GDP.PCAP.KD, color=country)) + geom_line() + 
    xlab('Year') + ylab('GDP per capita')
```

![GDP per capita in North America](https://github.com/vincentarelbundock/WDI/raw/master/web/gdp_per_capita.jpg)

Note: You can use 'all' to download data for all available countries. You can also feed a vector of indicator strings if you want to download multiple indicators at once.


# Updating series list

To speed up search, `WDI` ships with a local list of all available WDI series as of 2012-06-18. This list will be updated semi-regularly, but you may still want to update it manually to get access to the very latest data series. To do so, use the `cache` function:

new_cache = WDIcache()
WDIsearch('gdp', cache=new_cache)




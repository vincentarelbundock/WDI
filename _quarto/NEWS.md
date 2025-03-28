# News

## 2.7.9

* The bulk download URL has changed. The package now uses the new URL. Thanks to [@mjantti](https://github.com/mjantti) for issue [#66](https://github.com/vincentarelbundock/WDI/issues/66). 
* Fix a bug where ISO3 codes were wrongly attributed to column `iso2c` ([#54](https://github.com/vincentarelbundock/WDI/issues/54)).
* `wdi.dl()` and `wdi.query()` are not exported anymore. Those were undocumented anyway ([#65](https://github.com/vincentarelbundock/WDI/issues/65)).
* Update the series search cache.

## 2.7.8

* Move to `jsonlite`
* `end` is NULL by default

## 2.7.7

* Update the cache

## 2.7.6

* `WDI(extra = TRUE)` includes a new `lastupdated` column.

## 2.7.5

* Add the observation status (whether an observation is a forecast) when `extra = TRUE` in `WDI()` ([#48](https://github.com/vincentarelbundock/WDI/issues/48))
* Updated cache

## 2.7.4

* Bug fix for on-the-fly rename

## 2.7.3

* Better error message
* Bug fixes

## 2.7.2

* Add support for several languages with the "language" argument in `WDI()` ([#15](https://github.com/vincentarelbundock/WDI/issues/15))
* Add "latest" argument in `WDI()`, to fetch the most recent non-NA values ([#29](https://github.com/vincentarelbundock/WDI/issues/29), [#43](https://github.com/vincentarelbundock/WDI/issues/43))
* Speed improvement in WDIbulk (Thanks to [@etiennebacher](https://github.com/etiennebacher))
* WDIbulk gets a `timeout` argument

## 2.7.1

* Bug: did not download all countries
* WDIcache did not retrieve regions

## 2.7.0 

* API v2
* Fix a breaking change after API v2
* per_page not large enough when dataset is big
* Monthly and quarterly data

## 2.6.0

* `start` and `end` are NULL as default and download all available years.
* `label` attribute to the return data.frame includes descriptive labels for each
* `WDIbulk` function to download all of WDI
* WDIcache and data trim whitespace from region names

## 2.5.1

* New # WDI numbering scheme
* Updated country and indicator data

## 2.5.0

* Namespace issues (thanks to Jan Dietrich)
* Updated the list of available indicators
* Allow iso3c or iso2c as input for country argument
* Rolled-back indicator input clean up because too aggressive (Thanks to Geoff Wright)
* Rename columns automatically when using a named vector

## 2.4.0

* Call RJSONIO::fromJSON explicitly to avoid conflict with JSON package (Thanks to Richard Cotton)

## 2.3.0

* Change default start/end date per user request

## 2.2.0

* Bug: country names were stripped of digits

## 2.1.0

* Fixed various issues (thanks to Matthieu Stigler for bug reports and patches)
* Faster downloads when requesting multiple countries (leveraging the US;BR;CA API syntax)

## 2.0.0

* Now uses JSON instead of XML (Faster, easier)
* WDIcache function now allows users to update the locally stored database of available WDI series
* Fixed doc examples  
* Documentation now uses roxygen2
* Moved development to github 
* Series and country info now in a single data list. utf-8 encoding fixed    
 
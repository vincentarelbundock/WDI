# WDI 2.7.1

* Bug: did not download all countries
* WDIcache did not retrieve regions

# WDI 2.7.0 

* API v2
* Fix a breaking change after API v2
* per_page not large enough when dataset is big
* Monthly and quarterly data

# WDI 2.6.0

* `start` and `end` are NULL as default and download all available years.
* `label` attribute to the return data.frame includes descriptive labels for each
* `WDIbulk` function to download all of WDI
* WDIcache and data trim whitespace from region names

# WDI 2.5.1

* New # WDI numbering scheme
* Updated country and indicator data

# WDI 2.5.0

* Namespace issues (thanks to Jan Dietrich)
* Updated the list of available indicators
* Allow iso3c or iso2c as input for country argument
* Rolled-back indicator input clean up because too aggressive (Thanks to Geoff Wright)
* Rename columns automatically when using a named vector

# WDI 2.4.0

* Call RJSONIO::fromJSON explicitly to avoid conflict with JSON package (Thanks to Richard Cotton)

# WDI 2.3.0

* Change default start/end date per user request

# WDI 2.2.0

* Bug: country names were stripped of digits

# WDI 2.1.0

* Fixed various issues (thanks to Matthieu Stigler for bug reports and patches)
* Faster downloads when requesting multiple countries (leveraging the US;BR;CA API syntax)

# WDI 2.0.0

* Now uses JSON instead of XML (Faster, easier)
* WDIcache function now allows users to update the locally stored database of available WDI series
* Fixed doc examples  
* Documentation now uses roxygen2
* Moved development to github 
* Series and country info now in a single data list. utf-8 encoding fixed    
 

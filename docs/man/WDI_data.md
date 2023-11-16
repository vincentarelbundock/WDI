
# WDI_data

World Development Indicators series and country information

## Description

A list of two character matrices.

## Usage

<pre><code class='language-R'>WDI_data
</code></pre>

## Format

List of 2 data frames

## Details

The first character matrix includes a full list of WDI series. This list
is updated semi-regularly. Users can refresh the list manually using the
‘WDIcache()’ function and search in the updated list using the ‘cache’
argument.

<ul>
<li>

id WDI indicator code

</li>
<li>

name Short name of the series

</li>
<li>

sourceNote Detailed description provided by the source

</li>
<li>

source Source database

</li>
<li>

sourceOrganization Source organization

</li>
</ul>

The second character matrix includes extra country information (updated
2012-06-18):

<ul>
<li>

iso3c ISO 3-letter country code

</li>
<li>

iso2c ISO 2-letter country code

</li>
<li>

country Country name (long English form)

</li>
<li>

region Region

</li>
<li>

capital Capital

</li>
<li>

longitude Longitude

</li>
<li>

latitude Latitude

</li>
<li>

income Income level as defined by the World Bank

</li>
<li>

lending Lending category, as defined by the World Bank

</li>
</ul>

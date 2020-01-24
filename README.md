[![Build Status](https://travis-ci.org/stix-global/eutradeflows.svg?branch=master)](https://travis-ci.org/stix-global/eutradeflows)

# R package eutradeflows
A validation algorithm for forest products trade data from Comext

# Installation
To install this R package from github:

    devtools::install_github("stix-global/eutradeflows")
    
# Help
See package vignettes under inst/doc or in the help index
by entering `?eutradeflows` at the R prompt.



# Metadata


## Units

Trade value is expressed in euros. 

See [Comext user guide](https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=comext%2FCOMEXT_METADATA%2FDOCS_AND_GUIDES%2FUser+guide++-+2016+edition.pdf)
Net mass is expressed in kilograms i.e. the weight of the goods without any packaging.

The supplementary quantity is expressed in various units (m3, meters, ...) specified in the unit column.

## Flags and special values

https://ec.europa.eu/eurostat/data/database/information
Flags and special value used in Eurostat's online database
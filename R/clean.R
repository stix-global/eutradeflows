#' Clean Comext Monthly data
#' 
#' Re-uses several functions from the tradeflows package.
#' Each function mentionned specifically tradeflows::fun().
#' Hadley: http://r-pkgs.had.co.nz/namespace.html#imports
#' "
#'     It’s common for packages to be listed in Imports in DESCRIPTION, but not in NAMESPACE. 
#'     In fact, this is what I recommend: list the package in DESCRIPTION so that it’s installed,
#'     then always refer to it explicitly with pkg::fun().
#' "
#' @param dtf data frame of monthly data for one product
cleancomextmonthly <- function(dtf){
   
    # Checks
    # Check if the period is sufficiently long, at least 5 years 
    
    
    dtf %>%  
        tradeflows::addconversionfactorandprice() 
    
    ### Prepare conversion factors and prices
    price <- tradeflows::extractprices(dtf, grouping = c("flow", "year", "unit"))
    conversionfactor <- tradeflows::extractconversionfactors(dtf)
    
    
    
}



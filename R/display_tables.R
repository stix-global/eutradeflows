#' @name replace_repeated_by_empty_string
#' @title  Replace repeated values by the empty string
#' @description 
#' This function replaces duplicated values in a column by the empty string so that the table output is easier to read.
#' The removal is done by comparing the vector with a shifted (lagged) version of itself.
#' By default this function will convert numeric ouptut to character variables, 
#' unless you change the default \code{empty_value=''} argument.
#' @param x a vector
#' @param empty_value the default values used for the lag, should be different than the first value in x
#' @examples
#' replace_repeated_by_empty_string(c("bla","bla","bli", "bli"))
#' replace_repeated_by_empty_string(c(1,2,3))
#' replace_repeated_by_empty_string(c(0,0,0,1,2,2,3,3,3))
#' @return vector
#' @export
replace_repeated_by_empty_string <- function(x, empty_value=''){
    # Fill the first value considered in the comparison with x
    lag_default <- 0
    # Lag default should always be different than the first value
    if (x[1] == 0){lag_default<-1}
    ifelse(x==lag(x, default=lag_default),
           empty_value, x)
}

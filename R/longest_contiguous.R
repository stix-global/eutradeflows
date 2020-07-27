#' Longest contiguous time series for the given variable in the given data
#' frame
#' @param df data frame
#' @param variable character name of the variable of interest
#' @return list of start and end dates for the longest time series
longest_contiguous <- function(df, variable){
    df <- arrange(df, period)
    # Check the function is valid for one reporter, partner pair only
    if (nrow(distinct(df, reporter, partner)) != 1){
        stop("This function works on a single country pair only")
    }
    # Initialise hole = indicator of missing value
    df$hole <- is.na(df[variable])
    df$sumhole <- cumsum(df$hole)
    runlength <- rle(df$sumhole)
    start_longest <- runlength$value[which.max(runlength$lengths)]
    start_longest <- df$period[df$sumhole == start_longest][2]
    end_longest <- runlength$value[which.max(runlength$lengths)+1] 
    end_longest <- df$period[df$sumhole == end_longest]
    return(list(start = start_longest, end=end_longest))
}


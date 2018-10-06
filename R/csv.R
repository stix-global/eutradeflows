#' Load trade flows for the given product
#' @rdname write2csv
#' @export
loadflows <- function(RMariaDBcon, productanalysed,
                      periodstart = (as.numeric(format(Sys.time(), "%Y")) - 3) * 100, 
                      tableread = "vld_comext_monthly"){
    productpattern <- paste0(productanalysed, "%")
    dtf <- tbl(RMariaDBcon, tableread) %>%
        filter(productcode %like% productpattern &
                   period > periodstart) %>% 
        addproreppar2tbl(con,.) %>% 
        collect() 
    return(dtf)
}


loadaggflows <- function(){
    
}


#' @rdname write2csv_spread
#' @details \code{formatflows} edits information so that the exported data
#' is more pleasant to use. 
#' For example missing partner is replaced by the partnercode 
#' Long descriptions are truncated.
#' @export
formatflows <- function(dtf){
    dtf %>% 
        # Flow information (specifying import, export), 
        # cannot be added in addproreppar2tbl() 
        # because the little data frame created below
        # is not a database table
        left_join(data_frame(flow = c("import", "export"), flowcode = 1:2),
                  by = "flowcode") %>% 
        mutate_at(vars(starts_with("quantity")),round) %>% 
        mutate(
            # Cut long partner description
            partner = substr(partner, 0, 40),
            # Replace empty partner by the partner code
            partner = ifelse(is.na(partner), partnercode, partner),
            # Rename Germany so that it's consistent with partner name
            reporter = if_else(grepl("Fr Germany",reporter),
                               "Germany", reporter)) 
    
}


#' Spread quantity data along the period and write to a csv file
#' Write a data frame containing trade flows to a csv file
#' @param dtf a data frame containing validated trade flows 
#' @param csvfile character path to a csv file
#' @examples \dontrun{ # Clean product and country codes
#' con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "test")
#' csvfile <- tempfile(fileext = ".csv")
#' loadflows(con, productanalysed = "44071190", periodstart = 201700, 
#'           tableread = "vld_comext_monthly") %>% 
#'     write2csv_spread(csvfile)
#' readLines(csvfile,5) # Check the first lines of the csv file
#' RMariaDB::dbDisconnect(con)
#' }
#' @return The input data frame invisibly
#' @export
write2csv_spread <- function(dtf, csvfile){
    dtf %>%
        # Keep only these columns
        select(productcode, flow, statregime, flag, reporter, partner,
               reportercode, partnercode, flowcode, unitcode,
               period, quantity,
               productdescription) %>%
        # Collapse flags present in one line for all periods (after the spread)
        group_by(productcode, flowcode, reportercode, partnercode, statregime) %>% 
        mutate(flag = paste(unique(flag), collapse = ", ")) %>% 
        spread(period, quantity) %>% 
        # Move description and code to the last columns
        select(-productdescription, -reportercode, -partnercode, -flowcode, -unitcode,
               productdescription, reportercode, partnercode, flowcode, unitcode) %>% 
        arrange(productcode, flowcode, reporter, partner) %>% 
        write.csv(csvfile, row.names = FALSE, na="")
    return(invisible(dtf))
}


#' @rdname write2csv_spread
#' @export
write2csv_long <- function(dtf, csvfile){
    dtf %>%  
        select(productcode, flow, statregime, flag, reporter, partner, 
               period, tradevalue, weight, quantity, quantityraw,
               quantity_cf, quantity_up,productdescription,
               reportercode, partnercode, unitcode, flowcode) %>% 
        write.csv(csvfile, row.names = FALSE, na ="")
    return(invisible(dtf))
}


#' Extract a list of unique, most recent product, reporter and partner codes from Comext
#'
#' Takes a raw codes table from comext, select codes which have the most
#' recent \code{datestart} and make sure they are unique.
#' @param RMySQLcon database connection object created by RMySQL \code{\link[DBI]{dbConnect}}
#' @param tableread character name of the table to read from
#' @param tablewrite character name of the table to write to
#' @param codevariable unquoted code variable (à la dplyr verbs)
#' @return TRUE on success
#' The output is actually a database table containing the cleaned codes.
#' @examples \dontrun{ # Clean product and country codes
#' # Connect to the database
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' # Write dummy codes to the database table "raw_code"
#' raw_code <- data.frame(code = c(4L, 4L), datestart = c(1L, 2L))
#' RMySQL::dbWriteTable(con, "raw_code", raw_code, row.names = FALSE, overwrite = TRUE)
#' # Clean the codes and write them to the database table "vld_code" (for validated code)
#' cleancode(con, tableread = "raw_code", tablewrite = "vld_code", codevariable = "code")
#'
#' # Comext codes
#' if(FALSE){ # If raw codes are not present, transfer them
#' createdbstructure(sqlfile = "raw_comext.sql", dbname = "test")
#' tradeharvester::transfertxtcodesfolder2db(con, rawdatacomextfolder = "~/R/tradeharvester/data-raw/comext/201707/text/english/")
#' }
#' # Clean comext product, reporter and partner codes
#' cleanallcomextcodes(con)
#' # Disconnect from the database
#' RMySQL::dbDisconnect(con)
#' }
#' @export
cleancode <- function(RMySQLcon, tableread, tablewrite, codevariable){
    # Implementation based on the "programming with dplyr" vignette
    # https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
    codevariable <- enquo(codevariable)
    
    # Check if output fields are in input fields
    inputfields <- RMySQL::dbListFields(RMySQLcon, tableread)
    outputfields <- RMySQL::dbListFields(RMySQLcon, tablewrite)
    stopifnot(outputfields %in% inputfields)
    
    # This function cannot use  RMySQL::dbWriteTable with overwrite = TRUE
    # because this would also overwrites the field types and indexes.
    # dbWriteTable chooses default types that are not optimal,
    # for example, it changes date fields to text fields.
    # Therefore use RMySQL::dbWriteTable with append = TRUE,
    # but first check if the table is empty
    # and if it is not empty, ask to recreate the database
    # structure with empty tables.
    res <- RMySQL::dbSendQuery(RMySQLcon, sprintf("SELECT COUNT(*) as nrow FROM %s;",tablewrite))
    sqltable <- RMySQL::dbFetch(res)
    RMySQL::dbClearResult(res)
    # Check if the output table is empty
    if(sqltable$nrow > 0){
        stop("Table ", tablewrite, " is not empty.",
             "You can recreate an empty table structure with:\n",
             sprintf("createdbstructure(sqlfile = 'vld_comext.sql', dbname = '%s')",
                     RMySQL::dbGetInfo(RMySQLcon)$dbname))
    }
    
    
    # load all codes and keep only most recent codes
    rawcode <- tbl(RMySQLcon, tableread) %>%
        collect() 
    vldcode <- rawcode %>%
        group_by(!!codevariable) %>%
        filter(datestart == max(datestart)) %>%
        select(outputfields)
    
    # After cleaning, 
    # the number of distinct rows for all columns should be equal to
    # the number of distinct codes in the raw dataset
    stopifnot(identical(nrow(unique(vldcode)),
                        nrow(distinct(rawcode, !!codevariable))))
    # Remove duplicates
    vldcode <- unique(vldcode)
    # Write back to the database
    RMySQL::dbWriteTable(RMySQLcon, tablewrite, vldcode,
                         row.names = FALSE, append = TRUE)
}


#' @description \code{cleanallcomextcodes} extracts unique product
#' and country codes from the Comext raw data so that they are ready for use
#' as unique keys.
#' It is a porcelaine function based on the plumbing function \code{cleancode}.
#'
#' @rdname cleancode
#' @export
cleanallcomextcodes <- function(RMySQLcon){
    createdbstructure(sqlfile = "vld_comext.sql",
                      # extract db name from the RMySQL connection object
                      dbname = RMySQL::dbGetInfo(RMySQLcon)$dbname)
    message("Cleaning product, reporter and partner codes...")
    cleancode(RMySQLcon, "raw_comext_product", "vld_comext_product", productcode)
    cleancode(RMySQLcon, "raw_comext_reporter", "vld_comext_reporter", reportercode)
    cleancode(RMySQLcon, "raw_comext_partner", "vld_comext_partner", partnercode)
    
    # Diagnostics
    # Display row count information
    # based on https://stackoverflow.com/a/1775272/2641825
    res <- RMySQL::dbSendQuery(RMySQLcon, "SELECT
                               (SELECT COUNT(*) FROM   vld_comext_product)  AS product,
                               (SELECT COUNT(*) FROM   vld_comext_reporter) AS reporter,
                               (SELECT COUNT(*) FROM   vld_comext_partner)  AS partner")
    nrows <- RMySQL::dbFetch(res)
    RMySQL::dbClearResult(res)
    message("Transfered:\n",
            nrows$product, " rows to the vld_comext_product table\n",
            nrows$reporter, " rows to the vld_comext_reporter table\n",
            nrows$partner, " rows to the vld_comext_partner table.\n")
}


#' Add product reporter and partner to a tbl object
#' @return a tbl object left joined to the product, reporter and partner tables.
#' @param RMySQLcon database connection object created by RMySQL \code{\link[DBI]{dbConnect}}
#' @param maintbl tbl containing trade data, with productcode, reportercode and partnercode
#' @examples \dontrun{
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' monthly <- tbl(con, "raw_comext_monthly_201707")
#' monthly %>%
#'     filter(productcode == 44) %>%
#'     addproreppar2tbl(con, .) %>%
#'     collect()
#' RMySQL::dbDisconnect(con)
#'
#' }
#' @export
addproreppar2tbl <- function(RMySQLcon, maintbl){
    maintbl %>%
        left_join(tbl(RMySQLcon, "vld_comext_product"),
                  by = "productcode") %>%
        left_join(tbl(RMySQLcon, "vld_comext_reporter"),
                  by = "reportercode") %>%
        left_join(tbl(RMySQLcon, "vld_comext_partner"),
                  by = "partnercode")
}


#' Count the number of distinct rows in a database table for a given variable
#' @param RMySQLcon database connection object created by RMySQL \code{\link[DBI]{dbConnect}}
#' @param tablename character name of a database table 
#' @param variable character name of a variable in that database table
#' @return numeric value
#' @examples 
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
#' on.exit(RMySQL::dbDisconnect(con))
#' # Transfer the iris data frame to the database
#' RMySQL::dbWriteTable(con, "iris_in_db", iris, row.names = FALSE, overwrite = TRUE)
#' # Count the number of species
#' dbndistinct(con, "iris_in_db", Species)
#' @export
dbndistinct <- function(RMySQLcon, tablename, variable){
    variable <- enquo(variable)
    dtf <- tbl(RMySQLcon, tablename) %>% 
        distinct(!!variable) %>% 
        summarise(n = n()) %>% 
        collect() 
    return(dtf$n)
}



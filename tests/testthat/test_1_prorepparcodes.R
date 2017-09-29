# Tests below will only run if a test database is available
# RMySQL::mysqlHasDefault() checks if a default database is available
#
# If needed, create a test database
# MySQL should be installed on your system
# connect to the mysql client as root:
# $ mysql -u root -p
# Then give the following mysql commands:
# mysql> create database test;
# mysql> connect test;
# mysql> grant all privileges on * . * to R@localhost;
# Then configure the rs-dbi group by adding those lines to the ~/.my.cnf
#    [rs-dbi]
#    user = R
#    password = ***
#    host = localhost
#    database = test
#
# See RMySQL test examples at:
# https://github.com/rstats-db/RMySQL/tree/master/tests/testthat


# Create dummy database structure
dummysql <- "dummytables.sql" # file name will be reused to delete all tables at the end of the test suite
if (RMySQL::mysqlHasDefault()){
    createdbstructure(dummysql, dbname = "test", sqlfolder = ".",verbose=FALSE)
}

# Create dummy codes
# Declare numeric values as integer data type
# Keep default declaration of character vectors as factors, since that
# is what will be returned by the data frame reading operation.
raw_dummy_code <- data.frame(code = c(4, 4L, 4L),
                             datestart = c("2001-01-01", "2002-01-01", "2002-01-01"),
                             dateend = c("2001-12-31", "2500-01-01", "2500-01-01"),
                             description = c("old","recent","recent"),
                             stringsAsFactors = FALSE)
raw_dummy_product <- data.frame(productcode = c(44L, 44L),
                                productdescription = c("old","recent"),
                                datestart = c("2001-01-01", "2002-01-01"),
                                dateend = c("2001-12-31", "2500-01-01"),
                                stringsAsFactors = FALSE)
raw_dummy_reporter <- data.frame(reportercode = c(5L, 6L, 5L),
                                 reporter = c("oldcountryA", "recentcountryB", "recentcountryA"),
                                 datestart = c("2001-01-01", "2002-01-01","2002-01-01"),
                                 dateend = c("2001-12-31", "2500-01-01","2500-01-01"),
                                 stringsAsFactors = FALSE)
vld_dummy_unit <- data.frame(productcode = c("1", "2", "2", "3"),
                             unitcode    = c("A", "A", "B", "A"),
                             periodstart = c("200001", "200001", "201001", "200001"),
                             periodend   = c("250012", "200912", "250012", "250012"))

context("Test database writable")
test_that("dummy data can be written to the database and read back", {
    # These tests will only run if a test database is available
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    # Connect to the database defined by the rs-dbi group
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    # Write a csv file to disk
    write.csv(raw_dummy_code, "raw_dummy_code.csv", row.names = FALSE)
    # Transfer a csv file to the database
    RMySQL::dbWriteTable(con, "raw_dummy_code", "raw_dummy_code.csv", sep=",", eol="\n", overwrite = TRUE)
    expect_equal(RMySQL::dbReadTable(con, "raw_dummy_code"),
                 raw_dummy_code)
    # Transfer a data frame to the database
    RMySQL::dbWriteTable(con, "raw_dummy_code2", raw_dummy_code, row.names = FALSE,overwrite = TRUE)
    expect_equal(RMySQL::dbReadTable(con, "raw_dummy_code2"), raw_dummy_code)
    res <- RMySQL::dbSendQuery(con, "DROP TABLE IF EXISTS `raw_dummy_code2`;")
    # clear result, to avoid the warning "Closing open result sets"
    RMySQL::dbClearResult(res)
})


context("cleancode")
test_that("codes correspond to the max(datestart) and are unique", {
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    createdbstructure("dummytables.sql", dbname = "test", sqlfolder = ".",verbose=FALSE)
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    # Write all codes to the database
    RMySQL::dbWriteTable(con, "raw_dummy_code", raw_dummy_code, row.names = FALSE, append = TRUE)
    RMySQL::dbWriteTable(con, "raw_dummy_product", raw_dummy_product, row.names = FALSE, append = TRUE)
    RMySQL::dbWriteTable(con, "raw_dummy_reporter", raw_dummy_reporter, row.names = FALSE, append = TRUE)

    # Clean dummy codes
    # !Attention use unquoted name
    cleancode(con, tableread = "raw_dummy_code", tablewrite = "vld_dummy_code",
              codevariable = code)
    # Clean product codes
    cleancode(con, tableread = "raw_dummy_product", tablewrite = "vld_dummy_product",
              codevariable = productcode)
    # Clean reporter codes
    cleancode(con, tableread = "raw_dummy_reporter", tablewrite = "vld_dummy_reporter",
              codevariable = reportercode)
    # Read back data
    vld_dummy_code <- RMySQL::dbReadTable(con, "vld_dummy_code", row.names=FALSE)
    vld_dummy_product <- RMySQL::dbReadTable(con, "vld_dummy_product")
    vld_dummy_reporter <- RMySQL::dbReadTable(con, "vld_dummy_reporter")
    # Test unique
    expect_equal(vld_dummy_code$code, unique(raw_dummy_code$code))
    expect_equal(vld_dummy_product$code, unique(raw_dummy_product$code))
    expect_equal(vld_dummy_reporter$code, unique(raw_dummy_reporter$code))
    # Test most recent description
    expect_equal(vld_dummy_code$description, "recent")
    expect_equal(vld_dummy_product$productdescription, "recent")
    expect_equal(vld_dummy_reporter$reporter, c("recentcountryB", "recentcountryA"))
})


test_that("an error is raised if most recent codes are not exact duplicates", {
    # skip("want to see the mysql database content after the other test")
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    createdbstructure("dummytables.sql", dbname = "test", sqlfolder = ".",verbose=FALSE)
    raw_dummy_code_deffect <- data.frame(code = c(4L, 4L, 4L),
                                   datestart = c(1L, 2L, 2L),
                                   description = c("a", "b", "c"),
                                   stringsAsFactors = FALSE)
    RMySQL::dbWriteTable(con, "raw_dummy_code", raw_dummy_code_deffect, row.names = FALSE, overwrite = TRUE)
    expect_error(cleancode(con, tableread = "raw_dummy_code", tablewrite = "vld_dummy_code", codevariable = code),
                 regexp = "identical")
})


context("addunit2tbl")
test_that("Product codes are matched with the correct unit before and after a change", {
    # Connect to the database
    if (!RMySQL::mysqlHasDefault()) skip("Test database not available")
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    on.exit(RMySQL::dbDisconnect(con))
    # Transfer dummy data to the database
    createdbstructure("dummytables.sql", dbname = "test", sqlfolder = ".",verbose=TRUE)
    dtf <- data.frame(productcode = c("1", "2", "2", "2", "2",
                                      "3", "4"), 
                      period = c(201801L, 200911L, 200912L, 201001L, 201002L,
                                 201801L, 201801L),
                      tradevalue = 2*1:7)
    RMySQL::dbWriteTable(con, "raw_dummy_monthly", dtf,
                         row.names = FALSE, append = TRUE)
    RMySQL::dbWriteTable(con, "vld_dummy_unit", vld_dummy_unit,
                         row.names = FALSE, append = TRUE)
    # Keep 
    
    # Add unit to the trade flows table
    rawtbl <- tbl(con, "raw_dummy_monthly")
    dtf2 <- addunit2tbl(con, maintbl = rawtbl, tableunit = "vld_dummy_unit") %>%
        collect()
    expect_equal(dtf2$unitcode, c("A", "A", "A", "B", "B", "A", NA))


    # Return an error if the number of flows grows during the addunit process
    # There shouldn't be more than one unit at a time for a given product
    # in other words, units are unique through time
    # Add same unit data again
    RMySQL::dbWriteTable(con, "vld_dummy_unit", vld_dummy_unit,
                         row.names = FALSE, append = TRUE)
    expect_error(addunit2tbl(con, maintbl = rawtbl, tableunit = "vld_dummy_unit") ,
                 "more than one unit for a period")
})


# Remove dummy tables from the test database
# In case of test failures, you can comment these line out
# to investigate what was present in the dummy database tables.
if (RMySQL::mysqlHasDefault()){
    # Collect a list of drop statements
    tables2delete <- grep("DROP TABLE IF EXISTS",readLines(dummysql),value=TRUE)
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "test")
    # Send drop statements one by one to the database engine
    res <- sapply(tables2delete, 
                  function(dropstatement){
                      RMySQL::dbSendQuery(con, dropstatement)
                  })
    # Clear all results, to avoid the warning "Closing open result sets"
    sapply(res, RMySQL::dbClearResult)
    # RMySQL::dbRemoveTable(con, "raw_dummy_code2") # another way to remove a table
    RMySQL::dbDisconnect(con)
}

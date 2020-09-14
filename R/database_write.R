#' Write Comext data to the database
#'
#' @description \code{writedat2db} reads data from a dat file,
#' and writes it into a database table.
#' The connection to the database is oppened
#' with a username and password provided in the "tradeflows" group of the
#' ~/.my.cnf file by default.
#' This function was updated to consider zeros 0 and empty strings ""
#' as missing values, NULL in the database.
#' The updated used an SQL query to insert the data, instead of RMariaDB::dbWriteTable()
#' There was an "permission denied" issue when loading files
#' a solution is provided here:
#' https://stackoverflow.com/questions/3471474/mysql-load-data-infile-cant-get-stat-of-file-errcode-2
#' Specify LOCAL in LOAD DATA infile, so that it becomes LOAD DATA LOCAL infile.
#' @param RMariaDBcon database connection object created by RMariaDB::\link{dbConnect}
#' @param datfile character path to the .dat file
#' @param productcodestart numeric begining of a comext product code
#' @param tablename character name of the database table where the data will be inserted
#' @param file.types character vector of named SQL field types (see RMariaDB::\link{dbWriteTable})
#' @param sep field separator character in the input file (see RMariaDB::\link{dbWriteTable})
#' @examples \dontrun{ # Load data from a .dat file into the database
#' # Create a database connection
#' con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "test")
#' # Create a temporary dat file to be written to the database
#' datfile <- tempfile(fileext = ".dat")
#' write(c("col1,col2\n1,2"),datfile)
#' readLines(datfile)
#' # Write the temporary dat file to the database
#' writedat2db(datfile = datfile, tablename = "temporary", RMariaDBcon = con)
#' # Disconnect from the database
#' RMariaDB::dbDisconnect(con)
#' }
#' @seealso \code{\link{transfer7zfile2db}} which transfers data
#' from a compressed .7z file to the database.
#' @export
writedat2db <- function(RMariaDBcon, datfile, tablename, sep = ","){
    datfile <- normalizePath(datfile)
    sqlquery <- sprintf("
        LOAD DATA LOCAL infile '%s'
        INTO TABLE %s
        fields terminated BY '%s'
        lines terminated BY '\\n'
        IGNORE 1 LINES
        (reportercode,reporteriso,partnercode,partneriso,tradetype,productcode,productsitc,productcpa2002,productcpa2008,productcpa21,productbec,productsection,flowcode,statregime,unitcode,period,@tradevalue,@weight,@quantity)
        SET
        tradevalue = nullif(@tradevalue,0),
        weight = nullif(@weight,0),
        quantity = nullif(@quantity,0)
        ;", datfile, tablename, sep)
    # The query sent through RMariaDB::dbSendQuery() causes an error on the server
    # "Lost connection to MySQL server during query "
    # result <- RMariaDB::dbSendQuery(RMariaDBcon, sqlquery)
    # on.exit(rm(result)) # Try to avoid the message "closing open result set
    #
    # Replace RMariaDB::dbSendQuery() by a system() call piping the sql query to the mysql client.
    dbname <- RMariaDB::dbGetInfo(RMariaDBcon)$dbname
    system(sprintf('echo "%s"| mysql %s', sqlquery, dbname))
}


#' @description \code{writetxtcodes2db} reads country codes from
#' REPORTER.txt and PARTNER.txt files and writes them to the database.
#' It doesn't erases previous codes you have to run
#' eutradeflows::createdbstructure first to get a fresh database with empty tables.
#' @examples \dontrun{ # Load product codes and country codes into the database
#' con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "test")
#' # Write product codes to the database
#' writetxtcodes2db(con, "raw_comext_product", "~/R/tradeharvester/data_raw/comext/201707/text/english/CN.txt",
#'               columnnames =  c("productcode", "datestart", "dateend",
#'                                "productdescription", "datestart2", "dateend2"))
#' # Write reporter codes to the database
#' writetxtcodes2db(con, "raw_comext_reporter", "~/R/tradeharvester/data_raw/comext/201707/text/english/REPORTERS.txt",
#'               columnnames = c("reportercode", "datestart", "dateend",
#'                               "reporter", "datestart2", "dateend2"))
#' # Write partner codes to the database
#' writetxtcodes2db(con, "raw_comext_partner", "~/R/tradeharvester/data_raw/comext/201707/text/english/PARTNERS.txt",
#'               columnnames = c("partnercode", "datestart", "dateend",
#'                               "partner", "datestart2", "dateend2"))
#' # Write units to the database
#'
#' # Write unit descriptions to the database
#'
#'
#' RMariaDB::dbDisconnect(con)
#' }
#' @rdname writedat2db
#' @export
writetxtcodes2db <- function(RMariaDBcon, tablename, codesfile, columnnames){
    require(dplyr)
    # This function cannot use  RMariaDB::dbWriteTable with overwrite = TRUE
    # because this would also overwrites the field types and indexes.
    # dbWriteTable chooses default types that are not optimal,
    # for example, it changes date fields to text fields.
    # Therefore use RMariaDB::dbWriteTable with append = TRUE,
    # but first check if the table is empty
    # and if it is not empty, ask to recreate the database
    # structure with empty tables.
    createdbstructuremessage <- sprintf("You can create a database structure with empty tables with the function:\neutradeflows::createdbstructure(sqlfile = 'raw_comext.sql', dbname = '%s')",
                                        RMariaDB::dbGetInfo(RMariaDBcon)$dbname)
    tryCatch({
        res <- RMariaDB::dbSendQuery(RMariaDBcon, sprintf("SELECT COUNT(*) as nrow FROM %s;",tablename))
        sqltable <- RMariaDB::dbFetch(res)
        RMariaDB::dbClearResult(res)
        # Check if the table is empty
        if(sqltable$nrow > 0){
            stop("Table ", tablename, " is not empty.\n")
        }
    }, error = function(errorcondition){
        stop(toString(errorcondition), "\n", createdbstructuremessage)
    }, warning = function(warningcondition){
        message(toString(warningcondition), "\n", createdbstructuremessage)
    })

    # Load codes file
    dtf <- read.delim(codesfile,
                      header = FALSE,
                      quote = "", # disable quotation
                      stringsAsFactors = FALSE)
    # Check if the number of column names supplied matches the number of dtf columns
    if(!identical(ncol(dtf), length(columnnames))){
        message("Strucure of the input data in\n", codesfile)
        str(dtf)
        stop("The number of column names given does not match with the codes file. ",
             "The codes file has ", ncol(dtf), " columns, you supplied ",
             length(columnnames), " column names:\n",
             paste(columnnames, collapse = ", "))
    }
    # Rename columns
    names(dtf) <- columnnames

    # Parse dates in the day/month/year format
    dtf$datestart <- lubridate::dmy(dtf$datestart)
    dtf$dateend   <- lubridate::dmy(dtf$dateend)
    if("datestart2" %in% names(dtf)){
        dtf$datestart2 <- lubridate::dmy(dtf$datestart2)
        dtf$dateend2   <- lubridate::dmy(dtf$dateend2)
    }

    # Fix issues in product codes
    if("productdescription" %in% names(dtf)){
        regexpproducts2harvest <-  paste0("^", tradeharvester::products2harvest$productcode, collapse="|")
        dtf <- dtf %>%
            # Keep only harvested product codes
            # This is deactivated and we now copy all product codes
            # filter(grepl(regexpproducts2harvest, productcode)) %>%
            # Correct encoding issue, see docs/productcodes.Rmd
            mutate(productdescription = gsub(", MA.*ARANDUBA", ", MACARANDUBA",
                                             productdescription))
        # Remove misterious liben column
        dtf$liben_to_delete <- NULL
    }

    # Fix issues in partner codes
    if("partnercode" %in% names(dtf)){
        # Remove reporter codes which contain a character
        # i.e. keep only integer partner codes.
        selector <- !is.na(suppressWarnings(as.integer(dtf$partnercode)))
        dtf <- dtf[selector,]
    }

    # Write to the database
    RMariaDB::dbWriteTable(RMariaDBcon, tablename, dtf,
                         append = TRUE,
                         row.names = FALSE)
}


#' Transfer data from .7z files or text files to the database
#' @description \code{transfer7zfile2db} transfers data from a .7z file to the database.
#' @details \code{transfer7zfile2db} is a chain of several functions that:
#' \itemize{
#'  \item{}{Decompress comext file .7z file into .dat file in a temporary directory}
#'  \item{}{filter .dat file, keep only products starting by a certain code, such as "44"}
#'  \item{}{rename columns from comext to efi column names}
#'  \item{}{write file content to a database table}
#'  \item{}{delete decompressed version of the comext file}
#'  \item{}{(delete temporary directory)}
#' }
#' @param RMariaDBcon database connection object created by RMariaDB::\link{dbConnect}
#' @param file7z character path to a .7z file from Comext
#' @param productcodestart numeric vector of product codes. These products and
#' all their sub-products will be transfered to the database.
#' \itemize{
#'  \item{}{\code{transfer7zfile2db} transfers only one product code}
#'  \item{}{\code{transfer7zfolder2db} can transfer a vector of product codes}
#'  }
#' @param tablename character name of a database table
#' @seealso \code{\link{writedat2db}}
#' @examples \dontrun{ # Transfer data from a 7z file to the database
#' # Create a database connection
#' con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "test")
#' # Transfer data for product codes 44 and 94 from a 7z file to a database table
#' transfer7zfile2db(con, "data_raw/comext/201706/data/nc201501.7z", c(44,94), "raw_comext_monthly_201706")
#'
#' # Transfer data from many .7z files to the database
#' # decompose
#' harvestrecent(con, "/tmp",c(44,94))
#' # in several steps.
#' # Download the latest comext data
#' comextfiles <- downloadcomextmonthlyrecent("/tmp")
#' comextfolder <- unique(comextfiles$folder)
#' # Create a table name where this data will be saved
#' tablename <- paste0("raw_comext_monthly_",
#'                    scrapcomextfoldername(format(Sys.Date(),"\\[%Y")))
#' # Transfer recent monthly data to the database
#' transfer7zfolder2db(con,
#'                     rawdatacomextfolder = file.path("/tmp",comextfolder),
#'                     productcodestart = c(44, 94),
#'                     tablename = tablename,
#'                     tabletemplate = "raw_comext_monthly_template")
#'
#' # Transfer monthly archive to the database
#' transfer7zfolder2db(con,
#'                     rawdatacomextfolder = file.path("data_raw/comext/2016S1/data"),
#'                     productcodestart = c(44, 94),
#'                     tablename = "raw_comext_monthly_2016S1",
#'                     tabletemplate = "raw_comext_monthly_template")
#'
#' # Transfer yearly archive to the database
#' # This time the template is raw_comext yearly
#' transfer7zfolder2db(con,
#'                     rawdatacomextfolder = file.path("data_raw/comext/2016S2/data"),
#'                     productcodestart = c(44, 94),
#'                     tablename = "raw_comext_yearly_2016S2",
#'                     tabletemplate = "raw_comext_yearly_template")
#'
#' # Transfer product codes and country codes to the database
#' transfertxtcodesfolder2db(con,
#'                           rawdatacomextfolder = "data_raw/comext/201707/text/english/")
#'
#' # Disconnect from the database
#' RMariaDB::dbDisconnect(con)
#' }
#' @export
transfer7zfile2db <- function(RMariaDBcon, file7z, productcodestart, tablename){
    message("Transfering products ", paste(productcodestart,collapse = ", "),
            " from ", file7z,
            " to the database table ", tablename, ".")
    # Copy the 7z archive to a  temporary directory
    decompressdir <- tempdir()
    file.copy(file7z, decompressdir, overwrite = TRUE)
    tempfile7z <- file.path(decompressdir, basename(file7z))
    # Decompress the 7z archive
    datfilename <- decompress7z(tempfile7z)
    if(is.null(datfilename)){
        stop("No file in the archive, an error was written to the log file.")
    }
    # Transfer products one by one to the database
    message("Filtering products ", paste(productcodestart, collapse =", "),
            " from the file\n", datfilename,
            "\nand transfering them to the database table ", tablename)
    for (p in productcodestart){
        # Prepare the dat file
        datprepared <- filterandrenamedat(datfilename, p)
        # transfer to the database
        writedat2db(datfile = datprepared, tablename = tablename,
                    RMariaDBcon = RMariaDBcon)
    }
    # Delete the large decompressed file
    unlink(datfilename)
    # Delete all dat files generated by filterandrenamedat()
    tempfiles <- list.files(decompressdir, full.names = TRUE)
    fileswithoutextension <- gsub(".7z", "", basename(file7z))
    filestodelete <- grep(fileswithoutextension, tempfiles, value = TRUE)
    message("Deleting temporary files:\n", datfilename,
            "\n", paste(filestodelete, collapse="\n"))
    unlink(filestodelete)
}


#' @description \code{transfer7zfolder2db} lists all .7z files available in the
#' local copy of the raw Comext folder and transfers them to a database table.
#' Attention: \code{transfer7zfolder2db} erases all previous content in the database.
#' @param RMariaDBcon database connection object created by RMariaDB::\link{dbConnect}
#' @param rawdatacomextfoler character path of the folder where
#' comext files have been saved localy (usually ends with "[year][month]/data")
#' @param tabletemplate character name of a table template to be copied and
#' renamed to tablename
#' @rdname transfer7zfile2db
#' @export
transfer7zfolder2db <- function(RMariaDBcon,
                                rawdatacomextfolder,
                                productcodestart,
                                tablename,
                                tabletemplate){

    rawdatacomextfolder <- normalizePath(rawdatacomextfolder)

    message("If the database table ",tablename,
            " already exists, all its content will be erased and replaced.")

    # Remove the table if it already exists
    RMariaDB::dbSendQuery(RMariaDBcon, sprintf("DROP TABLE IF EXISTS `%s`;",
                                           tablename))

    # Copy the generic table raw_comext_monthly
    RMariaDB::dbSendQuery(RMariaDBcon, sprintf("CREATE TABLE %s LIKE %s;",
                                     tablename, tabletemplate))

    # list all .7z files in the folder
    filenames <- list.files(rawdatacomextfolder, full.names = TRUE)
    filenames <- filenames[grepl(".7z",filenames)]
    # I could use lapply(X = filename, FUN = transfer7zfile2db),
    # but I'm using a for loop because of the try() statement
    # Transfer each file one by one
    for (filename in filenames){
        # Use a try() statement. Errors will be logged to a file by some function
        # or printed on screen by other functions
        try(transfer7zfile2db(RMariaDBcon = RMariaDBcon,
                              file7z = filename,
                              productcodestart = productcodestart,
                              tablename = tablename))
    }
}


#' @description \code{transfertxtcodesfolder2db} transfers product and country codes to the database
#' @rdname transfer7zfile2db
#' @seealso \code{\link{writetxtcodes2db}}
#' @export
transfertxtcodesfolder2db <- function(RMariaDBcon, rawdatacomextfolder){
    rawdatacomextfolder <- normalizePath(rawdatacomextfolder)
    # Write product codes to the database
    # For information, writetxtcodes2db will only perform the transfer if the table is empty
    writetxtcodes2db(RMariaDBcon, "raw_comext_product", file.path(rawdatacomextfolder,"CN.txt"),
                     columnnames =  c("productcode", "datestart", "dateend", "liben_to_delete",
                                      "productdescription", "datestart2", "dateend2"))
    # Write units to the database
    # file name changed in December 2019
    writetxtcodes2db(RMariaDBcon, "raw_comext_unit", file.path(rawdatacomextfolder,"CN8-SU.txt"),
                     columnnames =  c("productcode", "unitcode", "datestart", "dateend"))
    # Write unit descriptions to the database
    # file name changed in December 2019
    writetxtcodes2db(RMariaDBcon, "raw_comext_unit_description", file.path(rawdatacomextfolder,"SU.txt"),
                     columnnames =  c("unitcode", "datestart", "dateend",
                                      "unitdescription", "datestart2", "dateend2"))
    # Write reporter codes to the database
    writetxtcodes2db(RMariaDBcon, "raw_comext_reporter",  file.path(rawdatacomextfolder,"REPORTERS.txt"),
                     columnnames = c("reportercode", "datestart", "dateend",
                                     "reporter", "datestart2", "dateend2"))
    # Write partner codes to the database
    writetxtcodes2db(RMariaDBcon, "raw_comext_partner",  file.path(rawdatacomextfolder,"PARTNERS.txt"),
                     columnnames = c("partnercode", "datestart", "dateend",
                                     "partner", "datestart2", "dateend2"))


}

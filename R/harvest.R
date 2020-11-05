#' Harvest: Download and transfer Comext data to the database
#' @description \code{harvestcomextdata}
#' downloads the most recent (based on the recentyears parameter) comext monthly data and transfer all sub products
#' of the given product codes to the database.
#' The raw comext database structure is recreated each time this function
#' is called.
#' The database table name ends with the name of the most recent comext folder.
#' @param RMariaDBcon database connection object created by RMariaDB::\link{dbConnect}
#' @param rawdatafolder character path to a folder where comext files will be downloaded
#' @param productcodestarts numeric vector of product codes to transfer to the database
#' @param tablename character name of the database table where data will be storred
#' @param tabletemplate character name of the table template giving the data structure
#' @param template character part of the table name to be replaced by the comext folder name
#' @examples \dontrun{
#' # Create a database connection object to be supplied as a parameter RMariaDBcon
#' con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "test")
#' harvestrecent(RMariaDBcon = con, rawdatafolder = "/tmp", productcodestart = c(44,94))
#' harvestmonthlyarchive(RMariaDBcon = con, rawdatafolder = "/tmp", startyear = 2015, productcodestart = c(44,94))
#' harvestyearlyarchive(RMariaDBcon = con, rawdatafolder = "/tmp", startyear = 2015, productcodestart = c(44,94))
#' RMariaDB::dbDisconnect(con)
#'
#' # Harvest creates its own database connection, dbname is passed as a parameter
#' harvest(rawdatafolder = "/tmp", dbname = "test", startyear = 2015, randomsleeptime = 0)
#' harvest(rawdatafolder = "/mnt/sdb/public", dbname = "tradeflows", startyear = 2015, randomsleeptime = 3)
#' }
#' @rdname harvest
#' @export
harvestcomextdata <- function(RMariaDBcon,
                               rawdatafolder,
                               productcodestart,
                               tabletemplate = "raw_comext_monthly_template",
                               tablemonthly = "raw_comext_monthly",
                               tableyearly = "raw_comext_yearly",
                               recentyears = 4,
                               template = getOption("tradeharvester")$template){
    # Create the empty database structure
    eutradeflows::createdbstructure(sqlfile = "raw_comext.sql",
                                    # Get dbname from the RMariaDBcon connection object
                                    dbname = RMariaDB::dbGetInfo(RMariaDBcon)$dbname)

    # Download recent comext .7z data files
    comextfiles <- downloadcomextmonthlyrecent(rawdatafolder, recentyears = recentyears)

    # On the Comext website,
    # Monthly and yearly data files are mixed in the same "/COMEXT_DATA/PRODUCTS" folder
    # Move yearly files to a separate "PRODUCTS_YEARLY" folder, above in the hierarchy
    comextfolder <- unique(comextfiles$folder)
    foldermonthly <- file.path(rawdatafolder, comextfolder)
    folderyearly <- gsub("PRODUCTS", "PRODUCTS_YEARLY", foldermonthly)
    downloadedcomextfiles <- list.files(foldermonthly, full.names = TRUE)
    # Create yearly folder if it doesn't exist
    if (!isTRUE(file.info(folderyearly)$isdir)) dir.create(folderyearly, recursive=TRUE)
    # filter yearly files
    downloadedcomextyearly <- downloadedcomextfiles[substr(downloadedcomextfiles, nchar(downloadedcomextfiles)-4,nchar(downloadedcomextfiles)-3) > 20]
    # Move yearly files to a new folder
    file.rename(from = downloadedcomextyearly,
                to = gsub("PRODUCTS", "PRODUCTS_YEARLY", downloadedcomextyearly))

    # Transfer monthly .7z data files to the database
    stopifnot(identical(length(comextfolder),1L)) # There should be only one
    message("Transfering monthly data from ", comextfolder,
            " to the database table name: ", tablemonthly)
    transfer7zfolder2db(RMariaDBcon = RMariaDBcon,
                        rawdatacomextfolder = foldermonthly,
                        productcodestart = productcodestart,
                        tablename = tablemonthly,
                        tabletemplate = tabletemplate)

    # Transfer yearly .7z data files to the database
    message("Transfering yearly data from ", comextfolder,
            "to the database table name: ", tablename)
    transfer7zfolder2db(RMariaDBcon = RMariaDBcon,
                        rawdatacomextfolder = folderyearly,
                        productcodestart = productcodestart,
                        tablename = tableyearly,
                        tabletemplate = tabletemplate)
}

#' @export
#' @rdname harvest
harvestcomextmetadata <- function(RMariaDBcon,
                                  rawdatafolder,
                                  pause = 0){
    # Uses the database structure created by the harvestcomextdata() function
    # Download recent comext .txt codes files
    comextfilestxt <- downloadcomextmetadata(rawdatafolder,
                                             comextfolderpath = getOption("comext")["metadatafolder"],
                                             extension = ".txt",
                                             pause = pause)
    comextfoldertxt <- unique(comextfilestxt$folder)

    # Transfer .txt codes files to the database
    transfertxtcodesfolder2db(RMariaDBcon = RMariaDBcon,
                              rawdatacomextfolder = file.path(rawdatafolder,comextfoldertxt))
}



#' Deprecated harvest recent data
#'  @export
harvestrecent <- function(){
    .Deprecated(new = "harvestcomextdata", package = "tradeharvester",
                msg = "'harvestrecent' is deprecated. Use 'harvestcomextdata' instead.
see example use in help('harvestcomextdata')
and a list of deprecated functions in help('tradeharvester-deprecated')")
}



#' @description \code{harvestmonthlyarchive}
#' Downloads archive data from comext and transfer all sub products
#' of the given product codes to the database.
#' The structure of the table template `raw_comext_monthly`
#' should already be present in the database.
#' The database structure is regularly recreated in \code{harvestrecent},
#' so there is no need to recreate it \code{harvestmonhtlyarchive}.
#' This function can be run on a yearly basis when new archives become available.
#' @rdname harvestrecent
#' @export
harvestmonthlyarchive <- function(...,
                                  rawdatafolder,
                                  startyear,
                                  tabletemplate = "raw_comext_monthly_template",
                                  template = getOption("tradeharvester")$template){
    .Deprecated(new = "harvestmonthlyarchive", package = "tradeharvester",
                msg = "'harvestmonthlyarchive' is deprecated. Use 'harvestcomextdata' instead.
see example use in help('harvestcomextdata')
and a list of deprecated functions in help('tradeharvester-deprecated')")
        # Download monthly archive files
        monthlyarchivefiles <- downloadcomextmonthlyarchive(startyear = startyear,
                                                            rawdatafolder = rawdatafolder)
        monthlyarchivefolder <- unique(monthlyarchivefiles$folder)

        # Transfer monthly archive .7z data files to the database
        tablename <- gsub(template, unique(monthlyarchivefiles$comextfoldername),
                          tabletemplate)
        message("tablename: ",tablename,
                "\nmonthlyarchivefolder: ", monthlyarchivefolder)
        transfer7zfolder2db(...,
                            rawdatacomextfolder = file.path(rawdatafolder, monthlyarchivefolder),
                            tablename = tablename,
                            tabletemplate = tabletemplate)
}


#' @description \code{harvestyearlyarchive}
#' downloads yearly archive and transfer it to the database.
#' The structure of the database table `raw_comext_yearly`
#' should already present in the database.
#' @param ... parameters passed to \code{\link{transfer7zfolder2db}}
#' @rdname harvestrecent
#' @export
harvestyearlyarchive <- function(...,
                                 rawdatafolder,
                                 startyear,
                                 tabletemplate = "raw_comext_yearly_template",
                                 template = getOption("tradeharvester")$template){
    .Deprecated(new = "harvestyearlyarchive", package = "tradeharvester",
                msg = "'harvestyearlyarchive' is deprecated. Use 'harvestcomextdata' instead.
see example use in help('harvestcomextdata')
and a list of deprecated functions in help('tradeharvester-deprecated')")
        # Download yearly archive files
        yearlyarchivefiles <- downloadcomextyearlyarchive(startyear = startyear,
                                                          rawdatafolder = rawdatafolder)
        yearlyarchivefolder <- unique(yearlyarchivefiles$folder)

        # Transfer yearly archive .7z data files to the database
        tablename <- gsub(template, unique(yearlyarchivefiles$comextfoldername),
                          tabletemplate)
        message("tablename: ",tablename,
                "\nyearlyarchivefolder: ", yearlyarchivefolder)
        transfer7zfolder2db(...,
                            rawdatacomextfolder = file.path(rawdatafolder,yearlyarchivefolder),
                            tablename = tablename,
                            tabletemplate = tabletemplate)
}


#' @description \code{harvest} checks for updates in the Comext bulk download
#' repository and downloads recent data if it's not yet present in the database.
#' If recent data has been updated, also check for updates in the archive data and
#' download accordingly.
#' @param logfile character path to the main log file.
#' The main log file is not to be confused with standard output and standard error of
#' Rscript which can also be sent to a lof file, see more info in the details below.
#' @param randomsleeptime numeric maximum number of seconds to wait before harvesting
#' @details The \code{harvest()} function extracts [year] and [month] from
#' the raw_comext_monthly_[year][month],
#' raw_comext_monthly_[year]S1 and raw_comext_yearly_[year]S2 tables to
#' compare them with the names of the most recent comext folder,
#' S1 and S2 folders.
#' If the most recent comext data is not present in the database,
#' this function will harvest it and then if the archive folders are not present,
#' it will harvest them as well.
#' To run \code{harvest} periodically as a cron job, edit crontab:
#'
#' \code{sudo vim /etc/crontab}
#'
#' and enter:
#'
#' \code{
#' 0 3 * * *    debian  Rscript -e "library(tradeharvester); harvest(rawdatafolder = '/mnt/sdb/public', dbname = 'tradeflows', startyear = 2000)" >> ~/log/harvest$(date +"\\\%Y\\\%m\\\%d").log 2>&1
#' }
#'
#' As explained in \link{https://serverfault.com/questions/117360/sending-cron-output-to-a-file-with-a-timestamp-in-its-name}
#' make sure to escape any \% with \\\%.
#'
#' To keep a detailed log of the harvesting process,
#' this cron tab entry writes standard errors and standard output to a file.
#' It was inspired by this StackoverFlow question:
#' \link{https://stackoverflow.com/questions/14008139/capturing-rscript-errors-in-an-output-file}.
#' You can follow the harvest in progress in that log file with \code{tail -f harvestlogfilename.log}.
#' The main log file given as the function parameter \code{logfile} will only contain the date
#' and folder name of major updates.
#'
#' @seealso \code{\link{crontime}}, a function that tests if a cron job is working as expected.
#' @export
harvest <- function(rawdatafolder,
                    dbname,
                    startyear,
                    productcodestart = tradeharvester::products2harvest$productcode,
                    tabletemplatemonthly = "raw_comext_monthly_template",
                    template = getOption("tradeharvester")$template,
                    logfile = paste0('/mnt/sdb/public/log/harvest', format(Sys.Date(), '%Y'),'.txt'),
                    randomsleeptime = 3600,
                    recentyears = 4){
    require(dplyr)

    # Wait a random number of seconds < randomsleeptime before starting to harvest
    Sys.sleep(runif(1, 0, randomsleeptime))
    message("\n\nStarting to harvest on ",
            format(Sys.time(),"%Y.%m.%d at %H:%M"),"\n")

    # Connect to the database
    con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = dbname)

    # Find the most recent period [year][month] in the database table raw_comext_monthly
    dbmaxperiod <- 0 # if the table doesn't exist, the default value will be zero
    try({DBtablemonthly <- tbl(con, sql("SELECT max(period) as maxperiod from raw_comext_monthly")) %>%
        collect()
    dbmaxperiod <- DBtablemonthly$maxperiod
    }
    )


    # Find the most recent file period [year][month] on the Comext data repository
    comextfiles <- scraplistoffilesincomextfolder(comextfolderpath = getOption("comext")["datafolder"]) %>%
        # Extract year and month information from the file name
        mutate(year = as.integer(substr(file,5,8)),
               month = as.integer(substr(file,9,10)),
               period = as.integer(year * 100 + month))
    comextmaxperiod <- max(comextfiles$period)

    # Compare name of recent data folder and download if necessary
    # If most recent [year][month] are the same on the server and in the database
    if(identical(dbmaxperiod, comextmaxperiod)){
        # Just write a message
        message("Data was already harvested. The most recent comext data file `",
                comextfiles$file[comextfiles$period == comextmaxperiod],
                "` matches the most recent period available in the database: `",
                dbmaxperiod,"`.")
        # Add a dot to the main logfile,
        # showing that this function did check for updates on Comext.
        tradeflows::adddot2logfile(logfile)

    } else {
        # Write to the log file
        write(paste(as.character(Sys.time()),
                    "\nDownloading recent Comext files from the",
                    getOption("comext")["datafolder"], "folder.\n"),
              logfile, append = TRUE)
        message("Recent data has not yet been harvested.")
        # Harvest recent data
        harvestcomextdata(RMariaDBcon = con,
                          rawdatafolder = rawdatafolder,
                          productcodestart = productcodestart,
                          recentyears = recentyears)

        # Harvest metadata
        harvestcomextmetadata(RMariaDBcon = con,
                              rawdatafolder = rawdatafolder,
                              pause = 0)
    }
    message("\nHarvest completed on ", format(Sys.time(),"%Y.%m.%d at %H:%M"))

    try({ # Dump all raw tables to the rawdatafolder
        rawtables <- RMariaDB::dbListTables(con)
        rawtables <- rawtables[grepl("^raw", rawtables)]
        lapply(rawtables,
               function(tablename) eutradeflows::dumptable("tradeflows", tablename, dumpfolder = file.path(rawdatafolder, "sqldump/")))
        message("\nDatabase dump completed on ", format(Sys.time(),"%Y.%m.%d at %H:%M"))
    })

    # Disconnect from the database
    RMariaDB::dbDisconnect(con)
}


#' Sets comext url connection parameters as a global R option comext
#'
#' Comext url parameters
#' Are storred in the installed version of the package
#' Run this command to find the location of the configuration file on your machine:
#' system.file("config/comextconfig.R", package="tradeharvester")
#' Use reload=TRUE to force reloading the file after modification.
#' @param reload logical, force reloading the configuration file
#' @param silent logical, do not print "already loaded" message when TRUE
#' @examples
#' setcomextconfig(reload=TRUE)
#' comext <- getOption("comext")
#' comext["urlbulkfile"]
#' @export
setcomextconfig <- function(reload = FALSE, silent = FALSE){
    if(is.null(getOption("comext"))|reload){
        # Path to the configuration file
    configfile <- system.file("config/comextconfig.R", package="tradeharvester",
                              mustWork = TRUE)
        message(paste("Loading comext url configuration from\n", configfile))
        source(configfile)
    } else {
        if (!silent){
            message("Comext configuration file already loaded.")
            message("Use the option reload=TRUE if you want to reload it.")
        }
    }
}


#' Download bulk data from Comext into a designated folder
#'
#' @description Download files from the Comext bulk data repository
#' located at http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext
#' Make sure that the output of paste0(comextfolder, comextfile) converted to html characters
#' returns an URL that has a matching file name on the bulk download repository.
#'
#' If the file doesn't exist, Comext will still return a page,
#' with a table containing the following message:
#' "File **** does not exist or is not readable on the server".
#' In this case, the download status will be 0 (sucess) even though
#' the file failed to download.
#' @param comextfolder name of the folder starting with "comext/"
#' @param comextfile name of the file in the comext platform
#' @param rawdatafolder folder where the raw data will be storred
#' @param logfile path to a log file, located in the user directory by default
#' @param pause numeric pause time in seconds before downloading the file (usefull for multiple downloads)
#' @param method, see \code{\link{download.file}}
#' @details Pause time was introduced because download returned an
#' error status when downloadingmany files in a row.
#' Pause time can be decided individually in the various functions that call
#' downloadcomextfile, usually a few seconds to a few minutes should be enough.
#' @examples \dontrun{
#' # This example will get outdated as time goes by
#' # Check the bulk download repository at
#' # http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext
#' # for the current name of the comext most recent data folder
#' # Both should work,
#' # with trailing slash in the folder name
#' downloadcomextfile("comext/201706/data/", "nc201701.7z", "/tmp")
#' # without trailing slash in the folder name
#' downloadcomextfile("comext/201706/data", "nc201702.7z", "/tmp")
#' # Test error logging by downloading empty file in empty folder
#' downloadcomextfile("", "", "/tmp", logfile = "~/comextlog.txt")
#' }
#' @return \code{downloadcomextfile} Returns the download status, see \code{\link{download.file}}
#' @export
downloadcomextfile <- function(comextfolder, comextfile, rawdatafolder,
                               pause = 0,
                               logfile = file.path("~/log", "harvesterrorlog.txt"),
                               method = "libcurl"){
    # Load configuration file containing the comext bulk repository URL.
    setcomextconfig(silent = TRUE)
    comext <- getOption("comext")
    # Prepare the download url
    folderandfilename <- file.path(comextfolder, comextfile)
    # What if comextfolder contained a trailing double // you could replace it with / using:
    # gsub("//","/",folderandfilename)
    comexturl <- paste0(comext["urlbulkfile"], URLencode(folderandfilename, reserved = TRUE))
    # If needed, create the local file path where to save the data
    if (!file.exists(file.path(rawdatafolder, comextfolder))){
        dir.create(file.path(rawdatafolder, comextfolder), recursive = TRUE)
    }
    # If asked for, pause for some time before downloading the file
    # usefull only for multiple downloads, to avoid server overload
    if(pause>0){
        message("Waiting ", pause, " seconds before the download starts ..." )
        Sys.sleep(pause)
    }
    # Download and save the file
    downloadstatus <- 1
    tryCatch({
        # Store the download status returned by download.file
        downloadstatus <- download.file(url = comexturl,
                                        destfile = file.path(rawdatafolder, folderandfilename),
                                        method = method)

    }, error = function(errorcondition){
        tradeflows::writeerror2log(errorcondition, logfile)
    }, warning = function(warningcondition){
        tradeflows::writeerror2log(warningcondition, logfile)
    }
    )
    invisible(downloadstatus)
}


#' Generate all monthly file names from startyear until endyear
#' @param startyear numeric or character, a start year
#' @param endyear numeric or character, an end year, defaults to the current year
#' @param extension character vector of length one only, a file extension
#' @return a vector of file names
generatemonthlyfilenames <- function(startyear,
                                     endyear = as.integer(format(Sys.Date(), "%Y")),
                                     extension = ".7z"){
    sprintf("nc%i%02i%s",
            rep(startyear:endyear, each=12),
            rep(1:12,2),
            extension)
}


#' Generate yearly file names
#' @rdname generatemonthlyfilenames
generateyearlyfilenames <- function(startyear,
                                    endyear = as.integer(format(Sys.Date(), "%Y")),
                                    extension = ".7z"){
    sprintf("nc%i52%s", startyear:endyear, extension)
}


#' \code{downloadcomextmonthlyrecent} loads all recent monthly data from comext.
#' @rdname downloadcomextfile
#' @param recentyears numeric number of years, will only download most recent files for
#' the given number of years. Use a large number (>20) to load all data the first time.
#' @description
#' If the destination folder is empty, download all files.
#' Otherwise, if there are files in the destination folder,
#' download only data from the past recentyears years.
#' @examples \dontrun{
#' # Downloads all recent .7z data files into the /tmp folder
#' # and returns a dataframe with a status column
#' # describing the status of the download for each file
#' dtf <- downloadcomextmonthlyrecent("/tmp")
#' dtf$status
#' # Download all recent .txt description files
#' # describing products and reporting countries into the /tmp folder
#' dtf2 <- downloadcomextmonthlyrecent("/tmp", subfolder = "text/english", extension = ".txt")
#' }
#' @export
#' @rdname downloadcomextfile
#' @return \code{downloadcomextmonthlyrecent} returns a data frame
#' of file names and paths with their download status.
downloadcomextmonthlyrecent <- function(rawdatafolder,
                                        comextfolderpath = "/COMEXT_DATA/PRODUCTS",
                                        extension = ".7z",
                                        recentyears = 4,
                                        pause = 10){
    comextfiles <- scraplistoffilesincomextfolder(comextfolderpath = comextfolderpath,
                                                  extension = extension) %>%
        # Extract year and month information from the file name
        mutate(year = as.numeric(substr(file,5,8)),
               month = as.numeric(substr(file,9,10)))
    # Make sure the file name is unique
    stopifnot(identical(nrow(comextfiles), length(unique(comextfiles$file))))

    if(length(list.files(file.path(rawdatafolder, unique(comextfiles$folder))))){
        # If there are files in the destination folder
        # Download only data from the past recentyears years
        comextfilesrecent <- comextfiles %>%
            filter(year >= as.numeric(format(Sys.time(), "%Y")) - recentyears)
    } else {
        sprintf("Destination folder %s is empty.",
                file.path(rawdatafolder, unique(comextfiles$folder)))
        # Download all files
        comextfilesrecent <- comextfiles
    }

    message("Downloading ", nrow(comextfilesrecent), " files from ",
            unique(comextfiles$folder)," into ", rawdatafolder )
    # Download files
    comextfiles <- comextfilesrecent %>%
        # group by file, keep folder and comextfoldername in the output dataframe
        group_by(file, folder, comextfolderpath) %>%
        do(status = downloadcomextfile(.$folder, .$file,
                                       rawdatafolder, pause = pause))
    return(comextfiles)
}


#' @export
#' @rdname downloadcomextfile
#' @return \code{downloadcomextmonthlyrecent} returns a data frame
#' of file names and paths with their download status.
#' @examples \dontrun{
#' downloadcomextmetadata(rawdatafolder = "/tmp", pause = 0)
#' }
downloadcomextmetadata <- function(rawdatafolder,
                                   comextfolderpath = "/COMEXT_METADATA/CLASSIFICATIONS_AND_RELATIONS/ENGLISH",
                                   extension = ".txt",
                                   pause = 10){
    comextfiles <- scraplistoffilesincomextfolder(comextfolderpath = comextfolderpath,
                                                  extension = extension)
    # Make sure the file name is unique
    stopifnot(identical(nrow(comextfiles), length(unique(comextfiles$file))))

    message("Downloading ", nrow(comextfiles), " files from ",
            unique(comextfiles$folder)," into ", rawdatafolder )

    # Download files
    comextfiles <- comextfiles %>%
        # group by file, keep folder and comextfoldername in the output dataframe
        group_by(file, folder, comextfolderpath) %>%
        do(status = downloadcomextfile(.$folder, .$file,
                                       rawdatafolder, pause = pause))
    return(comextfiles)
}




#' \code{downloadcomextmonthlyarchive} loads all archive data from a given start year.
#' @rdname downloadcomextfile
#' @param startyear numeric download files from that year onwards
#' @param pattern character pattern of the folder name, see \code{\link{scrapcomextfoldername}}
#' @examples \dontrun{
#' # download monthly archive from 2000
#' downloadcomextmonthlyarchive(startyear = 2000, rawdatafolder = "/tmp")
#' }
#' @export
downloadcomextmonthlyarchive <- function(startyear,
                                         rawdatafolder,
                                         pause = 60,
                                         pattern = "S1\\]",
                                         extension = ".7z"){
    .Deprecated(new = "harvestcomextdata", package = "tradeharvester",
                msg = "'harvestrecent' is deprecated. Use 'harvestcomextdata' instead.
see example use in help('harvestcomextdata')
and a list of deprecated functions in help('tradeharvester-deprecated')")

    require(dplyr)
    # File names from start year
    fromstartyear <- data_frame(file = generatemonthlyfilenames(startyear = startyear,
                                                                extension = extension))
    # Load list of files from Comext folder containing the given pattern
    comextfiles <- scraplistoffilesincomextfolder(pattern, extension = extension) %>%
        # Keep only file names after startyear
        inner_join(fromstartyear, by = "file")
    message("Downloading ", nrow(comextfiles), " files from ",
            unique(comextfiles$folder)," into ", rawdatafolder )
    # Make sure file name is unique
    stopifnot(identical(nrow(comextfiles),length(unique(comextfiles$file))))
    comextfiles %>%
        # group by file, keep folder and comextfoldername in the output dataframe
        group_by(file, folder, comextfoldername) %>%
        do(status = downloadcomextfile(.$folder, .$file,
                                       rawdatafolder,
                                       pause = pause))
    return(comextfiles)
}


#' \code{downloadcomextyearlyarchive} loads all archive data from a given start year.
#' @rdname downloadcomextfile
#' @examples \dontrun{
#' # download yearly archive from 2000
#' downloadcomextyearlyarchive(startyear = 2000, rawdatafolder = "/tmp")
#' }
#' @export
downloadcomextyearlyarchive <- function(startyear, rawdatafolder,
                                        pattern = "S2\\]",
                                        extension = ".7z"){
    .Deprecated(new = "harvestcomextdata", package = "tradeharvester",
                msg = "'harvestrecent' is deprecated. Use 'harvestcomextdata' instead.
see example use in help('harvestcomextdata')
and a list of deprecated functions in help('tradeharvester-deprecated')")
    require(dplyr)
    # File names from start year
    fromstartyear <- data_frame(file = generateyearlyfilenames(startyear = startyear,
                                                              extension = extension))
    # Load list of files from Comext folder containing the given pattern
    comextfiles <- scraplistoffilesincomextfolder(pattern, extension = extension) %>%
        # Keep only file names after startyear
        inner_join(fromstartyear, by = "file")
    message("Downloading ", nrow(comextfiles), " files from ",
            unique(comextfiles$folder)," into ", rawdatafolder )
    # Make sure file name is unique
    stopifnot(identical(nrow(comextfiles),length(unique(comextfiles$file))))
    comextfiles <- comextfiles %>%
        # group by file, keep folder and comextfoldername in the output dataframe
        group_by(file, folder, comextfoldername) %>%
        do(status = downloadcomextfile(.$folder, .$file, rawdatafolder))
    return(comextfiles)
}


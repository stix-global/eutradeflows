
#' Decompress a 7z file in its directory
#' @description Overwrite by default, meaning
#' the 7zr switch is set to -y  "Assume Yes on all queries"
#' because there cannot be interaction with the user.
#' @return Invisibly returns a character vector of the filepaths extracted to
#'  (same behaviour as \link{unzip})
#' return NULL if there is an error (the error is then logged to a file)
#' @param file7z a 7z file to be decompressed
#' @param stdout redirects standard output of the 7zr command, argument passed to \link{system2}.
#' defaults to FALSE (discard output)
#' @param logfile path to a log file, located in the user directory by default
#' @export
# TODO: use the archive package to make this operation cross platform.
# https://github.com/jimhester/archive
decompress7z <- function(file7z, stdout = FALSE,
                         logfile = file.path("~/log", "harvesterrorlog.txt")){
    # Make sure it's a full path
    file7zfullpath <- path.expand(file7z)
    message("Decompressing file7zfullpath:\n", file7zfullpath)
    # Change the working directory
    curdir <- getwd()
    setwd(dirname(file7zfullpath))
    on.exit(setwd(curdir))
    content <- NULL
    tryCatch({
        # Extract content from the 7z archive
        system2("7zr", args=c("e", "-y", file7zfullpath), stdout = stdout)

        # Find the name of the file that was just extracted
        # List the content of the 7z archive
        content <- system2("7zr", args=c("l", "-slt", file7zfullpath), stdout = TRUE)
        # Find the Path parameter of the file that is not the archive file
        content <- content[grepl("Path =",content)]
        content <- content[!grepl(".7z",content)]
        content <- gsub("Path = ","",content)
        content <- file.path(dirname(file7zfullpath),content)
    }, error = function(errorcondition){
        tradeflows::writeerror2log(errorcondition, logfile)
    }, warning = function(warningcondition){
        tradeflows::writeerror2log(warningcondition, logfile)
    }
    )
    invisible(content)
}


#' Read Comext .dat file for a given product code
#' @description This function uses the awk program to filter large .dat file
#' before reading them as csv.
#' This is a convenience function for raw data exploration,
#' the transfer to the database itself is performed by another function
#' called \code{\link{filterandrenamedat}}.
#' @seealso write2db for direct transfer of the dat file to the database
#' @param datfile character path to the .dat file
#' @param productcodestart numeric begining of a comext product code
#' @export
readdat <- function(datfile, productcodestart){
    # Awk command to filter for product codes starting with productcodestart
    cmd <- sprintf("awk -F, '$3 ~ /^%s/|| NR==1' %s",
                   productcodestart,
                   datfile)
    # Pipe the command into read.csv
    dtf <- read.csv(pipe(cmd), as.is = TRUE)
    return(dtf)
}


#' Create a shorter .dat file containing only products starting with the given
#' code and rename colmuns to an internal naming scheme.
#' @description
#' Prepare a .dat file for its transfer into the database.
#' See docs/read_comext_files.Rmd for developments that let to this awk command:
#' \code{
#' awk -F, 'NR==1 {print "different column"} $3 ~ /^44/' nc201501.dat|less
#' }
#' @examples \dontrun{
#' # Download a file from the [comext bulk download repository](http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext), then
#' mkdir /tmp/comext
#' then extract the .7z file with the bash command
#' # 7zr e nc201501.7z
#' # then prepare the .dat file with the R command
#' filterandrenamedat("/tmp/comext/nc201501.dat",44)
#' }
#' @description Prepare a dat file for input into the database
#' @param datfile character path to a .dat file
#' @param productcodestart numeric or character "08" product code starting with
#' @return character name of the result file
#' @export
filterandrenamedat <- function(datfile, productcodestart){
    resultfile <- file.path(dirname(datfile),
                            paste0(basename(datfile),productcodestart))
    firstlinefile <- file.path(dirname(datfile), "firstline.txt")
    # Rename colmuns using the naming scheme from the tradeflows package
    firstlineraw <- "DECLARANT,DECLARANT_ISO,PARTNER,PARTNER_ISO,TRADE_TYPE,PRODUCT_NC,PRODUCT_SITC,PRODUCT_CPA2002,PRODUCT_CPA2008,PRODUCT_CPA2_1,PRODUCT_BEC,PRODUCT_SECTION,FLOW,STAT_REGIME,SUPP_UNIT,PERIOD,VALUE_IN_EUROS,QUANTITY_IN_KG,SUP_QUANTITY"
    firstlineprepared <- "reportercode,reporteriso,partnercode,partneriso,tradetype,productcode,productsitc,productcpa2002,productcpa2008,productcpa21,productbec,productsection,flowcode,statregime,unitcode,period,tradevalue,weight,quantity"
    # firstlineraw should be present in the datfile, otherwise renaming doesn't make sense
    # future versions might change this hardcoded behaviour to make it more flexible
    stopifnot(identical(toupper(readLines(datfile, n=1)),  firstlineraw))

    # Awk command that filters product codes starting with productcodestart
    # and renames the first line of the file
    cmd <- sprintf("awk -F, 'NR==1 {print \"%s\"} $6 ~ /^%s/' %s>%s",
                   firstlineprepared, productcodestart,
                   datfile, resultfile)
    system(cmd)
    return(resultfile)
}

#' Compare the md5sum of files in folder 1 and folder2
#' @param folder1 character path to a folder
#' @param folder2 character path to a folder
#' @return a data frame containing file names and a md5equal column
#' which is TRUE if md5sums are equal for the given file and FALSE otherwise.
#' @examples \dontrun{
#' # This example compares files in 2 folders downloaded at one month interval
#' # From the  [MOST_RECENT_COMEXT_DATA] folder in the Comext bulk data repository
#' # Note, since February 2018 the folder doesn't exist anymore on Comext
#' # use the pattern format(Sys.Date(),"\\[%Y") instead
#' dtf <- comparemd5sum("/tmp/comext/201706/data","/tmp/comext/201707/data")
#' dtf$md5equal
#' # Comparing a folder with itself obvisouly returns TRUE
#' obviously <- comparemd5sum("/tmp/comext/201706/data","/tmp/comext/201706/data")
#' obviously$md5equal
#' }
#' @export
comparemd5sum <- function(folder1, folder2){
    md5sum1 <- data_frame(filewithpath1 = list.files(folder1,
                                                     full.names = TRUE)) %>%
        mutate(md5sum1 = tools::md5sum(filewithpath1),
               file = basename(filewithpath1))
    md5sum2 <- data_frame(filewithpath2 = list.files(folder2,
                                                     full.names = TRUE)) %>%
        mutate(md5sum2 = tools::md5sum(filewithpath2),
               file = basename(filewithpath2))
    result <- md5sum1 %>%
        full_join(md5sum2, by="file") %>%
        mutate(md5equal = md5sum1 == md5sum2)
    return(result)
}

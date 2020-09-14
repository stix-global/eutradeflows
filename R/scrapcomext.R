
#' Extract link texts and urls from a web page
#'
#' @description  A group of functions that use \code{rvest::\link{html_nodes}} to extract information
#' from the Eurostat Comext bulk download repository.
#' \code{scraplinks}, the main function, extracts links from a web page.
#' @param url character, an url
#' @return a data frame of link text and urls
#' @examples
#' \dontrun{
#' scraplinks("http://localhost/")
#' glinks <- scraplinks("http://google.com/")
#' }
#' @export
scraplinks <- function(url){
    require(dplyr)
    # Create an html document from the url
    webpage <- xml2::read_html(url)
    # Extract the URLs
    url_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    # Extract the link text
    linktext_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    return(data_frame(linktext = linktext_, url = url_))
}


#' \code{scrapcomextfoldername} finds the name of a Comext folder, based on characters in its link text
#' @return a character vector containing the name of the folder
#' @param pattern character string containing a regular expression, see \code{\link{grepl}}
#' @examples
#' \dontrun{ # Scrap the name of Comext recent and archive folders
#' # Name of the most recent monthly folder
#' scrapcomextfoldername(format(Sys.Date(),"\\[%Y"))
#' # Character escape needed, because "[" and "]" have a special meaning in a regular expression
#' # Name of the monthly data archive folder.
#' scrapcomextfoldername("S1\\]")
#' # Name of the yearly data archive folder
#' scrapcomextfoldername("S2\\]")
#' }
#' @rdname scraplinks
#' @return A comext folder name
#' @export
scrapcomextfoldername <- function(pattern, urlparameter = "dir"){
    require(dplyr, warn.conflicts = FALSE)
    # Load comext url from the package configuration file
    setcomextconfig(silent = TRUE)
    comext <- getOption("comext")
    message("Scraping the following url:\n", comext["urldir"],
            "\nfor the link containing : `", pattern , "`.")
    urlcomext <- scraplinks(comext["urldir"]) %>%
        # Find the link text which contains the pattern
        filter(grepl(pattern, linktext)) %>%
        # Extract the comext file path and folder from the url
        mutate(path = extractfilepath(url, urlparameter = urlparameter),
               folder = basename(path))

    return(urlcomext$folder)
}



#' \code{extractfilepath} extracts the file path from an url parameter
#' @param parameter character the url parameter where the file path is located
#' @return a character vector
#' @examples # Extract the file path form a Eurostat URL
#' eurostat_url_1 <- "http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&downfile=comext%2F201706%2Fdata%2Fnc201702.7z"
#' extractfilepath(eurostat_url_1, "downfile")
#' eurostat_url_2 <- "http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=comext%2F201706%2Fdata%2Fnc201702.7z"
#' extractfilepath(eurostat_url_2, "file")
#' extractfilepath(eurostat_url_2, "nonesense") # returns NA
#'
#' @rdname scraplinks
#' @export
extractfilepath <- function(url, urlparameter = "downfile"){
    URLdecode(RCurl::getFormParams(url)[urlparameter])
}


#' \code{scraplistoffilesincomext} scraps the list of files in a Comext folder
#' The Comext Bulk download repository provides lists of files that can be downloaded.
#' This function lists all files available on a given folder.
#' @param folderurl character url of the comext folder of interest
#' @return a data frame containing folder paths and file names
#' @examples
#' \dontrun{ # List files in the given comext folder
#' # Most recent data folder (url will change through time, this example will break)
#' recentfiles <- scraplistoffilesincomext("http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext%2F201706%2Fdata")
#' str(recentfiles)
#' # Archive folder
#' archive <- scraplistoffilesincomext("http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext%2F2016S1%2Fdata")
#' }
#' @rdname scraplinks
#' @export
scraplistoffilesincomext <- function(folderurl, urlparameter = "downfile"){
    filelinks <- scraplinks(folderurl) %>%
        filter(grepl(urlparameter, url)) %>%
        # Vectorise the extractfilepath() function
        mutate(path = sapply(url, extractfilepath,
                             urlparameter = urlparameter,
                             USE.NAMES = FALSE),
               file = basename(path),
               folder = dirname(path))
    return(filelinks)
}


#' \code{scraplistoffilesincomextfolder} lists all .7z files in a Comext folder
#' which contains the given pattern.
#' The extension can also be changed to something else like ".dat", see examples.
#' @param comextfolderpath path on the comext site (subfolder of the "comext" folder)
#' @param extension character file extension of interest
#' @examples
#' \dontrun{
#' # List files available on the comext metadata page
#' comextmetadata <- scraplistoffilesincomextfolder(comextfolderpath = getOption("comext")["metadatafolder"],
#'                                                  extension = ".txt")
#'
#' # List files available on the comext COMEXT_DATA/PRODUCTS page
#' comextcontent <- scraplistoffilesincomextfolder(comextfolderpath = getOption("comext")["datafolder"]) %>%
#'     # Extract year and month information from the file name
#'     mutate(year = as.numeric(substr(file,5,8)),
#'            month = as.numeric(substr(file,9,10)))
#' comextcontent$file
#' # keep only monhly data
#' comextmonthly <- comextcontent %>%
#'     filter(month < 20)
#' # filter yearly data
#' comextyearly <- comextcontent %>%
#'     filter(month > 20)
#' # keep only monthly data from the past 4 years
#' comextmonthlyrecent <- comextcontent %>%
#'     filter(year > as.numeric(format(Sys.time(), "%Y")) - 5 &
#'                month < 20)
#' }
#' @rdname scraplinks
#' @export
scraplistoffilesincomextfolder <- function(comextfolderpath = getOption("comext")["datafolder"],
                                           extension = ".7z"){
    require(dplyr)
    # Load comext url from the package configuration file
    setcomextconfig(silent = TRUE)
    comext <- getOption("comext")
    # Build Comext URL by adding the encoded comext folder path
    folderurl <- paste0(comext["urldir"],
                        URLencode(comextfolderpath, reserved = TRUE))
    # Scrap files on this url page and keep only files with the given extension
    files <- scraplistoffilesincomext(folderurl) %>%
        filter(grepl(extension, file)) %>%
        # Add comextfolderpath for later use
        mutate(comextfolderpath = comextfolderpath)
    return(files)
}


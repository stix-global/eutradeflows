#' Test a cron job by writing the comext folder name to a text file
#' @description This script waits a random number of seconds,
#' then scraps the most recent comext folder name and the name of the last file in that folder.
#' The ouput is written to a log file.
#' @details To run this script every day at 3:00 am, edit the crontab file
#'
#' \code{
#'     sudo vim /etc/crontab
#' }
#'
#' and enter the following command:
#'
#' \code{
#'     0 2 * * *    debian Rscript -e "tradeharvester::crontime(logfile = '/mnt/sdb/public/log/crontime.txt', randomsleeptime = 3600)"
#' }
#' @param logfile character path to a log file
#' @param randomsleeptime integer maximum number of second to wait before checking the
#' comext website.
#' @details The log file can be located in a publicly accessible web folder.
#' @seealso \code{\link{harvest}}, the main harvesting function run as a cron job.
#' @examples
#' templog <- tempfile(fileext = ".txt")
#' crontime(logfile = templog, randomsleeptime = 0)
#' readLines(templog)
#' @export
crontime <- function(logfile, randomsleeptime){
    require(dplyr)
    # If needed, create the local file path where to save the log file
    if (!file.exists(dirname(logfile))){
        dir.create(dirname(logfile), recursive = TRUE)
    }
    write(paste("script called on", as.character(Sys.time())),
          logfile, append=TRUE)
    # Sleep a random number of second within an hour
    Sys.sleep(runif(1, 0, randomsleeptime))
    write(paste("random sleep time finished on", as.character(Sys.time())),
          logfile, append=TRUE)

    # Find the most recent file period [year][month] on the Comext data repository
    comextfiles <- scraplistoffilesincomextfolder(comextfolderpath = getOption("comext")["datafolder"]) %>%
        # Extract year and month information from the file name
        mutate(year = as.integer(substr(file,5,8)),
               month = as.integer(substr(file,9,10)),
               period = as.integer(year * 100 + month))
    comextmaxperiod <- max(comextfiles$period)

    # Write the name and location of the most recent comext data
    write(paste("name of the Comext most recent data file: ",
                comextfiles$file[comextfiles$period == comextmaxperiod]),
                logfile, append=TRUE)
}

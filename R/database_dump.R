#' Copy the content of a database table to a compressed SQL dump file
#' 
#' This funcion uses the following shell utilities:
#' the mysqldump program to dump a database table.
#' the 7zr program to compress the resulting .sql text file into a .sql.7z file.
#' If mysqldump and 7zr are not available on your system, this function 
#' will not work.
#' @param databasename character name of the database
#' @param tablename character name of the table
#' @param dumpfolder character path where the .sql.7z file will be saved,
#' defaults to a folder on the server, which might not exist on your system.
#' @examples \dontrun{ # Dump a database table
#' dumptable("tradeflows", "raw_comext_monthly_201709")
#' }
#' @seealso \code{\link{loadtabledump}}
#' @export
dumptable <- function(databasename, tablename, 
                      dumpfolder = "/mnt/sdb/data_raw/sqldump/"){
    if(!file.exists(dumpfolder)){
        stop("Folder `", dumpfolder, "` doesn't exist.")
    }
    bashcommand <- sprintf("mysqldump %s %s | 7zr a -si%s.sql %s%s.sql.7z",
                           databasename, tablename, 
                           tablename, dumpfolder, tablename)
    message("Copying the table ", tablename, 
            " to a .sql.7z file with the bash command:\n", 
            bashcommand,"\n")
    system(bashcommand)
}


#' Load a dump file into the database
#' This function uses shell utilities to load a sql dump into the database. 
#' @param databasename character name of the database
#' @param dumpfile character path to a dumpfile
#' @seealso \code{\link{dumptable}}
#' @export
loadtabledump <- function(databasename, dumpfile){
    bashcommand <- sprintf("7zr e -so %s |mysql %s",
                           dumpfile, databasename)
    message("Loading ", dumpfile, 
            " into the ", databasename, 
            " database, with the bash command:\n",
            bashcommand,"\n")
    system(bashcommand)
}



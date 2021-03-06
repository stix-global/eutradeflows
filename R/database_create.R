#' Create a database connection for Shiny
#' 
#' Depending on the dbdocker parameter, 
#' this function will return a database connection 
#' inside a docker container or a local database connection.
#' @param dbdocker logical specifying if the database connection is to be made with a docker container or not
#' @return a database connection object, used to communicate with the database engine.
#' @export
dbconnecttradeflows <- function(dbdocker, dbname = "tradeflows"){
    if(dbdocker){
        # Return a connection to a database in a docker container.
        # Parameter are taken from environement variables in the container, 
        # see function documentation.
        return(eutradeflows::dbconnectdocker())
    } else {  
        # Return a connection to local database.
        return(RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = dbname))
    }
}


#' Creates a database structure based on a sql dump file
#' @description Load a .sql file containing a database structure.
#' @details The default .sql file is storred in the package configuration folder.
#' @param sqlfile character name of an SQL file
#' @param dbname character name of a database
#' @param sqlfolder character path to the folder where the sqlfile is located
#' (defaults to the package config directory)
#' @param messageonly boolean if TRUE prints only a message and doesn't
#' transfer the database structure
#' @param verbose boolean if TRUE print messages, if FALSE mute messages
#' @examples
#' # Load new database structures in the test database
#' # Load a database structure designed to contain raw data
#' createdbstructure(sqlfile = "raw_comext.sql", dbname = "test")
#' # Load a database structure designed to contain validated data
#' createdbstructure(sqlfile = "vld_comext.sql", dbname = "test")
#' @export
createdbstructure <- function(sqlfile,
                              dbname = "tradeflows",
                              sqlfolder = system.file("config", package="eutradeflows", mustWork = TRUE),
                              messageonly = FALSE,
                              verbose = TRUE){
    sqlfile <- file.path(sqlfolder, sqlfile)
    mysqlconfigmessage <-
        "In case of access denied try to edit the mysql configuration file in ~/.my.cnf, it should contain
    [client]
    user = usename
    password = password"
    if (messageonly){ # Only print a message explaining 
                      # how to load the database structure outside R, in the shell.
        message("This message gives 2 options to load the database structure.\n\n",
                "(1) If a user called 'R' is created in MySQL, you can run this from a shell command line: \n",
                sprintf("$ cat '%s' | mysql -u R -p %s", sqlfile, dbname),
                "\n\n",
                "(2) Another option is to call this from a mysql client:\n",
                sprintf("mysql> connect %s;\n", dbname),
                sprintf("mysql> source %s;", sqlfile))
        return()
    } else { # Load sqlfile into the database
        tryCatch({
            if(verbose){
                message("Loading table definitions from:\n", sqlfile,
                        "\ninto the `", dbname, "` database.")
            }
            system(sprintf("cat '%s' | mysql %s", sqlfile, dbname), intern = TRUE)
            # Display the names of created tables
            createtables <- gsub("\\(","",grep("CREATE TABLE",readLines(sqlfile),value=TRUE))
            if(verbose){message(paste(createtables, collapse = "\n"))}
            # In case of error or warning, print additional message
        }, error = function(errorcondition){
            message(toString(errorcondition), "\n", mysqlconfigmessage)
            stop(errorcondition)
        }, warning = function(warningcondition){
            message(toString(warningcondition), "\n", mysqlconfigmessage)
        }
        )
    }
}




#' Open a RMySQL database connection inside a docker container
#' 
#' Facilitate connection to a docker container reunning a mysql database
#' between inside the docker container
#' Connection parameters are passed as environment variables to 
#' the docker container. Use these to create a RMySQL connection object.
#' @param envhost name of the environment variable that contains the db host name
#' @param envuser name of the environment variable that contains the user name
#' @param envpassword name of the environment variable that contains the password
#' @return a RMySQL connection object.
# HARVESTER_DB_HOST: tradeflowsdb:3306
# HARVESTER_DB_USER: R
# HARVESTER_DB_PASSWORD_FILE: /run/secrets/db_password
#' @export
dbconnectdocker <- function(dbname = "tradeflows",
                            envuser = "HARVESTER_DB_USER",
                            envpassword = "HARVESTER_DB_PASSWORD_FILE",
                            envhost = "HARVESTER_DB_HOST", 
                            envport = "HARVESTER_DB_PORT"){
    password <- readLines(Sys.getenv("HARVESTER_DB_PASSWORD_FILE"))
    con <- RMySQL::dbConnect(RMySQL::MySQL(), 
                             dbname = dbname,
                             username = Sys.getenv(envuser),
                             password = password,
                             host = Sys.getenv(envhost),
                             port = as.integer(Sys.getenv(envport)))
    return(con)
}

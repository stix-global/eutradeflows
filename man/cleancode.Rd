% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prorepparcodes.R
\name{cleancode}
\alias{cleancode}
\alias{cleanunit}
\alias{cleanallcomextcodes}
\title{Extract a list of unique, most recent product, reporter and partner codes from Comext}
\usage{
cleancode(RMariaDBcon, tableread, tablewrite, codevariable)

cleanunit(
  RMariaDBcon,
  tableread = "raw_comext_unit",
  tablewrite = "vld_comext_unit"
)

cleanallcomextcodes(RMariaDBcon)
}
\arguments{
\item{RMariaDBcon}{database connection object created by RMySQL \code{\link[DBI]{dbConnect}}}

\item{tableread}{character name of the table to read from}

\item{tablewrite}{character name of the table to write to}

\item{codevariable}{unquoted code variable (à la dplyr verbs)}
}
\value{
TRUE on success
The output is actually a database table containing the cleaned codes.
}
\description{
Takes a raw codes table from comext, select codes which have the most
recent \code{datestart} and make sure they are unique.

\code{cleanallcomextcodes} extracts unique product
and country codes from the Comext raw data so that they are ready for use
as unique keys.
It is a porcelaine function based on the plumbing function \code{cleancode}.
}
\examples{
\dontrun{ # Clean product and country codes
# Connect to the database
con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "test")
# Write dummy codes to the database table "raw_code"
raw_code <- data.frame(code = c(4L, 4L), datestart = c(1L, 2L))
RMariaDB::dbWriteTable(con, "raw_code", raw_code, row.names = FALSE, overwrite = TRUE)
# Clean the codes and write them to the database table "vld_code" (for validated code)
cleancode(con, tableread = "raw_code", tablewrite = "vld_code", codevariable = "code")

# Comext codes
if(FALSE){ # If raw codes are not present, transfer them
createdbstructure(sqlfile = "raw_comext.sql", dbname = "test")
tradeharvester::transfertxtcodesfolder2db(con, 
    rawdatacomextfolder = "~/R/tradeharvester/data_raw/comext/201707/text/english/")
}
# Clean comext product, reporter and partner codes
cleanallcomextcodes(con)
# Disconnect from the database
RMariaDB::dbDisconnect(con)
}
}

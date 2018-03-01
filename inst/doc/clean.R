## ----buildvignette, eval=FALSE, echo=FALSE-------------------------------
#  # To build this vignette run
#  devtools::build_vignettes()
#  # Then rebuild the package CTRL+SHIFT+B so that the vignette is
#  # integrated to the package documentation

## ----setup, warning=FALSE, message=FALSE---------------------------------
library(knitr)
# Do not evaluate code chunks below, only display code
opts_chunk$set(eval=FALSE) 
library(dplyr)

## ------------------------------------------------------------------------
#  # create emtpty database structure for raw codes
#  eutradeflows::createdbstructure(sqlfile = 'raw_comext.sql', dbname = 'tradeflows')
#  # create empty database structure for validated codes
#  eutradeflows::createdbstructure(sqlfile = 'vld_comext.sql', dbname = 'tradeflows')

## ----dbConnect-----------------------------------------------------------
#  con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows")

## ------------------------------------------------------------------------
#  # transfer raw codes
#  tradeharvester::transfertxtcodesfolder2db(con, rawdatacomextfolder = "~/R/tradeharvester/data-raw/comext/201707/text/english/")
#  
#  # transfer raw codes on the server
#  tradeharvester::transfertxtcodesfolder2db(con, rawdatacomextfolder = "/mnt/sdb/data-raw/comext/201710/text/english/")

## ------------------------------------------------------------------------
#  cleanallcomextcodes(con)
#  
#  # Check the content of codes
#  # Display the first 6 lines of all validated `vld̀  tables
#  vldtables <- grep("vld", RMySQL::dbListTables(con), value = TRUE)
#  lapply(vldtables,
#         function(x){
#             tbl(con, x) %>% head() %>% collect() %>% kable(caption = x)
#         })

## ------------------------------------------------------------------------
#  tradeflows::cleancomext()

## ----dbDisconnect--------------------------------------------------------
#  RMySQL::dbDisconnect(con)


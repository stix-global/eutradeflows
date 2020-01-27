## ----buildvignette, eval=FALSE, echo=FALSE-------------------------------
#  # This vignette should be edited in the /vignettes folder
#  # Documentation on how to build vignettes in http://r-pkgs.had.co.nz/vignettes.html
#  # To build this vignette run
#  #devtools::build_vignettes()
#  devtools::build(vignettes = TRUE)
#  # This will generate  "/home/paul/rp/eutradeflows_0.0.1.tar.gz"
#  # Then in stall this source package in the bash shell:
#  # R CMD INSTALL /home/paul/rp/eutradeflows_0.0.1.tar.gz
#  # Then the vignette appears in the main help page for the package.
#  #
#  # The vignettes don't appear in the main help page for the package.
#  # when rebuilding the package CTRL+SHIFT+B in RSTUDIO
#  # CTRL+SHIFT+B used to integrate the vignette to the package documentation
#  # Why doesn't it work anymore?

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

## ------------------------------------------------------------------------
#  if(FALSE){
#      path <- '/tmp/comext_sql_dump/'
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_comext_monthly.sql.7z'))
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_comext_monthly_template.sql.7z'))
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_comext_partner.sql.7z'))
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_comext_product.sql.7z'))
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_comext_reporter.sql.7z'))
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_comext_unit_description.sql.7z'))
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_comext_unit.sql.7z'))
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_comext_yearly_template.sql.7z'))
#      eutradeflows::loadtabledump('tradeflows', file.path(path,'raw_flow_yearly.sql.7z'))
#  }

## ------------------------------------------------------------------------
#  if(FALSE){
#      data_raw_path <- "/tmp/data_raw"
#      # Connect to the database
#      con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "tradeflows")
#      eutradeflows::createdbstructure(sqlfile = 'raw_comext.sql', dbname = 'tradeflows')
#      tradeharveseter::harvestcomextmetadata(con, data_raw_path)
#  
#      # Issues with partner code lead me to load them separately not needed if everything works well
#      # Write partner codes to the database
#      # tradeharvester::writetxtcodes2db(con, "raw_comext_partner",
#      #                                  file.path(data_raw_path,
#      #                                            "/comext/COMEXT_METADATA/CLASSIFICATIONS_AND_RELATIONS/ENGLISH/PARTNERS.txt"),
#      #                                  columnnames = c("partnercode", "datestart", "dateend",
#      #                                                  "partner", "datestart2", "dateend2"))
#      # Disconnect from the database
#      RMariaDB::dbDisconnect(con)
#  }

## ------------------------------------------------------------------------
#  if(FALSE){
#      # Conect to the DB
#      con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "tradeflows")
#      # Extract unique match between each code and their most recent description
#      # Note: the raw data contains repeated codes because the description changes over time.
#      eutradeflows::cleanallcomextcodes(con)
#      # Check for the presence of codes
#      reporter <- tbl(con, "vld_comext_reporter") %>% collect()
#      partner <- tbl(con, 'vld_comext_partner') %>% collect()
#      product <- tbl(con, 'vld_comext_product') %>% collect()
#      unit <- tbl(con, 'vld_comext_unit') %>% collect()
#      unit_description <- tbl(con, 'vld_comext_unit_description') %>% collect()
#      # Disconnect from the DB
#      RMariaDB::dbDisconnect(con)
#  }


library(dplyr)
classificationimm <- read.csv("data_raw/classificationimm.csv", as.is=TRUE)
# Updated product classifiction received on April 11, 2018
classificationstix <- read.csv("data_raw/productclassificationstix.csv", as.is = TRUE)


classificationimm <- classificationimm %>% 
    select(productcode = TTCN,
           productgroupimm = IMM_major_product_group,
           productimm = IMM_product_summary)

classificationstix <- classificationstix %>% 
    select(productcode = Product_code,
           productgroupstix = STIX_major_product_group,
           productstix = STIX_product_summary,
           productstixorder = STIX_order)

devtools::use_data(classificationimm, overwrite = TRUE)
devtools::use_data(classificationstix, overwrite = TRUE)

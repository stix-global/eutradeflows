library(dplyr)
classificationimm <- read.csv("data-raw/classificationimm.csv", as.is=TRUE)

classificationimm <- classificationimm %>% 
    select(productcode = TTCN,
           productgroupimm = IMM_major_product_group,
           productimm = IMM_product_summary)

devtools::use_data(classificationimm, overwrite = TRUE)


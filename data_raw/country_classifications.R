library(dplyr)
library(tidyr)
# Use of enquo() !! based on the "programming with dplyr" vignette
# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
# codevariable <- enquo(codevariable)
# # then later 
# group_by(!!codevariable) 

# Load country groups from a CSV file
countrygroupimm <- read.csv("data-raw/countrygroupimm.csv", as.is=TRUE, na.strings = "")

# Prepare country groups for use in a drop down list in Shiny
countrygroupimm <- countrygroupimm %>% 
    select(partnercode = Eurostat_partner_code,
           partnername = Country_name,
           # Keep uppercase column names below 
           # since these column names will be reshaped to the groupcategory vector
           EU_Membership = EU,
           VPA_Status = VPA_Status_2018,
           Continent = Continent,
           Continent_Detail = Continent_detail,
           Forest_Zone_Short = Forest_zone_short) %>%
    mutate(EU_Membership = ifelse(EU_Membership > 0, "EU", "Non-EU"),
           VPA_Status = paste0("VPA-", VPA_Status)) 

# Get column names corresponding to a group category in an ordered way
# to create an ordered factor below
groupcategoryordered <- names(countrygroupimm)[-c(1,2)]
# Reshape in long format
countrygroupimm <- countrygroupimm %>% 
    gather(groupcategory, group, -partnercode, -partnername) %>% 
    filter(group != "VPA-NA") %>% 
    # Create an ordered factor for the group category (nice display of the drop-down list)
    mutate(groupcategory = factor(groupcategory, levels = groupcategoryordered))

# Add a group with all VPA statuses.
countrygroupvpa <- countrygroupimm %>% 
    filter(groupcategory == "VPA_Status") %>% 
    mutate(group = "VPA-All")
countrygroupimm <- countrygroupimm %>% 
    rbind(countrygroupvpa)

# Save the data frame inside the package
devtools::use_data(countrygroupimm, overwrite = TRUE)

# #
# # What will the group selection mechanism look like?
# # partnercode that belog to the EU
# countrygroupimm$partnername[countrygroupimm$group=="EU"]
# # Or
# countrygroupimm$partnername[countrygroupimm$group=="EUROPE - EU"]
# # Non-EU countries
# countrygroupimm$partnername[countrygroupimm$group=="Non-EU"]
# # if partnercode belongs to VPA
# countrygroupimm %>% filter(grepl("VPA",group))
# # or 
# countrygroupimm %>% filter(groupcategory == "vpastatus")
# # if partnercode belongs to Africa
# # if partnercode belongs to Africa - West Coast
# # if partnercode belongs to forest zone 
# # etc..
# # We need a list of groups and a list of countries which belong to these groups.
# EU == TRUE

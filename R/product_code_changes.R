 
#' Find subsequent code for a given product code
#' @param orig_codes_to_check character, usually one code
#' @param cn_code_changes a table of cn_code_changes (already contained in the package)
#' @return data frame of all subsequent codes for the given original code
#' @examples
#' subsequent_codes(cn_code_changes, "44072969")
subsequent_codes <- function(orig_codes_to_check, cn_code_changes = eutradeflows::cn_code_changes){
    # Accumulative CN code changes
    cn_changes_acc <- data.frame()
    i = 1
    while (TRUE){
        df <- cn_code_changes %>% 
            filter(origin_code %in% orig_codes_to_check)
        # print(df)
        # Keep previous accumulative data frame for comparison purposes
        df_acc_previous <- cn_changes_acc
        # Concatenate to the accumulative data frame
        cn_changes_acc <- distinct(rbind(cn_changes_acc, df))
        # if same as previous, break
        if(identical(df_acc_previous, cn_changes_acc)){
            break
        }
        # max iteration
        i = i + 1
        if(i>10) break
        # Use destination codes as orig codes for the next iteration
        orig_codes_to_check <- unique(df$destination_code)
    }
    return(cn_changes_acc)
}

#' Function to find previous codes
#' @param dest_codes_to_check usually only one code
#' @examples
#' previous_codes(cn_code_changes, "44072969")
previous_codes <- function(dest_codes_to_check, cn_code_changes = eutradeflows::cn_code_changes){
    # Accumulative CN code changes
    cn_changes_acc <- data.frame()
    i = 1
    while (TRUE){
        df <- cn_code_changes %>% 
            filter(destination_code %in% dest_codes_to_check)
        # print(df)
        # Keep previous accumulative data frame for comparison purposes
        df_acc_previous <- cn_changes_acc
        # Concatenate to the accumulative data frame
        cn_changes_acc <- distinct(rbind(cn_changes_acc, df))
        # if same as previous, break
        if(identical(df_acc_previous, cn_changes_acc)){
            break
        }
        # max iteration
        i = i + 1
        if(i>10) break
        # Use origin codes as destination codes for the next iteration
        dest_codes_to_check <- unique(df$origin_code)
    }
    return(cn_changes_acc)
}

#' Prepare the code changes table
#' Code changes are obtained from EUROSTAT Ramon
#' https://ec.europa.eu/eurostat/ramon/relations/index.cfm?TargetUrl=LST_REL
#' The Excel file is simply saved to a csv by hand. 
#' @param csv_file character path to the csv file from EUROSTAT Ramon where code changes are given
#' @param packagefodler character path to the rds file where to save the updated data. Defaults inside the package.
#' @return a data frame of this form
#'  origin_code  destination_code  orig_end_year  dest_start_year
#'   <chr>       <chr>                    <dbl>           <dbl>
#' 1 02012011    02012021                  1988            1989
#' 2 02012011    02012029                  1988            1989
#' @examples 
#' \dontrun{
#' # Update the codes while developing the package
#' update_code_changes()
#' }
#' @export
update_code_changes <- function(csv_file = "~/downloads/CN_2019_update_of_codes.csv", 
                                rdata_file = "data/cn_code_changes.rda"){
    cn_code_changes <- readr::read_csv(csv_file)
    names(cn_code_changes) <- tolower(gsub(" ","_",names(cn_code_changes)))
    cn_code_changes <- cn_code_changes %>% 
        mutate(orig_end_year = period %/% 1e4, 
               dest_start_year = period - (period %/% 1e4) * 1e4,
               # Remove space from product codes
               origin_code = gsub(" ","",origin_code),
               destination_code = gsub(" ","",destination_code)) %>% 
        select(-period)
    # Check there is always only one year difference
    unique(cn_code_changes$dest_start_year - cn_code_changes$orig_end_year)
    # Keep only the dest start year
    message(sprintf("saving the code changes file to : %s", rdata_file))
    save(cn_code_changes, file=rdata_file)
}


#' CN code changes
#'
#' This dataset is available as eutradeflows::cn_code_changes
#' Code changes are obtained from EUROSTAT Ramon
#' The Excel file is simply saved to a csv by hand. 
#' 
#' @format A data frame with 15000 rows and 4 variables:
#' \describe{
#'   \item{origin_code}{CN code}
#'   \item{destination_code}{CN code}
#'   \item{orig_end_year}{year}
#'   \item{dest_start_year}{year}
#' }
#' @source \url{https://ec.europa.eu/eurostat/ramon/relations/index.cfm?TargetUrl=LST_REL}
"cn_code_changes"


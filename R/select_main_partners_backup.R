#' @name select_main_partners
#' @title A function to select main partner(s) for each reporter
#' @param df a data frame of trade flows
#' @param slice_n number of partner(s) to slice, defaults to one
#' @return a data frame with the main partner for each reporter
#' @examples
#' \dontrun{
#' library(dplyr)
#' # Load sawnwood oak data from the database
#' con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "tradeflows")
#' swd_oak <- tbl(con, "raw_comext_monthly") %>%
#'     # Filter import flows and oak sawnwood
#'     filter(flowcode == 1 & productcode == "44079190") %>% 
#'     collect()
#' # Load reporter country codes and names
#' reporter_names <- tbl(con, 'vld_comext_reporter') %>% collect()
#' # Load partner codes and names
#' partner_names <- tbl(con, 'vld_comext_partner') %>%collect()
#' # Add reporter and partner information
#' swd_oak <- swd_oak %>%
#'     left_join(partner_names, by='partnercode') %>%
#'     left_join(reporter_names, by='reportercode')
#' RMariaDB::dbDisconnect(con)
#' # Now use this function:
#' swd_oak %>%
#'     select_main_partners()
#' }
#' @export
select_main_partners <- function(df, slice_n=1){
    df %>%
        # Sum trade value and weight over the whole period
        group_by(reporter, partner) %>%
        summarise(na_weight = sum(is.na(weight)),
                  na_quantity = sum(is.na(quantity)),
                  n = n(),
                  tradevalue = sum(tradevalue),
                  weight = sum(weight)) %>%
        # Absence of NA values for the weight
        filter(na_weight==0) %>%
        # Take the first partner
        group_by(reporter) %>%
        arrange(desc(tradevalue)) %>%
        slice(slice_n)
}

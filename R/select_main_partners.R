# Author Paul Rougieux.

#' @name select_main_partners
#' @title A function to select main partner(s) for each reporter
#' @param df a data frame of trade flows
#' @param slice_n vector of partner(s) to slice, defaults to one
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
#' # Now use this function 
#' # select the first partner for each reporter:
#' select_main_partners(swd_oak)
#' # Select the first 3 partners for each reporter:
#' select_main_partners(swd_oak, slice_n=1:3)
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

#' Select main tropical partners for the product of interest and related products
#' @param wood_imports data frame of wood imports from comext
#' @param vpa_partners data frame containing the country code of VPA partners
#' @param force_full_series boolean to force selection of only country pairs which have all data points for all period
#' @examples
#' # Load the main partners of tropical sawnwood
#' swd_trop_main <- select_main_partners(wood_imports, vpa_partners, "44072969")
#' # Load the main partners of plywood
#' ply_trop_main <- select_main_partners(wood_imports, vpa_partners, "44123190")
#' @return data fram of main tropical sawnwood partners
#' @export
select_main_partners_trop <- function(wood_imports, 
                                      partner_codes_of_interest, 
                                      product_codes_of_interest,
                                      force_full_series = TRUE){
    df <- select_wood_imports(wood_imports=wood_imports,
                                      partner_codes_of_interest=partner_codes_of_interest, 
                                      product_codes_of_interest=product_codes_of_interest)

    large_country_pairs <- select_large_country_pairs(df)

    # See the output of this table in sawnwood_tropical.Rmd
    print("Number of large country pairs which have the max number of flows ")
    print(nrow(large_country_pairs %>% filter(n==max(n))))
    print("Number of large country pairs")
    print(nrow(large_country_pairs))

    # Error if n is greater than len(unique(df$period))
    stopifnot(max(large_country_pairs$n)<=length(unique(df$period)))

    # Optionally remove country pairs with missing data
    if(force_full_series){
        # See the output of this table in sawnwood_tropical.Rmd
        large_country_pairs <- large_country_pairs %>%
            filter(n==max(n) & !is.na(weight))
    }
    # Keep only the reporter and partner columns for the right join
    large_country_pairs <- large_country_pairs %>%
        select(reporter, partner)

    # Right join to the main input data to keep only the country pairs of interest
    df_main <- df %>%
        right_join(large_country_pairs, by = c('reporter', 'partner'))
    return(df_main)
}

#' @rdname select_main_partners
#' @description \code{select_wood_imports} selects trade flows for the product 
#' code of interest and its parents.
#' @export
select_wood_imports <- function(wood_imports, 
                                partner_codes_of_interest, 
                                product_codes_of_interest){
    require(dplyr)
    # Create a data frame containing trade flows for the product code of interest and its parents
    df <- wood_imports %>%
        # Filter import flows in the partnercodes of interest
        filter(flowcode == 1 &
                   partnercode %in% partner_codes_of_interest &
                   productcode %in% product_codes_of_interest) %>%
        # Aggregate these flows together
        group_by(reporter, partner, period) %>%
        summarise(tradevalue = sum(tradevalue),
                  weight = sum(weight),
                  quantity = sum(quantity)) %>%
        mutate(pricew = tradevalue / weight)
    return(df)
}


#' @rdname select_main_partners
#' @description Select the largest country pairs
#' Select all country pairs which represent at least a given
#' percentage (default to 1%) of the trade value over the whole period.
#' @param df a data frame of trade flows
#' @export
select_large_country_pairs <- function(df, trade_pc_threshold = 0.01){
    # Note: a reporter might appear twice here with different partners.
    large_country_pairs <- df %>%
        group_by(reporter, partner) %>%
        summarise(tradevalue = sum(tradevalue),
                  weight = sum(weight),
                  quantity = sum(quantity),
                  n = n()) %>%
        ungroup() %>%
        mutate(trade_pct = round(tradevalue / sum(tradevalue, na.rm=TRUE),3)) %>%
        arrange(desc(tradevalue)) %>%
        filter(trade_pct >= trade_pc_threshold)
    return(large_country_pairs)
}

#' @rdname select_main_partners
#' @param df_large_country_pairs a data frame of main partners with columns `trade_pct` and `n`.
#' output of the select_large_country_pairs function.
#' @param caption character table caption
#' @export
display_table_percentage <- function(df_large_country_pairs,
                                     caption = "Percentage of trade value in the main country pairs compared to trade in all countries"){
    df_large_country_pairs %>%
        pivot_wider(id_cols=partner, names_from=reporter, values_from = trade_pct) %>%
        kable(caption = caption)
}

#' @rdname select_main_partners
#' @export
display_table_number_of_flows <- function(df_large_country_pairs,
                                          caption = "Number of trade flows in the main country pairs"){
    df_large_country_pairs %>%
        pivot_wider(id_cols=partner, names_from=reporter, values_from = n) %>%
        kable(caption = caption)

}


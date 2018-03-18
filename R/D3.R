#' Prepare nodes and links to generate a Sankey diagram
#' @param dtf a data frame of trade flows data
#' @param reporter data frame of reporter codes and names
#' @param partner data frame of partner codes and names
#' @return a list of data frames with links and nodes
#' @export
preparesankeynodes <- function(dtf, reporter, partner, debugname = FALSE){
    # convert dtf to a data frame to remove the grouping variables 
    # and to avoid the warning:
    # "Links is a tbl_df. Converting to a plain data frame."
    # "Nodes is a tbl_df. Converting to a plain data frame."
    dtf <- dtf %>% data.frame()
    
    # Create new indexes based on first appearance in the data
    newsourceindex <- dtf %>% 
        distinct(partnercode) %>% 
        mutate(source = row_number() - 1)
    newtargetindex <- dtf %>% 
        distinct(reportercode) %>% 
        mutate(target = row_number() + max(newsourceindex$source))
    
    # Add source and target index to the input data farme
    dtf2 <- dtf %>% 
        left_join(newsourceindex, by = "partnercode") %>% 
        left_join(newtargetindex, by = "reportercode")
    
    # Combine source and target nodes in a single data frame
    newsourceindex <- newsourceindex %>% 
        left_join(partner, by = "partnercode") %>% 
        transmute(name = partner,
                  code = partnercode)
    newtargetindex <- newtargetindex %>% 
        left_join(reporter, by = "reportercode") %>% 
        transmute(name = reporter,
                  code = reportercode)
    nodes <- rbind(newsourceindex, newtargetindex) 
    
    if (debugname){
        nodes <- nodes %>% 
            transmute(name = paste(name, code))
    }
    return(list(links = dtf2,
                nodes = nodes))
}


#' Plot a sankey diagram, using the D3 library networkD3
#' @param linksandnodes a list containing 2 dataframes: links and nodes
#' @param value the value to be displayed in the sankey diagram, a column 
#' in linksandnodes$links
#' @examples \dontrun{
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows")
#' wood <- tbl(con, "vld_comext_monthly") %>% 
#'    filter(productcode ==  44072199 & 
#'               flowcode == 1 & 
#'               period == "201708") %>% 
#'    # head(200) %>% 
#'    collect()
#' reporter <- tbl(con, "vld_comext_reporter") %>% collect()
#' partner <- tbl(con, "vld_comext_partner") %>% collect()
#' wood %>% 
#'     preparesankeynodes %>% 
#'     plotsankey()
#' }
#' @return NULL
#' @export
plotsankey <- function(linksandnodes, value = "tradevalue", units = "K€"){
    sankeyNetwork(Links = linksandnodes$links,
                  Nodes = linksandnodes$nodes, 
                  Source = "source",
                  Target = "target", 
                  Value = value, 
                  NodeID = "name",
                  units = units,
                  fontSize = 12, 
                  nodeWidth = 30) %>% 
        return()
}

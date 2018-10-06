library(dplyr)
library(tidyr)
library(eutradeflows)

# Run the application with 
# shiny::runApp('/home/R/eutradeflows/docs/visualization/shinycsv')
# In Docker, load the application 
# Hack to disconnect open connections
# RMariaDB::dbDisconnect(RMariaDB::dbListConnections(RMariaDB::MariaDB())[[1]])

rowstodisplay <- 100
dbdocker = FALSE

# Create a database connection
# Depending on the dbdocker parameter, it will create a connection 
# to a docker container or a local connection.
#' @param dbdocker logical specifying if the database connection is to be made with a docker container or not
dbconnecttradeflows <- function(dbdocker){
    if(dbdocker){
        # Return a connection to a database in a docker container.
        # Parameter are taken from environement variables in the container, 
        # see function documentation.
        return(eutradeflows::dbconnectdocker())
    } else {  
        # Return a connection to local database.
        return(RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = "tradeflows"))
    }
}


#' Load a data frame of trade flows 
#' @examples 
#' if(interactive){
#' swd <- loadvldcomextmonhtly(con, "4407%", 201701, 201709)
#' plywood <- loadvldcomextmonhtly(con, "4412%", 201701, 201709)
#' furniturekitchen <- loadvldcomextmonhtly(con, "940340%", 201701, 201709)
#' }
loadvldcomextmonhtly <- function(con, productcode_, 
                                 periodmin, periodmax,
                                 flowcode_ = 1){
    tbl(con, "vld_comext_monthly") %>% 
        filter(productcode %like%  productcode_ & 
                   flowcode == flowcode_ & 
                   period >= periodmin & period<=periodmax) %>% 
        addproreppar2tbl(con, .) %>% 
        collect() %>% 
        # Remove quotation marks from the product description
        mutate(productdescription = gsub('"',' ', productdescription)) %>% 
        select(productdescription, reporter, partner, period, 
               tradevalue, weight, quantity, quantityraw,
               productcode, everything())
}

# Transpose a data frame of trade flows
if(FALSE){
    swd <- loadvldcomextmonhtly(con, "4407%", 201701, 201709)    
    # Gather along period and all codes columns 
    # spread the 
    swd2 <- swd %>% 
        select(-quantityraw) %>% # Remove quantityraw
        # Gather along all columns except quantity, tradevalue and weight
        gather(variable, value, quantity, tradevalue, weight) %>% 
        # Rupert: start with value: tradevalue, weight, quantity
        # v, mt, 
        # v, mt, q
        left_join(data_frame(variable = c("tradevalue", "quantity", "weight"),
                             variable2 = c("v","q","w")), by="variable") %>% 
        unite(varperiod, variable2, period, sep="") %>% 
        spread()
}

# Debug on the server
if(FALSE){
    swd <- loadvldcomextmonhtly(con2, "4407%", 201701, 201709)    
}


# This function should be the last object in the script
function(input, output, session) {
    # Load a large data set from the database, based on date and productcode
    datasetInput <- reactive({
        # Fetch the appropriate data, depending on the value of input$produtcode
        inputproductcode <- switch(input$productcode,
               "Sawnwood" = "4407%",
               "Plywood" = "4412%",
               "Kitchen Furniture" = "940340%")
        # Load data from the database
        con <- dbconnecttradeflows(dbdocker = dbdocker)
        on.exit(RMariaDB::dbDisconnect(con))
        loadvldcomextmonhtly(con, inputproductcode, 
                             periodmin = format(input$range[1], "%Y%m"),
                             periodmax = format(input$range[2], "%Y%m"))
    })
    
    
    # Filter the database based on other variables country etc...
    datasetfiltered <- reactive({
        datasetInput() %>% 
            filter(reporter %in% input$reporter)
    })
        
    output$rowsinputtable <- renderText({ 
        paste(nrow(datasetInput()), "rows")
    })
    output$rowsfilteredtable <- renderText({ 
        paste(nrow(datasetfiltered()), "rows")
    })
    
    # Update list of reporting countries based on the data
    observe({
        reporterx <- unique(datasetInput()$reporter)
        updateSelectizeInput(session, "reporter",
                             label = "Choose reporting countries:",
                             choices = reporterx,
                             selected = reporterx)
    })
    
    output$table <- DT::renderDataTable(datasetfiltered(),
                                        # Columnd rendering option 
                                        # Copied from https://rstudio.github.io/DT/options.html
                                        options = list(columnDefs = list(list(
                                            targets = 1,
                                            render = DT::JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data.length > 15 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                                                "}")
                                        ))), callback = DT::JS('table.page(3).draw(false);'))
    
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$downloadData <- downloadHandler(
        
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            paste(input$productcode, input$filetype, sep = ".")
        },
        
        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
            
            # Write to a file specified by the 'file' argument
            write.table(select(datasetfiltered(),
                               productcode, everything(), -productdescription,  
                               productdescription), 
                        file, sep = sep,
                        row.names = FALSE)
        }
    )
}

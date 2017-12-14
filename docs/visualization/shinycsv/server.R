library(dplyr)
library(eutradeflows)
con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows")

rowstodisplay <- 100

# Only import values
loadvldcomextmonhtly <- function(con, productcode_, flowcode_ = 1){
    tbl(con, "vld_comext_monthly") %>% 
        filter(productcode %like%  productcode_ & 
                   flowcode == flowcode_ & 
                   period >= 201701) %>% 
        collect()
}
swd <- loadvldcomextmonhtly(con, "4407%")    
plywood <- loadvldcomextmonhtly(con, "4412%")    
furniturekitchen <- loadvldcomextmonhtly(con, "940340%")
rm(con) # Where should the database connection object be deleted?

function(input, output) {
    datasetInput <- reactive({
        # Fetch the appropriate data object, depending on the value
        # of input$dataset.
        switch(input$productcode,
               "Sawnwood" = head(swd, rowstodisplay),
               "Plywood" = head(plywood, rowstodisplay),
               "Kitchen Furniture" = head(furniturekitchen, rowstodisplay))
    })
    
    output$table <- renderTable({
        datasetInput()
    })
    
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
            write.table(datasetInput(), file, sep = sep,
                        row.names = FALSE)
        }
    )
}

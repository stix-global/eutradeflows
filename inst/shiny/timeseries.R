library(dplyr)
library(tidyr)
library(eutradeflows)
library(dygraphs)

# Run the application with 
# shiny::runApp('/home/paul/R/eutradeflows/docs/visualization/timeseries')
# In Docker, load the application 
# Hack to disconnect open connections
# RMySQL::dbDisconnect(RMySQL::dbListConnections(RMySQL::MySQL())[[1]])
# Debug on the server
# swd <- loadvldcomextmonhtly(con, c("44071015", "44071031", "44071033", "44071038", "44071091", "44071093", "44071098"), 201701, 201709)  
# chips <- loadvldcomextmonhtly(con, c("44012100", "44012200"), 201701, 201709)  


ui <- fluidPage(
    titlePanel("File download from the STIX database", windowTitle = "STIX-Global / eutradeflows"),
    sidebarLayout(
        sidebarPanel(
            helpText("Change product or date range to fetch data from the database.",
                     "Number of rows loaded from the database:"),
            textOutput("rowsinputtable"),
            
            # Input: Specification of the product
            selectInput("productgroupimm", "Choose a product group:", 
                        choices = eutradeflows::classificationimm$productgroupimm),
            selectInput("productimm", "Choose a product :", 
                        choices = eutradeflows::classificationimm$productimm),
            
            # Input: Specification of the date range
            sliderInput("range", "Date range:",
                        min = as.Date('2000-01-01'), max = as.Date('2017-09-01'),
                        value = c(as.Date('2017-01-01'),as.Date('2017-09-01')),
                        timeFormat = "%Y%m"),
            helpText("Change country to filter the table in your browser:"),
            helpText("Number of rows filtered in your browser:"),
            textOutput("rowsfilteredtable"),
            
            # Input: Specification of the reporter country
            selectizeInput("reporter", "Choose reporting country:", 
                           choices = NULL, multiple=FALSE),
            selectizeInput("partner", "Choose partner country:", 
                           choices = NULL, multiple=FALSE),
            radioButtons("tableformat", "Table Format:",
                         choices = c("long", "wide")),
            radioButtons("filetype", "File type:",
                         choices = c("csv", "tsv")),
            downloadButton('downloadData', 'Download'),
            width = 3
        ),
        mainPanel(
            dygraphOutput("dygraph")
        )
    )
)



rowstodisplay <- 100
dbdocker = FALSE
dateWindow <- c("2012-01-01", "2018-01-01") # used by dygraph::dyRangeSelector


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
        return(RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows"))
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
        filter(productcode %in% productcode_ & 
                   flowcode == flowcode_ & 
                   period >= periodmin & period<=periodmax) %>% 
        addproreppar2tbl(con, .) %>% 
        collect() #%>% 
        # # These operations should be performed in the cleaning procedure
        # # Remove quotation marks from the product description
        # mutate(productdescription = gsub('"',' ', productdescription)) %>% 
        # select(productdescription, reporter, partner, period, 
        #        tradevalue, weight, quantity, quantityraw,
        #        productcode, everything())
        # Remove spaces from the country names
}

# Transpose a data frame of trade flows
if(FALSE){
    swd <- loadvldcomextmonhtly(con,"codes as characters" , 201701, 201709)    
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



# This function should remain the last object in the server.R script
server <- function(input, output, session) {
    # Load a large data set from the database, based on date and productcode
    datasetInput <- reactive({
        # Fetch the appropriate data, depending on the value of input$productimm
        con <- dbconnecttradeflows(dbdocker = dbdocker)
        on.exit(RMySQL::dbDisconnect(con))
        # Convert imm product name to a vector of product codes
        productselected <- classificationimm %>% 
            filter(productimm %in% input$productimm)
        # Convert to character because SQL is slower if 
        # search is performed on character index, while using a numeric value
        productselected <- as.character(productselected$productcode)
        loadvldcomextmonhtly(con, productselected,
                             periodmin = format(input$range[1], "%Y%m"),
                             periodmax = format(input$range[2], "%Y%m"))
    })
    
    
    # Filter the database based on other variables country etc...
    datasetfiltered <- reactive({
        dtf <- datasetInput() %>%
            # Aggregates could be added to the input data or calculated here?
            filter(reporter %in% input$reporter & partner %in% input$partner) %>%
            mutate(date = lubridate::parse_date_time(period, "ym")) %>%
            select(date, tradevalue, quantity, weight) %>%
            data.frame()
        # Convert to a time series
        dtfxts <- xts::xts(dtf[,-1], order.by=dtf[,1])
        return(dtfxts)
    })
        
    output$rowsinputtable <- renderText({ 
        productimmselected <- classificationimm %>% 
            filter(productimm %in% input$productimm)
        paste(nrow(datasetInput()), "rows", input$productimm,
              paste(productimmselected$productcode, collapse=", "))
    })
    output$rowsfilteredtable <- renderText({ 
        paste(nrow(datasetfiltered()), "rows")
    })
    
    # Update list of reporting countries based on the data
    observe({
        reporterx <- unique(datasetInput()$reporter)
        updateSelectizeInput(session, "reporter",
                             label = "Choose reporting countries:",
                             choices = reporterx)
    })

    observe({
        partnerx <- unique(datasetInput()$partner)
        updateSelectizeInput(session, "partner",
                             label = "Choose partner countries:",
                             choices = partnerx)
    })
    
        
    output$dygraph <- renderDygraph({
        dygraph(datasetfiltered()[,"quantity"],
                main = "Quantity", ylab = "1000 M3", group="tf") %>% 
            dyRangeSelector(dateWindow=dateWindow)
        
        # dygraph(swd1xts[,"tradevalue"], main = "Trade Value", ylab = "1000 €", group="tf") %>% 
        #     dyRangeSelector(dateWindow=dateWindow)
        # 
        # dygraph(swd1xts[,"weight"], main = "Weight", ylab = "t", group="tf") %>% 
        #     dyRangeSelector(dateWindow=dateWindow)
        
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
            write.table(select(datasetfiltered(),
                               productcode, everything(), -productdescription,  
                               productdescription), 
                        file, sep = sep,
                        row.names = FALSE)
        }
    )
}


shinyApp(ui, server)
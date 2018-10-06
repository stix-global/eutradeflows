library(dplyr)
library(tidyr)
library(eutradeflows)
library(dygraphs)

rowstodisplay <- 100
dbdocker = FALSE
dateWindow <- c("2012-01-01", as.character(Sys.Date())) # used by dygraph::dyRangeSelector

# Run the application with 
# shiny::runApp('/home/paul/R/eutradeflows/docs/visualization/timeseries')
# In Docker, load the application 
# Hack to disconnect open connections
# RMariaDB::dbDisconnect(RMariaDB::dbListConnections(RMariaDB::MariaDB())[[1]])
# Load data to debug on the server
# swd <- loadvldcomextmonhtly(con, c("44071015", "44071031", "44071033", "44071038", "44071091", "44071093", "44071098"), 201701, 201709)  
# chips <- loadvldcomextmonhtly(con, c("44012100", "44012200"), 201701, 201709)  

ui <- function(request){
    fluidPage(
        includeCSS("styles.css"),
        titlePanel("Time series from the STIX database", windowTitle = "STIX-Global / eutradeflows"),
        sidebarLayout(
            sidebarPanel(
                selectInput("productgroupstix", "Choose a product group:", 
                            choices = unique(eutradeflows::classificationstix$productgroupstix),
                            selected = "Sawnwood"),
                selectInput("productstix", "Choose a product :", 
                            choices = unique(eutradeflows::classificationstix$productstix),
                            selected = "Tropical hardwood"),
                # selectInput("productcode", "Choose an 8 digit product Code (Optional):", 
                #             choices = "All",
                #             selected = "All"),
                radioButtons("flowcode", "Flow direction:", inline = TRUE,
                             choiceNames = c("Import", "Export"),
                             choiceValues = c(1, 2)),
                sliderInput("range", "Date range:",
                            min = as.Date('2000-01-01'), max = Sys.Date(),
                            value = c(as.Date('2012-01-01'), Sys.Date()),
                            timeFormat = "%Y%m"),
                selectInput("reporter", "Choose reporting country:", 
                            choices = c("All EU", "Utd. Kingdom"),
                            selected = "Utd. Kingdom",
                            multiple = FALSE),
                selectInput("partner", "Choose partner country:", 
                            choices = c("All", "Cameroon"), 
                            selected = "Cameroon",
                            multiple=FALSE),
                radioButtons("tableformat", "Table Format:",
                             choices = c("long", "wide")),
                radioButtons("filetype", "File type:",
                             choices = c("csv", "tsv")),
                downloadButton('downloadData', 'Download'),
                bookmarkButton(),
                helpText("Information on the current selection"),
                textOutput("info"),
                textOutput("rowsfilteredtable"),
                width = 3
            ),
            mainPanel(
                dygraphOutput("dygraphquantity"),
                dygraphOutput("dygraphtradevalue"),
                dygraphOutput("dygraphweight"),
                tableOutput("productdescription"),
                radioButtons("tableaggregationlevel",
                             "Show table of (flags only visible at the 8 digit level):",
                             inline = TRUE,
                             choiceNames = c("STIX product aggregates", "8 digit product codes"),
                             choiceValues = c("stix", "8d")),
                tableOutput("table")
            )
        )
    )
}    



#' Load a data frame of trade flows 
#' @examples 
#' if(interactive){
#' # Use full code
#' swd <- loadvldcomextmonhtly(con, c("44071015", "44071031", "44071033", "44071038", "44071091", "44071093", "44071098"), 201701, 201709)  
#' chips <- loadvldcomextmonhtly(con, c("44012100", "44012200"), 201701, 201709)  
#' }
loadvldcomextmonhtly <- function(con, productcode_, 
                                 periodmin, periodmax,
                                 flowcode_){
    remote <- tbl(con, "vld_comext_monthly") %>% 
        filter(productcode %in% productcode_ & 
                   flowcode == flowcode_ & 
                   period >= periodmin & period<=periodmax) %>% 
        addproreppar2tbl(con, .) 
    show_query(remote)
    collect(remote)
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
        # Fetch the appropriate data, depending on the value of input$productstix
        con <- dbconnecttradeflows(dbdocker = dbdocker)
        on.exit(RMariaDB::dbDisconnect(con))
        # Convert stix product name to a vector of product codes
        productselected <- classificationstix %>% 
            filter(productstix %in% input$productstix)
        # Convert to character because SQL is slower if 
        # search is performed on character index, while using a numeric value
        productselected <- as.character(productselected$productcode)
        loadvldcomextmonhtly(con, 
                             productcode_ = productselected,
                             periodmin = format(input$range[1], "%Y%m"),
                             periodmax = format(input$range[2], "%Y%m"),
                             flowcode_ = input$flowcode)
    })
    
    
    # Filter the database based on other variables 
    # such as reporter and partner country etc...
    datasetfiltered <- reactive({
        validate(
            need(input$reporter != "", "Please select a reporter country"),
            need(input$partner != "", "Please select a partner country")
        )
        dtf <- datasetInput() %>%
            # Aggregates could be added to the input data or calculated here?
            filter(reporter %in% input$reporter & partner %in% input$partner)
        return(dtf)
    })
    
    datasetaggregated <- reactive({
        dtf <- datasetfiltered() %>% 
            mutate(productstix = input$productstix) %>% 
            group_by(productstix, reporter, partner, period, flowcode) %>% 
            summarise(tradevalue = sum(tradevalue),
                      weight = sum(weight),
                      quantity = sum(quantity))
        return(dtf)
    })
    
    # The filtered dataset converted to a time series object for plotting
    datasetxts <- reactive({
        # req(input$partner)
        dtf <- datasetaggregated() %>%
            mutate(date = lubridate::parse_date_time(period, "ym")) %>%
            ungroup() %>% # Force removal of grouping variables from the selection
            select(date, tradevalue, quantity, weight) %>%
            data.frame()
        # Convert to a time series
        dtfxts <- xts::xts(dtf[,-1], order.by=dtf[,1])
        return(dtfxts)
    })
    
    output$info <- renderText({ 
        productstixselected <- classificationstix %>% 
            filter(productstix %in% input$productstix)
        paste(nrow(datasetInput()), "rows fetched from the database.",
              "Product group stix:", input$productgroupstix,
              ". Product stix:", input$productstix,
              ". Product codes 8 digit:",
              paste(productstixselected$productcode, collapse=", "),
              "Flow direction: ", input$flowcode)
    })
    
    output$rowsfilteredtable <- renderText({ 
        sprintf("%s rows filtered from the dataset",
                nrow(datasetfiltered()))
    })
    
    # update list of product stix based on the product group stix
    observe({
        stix <- eutradeflows::classificationstix %>%
            filter(productgroupstix == input$productgroupstix) %>%
            distinct(productstix, productstixorder) %>% 
            arrange(productstixorder)
        productstix <- stix$productstix
        # If the previous selection is present in the list, reuse the same selection
        # otherwise take the first value
        if(input$productstix %in% productstix){
            productstixselected <- input$productstix
        } else {
            productstixselected <- productstix[1]
        }
        updateSelectizeInput(session, "productstix",
                             choices = productstix,
                             selected = productstixselected)
    }, priority = 2) # 
    
    # Update list of reporting countries based on the data fetched from the database
    observe({
        reporterx <- unique(datasetInput()$reporter)
        reporterx <- reporterx[order(reporterx)]
        reporterx <- c("All EU", reporterx[!is.na(reporterx)])
        previousselection <- input$reporter 
        updateSelectizeInput(session, "reporter",
                             choices = reporterx,
                             selected = previousselection)
    })

    # Update list of partner countries based on the data fetched from the database
    observe({
        partnerx <- unique(datasetInput()$partner)
        partnerx <- partnerx[order(partnerx)]
        partnerx <- c( "All", partnerx[!is.na(partnerx)])
        previousselection <- input$partner
        updateSelectizeInput(session, "partner",
                             choices = partnerx,
                             selected = previousselection)
    })

    output$dygraphquantity <- renderDygraph({
        dygraph(datasetxts()[,"quantity"], main = "Quantity", ylab = "M3", group="tf") %>% 
            dyRangeSelector(dateWindow=dateWindow) %>% 
            dyRoller(rollPeriod=12)
    })
    
    output$dygraphtradevalue <- renderDygraph({
        dygraph(datasetxts()[,"tradevalue"], main = "Trade Value", ylab = "1000 €", group="tf") %>% 
            dyRangeSelector(dateWindow=dateWindow) %>% 
            dyRoller(rollPeriod=12)
    })
    
    output$dygraphweight <- renderDygraph({
        dygraph(datasetxts()[,"weight"], main = "Weight", ylab = "T", group="tf") %>% 
            dyRangeSelector(dateWindow=dateWindow) %>% 
            dyRoller(rollPeriod=12)
    })
    
    # Separate product description from the main table
    output$productdescription <- renderTable(distinct(datasetfiltered(),
                                                      productcode,productdescription))
    
    # 
    datasettoshow <-  reactive({
        if(input$tableaggregationlevel == "stix"){
            return(datasetaggregated())
        } else {
            return(select(datasetfiltered(), -productdescription))
        }
        })
    output$table <- renderTable(datasettoshow()) 
    
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$downloadData <- downloadHandler(
        
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            return("name_not_displayed_in_file_save_dialog.csv")
            # paste(input$productstix, input$filetype, sep = ".")
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
    # https://shiny.rstudio.com/articles/advanced-bookmarking.html
    # Save extra values in state$values when we bookmark
    onBookmark(function(state){
        state$values$reporter <- input$reporter
    })
    
    # Read values from state$values when we restore
    onRestore(function(state) {
        updateSelectizeInput(session, "reporter",
                             choices = state$values$reporter,
                             selected = state$values$reporter)
    })
    # 
    # Exclude the range input from bookmarking (because it's not working for the moment)
    setBookmarkExclude("range")
}

# Complete app with UI and server components
shinyApp(ui, server, enableBookmarking = "url")

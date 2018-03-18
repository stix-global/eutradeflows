library(dplyr)
library(tidyr)
library(eutradeflows)
library(dygraphs)

rowstodisplay <- 100
dbdocker = FALSE
dateWindow <- c("2012-01-01", as.character(Sys.Date())) # used by dygraph::dyRangeSelector

# Load country codes tables for later use by the drop down lists
con <- dbconnecttradeflows(dbdocker = dbdocker)
reportertable <- tbl(con, "vld_comext_reporter") %>% collect()
partnertable <- tbl(con, "vld_comext_partner") %>% collect() 
RMySQL::dbDisconnect(con)

# Run the application with 
# shiny::runApp('/home/paul/R/eutradeflows/docs/visualization/timeseries')
# In Docker, load the application 
# Hack to disconnect open connections
# RMySQL::dbDisconnect(RMySQL::dbListConnections(RMySQL::MySQL())[[1]])
# Load data to debug on the server
# swd <- loadvldcomextmonhtly(con, c("44071015", "44071031", "44071033", "44071038", "44071091", "44071093", "44071098"), 201701, 201709)  
# chips <- loadvldcomextmonhtly(con, c("44012100", "44012200"), 201701, 201709)  

ui <- function(request){
    fluidPage(
        includeCSS("styles.css"),
        titlePanel("CSV download from the STIX database", windowTitle = "STIX-Global / eutradeflows"),
        sidebarLayout(
            sidebarPanel(
                selectInput("productgroupimm", "Product group:", 
                            choices = unique(eutradeflows::classificationimm$productgroupimm),
                            selected = "Wood"),
                selectInput("productimm", "Product :", 
                            choices = unique(eutradeflows::classificationimm$productimm),
                            selected = "Sawn: tropical hardwood"),
                radioButtons("flowcode", "Flow direction:", inline = TRUE,
                             choiceNames = c("Import", "Export"),
                             choiceValues = c(1, 2)),
                sliderInput("range", "Date range:",
                            min = as.Date('2000-01-01'), max = Sys.Date(),
                            value = c(as.Date('2012-01-01'), Sys.Date()),
                            timeFormat = "%Y%m"),
                selectizeInput("reporter", "Reporting country:", 
                            choices = reportertable$reporter,
                            selected = "Utd. Kingdom",
                            multiple = TRUE),
                selectizeInput("partner", "Partner country:", 
                            choices = partnertable$partner, 
                            selected = "Cameroon",
                            multiple = TRUE),
                bookmarkButton(),
                helpText("Information on the current selection"),
                textOutput("info"),
                textOutput("rowsfilteredtable"),
                width = 3
            ),
            mainPanel(
                a(href="#productdescription","Product Description (below)"),
                h2("Data Table"),
                radioButtons("tableaggregationlevel",
                             "Choose a table aggregation level (flags only visible in the 8 digit table):",
                             inline = TRUE,
                             choiceNames = c("IMM product aggregates (long)",
                                             "IMM product aggregates (wide)",
                                             "8 digit product codes (long)"),
                             choiceValues = c("imml", "immw", "8dl"),
                             selected = "immw"),
                downloadButton('download', 'Download this table'),
                tableOutput("table"),
                h2("Product Description"),
                tableOutput("productdescription")
            )
        )
    )
}    


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
#' @param RMySQLcon MySQL connection 
#' @param productcode_ character product code
#' @param periodmin numeric period start
#' @param periodmax numeric period end
#' @param flowcode_ numeric flow code (1 for import, 2 for export)
#' @examples 
#' if(interactive){
#' con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows")
#' # Use full code
#' swd <- loadvldcomextmonhtly(con, c("44071015", "44071031", "44071033", "44071038", "44071091", "44071093", "44071098"), 201701, 201709)  
#' chips <- loadvldcomextmonhtly(con, c("44012100", "44012200"), 201701, 201709)  
#' }
loadvldcomextmonhtly <- function(RMySQLcon,
                                 productcode_, 
                                 periodmin, 
                                 periodmax,
                                 flowcode_){
    remote <- tbl(RMySQLcon, "vld_comext_monthly") %>% 
        filter(productcode %in% productcode_ & 
                   flowcode == flowcode_ & 
                   period >= periodmin & period<=periodmax) %>% 
        addproreppar2tbl(RMySQLcon, .) 
    show_query(remote)
    collect(remote) 
        # # These operations should be performed in the cleaning procedure
        # Remove Trailing Whitespace in country names 
        # mutate(reporter = trimws(reporter),
        #        partner = trimws(partner))
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
    
    con <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows")
    swd <- tbl(con, "vld_comext_monthly") %>% 
        filter(productcode == "44071098" & 
                   period %in% c(201501L, 201502L)) %>% 
        addproreppar2tbl(con,.) %>% 
        collect()
    swd2 <- swd %>% 
        select(-quantityraw, -productdescription) %>% # Remove quantityraw and description
        # Gather along all columns except quantity, tradevalue and weight
        gather(variable, value, quantity, tradevalue, weight) %>% 
        # Rupert: start with value: tradevalue, weight, quantity
        # v, mt, 
        # v, mt, q
        left_join(data_frame(variable = c("tradevalue", "quantity", "weight"),
                             variable2 = c("v","q","w")), by="variable") %>% 
        unite(varperiod, variable2, period, sep="") %>% 
        select(-variable) %>% 
        spread(varperiod, value)
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
            need(input$reporter != "", "Please select at least one reporter country on the left"),
            need(input$partner != "", "Please select at least one partner country on the left")
        )
        dtf <- datasetInput() %>%
            # Aggregates could be added to the input data or calculated here?
            filter(reporter %in% input$reporter & partner %in% input$partner)
        return(dtf)
    })
    
    datasetaggregated <- reactive({
        dtf <- datasetfiltered() %>% 
            mutate(productimm = input$productimm) %>% 
            group_by(productimm, reporter, partner, period, flowcode) %>% 
            summarise(tradevalue = sum(tradevalue),
                      weight = sum(weight),
                      quantity = sum(quantity)) %>% 
            mutate(tradevalue = round(tradevalue,3),
                   weight = round(weight,3),
                   quantity = round(quantity,3))
        return(dtf)
    })
    
    
    output$info <- renderText({ 
        productimmselected <- classificationimm %>% 
            filter(productimm %in% input$productimm)
        paste(nrow(datasetInput()), "rows fetched from the database.",
              "Product group imm:", input$productgroupimm,
              ". Product imm:", input$productimm,
              ". Product codes 8 digit:",
              paste(productimmselected$productcode, collapse=", "),
              "Flow direction: ", input$flowcode)
    })
    
    output$rowsfilteredtable <- renderText({ 
        sprintf("%s rows filtered from the dataset",
                nrow(datasetfiltered()))
    })
    
    # update list of product imm based on the product group imm
    observe({
        imm <- eutradeflows::classificationimm %>%
            filter(productgroupimm == input$productgroupimm) %>%
            arrange(productcode) %>% 
            distinct(productimm) 
        productimm <- imm$productimm
        # If the previous selection is present in the list, reuse the same selection
        # otherwise take the first value
        if(input$productimm %in% productimm){
            productimmselected <- input$productimm
        } else {
            productimmselected <- productimm[1]
        }
        updateSelectizeInput(session, "productimm",
                             choices = productimm,
                             selected = productimmselected)
    }, priority = 2) # 
    
    # # Update list of reporting countries based on the data fetched from the database
    # observe({
    #     reporterx <- unique(datasetInput()$reporter)
    #     reporterx <- reporterx[order(reporterx)]
    #     # reporterx <- c("All EU", reporterx[!is.na(reporterx)])
    #     reporterx <- reporterx[!is.na(reporterx)]
    #     previousselection <- input$reporter 
    #     updateSelectizeInput(session, "reporter",
    #                          choices = reporterx,
    #                          selected = previousselection)
    # })
    # 
    # # Update list of partner countries based on the data fetched from the database
    # observe({
    #     partnerx <- unique(datasetInput()$partner)
    #     partnerx <- partnerx[order(partnerx)]
    #     # partnerx <- c( "All", partnerx[!is.na(partnerx)])
    #     partnerx <- partnerx[!is.na(partnerx)]
    #     previousselection <- input$partner
    #     updateSelectizeInput(session, "partner",
    #                          choices = partnerx,
    #                          selected = previousselection)
    # })

    # Separate product description from the main table
    output$productdescription <- renderTable(distinct(datasetfiltered(),
                                                      productcode,productdescription))
    
    # Spread data along the period
    spreadperiod <- function(dtf){
        dtf %>% 
            # Gather along all columns except quantity, tradevalue and weight
            gather(variable, value, quantity, tradevalue, weight) %>% 
            # Rupert: start with value: tradevalue, weight, quantity
            # v, mt, 
            # v, mt, q
            left_join(data_frame(variable = c("tradevalue", "quantity", "weight"),
                                 variable2 = c("v","q","w")), by="variable") %>% 
            unite(varperiod, variable2, period, sep="") %>% 
            select(-variable) %>% 
            spread(varperiod, value)        
    }

    datasettoshow <-  reactive({
        switch(input$tableaggregationlevel,
               "imml" = datasetaggregated(),
               "immw" = spreadperiod(datasetaggregated()),
               "8dl" = select(datasetfiltered(), -productdescription)
               )
        })
    output$table <- renderTable(datasettoshow()) 
    

    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$download <- downloadHandler(
        
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            return(paste(input$productimm, 
                         input$reporter,
                         input$partner, 
                         input$flowcode, 
                         ".csv", sep = "_", collapse ="_"))
        },
        
        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            write.csv(datasettoshow(), file, row.names = FALSE)
        }
    )
    # https://shiny.rstudio.com/articles/advanced-bookmarking.html
    # Save extra values in state$values when we bookmark
    onBookmark(function(state){
        # state$values$reporter <- input$reporter
        # state$values$partner <- input$partner
    })
    
    # Read values from state$values when we restore
    onRestore(function(state) {
        # updateSelectizeInput(session, "reporter",
        #                      choices = state$values$reporter,
        #                      selected = state$values$reporter)
        # updateSelectizeInput(session, "partner",
        #                      choices = state$values$partner,
        #                      selected = state$values$partner)
    })
    # 
    # Exclude the range input from bookmarking (because it's not working for the moment)
    setBookmarkExclude(c("range", "tableaggregationlevel"))
}

# Complete app with UI and server components
shinyApp(ui, server, enableBookmarking = "url")

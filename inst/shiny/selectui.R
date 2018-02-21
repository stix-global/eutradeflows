library(dplyr)
library(tidyr)
library(eutradeflows)

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
        titlePanel("File download from the STIX database", windowTitle = "STIX-Global / eutradeflows"),
        sidebarLayout(
            sidebarPanel(
                helpText("Information on the current selection"),
                textOutput("info"),
                
                # Input: Specification of the product
                selectInput("productgroupimm", "Choose a product group:", 
                            choices = unique(eutradeflows::classificationimm$productgroupimm),
                            selected = "Wood"
                ),
                selectInput("productimm", "Choose a product :", 
                            choices = unique(eutradeflows::classificationimm$productimm)#,
                            # selected = "Chips"
                ),
                radioButtons("flowcode", "Flow direction:", inline = TRUE,
                             choiceNames = c("Import", "Export"),
                             choiceValues = c(1, 2)),
                
                # Input: Specification of the date range
                sliderInput("range", "Date range:",
                            min = as.Date('2000-01-01'), max = as.Date('2017-09-01'),
                            value = c(as.Date('2017-01-01'),as.Date('2017-09-01')),
                            timeFormat = "%Y%m"),
                
                helpText("Change country to filter the table in your browser:"),
                textOutput("rowsfilteredtable"),
                
                # Input: Specification of the reporter country
                selectizeInput("reporter", "Choose reporting country:", 
                               choices = "All EU", selected="All EU",
                               multiple=FALSE),
                selectizeInput("partner", "Choose partner country:", 
                               choices = "All", selected="All",
                               multiple=FALSE),
                radioButtons("tableformat", "Table Format:",
                             choices = c("long", "wide")),
                radioButtons("filetype", "File type:",
                             choices = c("csv", "tsv")),
                downloadButton('downloadData', 'Download'),
                bookmarkButton(),
                width = 3
            ),
            mainPanel(
                tableOutput("productdescription"),
                tableOutput("table")
            )
        )
    )
}    

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
        return(RMySQL::dbConnect(RMySQL::MySQL(), dbname = "tradeflows"))
    }
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
    
    
    # Filter the database based on other variables country etc...
    datasetfiltered <- reactive({
        dtf <- datasetInput() %>%
            filter(reporter %in% input$reporter & partner %in% input$partner) 
        # Aggregate when all is selected
        if(input$reporter == "All EU"){
            dtf <- datasetInput() %>% 
                filter(partner %in% input$partner) %>% 
                group_by(partner) %>% 
                summarise_at(vars(tradevalue, weight, starts_with("quantity")), 
                             sum) 
        }
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
    })
    
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
    

    # separate product description from the main table
    output$productdescription <- renderTable(distinct(datasetfiltered(),
                                                      productcode,productdescription))
    output$table <- renderTable(select(datasetfiltered(), -productdescription)) 
    
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$downloadData <- downloadHandler(
        
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            return("name_not_displayed_in_file_save_dialog.csv")
            # paste(input$productimm, input$filetype, sep = ".")
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

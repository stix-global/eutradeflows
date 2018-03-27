library(dplyr)
library(tidyr)
library(eutradeflows)
library(dygraphs)
library(networkD3)

rowstodisplay <- 100
dbdocker = FALSE
dateWindow <- c("2012-01-01", as.character(Sys.Date())) # used by dygraph::dyRangeSelector

# Load country codes tables for later used by the sankey diagram function
# And to build country drop down lists
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
#
# Country grouping
# finding rows containing a value or values in any column
# https://stackoverflow.com/questions/28233561/finding-rows-containing-a-value-or-values-in-any-column
#

ui <- function(request){
    fluidPage(
        includeCSS("styles.css"),
        titlePanel("Sankey Diagram from the STIX database", windowTitle = "STIX-Global / eutradeflows"),
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
                actionButton("yearminus3", as.numeric(format(Sys.time(), "%Y")) - 3),
                actionButton("yearminus2", as.numeric(format(Sys.time(), "%Y")) - 2),
                actionButton("yearminus1", as.numeric(format(Sys.time(), "%Y")) - 1),
                actionButton("year", format(Sys.time(), "%Y")),
                sliderInput("range", "Date range:",
                            min = as.Date('2000-01-01'), max = Sys.Date(),
                            value = c(as.Date('2012-01-01'), Sys.Date()),
                            timeFormat = "%Y%m"),
                uiOutput("partnergroupselector"),
                sliderInput("numberofpartners", "Max number of partners to display",
                            min = 1, max = 100, value = 20),
                selectizeInput("partner", "Partner countries:", 
                               choices = "All", selected="All",
                               multiple = TRUE),
                selectizeInput("reporter", "Reporting countries:", 
                               choices = "All EU", selected="All EU",
                               multiple = TRUE),
                bookmarkButton(),
                helpText("Information on the current selection"),
                textOutput("info"),
                textOutput("rowsfilteredtable"),
                width = 3
            ),
            mainPanel(
                h2("Trade Value"),
                sankeyNetworkOutput("sankeytradevalue"),
                h2("Quantity"),
                sankeyNetworkOutput("sankeyquantity"),
                h2("Weight"),
                sankeyNetworkOutput("sankeyweight"),
                tableOutput("productdescription"),
                radioButtons("tableaggregationlevel",
                             "Show table of (flags only visible at the 8 digit level):",
                             inline = TRUE,
                             choiceNames = c("IMM product aggregates", "8 digit product codes"),
                             choiceValues = c("imm", "8d")),
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


# This function should remain the last object in the server.R script
server <- function(input, output, session) {
    # Prepare country groups for selectinput
    cgimm <- eutradeflows::countrygroupimm %>%
        distinct(groupcategory, group) %>% 
        arrange(group)
    # Create a named list of vectors usable in shiny::selectinput().
    # Such a named list of vectors is used in the selectinput() example section.
    # It can be created with:
    # split(cgimm$group, cgimm$groupcategory))
    output$partnergroupselector <- renderUI({
        selectInput("partnergroup", "Partner country group:",
                    split(cgimm$group, cgimm$groupcategory),
                    selected = "VPA-All")
    })
    
    
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
            need(nrow(datasetInput()) > 0, "No input data for the selected period.")
        )
        validate(
            need(input$partnergroup != "", "Please select a reporter country group")
        )
        cgimm <- eutradeflows::countrygroupimm
        dtf <- datasetInput() %>%
            # Aggregates could be added to the input data or calculated here?
            # filter(partner %in% cgimm$partnername[cgimm$group == input$partnergroup])
            filter(partner %in% input$partner & 
                       reporter %in% input$reporter)
        return(dtf)
    })
    
    datasetaggregated <- reactive({
        dtf <- datasetfiltered() %>% 
            mutate(productimm = input$productimm) %>% 
            group_by(productimm, reporter, reportercode, partner, partnercode, flowcode) %>% 
            summarise(tradevalue = sum(tradevalue),
                      weight = sum(weight),
                      quantity = sum(quantity)) 
        
        return(dtf)
    })
    
    datasetsankeynodes <- reactive({
        # Import
        if(input$flowcode == 1){ 
            dtf <- datasetaggregated() %>% 
                # Remove strange country codes from the data
                filter(partnercode != 1010 & partnercode != 1011 & 
                           reportercode !=0) %>% 
                preparesankeynodes(reportertable, partnertable)
            return(dtf)
        }
        
        # Export invert reporter and partner
        if(input$flowcode == 2){ 
            # invert reporter and partner
            partnertable2 <- partnertable %>% 
                rename(reportercode  = partnercode,
                       reporter = partner)
            reportertable2 <- reportertable %>% 
                rename(partnercode = reportercode,
                       partner = reporter)
            
            dtf <- datasetaggregated() %>% 
                # Remove strange country codes from the data
                filter(partnercode != 1010 & partnercode != 1011 & 
                           reportercode !=0) %>% 
                # Ungroup to avoid Error:
                # Column `partnercode` can't be modified because it's a grouping variable
                ungroup() %>% 
                # invert reporter and partner
                mutate(partnercode_before = partnercode,
                       partnercode = reportercode,
                       reportercode = partnercode_before) %>% 
                preparesankeynodes(reporter = partnertable2,
                                   partner =  reportertable2)
            return(dtf)
        }

    })
    
    output$info <- renderText({ 
        productimmselected <- classificationimm %>% 
            filter(productimm %in% input$productimm)
        paste("input$flowcode",input$flowcode,
              nrow(datasetInput()), "rows fetched from the database.",
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
    
    # Update slider input based on the year buttons 
    yearrange <- function(minus){
        inputyear <- as.numeric(format(Sys.time(), "%Y")) - minus
        return(c(as.Date(paste0(inputyear, "-01-01")),
                 as.Date(paste0(inputyear, "-12-31"))))
    }
    observe({
        input$year
        updateSliderInput(session, "range", value = yearrange(minus = 0))
    })
    observe({
        input$yearminus1
        updateSliderInput(session, "range", value = yearrange(minus = 1))
    })
    observe({
        input$yearminus2
        updateSliderInput(session, "range", value = yearrange(minus = 2))
    })
    observe({
        input$yearminus3
        updateSliderInput(session, "range", value = yearrange(minus = 3))
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
    }, priority = 2) # 
    
    # Update list of reporting countries based on the data fetched from the database
    observe({
        reporterx <- unique(datasetInput()$reporter)
        reporterx <- reporterx[order(reporterx)]
        reporterx <- reporterx[!is.na(reporterx)]
        updateSelectizeInput(session, "reporter",
                             choices = reporterx,
                             selected = reporterx)
        
    })
    
    # Update list of partner countries based on the country group selected
    observe({
        validate(
            need(input$partnergroup != "", "Please select a reporter country group")
        )
        cgimm <- eutradeflows::countrygroupimm
        dtf <- datasetInput() %>%
            distinct(partner, partnercode) %>% 
            filter(partnercode %in% cgimm$partnercode[cgimm$group == input$partnergroup]) 
        updateSelectizeInput(session, "partner",
                             choices = dtf$partner,
                             selected = dtf$partner)
    })
    
    # Update list of partner countries based on the max number of partners selected
    observe({
        validate(
            need(nrow(datasetInput()) > 0, "No input data for the selected period.")
        )
        validate(
            need(input$partnergroup != "", "Please select a reporter country group")
        )
        
        cgimm <- eutradeflows::countrygroupimm
        # Get a list of the n largest partner countries, in terms of trade value
        dtf <- datasetInput() %>%
            # Aggregates could be added to the input data or calculated here?
            # filter(partner %in% cgimm$partnername[cgimm$group == input$partnergroup])
            filter(partnercode %in% cgimm$partnercode[cgimm$group == input$partnergroup] & 
                       reporter %in% input$reporter) %>% 
            mutate(productimm = input$productimm) %>% 
            group_by(productimm, partner, partnercode, flowcode) %>% 
            summarise(tradevalue = sum(tradevalue),
                      weight = sum(weight),
                      quantity = sum(quantity)) %>% 
            # Order the list of partners by decreasing tradevalue 
            arrange(desc(tradevalue)) %>%
            ungroup() 
        
        # Get the max number of partner countries
        updateSliderInput(session, "numberofpartners", max = nrow(dtf))
        
        # Keep only the n largest countries
        dtf <- dtf %>% 
            slice(1:input$numberofpartners)
        updateSelectizeInput(session, "partner",
                             selected = dtf$partner)
    })
    
    
    
    # Draw Sankey diagrams for 3 variables: tradevalue, weight and quantity 
    output$sankeytradevalue <- renderSankeyNetwork({
        datasetsankeynodes() %>% 
            plotsankey(value = "tradevalue", units = "K€")
    })
    
    output$sankeyweight <- renderSankeyNetwork({
        datasetsankeynodes() %>% 
            plotsankey(value = "weight", units = "t")
    })
    
    output$sankeyquantity <- renderSankeyNetwork({
        datasetsankeynodes() %>% 
            plotsankey(value = "quantity", 
                       units = as.character(unique(select(datasetfiltered(),unitcode))))
    })
    
    # Separate product description from the main table
    output$productdescription <- renderTable(distinct(datasetfiltered(),
                                                      productcode,productdescription))
    
    # 
    datasettoshow <-  reactive({
        if(input$tableaggregationlevel == "imm"){
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

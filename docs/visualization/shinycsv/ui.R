

fluidPage(
    titlePanel("File download from the STIX database", windowTitle = "STIX-Global / eutradeflows"),
    sidebarLayout(
        sidebarPanel(
            helpText("Change product or date range to fetch data from the database.",
                     "Number of rows loaded from the database:"),
            textOutput("rowsinputtable"),
            
            # Input: Specification of the product
            selectInput("productcode", "Choose a product group:", 
                        choices = c("Sawnwood", "Plywood", "Kitchen Furniture")),
            
            # Input: Specification of the date range
            sliderInput("range", "Date range:",
                        min = as.Date('2000-01-01'), max = as.Date('2017-09-01'),
                        value = c(as.Date('2017-01-01'),as.Date('2017-09-01')),
                        timeFormat = "%Y%m"),
            helpText("Change country to filter the table in your browser:"),
            helpText("Number of rows filtered in your browser:"),
            textOutput("rowsfilteredtable"),
            
            # Input: Specification of the reporter country
            selectizeInput("reporter", "Choose reporting countries:", 
                           choices = NULL, multiple=TRUE),
            radioButtons("filetype", "File type:",
                         choices = c("csv", "tsv")),
            downloadButton('downloadData', 'Download'),
            width = 3
        ),
        mainPanel(
            DT::dataTableOutput('table')
        )
    )
)

# 
# if(FALSE){
#     # Minimal example tying to solve title page modification issue
#     # displays <div id="selected_var" in the title
#     # [Display reactive output tutorial](https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/)
#     #
#     # Related question
#     # https://stackoverflow.com/questions/30876044/shiny-output-not-enclosed-in-tags-is-it-possible
#     shinyApp(ui = fluidPage(titlePanel(textOutput("selected_var")),
#                             sidebarLayout(
#                                 sidebarPanel(
#                                     selectInput("fruit", "Choose a fruit:", 
#                                                 choices = c("Apple", "Pear",
#                                                             "Banana"))),
#                                 mainPanel(
#                                     
#                                     textOutput("selected_var")
#                                 ))),
#              server = function(input, output) {
#                  output$selected_var <- renderText({ 
#                      paste("The", input$fruit, "page")
#                  })
#              }
#     )
# }


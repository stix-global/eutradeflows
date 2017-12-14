
fluidPage(
    titlePanel('File download'),
    sidebarLayout(
        sidebarPanel(
            selectInput("productcode", "Choose a product code:", 
                        choices = c("Sawnwood", "Plywood", "Kitchen Furniture")),
            radioButtons("filetype", "File type:",
                         choices = c("csv", "tsv")),
            downloadButton('downloadData', 'Download')
        ),
        mainPanel(
            tableOutput('table')
        )
    )
)


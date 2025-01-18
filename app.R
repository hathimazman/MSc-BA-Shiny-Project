#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application
ui <- 
  fluidPage(
    titlePanel("Stock Market Analysis"),
    p("This app will allow users to analyse stock market data and provide insight on time series analysis of the selected stock"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "algorithm", 
                     label = "1. Select preferred algorithm", 
                     choices = c("ARIMA", "TBATS"), selected = "ARIMA"),
        
        dateRangeInput(inputId = 'dateRange',
                       label = '2. Select date range',
                       start = Sys.Date() - 2, end = Sys.Date() + 2),
        
        dateInput('date_input',
                  label = '3. Select month and year to highlight',
                  value = Sys.Date(),
                  format = 'yyyy-mm',
                  startview = 'year')
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("Stock Summary", verbatimTextOutput("summary")),
                    tabPanel("Time-Series Analysis", tableOutput("decompose"))
        )
      )
    )
  )

# Define server logic 
server <- function(input, output, session) {}

# Run the application 
shinyApp(ui = ui, server = server)

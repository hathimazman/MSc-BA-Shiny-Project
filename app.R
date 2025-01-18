library(shiny)
library(tidyverse)
library(forecast)
library(mice)
library(zoo)
library(lubridate)
library(yfR)

# Define UI for application
ui <- 
  fluidPage(
    titlePanel("Stock Market Analysis"),
    p("This app will allow users to analyse stock market data and provide insight on time series analysis of the selected stock"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "ts_algo", 
                     label = "1. Select preferred algorithm", 
                     choices = c("ARIMA", "TBATS"), selected = "ARIMA"),
        
        dateRangeInput(inputId = 'dateRange',
                       label = '2. Select date range',
                       start = '2010-01-01', end = Sys.Date()),
        
        dateInput('date_input',
                  label = '3. Select month and year to highlight',
                  value = Sys.Date(),
                  format = 'yyyy-mm',
                  startview = 'year'),
        
        dateRangeInput(inputId = 'date_highlight',
                       label = '4. Select date range to highlight',
                       start = '2010-01-01', end = Sys.Date()),
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("ts_plot")),
                    tabPanel("Stock Summary", verbatimTextOutput("dateRangeText")),
                    tabPanel("Time-Series Analysis", tableOutput("decompose"))
        )
      )
    )
  )

# Define server logic 
server <- function(input, output) {
  
  # Reactive values for the date range
  first_date <- reactive({ as.Date(input$dateRange[1]) })
  last_date <- reactive({ as.Date(input$dateRange[2]) })
  
  
  ########------------------------------
  highlight_date_first = reactive({ as.Date(input$date_highlight[1]) })
  highlight_date_last = reactive({ as.Date(input$date_highlight[2]) })
  
  highlight_start <- reactive({
    year(highlight_date_first()) + (month(highlight_date_first()) - 1) / 12
  })
  
  highlight_end <- reactive({
    year(highlight_date_last()) + (month(highlight_date_last()) - 1) / 12
  })
  ########------------------------------
  
  # Load data reactively
  df_yf <- reactive({
    yf_get(tickers = '^KLSE', 
           first_date = first_date(),
           last_date = last_date())
  })
  
  # Convert daily to monthly data
  df <- reactive({
    df_yf() %>%
      group_by(year = year(ref_date), month = month(ref_date)) %>%
      summarise(price = mean(price_close, na.rm = TRUE))
  })
  
  # Create time series object
  df_ts <- reactive({
    ts(df()$price, start = c(min(df()$year), 1), frequency = 12)
  })
  
  # Choose time series method
  forecasted_data <- reactive({
    if (input$ts_algo == "TBATS") {
      forecast(auto.arima(df_ts()))
    } 
    else {
      forecast(tbats(df_ts()))
    }
  })
  
  # Plot data
  output$ts_plot <- renderPlot({
    plot(forecasted_data(), main = 'FBMKLCI Time Series Analysis', 
         ylab = 'Price', xlab = 'Date',
         xlim = c(highlight_start, highlight_end)   ########------------------------------
         )
  })
  
  # Display the selected date range
  output$dateRangeText <- renderText({
    paste("Selected date range:", first_date(), "to", last_date())
  })
  
  # Decompose the time series
  output$decompose <- renderTable({
    decompose(df_ts())
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(forecast)
library(lubridate)
library(yfR)
library(plotly)

# Define UI for application
ui <- 
  fluidPage(
    tags$style(HTML("
      body {
        background-color: #F9F7F7;
      }
    ")),
    div(style = "background-color: #72383D; color: white; padding: 15px; margin: 10px; border-radius: 5px;",
        titlePanel("Stock Market Analysis Dashboard 2000-2025"),
    p("This dashboard will allow users to analyse stock market data using data from the past 25 years and provide insight on time series analysis of the selected stock."),
    p("This data was sourced from Yahoo Finance using the `yfR` package. The data is then converted to monthly data and analysed using the ARIMA and TBATS algorithms."),
    br(),
    p("Users can select the stock ticker symbol, the algorithm to use and the date range for analysis")
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "ticker", 
                  label = "1. Enter stock ticker symbol",
                  choices = c('^KLSE','^NDX','^GSPC','^HSI','^N225','^DJI'),
                  selected = "^KLSE"),
        p("Further details on tickers can be obtained at:"), 
        tags$a(href='https://finance.yahoo.com/lookup/', 'Yahoo Finance'),
        br(),
        br(),
        radioButtons(inputId = "ts_algo", 
                    label = "2. Select preferred algorithm", 
                    choices = c("ARIMA", "TBATS"), 
                    selected = "ARIMA"),

        dateRangeInput(inputId = 'dateRange',
                      label = '3. Select date range to visualize \n (Select future dates to see forecast of price)',
                      start = '2000-01-01', 
                      end = Sys.Date(),
                      format = 'yyyy-mm',
                      startview = 'year'),
        sliderInput("integer", "4. How many months to forecast price?",
                  min = 12, max = 36,
                  value = 12),
        br(),

        fluidRow(
          column(12,
                 div(style = "background-color: #CEDDBB; color: black; font-size: 18px; padding: 10px; margin: 10px; border-radius: 5px;",
                     p("Guide:"),
                     div(style = "background-color: #CEDDBB; color: black; font-size: 10px; padding: 10px; margin: 10px; border-radius: 5px;",
                      p(paste("Current Price : Current price of the stock as of ", Sys.Date())),
                      p("Latest Change (%) : Difference in closing price between today and yesterday"),
                      p("Latest Volume : Total number of shares traded today"),
                      p("YTD Performance : Year-to-date performance of the stock")),
                    div(style = "background-color: #CEDDBB; color: blue; font-size: 10px; padding: 10px; margin: 10px; border-radius: 5px;",
                      p("How to choose between ARIMA and TBATS:"),
                      p("ARIMA is a univariate time series model that works well with data that has a clear trend and seasonality."),
                      p("TBATS is a more complex model that can handle multiple seasonalities and is more robust to outliers."))
              )         
          )
        ),

        fluidRow(
          column(12,
                 div(style = "background-color: #D2DCE6; color: black; font-size: 16px; padding: 10px; margin: 10px; border-radius: 5px;",
                     p("This dashboard was created by:"),
                     tags$a(href = "https://github.com/hathimazman", "Hathim Azman", target = '_blank'),
                     br(),
                     tags$a(href = "https://github.com/Hazim-HF", "Hazim Fitri", target = '_blank')
                 )
          )
        )
        
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot",
                             fluidRow(
                               column(12, uiOutput('kpi_cards'))
                             ),
                             fluidRow(
                               column(12, plotlyOutput("ts_plot"))
                             ),
                             fluidRow(
                               column(12,plotlyOutput("volume_plot"))
                             )
                    ),
                    tabPanel("Time-Series Analysis",
                            fluidRow(
                              column(12, 
                                    plotlyOutput("seasonal_plot"))
                            ),
                            fluidRow(
                              column(12,
                                    div(style = "background-color: #AB644B; color: white; font-size: 18px; padding: 10px; margin: 10px; border-radius: 5px;",
                                        textOutput("seasonal_text")
                                    )
                              )
                            ),
                            fluidRow(
                              column(12,
                              plotOutput("trend_plot"))
                            ),
                            fluidRow(
                              column(12,
                                    div(style = "background-color: #AB644B; color: white; font-size: 18px; padding: 10px; margin: 10px; border-radius: 5px;",
                                        textOutput("trend_text")
                                    )
                              )
                            )
                      )
          )
      )
    )
  )

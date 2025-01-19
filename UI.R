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
    p("This dashboard will allow users to analyse stock market data using data from the past 25 years and provide insight on time series analysis of the selected stock"),
    br(),
    p("This data was sourced from Yahoo Finance using the yfR package \n The data is then converted to monthly data and analysed using the ARIMA and TBATS algorithms"),
    br(),
    p("Users can select the stock ticker symbol, the algorithm to use and the date range for analysis"),
    sidebarLayout(
      sidebarPanel(
        textInput(inputId = "ticker", 
                  label = "1. Enter stock ticker symbol",
                  value = "^KLSE"),
        radioButtons(inputId = "ts_algo", 
                    label = "2. Select preferred algorithm", 
                    choices = c("ARIMA", "TBATS"), 
                    selected = "ARIMA"),

        dateRangeInput(inputId = 'dateRange',
                      label = '3. Select date range for analysis',
                      start = '2000-01-01', 
                      end = Sys.Date(),
                      format = 'yyyy-mm',
                      startview = 'year')
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("ts_plot")),
                    tabPanel("Stock Summary",
                            fluidRow(
                              column(12, verbatimTextOutput("dateRangeText"))
                            )
                    ),
                    tabPanel("Time-Series Analysis",
                            fluidRow(
                              column(12, 
                                    plotOutput("seasonal_plot"))
                            ),
                            fluidRow(
                              column(12,
                                    div(style = "background-color: #77B254; color: white; font-size: 18px; padding: 10px; margin: 10px; border-radius: 5px;",
                                        textOutput("seasonal_text")
                                    )
                              )
                            ),
                            fluidRow(
                              column(12,
                              plotOutput("trend_plot"))
                            )
                      )
          )
      )
    )
  )

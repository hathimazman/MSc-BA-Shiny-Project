library(tidyverse)
library(forecast)
library(mice)
library(zoo)
library(lubridate)
library(yfR)

server = function(input, output) {
  
  # Load data
  my_ticker <- '^KLSE'
  first_date <- input$first_date
  last_date <- input$last_date
  
  df_yf <- yf_get(tickers = my_ticker, 
                  first_date = first_date,
                  last_date = last_date)
  
  # Convert daily to monthly data
  df = df_yf %>%
    group_by(year(ref_date), month(ref_date)) %>%
    summarise(price = mean(price_close))
  
  df_ts = ts(df$price, start=c(2000,1), frequency=12)
  
  # Choose time series method
  if (input$ts_algo == "ARIMA") {
    x = forecast(auto.arima(df_ts))
  } else {
    x = forecast(tbats(df_ts))
  }
  
  # Plot data
  output$ts_plot = plot(x, main = 'FBMKLCI Time Series Analysis using TBATS',
                        ylab = 'Price', xlab = 'Date')
  
  # KPI card
  output$kpi_mom = diff(input$mom)
}
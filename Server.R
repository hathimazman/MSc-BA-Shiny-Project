library(tidyverse)
library(forecast)
library(mice)
library(lubridate)
library(yfR)

server <- function(input, output) {
  
  # Load data reactively with error handling
  df_yf <- reactive({
      yf_get(tickers = input$ticker, 
             first_date = "2000-01-01",
             last_date = Sys.Date())
             })
  
  # Convert daily to monthly data
  df <- reactive({
    df_yf() %>%
      group_by(year = year(ref_date), month = month(ref_date)) %>%
      summarise(price = mean(price_close, na.rm = TRUE))
  })
  
  # Create a zoo object (time series with Date index)
  df_ts <- reactive({
    ts(df()$price, start = c(min(df()$year), 1), frequency = 12)
  })
  
  # Choose time series method
  forecasted_data <- reactive({
      if (input$ts_algo == "TBATS") {
        forecast(tbats(df_ts()))
      } 
      else {
        forecast(auto.arima(df_ts()))
      }
    })
  
  # Plot data
  output$ts_plot <- renderPlot({
    req(forecasted_data())
    plot(forecasted_data(), 
         main = paste(input$ticker, "Time Series Analysis Year '00 - '25"), 
         ylab = 'Price', 
         xlab = 'Date')
  })
  
  # Display the selected date range
  output$dateRangeText <- renderText({
    req(input$dateRange)
    paste("Analysis Period:", format(input$dateRange[1], "%B %Y"),
          "to", format(input$dateRange[2], "%B %Y"))
  })

  # Plot the seasonal data
  output$seasonal_plot <- renderPlot({
    # Create a seasonal data
    seasonal_data <- decompose(df_ts())$seasonal

    # Create seasonal data frame
    seasonal_df <- data.frame(time = as.numeric(time(seasonal_data)),
                                seasonal = as.numeric(seasonal_data)) %>%
                                filter(time >= (year(Sys.Date()) - 1) & time <= year(Sys.Date()))
    
    # Find the time corresponding to the lowest point of the seasonal component
    lowest_time <- seasonal_df$time[which.min(seasonal_df$seasonal)]

    autoplot(seasonal_data) +
      ggtitle("Seasonal Decomposition") +
      xlim(c(year(Sys.Date()) - 1, year(Sys.Date()))) +  # Adjust the x-axis limits
      geom_vline(xintercept = lowest_time, color = "red", linetype = "dotted") +  # Add vertical line +
      annotate("text", x = lowest_time, y = min(seasonal_df$seasonal), label = "Lowest Point of Year\nBest entry point", vjust = -10) +  # Add text annotation
      labs(x = "Time", y = "Seasonal Component") +
      theme_minimal()
  })

  # Seasonal text
  output$seasonal_text <- renderText({
    # Create a seasonal data
    seasonal_data <- decompose(df_ts())$seasonal

    # Create seasonal data frame
    seasonal_df <- data.frame(time = as.numeric(time(seasonal_data)),
                                seasonal = as.numeric(seasonal_data)) %>%
                                filter(time >= (year(Sys.Date()) - 1) & time <= year(Sys.Date()))
    
    # Find the time corresponding to the lowest point of the seasonal component
    lowest_time <- seasonal_df$time[which.min(seasonal_df$seasonal)]

    mth = round((lowest_time %% 1) * 12) 

    date = lowest_time
    paste("The lowest point of the seasonal component occurs in the month of", month(mth, label = TRUE))
  })

  # Plot the trend data
  output$trend_plot <- renderPlot({
    # Create a trend data
    trend_data <- decompose(df_ts())$trend
    
    autoplot(trend_data) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      ggtitle("Trend Decomposition") +
      labs(x = "Time", y = "Trend Component") +
      theme_minimal()
  })
  
  # Trend Text
  output$trend_text <- renderText({
    # Create a trend data
    trend_data <- decompose(df_ts())$trend

    # Create trend data frame
    trend_df <- data.frame(time = as.numeric(time(trend_data)),
                                trend = as.numeric(trend_data)) %>%
                                filter(time >= (year(Sys.Date()) - 1) & time <= year(Sys.Date()))
    
    # create a linear model
    lm_model <- lm(trend ~ time, data = trend_df)
    
    if (lm_model$coefficients[2] > 0) {
      paste("The trend is increasing")
    } else {
      paste("The trend is decreasing")
    }
    
  })
}
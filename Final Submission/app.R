# Install Required libraries
install.packages('tidyverse')
install.packages('forecast')
install.packages('lubridate')
install.packages('yfR')
install.packages('plotly')
install.packages('shiny')

# Load Libraries
library(tidyverse)
library(forecast)
library(lubridate)
library(yfR)
library(plotly)
library(shiny)

# Define UI
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
        div(style = "background-color: #CEDDBB; color: grey; font-size: 12px; padding: 10px; margin: 10px; border-radius: 5px;",
            p("Selected Stock:"),
            textOutput("stock_info"),
            br(),
            p("Further details on tickers can be obtained at:"), 
            tags$a(href='https://finance.yahoo.com/markets/world-indices/?guce_referrer=aHR0cHM6Ly9naXRodWIuY29tL3JvcGVuc2NpL3lmUg&guce_referrer_sig=AQAAAAviOKFwn0EbfBgcZmifeubXi_KnuezrMaHmWgCYFwP6SN4Z2736sUmr-RxVpc7tTOlYx9glnWlJyrw8EfEdE9aEzRkx1B1crppLCB6aWXCLoCvZPHuWCop52wGHPFcKo-r5kEF3ZdLJJs4pDhtYSSylUnhl55Nr34KQcMGfAo5O', 'Yahoo Finance')),
        br(),
        br(),
        radioButtons(inputId = "ts_algo", 
                    label = "2. Select preferred algorithm", 
                    choices = c("ARIMA", "TBATS"), 
                    selected = "ARIMA"),

        dateRangeInput(inputId = 'dateRange',
                      label = '3. Select date range to visualize \n (Select future dates to see forecast of price)',
                      start = '2000-01-01', 
                      end = Sys.Date() + 365,
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
                 ),
                 br(),
                 tags$a(href='https://github.com/ropensci/yfR', 'Link to yfR Package Documentation in Github.')
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

# Define Server
server <- function(input, output) {
  
  # Load data reactively with error handling
  df_yf <- reactive({
    yf_get(tickers = input$ticker, 
           first_date = "2000-01-01",
           last_date = Sys.Date())
  })

  # KPI Calculation
  current_price <- reactive({
    tail(df_yf()$price_close, 1)
  })
  
  daily_change <- reactive({
    if (nrow(df_yf()) > 1) {
      last_close <- tail(df_yf()$price_close, 1)
      previous_close <- tail(df_yf()$price_close, 2)[1]
      return(last_close - previous_close)
    } else {
      return(NA)
    }
  })
  
  daily_change_percent <- reactive({
    if (nrow(df_yf()) > 1) {
      (daily_change() / tail(df_yf()$price_close, 2)[1]) * 100
    } else {
      NA
    }
  })
  
  daily_volume <- reactive({
    tail(df()$volume, 1)
  })
  
  ytd_performance <- reactive({
    if(nrow(df_yf()) > 1) {
    first_day_of_year <- as.Date(paste0(year(Sys.Date()), "-01-01"))
    ytd_close = df_yf() %>% filter(ref_date >= first_day_of_year) %>% select(ref_date, price_close)
    ytd_start_price = ytd_close$price_close[1]
    current_price = tail(df_yf()$price_close, 1)
    ytd_change = (current_price - ytd_start_price) / ytd_start_price * 100
    return(round(ytd_change,2))
    } else {
      return(NA)
    }
  })

  # Stock Information
  output$stock_info <- renderText({
    if(input$ticker == '^KLSE') {
      "Kuala Lumpur Stock Exchange (KLSE)"
    } else if(input$ticker == '^NDX') {
      "Nasdaq 100 Index"
    } else if(input$ticker == '^GSPC') {
      "S&P 500 Index"
    } else if(input$ticker == '^HSI') {
      "Hang Seng Index"
    } else if(input$ticker == '^N225') {
      "Nikkei 225 Index"
    } else if(input$ticker == '^DJI') {
      "Dow Jones Industrial Average"
    }
  })
  
  # KPI Card
  output$kpi_cards <- renderUI({
    fluidRow(
      column(3, 
             div(style = "background-color: #112D4E; padding: 10px; border-radius: 5px; color: white;",
                 h4("Current Price"),
                 h3(paste0("$", round(current_price(), 2)))
             )
      ),
      column(3, 
             div(style = paste0("background-color: ",
                                ifelse(daily_change() < 0, "red", "green"),
                                "; padding: 10px; border-radius: 5px; color: white;"),
                 h4("Latest Change"),
                 h3(paste0(round(daily_change(), 2), " (", round(daily_change_percent(), 2), "%)"))
             )
      ),
      column(3, 
             div(style = "background-color: #112D4E; padding: 10px; border-radius: 5px; color: white;",
                 h4("Latest Volume"),
                 h3(format(daily_volume(), big.mark = ","))
             )
      ),
      column(3, 
             div(style = paste0("background-color: ",
                 ifelse(ytd_performance() < 0, "red", "green"),
                 "; padding: 10px; border-radius: 5px; color: white;"),
                 h4("YTD Performance"),
                 h3(paste0(round(ytd_performance(), 2), "%"))
             )
      )
    )
  })
  
  # Reactive DataFrame
  df <- reactive({
    df_yf() %>%
      group_by(year = year(ref_date), month = month(ref_date)) %>%
      summarise(
        close = mean(price_close, na.rm = TRUE),
        open = mean(price_open, na.rm = TRUE),
        high = mean(price_high, na.rm = TRUE),
        low = mean(price_low, na.rm = TRUE),
        volume = mean(volume, na.rm = TRUE)
      ) %>%
      mutate(
        date = as.Date(paste(year, month, "01", sep = "-")),
        volume_legend = "Volume"
      )
  })
  
  
  
  # Reactive Time Series
  df_ts <- reactive({
    ts(df()$close, start = c(min(df()$year), 1), frequency = 12)
  })

  
  # Reactive Forecast
  forecasted <- reactive({
    if (input$ts_algo == "TBATS") {
      forecast(tbats(df_ts()), h = input$integer)
    } else {
      forecast(auto.arima(df_ts()), h = input$integer)
    }
  })
  
  # Forecast Data Frame
  forecast_data <- reactive({
    forecast_df <- data.frame(
      date = seq(max(df()$date) + months(1), by = "month", length.out = input$integer),
      forecast = forecasted()$mean,
      lower_80 = forecasted()$lower[, 1],
      upper_80 = forecasted()$upper[, 1],
      lower_95 = forecasted()$lower[, 2],
      upper_95 = forecasted()$upper[, 2]
    )
    
    forecast_data <- forecast_df %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
  })
  
  # Plot Data
  output$ts_plot <- renderPlotly({
    p <- ggplot() +
      # Candlestick chart (price data)
      geom_segment(
        data = df(),
        aes(x = date, xend = date, y = low, yend = high),
        color = "black",
        alpha = 0.5
      ) +
      # Candlestick chart (price data)
      geom_rect(
        data = df(),
        aes(
          xmin = date - 10,
          xmax = date + 10,
          ymin = pmin(open, close),
          ymax = pmax(open, close),
          fill = ifelse(close >= open, "bullish", "bearish")
        )
      ) +
      # Forecast lines
      geom_line(
        data = forecast_data(),
        aes(x = date, y = forecast),
        color = "blue",
        linetype = "dotted"
      ) +
      # Cofidence Interval 95% ribbons
      geom_ribbon(
        data = forecast_data(),
        aes(x = date, ymin = lower_95, ymax = upper_95),
        fill = "blue",
        alpha = 0.1
      ) +
      # Cofidence Interval 80% ribbons
      geom_ribbon(
        data = forecast_data(),
        aes(x = date, ymin = lower_80, ymax = upper_80),
        fill = "blue",
        alpha = 0.2
      ) +
      # Customizations
      scale_fill_manual(
        values = c(bullish = "green", bearish = "red")
      ) +
      scale_y_continuous(
        name = "Price",  # Keep the label "Price"
        expand = expansion(mult = c(0.05, 0.05))  
      ) +
      scale_x_date(
        name = "Date",
        limits = as.Date(c(input$dateRange[1], input$dateRange[2])),  
        date_breaks = "6 month",  
        date_labels = "%b %Y"     
      ) +
      labs(
        title = "Monthly Candlestick Chart with Forecast",
        x = "none"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",  # This hides the legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      ) +
      guides(fill = "none")  # Explicitly remove the fill legend
    
    # Return interactive plot
    plotly::ggplotly(p)
  })
  
  # Volume Plot
  output$volume_plot <- renderPlotly({
    # Create a volume plot
    p <- ggplot() +
      # Volume bar chart
      geom_col(
        data = df(),
        aes(x = date, y = volume),
        fill = "grey",
        alpha = 0.5
      ) +
      # Adjust y axis
      scale_y_continuous(
        name = "Volume"
      ) +
      # Set x-axis limits
      scale_x_date(
        name = "Date",
        limits = as.Date(c(input$dateRange[1], input$dateRange[2])),
        date_breaks = "6 month",
        date_labels = "%b %Y"
      ) +
      # Additional customizations
      labs(
        title = "Monthly Volume Chart",
        x = "Date"
      ) +
      # Theme
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
    
    # Return interactive plot
    plotly::ggplotly(p)
  })
  
  
  # Plot the seasonal data
  output$seasonal_plot <- renderPlotly({
    # Create a seasonal data
    seasonal_data <- decompose(df_ts())$seasonal
    
    # Create seasonal data frame
    seasonal_df <- data.frame(
      time = time(seasonal_data),
      seasonal = as.numeric(seasonal_data)
    ) %>% filter(time >= (year(Sys.Date()) - 1) & time <= year(Sys.Date()))
    
    # Create a date sequence for the x-axis
    last_year <- year(Sys.Date())-1
    seasonal_df$date <- seq(from = as.Date(paste0(last_year, "-01-01")), 
                          by = "month", 
                          length.out = length(seasonal_df$seasonal))
    
    # Find the time corresponding to the lowest point of the seasonal component
    lowest_time <- seasonal_df$date[which.min(seasonal_df$seasonal)]
    
    # Plot the seasonal data
    p <- ggplot() +
      geom_line(
        data = seasonal_df,
        aes(x = date, y = seasonal)
      ) +
      ggtitle("Seasonal Decomposition for the past 1 year") +
      geom_vline(xintercept = as.numeric(lowest_time), color = "red", linetype = "dotted") +
      labs(x = "Time", y = "Seasonal Component") +
      scale_x_date(date_labels = "%b %Y", 
                  date_breaks = "1 month",
                  limits = c(min(seasonal_df$date), max(seasonal_df$date))) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
    
    # Convert to plotly
    ggplotly(p)
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
    
    # Extract the month from the lowest time
    mth = round((lowest_time %% 1) * 12 + 1) 
    date = lowest_time

    # Create text
    paste("The lowest point of the seasonal component occurs in the month of", month(mth, label = TRUE))
  })
  
  # Plot the trend data
  output$trend_plot <- renderPlot({
    # Create a trend data
    trend_data <- decompose(df_ts())$trend

    # Create trend data frame
    trend_df <- data.frame(time = as.numeric(time(trend_data)),
                           trend = as.numeric(trend_data)) %>%
      filter(time >= (year(Sys.Date()) - 1) & time <= year(Sys.Date()))
    
    # Create a linear model
    lm_model <- lm(trend ~ time, data = trend_df)

    # Create plotting based on trend direction
    if (lm_model$coefficients[2] > 0) {
      autoplot(trend_data) +
      geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "dashed") +
      ggtitle("Trend Decomposition Since 2000") +
      labs(x = "Time", y = "Trend Component") +
      theme_minimal()
    } else {
      autoplot(trend_data) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      ggtitle("Trend Decomposition Since 2000") +
      labs(x = "Time", y = "Trend Component") +
      theme_minimal()
    }
    
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
    
    # Create text based on trend direction
    if (lm_model$coefficients[2] > 0) {
      paste("The price trend since 2000 is increasing")
    } else {
      paste("The price trend since 2000 is decreasing")
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(tidyverse)
library(forecast)
library(mice)
library(zoo)
library(lubridate)
library(yfR)
library(plotly)

df_yf =  yf_get(tickers = '^KLSE', 
                first_date = "2000-01-01",
                last_date = Sys.Date())
str(df)

df = df_yf %>%
  group_by(year = year(ref_date), month = month(ref_date)) %>%
  summarise(close = mean(price_close, na.rm = TRUE),
            open = mean(price_open, na.rm=T),
            high = mean(price_high, na.rm=T),
            low = mean(price_low, na.rm=T),
            volume = mean(volume, na.rm=T)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

df_ts = ts(df$close, start = c(min(df$year), 1), frequency = 12)
str(df_ts)
arima.model = auto.arima(df_ts)
forecasted = forecast(arima.model)
str(forecasted)
forecast_data <- data.frame(
  date = seq(max(df$date) + months(1), by = "month", length.out = 12),
  forecast = forecasted$mean,
  lower_80 = forecasted$lower[, 1],
  upper_80 = forecasted$upper[, 1],
  lower_95 = forecasted$lower[, 2],
  upper_95 = forecasted$upper[, 2]
)
tail(combined_data)
combined_data <- bind_rows(
  df %>% mutate(type = "actual"),
  forecast_data %>% mutate(
    open = NA, high = NA, low = NA, volume = NA, type = "forecast"
  )
)

# Calculate scaling factor
volume_max <- max(df$volume, na.rm = TRUE)
price_max <- max(df$high, na.rm = TRUE)
scaling_factor <- price_max / volume_max

#-------------------------------------
p = ggplot() +
  # Candlestick chart (price data)
  geom_segment(data = df, aes(x = date, xend = date, y = low, yend = high), color = "black") +
  geom_rect(data = df, aes(
    xmin = date - 10, xmax = date + 10,
    ymin = pmin(open, close), ymax = pmax(open, close),
    fill = ifelse(close >= open, "bullish", "bearish")
  )) +
  # Forecast lines
  geom_line(data = forecast_data, aes(x = date, y = forecast), color = "blue", linetype = "dotted") +
  geom_ribbon(data = forecast_data, aes(
    x = date, ymin = lower_95, ymax = upper_95
  ), fill = "blue", alpha = 0.1) +
  geom_ribbon(data = forecast_data, aes(
    x = date, ymin = lower_80, ymax = upper_80
  ), fill = "blue", alpha = 0.2) +
  # Volume chart (scaled to match price)
  geom_col(data = df, aes(x = date, y = volume * scaling_factor), fill = "grey", alpha = 0.5) +
  # Customizations
  scale_fill_manual(values = c(bullish = "green", bearish = "red")) +
  scale_y_continuous(
    name = "Price",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Volume")
  ) +
  labs(
    title = "Monthly Candlestick Chart with Forecast and Volume",
    x = "Date",
    fill = "Candle Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title.y.right = element_text(color = "grey")  # Match secondary axis text color with volume
  )
  
ggplotly(p)

#----------------------------------------
# Plot with ggplot
p <- ggplot() +
  # Candlestick chart (price data)
  geom_segment(data = df, aes(x = date, xend = date, y = low, yend = high), color = "black") +
  geom_rect(data = df, aes(
    xmin = date - 10, xmax = date + 10,
    ymin = pmin(open, close), ymax = pmax(open, close),
    fill = ifelse(close >= open, "bullish", "bearish")
  )) +
  # Forecast lines
  # Fix the geom_line for forecast
  geom_line(data = forecast_data, aes(x = date, y = forecast), color = "blue", linetype = "dotted") +
  geom_ribbon(data = forecast_data, aes(
    x = date, ymin = lower_95, ymax = upper_95
  ), fill = "blue", alpha = 0.1) +
  geom_ribbon(data = forecast_data, aes(
    x = date, ymin = lower_80, ymax = upper_80
  ), fill = "blue", alpha = 0.2) +
  # Volume chart (scaled to match price)
  geom_col(data = df, aes(x = date, y = volume * scaling_factor), fill = "grey", alpha = 0.5) +
  # Customizations
  scale_fill_manual(values = c(bullish = "green", bearish = "red")) +
  scale_y_continuous(
    name = "Price",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Volume")
  ) +
  labs(
    title = "Monthly Candlestick Chart with Forecast and Volume",
    x = "Date",
    fill = "Candle Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title.y.right = element_text(color = "grey")  # Match secondary axis text color with volume
  )

# Convert to plotly and manually adjust layout
p_interactive <- ggplotly(p)

# Fix secondary axis manually in Plotly
p_interactive <- p_interactive %>%
  layout(
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = "Volume",
      tickfont = list(color = "grey"),
      showgrid = FALSE,
      rangemode = "tozero"  # Ensure proper alignment
    ),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1)
  )

# Display plot
p_interactive

# =========================================================================================================
seasonal_data <- decompose(df_ts)$seasonal

# Create a data frame with seasonal values and corresponding time
seasonal_df <- data.frame(
  time = as.numeric(time(seasonal_data)),
  seasonal = as.numeric(seasonal_data)
) %>%
  filter(time >= (year(Sys.Date()) - 1) & time <= year(Sys.Date()))


# Find the time corresponding to the lowest point
lowest_time <- seasonal_df$time[which.min(seasonal_df$seasonal)]

month(lowest_time)

month(round(lowest_time %% 12,0), label=T)

?month
0.75*12

min(seasonal_df$seasonal)

autoplot(seasonal_data) +
  ggtitle("Seasonal Decomposition") +
  xlim(c(year(Sys.Date()) - 1, year(Sys.Date()))) +  # Adjust the x-axis limits
  geom_vline(xintercept = lowest_time, color = "red", linetype = "dashed") +  # Add vertical line +
  annotate("text", x = lowest_time, y = min(seasonal_df$seasonal), label = "Lowest Point of Year\nBest entry point", vjust = -10) +  # Add text annotation
  labs(x = "Time", y = "Seasonal Component") +
  theme_minimal()

trend_data <- decompose(df_ts)$trend

autoplot(trend_data) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  ggtitle("Trend Decomposition") +
  labs(x = "Time", y = "Trend Component") +
  theme_minimal()


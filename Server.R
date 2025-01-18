library(tidyverse)
library(forecast)
library(mice)
library(zoo)
library(lubridate)
library(yfR)

my_ticker <- '^KLSE'
first_date <- "2000-01-01"
last_date <- Sys.Date()

df_yf <- yf_get(tickers = my_ticker, 
                first_date = first_date,
                last_date = last_date)

str(df_yf)

df = df_yf %>%
  group_by(year(ref_date), month(ref_date)) %>%
  summarise(price = mean(price_close))

df_ts = ts(df$price, start=c(2000,1), frequency=12)

plot.ts(df_ts)

plot(decompose(df_ts))

plot(decompose(df_ts)$seasonal, xlim=c(2011,2012))


plot(decompose(df_ts)$seasonal, xlim=c(2011,2012))

autoplot(forecast(auto.arima(df_ts)))
autoplot(forecast(tbats(df_ts), h=24))

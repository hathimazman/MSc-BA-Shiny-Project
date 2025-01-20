library(shiny)
library(tidyverse)
library(forecast)
library(mice)
library(lubridate)
library(yfR)
library(plotly)

# Source UI and server logic
source("UI.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)

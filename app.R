library(shiny)
library(tidyverse)
library(forecast)
library(mice)
library(zoo)
library(lubridate)
library(yfR)

# Source UI and server logic
source("UI.R")
source("Server.R")

# Run the application 
shinyApp(ui = ui, server = server)

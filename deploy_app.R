# Shiny app for dashboard
# clear session
rm(list = ls())

# Host on server ?
library(rsconnect)
rsconnect::deployApp('Do')

# Run locally
library(shiny)
runApp('Do')

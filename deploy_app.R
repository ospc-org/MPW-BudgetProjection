# Shiny app for dashboard
# clear session
rm(list = ls())

# Host on server ?
library(rsconnect)
rsconnect::deployApp('mpw_dashboard')

# Run locally
library(shiny)
runApp('mpw_dashboard')

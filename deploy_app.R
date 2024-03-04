# Shiny app for dashboard
# clear session
rm(list = ls())

# # Load Packages
# library(shiny)
# library(ggplot2)
# library(ggiraph)
# library(ggthemes)
# library(tidyverse)
# library(extrafont)
# library(patchwork)
# library(tibble)

# Host on server ?
library(rsconnect)
rsconnect::deployApp('mpw_dashboard')

# Run locally
library(shiny)
runApp('mpw_dashboard')

# User interface
# Load Packages
library(shiny)
library(ggplot2)
library(ggiraph)
library(ggthemes)
library(tidyverse)
library(extrafont)
library(patchwork)
library(tibble)

ui <- fluidPage(
  tags$head(tags$style('
   body {
      font-family: Gibson Light;
      font-size: 18px;
   }'
  )),
  
  # tags$head(
  #   tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "aei.png")),
  
  # titlePanel(title = "Dashboard Companion for Mantus, Pang, and Warshawsky (2023):",
  #            windowTitle = "MPW Dashboard"),
  
  titlePanel(title =  span(
    strong('Dashboard Companion for Mantus, Pang, and Warshawsky (2023):'), 
    em('A Unified Long-Run Macroeconomic Projection of Health Care Spending, the Federal Budget, and
    Benefit Programs in the US')),
    windowTitle = "MPW Model"),
  
  tags$div(
    em("(Read the full ",
       tags$a(href="https://www.aei.org/research-products/working-paper/a-unified-long-run-macroeconomic-projection-of-health-care-spending-the-federal-budget-and-benefit-programs-in-the-us/", 
              "AEI Economic Policy Working Paper.", target="_blank"),
       ")")
  ),
  
  tags$div("This dashboard allows you to change initial conditions and key parameters of the model
    described and simulated in Mantus, Pang, and Warshawsky (2023). This serves as a tool
    for policymakers and those interested in the fiscal and economic trajectories of the U.S."),
  tags$div(
    "To test out different conditions, simply adjust the sliders on the left then click 'Run Model.' 
    The initial values of these sliders and the initial figure reflect the baseline assumptions
    made by the authors."),
  tags$div(
    "You are allowed to display up to 4 alternative scenarios. They will appear as non-filled shapes
    while the baseline will remain as filled circles."),
  
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Economic Parameters
      h2("Model/Policy Assumptions"),
      sliderInput("inv_coeff_input", "Investment share of GDP:",
                  ticks = FALSE,
                  min = 0.18, max = 0.24, value = 0.21, step = 0.005),
      sliderInput("g1_input", "General Productivity:",
                  ticks = FALSE,
                  min = 0.014, max = 0.022, value = 0.018, step = 0.001),
      sliderInput("g3_input", "Health Care Productivity:",
                  ticks = FALSE,
                  min = 0, max = .008, value = 0.004, step = 0.001),
      sliderInput("hc_inc_el_input", "Health Care Income Elasticity:",
                  ticks = FALSE,
                  min = 0.8, max = 1.6, value = 1.2, step = 0.1),
      sliderInput("hc_pri_el_input", "Health Care Price Elasticity:",
                  ticks = FALSE,
                  min = -1.1, max = 0.1, value = -0.5, step = 0.1),
      sliderInput("rgov_0_input", "Initial Real Interest Rate (%):",
                  ticks = FALSE,
                  min = 1.3, max = 2.3, value = 1.8, step = 0.1),
      
      # Separator line
      tags$hr(),
      
      # Fiscal Parameters
      h2("Fiscal Policy Parameters"),
      sliderInput("non_health_cut_input", "Cut Non-Health Spending (%):",
                  ticks = FALSE,
                  min = 0, max = 15, value = 0, step = 1),
      sliderInput("income_tax_raise_input", "Raise Income Tax (%):",
                  ticks = FALSE,
                  min = 0, max = 2, value = 0, step = 0.5),
      sliderInput("payroll_tax_raise_input", "Raise Soc. Sec. Payroll Tax (%):",
                  ticks = FALSE,
                  min = 0, max = 2, value = 0, step = 0.5),
      sliderInput("ssa_rep_rate_raise_input", "Reduce Soc. Sec. Replacement Rate (%):",
                  ticks = FALSE,
                  min = 0, max = 10, value = 0, step = 1),
      actionButton("reset_sliders", "Reset"),
      actionButton("run_model", "Run Model")
      
    ),
    mainPanel(
      width = 9,
      # style = "background-color: #C0B8B8;", 
      girafeOutput("debt_gdp",
                   height = '100%')
    )
  ),
  tags$hr(),
  p("The American Enterprise Institute (AEI) is a nonpartisan, nonprofit, 
    501(c)(3) educational organization and does not take institutional positions
    on any issues. The views expressed are those of the authors."),
)

# Server
# Load Packages
library(shiny)
library(ggplot2)
library(ggiraph)
library(ggthemes)
library(tidyverse)
library(extrafont)
library(patchwork)
library(tibble)

# call in model simulation functions
source("init_fct.R")
source("simulation_fct.R")
source("init_df_hc_el.R")
# source("init.R") # initialize base year
years <- 2021:2095
# df_hc_el <- init_df_hc_el()
df_projections <- init_model(df_hc_el = init_df_hc_el())

# define baseline
baseline_projections <- estimate_model(df_projections = df_projections,
                                       years = years) %>%
  mutate(debt_to_gdp = (D/Y)*100,
         deficit_to_gdp = (deficit/Y)*100,
         nhe_to_gdp = ((f_2*p_rel)/Y)*100,
         welfare = ((C-f_2*p_rel)*1000000000000)/population,
         welfare_growth = ((welfare-lag(welfare))/lag(welfare))*100,
         tooltip_text_debt = paste0(year, "\n",
                                    round(debt_to_gdp, 1), "%"),
         tooltip_text_deficit = paste0(year, "\n",
                                       round(deficit_to_gdp, 1), "%"),
         tooltip_text_nhe = paste0(year, "\n",
                                   round(nhe_to_gdp, 1), "%"),
         tooltip_text_welfare = paste0(year, "\n",
                                       round(welfare_growth, 1), "%"))

# Try to add multiple lines to the figures
df_debt <- data.frame(
  year = years,
  baseline = baseline_projections$debt_to_gdp,
  alt_1 = NA,
  alt_2 = NA,
  alt_3 = NA,
  alt_4 = NA
)

df_deficit <- data.frame(
  year = years,
  baseline = baseline_projections$deficit_to_gdp,
  alt_1 = NA,
  alt_2 = NA,
  alt_3 = NA,
  alt_4 = NA
)

df_nhe <- data.frame(
  year = years,
  baseline = baseline_projections$nhe_to_gdp,
  alt_1 = NA,
  alt_2 = NA,
  alt_3 = NA,
  alt_4 = NA
)

df_welfare <- data.frame(
  year = years,
  baseline = baseline_projections$welfare_growth,
  alt_1 = NA,
  alt_2 = NA,
  alt_3 = NA,
  alt_4 = NA
)

# Set default font sizes
base_size_set <- 10
base_plot_size <- 1
background_width <- 1
background_color <- 'gray95'

# Compute debt to GDP
simulate_model <- function(a1 = 0.21,
                           g1_input = 0.018,
                           g3_input = 0.004,
                           rgov_0_input = 0.018,
                           hc_inc_el_input = 1.2,
                           hc_pri_el_input = -0.5,
                           non_health_spending_cut = 1,
                           income_tax_raise_input = 0,
                           payroll_tax_raise_input = 0,
                           ssa_rep_rate_raise_input = 0
) {
  df_projections <- init_model(g1_input = g1_input,
                               g3_input = g3_input,
                               inv_coeff_input = a1,
                               rgov_0_input = rgov_0_input,
                               income_tax_raise = income_tax_raise_input,
                               payroll_tax_raise = payroll_tax_raise_input,
                               non_health_spending_cut = non_health_spending_cut,
                               ssa_rep_rate_raise = ssa_rep_rate_raise_input,
                               df_hc_el = init_df_hc_el(years = years,
                                                        hc_inc_el_input = hc_inc_el_input,
                                                        hc_pri_el_input = hc_pri_el_input))
  df_projections <- estimate_model(inv_coeff = a1,
                                   df_projections = df_projections,
                                   non_health_spending_cut = non_health_spending_cut,
                                   years = years)
  df_projections <- df_projections %>%
    mutate(debt_to_gdp = (D/Y)*100,
           deficit_to_gdp = (deficit/Y)*100,
           nhe_to_gdp = ((f_2*p_rel)/Y)*100,
           welfare = ((C-(f_2*p_rel))*1000000000000)/population,
           welfare_growth = ((welfare-lag(welfare))/lag(welfare))*100,
           tooltip_text_debt = paste0(year, "\n",
                                      round(debt_to_gdp, 1), "%"),
           tooltip_text_deficit = paste0(year, "\n",
                                         round(deficit_to_gdp, 1), "%"),
           tooltip_text_nhe = paste0(year, "\n",
                                     round(nhe_to_gdp, 1), "%"),
           tooltip_text_welfare = paste0(year, "\n",
                                         round(welfare_growth, 1), "%"))
  return(df_projections)
}

server <- function(input, output, session) {
  
  values <- reactiveValues(
    inv_coeff_input = 0.21,
    g1_input = 0.018,
    g3_input = 0.004,
    rgov_0_input = 0.018,
    hc_inc_el_input = 1.2,
    hc_pri_el_input = -0.5,
    non_health_spending_cut = 1,
    income_tax_raise_input = 0,
    payroll_tax_raise_input = 0,
    ssa_rep_rate_raise_input = 0,
    alt_counter = 0
  )
  
  # Render plot
  output$debt_gdp <- renderGirafe({
    df_projections <- simulate_model(a1 = values$inv_coeff_input,
                                     g1_input = values$g1_input,
                                     g3_input = values$g3_input,
                                     rgov_0_input = values$rgov_0_input,
                                     non_health_spending_cut = values$non_health_spending_cut,
                                     income_tax_raise_input = values$income_tax_raise_input,
                                     payroll_tax_raise_input = values$payroll_tax_raise_input,
                                     ssa_rep_rate_raise_input = values$ssa_rep_rate_raise_input,
                                     hc_inc_el_input = values$hc_inc_el_input,
                                     hc_pri_el_input = values$hc_pri_el_input)
    # Code for baseline and one alternative
    # gg_plot_debt <- ggplot(df_projections, aes(x = year, y = debt_to_gdp, 
    #                                            data_id = year, tooltip = tooltip_text_debt)) +
    #   theme_bw(base_family = "Gibson Light",
    #            base_size = base_size_set) +
    #   theme(plot.caption = element_text(hjust = 0),
    #         plot.title=element_text(face="bold"),
    #         plot.background = element_rect(color = background_color, fill = NA, size = background_width)) + 
    #   geom_point_interactive(aes(data_id = year, tooltip = tooltip_text_debt),
    #                          color = 'black', size = base_plot_size, shape = 1) +
    #   geom_point_interactive(data = baseline_projections, color = 'black', size = base_plot_size,
    #                          aes(data_id = year, tooltip = tooltip_text_debt)) +
    #   xlim(2020, 2100) +
    #   labs(title = "Panel A. Projected Debt to GDP, 2021-2095 (%)",
    #        x = "Year",
    #        y = "Percentage of GDP")
    
    # Try to add multiple lines for Debt to GDP graph
    if (values$alt_counter == 0){
      # Debt to GDP
      gg_plot_debt <- ggplot() +
        theme_bw(base_family = "Gibson Light",
                 base_size = base_size_set) +
        theme(plot.caption = element_text(hjust = 0),
              plot.title=element_text(face="bold"),
              plot.background = element_rect(color = background_color, fill = NA, size = background_width)) +
        geom_point_interactive(data = baseline_projections, color = 'black', size = base_plot_size,
                               aes(x = year, y = debt_to_gdp,
                                   data_id = year, tooltip = tooltip_text_debt)) +
        xlim(2020, 2100) +
        labs(title = "Panel A. Projected Debt to GDP, 2021-2095 (%)",
             x = "Year",
             y = "Percentage of GDP")
      # Deficit to GDP
      gg_plot_deficit <- ggplot() +
        theme_bw(base_family = "Gibson Light",
                 base_size = base_size_set) +
        theme(plot.caption = element_text(hjust = 0),
              plot.title=element_text(face="bold"),
              plot.background = element_rect(color = background_color, fill = NA, size = background_width)) +
        geom_point_interactive(data = baseline_projections, color = 'blue4', size = base_plot_size,
                               aes(x = year, y = deficit_to_gdp,
                                   data_id = year, tooltip = tooltip_text_deficit)) +
        xlim(2020, 2100) +
        labs(title = "Panel B. Projected Deficit to GDP, 2021-2095 (%)",
             x = "Year",
             y = "Percentage of GDP")
      # NHE to GDP
      gg_plot_nhe <- ggplot() +
        theme_bw(base_family = "Gibson Light",
                 base_size = base_size_set) +
        theme(plot.caption = element_text(hjust = 0),
              plot.title=element_text(face="bold"),
              plot.background = element_rect(color = background_color, fill = NA, size = background_width)) +
        geom_point_interactive(data = baseline_projections, color = 'darkred', size = base_plot_size,
                               aes(x = year, y = nhe_to_gdp,
                                   data_id = year, tooltip = tooltip_text_nhe)) +
        xlim(2020, 2100) +
        labs(title = "Panel C. Projected NHE to GDP, 2021-2095 (%)",
             x = "Year",
             y = "Percentage of GDP")
      
      # Welfare Growth
      gg_plot_welfare <- ggplot() +
        theme_bw(base_family = "Gibson Light",
                 base_size = base_size_set) +
        theme(plot.caption = element_text(hjust = 0),
              plot.title=element_text(face="bold"),
              plot.background = element_rect(color = background_color, fill = NA, size = background_width)) +
        geom_point_interactive(data = baseline_projections, color = 'darkgreen', size = base_plot_size,
                               aes(x = year, y = welfare_growth,
                                   data_id = year, tooltip = tooltip_text_welfare)) +
        xlim(2025, 2100) +
        labs(title = "Panel D. Projected Welfare Growth Rate, 2025-2095 (%)",
             x = "Year",
             y = "Annual Percent Change")
      
    } else {
      if (values$alt_counter >= 5) {
        alt_num <- paste0('alt_', ((values$alt_counter %% 5) + 1))
      } else {
        alt_num <- paste0('alt_', values$alt_counter %% 5)
      }

      ####
      # Debt to GDP
      df_debt[alt_num] <<- df_projections$debt_to_gdp
      # Reshape
      df_debt_figure <- df_debt %>%
        pivot_longer(
          !year,
          names_to = 'Scenario',
          values_to = 'debt_to_gdp'
        ) %>%
        mutate(Scenario_disp = case_when(Scenario == 'baseline' ~ 'Baseline',
                                    Scenario == 'alt_1' ~ 'Alternative 1',
                                    Scenario == 'alt_2' ~ 'Alternative 2',
                                    Scenario == 'alt_3' ~ 'Alternative 3',
                                    Scenario == 'alt_4' ~ 'Alternative 4'),
               tooltip_text_debt = paste0(Scenario_disp, "\n",
                                          year, "\n",
                                          round(debt_to_gdp, 1), "%"))
      # Make graph
      gg_plot_debt <- ggplot() +
        theme_bw(base_family = "Gibson Light",
                 base_size = base_size_set) +
        theme(legend.position = "",
              plot.caption = element_text(hjust = 0),
              plot.title=element_text(face="bold"),
              plot.background = element_rect(color = background_color, fill = NA, size = background_width)) +
        geom_point_interactive(data = df_debt_figure, size = base_plot_size,
                               aes(x = year, y = debt_to_gdp, shape = Scenario, color = Scenario,
                                   data_id = year, tooltip = tooltip_text_debt)) +
        scale_shape_manual_interactive(
          values = c(baseline = 19,
                     alt_1 = 1,
                     alt_2 = 1,
                     alt_3 = 1,
                     alt_4 = 1
        )) +
        scale_color_manual_interactive(
          values = c(baseline = 'black',
                     alt_1 = 'black',
                     alt_2 = 'gray40',
                     alt_3 = 'gray70',
                     alt_4 = 'gray85'
          )) +
        xlim(2020, 2100) +
        labs(title = "Panel A. Projected Debt to GDP, 2021-2095 (%)",
             x = "Year",
             y = "Percentage of GDP")
      ####
      # Deficit to GDP
      df_deficit[alt_num] <<- df_projections$deficit_to_gdp
      # Reshape
      df_deficit_figure <- df_deficit %>%
        pivot_longer(
          !year,
          names_to = 'Scenario',
          values_to = 'deficit_to_gdp'
        ) %>%
        mutate(Scenario_disp = case_when(Scenario == 'baseline' ~ 'Baseline',
                                    Scenario == 'alt_1' ~ 'Alternative 1',
                                    Scenario == 'alt_2' ~ 'Alternative 2',
                                    Scenario == 'alt_3' ~ 'Alternative 3',
                                    Scenario == 'alt_4' ~ 'Alternative 4'),
               tooltip_text_deficit = paste0(Scenario_disp, "\n",
                                             year, "\n",
                                             round(deficit_to_gdp, 1), "%"))
      # Make graph
      gg_plot_deficit <- ggplot() +
        theme_bw(base_family = "Gibson Light",
                 base_size = base_size_set) +
        theme(legend.position = "",
              plot.caption = element_text(hjust = 0),
              plot.title=element_text(face="bold"),
              plot.background = element_rect(color = background_color, fill = NA, size = background_width)) +
        geom_point_interactive(data = df_deficit_figure, size = base_plot_size,
                               aes(x = year, y = deficit_to_gdp, shape = Scenario, color = Scenario,
                                   data_id = year, tooltip = tooltip_text_deficit)) +
        scale_shape_manual_interactive(
          values = c(baseline = 19,
                     alt_1 = 1,
                     alt_2 = 1,
                     alt_3 = 1,
                     alt_4 = 1
          )) +
        scale_color_manual_interactive(
          values = c(baseline = 'blue4',
                     alt_1 = 'blue4',
                     alt_2 = 'blue',
                     alt_3 = 'deepskyblue',
                     alt_4 = 'cyan3'
          )) +
        xlim(2020, 2100) +
        labs(title = "Panel B. Projected Deficit to GDP, 2021-2095 (%)",
             x = "Year",
             y = "Percentage of GDP")
      ####
      # NHE to GDP
      df_nhe[alt_num] <<- df_projections$nhe_to_gdp
      # Reshape
      df_nhe_figure <- df_nhe %>%
        pivot_longer(
          !year,
          names_to = 'Scenario',
          values_to = 'nhe_to_gdp'
        ) %>%
        mutate(Scenario_disp = case_when(Scenario == 'baseline' ~ 'Baseline',
                                    Scenario == 'alt_1' ~ 'Alternative 1',
                                    Scenario == 'alt_2' ~ 'Alternative 2',
                                    Scenario == 'alt_3' ~ 'Alternative 3',
                                    Scenario == 'alt_4' ~ 'Alternative 4'),
               tooltip_text_nhe = paste0(Scenario_disp, "\n",
                                         year, "\n",
                                             round(nhe_to_gdp, 1), "%"))
      # Make graph
      gg_plot_nhe <- ggplot() +
        theme_bw(base_family = "Gibson Light",
                 base_size = base_size_set) +
        theme(legend.position = "",
              plot.caption = element_text(hjust = 0),
              plot.title=element_text(face="bold"),
              plot.background = element_rect(color = background_color, fill = NA, size = background_width)) +
        geom_point_interactive(data = df_nhe_figure, size = base_plot_size,
                               aes(x = year, y = nhe_to_gdp, shape = Scenario, color = Scenario,
                                   data_id = year, tooltip = tooltip_text_nhe)) +
        scale_shape_manual_interactive(
          values = c(baseline = 19,
                     alt_1 = 1,
                     alt_2 = 1,
                     alt_3 = 1,
                     alt_4 = 1
          )) +
        scale_color_manual_interactive(
          values = c(baseline = 'darkred',
                     alt_1 = 'darkred',
                     alt_2 = 'red1',
                     alt_3 = 'salmon3',
                     alt_4 = 'salmon'
          )) +
        xlim(2020, 2100) +
        labs(title = "Panel C. Projected NHE to GDP, 2021-2095 (%)",
             x = "Year",
             y = "Percentage of GDP")
      
      ####
      # Welfare Growth
      df_welfare[alt_num] <<- df_projections$welfare_growth
      # Reshape
      df_welfare_figure <- df_welfare %>%
        pivot_longer(
          !year,
          names_to = 'Scenario',
          values_to = 'welfare_growth'
        ) %>%
        mutate(Scenario_disp = case_when(Scenario == 'baseline' ~ 'Baseline',
                                    Scenario == 'alt_1' ~ 'Alternative 1',
                                    Scenario == 'alt_2' ~ 'Alternative 2',
                                    Scenario == 'alt_3' ~ 'Alternative 3',
                                    Scenario == 'alt_4' ~ 'Alternative 4'),
               tooltip_text_welfare = paste0(Scenario_disp, "\n",
                                             year, "\n",
                                         round(welfare_growth, 1), "%"))
      # Make graph
      gg_plot_welfare <- ggplot() +
        theme_bw(base_family = "Gibson Light",
                 base_size = base_size_set) +
        theme(legend.position = "",
              plot.caption = element_text(hjust = 0),
              plot.title=element_text(face="bold"),
              plot.background = element_rect(color = background_color, fill = NA, size = background_width)) +
        geom_point_interactive(data = df_welfare_figure, size = base_plot_size,
                               aes(x = year, y = welfare_growth, shape = Scenario, color = Scenario,
                                   data_id = year, tooltip = tooltip_text_welfare)) +
        scale_shape_manual_interactive(
          values = c(baseline = 19,
                     alt_1 = 1,
                     alt_2 = 1,
                     alt_3 = 1,
                     alt_4 = 1
          )) +
        scale_color_manual_interactive(
          values = c(baseline = 'darkgreen',
                     alt_1 = 'darkgreen',
                     alt_2 = 'green3',
                     alt_3 = 'green',
                     alt_4 = 'springgreen'
          )) +
        xlim(2025, 2100) +
        labs(title = "Panel D. Projected Welfare Growth Rate, 2025-2095 (%)",
             x = "Year",
             y = "Annual Percent Change")
      
    }

    # gg_plot_deficit <- ggplot(df_projections, aes(x = year, y = deficit_to_gdp, 
    #                                               data_id = year, tooltip = tooltip_text_deficit)) +
    #   theme_bw(base_family = "Gibson Light",
    #            base_size = base_size_set) +
    #   theme(plot.caption = element_text(hjust = 0),
    #         plot.title=element_text(face="bold"),
    #         plot.background = element_rect(color = background_color, fill = NA, size = background_width)) + 
    #   geom_point_interactive(aes(data_id = year, tooltip = tooltip_text_deficit),
    #                          color = 'darkblue', size = base_plot_size, shape = 1) +
    #   geom_point_interactive(data = baseline_projections, color = 'darkblue', size = base_plot_size,
    #                          aes(data_id = year, tooltip = tooltip_text_deficit)) +
    #   xlim(2020, 2100) +
    #   labs(title = "Panel B. Projected Deficit to GDP, 2021-2095 (%)",
    #        x = "Year",
    #        y = "Percentage of GDP")
    
    # gg_plot_nhe <- ggplot(df_projections, aes(x = year, y = nhe_to_gdp, 
    #                                           data_id = year, tooltip = tooltip_text_nhe)) +
    #   theme_bw(base_family = "Gibson Light",
    #            base_size = base_size_set) +
    #   theme(plot.caption = element_text(hjust = 0),
    #         plot.title=element_text(face="bold"),
    #         plot.background = element_rect(color = background_color, fill = NA, size = background_width)) + 
    #   geom_point_interactive(aes(data_id = year, tooltip = tooltip_text_nhe),
    #                          color = 'darkred', size = base_plot_size, shape = 1) +
    #   geom_point_interactive(data = baseline_projections, color = 'darkred', size = base_plot_size, 
    #                          aes(data_id = year, tooltip = tooltip_text_nhe)) +
    #   xlim(2020, 2100) +
    #   labs(title = "Panel C. Projected NHE to GDP, 2021-2095 (%)",
    #        x = "Year",
    #        y = "Percentage of GDP")
    
    # gg_plot_welfare <- ggplot(df_projections, aes(x = year, y = welfare_growth, 
    #                                               data_id = year, tooltip = tooltip_text_welfare)) +
    #   theme_bw(base_family = "Gibson Light",
    #            base_size = base_size_set) +
    #   theme(plot.caption = element_text(hjust = 1),
    #         plot.title=element_text(face="bold"),
    #         plot.background = element_rect(color = background_color, fill = NA, size = background_width)) + 
    #   geom_point_interactive(aes(data_id = year, tooltip = tooltip_text_welfare),
    #                          color = 'red3', size = base_plot_size, shape = 1) +
    #   geom_point_interactive(data = baseline_projections, color = 'red3', size = base_plot_size,
    #                          aes(data_id = year, tooltip = tooltip_text_welfare)) +
    #   xlim(2025, 2095) +
    #   ylim(-2,2) +
    #   labs(title = "Panel D. Projected Welfare Growth Rate, 2025-2095 (%)",
    #        x = "Year",
    #        y = "Annual Percent Change",
    #        caption = "Source: Mantus, Pang, and Warshawsky (2023)")
    
    final_plot <- (gg_plot_debt + gg_plot_deficit)/(gg_plot_nhe + gg_plot_welfare)

    girafe(
      ggobj = final_plot,
      height_svg = 6,
      width_svg = 9
    )
  })
  
  # Event handler for prediction button
  observeEvent(input$run_model, {
    values$inv_coeff_input <- input$inv_coeff_input
    values$g1_input <- input$g1_input
    values$g3_input <- input$g3_input
    values$rgov_0_input <- input$rgov_0_input/100
    values$hc_inc_el_input <- input$hc_inc_el_input
    values$hc_pri_el_input <- input$hc_pri_el_input
    values$non_health_spending_cut <- 1 - input$non_health_cut_input/100
    values$income_tax_raise_input <- input$income_tax_raise_input/100
    values$payroll_tax_raise_input <- input$payroll_tax_raise_input/100
    values$ssa_rep_rate_raise_input <- input$ssa_rep_rate_raise_input/100
    # Update alternatives count
    values$alt_counter <- values$alt_counter + 1
  })
  
  # Reset sliders
  observeEvent(input$reset_sliders, {
    # Reset sliders
    updateSliderInput(session,'inv_coeff_input',value = 0.21)
    updateSliderInput(session,'g1_input',value = 0.018)
    updateSliderInput(session,'g3_input',value = 0.004)
    updateSliderInput(session,'rgov_0_input',value = 1.8)
    updateSliderInput(session,'hc_inc_el_input',value = 1.2)
    updateSliderInput(session,'hc_pri_el_input',value = -0.5)
    updateSliderInput(session,'non_health_cut_input',value = 0)
    updateSliderInput(session,'income_tax_raise_input',value = 0)
    updateSliderInput(session,'payroll_tax_raise_input',value = 0)
    updateSliderInput(session,'ssa_rep_rate_raise_input',value = 0)
    # Reset figures
    values$inv_coeff_input <- 0.21
    values$g1_input <- 0.018
    values$g3_input <- 0.004
    values$rgov_0_input <- 1.8/100
    values$hc_inc_el_input <- 1.2
    values$hc_pri_el_input <- -0.5
    values$non_health_spending_cut <- 1 - 0/100
    values$income_tax_raise_input <- 0/100
    values$payroll_tax_raise_input <- 0/100
    values$ssa_rep_rate_raise_input <- 0/100
    # Reset counter
    values$alt_counter <- 0
    df_debt <<- data.frame(
      year = years,
      baseline = baseline_projections$debt_to_gdp,
      alt_1 = NA,
      alt_2 = NA,
      alt_3 = NA,
      alt_4 = NA
    )
    df_deficit <<- data.frame(
      year = years,
      baseline = baseline_projections$deficit_to_gdp,
      alt_1 = NA,
      alt_2 = NA,
      alt_3 = NA,
      alt_4 = NA
    )
    
    df_nhe <<- data.frame(
      year = years,
      baseline = baseline_projections$nhe_to_gdp,
      alt_1 = NA,
      alt_2 = NA,
      alt_3 = NA,
      alt_4 = NA
    )
    
    df_welfare <<- data.frame(
      year = years,
      baseline = baseline_projections$welfare_growth,
      alt_1 = NA,
      alt_2 = NA,
      alt_3 = NA,
      alt_4 = NA
    )
    
  })
}

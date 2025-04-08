# Define base year in MPW projections
init_model <- function(g1_input = 0.018,
                       g3_input = 0.004,
                       inv_coeff_input = 0.21, 
                       rgov_0_input = 0.023,
                       non_health_spending_cut = 1,
                       income_tax_raise = 0,
                       payroll_tax_raise = 0,
                       ssa_rep_rate_raise = 0,
                       df_hc_el = data.frame()){
  
  # define year range
  years <- 2023:2096
  
  # Import demographics and spending/LFPR matrices
  ###############
  # import spending matrix
  df_hc_spending_mat <- read_excel("C:/AEI/Warshawsky/Dashboard/Data/hc_spending_estimates_2023.xlsx") %>%
    filter(age_group != 'total') %>%
    select(!total) %>%
    rename(gender = sex) %>%
    pivot_longer(
      !c(gender, age_group),
      names_to = "source",
      values_to = "est_spending_pc"
    ) 

  # import LFPR matrix
  df_lfpr <- read_excel("C:/AEI/Warshawsky/Dashboard/Data/lf_proj_cbo_2023.xlsx") %>%
    mutate(gender = str_to_lower(gender)) %>%
    pivot_longer(
      !c(gender, age_group, unemployment, employment),
      names_to = 'year',
      values_to = 'lfpr'
    ) %>%
    select(year, gender, age_group, unemployment, employment, lfpr) %>%
    mutate(year = as.numeric(year))
  
  # define age groups for HC spending
  hc_age_groups <- c("0-18", "19-44", "45-64", "65-84", "85+")
  
  # import demographic projections
  df_cbo_demo <- read_excel("C:/AEI/Warshawsky/Dashboard/Data/CBO_2025_Projections_cleaned.xlsx") %>%
    mutate(gender = str_to_lower(gender))
  
  
  # clean for HC
  df_hc_cbo_demo <- df_cbo_demo %>%
    mutate(age_group = cut(age, breaks = c(-Inf, 18, 44, 64, 84, Inf), labels = hc_age_groups, include.lowest = TRUE))
  
  # repeat for labor force projections
  # Specify the new age groups
  lf_age_groups <- c("0-15", "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                     "55-59", "60-61", "62-64", "65-69", "70-79", "80-89", "90+")
  
  # Use mutate to create the 'age_group' variable
  df_lf_cbo_demo <- df_cbo_demo %>%
    mutate(age_group = cut(age, breaks = c(-Inf, 15, 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 61, 64, 69, 79, 89, Inf),
                           labels = lf_age_groups, include.lowest = TRUE))
  
  # generate projections of labor force size (# and hours)
  hrs_0 <- 34.73656 # initial hours worked per week
  hrs_decline <- 0.0005 # annual decline in hours worked per week
  
  # merge projections with lfpr data
  # compute hours per week using assumptions above
  df_lf <- df_lf_cbo_demo %>%
    group_by(year, gender, age_group) %>%
    summarize(population = sum(proj_pop)) %>%
    left_join(df_lfpr,
              by = join_by(year, gender, age_group)) %>%
    mutate(hrs_per_week = case_when(year == min(years) ~ hrs_0,
                                    .default = hrs_0*(1-hrs_decline)^(year-min(years))),
           lf = population*lfpr*employment,
           lf_hours = lf*hrs_per_week*52) %>%
    group_by(year) %>%
    summarize(L = sum(lf_hours)/1000000000000,
              lf_size = sum(lf)/1000000) %>%
    mutate(hrs_per_week = case_when(year == min(years) ~ hrs_0,
                                    .default = hrs_0*(1-hrs_decline)^(year-min(years)))) # mistake in excel file, used old hrs per week
  ############### 
  # Compute growth factors over time
  # initialize data frame
  df_growth <- data.frame(
    year = years,
    g_1 = NA,
    g_2 = NA,
    g_3 = NA
  )
  
  # growth factors
  g1 <- g1_input  #labor-augmenting tech progress
  g2 <- 0 # capital deepening
  g3 <- g3_input # health sector productivity
  
  # compute values
  df_growth <- df_growth %>%
    mutate(g_1 = (1+g1)^(year-min(years)),
           g_2 = (1+g2)^(year-min(years)),
           g_3 = (1+g3)^(year-min(years)))
  
  # Simulate the model!
  ###############

  # initialize variables
  # Economy
  # capital stocks
  K_0 <- 88.9323 # total
  K2_0 <- 3.027007 # HC
  K1_0 <- K_0 - K2_0
  
  # Rate of capital depreciation
  k_dep <- 0.054
  
  # labor force
  L_0 <- df_lf$L[1]
  L2_0 <- 0.031839 # HC
  L1_0 <- L_0 - L2_0
  
  # output
  Y_0 <- 27.7000
  Y2_0 <- 4.866499862 
  Y1_0 <- Y_0 - Y2_0
  
  # debt
  D_0 <- 27.3
  
  # production parameters
  beta1 <- 1-0.63
  beta2 <- L2_0/Y2_0
  beta3 <- K2_0/Y2_0
  
  # adjustment parameter - ratio of actual output from all other to computed
  alpha1 <- Y1_0/(L1_0^(1-beta1)*K1_0^beta1)
  
  # Federal Government
  
  # Compute total annual population and 65+ pop
  # used for computing non-defense spending and Soc Sec spending
  df_pop <- df_cbo_demo %>%
    group_by(year) %>%
    summarize(population = sum(proj_pop))
  
  df_pop_elderly <- df_cbo_demo %>%
    filter(age >= 65) %>%
    group_by(year) %>%
    summarize(pop_65plus = sum(proj_pop))
  
  # Non-defense spending as a share of GDP
  non_def_coeff <- 0.7605/(df_pop %>% filter(year == 2023) %>% select(population) %>% pull())
  # Defense spending as a share of GDP
  def_coeff <- 0.036176895
  
  # Investment as a share of GDP
  inv_coeff <- inv_coeff_input
  
  # Initial government interest rate
  r_gov_0 <- rgov_0_input
  
  # Assume public health/investment spending as a share of non-investment NHE
  hc_inv_coeff <- 0.058755505 
  hc_ph_coeff <- 0.138621657  
  
  # Assume federal shares of medicaid, other, public health, and investment spending
  hc_fed_medicaid_coeff <- 0.60
  hc_fed_other_coeff <- 0.44
  hc_fed_ph_coeff <- 0.25
  hc_fed_inv_coeff <- 2/3
  
  # Compute federal health care spending
  # begin by compute PHC, then convert to NHE
  df_hc <- df_hc_cbo_demo %>%
    group_by(year, age_group, gender) %>%
    summarize(population = sum(proj_pop)) %>%
    inner_join(
      df_hc_spending_mat,
      by = c('gender', 'age_group'),
      relationship = "many-to-many"
    ) %>%
    mutate(phc_to_nhe = case_when( # Convert PHC estimates to NHE estimates using 2023 values
      source == 'medicare' ~ 1,  
      source == 'medicaid' ~ 1,   
      source == 'private_insurance' ~ 1,  
      source == 'oop' ~ 1, 
      source == 'other' ~ 1 
    )) %>%
    mutate(phc_spending = population*est_spending_pc,
           nhe_spending = phc_spending*phc_to_nhe) %>%
    group_by(year, source) %>%
    summarize(hc_spending = sum(nhe_spending)/1000000000000) %>%
    pivot_wider(
      names_from = source, 
      values_from = hc_spending
    ) %>%
    mutate(non_investment_hc = medicare + medicaid + private_insurance + oop + other,
           public_health = hc_ph_coeff*non_investment_hc,
           hc_investment = hc_inv_coeff*non_investment_hc,
           f2_hat = non_investment_hc + public_health + hc_investment)

  
  # Assumptions for social security spending
  ssa_pop_coeff <- 1.13
  ssa_earn_coeff <- 0.6920
  ssa_rep_rate <- 0.302 - ssa_rep_rate_raise
  
  # Assumptions for tax revenue
  tau <- 0.0991478 + income_tax_raise
  pi <- 0.11 + payroll_tax_raise
  
  # Debt Assumption (Previous year's debt in $T)
  D_prev <- 24.7
  
  # begin with year 1
  # start with production functions
  # initialize dataframe which will contain projections once computed
  df_projections <- data.frame(
    year = years,
    f_1 = NA,
    f_2 = NA,
    L1 = L1_0,
    L2 = L2_0,
    K = K_0,
    K1 = K1_0,
    K2 = K2_0,
    p_rel = 1,
    Y = NA, 
    w = NA,
    r = NA,
    p = NA,
    A = NA,
    B = NA,
    rel_change_w = NA,
    rel_change_p = NA,
    hc_el_adj = NA, 
    tau = tau,
    pi = pi,
    ssa_pop_coeff = ssa_pop_coeff,
    ssa_earn_coeff = ssa_earn_coeff,
    ssa_rep_rate = ssa_rep_rate,
    hc_inv_coeff = hc_inv_coeff,
    hc_ph_coeff = hc_ph_coeff,
    hc_fed_medicaid_coeff = hc_fed_medicaid_coeff,
    hc_fed_other_coeff = hc_fed_other_coeff,
    hc_fed_ph_coeff = hc_fed_ph_coeff,
    hc_fed_inv_coeff = hc_fed_inv_coeff,
    beta1 = beta1,
    beta2 = beta2,
    beta3 = beta3,
    alpha1 = alpha1,
    non_def_coeff = non_def_coeff,
    def_coeff = def_coeff,
    k_dep = k_dep
  ) %>%
    left_join(df_growth,
              join_by(year)) %>%
    left_join(df_pop,
              join_by(year)) %>%
    left_join(df_lf) %>%
    left_join(df_hc,
              join_by(year)) %>%
    left_join(df_pop_elderly,
              join_by(year)) %>%
    left_join(df_hc_el)
  
  # compute output
  df_projections <- df_projections %>%
    mutate(f_1 = case_when(.default = f_1,
                           year == min(years) ~ alpha1*(((g_1*L1)^(1-beta1))*K1^beta1)),
           f_2 = case_when(.default = f_2,
                           year == min(years) ~ (g_2*K2)/beta3),
           Y = case_when(.default = Y,
                         year == min(years) ~ f_1 + p_rel*f_2))
  
  # Compute first-order conditions
  df_projections <- df_projections %>%
    mutate(w = case_when(.default = w,
                         year == min(years) ~ (1-beta1)*alpha1*g_1*((K-K2/g_2)/(g_1*(L-L2)))^beta1),
           r = case_when(.default = r,
                         year == min(years) ~ beta1*alpha1*((K-K2*g_2)/(g_1*(L-L2)))^(beta1-1)),
           B = case_when(.default = B,
                         year == min(years) ~ (K-K2)/(g_1*(L-L2))),
           A = case_when(.default = A,
                         year == min(years) ~ alpha1*(g_1*(1.0-beta1)*(B)^beta1*L + beta1*(B)^(beta1-1)*K - B^beta1*(g_1*(L-L2)))),
           p = case_when(.default = p,
                         year == min(years) ~ A/f2_hat)) 
  
  # Compute identities
  df_projections <- df_projections %>%
    mutate(Def = case_when(.default = NA,
                           year == min(years) ~ def_coeff*Y*non_health_spending_cut),
           Non_def = case_when(.default = NA,
                               year == min(years) ~ non_def_coeff*population*non_health_spending_cut),
           G = case_when(.default = NA,
                         year == min(years) ~ Def + Non_def),
           I = case_when(.default = NA,
                         year == min(years) ~ inv_coeff*Y),
           C = case_when(.default = NA,
                         year == min(years) ~ Y - I - G))

  # Solve federal Government
  
  # Compute government risk premium
  r_0 <- (df_projections %>% filter(year == 2023) %>% select(r) %>% pull())
  r_gov_prem <- (1-r_gov_0/r_0)*(r_0)
  
  df_projections <- df_projections %>%
    mutate(r_gov_prem = r_gov_prem)
  
  # start with debt, interest spending, and health care
  df_projections <- df_projections %>%
    mutate(D = case_when(.default = NA,
                         year == min(years) ~ D_0),
           r_gov = case_when(.default = NA,
                             year == min(years) ~ r - r_gov_prem),
           R = case_when(.default = NA,
                         year == min(years) ~ D_prev*r_gov), # Old was just D #D_prev
           GHC = case_when(.default = NA,
                           year == min(years) ~ medicaid + medicare + other + public_health + hc_investment),
           GHC_fed = case_when(.default = NA,
                               year == min(years) ~ GHC - (1 - hc_fed_medicaid_coeff) * medicaid
                               - (1 - hc_fed_other_coeff) * other
                               - (1 - hc_fed_ph_coeff) * public_health
                               - (1 - hc_fed_inv_coeff) * hc_investment))
  # Now compute social security spending
  df_projections <- df_projections %>%
    mutate(SocSec = case_when(.default = NA,
                              year == min(years) ~ ssa_pop_coeff*pop_65plus*ssa_earn_coeff*w*
                                ssa_rep_rate*52*hrs_per_week/1000000000000),
           Bf = case_when(.default = NA,
                          year == min(years) ~ GHC_fed + SocSec))
  # Now compute taxes and deficit
  df_projections <- df_projections %>%
    mutate(eghi = case_when(.default = NA,
                            year == min(years) ~ f2_hat*0.3*p_rel),
           eghi_growth = 0,
           # compute SSA tax rate based on trustees assumptions
           ssa_tax = case_when(year == min(years) ~ 0,
                               year > min(years) & year < min(years) + 12 ~ (year - min(years))*0.00153,
                               .default = 0.0203),
           tax_revenue = case_when(.default = NA,
                                   year == min(years) ~ tau*Y+pi*(w*L-eghi_growth)+ssa_tax*SocSec + 0.15*medicare),
           deficit = case_when(.default = NA,
                               year == min(years) ~ G + Bf + R - tax_revenue))
  
  p_0 <- df_projections %>% filter(year == min(years)) %>% pull(p)
  w_0 <- df_projections %>% filter(year == min(years)) %>% pull(w)
  L_0 <- df_projections %>% filter(year == min(years)) %>% pull(L)
  deficit_0 <- df_projections %>% filter(year == min(years)) %>% pull(deficit)
  
  df_projections <- df_projections %>%
    mutate(
      p_0 = p_0,
      w_0 = w_0,
      L_0 = L_0,
      deficit_0 = deficit_0
    )

  return(df_projections)
}



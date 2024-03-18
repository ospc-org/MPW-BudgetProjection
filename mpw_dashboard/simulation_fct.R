# Solve model for every other year

# Now compute stuff for the next 75 years
estimate_model <- function(inv_coeff = 0.21,
                           non_health_spending_cut = 1,
                           df_projections = data.frame(),
                           years = years){
  # Iterate through projection years
  for (y in years){
    if (y == min(years)) next
    print(y)
    curr_year <- as.character(y)
    lag_year <- as.character(y-1)
    
    # Compute changes to prices and incomes for HC elasticity
    
    # NHE is same in 2022 as 2021
    if (y == min(years) + 1){
      df_projections <- df_projections %>%
        mutate(GHC = case_when(.default = GHC,
                               year == y ~ medicaid + medicare + other + public_health + hc_investment),
               GHC_fed = case_when(.default = GHC_fed,
                                   year == y ~ GHC - (1 - hc_fed_medicaid_coeff) * medicaid
                                   - (1 - hc_fed_other_coeff) * other
                                   - (1 - hc_fed_ph_coeff) * public_health
                                   - (1 - hc_fed_inv_coeff) * hc_investment))
    } else {
      # compute elasticity adjustment factor
      # Use adjustment factor to scale HC spending
      df_projections <- df_projections %>%
        mutate(rel_change_w = ((lag(w)*lag(L))-(w_0*L_0))/(w_0*L_0),
               rel_change_p = (lag(p)-p_0)/p_0,
               hc_el_adj = 1 + rel_change_w*inc_el + rel_change_p*pri_el) %>%
        mutate(medicare = case_when(.default = medicare,
                                    year == y ~ medicare*hc_el_adj),
               medicaid = case_when(.default = medicaid,
                                    year == y ~ medicaid*hc_el_adj),
               private_insurance = case_when(.default = private_insurance,
                                             year == y ~ private_insurance*hc_el_adj),
               oop = case_when(.default = oop,
                               year == y ~ oop*hc_el_adj),
               other = case_when(.default = other,
                                 year == y ~ other*hc_el_adj),
               public_health = case_when(.default = public_health,
                                         year == y ~ public_health*hc_el_adj),
               hc_investment = case_when(.default = hc_investment,
                                         year == y ~ hc_investment*hc_el_adj),
               GHC = case_when(.default = GHC,
                               year == y ~ medicaid + medicare + other + public_health + hc_investment),
               GHC_fed = case_when(.default = GHC_fed,
                                   year == y ~ GHC - (1 - hc_fed_medicaid_coeff) * medicaid
                                   - (1 - hc_fed_other_coeff) * other
                                   - (1 - hc_fed_ph_coeff) * public_health
                                   - (1 - hc_fed_inv_coeff) * hc_investment),
               f2_hat = case_when(.default = f2_hat,
                                  year == y ~ medicare + medicaid + private_insurance + oop + other + public_health + hc_investment))
    }
    
    # Compute capital based on existing stocks and last years investment
    df_projections <- df_projections %>%
      mutate(K = case_when(.default = K,
                           year == y ~ lag(K)*(1-k_dep)+ lag(I)))
    
    # Compute labor and capital to all other and HC
    df_projections <- df_projections %>%
      mutate(K2 = case_when(.default = K2,
                            year == y ~ f2_hat*beta3),
             K1 = case_when(.default = K1,
                            year == y ~ K - K2),
             L2 = case_when(.default = L2,
                            year == y ~ f2_hat*beta2/g_3),
             L1 = case_when(.default = L1,
                            year == y ~ L - L2))
    
    # Compute first-order conditions
    df_projections <- df_projections %>%
      mutate(B = case_when(.default = B,
                           year == y ~ (K-K2)/(g_1*(L-L2))),
             A = case_when(.default = A,
                           year == y ~ alpha1*(g_1*(1.0-beta1)*(B)^beta1*L + beta1*(B)^(beta1-1)*K - B^beta1*(g_1*(L-L2)))),
             p = case_when(.default = p,
                           year == y ~ A/f2_hat),
             w = case_when(.default = w,
                           year == y ~ (1-beta1)*alpha1*g_1*((K-K2)/(g_1*(L-L2)))^beta1),
             r = case_when(.default = r,
                           year == y ~ beta1*alpha1*((K-K2*g_2)/(g_1*(L-L2)))^(beta1-1)),
             p_rel = case_when(.default = p_rel,
                               year == y ~ p/p_0))
    # Compute output
    df_projections <- df_projections %>%
      mutate(f_1 = case_when(.default = f_1,
                             year == y ~ alpha1*(((g_1*L1)^(1-beta1))*K1^beta1)),
             f_2 = case_when(.default = f_2,
                             year == y ~ (g_2*K2)/beta3),
             Y = case_when(.default = Y,
                           year == y ~ f_1 + p_rel*f_2))
    # Compute macro identities
    df_projections <- df_projections %>%
      mutate(Def = case_when(.default = Def,
                             year == y ~ def_coeff*Y*non_health_spending_cut),
             Non_def = case_when(.default = Non_def,
                                 year == y ~ non_def_coeff*population*non_health_spending_cut),
             G = case_when(.default = G,
                           year == y ~ Def + Non_def))
    
    # Solve federal Government
    
    # start with debt, interest spending, and health care
    df_projections <- df_projections %>%
      mutate(D = case_when(.default = D,
                           year == y ~ lag(D) + lag(deficit)),
             r_gov = case_when(.default = r_gov,
                               year == y ~ r - r_gov_prem),
             R = case_when(.default = R,
                           year == y ~ lag(D)*r_gov)
      )
    
    # Now compute social security spending
    df_projections <- df_projections %>%
      mutate(SocSec = case_when(.default = SocSec,
                                year == y ~ ssa_pop_coeff*pop_65plus*ssa_earn_coeff*w*
                                  ssa_rep_rate*52*hrs_per_week/1000000000000),
             Bf = case_when(.default = Bf,
                            year == y ~ GHC_fed*p_rel + SocSec))
    
    # Now compute taxes and deficit
    df_projections <- df_projections %>%
      mutate(eghi = case_when(.default = eghi,
                              year == y ~ f2_hat*0.3*p_rel),
             eghi_growth = case_when(.default = eghi_growth,
                                     year == y ~ eghi - lag(eghi)),
             tax_revenue = case_when(.default = tax_revenue,
                                     year == y ~ tau*Y + pi*(w*L-eghi_growth) + ssa_tax*SocSec + 0.15*medicare),
             deficit = case_when(.default = deficit,
                                 year == y ~ G + Bf + R - tax_revenue))
    
    # Now compute investment and consumption
    df_projections <- df_projections %>%
      mutate(I = case_when(.default = I,
                           year == y ~ inv_coeff*Y - 0.15 * (deficit-deficit_0)),
             C = case_when(.default = C,
                           year == y ~ Y - I - G - 0.61 * (deficit-deficit_0)))
  }
  return(df_projections)
}

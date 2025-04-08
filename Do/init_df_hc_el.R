# Compute health care elasticities
###############
# Compute health care spending elasticities to be used
# Baseline
# Income elasticity
# Begins at 1.2
# declines to 1.1 over first ten years
# declines to 1.0 over next 15 years
# declines to 0.9 over next 25 years
# constant for remainder
# Price elasticity
# Begins at -0.5
# declines to -0.56 over first ten years
# constant for remainder

init_df_hc_el <- function(
    years = 2023:2096,
    hc_inc_el_input = 1.2,
    hc_pri_el_input = -0.5
){
  
  inc_el_0 <- hc_inc_el_input
  pri_el_0 <- hc_pri_el_input
  
  df_hc_el <- data.frame(
    year = years,
    inc_el = NA,
    pri_el = NA
  )
  # Populate data frame 
  if (inc_el_0 == 1.2) {
    df_hc_el <- df_hc_el %>%
      mutate(inc_el = case_when(year == min(years) ~ inc_el_0,
                                between(year, min(years) + 1, min(years) + 10) ~ inc_el_0 - 0.01*(year - (min(years))),
                                between(year, min(years) + 10, min(years) + 25) ~ inc_el_0 - 0.1 - (0.1/15)*(year - (min(years) + 10)),
                                between(year, min(years) + 25, min(years) + 50) ~ inc_el_0 - 0.2 - (0.1/25)*(year - (min(years) + 25)),
                                .default = 0.9
      ))
  } else {
    df_hc_el <- df_hc_el %>%
      mutate(inc_el = inc_el_0)
  } 
  
  if (pri_el_0 == -0.5) {
    df_hc_el <- df_hc_el %>%
      mutate(pri_el = case_when(year == min(years) ~ pri_el_0,
                                between(year, min(years) + 1, min(years) + 10) ~ pri_el_0 - 0.006*(year - (min(years))),
                                .default = -0.56))
  } else {
    df_hc_el <- df_hc_el %>%
      mutate(pri_el = pri_el_0)
  }
  df_hc_el
}



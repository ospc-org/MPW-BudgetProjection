
## Dashboard code for Mantus, Pang, Warshawsky (2023), [*A Unified Long-Run Macroeconomic Projection of Health Care Spending, the Federal Budget, and Benefit Programs in the US*](https://www.aei.org/research-products/working-paper/a-unified-long-run-macroeconomic-projection-of-health-care-spending-the-federal-budget-and-benefit-programs-in-the-us/)

## Please check [the interactive web app](https://ospc.shinyapps.io/ospc_mpw_dashboard/) hosted on shinyapps
## Dashboard is developed by [John Mantus](https://www.johnmantus.com/)

## Abstract
In the official models for projections and policy analysis (used by the Treasury, the Social Security and Medicare Trustees, and the Congressional Budget Office (CBO)), many key variables are assumed as a continuation of past trends. By contrast, in our model, these variables are simultaneously determined by supply and demand, based on logical functional forms and parameter estimates from the literature or empirical analysis. This approach better reflects real economic relationships—between health care spending, the federal budget, and investment in capital—and changing underlying conditions, especially demographics. Within the next ten years, we find the federal government budget deficit will grow significantly beyond historical experience and should be regarded as unsustainable. We project that debt-to-GDP will be 135 percent in 2032 and 268 percent in 2052, compared to CBO’s 112 percent and 177 percent, respectively. Real interest rates rise in the long run, ratcheting interest payments, deficits, and debt, and vice versa. Our projection of national health expenditures relative to GDP in 2072 is 31.4 percent, compared to 28.4 percent by the Centers for Medicare & Medicaid Services (CMS). These higher costs of health care arise from labor shortage effects in an aging economy because health care is produced in a low productivity, labor-dependent sector. Health care expenditure further deteriorates the federal budget and lowers consumer welfare.

This repo contains the code underlying the companion dashboard found on [OSPC](https://www.ospc.org/portfolio/).


## Note

The dashboard does not exactly replicate the results found in the working paper linked above. The working paper relies on a spreadsheet model in Excel while the dashboard is built in R using the [*shiny*](https://www.rstudio.com/products/shiny/) package. We believe these differences primarily result from differences in rounding but there may be other small differences. While the results are qualitatively similar, point estimates of future outcomes differ. In the case of NHE, e.g., these differences are negligible (both report a baseline of about 41.5% of GDP in 2095), but in other cases, e.g., debt to GDP, the outcome variable ultimately grows exponentially, meaning small rounding differences in early projection years lead to large differences in later years.
Ultimately, this dashboard serves as a tool for interested parties to see how various policy decisions and economic assumptions can change the major outcomes of interest in the MPW model.

## Files in this package

***readme.md*** - this one

### code
- **init_df_hc_el.R** - contains a simple helper function for computing future health care elasticities with respect to price and income
- **init_fct.R** - contains a helper function for initializing the projections, computing base year (2021) values
- **server.R** - code which directly ties the projection code to the user interface, beginning with baseline projections then updating when users change assumptions
- **simulation_fct.R** - simulates the MPW model in non-base years (2022-2095)
- **ui.R** - shiny UI for dashboard

### data
- **data/CBO_2025_Projections_cleaned.xlsx** - CBO demographic projections from 2023 through 2096
- **data/hc_spending_estimates_2023.xlsx** - per capita HC spending matrix by source of funds, sex, and age group
- **data/lf_proj_cbo_2023.xlsx** - annual labor force participation rates, as projected by CBO

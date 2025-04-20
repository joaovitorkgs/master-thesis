# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data from the pipeline --------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 3. Linear Regressions (Fixed Effects) ----------------------------------------

# Function to log-transform null values and return small constant
safe_log <- function(x) {
  ifelse(x > 0, 
         log(x), 
         ifelse(x == 0, 
                log(0.01),  # Small constant for zeros
                -log(abs(x) + 1)))  # For negative values
}

# Transforming variables in the dataframe
df_combined_state_transformed <- df_combined_state %>%
  mutate(across(c(electric, population, PHEV, gasoline, ethanol,
                  min_ev, mean_gas, mean_hyb, 
                  avg_taxable_income_50, avg_taxable_income_100),
                ~ safe_log(.x),
                .names = "log_{.col}"))

# Running fixed-effects models

fe_model_1 <- plm(log_electric ~ 
                    log_population,
                  data = df_combined_state_transformed,
                  model = "within",
                  index = c("sigla_uf", "date"))

fe_model_2 <- plm(log_electric ~ 
                    log_population + 
                    log_PHEV + log_gasoline + log_ethanol,
                  data = df_combined_state_transformed,
                  model = "within",
                  index = c("sigla_uf", "date"))

fe_model_3 <- plm(log_electric ~ 
                    log_population + 
                    log_PHEV + log_gasoline + log_ethanol +
                    log_min_ev + log_mean_gas + log_mean_hyb,
                  data = df_combined_state_transformed,
                  model = "within",
                  index = c("sigla_uf", "date"))

fe_model_4 <- plm(log_electric ~ 
                    log_population + 
                    log_PHEV + log_gasoline + log_ethanol +
                    log_min_ev + log_mean_gas + log_mean_hyb + 
                    log_avg_taxable_income_50 + log_avg_taxable_income_100,
                  data = df_combined_state_transformed,
                  model = "within",
                  index = c("sigla_uf", "date"))


stargazer(fe_model_1, fe_model_2, fe_model_3, fe_model_4, type = "html",
          title = "BEV stock regressed on population size, time, vehicle prices and income with panel data and fixed effects",
          single.row = FALSE,
          dep.var.labels = c("BEV stock (log)"),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          keep.stat = c("rsq", "f"),
          notes.align = "l",
          omit = "as.factor\\(months_nr\\)[0-9]+",
          intercept.bottom = TRUE,
          covariate.labels=c("Population",
                             "PHEV Stock",
                             "ICE Stock - Gasoline",
                             "ICE Stock - Ethanol",
                             "Vehicle price - BEV",
                             "Vehicle price - ICE (log)",
                             "Vehicle price - ICE (log)",
                             "Avg. Income - 50th perc. (log)",
                             "Avg. Income - 100th perc. (log)",
          out = "./5_analysis/1_regression_tables/fe_model.html"))

# 4. Linear Regressions (Time Series) ------------------------------------------

fleet_df_ts <- df_combined_state %>% 
  ungroup() %>% 
  select(sigla_uf, date, year, month, 
         diesel, ethanol, gasoline, electric, PHEV,
         population, 
         mean_ev, mean_gas, mean_hyb,
         avg_taxable_income_50, avg_taxable_income_100) %>% 
  group_by(date) %>% 
  summarize(
    electric                = sum(electric),
    population              = sum(population),
    mean_ev                 = mean(mean_ev),
    mean_gas                = mean(mean_gas),
    mean_hyb                = mean(mean_hyb),
    avg_taxable_income_50   = mean(avg_taxable_income_50),
    avg_taxable_income_100  = mean(avg_taxable_income_100),
    log_electric            = log(electric),
    log_population          = log(population),
    log_mean_ev             = log(mean_ev),
    log_mean_gas            = log(mean_gas),
    log_mean_hyb            = log(mean_hyb),
    log_avg_taxable_income_50  = log(avg_taxable_income_50),
    log_avg_taxable_income_100 = log(avg_taxable_income_100)
  ) %>%  
  mutate(
    electric   = if_else(date == as.Date("2018-06-01"), electric   / 2, electric),
    population = if_else(date == as.Date("2018-06-01"), population / 2, population)
  )

fleet_df_ts <- fleet_df_ts %>% arrange(date)

# Extract start year and month
start_year <- as.numeric(format(min(fleet_df_ts$date), "%Y"))
start_month <- as.numeric(format(min(fleet_df_ts$date), "%m"))


# Convert to a multivariate time series object
multivariate_ts <- ts(
  fleet_df_ts,
  start = c(start_year, start_month),
  frequency = 12
)

### Time Series Models

ts_model_1 <- tslm(log_electric ~ 
                     log_population, 
                   data = multivariate_ts)


ts_model_2 <- tslm(log_electric ~ 
                     log_population * 
                     log_mean_ev + 
                     log_mean_gas + 
                     log_mean_hyb,
                   data = multivariate_ts)

ts_model_3 <- tslm(log_electric ~ 
                     log_population * 
                     log_mean_ev + 
                     log_mean_gas + 
                     log_mean_hyb +
                     log_avg_taxable_income_50 +
                     log_avg_taxable_income_100,
                   data = multivariate_ts)

options(scipen = 999)
summary(ts_model_1)
summary(ts_model_2)
summary(ts_model_3)


CV(ts_model_3)


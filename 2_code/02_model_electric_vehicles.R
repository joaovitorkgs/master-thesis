# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Importing processed dataframes---------------------------------------------

df_fuel        <- read_csv("3_processed_data/fuel_yearly_agg_data_wide.csv")           # 2016 to 2023
df_population  <- read_csv("3_processed_data/yearly_pop_data.csv")                     # 2000 to 2021
df_tax_revenue <- read_csv("3_processed_data/taxrev_federal_gasstation_fueltaxes.csv") # 2013 to 2024
df_fleet       <- read_csv("3_processed_data/fleet_yearly_state_wide.csv")             # 2013 to 2024


# 3. Further processing data ---------------------------------------------------

df_fuel <- df_fuel %>% 
  filter(year >= 2013 & year <= 2021)

df_population <- df_population %>% 
  filter(ano >= 2013 & ano <= 2021) %>% 
  rename(year = ano)

df_tax_revenue <- df_tax_revenue %>% 
  mutate(year = as.numeric(substr(ano_mes_date, 1, 4))) %>% 
  group_by(year) %>% 
  summarize(fuel_taxes = sum(fuel_taxes)) %>% 
  filter(year >= 2013 & year <= 2021) 
  
df_fleet       <- df_fleet %>% 
  filter(year >= 2013 & year <= 2021)
  
df_all <- df_fleet %>% 
  left_join(df_population,  by = "year") %>% 
  left_join(df_tax_revenue, by = "year") %>% 
  left_join(df_fuel,        by = "year") %>% 
  mutate(
    tax_per_capita = fuel_taxes/population
  )


# 4. Analysis ------------------------------------------------------------------

install.packages("Metrics")
install.packages("caret")
library(Metrics)
library(caret)


# Function to calculate RMSE
calculate_rmse <- function(model, data) {
  predictions <- predict(model, data)
  rmse(data$ev_count, predictions)
}

require(caret)

set.seed(123)
train_index <- createDataPartition(df_all$electric, p = 0.8, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

















test_data <- df_all

# Compare RMSE for each model
lm_rmse <- calculate_rmse(lm_model, test_data)
rf_rmse <- calculate_rmse(rf_model, test_data)
xgb_rmse <- rmse(test_data$ev_count, predict(xgb_model, as.matrix(test_data %>% select(-ev_count))))

print(paste("Linear Regression RMSE:", lm_rmse))
print(paste("Random Forest RMSE:", rf_rmse))
print(paste("XGBoost RMSE:", xgb_rmse))


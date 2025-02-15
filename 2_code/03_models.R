# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Data from project pipeline ---------------------------------------------

fleet_2013_2024_clean <- read_csv("./3_processed_data/fleet_2013_2024_id_clean.csv")


# 3. Models --------------------------------------------------------------------

## 3.1. Supervised Learning ----------------------------------------------------

### 3.1.1. Simple Linear Regression --------------------------------------------

months <- fleet_2013_2024_clean %>% 
  arrange(date) %>%  
  distinct(date) %>%  
  mutate(months_nr = row_number()) 

fleet_df <- fleet_2013_2024_clean %>% 
  mutate(ev_pc        = electric / populacao,
         ff_pc        = (gasoline+diesel)/populacao,
         log_electric = log(electric),
         log_ff       = log(gasoline+diesel),
         log_pop      = log(populacao)) %>% 
  filter(log_electric >= 0) %>% 
  left_join(months, by="date")

summary(fleet_df)

options(scipen = 999)


model1 <- lm(log_electric ~ months_nr, data = fleet_df)
stargazer(model1, type = "text",
          title="Linear Regression Results: Model 1", single.row=TRUE)

stargazer(model1, type = "html",
          title="Linear Regression Results: Model 1", single.row=TRUE,
          out = "./5_analysis/1_regression_tables/lm_model1.html")

model2 <- lm(log_electric ~ log_pop * months_nr, data = fleet_df)

stargazer(model2, type = "text",
          title="Linear Regression Results: Model 2", single.row=TRUE)

stargazer(model2, type = "html",
          title="Linear Regression Results: Model 2", single.row=TRUE,
          out = "./5_analysis/1_regression_tables/lm_model2.html")


### 3.1.2 Panel data with fixed effects for cities -----------------------------

panel_data <- pdata.frame(fleet_df, index = c("id_municipio_nome", "date"))

fe_model_1 <- plm(electric ~ log(populacao) + as.factor(months_nr),
                data  = panel_data,
                model = "within")

fe_model_2 <- plm(electric ~ log(populacao) + log(gasoline) + log(diesel) + log(ethanol) + as.factor(months_nr),
                  data  = panel_data,
                  model = "within")

stargazer(fe_model_1, fe_model_2, type = "text")

stargazer(fe_model_1, fe_model_2, type = "html",
          title="Fixed-Effects Linear Regression Results: Panel Data", single.row=TRUE,
          out = "./5_analysis/1_regression_tables/fe_models_1&2.html")


### 3.1.3 Panel data with fixed effects for states -----------------------------

panel_data <- pdata.frame(fleet_df, index = c("id_municipio_nome", "date"))

fe_model_1 <- plm(electric ~ log(populacao) + as.factor(months_nr),
                  data  = panel_data,
                  model = "within")

fe_model_2 <- plm(electric ~ log(populacao) + log(gasoline) + log(diesel) + log(ethanol) + as.factor(months_nr),
                  data  = panel_data,
                  model = "within")

stargazer(fe_model_1, fe_model_2, type = "text")

stargazer(fe_model_1, fe_model_2, type = "html",
          title="Fixed-Effects Linear Regression Results: Panel Data", single.row=TRUE,
          out = "./5_analysis/1_regression_tables/fe_models_1&2.html")




### 3.1.3. Time Series ---------------------------------------------------------

# Comment: I am not sure if this makes sense. Left some initial code here, but
# will be skipping this for now.

# Aggregate data to monthly national totals
national_ts <- fleet_df %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarize(
    electric_vehicles = sum(electric),
    total_population = sum(populacao),
  )

library(zoo)

ts_data <- zoo(national_ts[, c("electric_vehicles", "total_population")], 
               order.by = national_ts$date)

install.packages("forecast")
library(forecast)

# Fit ARIMA model
arima_model <- arima(ts_data[, "electric_vehicles"], order=c(1,1,0))


# Forecast
forecast_result <- forecast(arima_model, h = 12)  # Forecast next 12 months
print(forecast_result)
plot(forecast_result,
     main = "Forecast of Electric Vehicles",
     ylab = "Electric Vehicles",
     xlab = "Date")



## 3.1. Regularized regression -------------------------------------------------

# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept



X <- model.matrix(electric ~ ., fleet_2013_2024_clean)[, -1]

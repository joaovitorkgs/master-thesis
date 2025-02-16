# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Data from project pipeline ---------------------------------------------

fleet_2013_2024_clean <- read_csv("./3_processed_data/fleet_2013_2024_id_clean.csv")
price_df <- read_csv("3_processed_data/fipe_price_monthly_trends_deflated.csv")


# 3. Models --------------------------------------------------------------------

## Preparing data --------------------------------------------------------------

### a) Fleet size and population -----------------------------------------------

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

### b) Price information -------------------------------------------------------

fleet_df_b_city <- fleet_df %>% 
  left_join(price_df, by = "date") 

fleet_df_b_uf <- fleet_df %>% 
  left_join(price_df, by = "date") %>% 
  group_by(sigla_uf, date, months_nr) %>% 
  summarize(diesel     = sum(diesel),
            ethanol    = sum(ethanol),
            gasoline   = sum(gasoline),
            other      = sum(other),
            electric   = sum(electric),
            gas        = sum(gas),
            population = sum(populacao))

fleet_df_b_uf <- fleet_df %>% 
  left_join(price_df, by = "date") %>% 
  group_by(sigla_uf, date, months_nr) %>% 
  summarize(diesel     = sum(diesel),
            ethanol    = sum(ethanol),
            gasoline   = sum(gasoline),
            other      = sum(other),
            electric   = sum(electric),
            gas        = sum(gas),
            population = sum(populacao),
            mean_gas   = mean(mean_price_gasoline),
            mean_hyb   = mean(mean_price_hybrid),
            mean_ev    = mean(mean_price_electric),
            median_gas = mean(median_price_gasoline),
            median_hyb = mean(median_price_hybrid),
            median_ev  = mean(median_price_electric),
            min_gas    = mean(min_price_gasoline),
            min_hyb    = mean(min_price_hybrid),
            min_ev     = mean(min_price_electric)) %>% 
  mutate(ev_pc        = electric / population,
         ff_pc        = (gasoline+diesel)/population,
         log_electric = log(electric),
         log_ff       = log(gasoline+diesel),
         log_pop      = log(population))


## 3.1. Supervised Learning ----------------------------------------------------

### 3.1.1. Simple Linear Regression --------------------------------------------


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


model3 <- lm(log_electric ~ log_pop * months_nr + mean_ev + mean_gas + mean_hyb, data = fleet_df_b_uf)

stargazer(model3, type = "text",
       title="Linear Regression Results: Model 3", single.row=TRUE)




### 3.1.2 Panel data with fixed effects for cities -----------------------------

panel_data <- pdata.frame(fleet_df, index = c("id_municipio_nome", "date"))

fe_model_1 <- plm(electric ~ log(populacao) + as.factor(months_nr),
                data  = panel_data,
                model = "within")

fe_model_2 <- plm(electric ~ log(populacao) + log(gasoline) + log(diesel) + log(ethanol) + as.factor(months_nr),
                  data  = panel_data,
                  model = "within")


panel_data_b <- pdata.frame(fleet_df_b_uf, index = c("sigla_uf", "date"))

fe_model_3 <- plm(electric ~ log(population) + log(gasoline) + log(diesel) + log(ethanol) + mean_ev + mean_gas + mean_hyb + as.factor(months_nr),
                  data  = panel_data_b,
                  model = "within",
                  singular.ok = TRUE)

stargazer(fe_model_1, fe_model_2, fe_model_3, type = "text")

stargazer(fe_model_1, fe_model_2, fe_model_3, type = "html",
          title="Fixed-Effects Linear Regression Results: Panel Data", single.row=TRUE,
          out = "./5_analysis/1_regression_tables/fe_models_1&2.html")

vif(fe_model_3)


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



## 3.2. Regularized regression -------------------------------------------------

# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept

X <- model.matrix(electric ~ ., fleet_df_b_uf)[, -1]
Y <- log(fleet_df_b_uf$electric)

# Ridge model
ridge_min <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

# Lasso model
lasso_min <- glmnet(
  x = X,
  y = Y,
  alpha = 1
)

par(mfrow = c(1, 2))
# plot ridge model
plot(ridge_min, xvar = "lambda", main = "Ridge penalty\n\n")
abline(v = log(ridge$lambda.min), col = "red", lty = "dashed")
abline(v = log(ridge$lambda.1se), col = "blue", lty = "dashed")

# plot lasso model
plot(lasso_min, xvar = "lambda", main = "Lasso penalty\n\n")
abline(v = log(lasso$lambda.min), col = "red", lty = "dashed")
abline(v = log(lasso$lambda.1se), col = "blue", lty = "dashed")


# for reproducibility
set.seed(123)

# grid search across 
cv_glmnet <- train(
  x = X,
  y = Y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# model with lowest RMSE
cv_glmnet$bestTune

cv_glmnet$results %>%
  filter(alpha == cv_glmnet$bestTune$alpha, lambda == cv_glmnet$bestTune$lambda)


# predict number of electric vehicles on training data
pred <- predict(cv_glmnet, X)

# compute RMSE of transformed predicted
RMSE(exp(pred), exp(Y))

# The first conclusion here is that this first model using just population data
# predicts a RMSE of 1550 given the observations (nr of evs in # all 25 Brazilian
# UFs in the past 11 years. )


## 4. Projections --------------------------------------------------------------


# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Data from project pipeline ---------------------------------------------

fleet_2013_2024_clean <- read_csv("./3_processed_data/fleet_2013_2024_id_clean.csv")
price_df <- read_csv("3_processed_data/fipe_price_monthly_trends_deflated.csv")
income_data_wide_uf <- read_csv("3_processed_data/income_data_wide_uf.csv")



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
  group_by(sigla_uf, date, year, month, months_nr) %>% 
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
  mutate(ff_pc        = (gasoline+diesel)/population,
         log_ff       = log(gasoline+diesel),
         log_pop      = log(population))


### c) Income information ------------------------------------------------------

fleet_df_c <- fleet_df_b_uf %>% 
  left_join(income_data_wide_uf, by = c("year", "sigla_uf")) %>% 
  drop_na()


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


model3 <- lm(log_electric ~ log_pop * months_nr + mean_ev + mean_gas + mean_hyb,
             data = fleet_df_b_uf)

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


stargazer(fe_model_1, fe_model_2, type = "text")


### 3.1.3 Panel data with fixed effects for states -----------------------------

panel_data <- pdata.frame(fleet_df_c, index = c("sigla_uf", "date"))

fe_model_1 <- plm(log(electric) ~ log(population) + as.factor(months_nr),
                  data  = panel_data,
                  model = "within")

fe_model_2 <- plm(log(electric) ~ log(population) + log(min_ev) + log(mean_gas) + log(mean_hyb) + as.factor(months_nr),
                  data  = panel_data,
                  model = "within")

fe_model_3 <- plm(log(electric) ~ log(population) + log(min_ev) + log(mean_gas) + log(mean_hyb) + log(avg_taxable_income_50) + log(avg_taxable_income_100) + as.factor(months_nr),
                  data  = panel_data,
                  model = "within")

stargazer(fe_model_1, fe_model_2, fe_model_3, type = "text")

stargazer(fe_model_1, fe_model_2, fe_model_3, fe_model_4, type = "html",
          title="Fixed-Effects Linear Regression Results: Panel Data", single.row=TRUE,
          out = "./5_analysis/1_regression_tables/fe_models_1-4.html")


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

library(glmnet)

X <- modglmnetX <- model.matrix(electric ~ ., fleet_df_c)[, -1]
Y <- log(fleet_df_c$electric)

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
# predicts a RMSE (Root Mean Square Error) of 372. 

# This means that, on average, my model}s predictions deviate
# from the actual number of electric vehicles by about 372 vehicles.

fleet_df_explore <- fleet_df_c %>% 
  group_by(year, sigla_uf) %>% 
  summarize(electric = mean(electric))


ggplot(fleet_df_explore, aes(x=electric)) +
  geom_histogram() +
  ggtitle("Distribution of Electric Vehicles in Brazil")+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +
  facet_wrap(~year, nrow = 3) +
  ylab("Density") +
  xlab("Amount of EVs in Brazil per Year") 

# Considering the data distribution, this is a large value for the RSME. This
# model is still performing poorly.






## 3.3. Multivariate Adaptive Regression Splines--------------------------------

### Models without interaction -------------------------------------------------

# Creating the MARS model object
mars1 <- earth(
  log(electric) ~ .,
  data = fleet_df_c
)

print(mars1)
summary(mars1)

# Ploting the model selection
plot(mars1, which = 1)

# Interpretation of the plot: the vertical dashed lined at 25 tells us the optimal 
# number of terms retained where marginal increases in GCV (generalized cross-validation)
# R²  are less than 0.001.

# For this model, 25 non-intercept terms were retained based on the 29 predictors.

### Models with interaction ----------------------------------------------------

# Creating the model object including non.linear relationships and polynomial terms
mars2 <- earth(
  log(electric) ~ .,
  data = fleet_df_c,
  degree = 2
)

# Summary of the most important predictors and interactions
summary(mars2) %>% .$coefficients %>% head(10)

# Preparing a grid with hyperparameters
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)
# Explanation: hyperparameters minimize prediction error by choosing how many terms
# in the final model and the complexity of the interactions. Grid search provides
# a set of different combinations of these two combinations. This is intended to
# prevent overfitting (the model does well for training data, but not well for testing
# or validating data) and underfitting (where the model is too simple to capture relevant
# patterns.)


# Training the algorithm 
set.seed(123)  # for reproducibility
cv_mars <- train(
  x = subset(fleet_df_c, select = -electric),
  y = fleet_df_c$electric,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# Finding the best hyperparameters
cv_mars$bestTune
cv_mars$results %>%
  filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)

# Plotting the RMSE according to the number of terms
ggplot(cv_mars)


# Mapping out the most relevant predictors
p1 <- vip(cv_mars, num_features = 25, geom = "point", value = "gcv") + ggtitle("GCV")
p2 <- vip(cv_mars, num_features = 25, geom = "point", value = "rss") + ggtitle("RSS")

# Population and log_pop are coming up as the most important predictors
# Maybe I should use a different predictor instead?


gridExtra::grid.arrange(p1, p2, ncol = 2)

# Check for interactions
cv_mars$finalModel %>%
  coef() %>%  
  broom::tidy() %>%  
  filter(stringr::str_detect(names, "\\*")) 
# So far, the model is not yielding any relevant variables


cv_mars$finalModel %>%
  coef() %>%  
  broom::tidy() %>%  
  filter(stringr::str_detect(names, "\\*")) 

# Construct partial dependence plots
p1 <- partial(cv_mars, pred.var = "population", grid.resolution = 10) %>% 
  autoplot()
p2 <- partial(cv_mars, pred.var = "avg_taxable_income_100", grid.resolution = 10) %>% 
  autoplot()
p3 <- partial(cv_mars, pred.var = c("population", "avg_taxable_income_100"), 
              grid.resolution = 10) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE, 
              screen = list(z = -20, x = -60))

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)


?rsample
# get attrition data
df <- rsample::attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

# Create training (70%) and test (30%) sets for the rsample::attrition data.
# Use set.seed for reproducibility
set.seed(123)
churn_split <- rsample::initial_split(df, prop = .7, strata = "Attrition")
churn_train <- rsample::training(churn_split)
churn_test  <- rsample::testing(churn_split)


# for reproducibiity
set.seed(123)

# cross validated model
tuned_mars <- train(
  x = subset(churn_train, select = -Attrition),
  y = churn_train$Attrition,
  method = "earth",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# best model
tuned_mars$bestTune
##   nprune degree
## 2     12      1

# plot results
ggplot(tuned_mars)









## 3.4. Decision Trees ---------------------------------------------------------

### Overall explanation --------------------------------------------------------  

# A basic decision tree partitions the training data into homogeneous subgroups 
# (i.e., groups with similar response values) and then fits a simple constant in each subgroup
# (e.g., the mean of the within group response values for regression). The subgroups
# (also called nodes) are formed recursively using binary partitions formed by asking
# simple yes-or-no questions about each feature (e.g., is age < 18?). This is done a number
# of times until a suitable stopping criteria is satisfied (e.g., a maximum depth of the tree is reached).
# After all the partitioning has been done, the model predicts the output based on (1) the
# average response values for all observations that fall in that subgroup (regression problem),
# or (2) the class that has majority representation (classification problem). For classification,
# predicted probabilities can be obtained using the proportion of each class within the subgroup.


### Data exploration -----------------------------------------------------------

# Before starting the models, I tried to visualize the relationship between some 
# key variables (population size, ev price, income levels) to see what would make
# sense to find in the model.

# Population has a clear positive correlation (maybe obvious)

ggplot(data = fleet_df_c, aes(x=population, y=electric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  ggtitle("Relationship between Population Size and EV stock per Brazilian States")+
  theme_bw() +
  ylab("Electric Cars (log)") +
  xlab("Average Taxable Income of the 100th Percentile (log)") +
  scale_x_continuous(trans = "log10", labels = scales::comma) + # Log scale for x-axis with meaningful labels
  scale_y_continuous(trans = "log10", labels = scales::comma) +  # Log scale for y-axis with meaningful labels
  facet_wrap(~year, nrow = 3, ncol = 4)

# Average taxable income of the richest percentile also seems to have a correlation

fleet_df_c %>% 
  group_by(year, sigla_uf) %>% 
  summarize(
    avg_taxable_income_100 = mean(avg_taxable_income_100),
    electric = mean(electric)) %>% 
  ggplot(aes(x=avg_taxable_income_100, y=electric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  ggtitle("Relationship between Average Income of the 100th Percentile and EV stock per Brazilian States")+
  theme_bw() +
  ylab("Electric Cars (log)") +
  xlab("Average Taxable Income of the 100th Percentile (log)") +
  scale_x_continuous(trans = "log10", labels = scales::comma) + # Log scale for x-axis with meaningful labels
  scale_y_continuous(trans = "log10", labels = scales::comma) +  # Log scale for y-axis with meaningful labels
  facet_wrap(~year, nrow = 3, ncol = 4)
  
# This is not true for the median income percentile

fleet_df_c %>% 
  group_by(year, sigla_uf) %>% 
  summarize(
    avg_taxable_income_50 = mean(avg_taxable_income_50),
    electric = mean(electric)) %>% 
  ggplot(aes(x=avg_taxable_income_50, y=electric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  ggtitle("Relationship between Average Income of the 50th Percentile and EV stock per Brazilian States")+
  theme_bw() +
  ylab("Electric Cars (log)") +
  xlab("Average Taxable Income of the 50th Percentile (log)") +
  scale_x_continuous(trans = "log10", labels = scales::comma) + # Log scale for x-axis with meaningful labels
  scale_y_continuous(trans = "log10", labels = scales::comma) +  # Log scale for y-axis with meaningful labels
  facet_wrap(~year, nrow = 3, ncol = 4)

# Also not true for the bottom 10 percentile

fleet_df_c %>% 
  group_by(year, sigla_uf) %>% 
  summarize(
    avg_taxable_income_10 = mean(avg_taxable_income_10),
    electric = mean(electric)) %>% 
  ggplot(aes(x=avg_taxable_income_10, y=electric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  ggtitle("Relationship between Average Income of the 10th Percentile and EV stock per Brazilian States")+
  theme_bw() +
  ylab("Electric Cars (log)") +
  xlab("Average Taxable Income of the 50th Percentile (log)") +
  scale_x_continuous(labels = scales::comma) + # Log scale for x-axis with meaningful labels
  scale_y_continuous(trans = "log10", labels = scales::comma) +  # Log scale for y-axis with meaningful labels
  facet_wrap(~year, nrow = 3, ncol = 4)


# EV prices may have a relevant correlation

fleet_df_c %>% 
  filter(year < 2017) %>% 
  group_by(date) %>% 
  summarize(
    ev_price = mean(mean_ev),
    electric = mean(electric)) %>% 
  ggplot(aes(x=ev_price, y=electric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add linear regression line
  ggtitle("Relationship between EV priceand EV stock per Month (2017-2022)")+
  theme_bw() +
  ylab("Electric Cars (log)") +
  xlab("EV average price (per month)") +
  scale_x_continuous(labels = scales::comma) + # Log scale for x-axis with meaningful labels
  scale_y_continuous(trans = "log10", labels = scales::comma) # Log scale for y-axis with meaningful labels




## 4. Projections --------------------------------------------------------------


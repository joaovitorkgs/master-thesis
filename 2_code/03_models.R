# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Data from project pipeline ---------------------------------------------

fleet_2013_2024_clean <- read_csv("./3_processed_data/fleet_2013_2024_id_clean.csv")


# 3. Models --------------------------------------------------------------------

## 3.1. Supervised Learning

### 3.1.1 Simple Linear Regression

months <- fleet_2013_2024_clean %>% 
  arrange(date) %>%  # Ensure dates are in order
  distinct(date) %>%  # Keep only unique dates
  mutate(months_nr = row_number())  # Add sequence number

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
stargazer(model1, type = "text")

model2 <- lm(log_electric ~ log_pop * months_nr, data = fleet_df)
stargazer(model2, type = "text")


### 3.1.1 Simple Linear Regression (as panel data with fixed effects for cities)

panel_data <- pdata.frame(fleet_df, index = c("id_municipio_nome", "date"))

fe_model_1 <- plm(electric ~ log(populacao) + as.factor(months_nr),
                data  = panel_data,
                model = "within")

fe_model_2 <- plm(electric ~ log(populacao) + log(gasoline) + log(diesel) + log(ethanol) + as.factor(months_nr),
                  data  = panel_data,
                  model = "within")

stargazer(fe_model_1, fe_model_2, type = "text")


## 3.1. Regularized regression -------------------------------------------------

# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept



X <- model.matrix(electric ~ ., fleet_2013_2024_clean)[, -1]

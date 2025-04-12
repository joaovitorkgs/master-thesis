# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------


fleet_2013_2024_clean <- read_csv("./3_processed_data/fleet_2013_2024_id_clean.csv")
price_df              <- read_csv("./3_processed_data/fipe_price_monthly_trends_deflated.csv")
income_data_wide_uf   <- read_csv("./3_processed_data/income_data_wide_uf.csv")

# 3. Data transformation for the models ----------------------------------------

## Organizing population data and adding months

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

## Adding price information and grouping by state
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

## Adding income distribution data
fleet_df_c <- fleet_df_b_uf %>% 
  left_join(income_data_wide_uf, by = c("year", "sigla_uf")) %>% 
  drop_na()

# 4. Time Series ---------------------------------------------------------------

## 4.1. Creating time series object --------------------------------------------

fleet_df_ts <- fleet_df_c %>% 
  ungroup() %>% 
  select(sigla_uf, date, year, month, electric, population, min_ev, mean_gas, mean_hyb, avg_taxable_income_50, avg_taxable_income_100) %>% 
  group_by(date) %>% 
  summarize(
    electric                = sum(electric),
    population              = sum(population),
    min_ev                  = mean(min_ev),
    mean_gas                = mean(mean_gas),
    mean_hyb                = mean(mean_hyb),
    avg_taxable_income_50   = mean(avg_taxable_income_50),
    avg_taxable_income_100  = mean(avg_taxable_income_100),
    log_electric               = log(electric),
    log_population             = log(population),
    log_min_ev                 = log(min_ev),
    log_mean_gas               = log(mean_gas),
    log_mean_hyb               = log(mean_hyb),
    log_avg_taxable_income_50  = log(avg_taxable_income_50),
    log_avg_taxable_income_100 = log(avg_taxable_income_100)
  ) %>%  
  mutate(
    electric   = if_else(date == as.Date("2018-06-01"), electric   / 2, electric),
    population = if_else(date == as.Date("2018-06-01"), population / 2, population)
  )

fleet_df_ts_log <- fleet_df_c %>% 
  ungroup() %>% 
  select(sigla_uf, date, year, month, electric, population, min_ev, mean_gas, mean_hyb, avg_taxable_income_50, avg_taxable_income_100) %>% 
  group_by(date) %>% 
  summarize(
    electric                = sum(electric),
    population              = sum(population),
    min_ev                  = mean(min_ev),
    mean_gas                = mean(mean_gas),
    mean_hyb                = mean(mean_hyb),
    avg_taxable_income_50   = mean(avg_taxable_income_50),
    avg_taxable_income_100  = mean(avg_taxable_income_100),
    log_electric               = log(electric),
    log_population             = log(population),
    log_min_ev                 = log(min_ev),
    log_mean_gas               = log(mean_gas),
    log_mean_hyb               = log(mean_hyb),
    log_avg_taxable_income_50  = log(avg_taxable_income_50),
    log_avg_taxable_income_100 = log(avg_taxable_income_100)
  ) %>% 
  select(date,log_electric:log_avg_taxable_income_100) 

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

multivariate_ts_2 <- ts(
  fleet_df_ts_log,
  start = c(start_year, start_month),
  frequency = 12
)



## 4.2. Models -----------------------------------------------------------------

### Linear Models

ts_model_1 <- tslm(log_electric ~ 
                     log_population, 
                   data = multivariate_ts)


ts_model_2 <- tslm(log_electric ~ 
                     log_population * 
                     log_min_ev + 
                     log_mean_gas + 
                     log_mean_hyb,
                   data = multivariate_ts)

ts_model_3 <- tslm(log_electric ~ 
                     log_population * 
                     log_min_ev + 
                     log_mean_gas + 
                     log_mean_hyb +
                     log_avg_taxable_income_50 +
                     log_avg_taxable_income_100,
                   data = multivariate_ts)

options(scipen = 999)
summary(ts_model_3)

### Comparing linear and non-linear models
h <-  90
t <- time(multivariate_ts)

#### Linear
ts_model_lin <- tslm(electric ~ trend, data = multivariate_ts)
fcasts.lin <- forecast(ts_model_lin, h = h)

#### Exponential
ts_model_exp <- tslm(electric ~ trend, lambda = 0, data = multivariate_ts)


### Goodness of Fit tests ------------------------------------------------------

#### Correlation Matrix

plot_correlation <- multivariate_ts %>%
  as.data.frame() %>%
  select(log_electric:log_avg_taxable_income_100) %>% 
  rename(
      `BEV Stock`  = log_electric, 
      Population   = log_population, 
      `BEV prices` = log_min_ev,
      `ICEV (Gasol.) prices` = log_mean_gas,
      `ICEV (Hybrid) prices` = log_mean_hyb,
      `Income (50th)` = log_avg_taxable_income_50,
      `Income (100th)`= log_avg_taxable_income_100
  ) %>% 
  GGally::ggpairs()

ggsave(
  filename = "./4_plots/correlation_plot.png",
  plot     = plot_correlation,
  width    = 10,
  height   = 6)
  

#### First differences and Stationarity

# Schumway and Stoffer (pp. 56-59)

single_ts <- fleet_df_ts %>% 
  select(date, electric) 

electric_ts <- ts(single_ts$electric, start = c(2013, 5), frequency = 12)

fit <- lm(electric_ts ~time(electric_ts), na.action=NULL) # regress chicken on time 
par(mfrow=c(2,1))
plot(resid(fit), type="o", main="detrended")
plot(diff(electric_ts), type="o", main="first difference")
par(mfrow=c(3,1)) # plot ACFs
acf(electric_ts, 48, main="BEVs")
?acf(resid(fit), 48, main="Detrended")
acf(diff(electric_ts), 48, main="First difference")

# Save detrended and first difference plots
png("./4_plots/ts_detrended_and_first_difference.png", width = 800, height = 600)
par(mfrow = c(2, 1)) # Set up a 2x1 layout for plots
plot(resid(fit), type = "o", main = "Detrended")
plot(diff(electric_ts), type = "o", main = "First Difference")
dev.off() # Close the graphics device

# Save ACF plots
png("./4_plots/ts_acf_plots.png", width = 800, height = 900)
par(mfrow = c(3, 1)) # Set up a 3x1 layout for plots
acf(electric_ts, lag.max = 48, main = "BEVs")
acf(resid(fit), lag.max = 48, main = "Detrended")
acf(diff(electric_ts), lag.max = 48, main = "First Difference")
dev.off() # Close the graphics device


#### Classical and X11 decomposition of time series

# Classical
fit_2 <- decompose(electric_ts)
autoplot(fit_2)

electric_ts %>% decompose(type="multiplicative") %>% 
  autoplot() + 
  xlab("Year") + 
  ggtitle("Classical multiplicative decomposition of Brazil's BEV stock")

# X11 
fit_3 <- electric_ts %>% seas(x11="") 
autoplot(fit_3) +
  ggtitle("X11 decomposition of Brazil's BEV stock")


### Histogram of residuals

checkresiduals(ts_model_3)
png("./4_plots/residuals_plot.png", width = 1000, height = 800)
dev.off() # Close the graphics device

### Selecting predictors

CV(ts_model_1)




## 4.4. Forecasting ------------------------------------------------------------

### Linear regression model ----------------------------------------------------

h  <-  90

# Create future date sequence
last_date <- max(fleet_df_ts$date)
future_dates <- seq(last_date %m+% months(1), by = "month", length.out = 90)

# Initialize empty data frame
future_data <- data.frame(date = future_dates)

# Forecast each predictor using trend models
predictors <- c('log_population', 'log_min_ev', 'log_mean_gas', 
                'log_mean_hyb', 'log_avg_taxable_income_50', 
                'log_avg_taxable_income_100')

for(var in predictors) {
  trend_model <- tslm(as.formula(paste(var, "~ trend")), 
                      data = multivariate_ts_2)
  future_data[[var]] <- forecast(trend_model, h = 90)$mean
}

# Add trend component for interaction term
future_data$trend <- seq_len(nrow(future_data)) + 
  nrow(window(multivariate_ts, start=c(start_year,start_month)))

future_ts <- ts(future_data,
                start = c(year(last_date), month(last_date)),
                frequency = 12)

required_vars <- all.vars(formula(ts_model_3))
print(setdiff(required_vars, names(future_data)))

# Convert ts object to proper data frame
future_data <- as.data.frame(future_ts) %>% 
  setNames(colnames(multivariate_ts_2)) %>%  # Preserve original names
  select(all_of(required_vars)) %>%  # Explicit column selection
  mutate(across(everything(), as.numeric))  # Force numeric types

# Ensure numeric type conversion
future_data <- future_data %>% 
  mutate(across(everything(), as.numeric))

# Forecast with explicit newdata specification
fcast <- forecast(ts_model_3, newdata = future_data)


fleet_tsibble <- fleet_df_ts_log %>%
  as_tsibble(index = date)

# 2. Plot using autoplot
fleet_tsibble %>% 
  select(log_electric) %>% 
  autoplot()

autoplot(fcast)


### Exp model ------------------------------------------------------------------

fcast_exp <- forecast(ts_model_exp, h=h)
autoplot(fcast_exp)

### Goodness of fit (exp model) ------------------------------------------------

time_df <- data.frame(
  date = as.Date(time(multivariate_ts)),
  data = multivariate_ts[,'log_electric'],
  fitted = fitted(ts_model_3)
)

ggplot(time_df, aes(x = date)) +
  geom_line(aes(y = data, color = "Data")) +
  geom_line(aes(y = fitted, color = "Fitted")) +
  xlab("Year/Month") + 
  ylab("BEV Stock (log transformed)") +
  guides(colour = guide_legend(title = " "))








# 5. ARIMA ---------------------------------------------------------------------
## 5.1. Unit root tests --------------------------------------------------------

electric_ts %>% diff(lag=12) %>% ur.kpss() %>% summary()

# The result of these diagnostics show a non-stationary time series


## 5.2. Seasonal ARIMA models --------------------------------------------------

electric_ts %>% diff() %>% ggtsdisplay()

electric_ts %>% 
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

fit_arima <- Arima(electric_ts, order=c(0,1,3), seasonal=c(0,1,1))



## 5.3. Forecasting ------------------------------------------------------------

fit_arima %>% forecast(h=120) %>%
  autoplot() +
  ylab("BEV Stock in Brazil") + xlab("Year")

auto.arima(electric_ts)

forecast(fit_arima)







## Goodness-of-fit measures

accuracy(fit_arima)
accuracy(ts_model_3)

# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data from the pipeline --------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 3. Linear models -------------------------------------------------------------

## First differences and Stationarity ------------------------------------------

# Schumway and Stoffer (pp. 56-59)

fit <- lm(univariate_ts ~time(univariate_ts), na.action=NULL) # regress BEVs on time 
par(mfrow=c(2,1))
plot(resid(fit), type="o", main="detrended")
plot(diff(electric_ts), type="o", main="first difference")
# Save detrended and first difference plots
png("./4_plots/ts_detrended_and_first_difference_updated.png", width = 800, height = 600)

par(mfrow=c(3,1)) # plot ACFs
acf(univariate_ts, 48, main="BEVs")
acf(resid(fit), 48, main="Detrended")
acf(diff(electric_ts), 48, main="First difference")
# Save ACF plots
png("./4_plots/ts_ACF_updated.png", width = 800, height = 600)

par(mfrow = c(2, 1)) # Set up a 2x1 layout for plots
plot(resid(fit), type = "o", main = "Detrended")
plot(diff(electric_ts), type = "o", main = "First Difference")
# Save ACF plots
png("./4_plots/ts_acf_plots_updated.png", width = 800, height = 900)
dev.off() # Close the graphics device

par(mfrow = c(3, 1)) # Set up a 3x1 layout for plots
acf(electric_ts, lag.max = 48, main = "BEVs")
acf(resid(fit), lag.max = 48, main = "Detrended")
acf(diff(electric_ts), lag.max = 48, main = "First Difference")
dev.off() # Close the graphics device


## Classical and X11 decomposition of time series ------------------------------

# Classical
fit_2 <- decompose(univariate_ts)
autoplot(fit_2)

univariate_ts %>% decompose(type="multiplicative") %>% 
  autoplot() + 
  xlab("Year") + 
  ggtitle("Classical multiplicative decomposition of Brazil's BEV stock")

# X11 
fit_3 <- univariate_ts %>% seas(x11="") 
autoplot(fit_3) +
  ggtitle("X11 decomposition of Brazil's BEV stock")


## Histogram of residuals ------------------------------------------------------

checkresiduals(fit_3)
png("./4_plots/residuals_plot_updated.png", width = 1000, height = 800)
dev.off() # Close the graphics device


# 3. Exp Model (quadratic order trend) -----------------------------------------

### Exp model ------------------------------------------------------------------

options(scipen = 999)
ts_model_exp <- tslm(BEV ~ trend, lambda = 0, data = univariate_ts)
h <-  (5 * 12)

fcast_exp <- forecast(ts_model_exp, h=h)
autoplot(fcast_exp)

### Goodness of fit (exp model) ------------------------------------------------

# time_df <- data.frame(
#   date = as.Date(time(multivariate_ts)),
#   data = multivariate_ts[,'log_electric'],
#   fitted = fitted(ts_model_3)
# )

# ggplot(time_df, aes(x = date)) +
#   geom_line(aes(y = data, color = "Data")) +
#   geom_line(aes(y = fitted, color = "Fitted")) +
#   xlab("Year/Month") + 
#   ylab("BEV Stock (log transformed)") +
#   guides(colour = guide_legend(title = " "))



# 4. ARIMA ---------------------------------------------------------------------

## 4.1. Unit root tests --------------------------------------------------------

univariate_ts %>% diff(lag=12) %>% ur.kpss() %>% summary()

# The result of these diagnostics indicate a non-stationary time series

## Different ARIMA combinations

autoplot(univariate_ts) + 
  ggtitle("Time Series Plot of Electric Data") +
  xlab("Time") + ylab("Value")

# Seasonal decomposition
dec <- decompose(univariate_ts)
autoplot(dec)

# ACF and PACF plots
ggAcf(univariate_ts)
ggPacf(univariate_ts)

# If non-stationary, check differenced series
ggAcf(diff(univariate_ts))
ggPacf(diff(univariate_ts))

# Automatic ARIMA model selection
auto_fit <- auto.arima(univariate_ts, seasonal=TRUE, stepwise=FALSE, approximation=FALSE)
summary(auto_fit)

# Generate a grid of potential ARIMA models
models <- list()
orders <- expand.grid(p=0:2, d=0:2, q=0:2, P=0:1, D=0:1, Q=0:1)

# Function to safely fit ARIMA models
safe_arima <- function(p, d, q, P, D, Q, data) {
  model_name <- paste0("ARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")_12")
  tryCatch({
    model <- Arima(data, order=c(p, d, q), seasonal=list(order=c(P, D, Q), period=12))
    return(data.frame(
      model = model_name,
      AIC = model$aic,
      BIC = model$bic,
      AICc = model$aicc,
      RMSE = sqrt(mean(model$residuals^2)),
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    return(data.frame(
      model = model_name,
      AIC = NA,
      BIC = NA,
      AICc = NA,
      RMSE = NA,
      stringsAsFactors = FALSE
    ))
  })
}

# Fit models and collect results
results <- list()
for(i in 1:nrow(orders)) {
  results[[i]] <- safe_arima(
    orders$p[i], orders$d[i], orders$q[i],
    orders$P[i], orders$D[i], orders$Q[i],
    univariate_ts
  )
}

# Combine results
all_models <- do.call(rbind, results)

# Sort by AIC
all_models_sorted <- all_models %>% 
  arrange(AIC) %>%
  filter(!is.na(AIC))

# Display top 10 models
head(all_models_sorted, 10)


# Export results to CSV
write.table(all_models_sorted,
            file = "./6_tables/arima_models_comparison_updated.csv", 
            sep = ";",
            row.names = FALSE,
            col.names = TRUE)



## 5.2. Forecasting ------------------------------------------------------------

### Diagnostic plots

univariate_ts %>% 
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

### Forecasting model

fit_arima <- Arima(univariate_ts, order=c(0,1,1), seasonal=c(0,1,1))

### Ploting forecasts

fit_arima %>% forecast(h=60) %>%
  autoplot() +
  ggtitle("Forecasting of BEV stock in Brazil: ARIMA model") +
  ylab("BEV Stock in Brazil") + xlab("Year")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

plot_ARIMA <- fit_arima %>% forecast(h=60) %>%
  autoplot() +
  ggtitle("Forecasting of BEV stock in Brazil: ARIMA model") +
  ylab("BEV Stock in Brazil") + xlab("Year")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

plot_ARIMA %>% 
  ggsave(filename = "./4_plots/plot_ARIMA_updated.png", 
         width = 8, height = 5)

forecast(fit_arima)


## Goodness-of-fit measures

accuracy(fit_arima)


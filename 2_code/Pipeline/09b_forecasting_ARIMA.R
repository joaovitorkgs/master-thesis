# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 2. SARIMA Diagnostics --------------------------------------------------------

## 2.1. Unit root tests --------------------------------------------------------

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



# 3. ARIMA Forecasting ---------------------------------------------------------

### Forecasting model ----------------------------------------------------------

# Using 2020-2025 data:
# The plot without decreasing projections uses (0,1,0)(0,1,0) as parameters, but
# It performs poorly with the metrics used. A trade off would be a (1,1,0)(0,1,1),
# which performs well, but projects negative data.

options(scipen=999)
Arima(univariate_ts, order=c(0,1,0), seasonal=c(0,1,1), include.mean = FALSE) %>% 
  forecast(h=60) %>% 
  autoplot()



fit_ARIMA <- Arima(univariate_ts, 
                   order=c(0,1,0), seasonal=c(0,1,1),
                   include.mean = FALSE)

### Diagnostic plots

fit_ARIMA %>%
  residuals() %>% ggtsdisplay()

# changing the first parameter to 1 yields a better visualisation of the residuals,
# but it also increases the confidence margin a lot. why?


### Data frame with forecasted values ------------------------------------------

fcast_ARIMA <- forecast(fit_ARIMA, h=60)


### Plotting forecasts ---------------------------------------------------------

plot_fcast_ARIMA <- fcast_ARIMA %>%
  autoplot() +
  ggtitle("Forecasting of BEV stock in Brazil: ARIMA model") +
  ylab("BEV Stock in Brazil") + xlab("Year")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

if (!file.exists("./4_plots/plot_fcast_TS.png")) {
  ggsave("./4_plots/plot_fcast_ARIMA.png",
         plot   = plot_fcast_ARIMA,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

### Performance metrics --------------------------------------------------------

accuracy(fcast_ARIMA)
accuracy_ARIMA <- accuracy(fcast_ARIMA)


### Observed vs. Fitted Plot ---------------------------------------------------

time_df_ARIMA <- data.frame(
  date = as.Date(time(multivariate_ts)),
  data = multivariate_ts[,'BEV'],
  fitted = fitted(fcast_ARIMA)
)

plot_fcast_fitvsobs_ARIMA <- ggplot(time_df_ARIMA, aes(x = date)) +
  geom_line(aes(y = data, color = "Data")) +
  geom_line(aes(y = x, color = "Fitted")) +
  xlab("Year/Month") + 
  ylab("BEV Stock (log transformed)") +
  guides(colour = guide_legend(title = " ")) +
  theme_bw()

if (!file.exists("./4_plots/plot_fcast_fitvsobs_ARIMA.png")) {
  ggsave("./4_plots/plot_fcast_fitvsobs_ARIMA.png",
         plot   = plot_fcast_fitvsobs_ARIMA,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}



# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 2. Prophet -------------------------------------------------------------------

## 2.1. Creating objects and models --------------------------------------------

# Original univariate timeseries
univariate_ts

# Converting it back to a df for the prophet package
start_date <- as.Date("2020-01-01") 
end_date   <- length(univariate_ts)
dates      <- seq.Date(from = start_date, by = "month", length.out = end_date)
df         <- data.frame(ds = dates, y = as.vector(univariate_ts))

# Creating the model
fit_Prophet <- prophet(df, seasonality.mode = "multiplicative", growth = "linear")

# Creating the future dataframe to be populated with forecasts
df_2030 <- make_future_dataframe(fit_Prophet, periods=60, freq='month')


## 2.2. Forecasting values and plotting results --------------------------------

# Running forecast
fcast_Prophet <- predict(fit_Prophet, df_2030)

# Plotting results
plot(fit_Prophet, fcast_Prophet)

prophet_plot_components(fit_Prophet, fcast_Prophet)


### Plotting forecasts ---------------------------------------------------------

plot_fcast_Prophet <- plot(fit_Prophet, fcast_Prophet)+ 
  labs(
    title = "Forecasting of BEV stock in Brazil: Meta's Prophet model",
    subtitle = "Model fitted with linear growth and multiplicative seasonality",
    x = "Year",
    y = "BEV Stock in Brazil")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

if (!file.exists("./4_plots/plot_fcast_Prophet.png")) {
  ggsave("./4_plots/plot_fcast_Prophet.png",
         plot   = plot_fcast_Prophet,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

### Performance metrics --------------------------------------------------------

# Cross-validation
df_cv <- cross_validation(fit_Prophet,
                          units    = "days",
                          horizon  = 180,
                          period   = 90,
                          initial  = 890)

df_p <- performance_metrics(df_cv)

plot_cross_validation_metric(df_cv, metric = 'rmse')


### Observed vs. Fitted Plot ---------------------------------------------------


fitted_values_Prophet <- fcast_Prophet %>% 
  head(60) %>% 
  select(yhat)



time_df_Prophet <- data.frame(
  date = as.Date(time(multivariate_ts)),
  data = multivariate_ts[,'BEV'],
  fitted = fitted_values_Prophet
)

plot_fcast_fitvsobs_Prophet <- ggplot(time_df_Prophet, aes(x = date)) +
  geom_line(aes(y = data, color = "Data")) +
  geom_line(aes(y = yhat, color = "Fitted")) +
  xlab("Year/Month") + 
  ylab("BEV Stock (log transformed)") +
  guides(colour = guide_legend(title = " ")) +
  theme_bw()


if (!file.exists("./4_plots/plot_fcast_fitvsobs_Prophet.png")) {
  ggsave("./4_plots/plot_fcast_fitvsobs_Prophet.png",
         plot   = plot_fcast_fitvsobs_Prophet,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}








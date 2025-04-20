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
                          units    = "days", # Prophet only takes days as unit
                          horizon  = 360,    # 12 months ≈ 360 days
                          period   = 180,    # 6 months ≈ 180 days
                          initial  = 720)    # 24 months ≈ 720 days

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

Prophet_RMSE <- sqrt(mean((time_df_Prophet$data - time_df_Prophet$yhat)^2))
Prophet_MAE  <- mean(abs(time_df_Prophet$data - time_df_Prophet$yhat))
Prophet_MAPE <- mean(abs((time_df_Prophet$data - time_df_Prophet$yhat)/time_df_Prophet$data))

## 2.3. Robustness checks with residuals ---------------------------------------

library(fable.prophet)
library(fpp3)
library(patchwork)

# Creating the special object used for the prophet package
univariate_ts_prophet <- tsibble::as_tsibble(univariate_ts)

# Creating objects both for training data and actual forecasting
prophet_fpp3_data  <- univariate_ts_prophet 
prophet_fpp3_train <- univariate_ts_prophet %>% 
  filter(year(index) <= 2022)

# To generate the plots for residuals (time, ACF and histogram plots), I had to
# recreate the model with similar specifications, but with a different package

# Fitting the model
fit_Prophet_fpp3 <- prophet_fpp3_data %>% 
  model(
    prophet = prophet(value ~ season("year", 1, type = "multiplicative")))

# Testing the forecasting
fcast_Prophet_fpp3 <- fit_Prophet_fpp3 %>% forecast(h = "5 years")

# Plotting the forecasted values in the second model
fcast_Prophet_fpp3 %>%  autoplot(prophet_fpp3_data)

# Generating the plots for the residuals
fit_Prophet_fpp3 %>% 
  gg_tsresiduals()

### Residuals Diagnostics plots ------------------------------------------------

# Steps to manually prepare the plots, so that they better match other models

# Extract residuals
res <- residuals(fit_Prophet_fpp3)

# Create the time plot with no y-axis label
p1 <- res %>%
  ggplot(aes(x = as.Date(index), y = .resid)) +
  geom_line() +
  geom_point(size = 0.7) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
  theme(axis.title.y = element_blank(),
        panel.grid.minor = element_line(color = "white"),
        panel.grid.major = element_line(color = "white")) +
  labs(title = "", x = "")

# Create the ACF plot
p2 <- ACF(res, .resid) %>% 
  autoplot() + 
  labs(title = "", y = "ACF", x = "Lag")

# Create the histogram
p3 <- ggplot(res, aes(x = .resid)) + 
  geom_histogram(bins = 30, aes(y = ..density..), fill = "grey1", alpha = 0.7) + 
  geom_density(color = "orangered", size = 0.5) +
  labs(title = "", y = "df$y", x = "")

# Arrange the plots
(p1 / (p2 + p3)) + plot_layout(heights = c(1, 1))

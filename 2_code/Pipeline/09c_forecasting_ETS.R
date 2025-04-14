# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 2. ETS Forecasting (Holt-Winter) ---------------------------------------------

# Holt-Winter's  method chosen over simple exponential smoothing due to seasonality
fit_ETS <- hw(univariate_ts, seasonal = "additive", h=60)

fit_ETS %>%
  residuals() %>% ggtsdisplay()



### Data frame with forecasted values ------------------------------------------

fcast_ETS <- forecast(fit_ETS, h=60)


### Plotting forecasts ---------------------------------------------------------

plot_fcast_ETS <- fcast_ETS %>%
  autoplot() +  
  labs(
    title = "Forecasting of BEV stock in Brazil: ETS model",
    subtitle = "ETS specifications according to Holt-Winter's Model",
    x = "Year",
    y = "BEV Stock in Brazil")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

if (!file.exists("./4_plots/plot_fcast_ETS.png")) {
  ggsave("./4_plots/plot_fcast_ETS.png",
         plot   = plot_fcast_ETS,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

### Performance metrics --------------------------------------------------------

accuracy(fcast_ETS)
accuracy_ETS <- accuracy(fcast_ETS)


### Observed vs. Fitted Plot ---------------------------------------------------

time_df_ETS <- data.frame(
  date = as.Date(time(multivariate_ts)),
  data = multivariate_ts[,'BEV'],
  fitted = fitted(fcast_ETS)
)

plot_fcast_fitvsobs_ETS <- ggplot(time_df_ETS, aes(x = date)) +
  geom_line(aes(y = data, color = "Data")) +
  geom_line(aes(y = y, color = "Fitted")) +
  xlab("Year/Month") + 
  ylab("BEV Stock (log transformed)") +
  guides(colour = guide_legend(title = " ")) +
  theme_bw()


if (!file.exists("./4_plots/plot_fcast_fitvsobs_ETS.png")) {
  ggsave("./4_plots/plot_fcast_fitvsobs_ETS.png",
         plot   = plot_fcast_fitvsobs_ETS,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

 

# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 2. Decomposing time series ---------------------------------------------------

## Classical and X11 decomposition of time series 

# Classical
decomp_classic <- decompose(univariate_ts)
autoplot(decomp_classic)

# X11 
decomp_x11 <- univariate_ts %>% seas(x11="") 
autoplot(decomp_x11) +
  ggtitle("X11 decomposition of Brazil's BEV stock")


# 3. Linear Regression model (quadratic order trend) ---------------------------

### Crating model --------------------------------------------------------------

options(scipen = 999)

fit_TS <- tslm(BEV ~ trend, lambda = 0, data = univariate_ts)
h <- 60

### Diagnostic plots

diagnostics_TS <- fit_TS %>%
  residuals() %>% ggtsdisplay()


### List with forecasted values ------------------------------------------------

fcast_TS <- forecast(fit_TS, h=h)

### Plots with forecasted values -----------------------------------------------

plot_fcast_TS <- autoplot(fcast_TS) +
  labs(
    title = "Forecasting of BEV stock in Brazil: Linear Regression Model",
    subtitle = "Linear Regression with Quadratic Order Trend",
    x = "Time",
    y = "BEV Value"
  ) +
  theme_bw(base_size = 12) + # Use a minimal theme for clarity
  theme(
    plot.title = element_text(), # Center and bold title
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    legend.position = "bottom" # Move legend to bottom for better visibility
  ) +
  scale_y_continuous(labels = scales::comma)

if (!file.exists("./4_plots/plot_fcast_TS.png")) {
  ggsave("./4_plots/plot_fcast_TS.png",
         plot   = plot_fcast_TS,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

### Performance metrics --------------------------------------------------------

accuracy(fcast_TS)
accuracy_TS <- accuracy(fcast_TS)


### Observed vs. Fitted Plot ---------------------------------------------------

time_df_TS <- data.frame(
   date = as.Date(time(multivariate_ts)),
   data = multivariate_ts[,'BEV'],
   fitted = fitted(fcast_TS)
 )

plot_fcast_fitvsobs_TS <- ggplot(time_df_TS, aes(x = date)) +
  geom_line(aes(y = data, color = "Data")) +
  geom_line(aes(y = fitted, color = "Fitted")) +
  xlab("Year/Month") + 
  ylab("BEV Stock (log transformed)") +
  guides(colour = guide_legend(title = " ")) +
  theme_bw()


if (!file.exists("./4_plots/plot_fcast_fitvsobs_TS.png")) {
  ggsave("./4_plots/plot_fcast_fitvsobs_TS.png",
         plot   = plot_fcast_fitvsobs_TS,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}



 

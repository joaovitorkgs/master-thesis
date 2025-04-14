# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 2. Assessing the time series -------------------------------------------------

## First differences and Stationarity ------------------------------------------

# Schumway and Stoffer (pp. 56-59)

fit <- lm(univariate_ts ~time(univariate_ts), na.action=NULL) # regress BEVs on time 
par(mfrow=c(2,1))
plot(resid(fit), type="o", main="detrended")
plot(diff(univariate_ts), type="o", main="first difference")
# Save detrended and first difference plots
png("./4_plots/ts_detrended_and_first_difference_updated.png", width = 800, height = 600)

par(mfrow=c(3,1)) # plot ACFs
acf(univariate_ts, 48, main="BEVs")
acf(resid(fit), 48, main="Detrended")
acf(diff(univariate_ts), 48, main="First difference")
# Save ACF plots
png("./4_plots/ts_ACF_updated.png", width = 800, height = 600)

par(mfrow = c(2, 1)) # Set up a 2x1 layout for plots
plot(resid(fit), type = "o", main = "Detrended")
plot(diff(univariate_ts), type = "o", main = "First Difference")
# Save ACF plots
png("./4_plots/ts_acf_plots_updated.png", width = 800, height = 900)
# dev.off() # Close the graphics device

par(mfrow = c(3, 1)) # Set up a 3x1 layout for plots
acf(univariate_ts, lag.max = 48, main = "BEVs")
acf(resid(fit), lag.max = 48, main = "Detrended")
acf(diff(univariate_ts), lag.max = 48, main = "First Difference")
# dev.off() # Close the graphics device


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
# dev.off() # Close the graphics device



# 3. Linear Regression model (quadratic order trend) ---------------------------

### Crating model --------------------------------------------------------------

options(scipen = 999)

fit_TS <- tslm(BEV ~ trend, lambda = 0, data = univariate_ts)
h <- 60

### Diagnostic plots

fit_TS %>%
  residuals() %>% ggtsdisplay()


### Data frame with forecasted values ------------------------------------------

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



 

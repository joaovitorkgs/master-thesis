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

ts_model_exp <- tslm(BEV ~ trend, lambda = 0, data = univariate_ts)
h <-  (5 * 12)

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





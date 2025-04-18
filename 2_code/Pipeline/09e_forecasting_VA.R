# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 2. Vector Autoregression model (quadratic order trend) -----------------------

### Crating model --------------------------------------------------------------

VARselect(multivariate_ts)

VARselect(multivariate_ts, lag.max=8, type="const")[["selection"]]

var1 <- VAR(multivariate_ts[,4:10], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")

var2 <- VAR(multivariate_ts[,4:10], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

var3 <- VAR(multivariate_ts[,4:10], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")

# Selecting best model and applying forecast function
fcast_VA <- forecast(var3, h = 60)

### List with forecasted values ------------------------------------------------

# Extracting prediction only regarding BEVs
fcast_VA_BEV <- fcast_VA$forecast$BEV 

### Diagnostic plots
diagnostics_VA <- fcast_VA_BEV %>%
  residuals() %>% ggtsdisplay()


### Plots with forecasted values -----------------------------------------------

options(scipen = 999)
fcast_VA_BEV %>% 
  autoplot() +
  ggtitle("Forecasting of BEV stock in Brazil: Vector Autoregression model") +
  ylab("BEV Stock in Brazil") + xlab("Year")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()


### Observed vs. Fitted Plot ---------------------------------------------------

time_df_VA <- data.frame(
  date = as.Date(time(multivariate_ts)),
  data = multivariate_ts[,'BEV'],
  fitted = fitted(fcast_VA_BEV)
)

plot_fcast_fitvsobs_VA <- ggplot(time_df_VA, aes(x = date)) +
  geom_line(aes(y = data, color = "Data")) +
  geom_line(aes(y = fitted, color = "Fitted")) +
  xlab("Year/Month") + 
  ylab("BEV Stock (log transformed)") +
  guides(colour = guide_legend(title = " ")) +
  theme_bw()


### Performance metrics --------------------------------------------------------

accuracy(fcast_VA_BEV)
accuracy_VA <- accuracy(fcast_VA_BEV)

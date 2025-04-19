# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 2. Vector Autoregression model (quadratic order trend) -----------------------

### Crating model --------------------------------------------------------------

VARselect(multivariate_ts[,4:14])

VARselect(multivariate_ts[,4:14], lag.max=8, type="const")[["selection"]]

fit_VA_all_1 <- VAR(multivariate_ts[,4:14], p=1, type="const")
serial.test(fit_VA_all_1, lags.pt=10, type="PT.asymptotic")

fit_VA_all_2 <- VAR(multivariate_ts[,4:14], p=2, type="const")
serial.test(fit_VA_all_2, lags.pt=10, type="PT.asymptotic")

fit_VA_all_3 <- VAR(multivariate_ts[,4:14], p=3, type="const")
serial.test(fit_VA_all_3, lags.pt=10, type="PT.asymptotic")

# Selecting best model and applying forecast function
fcast_VA_all <- forecast(fit_VA_all_3, h = 60)

# Removing income from the model
fit_VA <- VAR(multivariate_ts[,4:11], p=3, type="const")
serial.test(fit_VA, lags.pt=10, type="PT.asymptotic")

# Selecting best model and applying forecast function
fcast_VA_selected <- forecast(fit_VA, h = 60)



### List with forecasted values ------------------------------------------------

# Including income
# Extracting prediction only regarding BEVs
fcast_VA_all_BEV <- fcast_VA_all$forecast$BEV 

# Diagnostic plots
diagnostics_VA_all <- fcast_VA_all_BEV %>%
  residuals() %>% ggtsdisplay()

# Excluding income
# Extracting prediction only regarding BEVs
fcast_VA_selected_BEV <- fcast_VA_selected$forecast$BEV 

# Diagnostic plots
diagnostics_VA_selected <- fcast_VA_selected_BEV %>%
  residuals() %>% ggtsdisplay(plot.type = "histogram") 


### Plots with forecasted values -----------------------------------------------

options(scipen = 999)
plot_fcast_VA_all_BEV <- fcast_VA_all_BEV %>% 
  autoplot() +
  ggtitle("Forecasting of BEV stock in Brazil: Vector Autoregression model (all variables)") +
  ylab("BEV Stock in Brazil") + xlab("Year")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

if (!file.exists("./4_plots/plot_fcast_VA_all_BEV.png")) {
  ggsave("./4_plots/plot_fcast_VA_all_BEV.png",
         plot   = plot_fcast_VA_all_BEV,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}


plot_fcast_VA_selected_BEV <- fcast_VA_selected_BEV %>% 
  autoplot() +
  ggtitle("Forecasting of BEV stock in Brazil: Vector Autoregression model (selected variables)") +
  ylab("BEV Stock in Brazil") + xlab("Year")+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

if (!file.exists("./4_plots/plot_fcast_VA_selected_BEV.png")) {
  ggsave("./4_plots/plot_fcast_VA_selected_BEV.png",
         plot   = plot_fcast_VA_selected_BEV,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

### Observed vs. Fitted Plot ---------------------------------------------------

time_df_VA <- data.frame(
  date = as.Date(time(multivariate_ts)),
  data = multivariate_ts[,'BEV'],
  fitted = fitted(fcast_VA_selected_BEV)
)

plot_fcast_fitvsobs_VA <- ggplot(time_df_VA, aes(x = date)) +
  geom_line(aes(y = data, color = "Data")) +
  geom_line(aes(y = fitted, color = "Fitted")) +
  xlab("Year/Month") + 
  ylab("BEV Stock (log transformed)") +
  guides(colour = guide_legend(title = " ")) +
  theme_bw()


### Performance metrics --------------------------------------------------------

accuracy_all_VA <- accuracy(fcast_VA_selected_BEV)
accuracy_selected_VA <- accuracy(fcast_VA_selected_BEV)

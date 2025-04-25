# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/07_combined_data.R")

# 2. Vector Autoregression model (quadratic order trend) -----------------------

### Crating model --------------------------------------------------------------

all_variables <- multivariate_ts[, c(4:15)]

income_excluded <- multivariate_ts[,4:11]

VARselect(all_variables)

VARselect(all_variables, lag.max=8, type="const")[["selection"]]
VARselect(income_excluded, lag.max=8, type="const")[["selection"]]

fit_VA_all_1 <- VAR(all_variables, p=1, type="const")
serial.test(fit_VA_all_1, lags.pt=10, type="PT.asymptotic")

fit_VA_all_2 <- VAR(all_variables, p=2, type="const")
serial.test(fit_VA_all_2, lags.pt=10, type="PT.asymptotic")

fit_VA_all_3 <- VAR(all_variables, p=3, type="const")
serial.test(fit_VA_all_3, lags.pt=10, type="PT.asymptotic")

# Selecting best model and applying forecast function
fcast_VA_all <- forecast(fit_VA_all_3, h = 60)

# Removing income from the model
fit_VA <- VAR(income_excluded, p=2, type="const")
serial.test(fit_VA, lags.pt=10, type="PT.asymptotic")

# Selecting best model and applying forecast function
fcast_VA_selected <- forecast(fit_VA, h = 60)



### List with forecasted values ------------------------------------------------

# Including income
# Extracting prediction only regarding BEVs
fcast_VA_all_BEV <- fcast_VA_all$forecast$BEV 

# Excluding income
# Extracting prediction only regarding BEVs
fcast_VA_selected_BEV <- fcast_VA_selected$forecast$BEV 


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

### Diagnostics ----------------------------------------------------------------

# Diagnostic plots with all variables
diagnostics_VA_all <- fcast_VA_all_BEV %>%
  residuals() %>% ggtsdisplay(plot.type = "histogram")

# Diagnostic plots without income
diagnostics_VA_selected <- fcast_VA_selected_BEV %>%
  residuals() %>% ggtsdisplay(plot.type = "histogram")


serial_test_summary <- function(ts_list, lags = 1:5, lags_pt = 10, type = "const") {
  # ts_list: a named list of multivariate time series data.frames/matrices
  # lags: vector of VAR lag orders to test
  # lags_pt: lag parameter for serial.test
  # type: type argument for VAR
  
  results <- list()
  
  for (set_name in names(ts_list)) {
    ts_data <- ts_list[[set_name]]
    for (p_val in lags) {
      fit <- tryCatch(
        VAR(ts_data, p = p_val, type = type),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        stest <- tryCatch(
          serial.test(fit, lags.pt = lags_pt, type = "PT.asymptotic"),
          error = function(e) NULL
        )
        if (!is.null(stest)) {
          stat <- stest$serial$statistic
          df <- stest$serial$parameter
          pval <- stest$serial$p.value
          results[[length(results) + 1]] <- tibble(
            variable_set = set_name,
            p = p_val,
            chi_squared = stat,
            df = df,
            p_value = pval
          )
        }
      }
    }
  }
  
  bind_rows(results)
}

# Example usage:
all_variables <- multivariate_ts[, c(4:15)]
income_excluded <- multivariate_ts[, 4:11]

# Put your variable sets in a named list
ts_sets <- list(
  all_variables = all_variables,
  income_excluded = income_excluded
)

# Run the function for p = 1:5
serial_results <- serial_test_summary(ts_sets, lags = 1:5, lags_pt = 10)

options(scipen = 999)
print(serial_results)

if (!file.exists("./6_tables/VAR_model_serial_results.csv")) {
  write.csv(serial_results,
            row.names = FALSE,
                 "./6_tables/VAR_model_serial_results.csv"
           )
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
accuracy_VA <- accuracy(fcast_VA_selected_BEV)


### Correlation Matrix ---------------------------------------------------------

multivariate_ts_df <- multivariate_ts[,4:17] %>%
  as.data.frame()

plot_correlation <- multivariate_ts_df %>% 
  mutate(
    Diesel                 = log(Diesel),
    Ethanol                = log(Ethanol),
    Gasoline               = log(Gasoline),
    BEV                    = log(BEV),
    PHEV                   = log(PHEV),
    Population             = log(Population),
    avg_taxable_income_50  = log(avg_taxable_income_50),
    mean_price_electric    = log(mean_price_electric),
    mean_price_gasoline    = log(mean_price_gasoline)) %>% 
  select(Diesel, Ethanol, Gasoline, BEV, PHEV,
         Population,
         avg_taxable_income_50, 
         mean_price_electric,mean_price_gasoline) %>% 
  rename(
    `ICEV (Diesel)`    = Diesel,
    `ICEV (Ethanol)`   = Ethanol,
    `ICEV (Gasoline)`  = Gasoline,
    `Income (50th)`    = avg_taxable_income_50,
    `Mean BEV Price`   = mean_price_electric,
    `Mean BEV Gasoline`= mean_price_gasoline
  ) %>% 
  GGally::ggpairs()

ggsave(
  filename = "./4_plots/correlation_plot.png",
  plot     = plot_correlation,
  width    = 10,
  height   = 6)



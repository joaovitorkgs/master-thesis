# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/Pipeline/09a_forecasting_TS.R")
source("./2_code/Pipeline/09b_forecasting_ARIMA.R")
source("./2_code/Pipeline/09c_forecasting_ETS.R")
source("./2_code/Pipeline/09d_forecasting_prophet.R")

# 2. Combined observed vs. fitted plots ----------------------------------------

## Combining all data in one single df

time_df_TS_clean <- time_df_TS %>% 
  rename("Linear" = "fitted") 

time_df_ETS_clean <- time_df_ETS %>% 
  rename("ETS" = "y") %>% 
  select("date", "ETS")

time_df_ARIMA_clean <- time_df_ARIMA %>% 
  rename("ARIMA" = "x") %>% 
  select("date", "ARIMA")

time_df_Prophet_clean <- time_df_Prophet %>% 
  rename("Prophet" = "yhat") %>% 
  select("date", "Prophet")

time_df_all <- time_df_TS_clean %>% 
  left_join(time_df_ETS_clean, by = "date") %>% 
  left_join(time_df_ARIMA_clean, by = "date") %>% 
  left_join(time_df_Prophet_clean, by = "date") 

plot_df <- time_df_all %>%
  pivot_longer(
    cols = c(Linear, ETS, ARIMA, Prophet),
    names_to = "model",
    values_to = "fitted_value"
  )

plot_fitvsobs_all <- ggplot(plot_df, aes(x = date)) +
  # Observed data line
  geom_line(aes(y = data, color = "Observed"), linewidth = 1) +
  # Model fitted values
  geom_line(aes(y = fitted_value, color = model), linewidth = 0.8, alpha = 0.8) +
  # Styling
  scale_color_manual(
    values = c(
      "Observed" = "black",
      "Linear" = "orange3",
      "ETS" = "red3",
      "ARIMA" = "blue3",
      "Prophet" = "green4"
    )
  ) +
  labs(
    title = "Observed vs. Fitted Values Across Forecasting Models",
    x = "Date",
    y = "Value",
    color = "Series"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_x_date(
    date_breaks = "12 months",
    date_labels = "%Y",
    expand = expansion(mult = 0.02)
  ) +
  scale_y_continuous(labels = comma) 

if (!file.exists("./4_plots/plot_fitvsobs_all.png")) {
  ggsave("./4_plots/plot_fitvsobs_all.png",
         plot   = plot_fitvsobs_all,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}


## Creating a plot with 4 panels for each model

plot_fitvsobs_TS       <- plot_fcast_fitvsobs_TS      +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10), 
        legend.margin = margin(t = -10),                      
        legend.spacing.y = unit(0, "pt")) +
  scale_y_continuous(limits = c(0, 50000), labels = scales::comma)


plot_fitvsobs_ETS      <- plot_fcast_fitvsobs_ETS     +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10), 
        legend.margin = margin(t = -10),                      
        legend.spacing.y = unit(0, "pt")) +
  scale_y_continuous(limits = c(0, 50000), labels = scales::comma)

plot_fitvsobs_ARIMA    <- plot_fcast_fitvsobs_ARIMA   +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10), 
        legend.margin = margin(t = -10),                      
        legend.spacing.y = unit(0, "pt")) +
  scale_y_continuous(limits = c(0, 50000), labels = scales::comma)

plot_fitvsobs_Prophet  <- plot_fcast_fitvsobs_Prophet +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        legend.position = "bottom",        plot.margin = margin(t = 10, r = 10, b = 10, l = 10), 
        legend.margin = margin(t = -10),                      
        legend.spacing.y = unit(0, "pt")) +
  scale_y_continuous(limits = c(0, 50000), labels = scales::comma)


plot_fitvsobs_all <- grid.arrange(
  grobs = list(
    arrangeGrob(plot_fitvsobs_TS,      top = "(a) Linear"),
    arrangeGrob(plot_fitvsobs_ETS,     top = "(b) ETS"),
    arrangeGrob(plot_fitvsobs_ARIMA,   top = "(c) ARIMA"),
    arrangeGrob(plot_fitvsobs_Prophet, top = "(d) Prophet")
  ),
  ncol = 2,
  top = textGrob("BEV Stock in Brazil: Observed vs. Fitted Values Comparison Across Models", gp = gpar(fontsize = 16)),
  bottom = textGrob("Year", gp = gpar(fontsize = 10)),
  left = textGrob("BEV Stock Values", 
                  rot = 90,  # Full 90° rotation
                  gp = gpar(fontsize = 10))
)

if (!file.exists("./4_plots/plot_fitvsobs_all.png")) {
  ggsave(
    filename = "./4_plots/plot_fitvsobs_all.png",
    plot     = plot_fitvsobs_all,
    height   = 8,
    width    = 10
  )
  print("File successfully saved.")
} else {
  print("File already exists in the directory.")
}



# 2. Combined performance metrics ----------------------------------------------

accuracy_TS_df <- data.frame(accuracy_TS) %>% 
  mutate(model = "Linear")

accuracy_ETS_df <- data.frame(accuracy_ETS) %>% 
  mutate(model = "ETS")

accuracy_ARIMA_df <- data.frame(accuracy_ARIMA) %>% 
  mutate(model = "ARIMA")

accuracy_all_cf <- accuracy_TS_df %>% 
  rbind(accuracy_ETS_df,accuracy_ARIMA_df) %>% 
  select(model,ME,RMSE,MAE,MPE,MAPE,MASE,ACF1) %>% 
  select(model,RMSE,MAE,MAPE) %>% 
  rename(Model = model)




accuracy_Prophet_df <- tail(df_p,1) %>% 
  mutate(Model = "Prophet") %>% 
  rename(
    MSE   = mse,
    RMSE  = rmse,
    MAE   = mae,
    MAPE  = mape,
    MDAPE = mdape,
    SMAPE = smape,
  ) %>% 
  select(Model,RMSE,MAE,MAPE)

accuracy_all_cf <- accuracy_all_cf %>% 
  rbind(accuracy_Prophet_df)

rownames(accuracy_all_cf) <-  NULL

if (    !file.exists("./6_tables/accuracy_all_models.csv")) {
  write.table(file = "./6_tables/accuracy_all_models.csv",
              x    = accuracy_all_cf,
              sep  = ";",
              row.names = FALSE)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

?accuracy()


# 3. Combined forecasting plots ------------------------------------------------

plot_fcast_ARIMA_blank    <- plot_fcast_ARIMA   +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())

plot_fcast_ETS_blank      <- plot_fcast_ETS     +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())+
  scale_y_continuous(limits = c(0, 150000), labels = scales::comma)

plot_fcast_TS_blank       <- plot_fcast_TS      +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())+
  scale_y_continuous(limits = c(0, 750000), labels = scales::comma)

plot_fcast_Prophet_blank  <- plot_fcast_Prophet +
  theme(plot.title    = element_blank(),
        plot.subtitle = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank())


plot_fcast_all <- grid.arrange(
  grobs = list(
    arrangeGrob(plot_fcast_TS_blank,      top = "(a) Linear"),
    arrangeGrob(plot_fcast_ETS_blank,     top = "(b) ETS"),
    arrangeGrob(plot_fcast_ARIMA_blank,   top = "(c) ARIMA"),
    arrangeGrob(plot_fcast_Prophet_blank, top = "(d) Prophet")
  ),
  ncol = 2,
  top = textGrob("BEV Stock in Brazil: Forecast Comparison Across Models", gp = gpar(fontsize = 16)),
  bottom = textGrob("Year", gp = gpar(fontsize = 10)),
  left = textGrob("BEV Stock", 
                  rot = 90,  # Full 90° rotation
                  gp = gpar(fontsize = 10))
)


if (!file.exists("./4_plots/plot_fcast_all.png")) {
  ggsave(        "./4_plots/plot_fcast_all.png",
                 plot   = plot_fcast_all,
                 height = 8,
                 width  = 10)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

# 4. Combining projection information ------------------------------------------

## TS  -------------------------------------------------------------------------

# Extracting data from model
fcast_TS_data <- fcast_TS$x %>% 
  cbind(fcast_TS$lower, fcast_TS$upper, fcast_TS$mean)

# Organizing data frame
fcast_TS_df <- data.frame(
  date = as.Date(time(fcast_TS_data)),
  actual = as.numeric(fcast_TS_data[,1]),
  TS_lower_80 = as.numeric(fcast_TS_data[,2]),
  TS_lower_95 = as.numeric(fcast_TS_data[,3]),
  TS_upper_80 = as.numeric(fcast_TS_data[,4]),
  TS_upper_95 = as.numeric(fcast_TS_data[,5]),
  TS_mean     = as.numeric(fcast_TS_data[,6])
)

# Testing data visualization
ggplot(fcast_TS_df, aes(x = date, y = actual)) +
  # Observed values from 2020 to 2025
  geom_line(color = "black", size = 1) +
  
  # TS projections
  geom_line(aes(y = TS_mean), color = "red", size = 0.8, linetype = "dashed") +
  geom_ribbon(data = subset(fcast_TS_df, is.na(actual)),
              aes(ymin = TS_lower_95, ymax = TS_upper_95), fill = "red", alpha = 0.2) +
  geom_ribbon(data = subset(fcast_TS_df, is.na(actual)),
              aes(ymin = TS_lower_80, ymax = TS_upper_80), fill = "red", alpha = 0.3) +
  
  # Plot labels and theme
  labs(title = "Projected Values: TS Model",
       x = "Date",
       y = "Value") +
  theme_bw() +
  scale_x_date(date_breaks = "12 month", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ETS  -------------------------------------------------------------------------

# Extracting data from model
fcast_ETS_data <- fcast_ETS$x %>% 
  cbind(fcast_ETS$lower, fcast_ETS$upper, fcast_ETS$mean)

# Organizing data frame
fcast_ETS_df <- data.frame(
  date = as.Date(time(fcast_ETS_data)),
  actual = as.numeric(fcast_ETS_data[,1]),
  ETS_lower_80 = as.numeric(fcast_ETS_data[,2]),
  ETS_lower_95 = as.numeric(fcast_ETS_data[,3]),
  ETS_upper_80 = as.numeric(fcast_ETS_data[,4]),
  ETS_upper_95 = as.numeric(fcast_ETS_data[,5]),
  ETS_mean     = as.numeric(fcast_ETS_data[,6])
)

# Testing data visualization
ggplot(fcast_ETS_df, aes(x = date, y = actual)) +
  # Observed values from 2020 to 2025
  geom_line(color = "black", size = 1) +
  
  # TS projections
  geom_line(aes(y = ETS_mean), color = "orange3", size = 0.8, linetype = "dashed") +
  geom_ribbon(data = subset(fcast_ETS_df, is.na(actual)),
              aes(ymin = ETS_lower_95, ymax = ETS_upper_95), fill = "orange3", alpha = 0.2) +
  geom_ribbon(data = subset(fcast_ETS_df, is.na(actual)),
              aes(ymin = ETS_lower_80, ymax = ETS_upper_80), fill = "orange3", alpha = 0.3) +
  
  # Plot labels and theme
  labs(title = "Projected Values: ETS Model",
       x = "Date",
       y = "Value") +
  theme_bw() +
  scale_x_date(date_breaks = "12 month", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ARIMA -----------------------------------------------------------------------

# Extracting data from model
fcast_ARIMA_data <- fcast_ARIMA$x %>% 
  cbind(fcast_ARIMA$lower, fcast_ARIMA$upper, fcast_ARIMA$mean)

# Organizing data frame
fcast_ARIMA_df <- data.frame(
  date = as.Date(time(fcast_ARIMA_data)),
  actual = as.numeric(fcast_ARIMA_data[,1]),
  ARIMA_lower_80 = as.numeric(fcast_ARIMA_data[,2]),
  ARIMA_lower_95 = as.numeric(fcast_ARIMA_data[,3]),
  ARIMA_upper_80 = as.numeric(fcast_ARIMA_data[,4]),
  ARIMA_upper_95 = as.numeric(fcast_ARIMA_data[,5]),
  ARIMA_mean     = as.numeric(fcast_ARIMA_data[,6])
)

# Testing data visualization
ggplot(fcast_ARIMA_df, aes(x = date, y = actual)) +
  # Observed values from 2020 to 2025
  geom_line(color = "black", size = 1) +
  
  # ARIMA projections
  geom_line(aes(y = ARIMA_mean), color = "blue", size = 0.8, linetype = "dashed") +
  geom_ribbon(data = subset(fcast_ARIMA_df, is.na(actual)),
              aes(ymin = ARIMA_lower_95, ymax = ARIMA_upper_95), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = subset(fcast_ARIMA_df, is.na(actual)),
              aes(ymin = ARIMA_lower_80, ymax = ARIMA_upper_80), fill = "blue", alpha = 0.3) +
  
  # Plot labels and theme
  labs(title = "Projected Values: ARIMA Model",
       x = "Date",
       y = "Value") +
  theme_bw() +
  scale_x_date(date_breaks = "12 month", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Prophet ---------------------------------------------------------------------

fcast_Prophet_1 <- fcast_Prophet %>% 
  head(60) %>% 
  select(ds, yhat, yhat_lower, yhat_upper, trend_lower, trend_upper) %>% 
  mutate(across(c(yhat_lower, yhat_upper, trend_lower, trend_upper), ~NA)) %>% 
  rename(actual = yhat, # I have to correct this at some point; these are not the actual values
         Prophet_lower_80      = yhat_lower,
         Prophet_lower_95      = trend_lower,
         Prophet_upper_80      = yhat_upper,
         Prophet_upper_95      = trend_upper) 

fcast_Prophet_2 <- fcast_Prophet %>% 
  tail(60) %>% 
  select(ds, yhat, yhat_lower, yhat_upper, trend_lower, trend_upper) %>% 
  rename(Prophet_lower_80      = yhat_lower,
         Prophet_lower_95      = trend_lower,
         Prophet_upper_80      = yhat_upper,
         Prophet_upper_95      = trend_upper,
         Prophet_mean          = yhat)

fcast_Prophet_data <- full_join(fcast_Prophet_1,fcast_Prophet_2) 

# Organizing data frame
fcast_Prophet_df <- data.frame(
  date = as.Date(fcast_Prophet_data$ds),
  actual = as.numeric(fcast_ARIMA_data[,1]),
  Prophet_lower_80 = as.numeric(fcast_Prophet_data$Prophet_lower_80),
  Prophet_lower_95 = as.numeric(fcast_Prophet_data$Prophet_lower_95),
  Prophet_upper_80 = as.numeric(fcast_Prophet_data$Prophet_upper_80),
  Prophet_upper_95 = as.numeric(fcast_Prophet_data$Prophet_upper_95),
  Prophet_mean     = as.numeric(fcast_Prophet_data$Prophet_mean))
  

# Testing data visualization
ggplot(fcast_Prophet_df, aes(x = date, y = actual)) +
  # Observed values from 2020 to 2025
  geom_line(color = "black", size = 1) +
  
  # ARIMA projections
  geom_line(aes(y = Prophet_mean), color = "brown4", size = 0.8, linetype = "dashed") +
  geom_ribbon(data = subset(fcast_Prophet_df, is.na(actual)),
              aes(ymin = Prophet_lower_95, ymax = Prophet_upper_95), fill = "brown4", alpha = 0.2) +
  geom_ribbon(data = subset(fcast_Prophet_df, is.na(actual)),
              aes(ymin = Prophet_lower_80, ymax = Prophet_upper_80), fill = "brown4", alpha = 0.3) +
  # Plot labels and theme
  labs(title = "Projected Values: Prophet Model",
       x = "Date",
       y = "Value") +
  theme_bw() +
  scale_x_date(date_breaks = "12 month", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



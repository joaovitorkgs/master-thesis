# 1. Packages ------------------------------------------------------------------

setwd("C:/Users/joaov/Dropbox/R Assignments/master-thesis")
source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

fleet_2013_2024_id_clean <- read_csv("3_processed_data/fleet_2013_2024_id_clean.csv")

# 3. Cleaning data --- ---------------------------------------------------------

## Adding the variation on registered BEVs on a monthly basis to be able to use an ARIMA model

fleet_df <- fleet_2013_2024_id_clean %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(year, date, sigla_uf, id_municipio,id_municipio_nome) %>%
  summarize(
    electric_vehicles = sum(electric),
    total_population = sum(populacao),
  )

fleet_df <- fleet_df %>%
  arrange(id_municipio, date) %>%
  group_by(id_municipio) %>%
  mutate(lag_electric = dplyr::lag(electric_vehicles)) %>%
  ungroup() %>% 
  mutate(new_electric = electric_vehicles - lag_electric) %>% 
  select(-lag_electric)

fleet_df$new_electric[is.na(fleet_df$new_electric)] <- 0

fleet_df_uf <- fleet_df %>% 
  group_by(year, date, sigla_uf) %>% 
  summarize(
    EVs_total = sum(electric_vehicles),
    EVs_delta = sum(new_electric))

fleet_df_BR <- fleet_df %>% 
  group_by(year, date) %>% 
  summarize(
    EVs_total = sum(electric_vehicles),
    EVs_delta = sum(new_electric))


# 4. Visualizing the data ------------------------------------------------------


fleet_df_uf %>% 
  filter(year > 2018, sigla_uf != "SP") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = EVs_delta, color = "New EVs (Delta)"), size = 1) +
  labs(
    title = "Electric Vehicles Over Time",
    x = "Date",
    y = "Number of Electric Vehicles",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Total EVs" = "blue", "New EVs (Delta)" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  ) + facet_wrap(~ sigla_uf, nrow = 5)

fleet_df_BR %>% 
  filter(year > 2018) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = EVs_delta, color = "New EVs (Delta)"), size = 1) +
  labs(
    title = "Electric Vehicles Over Time",
    x = "Date",
    y = "Number of New Electric Vehicles",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Total EVs" = "blue", "New EVs (Delta)" = "orange")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

# 5. Attempting ARIMA projections ----------------------------------------------

## 5.1. Using delta in monthly EV fleet to run projections ---------------------

options(scipen = 999)
Box.test(diff(fleet_df_BR$EVs_delta), lag=10, type="Ljung-Box")

fleet_df_BR$EVs_delta %>% 
  ur.kpss %>% 
  summary()

# The test statistic obtained is higher that the 1% critical value, indicating that 
# the data is non-stationary.

fleet_df_BR_ARIMA <- fleet_df_BR 

fleet_df_BR_ARIMA$EVs_delta %>% 
  diff() %>% 
  ur.kpss %>% 
  summary()

# When differencing the data, we obtain a tiny test statistic value, indicating
# stationary data

fit <- auto.arima(fleet_df_BR_ARIMA$EVs_delta, seasonal = FALSE)

fit %>% 
  forecast(h=120) %>% 
  autoplot(include = 80)


## 5.2. Using total values to run projections ----------------------------------

fleet_df_BR_ARIMA$EVs_total %>% 
  diff() %>% 
  ur.kpss %>% 
  summary()

fit_arima <- auto.arima(fleet_df_BR_ARIMA$EVs_total, seasonal = FALSE)

plot_arima_total <- fit_arima %>% 
  forecast(h=120) %>% 
  autoplot(include = 80)

projected_arima <- fit_arima %>% 
  forecast(h=120)

tail(projected_arima$upper, 1)

tail(projected_arima$mean, 1)

tail(projected_arima$lower, 1)

## 5.3. Comparing ARIMA and ETS methods ----------------------------------------

fit_ets_delta <- ets(fleet_df_BR_ARIMA$EVs_delta)

plot_ets_delta <- fit_ets_delta %>% 
  forecast(h=120) %>% 
  autoplot()




fit_ets_total <- ets(fleet_df_BR_ARIMA$EVs_total)

plot_ets_total <- fit_ets_total %>% 
  forecast(h=120) %>% 
  autoplot()

gridExtra::grid.arrange(plot_arima_total, plot_ets_total, ncol =2)




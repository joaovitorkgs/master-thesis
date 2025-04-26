# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Data from project pipeline ---------------------------------------------

fleet_2013_2024_clean <- read_csv("./3_processed_data/fleet_2013_2024_id_clean.csv")
price_df              <- read_csv("3_processed_data/fipe_price_monthly_trends_deflated.csv")
income_data_wide_uf   <- read_csv("3_processed_data/income_data_wide_uf.csv")
pop_city              <- read_csv("1_raw_data/0_demographics/pop_city.csv")



# 3. Models --------------------------------------------------------------------

## Preparing data --------------------------------------------------------------

### a) Fleet size and population -----------------------------------------------

months <- fleet_2013_2024_clean %>% 
  arrange(date) %>%  
  distinct(date) %>%  
  mutate(months_nr = row_number()) 

fleet_df <- fleet_2013_2024_clean %>% 
  mutate(ev_pc        = electric / populacao,
         ff_pc        = (gasoline+diesel)/populacao,
         log_electric = log(electric),
         log_ff       = log(gasoline+diesel),
         log_pop      = log(populacao)) %>% 
  filter(log_electric >= 0) %>% 
  left_join(months, by="date")

### b) Price information -------------------------------------------------------

fleet_df_b_city <- fleet_df %>% 
  left_join(price_df, by = "date") 

fleet_df_b_uf <- fleet_df %>% 
  left_join(price_df, by = "date") %>% 
  group_by(sigla_uf, date, months_nr) %>% 
  summarize(diesel     = sum(diesel),
            ethanol    = sum(ethanol),
            gasoline   = sum(gasoline),
            other      = sum(other),
            electric   = sum(electric),
            gas        = sum(gas),
            population = sum(populacao))

fleet_df_b_uf <- fleet_df %>% 
  left_join(price_df, by = "date") %>% 
  group_by(sigla_uf, date, year, month, months_nr) %>% 
  summarize(diesel     = sum(diesel),
            ethanol    = sum(ethanol),
            gasoline   = sum(gasoline),
            other      = sum(other),
            electric   = sum(electric),
            gas        = sum(gas),
            population = sum(populacao),
            mean_gas   = mean(mean_price_gasoline),
            mean_hyb   = mean(mean_price_hybrid),
            mean_ev    = mean(mean_price_electric),
            median_gas = mean(median_price_gasoline),
            median_hyb = mean(median_price_hybrid),
            median_ev  = mean(median_price_electric),
            min_gas    = mean(min_price_gasoline),
            min_hyb    = mean(min_price_hybrid),
            min_ev     = mean(min_price_electric)) %>% 
  mutate(ff_pc        = (gasoline+diesel)/population,
         log_ff       = log(gasoline+diesel),
         log_pop      = log(population))


### c) Income information ------------------------------------------------------

fleet_df_c <- fleet_df_b_uf %>% 
  left_join(income_data_wide_uf, by = c("year", "sigla_uf")) %>% 
  drop_na()






# 4. Projections --------------------------------------------------------------------

# Create a function to project values
project_values <- function(initial_value, growth_rate, years) {
  initial_value * (1 + growth_rate)^years
}

# Set up projection parameters
start_date <- as.Date("2022-08-01")
end_date   <- as.Date("2032-12-31")
months     <- seq(start_date, end_date, by = "month")
n_months   <- length(months)

# Project population (assuming 1% annual growth)
last_population  <- pop_city %>% 
  summarize(population = sum(populacao)) %>% 
  getElement("population")

last_min_ev  <- price_df %>% 
  filter(date == max(date)) %>% 
  getElement("min_price_hybrid")
 
last_income_50  <- income_data_wide_uf %>% 
  filter(year == 2022) %>% 
  summarize(initial_income_50 = mean(avg_taxable_income_50)) %>% 
  getElement("initial_income_50")

last_income_100  <- income_data_wide_uf %>% 
  filter(year == 2022) %>% 
  summarize(initial_income_100 = mean(avg_taxable_income_100)) %>% 
  getElement("initial_income_100")  # Placeholder value, replace with actual data



coefficients <- list(
  log_population             = 1.753,
  log_min_ev                 = -4.023,
  log_avg_taxable_income_50  = -0.504,
  log_avg_taxable_income_100 = -2.226,
  intercept                  = -21.980 
)

future_months <- seq.Date(from = as.Date("2023-01-01"), to = as.Date("2032-12-01"), by = "month")
future_data <- data.frame(
  date = future_months,
  year = as.numeric(format(future_months, "%Y")),
  month = as.numeric(format(future_months, "%m")),
  months_nr = seq(max(fleet_df_c$months_nr) + 1, by = 1, length.out = length(future_months))
)

growth_rate_population <- 0.01 
growth_rate_min_ev     <- -0.02    
growth_rate_income     <- 0.03


future_data <- future_data %>%
  mutate(
    population = last_population * (1 + growth_rate_population) ^ (year - max(fleet_df_c$year)),
    min_ev = last_min_ev * (1 + growth_rate_min_ev) ^ (year - max(fleet_df_c$year)),
    avg_taxable_income_50 = last_income_50 * (1 + growth_rate_income) ^ (year - max(fleet_df_c$year)),
    avg_taxable_income_100 = last_income_100 * (1 + growth_rate_income) ^ (year - max(fleet_df_c$year))
  )

future_data <- future_data %>%
  mutate(
    log_population = log(population),
    log_min_ev = log(min_ev),
    log_avg_taxable_income_50 = log(avg_taxable_income_50),
    log_avg_taxable_income_100 = log(avg_taxable_income_100)
  )


future_data <- future_data %>%
  mutate(
    log_electric = coefficients$log_population * log_population +
      coefficients$log_min_ev * log_min_ev +
      coefficients$log_avg_taxable_income_50 * log_avg_taxable_income_50 +
      coefficients$log_avg_taxable_income_100 * log_avg_taxable_income_100,
    electric = exp(log_electric) # Back-transform to get projected EV counts
  )

options(sciplot = 999)

# Plot projections
ggplot(future_data, aes(x = date, y = log_electric)) +
  geom_line(color = "blue") +
  labs(title = "Projected Number of Electric Vehicles in Brazil",
       x = "Date", y = "Number of Electric Vehicles") +
  theme_minimal()

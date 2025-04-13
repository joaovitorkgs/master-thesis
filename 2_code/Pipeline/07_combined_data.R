# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

# Fleet stock per vehicle type and population
df_fleet_city         <- read_csv("./3_processed_data/df_fleet_city.csv")
df_fleet_state        <- read_csv("./3_processed_data/df_fleet_state.csv")
df_fleet_brazil       <- read_csv("./3_processed_data/df_fleet_brazil.csv")

# Vehicle price data
price_df              <- read_csv("./3_processed_data/fipe_price_monthly_trends_deflated.csv")

# Income distribution per state 
income_data_wide_uf   <- read_csv("./3_processed_data/income_data_wide_uf.csv")

# 3. Data transformation for the models ----------------------------------------

## Data for the linear regressions ---------------------------------------------

## Adding price information and grouping by state
df_combined_state <- df_fleet_state %>% 
  left_join(price_df, by = "date") %>% 
  group_by(sigla_uf, date, year, month) %>% 
  summarize(diesel     = sum(Diesel),
            ethanol    = sum(Ethanol),
            gasoline   = sum(Gasoline),
            other      = sum(Other),
            electric   = sum(BEV),
            PHEV       = sum(PHEV),
            gas        = sum(gas),
            population = sum(population),
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

## Adding income distribution data
df_combined_state <- df_combined_state %>% 
  left_join(income_data_wide_uf, by = c("year", "sigla_uf")) %>% 
  drop_na()

if (!file.exists(  "./3_processed_data/df_combined_state.csv")) {
  write_csv(df_combined_state,
            file = "./3_processed_data/df_combined_state.csv")
} else {
  print("File already exists in the repository")
}

## Data for the univariate time series -----------------------------------------

start_year <- as.numeric(format(min(df_fleet_brazil$date), "%Y"))
start_month <- as.numeric(format(min(df_fleet_brazil$date), "%m"))


df <-  df_fleet_state%>% 
  group_by(date) %>% 
  summarize(BEV=sum(BEV))

df_combined_state %>% 
  group_by(date) %>% 
  summarize(BEV=sum(electric))

fleet_df_ts <- df_combined_state %>% 
  select(sigla_uf, date, year, month, 
         diesel, ethanol, gasoline, other, electric, PHEV,
         population,
         mean_gas, mean_hyb, min_ev,
         avg_taxable_income_50, avg_taxable_income_100) %>% 
  group_by(date) %>% 
  summarize(
    electric                   = sum(electric),
    PHEV                       = sum(PHEV),
    population                 = sum(population),
    min_ev                     = mean(min_ev),
    mean_gas                   = mean(mean_gas),
    mean_hyb                   = mean(mean_hyb),
    avg_taxable_income_50      = mean(avg_taxable_income_50),
    avg_taxable_income_100     = mean(avg_taxable_income_100),
    log_electric               = log(electric),
    log_population             = log(population),
    log_min_ev                 = log(min_ev),
    log_mean_gas               = log(mean_gas),
    log_mean_hyb               = log(mean_hyb),
    log_avg_taxable_income_50  = log(avg_taxable_income_50),
    log_avg_taxable_income_100 = log(avg_taxable_income_100)
  ) %>%  
  mutate(
    electric   = if_else(date == as.Date("2018-06-01"), electric   / 2, electric),
    population = if_else(date == as.Date("2018-06-01"), population / 2, population)
  )


multivariate_ts <- df_fleet_brazil %>% 
  arrange(date) %>% 
  ts(start = c(start_year, start_month), frequency = 12)

univariate_ts <- df_fleet_brazil %>% 
  arrange(date) %>% 
  select(BEV) %>% 
  ts(start = c(start_year, start_month), frequency = 12)


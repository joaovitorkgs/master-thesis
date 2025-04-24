# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

# Fleet stock per vehicle type and population
df_fleet_city         <- read_csv("./3_processed_data/df_fleet_city.csv")
df_fleet_state        <- read_csv("./3_processed_data/df_fleet_state.csv")
df_fleet_brazil       <- read_csv("./3_processed_data/df_fleet_brazil.csv")

# Vehicle price data
price_df              <- read_csv("./3_processed_data/fipe_price_monthly_trends_deflated.csv")
price_df_projected    <- read_csv("./3_processed_data/fipe_price_projected_2022_2024.csv")

# Income data
income_data_wide_uf   <- read_csv("./3_processed_data/income_data_wide_uf.csv") # State-level
income_data_wide_br   <- read_csv("./3_processed_data/income_br_monthly_nominal.csv") # BR-level

# Population data
BR_pop_monthly        <- read_csv("./3_processed_data/BR_pop_monthly.csv")


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

## Adding population data to df_fleet_brazil 

BR_pop_monthly_20_24 <- BR_pop_monthly %>% 
  tail(60) 


df_fleet_brazil_pop  <- df_fleet_brazil %>% 
  filter(year > 2019) %>% 
  left_join(BR_pop_monthly_20_24, by = "date")

df_fleet_brazil_pop_income  <- df_fleet_brazil_pop %>% 
  left_join(income_data_wide_br, by = "date")

## Adding prices data frame

df_fleet_brazil_pop_income_prices <- df_fleet_brazil_pop_income %>% 
  left_join(price_df_projected, by = "date")




## Data for the univariate time series -----------------------------------------

start_year  <- 2020
start_month <- 01


univariate_ts <- df_fleet_brazil %>% 
  filter(year > 2019) %>% 
  arrange(date) %>% 
  dplyr::select(BEV) %>% 
  ts(start = c(start_year, start_month), frequency = 12)


## Data for the multivariate time series -----------------------------------------

multivariate_ts <- df_fleet_brazil_pop_income_prices %>% 
  arrange(date) %>% 
  ts(start = c(start_year, start_month), frequency = 12)


# 1. Packages ------------------------------------------------------------------

setwd("C:/Users/joaov/Dropbox/R Assignments/master-thesis")
source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

imported_data <- read_csv("./3_processed_data/fleet_2013_2024_id.csv")

# 3. Cleaning data --- ---------------------------------------------------------

## City-level fleet per powertrain and fueltype --------------------------------

fuel_types <- tibble(unique(imported_data$fuel))

# For older entries on the public dataset, BEV vehicles are referred to as 
# "ELETRICO/FONTE INTERNA", which refers to BEV only vehicles. (See "CAMPO 36" 
# on this regulation: https://tinyurl.com/mr3nrdhy)


df_fleet_city <- imported_data %>%
  mutate(
    fuel = case_when(
      # Prioritize hybrid categories first
      str_detect(fuel, regex("HIBRIDO PLUG-IN|^HIBRIDO$|ELETRICO", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("^ELETRICO$|ELETRICO/FONTE INTERNA", ignore_case = TRUE)) ~ "PHEV",
      
      # Electric vehicles (pure electric only)
      str_detect(fuel, regex("^ELETRICO$|ELETRICO/FONTE INTERNA", ignore_case = TRUE)) ~ "BEV",
      
      # Ethanol vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("ALCOOL|ETANOL", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Ethanol",
      
      # Gasoline vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("GASOLINA", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Gasoline",
      
      # Diesel vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("DIESEL", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Diesel",
      
      # Gas vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("GAS|HIBRIDO/GAS NATURAL VEICULAR", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Gas",
      
      # Catch-all for uncategorized entries
      TRUE ~ "Other"
    )
  ) %>%
  group_by(sigla_uf, id_municipio_nome, month, year, date, populacao, fuel) %>%  # Add fuel here
  summarise(
    total      = sum(total, na.rm = TRUE),
    .groups = "drop") %>%
  pivot_wider(
    names_from = fuel,
    values_from = total,
    values_fill = list(total = 0)
  ) %>% 
  mutate(across(
    Diesel:PHEV, 
    ~ case_when(
      date == "2014-01-01" ~ round(.x / 2, digits = 0),
      date == "2024-02-01" ~ round(.x / 10, digits = 0),
      TRUE ~ .x
    )
  ))


if (!file.exists(  "./3_processed_data/df_fleet_city.csv")) {
  write_csv(df_fleet_city,
            file = "./3_processed_data/df_fleet_city.csv")
} else {
  print("File already exists in the repository")
}

## State-level fleet per powertrain and fueltype -------------------------------

df_fleet_state <- imported_data %>%
  mutate(
    fuel = case_when(
      # Prioritize hybrid categories first
      str_detect(fuel, regex("HIBRIDO PLUG-IN|^HIBRIDO$|ELETRICO", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("^ELETRICO$|ELETRICO/FONTE INTERNA", ignore_case = TRUE)) ~ "PHEV",
      
      # Electric vehicles (pure electric only)
      str_detect(fuel, regex("^ELETRICO$|ELETRICO/FONTE INTERNA", ignore_case = TRUE)) ~ "BEV",
      
      # Ethanol vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("ALCOOL|ETANOL", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Ethanol",
      
      # Gasoline vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("GASOLINA", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Gasoline",
      
      # Diesel vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("DIESEL", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Diesel",
      
      # Gas vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("GAS|HIBRIDO/GAS NATURAL VEICULAR", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Gas",
      
      # Catch-all for uncategorized entries
      TRUE ~ "Other"
    )
  ) %>%
  group_by(sigla_uf, month, year, date, fuel) %>%
  summarise(total = sum(total), .groups = "drop") %>% 
  pivot_wider(
    names_from = fuel,
    values_from = total,
    values_fill = 0
  ) %>% 
  # Remove redundant summarize here
  mutate(across(Diesel:PHEV, ~ case_when(
    date == "2014-01-01" ~ round(.x / 2), 
    date == "2024-02-01" ~ round(.x / 10),
    TRUE ~ .x
  ))) %>%
  filter(complete.cases(sigla_uf))

# testing total number of BEVs
BEV_total_uf <- df_fleet_state %>% 
  group_by(date) %>% 
  summarize(BEV=sum(BEV))

# Adding data for population size
state_population <- imported_data %>% 
  group_by(sigla_uf, date) %>% 
  summarize(population = sum(populacao)) %>% 
  mutate(population = population/10) %>% 
  mutate(across(
    population, 
    ~ case_when(
      date == "2014-01-01" ~ round(.x / 2, digits = 0),
      date == "2024-02-01" ~ round(.x / 10, digits = 0),
      date == "2018-06-01" ~ round(.x / 2, digits = 0),
      TRUE ~ .x
    )))

df_fleet_state <- df_fleet_state %>% 
  left_join(
    state_population, 
    by = c("sigla_uf", "date"))

if (!file.exists(  "./3_processed_data/df_fleet_state.csv")) {
  write_csv(df_fleet_state,
            file = "./3_processed_data/df_fleet_state.csv")
} else {
  print("File already exists in the repository")
}

## Country-level fleet per powertrain and fueltype -----------------------------

df_fleet_brazil <- imported_data %>%
  mutate(
    fuel = case_when(
      # Prioritize hybrid categories first
      str_detect(fuel, regex("HIBRIDO PLUG-IN|^HIBRIDO$|ELETRICO", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("^ELETRICO$|ELETRICO/FONTE INTERNA", ignore_case = TRUE)) ~ "PHEV",
      
      # Electric vehicles (pure electric only)
      str_detect(fuel, regex("^ELETRICO$|ELETRICO/FONTE INTERNA", ignore_case = TRUE)) ~ "BEV",
      
      # Ethanol vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("ALCOOL|ETANOL", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Ethanol",
      
      # Gasoline vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("GASOLINA", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Gasoline",
      
      # Diesel vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("DIESEL", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Diesel",
      
      # Gas vehicles (excluding those that also have electric components)
      str_detect(fuel, regex("GAS|HIBRIDO/GAS NATURAL VEICULAR", ignore_case = TRUE)) & 
        !str_detect(fuel, regex("ELETRICO", ignore_case = TRUE)) ~ "Gas",
      
      # Catch-all for uncategorized entries
      TRUE ~ "Other"
    )
  ) %>%
  group_by(month, year, date, populacao, fuel) %>%
  summarise(
    total   = sum(total),
    .groups = "drop") %>%
  pivot_wider(
    names_from = fuel,
    values_from = total,
    values_fill = list(total = 0)
  ) %>% 
  group_by(month, year, date) %>%  
  summarize(
    Diesel    = sum(Diesel),
    Ethanol   = sum(Ethanol),
    Gasoline  = sum(Gasoline),
    Gas       = sum(Gas),
    Other     = sum(Other),
    BEV       = sum(BEV),
    PHEV      = sum(PHEV),
    .groups = "drop") %>% 
  mutate(across(
    Diesel:PHEV, 
    ~ case_when(
      date == "2014-01-01" ~ round(.x / 2, digits = 0),
      date == "2024-02-01" ~ round(.x / 10, digits = 0),
      TRUE ~ .x
    )
  )) %>% 
  drop_na()

BEV_total_br <- df_fleet_brazil %>% 
  group_by(date) %>% 
  summarize(BEV=sum(BEV))


if (!file.exists(  "./3_processed_data/df_fleet_brazil.csv")) {
  write_csv(df_fleet_brazil,
            file = "./3_processed_data/df_fleet_brazil.csv")
} else {
  print("File already exists in the repository")
}
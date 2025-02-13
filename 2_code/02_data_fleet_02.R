# 1. Packages ------------------------------------------------------------------

setwd("C:/Users/joaov/Dropbox/R Assignments/master-thesis")
source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

fleet_2013_2024 <- read_csv("1_raw_data/2_vehicle_fleet/frota_2013_2024.csv")

# 3. Cleaning data --- ---------------------------------------------------------

fleet_2013_2024_clean <- fleet_2013_2024 %>%
  mutate(
    fuel_category = case_when(
      str_detect(fuel, regex("ELETRICO|HIBRIDO", ignore_case = TRUE)) ~ "electric",
      str_detect(fuel, regex("ALCOOL|ETANOL", ignore_case = TRUE)) ~ "ethanol",
      str_detect(fuel, regex("GASOLINA", ignore_case = TRUE)) ~ "gasoline",
      str_detect(fuel, regex("DIESEL", ignore_case = TRUE)) ~ "diesel",
      str_detect(fuel, regex("GAS", ignore_case = TRUE)) ~ "gas",
      TRUE ~ "other"
    )
  ) %>%
  group_by(UF, city, month, year, date, fuel_category) %>%
  summarise(total = sum(total, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = fuel_category,
    values_from = total,
    values_fill = 0
  )


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2013_2024_clean.csv")) {
  write_csv(fleet_2013_2024_clean,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2013_2024_clean.csv")
} else {
  print("File already exists in the repository")
}




# 4. Data Exploration ----------------------------------------------------------




# 1. Packages ------------------------------------------------------------------

setwd("C:/Users/joaov/Dropbox/R Assignments/master-thesis")
source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

imported_data <- read_csv("3_processed_data/fleet_2013_2024_id_clean.csv")

# 3. Cleaning data --- ---------------------------------------------------------

fleet_2013_2024_clean <- imported_data %>%
  mutate(
    fuel_category = case_when(
      # Ensure "ELETRICO" matches only pure electric vehicles
      str_detect(fuel, regex("^ELETRICO$|ELETRICO/FONTE EXTERNA", ignore_case = TRUE)) ~ "electric",
      str_detect(fuel, regex("ALCOOL|ETANOL", ignore_case = TRUE)) ~ "ethanol",
      str_detect(fuel, regex("GASOLINA", ignore_case = TRUE)) ~ "gasoline",
      str_detect(fuel, regex("DIESEL", ignore_case = TRUE)) ~ "diesel",
      str_detect(fuel, regex("GAS", ignore_case = TRUE)) ~ "gas",
      TRUE ~ "other" # Catch-all for uncategorized entries
    )
  ) %>%
  group_by(month, year, date, fuel_category) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>% # Use `.groups = "drop"` for clarity
  pivot_wider(
    names_from = fuel_category,
    values_from = total,
    values_fill = list(total = 0) # Explicitly set `values_fill` to avoid ambiguity
  )

unique(fleet_2013_2024$fuel)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2013_2024_clean.csv")) {
  write_csv(fleet_2013_2024_clean,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2013_2024_clean.csv")
} else {
  print("File already exists in the repository")
}



# 4. Data Exploration ----------------------------------------------------------

ggplot(fleet_2013_2024_clean, aes(x = as.integer(year), y = electric)) +
  geom_line(color = "#69b3a2", linewidth = 2) +
  theme_bw()+
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles",
    title = "Evolution of Electric Vehicles in Brazil",
    subtitle = "Source: National Traffic Secretariat, 2024")


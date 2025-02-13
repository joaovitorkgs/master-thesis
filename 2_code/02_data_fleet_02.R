# 1. Packages ------------------------------------------------------------------

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



# 4. Data Exploration ----------------------------------------------------------


plot1 <- ggplot(fleet_2013_2024, aes(x = fuel, y = total)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  labs(
    title = "Total Vehicles by Fuel Type",
    x = "Fuel Type",
    y = "Total Vehicles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot2 <- ggplot(fleet_2013_2024_clean, aes(x = year, y = electric)) +
  geom_point(alpha = 0.7, color = "orange") +
  labs(
    title = "Electric Vehicles Over Time",
    x = "Year",
    y = "Number of Electric Vehicles"
  ) +
  theme_minimal()



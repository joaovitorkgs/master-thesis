# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Raw dataframes ------------------------------------------------------------

# 2.1. Dowloaded files ---------------------------------------------------------

fleet_df_nov13 <- read_excel("1_raw_data/2_vehicle_fleet/2013_11_3_frota_uf_municipio_combustivel_nov_2013.xlsx")
fleet_df_nov14 <- read_excel("1_raw_data/2_vehicle_fleet/2014-11_3_frota_uf_municipio_combustivel_nov_2014.xlsx")
fleet_df_nov15 <- read_excel("1_raw_data/2_vehicle_fleet/2015-11_3_Frota_Por_UF_Municipio_Combustivel_NOV_2015.xlsx")
fleet_df_nov16 <- read_excel("1_raw_data/2_vehicle_fleet/2016-11_3_-_combustivel_-novembro_-2016.xlsx")
fleet_df_nov17 <- read_excel("1_raw_data/2_vehicle_fleet/2017-11_frota_por_uf_municipio_combustivel_nov_17.xlsx")
fleet_df_nov18 <- read_excel("1_raw_data/2_vehicle_fleet/2018-11_d_frota_por_uf_municipio_combustivel_novembro_2018.xlsx")
fleet_df_nov19 <- read_excel("1_raw_data/2_vehicle_fleet/2019-11_d_frota_por_uf_municipio_combustivel_novembro_2019.xlsx")
fleet_df_nov20 <- read_excel("1_raw_data/2_vehicle_fleet/2020-11_d_frota_por_uf_municipio_combustivel_novembro_2020.xlsx")
fleet_df_nov21 <- read_excel("1_raw_data/2_vehicle_fleet/2021-11_d_frota_por_uf_municipio_combustivel_novembro_2021.xlsx")
fleet_df_nov22 <- read_excel("1_raw_data/2_vehicle_fleet/2022-11_d_frota_por_uf_municipio_combustivel_novembro_2022.xlsx")
fleet_df_nov23 <- read_excel("1_raw_data/2_vehicle_fleet/2023-11_d_frota_por_uf_municipio_combustivel_novembro_2023.xlsx")
fleet_df_nov24 <- read_excel("1_raw_data/2_vehicle_fleet/2024-11_D_Frota_por_UF_Municipio_COMBUSTIVEL_Novembro_2024.xlsx")

# 2.2. Files from packages -----------------------------------------------------

# State data with geographical coordinates

state_df <- geobr::read_state(year = 2020,showProgress = FALSE) 



# 2.3. Files from BigQuery ----------------------------------------------------

source("./2_code/01_descriptive-demographics.R")


# 3. Manipulating data to generate plots and tables ----------------------------

# 3.1 Vehicle fleet (total/electric) -------------------------------------------

create_electric_vehicles_dfs <- function(fleet_dfs) {
  electric_vehicles_dfs <- list()
  
  for (year in 2013:2024) {
    fleet_df <- fleet_dfs[[paste0("fleet_df_nov", substr(as.character(year), 3, 4))]]
    
    # Aggregate municipal data in state level
    fleet_state <- fleet_df %>% 
      group_by(UF, `Combustível Veículo`) %>%
      summarize(Total = sum(`Qtd. Veículos`), .groups = "drop")
    
    # Data frame of total vehicles per state
    vehicles_per_state <- fleet_state %>%
      group_by(UF) %>%
      summarise(total_vehicles = sum(Total))
    
    # Filter electric vehicles and creating a new variable for % of electric or hybrid vehicles
    electric_vehicles_per_state <- fleet_state %>%
      filter(grepl("^ELETRICO", `Combustível Veículo`)) %>%
      group_by(UF) %>%
      summarise(total_electric_vehicles = sum(Total)) %>% 
      left_join(vehicles_per_state, by = "UF") %>% 
      mutate(percentage = scales::percent(total_electric_vehicles/total_vehicles, accuracy = 0.01)) %>% 
      filter(UF != "Sem Informação") %>% 
      mutate(year = year)
    
    electric_vehicles_dfs[[paste0("electric_vehicles_per_state_", year)]] <- electric_vehicles_per_state
  }
  
  return(electric_vehicles_dfs)
}

fleet_dfs <- list(
  fleet_df_nov13 = fleet_df_nov13,
  fleet_df_nov14 = fleet_df_nov14,
  fleet_df_nov15 = fleet_df_nov15,
  fleet_df_nov16 = fleet_df_nov16,
  fleet_df_nov17 = fleet_df_nov17,
  fleet_df_nov18 = fleet_df_nov18,
  fleet_df_nov19 = fleet_df_nov19,
  fleet_df_nov20 = fleet_df_nov20,
  fleet_df_nov21 = fleet_df_nov21,
  fleet_df_nov22 = fleet_df_nov22,
  fleet_df_nov23 = fleet_df_nov23,
  fleet_df_nov24 = fleet_df_nov24
  )

# Saving dataframes in a single list
all_electric_vehicles_dfs <- create_electric_vehicles_dfs(fleet_dfs)


fleet_electric_vehicles_all_years <- bind_rows(all_electric_vehicles_dfs)





# 3.1 Vehicle fleet (total/electric) -------------------------------------------


# Create a string to use as common key to join the data frames
state_df$UF <- state_df$name_state %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  toupper()

# Join electric vehicle df with geographic info on the states
fleet_electric_vehicles_all_years <- fleet_electric_vehicles_all_years %>% 
  left_join(state_df, by = "UF") %>% 
  select(code_state, abbrev_state, name_state, code_region, name_region, geom, total_electric_vehicles, total_vehicles, percentage, year) 

# Update variable name
fleet_electric_vehicles_all_years$name_state <- tolower(fleet_electric_vehicles_all_years$name_state)


if (!file.exists("./3_processed_data/fleet_electric_vehicles_all_years.csv")) {
  write_csv(fleet_electric_vehicles_all_years,
            file = "./3_processed_data/fleet_electric_vehicles_all_years.csv")
} else {
  print("File already exists in the repository")
}


# Adding State population to the data frame
state_pop <- pop_city %>% 
  group_by(sigla_uf) %>% 
  summarize(total_pop = sum(populacao)) %>% 
  rename(abbrev_state = sigla_uf)

electric_vehicles_per_state <- fleet_electric_vehicles_all_years %>% 
  filter(year == 2022) %>% 
  left_join(state_pop, by = "abbrev_state")

electric_vehicles_per_state <- electric_vehicles_per_state %>% 
  mutate(vehicle_per_capita = total_vehicles/total_pop,
         electric_vehicle_per_capita = total_electric_vehicles/total_pop)


if (!file.exists("./3_processed_data/fleet_electric_vehicles_per_state_23.csv")) {
  write_csv(electric_vehicles_per_state,
            file = "./3_processed_data/fleet_electric_vehicles_per_state_23.csv")
} else {
  print("File already exists in the repository")
}




# 4. Plots ---------------------------------------------------------------------

## 4.1. Maps -------------------------------------------------------------------

# Convert the df to generate the map
electric_vehicles_per_state <- st_as_sf(electric_vehicles_per_state)

# Update the df to make the percentage column numeric
electric_vehicles_per_state$percentage_num <- as.numeric(sub("%", "", electric_vehicles_per_state$percentage))

# Create a variable for the theme in the plots
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Percentage of electric vehicles per State

# Total e-Vehicles
map_total_e_vehicles <- ggplot() +
  geom_sf(data=electric_vehicles_per_state, aes(fill=total_electric_vehicles), color=NA, size=.15) +
  labs(
    title = "Electric Vehicle Fleet in Brazil",
    subtitle = "Sum of Electrical Vehicles per State",
    fill = "Total Vehicles") +
  geom_sf_text(data=electric_vehicles_per_state, aes(label=abbrev_state), size=2, color="black") +
  scale_fill_distiller(palette="Purples", name="Total Vehicles", direction = 1, labels = scales::comma_format()) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  ) +
  no_axis

map_total_e_vehicles
ggsave("./4_plots/plot_total_e_vehicles.png", plot = map_total_e_vehicles)


# Electric vehicles as a percentage of the total state fleet
map_share_of_e_vehicles <- ggplot() +
  geom_sf(data=electric_vehicles_per_state, aes(fill=percentage_num), color=NA, size=.15) +
  labs(
    title = "Vehicle Fleet in Brazil",
    subtitle = "Percentage of Electrical Vechile from Total Fleet per State",
    fill = "Vehicles per Capita") +
  geom_sf_text(data=electric_vehicles_per_state, aes(label=abbrev_state), size=2, color="black") +
  scale_fill_distiller(palette="BuPu", name="% of Total Fleet", direction = 1) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  ) +
  no_axis

map_share_of_e_vehicles
ggsave("./4_plots/map_share_of_e_vehicles.png", plot = map_share_of_e_vehicles)


# Total Vehicles per Capita
map_vehicles_per_capita <- ggplot() +
  geom_sf(data=electric_vehicles_per_state, aes(fill=vehicle_per_capita), color=NA, size=.15) +
  labs(
    title = "Vehicle Fleet in Brazil",
    subtitle = "Vehicle per Capita per State",
    fill = "Vehicles per Capita") +
  geom_sf_text(data=electric_vehicles_per_state, aes(label=abbrev_state), size=2, color="black") +
  scale_fill_distiller(palette="Greens", name="Vehicles per Capita", direction = 1) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  ) +
  no_axis

map_vehicles_per_capita
ggsave("./4_plots/map_vehicles_per_capita.png", plot = map_vehicles_per_capita)


# Electric Vehicles per Capita
map_e_vehicles_per_capita <- ggplot() +
  geom_sf(data=electric_vehicles_per_state, aes(fill=electric_vehicle_per_capita), color=NA, size=.15) +
  labs(
    title = "Vehicle Fleet in Brazil",
    subtitle = "Electric Vehicle per Capita per State",
    fill = "Vehicles per Capita") +
  geom_sf_text(data=electric_vehicles_per_state, aes(label=abbrev_state), size=2, color="black") +
  scale_fill_distiller(palette="Blues", name="Electric Vehicles per Capita", direction = 1) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  ) +
  no_axis

map_e_vehicles_per_capita
ggsave("./4_plots/map_e_vehicles_per_capita.png", plot = map_e_vehicles_per_capita)


# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")


# 2. Raw dataframes ------------------------------------------------------------

# 2.1. Dowloaded files ---------------------------------------------------------

# Vehicle data on municipal level (per fuel type)
fleet_df_nov24 <- read_excel("1_raw_data/2_vehicle_fleet/D_Frota_por_UF_Municipio_COMBUSTIVEL_Novembro_2024.xlsx")


# 2.2. Files from packages -----------------------------------------------------

# State data with geographical coordinates
state_df <- geobr::read_state(year = 2020,showProgress = FALSE) 


# 2.3. Files from BigQuery ----------------------------------------------------

source("./2_code/01_descriptive-demographics.R")


# 3. Manipulating data to generate plots and tables ----------------------------

# 3.1 Vehicle fleet (total/electric) -------------------------------------------


# Aggregate municipal data in state level
fleet_state <- fleet_df_nov24 %>% 
  group_by(UF,`Combustível Veículo`) %>%  # Grouping by kind of fuel used
  summarize(Total = sum(`Qtd. Veículos`))

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
  filter(UF != "Sem Informação")

# Create a string to use as common key to join the data frames
state_df$UF <- state_df$name_state %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  toupper()

# Join electric vehicle df with geographic info on the states
electric_vehicles_per_state <- electric_vehicles_per_state %>% 
  left_join(state_df, by = "UF") %>% 
  select(code_state, abbrev_state, name_state, code_region, name_region, geom, total_electric_vehicles, total_vehicles, percentage) 

# Update variable name
electric_vehicles_per_state$name_state <- tolower(electric_vehicles_per_state$name_state)


# Adding State population to the data frame
state_pop <- pop_city %>% 
  group_by(sigla_uf) %>% 
  summarize(total_pop = sum(populacao)) %>% 
  rename(abbrev_state = sigla_uf)

electric_vehicles_per_state <- electric_vehicles_per_state %>% 
  left_join(state_pop, by = "abbrev_state")

electric_vehicles_per_state <- electric_vehicles_per_state %>% 
  mutate(vehicle_per_capita = total_vehicles/total_pop,
         electric_vehicle_per_capita = total_electric_vehicles/total_pop)


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
plot_total_e_vehicles <- ggplot() +
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

plot_total_e_vehicles
ggsave("./4_plots/plot_total_e_vehicles.png", plot = plot_total_e_vehicles)


# Electric vehicles as a percentage of the total state fleet
plot_share_of_e_vehicles <- ggplot() +
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

plot_share_of_e_vehicles
ggsave("./4_plots/plot_share_of_e_vehicles.png", plot = plot_share_of_e_vehicles)


# Total Vehicles per Capita
plot_vehicles_per_capita <- ggplot() +
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

plot_vehicles_per_capita
ggsave("./4_plots/plot_vehicles_per_capita.png", plot = plot_vehicles_per_capita)


# Electric Vehicles per Capita
plot_e_vehicles_per_capita <- ggplot() +
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

plot_e_vehicles_per_capita
ggsave("./4_plots/plot_e_vehicles_per_capita.png", plot = plot_e_vehicles_per_capita)


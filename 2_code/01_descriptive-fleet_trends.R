# 1. Packages ------------------------------------------------------------------

pacman::p_load(readr,        # Read CSV files
               readxl,       # Read Excel files
               dplyr,        # Data manipulation
               tidyr,        # Data tidying and reshaping
               ggplot2,      # Create data visualizations
               scales,       # Format axes and legends (percentages, currencies)
               stringr,      # String manipulation
               lubridate,    # Date and time manipulation
               basedosdados, # Access to Brazilian public data via BigQuery
               geobr,        # Brazilian geographic data
               sf,           # Handle spatial/geographic data
               viridis,      # Color palettes for data visualization
               stringi,
               RColorBrewer) # Additional color palettes for plotting


# 2. Raw dataframes ------------------------------------------------------------

# 2.1. Dowloaded files ---------------------------------------------------------

# Vehicle data on municipal level (per fuel type)


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


# Aggregate municipal data in state level
fleet_state_24 <- fleet_df_nov24 %>% 
  group_by(UF,`Combustível Veículo`) %>%  # Grouping by kind of fuel used
  summarize(Total = sum(`Qtd. Veículos`))

# Data frame of total vehicles per state
vehicles_per_state_24 <- fleet_state_24 %>%
  group_by(UF) %>%
  summarise(total_vehicles = sum(Total))

# Filter electric vehicles and creating a new variable for % of electric or hybrid vehicles
electric_vehicles_per_state_24 <- fleet_state_24 %>%
  filter(grepl("^ELETRICO", `Combustível Veículo`)) %>%
  group_by(UF) %>%
  summarise(total_electric_vehicles = sum(Total)) %>% 
  left_join(vehicles_per_state, by = "UF") %>% 
  mutate(percentage = vehicles_per_state_24::percent(total_electric_vehicles/total_vehicles, accuracy = 0.01)) %>% 
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




# Aggregate municipal data in state level
fleet_state_21 <- fleet_df_nov21 %>% 
  group_by(UF,`Combustível Veículo`) %>%  # Grouping by kind of fuel used
  summarize(Total = sum(`Qtd. Veículos`))

# Data frame of total vehicles per state
vehicles_per_state_21 <- fleet_state_21 %>%
  group_by(UF) %>%
  summarise(total_vehicles = sum(Total))

# Filter electric vehicles and creating a new variable for % of electric or hybrid vehicles
electric_vehicles_per_state_21 <- fleet_state_21 %>%
  filter(grepl("^ELETRICO", `Combustível Veículo`)) %>%
  group_by(UF) %>%
  summarise(total_electric_vehicles = sum(Total)) %>% 
  left_join(vehicles_per_state_21, by = "UF") %>% 
  mutate(percentage = total_electric_vehicles/total_vehicles, accuracy = 0.01) %>% 
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



















file_paths <- c(
  "1_raw_data/2_vehicle_fleet/2013_11_3_frota_uf_municipio_combustivel_nov_2013.xlsx",
  "1_raw_data/2_vehicle_fleet/2014-11_3_frota_uf_municipio_combustivel_nov_2014.xlsx",
  "1_raw_data/2_vehicle_fleet/2015-11_3_Frota_Por_UF_Municipio_Combustivel_NOV_2015.xlsx",
  "1_raw_data/2_vehicle_fleet/2016-11_3_-_combustivel_-novembro_-2016.xlsx",
  "1_raw_data/2_vehicle_fleet/2017-11_frota_por_uf_municipio_combustivel_nov_17.xlsx",
  "1_raw_data/2_vehicle_fleet/2018-11_d_frota_por_uf_municipio_combustivel_novembro_2018.xlsx",
  "1_raw_data/2_vehicle_fleet/2019-11_d_frota_por_uf_municipio_combustivel_novembro_2019.xlsx",
  "1_raw_data/2_vehicle_fleet/2020-11_d_frota_por_uf_municipio_combustivel_novembro_2020.xlsx",
  "1_raw_data/2_vehicle_fleet/2021-11_d_frota_por_uf_municipio_combustivel_novembro_2021.xlsx",
  "1_raw_data/2_vehicle_fleet/2022-11_d_frota_por_uf_municipio_combustivel_novembro_2022.xlsx",
  "1_raw_data/2_vehicle_fleet/2023-11_d_frota_por_uf_municipio_combustivel_novembro_2023.xlsx",
  "1_raw_data/2_vehicle_fleet/2024-11_D_Frota_por_UF_Municipio_COMBUSTIVEL_Novembro_2024.xlsx")


create_fleet_analysis_function <- function(file_paths, state_df) {
  
  # Function to process each file
  process_file <- function(file_path) {
    # Extract year from filename (first 4 characters)
    year <- as.numeric(substr(basename(file_path), 1, 4))
    
    fleet_df <- read_excel(file_path)
    
    # Aggregate municipal data at state level
    fleet_state <- fleet_df %>% 
      group_by(UF, `Combustível Veículo`) %>%
      summarize(Total = sum(`Qtd. Veículos`), .groups = 'drop')
    
    # Data frame of total vehicles per state
    vehicles_per_state <- fleet_state %>%
      group_by(UF) %>%
      summarise(total_vehicles = sum(Total), .groups = 'drop')
    
    # Filter electric vehicles and create percentage
    electric_vehicles_per_state <- fleet_state %>%
      filter(grepl("^ELETRICO", `Combustível Veículo`)) %>%
      group_by(UF) %>%
      summarise(total_electric_vehicles = sum(Total), .groups = 'drop') %>% 
      left_join(vehicles_per_state, by = "UF") %>% 
      mutate(
        percentage = scales::percent(total_electric_vehicles/total_vehicles, accuracy = 0.01),
        year = year  # Add year column
      ) %>% 
      filter(UF != "Sem Informação")
    
    return(electric_vehicles_per_state)
  }
  
  # Process all files and combine results
  all_results <- lapply(file_paths, process_file)
  combined_results <- bind_rows(all_results)
  
  # Prepare state data frame
  state_df$UF <- state_df$name_state %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    toupper()
  
  # Final join with geographic information
  final_result <- combined_results %>% 
    left_join(state_df, by = "UF") %>% 
    select(year, code_state, abbrev_state, name_state, code_region, 
           name_region, geom, total_electric_vehicles, 
           total_vehicles, percentage)
  
  # Store the result in the global environment
  assign("fleet_analysis_results", final_result, envir = .GlobalEnv)
  
  return(final_result)
}

create_fleet_analysis_function(file_paths, state_df)

yearly_results <- fleet_analysis_results %>% 
  group_by(year) %>% 
  summarize(total_e_vehicles = sum(total_electric_vehicles),
            total_vehicles = sum(total_vehicles)) %>% 
  mutate(share_e_vehicles = total_e_vehicles/total_vehicles)









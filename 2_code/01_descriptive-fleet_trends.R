# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")


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


# 3. Overview of the vehicle fleet per combustible type ------------------------

# 3.0 Exploring categories per year  -------------------------------------------

## This chunk of code was used only to explore individual data sets, as I was having 
## issues with distorted values in certain years and categories, and I decided to look
## into what the issue could be by exploring the data frames a bit more in depth.

categorias13 <- fleet_df_nov13 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2013)

categorias14 <- fleet_df_nov14 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2014)

categorias15 <- fleet_df_nov15 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2015)

categorias16 <- fleet_df_nov16 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2016)

categorias17 <- fleet_df_nov17 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2017)

categorias18 <- fleet_df_nov18 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2018)

categorias19 <- fleet_df_nov19 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2019)

categorias20 <- fleet_df_nov20 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2020)

categorias21 <- fleet_df_nov21 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2021)

categorias22 <- fleet_df_nov22 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2022)

categorias23 <- fleet_df_nov23 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2023)

categorias24 <- fleet_df_nov24 %>% 
  group_by(`Combustível Veículo`) %>% 
  summarize(total = sum(`Qtd. Veículos`)) %>% 
  mutate(year = 2024)

all_categories <- bind_rows(categorias13,
                            categorias14,
                            categorias15,
                            categorias16,
                            categorias17,
                            categorias18,
                            categorias19,
                            categorias20,
                            categorias21,
                            categorias22,
                            categorias23,
                            categorias24)


## Findings: difference between "pure" electric vehicles and hybrids, NA values for
## uncategorized vehicles in large numbers between 2016 and 2017, and all categories
## in 2021 had a 0 after the number (needed to divide by 10 to adjust.)


# 3.1 Function to create the data frames for analysis --------------------------

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
    
    # Categorize vehicles and calculate totals
    vehicles_by_type <- fleet_state %>%
      mutate(vehicle_type = case_when(
        `Combustível Veículo` %in% c("ELETRICO", "ELETRICO/FONTE INTERNA") ~ "Electric",
        grepl("/ELETRICO", `Combustível Veículo`) | `Combustível Veículo` == "ELETRICO/FONTE EXTERNA" ~ "Hybrid",
        TRUE ~ "Other"
      )) %>%
      group_by(UF, vehicle_type) %>%
      summarise(total = sum(Total), .groups = 'drop') %>%
      pivot_wider(
        names_from = vehicle_type,
        values_from = total,
        values_fill = 0
      ) %>%
      left_join(vehicles_per_state, by = "UF") %>%
      mutate(
        # Adjust 2021 values
        Electric = if(year == 2021) Electric/10 else Electric,
        Hybrid = if(year == 2021) Hybrid/10 else Hybrid,
        Other = if(year == 2021) Other/10 else Other,
        total_vehicles = if(year == 2021) total_vehicles/10 else total_vehicles,
        # Calculate percentages
        electric_percentage = scales::percent(Electric/total_vehicles, accuracy = 0.01),
        hybrid_percentage = scales::percent(Hybrid/total_vehicles, accuracy = 0.01),
        year = year
      ) %>%
      filter(UF != "Sem Informação")
    
    return(vehicles_by_type)
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
           name_region, geom, Electric, Hybrid, Other,
           total_vehicles, electric_percentage, hybrid_percentage)
  
  # Store the result in the global environment
  assign("fleet_analysis_results", final_result, envir = .GlobalEnv)
  
  return(final_result)
}

create_fleet_analysis_function(file_paths, state_df)


# 3.2 Creating data sets for the plots -----------------------------------------

# Aggregate values for all states and years 
yearly_results_aggregate <- fleet_analysis_results %>% 
  drop_na() %>% 
  group_by(year) %>% 
  summarize(Electric = sum(Electric),
            Hybrid   = sum(Hybrid),
            Other    = sum(Other),
            Total   = sum(total_vehicles)) %>% 
  mutate(Share_electric = Electric/Total,
         Share_hybrid   = Hybrid/Total)

if (!file.exists("./3_processed_data/fleet_yearly_state_wide.csv")) {
  write_csv(yearly_results_aggregate,
            file = "./3_processed_data/fleet_yearly_state_wide.csv")
} else {
  print("File already exists in the repository")
}


# Observations per state
yearly_results_state <- fleet_analysis_results %>% 
  drop_na() %>% 
  group_by(year, name_state) %>% 
  summarize(Electric = sum(Electric),
            Hybrid   = sum(Hybrid),
            Other    = sum(Other),
            Total   = sum(total_vehicles)) %>% 
  mutate(Share_electric = Electric/Total,
         Share_hybrid   = Hybrid/Total) 

if (!file.exists("./3_processed_data/fleet_yearly_state_long.csv")) {
  write_csv(yearly_results_state,
            file = "./3_processed_data/fleet_yearly_state_long.csv")
} else {
  print("File already exists in the repository")
}


yearly_results_aggregate <- read.csv("./3_processed_data/fleet_yearly_state_wide.csv")

# Pivoted table per category
yearly_results_agg_long <- yearly_results_aggregate %>% 
  select(year,Electric, Hybrid,Other) %>% 
  pivot_longer(
    cols      = !year,
    names_to  = "Category",
    values_to = "Count"
  )

if (!file.exists("./3_processed_data/fleet_yearly_agg_long.csv")) {
  write_csv(yearly_results_agg_long,
            file = "./3_processed_data/fleet_yearly_agg_long.csv")
} else {
  print("File already exists in the repository")
}

yearly_results_agg_long %>% 
  filter(year == 2024) %>% 
  group_by(year) %>% 
  summarize(total = sum(Count))



# 3.3 Line plots to check trends in data ---------------------------------------

# Electric vehicles (aggregate)

plot_trend_ev_agg <- ggplot(yearly_results_aggregate, aes(x = as.integer(year), y = Electric)) +
  geom_line(color = "#69b3a2", linewidth = 2) +
  geom_point(size = 3, color = "#69b3a2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles",
    title = "Evolution of Electric Vehicles in Brazil",
    subtitle = "Source: National Traffic Secretariat, 2024")

plot_trend_ev_agg

ggsave("./4_plots/plot_trend_ev_agg.png",
       plot = plot_trend_ev_agg)

# Hybrid vehicles (aggregate)

plot_trend_hyb_agg <- ggplot(yearly_results_aggregate, aes(x = as.integer(year), y = Hybrid)) +
  geom_line(color = "#69b3a2", linewidth = 2) +
  geom_point(size = 3, color = "#69b3a2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles",
    title = "Evolution of Hybrid Vehicles in Brazil")

plot_trend_hyb_agg

ggsave("./4_plots/plot_trend_hyb_agg.png",
       plot = plot_trend_hyb_agg)


# Other vehicles (aggregate)

plot_trend_other_agg <- ggplot(yearly_results_aggregate, aes(x = as.integer(year), y = Other)) +
  geom_line(color = "#69b3a2", linewidth = 2) +
  geom_point(size = 3, color = "#69b3a2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles",
    title = "Evolution of Non-Electric Vehicles in Brazil")

plot_trend_other_agg

ggsave("./4_plots/plot_trend_other_agg.png",
       plot   = plot_trend_other_agg)

# All vehicles (aggregate, compared across categories)

plot_trend_compared_cat <- ggplot(yearly_results_agg_long, aes(x = as.integer(year), y = Count, color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(
    trans = "log10",
    breaks = scales::trans_breaks("log10", function(x) 10^x), # Powers of 10
    labels = scales::comma # Readable labels with commas
  ) +
  theme(
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"), # Dashed grid lines for y-axis
    panel.grid.minor.y = element_blank() # Remove minor grid lines for clarity
  ) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles",
    title = "Evolution of Registered Vehicles in Brazil per Combustible Type",
    subtitle = "Source: National Traffic Secretariat, 2024",
    color = "Category"
  )+
  scale_color_manual(
    values = c(
      "Electric" = "orange3",   # Teal
      "Hybrid" = "turquoise3",     # Orange
      "Gasoline" = "red4",   # Purple
      "Diesel" = "#e7298a",     # Pink
      "Other" = "red4"       # Green
    )
  )

plot_trend_compared_cat

ggsave("./4_plots/plot_trend_compared_cat.png",
        plot   = plot_trend_compared_cat,
        height = 4,
        width  = 8)



# Electric vehicles (compared across states)

plot_trend_compared_states <- ggplot(yearly_results_state, aes(x = as.integer(year), y = log(Electric))) +
  geom_line(color = "#69b3a2", linewidth = 2) +
  geom_point(size = 3, color = "#69b3a2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles (log scale)",
    title = "Evolution of Electric Vehicles in Brazil")+
  facet_wrap(~ name_state, nrow = 5)

plot_trend_compared_states

ggsave("./4_plots/plot_trend_compared_states.png",
       plot = plot_trend_compared_states,
       width  = 15,
       height = 15,
       units = "in")






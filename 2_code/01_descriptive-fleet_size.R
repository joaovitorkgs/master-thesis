# Packages and raw dataframes -- -----------------------------------------------

pacman::p_load (readr,        # Ler arquivos .csv
                readxl,       # Ler arquivos .xlsx
                dplyr,        # Manipulacao de dados
                tidyr,        # Manipulacao de dados
                ggplot2,      # Criacao de gráficos
                scales,       # Uso de porcentages
                stringr,      # Manipulacao de strings
                lubridate, 
                basedosdados, # BigQuery from public data on Brazil
                geobr,
                sf,
                viridis,
                RColorBrewer)


fleet_df_nov24 <- read_excel("1_raw_data/2_vehicle_fleet/D_Frota_por_UF_Municipio_COMBUSTIVEL_Novembro_2024.xlsx")


# This code chunk below is not so useful; disregard for noew

pop_city <- read_csv("1_raw_data/0_demographics/pop_city.csv")

pop_city$id_municipio_nome <- pop_city$id_municipio_nome %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  toupper()

fleet_df_nov24 <- fleet_df_nov24 %>% 
  mutate(id_municipio_nome = Município)

fleet_df_pop <- left_join(fleet_df_nov24, pop_city, by= "id_municipio_nome")

# Fleet df

fleet_state <- fleet_df_nov24 %>% 
  group_by(UF,`Combustível Veículo`) %>% 
  summarize(Total = sum(`Qtd. Veículos`))

vehicles_per_state <- fleet_state %>%
  group_by(UF) %>%
  summarise(total_vehicles = sum(Total))

electric_vehicles_per_state <- fleet_state %>%
  filter(grepl("^ELETRICO", `Combustível Veículo`)) %>%
  group_by(UF) %>%
  summarise(total_electric_vehicles = sum(Total)) %>% 
  left_join(vehicles_per_state, by = "UF") %>% 
  mutate(percentage = scales::percent(total_electric_vehicles/total_vehicles, accuracy = 0.01)) %>% 
  filter(UF != "Sem Informação")


state_df <- geobr::read_state(year = 2020,showProgress = FALSE) 

  
state_df$UF <- state_df$name_state %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  toupper()

electric_vehicles_per_state <- electric_vehicles_per_state %>% 
  left_join(state_df, by = "UF") %>% 
  select(code_state, abbrev_state, name_state, code_region, name_region, geom, total_electric_vehicles, total_vehicles, percentage) 

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


electric_vehicles_per_state$name_state <- tolower(electric_vehicles_per_state$name_state)




ggplot() +
  geom_sf(data=electric_vehicles_per_state, aes(fill=total_electric_vehicles), color= NA, size=.15) +
  labs(subtitle="% of Electric Vehicles per State ", size=8) +
  scale_fill_distiller(palette = "Blues", name="Percentage", limits = c(65,80)) +
  theme_minimal() +
  no_axis


stat_sf()




electric_vehicles_per_state <- st_as_sf(electric_vehicles_per_state)

electric_vehicles_per_state$percentage_num <- as.numeric(sub("%", "", electric_vehicles_per_state$percentage))



ggplot() +
  geom_sf(data=electric_vehicles_per_state, aes(fill=percentage_num), color=NA, size=.15) +
  labs(subtitle="Percentage of Electric Vehicles per State", size=8) +
  scale_fill_distiller(palette="YlGn", name="Percentage") + 
  theme_minimal() +
  no_axis


ggplot() +
  geom_sf(data=electric_vehicles_per_state, aes(fill=total_electric_vehicles), color=NA, size=.15) +
  labs(subtitle ="Total Number of Electric Vehicles per State", size=8) +
  scale_fill_distiller(palette="YlGn", name="Number of Electric Vehicles") + 
  theme_minimal() +
  no_axis


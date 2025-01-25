# 1. Packages and raw dataframes -- --------------------------------------------

source("./2_code/00_packages.R")


# 2. Raw Dataframes-------------------------------------------------------------

# 2.1. Files from BigQuery -----------------------------------------------------

query <- "
SELECT
    dados.ano as ano,
    dados.mes as mes,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.tipo_consumo as tipo_consumo,
    dados.numero_consumidores as numero_consumidores,
    dados.consumo as consumo
FROM `basedosdados.br_mme_consumo_energia_eletrica.uf` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
"

df_electricity <- read_sql(query, billing_project_id = get_billing_id())

# Adjusted main dataframe to include date information

df_electricity_date <- df_electricity %>% 
  mutate(
    mes = as.numeric(as.character(mes)),    # Convert 'mes' to numeric if it's not
    mes = sprintf("%02d", mes),             # Format 'mes' as double digits
    ano_mes = paste(ano, mes, sep = "-"),   # Combine 'ano' and 'mes'
    ano_mes_date = as.Date(paste(ano, mes, "01", sep = "-")) # Create date column
  ) 

df_electricity_date <- df_electricity_date %>%
  mutate(tipo_consumo = case_when(
    tipo_consumo == "Cativo" ~ "Captive (regulated) consumers",
    tipo_consumo == "Comercial" ~ "Commercial",
    tipo_consumo == "Outros" ~ "Others",
    tipo_consumo == "Residencial" ~ "Residential",
    TRUE ~ tipo_consumo # Keep original value if no match
  ))


# 2.2. Files from packages -----------------------------------------------------

# State data with geographical coordinates
state_df <- geobr::read_state(year = 2020,showProgress = FALSE) 
city_df <- geobr::read_municipality(year = 2020,showProgress = TRUE)



# 3. Energy Consumption --------------------------------------------------------

# 3.1. Aggregate Energy consumption

# Total consumption nationwide

energy_aggregate_all <- df_electricity_date %>%
  filter(tipo_consumo != ("Total")) %>% 
  group_by(ano_mes_date, tipo_consumo) %>%
  summarise(total = sum(consumo))

energy_aggregate_total <- df_electricity_date %>%
  filter(tipo_consumo == ("Total")) %>% 
  group_by(ano_mes_date, tipo_consumo) %>%
  summarise(total = sum(consumo))


# Total consumption per State

state_df_UF <- state_df %>% 
  mutate(sigla_uf = abbrev_state)

energy_aggregate_UF <- df_electricity_date %>% 
  left_join(state_df_UF, by = "sigla_uf") %>% 
  select(ano, ano_mes_date,sigla_uf,sigla_uf_nome,tipo_consumo,name_region,consumo,numero_consumidores,geom) %>% 
  rename(`Region Name` = name_region)


# Translation of region names
energy_aggregate_UF <- energy_aggregate_UF %>%
  mutate(`Region Name` = case_when(
    `Region Name` == "Norte" ~ "North",
    `Region Name` == "Nordeste" ~ "Northeast",
    `Region Name` == "Centro Oeste" ~ "Central-West",
    `Region Name` == "Sudeste" ~ "Southeast",
    `Region Name` == "Sul" ~ "South",
    TRUE ~ `Region Name` # Keep original value if no match
  ))

energy_aggregate_all_UF <- energy_aggregate_UF %>%
  filter(tipo_consumo != ("Total")) %>% 
  group_by(ano_mes_date, tipo_consumo,`Region Name`) %>%
  summarise(total = sum(consumo))

energy_aggregate_total_UF <- energy_aggregate_UF %>%
  filter(tipo_consumo == ("Total")) %>% 
  group_by(ano_mes_date, tipo_consumo,sigla_uf_nome) %>%
  summarise(total = sum(consumo))

# Total Consumption per State (2023)

energy_aggregate_total_UF_23 <- energy_aggregate_UF %>%
  filter(tipo_consumo == ("Total")) %>% 
  filter(ano == 2023) %>% 
  group_by(sigla_uf) %>%
  summarise(total = sum(consumo)) %>% 
  left_join(state_df_UF, by = "sigla_uf")

  


# 3.2. Plots -------------------------------------------------------------------

# All types in same chart

# Total consumption
plot_trend_energy_total_agg <- ggplot(energy_aggregate_total, 
                                  aes(x = ano_mes_date, 
                                      y = (total / 1000), 
                                      color = tipo_consumo, 
                                      group = tipo_consumo)) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Electricity Consumption (GWh)",
    title = "Aggregate Electricity Consumption in Brazil per Type",
    subtitle = "Source: Ministry of Mines and Energy, 2024",
    color = "Consumption Type"  # Legend title for 'tipo_consumo'
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels
  )

ggsave("./4_plots/plot_trend_energy_total_agg.png",
       plot = plot_trend_energy_total_agg,
       units = "in")


# All types, except "Total"
plot_trend_energy_all_agg <- ggplot(energy_aggregate_all, 
                                  aes(x = ano_mes_date, 
                                      y = (total / 1000), 
                                      color = tipo_consumo, 
                                      group = tipo_consumo)) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Electricity Consumption (GWh)",
    title = "Aggregate Electricity Consumption in Brazil per Type",
    subtitle = "Source: Ministry of Mines and Energy, 2024",
    color = "Consumption Type"  # Legend title for 'tipo_consumo'
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels
  )


ggsave("./4_plots/plot_trend_energy_all_agg.png",
       plot = plot_trend_energy_all_agg,
       units = "in")



# Breakdown per Type per Region
plot_trend_energy_regions_agg <- ggplot(energy_aggregate_all_UF, 
                                        aes(x = ano_mes_date, 
                                            y = log((total / 1000)), 
                                            color = tipo_consumo, 
                                            group = tipo_consumo)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = log(c(150, 500, 1000, 3000, 8000)),  # Define nice round numbers
    labels = function(x) scales::comma(round(exp(x))),  # Transform and round labels
    name = "Total Electricity Consumption (GWh) - Log Scale"
  ) +
  labs(
    x = "Year",
    title = "Aggregate Electricity Consumption in Brazil's 5 Regions per Type",
    subtitle = "Source: Ministry of Mines and Energy, 2024",
    color = "Consumption Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  facet_wrap(~ `Region Name`, nrow = 2)

ggsave("./4_plots/plot_trend_energy_regions_agg.png",
       plot = plot_trend_energy_regions_agg,
       units = "in",
       width = 15,
       height = 7)


# Breakdown per State (Total Consumption)

# Total consumption
plot_trend_energy_total_UF <- ggplot(energy_aggregate_total_UF, 
                                      aes(x = ano_mes_date, 
                                          y = log((total / 1000)), 
                                          color = tipo_consumo, 
                                          group = tipo_consumo)) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Electricity Consumption (GWh)",
    title = "Total Electricity Consumption in Brazilian States per Year",
    subtitle = "Source: Ministry of Mines and Energy, 2024",
    color = "Consumption Type"  # Legend title for 'tipo_consumo'
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels
  ) +
  facet_wrap(~ sigla_uf_nome, nrow = 6)




# For future reference:
# In the context of Brazilian electricity markets, "Cativo" refers to consumers
# who purchase electricity at regulated tariffs from distribution companies.
# These consumers cannot choose their electricity supplier and are subject to
# government-regulated pricing. This category typically includes small businesses,
# households, and other entities that lack direct access to the free market for electricity.


# Look into: "The Governance of Regulators  Driving Performance at Brazil’s
# Electricity Regulatory Agency, OECD, 2021, p. 56"




# 3.3. Maps --------------------------------------------------------------------

# Convert the df to generate the map
energy_aggregate_total_UF_23 <- st_as_sf(energy_aggregate_total_UF_23)

# Create a variable for the theme in the plots
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Total Energy consumption per State in 2023

map_total_energy_consumption_23 <- ggplot() +
  geom_sf(data=energy_aggregate_total_UF_23, aes(fill=(total/1000)), color=NA, size=.15) +
  labs(
    title = "Total Energy Consumption per State in 2023",
    subtitle = "Source: Ministry of Mines and Energy, 2024",
    fill = "Energy Consumption") +
  geom_sf_text(data=energy_aggregate_total_UF_23, aes(label=abbrev_state), size=2, color="black") +
  scale_fill_distiller(palette="YlOrRd", name="Energy Consumption (GWh)", direction = 1, labels = scales::comma_format()) + 
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

map_total_energy_consumption_23
ggsave("./4_plots/map_total_energy_consumption_23.png", plot = map_total_energy_consumption_23)


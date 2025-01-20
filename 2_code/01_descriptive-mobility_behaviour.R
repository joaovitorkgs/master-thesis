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
               gt,
               webshot2,
               RColorBrewer) # Additional color palettes for plotting


# 2. Raw dataframes ------------------------------------------------------------

# 2.1. Dowloaded files ---------------------------------------------------------



# 2.2. Files from packages -----------------------------------------------------

# State data with geographical coordinates
state_df <- geobr::read_state(year = 2020,showProgress = FALSE) 
city_df <- geobr::read_municipality(year = 2020,showProgress = TRUE)



# 2.3. Files from BigQuery ----------------------------------------------------

# Population Size
source("./2_code/01_descriptive-demographics.R")

# Commute time
set_billing_id("central-stream-297218")

query_commute_time <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.tempo_medio_deslocamento as tempo_medio_deslocamento,
    dados.prop_deslocamento_acima_1_hora as prop_deslocamento_acima_1_hora
FROM `basedosdados.br_mobilidados_indicadores.tempo_deslocamento_casa_trabalho` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

df_commute_time <-read_sql(query_commute_time, billing_project_id = get_billing_id())


# 3. Overview of the commute time per capitals ---------------------------------

# 3.0 Exploring categories per year  -------------------------------------------
  
  
city_coordinates <- city_df %>%
  select(code_muni, geom, name_region) %>% 
  mutate(code_muni = as.character(code_muni)) %>% 
  rename(id_municipio  = code_muni,
         `Region Name` = name_region)

pop_size <- pop_city %>% 
  select(id_municipio, populacao) 
  
df_commute_cities <- df_commute_time %>% 
  left_join(city_coordinates, by = "id_municipio") %>% 
  left_join(pop_size, by = "id_municipio") %>% 
  na.omit()
  
# Translation of region names
df_commute_cities <- df_commute_cities %>%
  mutate(`Region Name` = case_when(
    `Region Name` == "Norte" ~ "North",
    `Region Name` == "Nordeste" ~ "Northeast",
    `Region Name` == "Centro Oeste" ~ "Central-West",
    `Region Name` == "Sudeste" ~ "Southeast",
    `Region Name` == "Sul" ~ "South",
    TRUE ~ `Region Name` # Keep original value if no match
  ))


plot_scatter_popsize_commute <- ggplot(df_commute_cities, aes(x = tempo_medio_deslocamento, y = populacao, color = `Region Name`)) + 
  geom_point(size = 4, alpha = 0.5) + # Adjust point transparency
  scale_y_continuous(
    name = "Population size",
    trans = "log10", # Logarithmic scale for better readability
    breaks = c(1e4, 1e5, 1e6, 1e7, 5e7), # Custom breaks for markers
    labels = c("10k", "100k", "1M", "10M", "50M") # Custom labels
  ) +
  scale_x_continuous(name = "Average Commute Time (minutes)") +
  scale_color_brewer(palette = "Dark2") + # Improved color palette
  labs(
    title = "Average Commute Time in Selected Cities in Brazil",
    subtitle = "Source: MobiliDADOS, 2010"
  ) +
  theme_minimal() + # Clean background
  theme(
    panel.grid.minor = element_blank(), # Remove minor gridlines
    panel.grid.major.x = element_line(color = "gray80"), # Subtle gridlines for x-axis
    panel.grid.major.y = element_line(color = "gray80"), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "right",
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave("./4_plots/plot_scatter_popsize_commute.png", plot = plot_scatter_popsize_commute)


plot_scatter_popsize_commute_reg <- ggplot(df_commute_cities, aes(x = tempo_medio_deslocamento, y = populacao, color = `Region Name`)) + 
  geom_point(size = 4, alpha = 0.5) + # Adjust point transparency
  scale_y_continuous(
    name = "Population size",
    trans = "log10", # Logarithmic scale for better readability
    breaks = c(1e4, 1e5, 1e6, 1e7, 5e7), # Custom breaks for markers
    labels = c("10k", "100k", "1M", "10M", "50M") # Custom labels
  ) +
  scale_x_continuous(name = "Average Commute Time (minutes)") +
  labs(
    title = "Average Commute Time in Selected Cities in Brazil per Region",
    subtitle = "Source: MobiliDADOS, 2010"
  ) +
  theme_bw() + # Clean background
  theme(
    panel.grid.minor = element_blank(), # Remove minor gridlines
    panel.grid.major.x = element_line(color = "gray80"), # Subtle gridlines for x-axis
    panel.grid.major.y = element_line(color = "gray80"), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none",
    plot.background = element_rect(fill = "#ffffff", color = NA)
  ) + facet_wrap(~ `Region Name`, nrow = 2)

ggsave("./4_plots/plot_scatter_popsize_commute_reg.png", plot = plot_scatter_popsize_commute_reg)


# Formatting tables with the data

# Table 1: ordered by commute time
df_commute_cities_gt_1 <- df_commute_cities %>% 
  arrange(desc(tempo_medio_deslocamento)) %>% 
  select(id_municipio_nome, `Region Name`, tempo_medio_deslocamento, populacao) %>% 
  mutate(tempo_medio_deslocamento = as.numeric(tempo_medio_deslocamento),
         populacao = as.numeric(populacao)) %>% 
  head(15)

# Format the gt table
commute_cities_gt_1 <- df_commute_cities_gt_1 %>%
  gt() %>%
  tab_header(
    title = "Average Commute Time in Selected Cities in Brazil",
    subtitle = "15 cities with greater commute times"
  ) %>%
  cols_label(
    id_municipio_nome = "City Name",
    `Region Name` = "Region",
    tempo_medio_deslocamento = "Time (min)",
    populacao = "Population Size"
  ) %>%
  fmt_number(
    columns = c(populacao), # Format population size with thousand separators
    sep_mark = ",", # Use comma as the thousand separator
    decimals = 0 # No decimal places
  ) %>%
  cols_align(
    align = "left",
    columns = c(id_municipio_nome) # Left align City Name column
  ) %>% 
  tab_footnote(
    locations = NULL,
    footnote  = "Source: Mobilidados, 2010")

# Display the formatted table
commute_cities_gt_1

# Saving formatted table
commute_cities_gt_1 %>% 
  gtsave("./6_tables/commute_cities_gt_1.png", expand = 10)



# Table 2: ordered by population size
df_commute_cities_gt_2 <- df_commute_cities %>% 
  arrange(desc(populacao)) %>% 
  select(id_municipio_nome, `Region Name`, tempo_medio_deslocamento, populacao) %>% 
  mutate(tempo_medio_deslocamento = as.numeric(tempo_medio_deslocamento),
         populacao = as.numeric(populacao)) %>% 
  head(15)

# Format the gt table
commute_cities_gt_2 <- df_commute_cities_gt_2 %>%
  gt() %>%
  tab_header(
    title = "Average Commute Time in Selected Cities in Brazil",
    subtitle = "15 largest cities by population size"
  ) %>%
  cols_label(
    id_municipio_nome = "City Name",
    `Region Name` = "Region",
    tempo_medio_deslocamento = "Time (min)",
    populacao = "Population Size"
  ) %>%
  fmt_number(
    columns = c(populacao), # Format population size with thousand separators
    sep_mark = ",", # Use comma as the thousand separator
    decimals = 0 # No decimal places
  ) %>%
  cols_align(
    align = "left",
    columns = c(id_municipio_nome) # Left align City Name column
  ) %>% 
  tab_footnote(
    locations = NULL,
    footnote  = "Source: Mobilidados, 2010")

# Display the formatted table
commute_cities_gt_2

# Saving formatted table
commute_cities_gt_2 %>% 
  gtsave("./6_tables/commute_cities_gt_2.png", expand = 10)










## This chunk below is not yet being used --------------------------------------

# Convert the df to generate the map
df_commute_cities <- st_as_sf(df_commute_cities)


# Create a variable for the theme in the plots
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# 
plot_commute_time <- ggplot() +
  geom_sf(data=df_commute_cities, aes(fill=tempo_medio_deslocamento), color=NA, size=.15) +
  labs(
    title = "Average Commute Time in Selected Cities in Brazil",
    subtitle = "Time measured in Minutes. Source: MobiliDADOS, 2010",
    fill = "Avg. Commute Time") +
  scale_fill_distiller(palette="Purples", name="Avg. Commute Time", direction = 1, labels = scales::comma_format()) + 
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

plot_commute_time
  

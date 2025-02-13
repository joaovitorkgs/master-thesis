# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Files from BigQuery ----------------------------------------------------

if (!file.exists("./1_raw_data/0_demographics/ibge_pop_raw_df.csv")) {
  
  query_pop <- "
  SELECT
  dados.ano as ano,
  dados.sigla_uf AS sigla_uf,
  diretorio_sigla_uf.nome AS sigla_uf_nome,
  dados.id_municipio AS id_municipio,
  diretorio_id_municipio.nome AS id_municipio_nome,
  dados.populacao as populacao
  FROM `basedosdados.br_ibge_populacao.municipio` AS dados
  LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
  ON dados.sigla_uf = diretorio_sigla_uf.sigla
  LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
  ON dados.id_municipio = diretorio_id_municipio.id_municipio
  "
  
  ibge_pop_raw_df <-   read_sql(query_pop, billing_project_id = get_billing_id())
  write_csv(ibge_pop_raw_df,
            file = "./1_raw_data/0_demographics/ibge_pop_raw_df.csv")
} else {
  ibge_pop_raw_df <- read_csv("1_raw_data/0_demographics/ibge_pop_raw_df.csv")
  print("File already exists in the repository")
}

## 2.2. Data from project pipeline ---------------------------------------------

fleet_2013_2024 <- read_csv("1_raw_data/2_vehicle_fleet/frota_2013_2024.csv")
fleet_2013_2024_clean <- read_csv("1_raw_data/2_vehicle_fleet/fleet_2013_2024_clean.csv")

## 3. Cleaning city data and adding ids ----------------------------------------

### 3.1. Separating cities by state --------------------------------------------

#### 3.1.1 DENATRAM city data --------------------------------------------------

fleet_df_cities <- fleet_2013_2024_clean %>% 
  group_by(UF) %>% 
  distinct(city)

fleet_df_cities <- fleet_df_cities[!grepl('^Sem ', fleet_df_cities$UF), ]
fleet_df_cities <- fleet_df_cities[!grepl('^N', fleet_df_cities$UF), ]
fleet_df_cities <- fleet_df_cities[!grepl('^U', fleet_df_cities$UF), ]

fleet_df_cities <- fleet_df_cities %>% 
  mutate(key = paste(UF,city, sep = "_")) 


#### 3.1.1 IBGE city data ------------------------------------------------------

ibge_df_cities <- ibge_pop_raw_df %>% 
  select(sigla_uf, sigla_uf_nome, id_municipio, id_municipio_nome) %>% 
  distinct(sigla_uf, sigla_uf_nome, id_municipio, id_municipio_nome) %>% 
  rename(
    sigla_uf      = sigla_uf,
    UF            = sigla_uf_nome,
    id_municipio  = id_municipio,
    city          = id_municipio_nome
  )

clean_text <- function(text) {
  text %>%
    toupper() %>%
    stri_trans_general("Latin-ASCII")
}


ibge_df_cities <- ibge_df_cities %>%
  mutate(
    city = clean_text(city),
    UF = clean_text(UF)
  )

ibge_df_cities_key <- ibge_df_cities %>% 
  mutate(key = paste(UF,city, sep = "_")) %>% 
  select(key, id_municipio)


### 3.2. Attempt at joining tables ---------------------------------------------

fleet_df_key <- fleet_df_cities %>% 
  left_join(ibge_df_cities_key, by = "key") %>% 
  drop_na()

### 3.3. Adding city key identifier to main data frames ------------------------

ibge_pop_raw_df <- ibge_pop_raw_df %>% 
  rename(year = ano)

fleet_2013_2024_id <- fleet_2013_2024 %>% 
  mutate(key = paste(UF,city, sep = "_")) %>% 
  left_join(ibge_df_cities_key, by = "key") %>% 
  left_join(ibge_pop_raw_df, by = c("year" = "year", "id_municipio" = "id_municipio")) %>% 
  select(sigla_uf, sigla_uf_nome, id_municipio, id_municipio_nome, fuel, total, month, year, date, populacao)

if (!file.exists(  "./3_processed_data/fleet_2013_2024_id.csv")) {
  write_csv(fleet_2013_2024_id,
            file = "./3_processed_data/fleet_2013_2024_id.csv")
} else {
  print("File already exists in the repository")
}


fleet_2013_2024_clean_id <- fleet_2013_2024_clean %>% 
  mutate(key = paste(UF,city, sep = "_")) %>% 
  left_join(ibge_df_cities_key, by = "key") %>% 
  left_join(ibge_pop_raw_df, by = c("year" = "year", "id_municipio" = "id_municipio")) %>% 
  select(sigla_uf, sigla_uf_nome, id_municipio, id_municipio_nome, diesel, ethanol, gasoline, other, electric, gas, month, year, date, populacao) %>% 
  drop_na()

if (!file.exists(  "./3_processed_data/fleet_2013_2024_id_clean.csv")) {
  write_csv(fleet_2013_2024_clean_id,
            file = "./3_processed_data/fleet_2013_2024_id_clean.csv")
} else {
  print("File already exists in the repository")
}

## 4. Exploring the data -------------------------------------------------------

# Correlation between population and electric vehicles

fleet_2022_ev <- fleet_2013_2024_clean_id %>% 
  mutate(ev_per_capita = electric/populacao) %>% 
  filter(year == 2022) %>% 
  group_by(year, id_municipio) %>% 
  summarize(ev_per_capita = mean(ev_per_capita),
            population    = mean(populacao))

ggplot(fleet_2022_ev, aes(x=log(ev_per_capita), y=log(population))) + 
  geom_point()


# Correlation between population and number of vehicles

fleet_2022_all <- fleet_2013_2024_clean_id %>% 
  mutate(cars_per_capita = (diesel+ethanol+gasoline+other+electric)/populacao) %>% 
  filter(year == 2022) %>% 
  group_by(year, id_municipio) %>% 
  summarize(cars_per_capita = mean(cars_per_capita),
            population    = mean(populacao))

ggplot(fleet_2022_all, aes(x=cars_per_capita, y=log(population))) + 
  geom_point()

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


## 2.2. Downloaded data --------------------------------------------------------

ibge_pop_projections <- read_excel(
  "1_raw_data/0_demographics/projecoes_2024_tab1_idade_simples.xlsx",
  skip = 5)

## 2.3. Data from project pipeline ---------------------------------------------

fleet_2013_2024 <- read_csv("./1_raw_data/2_vehicle_fleet/frota_2013_2024.csv")

## Keeping a record of how many undefined registered vehicles were in the data

unique(fleet_2013_2024$UF)
missing_data <- c(
  "Sem InformaÃ§Ã£o",
  "Nao Identificado",
  "Nao se Aplica",
  "Sem Informacao",
  "Sem Combustível",
  "UF",
  "Não Identificado",
  "Não se Aplica",
  "NÃ£o Identificado",
  "NÃ£o se Aplica")

BEV_missing_data <- fleet_2013_2024 %>% 
  filter(UF %in% missing_data) 

BEV_missing_data %>% 
  filter(fuel == "ELETRICO/FONTE INTERNA") %>% 
  group_by() %>% 
  summarize(BEV = sum(total)) 


## 3. Cleaning city data and adding ids ----------------------------------------

### 3.1. Separating cities by state --------------------------------------------

#### 3.1.1 DENATRAM city data --------------------------------------------------

fleet_df_cities <- fleet_2013_2024 %>% 
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

## 4. Cleaning aggregate population data from national projections -------------

BR_pop_projections <- ibge_pop_projections %>% 
  filter(SIGLA == "BR",
         SEXO  == "Ambos") %>% 
  group_by(SIGLA) %>% 
  summarize(
    `2019` = sum(`2019`), 
    `2020` = sum(`2020`), 
    `2021` = sum(`2021`),  
    `2022` = sum(`2022`),  
    `2023` = sum(`2023`),  
    `2024` = sum(`2024`))

BR_pop_long <- BR_pop_projections %>%
  pivot_longer(
    cols = c("2019", "2020", "2021", "2022", "2023", "2024"),
    names_to = "Year",
    values_to = "Population"
  ) %>% 
  mutate(date = as.Date(paste(Year, "12", "01", sep = "-")))

# Interpolating monthly data from the available projections

monthly_dates <- seq(from = as.Date("2019-12-01"), 
                     to = as.Date("2024-12-01"), 
                     by = "month")

BR_pop_monthly <- data.frame(date = monthly_dates)

BR_pop_monthly$Population <- approx(
  x = as.numeric(BR_pop_long$date),  # Convert dates to numeric for interpolation
  y = BR_pop_long$Population,        # Original population values
  xout = as.numeric(BR_pop_monthly$date)  # Dates to interpolate for
)$y

BR_pop_monthly %>% 
  ggplot(aes(x=date, y=Population)) +
  geom_line()


if (!file.exists(  "./3_processed_data/BR_pop_monthly.csv")) {
  write_csv(BR_pop_monthly,
            file = "./3_processed_data/BR_pop_monthly.csv")
} else {
  print("File already exists in the repository")
}

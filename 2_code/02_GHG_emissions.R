# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Downloaded data --------------------------------------------------------

edgar_GHG_CO2_per_sector        <- read_excel("1_raw_data/7_GHG_emissions/edgar_GHG_CO2_per_sector.xlsx")
edgar_GHG_totals_by_country     <- read_excel("1_raw_data/7_GHG_emissions/edgar_GHG_totals_by_country.xlsx")
edgar_GHG_per_capita_by_country <- read_excel("1_raw_data/7_GHG_emissions/edgar_GHG_per_capita_by_country.xlsx")
edgar_GHG_per_GDP_by_country    <- read_excel("1_raw_data/7_GHG_emissions/edgar_GHG_per_GDP_by_country.xlsx")

## 2.2. Big query data ---------------------------------------------------------

query <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.nivel_1 as nivel_1,
    dados.nivel_2 as nivel_2,
    dados.nivel_3 as nivel_3,
    dados.nivel_4 as nivel_4,
    dados.nivel_5 as nivel_5,
    dados.nivel_6 as nivel_6,
    dados.tipo_emissao as tipo_emissao,
    dados.gas as gas,
    dados.atividade_economica as atividade_economica,
    dados.produto as produto,
    dados.emissao as emissao
FROM `basedosdados.br_seeg_emissoes.municipio` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

# Warning: data frame below has 2.12GB in size
if (!file.exists(  "./1_raw_data/7_GHG_emissions/GHG_BR_city.csv")) {
  GHG_BR_city <- read_sql(query, billing_project_id = get_billing_id())
  write_csv(GHG_BR_city,
            file = "./1_raw_data/7_GHG_emissions/GHG_BR_city.csv")
  print("File saved in the repository.")
} else {
  print("File already exists in the repository.")
}

# Para carregar o dado direto no R
query_uf <- "
SELECT
    dados.ano as ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.nivel_1 as nivel_1,
    dados.nivel_2 as nivel_2,
    dados.nivel_3 as nivel_3,
    dados.nivel_4 as nivel_4,
    dados.nivel_5 as nivel_5,
    dados.nivel_6 as nivel_6,
    dados.tipo_emissao as tipo_emissao,
    dados.gas as gas,
    dados.atividade_economica as atividade_economica,
    dados.produto as produto,
    dados.emissao as emissao
FROM `basedosdados.br_seeg_emissoes.uf` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
"

# Warning: data frame below has 2.12GB in size
if (!file.exists(  "./1_raw_data/7_GHG_emissions/GHG_BR_uf.csv")) {
  GHG_BR_uf <- read_sql(query_uf, billing_project_id = get_billing_id())
  write_csv(GHG_BR_uf,
            file = "./1_raw_data/7_GHG_emissions/GHG_BR_uf.csv")
  print("File saved in the repository.")
} else {
  print("File already exists in the repository.")
}





# 3. Data manipulation ---------------------------------------------------------

## Brazilian GHG emission per Sector -------------------------------------------

# Filtering Brazil data from the dataset
BRA_CO2_per_sector <- edgar_GHG_CO2_per_sector %>% 
  filter(Country == "Brazil")

# Unpivoting data to use in plots
BRA_CO2_per_sector_longer <- BRA_CO2_per_sector %>% 
  select(-"EDGAR Country Code") %>% 
  pivot_longer(
    cols = "1970":"2023",
    values_to = "emissions"
  ) %>% 
  rename(year = name) %>% 
  mutate(year = as.numeric(year))

# Saving processed .csv
if (!file.exists(  "./3_processed_data/BRA_CO2_per_sector_longer.csv")) {
  write_csv(BRA_CO2_per_sector_longer,
            file = "./3_processed_data/BRA_CO2_per_sector_longer.csv")
  print("File saved in the repository.")
} else {
  print("File already exists in the repository.")
}

# Exploring sectors
BRA_CO2_per_sector_longer %>% 
  group_by(Sector) %>% 
  summarize(total = sum(emissions))

## List of countries per GHG emission ------------------------------------------

### Total GHG Emissions

GHG_total_23 <- edgar_GHG_totals_by_country %>% 
  select("Country", "2023") %>% 
  rename("Emissions" = "2023") %>% 
  arrange(desc(Emissions)) %>% 
  filter(Country != "GLOBAL TOTAL") %>%
  filter(Country != "International Shipping") %>% 
  filter(Country != "International Aviation") %>% 
  head(20)

## Trend for top emitting countries

GHG_total_trend_20 <- edgar_GHG_totals_by_country %>% 
  filter(Country %in% c(
    "China",
    "United States",
    "India",
    "EU27",
    "Russia",
    "Brazil",
    "Indonesia",
    "Japan",
    "Iran",
    "Saudi Arabia",
    "Canada",
    "Mexico",
    "Germany",
    "South Korea",
    "Türkiye",
    "Australia",
    "Pakistan",
    "Viet Nam",
    "South Africa",
    "Thailand")) %>% 
  select(-"EDGAR Country Code") %>% 
  pivot_longer(
    cols = "1970":"2023",
    values_to = "emissions"
  ) %>% 
  rename(year = name) %>% 
  mutate(year = as.numeric(year))
  

GHG_total_trend_6 <- edgar_GHG_totals_by_country %>% 
  filter(Country %in% c(
    "China",
    "United States",
    "India",
    "EU27",
    "Russia",
    "Brazil")) %>% 
  select(-"EDGAR Country Code") %>% 
  pivot_longer(
    cols = "1970":"2023",
    values_to = "emissions"
  ) %>% 
  rename(year = name) %>% 
  mutate(year = as.numeric(year))

    
  


### GHG Emissions per Capita

GHG_pc_23 <- edgar_GHG_per_capita_by_country %>% 
  select("Country", "2023") %>% 
  rename("Emissions" = "2023") %>% 
  arrange(desc(Emissions)) %>% 
  filter(Country != "GLOBAL TOTAL") %>%
  filter(Country != "International Shipping") %>% 
  filter(Country != "International Aviation") %>% 
  head(20)

# Not sure how to interpret this data


### GHG Emissions per GDP 

GHG_gdp_23 <- edgar_GHG_per_GDP_by_country %>% 
  select("Country", "2023") %>% 
  rename("Emissions" = "2023") %>% 
  arrange(desc(Emissions)) %>% 
  filter(Country != "GLOBAL TOTAL") %>%
  filter(Country != "International Shipping") %>% 
  filter(Country != "International Aviation") %>% 
  head(20)



# Not sure how to interpret this either



# 4. Plot ----------------------------------------------------------------------

## 4.1. Data on Brazilian Emissions --------------------------------------------

### GHG Emissions per sector ---------------------------------------------------

plot_trend_GHG_per_sector_yearly_BR <- 
  BRA_CO2_per_sector_longer %>% 
  ggplot(aes(x = year, y = emissions, color = Sector)) +
  geom_line(size = 1) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = label_number(accuracy = 0.01) # Adjust accuracy as needed
  ) +
  labs(title = "Yearly CO2 Emissions in Brazil by Sector",
       subtitle = "Source: EDGAR (Emissions Database for Global Atmospheric Research) Community GHG Database",
       x = "Year",
       y = "Emissions (Mt CO2eq/yr)",
       color = "Sector") +
  theme_bw() +
  facet_wrap(~Sector, nrow = 3) +
  theme(legend.position = "none")

if (!file.exists("./4_plots/plot_trend_GHG_per_sector_yearly_BR.png")) {
  ggsave("./4_plots/plot_trend_GHG_per_sector_yearly_BR.png",
         plot = plot_trend_GHG_per_sector_yearly_BR,
         width  = 8,
         height = 8)
  print("File saved in the repository.")
  } else {
    print("File already exists in the repository.")
    }


### Transport Sector only ------------------------------------------------------

plot_trend_GHG_transportation_yearly_BR <- 
  BRA_CO2_per_sector_longer %>% 
  filter(Sector == "Transport") %>% 
  ggplot(aes(x = year, y = emissions, color = Sector)) +
  geom_line(size = 1) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = label_number(accuracy = 0.01) # Adjust accuracy as needed
  ) +
  labs(title = "CO2 Emissions in Brazil by Sector",
       subtitle = "1971 to 2023",
       x = "Year",
       y = "Emissions (CO2)",
       color = "Sector") +
  theme_bw() +
  theme(legend.position = "none")



## 4.2. Global Emissions -------------------------------------------------------

# Simple bar plot
GHG_total_23 %>% 
  mutate(Country = fct_reorder(Country, Emissions)) %>%
  ggplot(aes(x=Country, y=Emissions)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# Better looking bar plot
GHG_total_23 %>% 
  mutate(Country = fct_reorder(Country, Emissions),
         Highlight = ifelse(Country == "Brazil", "Brazil", "Other")) %>%
  ggplot(aes(x=Country, y=Emissions, fill=Highlight)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Brazil" = "#FF5733", "Other" = "#4682B4")) +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(title = "Total GHG Emissions by Country in 2023",
       subtitle = "Source: EDGAR (Emissions Database for Global Atmospheric Research) Community GHG Database",
       x = "Country",
       y = "Emissions (Mt CO2eq/yr)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title.position = "plot", # Align title and subtitle to the left
        plot.title = element_text(size=14, face="bold", hjust=0), # Title styling
        plot.subtitle = element_text(size=10, hjust=0, margin=margin(t=5, b=10)), # Subtitle styling with reduced space
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12)) +
  geom_text(aes(label=round(Emissions)), 
            hjust=-0.1, size=3)

# Line plot
GHG_total_trend_6 %>% 
  ggplot(aes(x = year, y = emissions, color = Country)) +
  geom_line(size = 1) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = label_number(accuracy = 1) # Adjust accuracy as needed
  ) +
  labs(title = "Yearly GHG Emissions per Country from 1970 to 2023",
       subtitle = "Source: EDGAR (Emissions Database for Global Atmospheric Research) Community GHG Database",
       x = "Year",
       y = "Emissions (Mt CO2eq/yr)",
       color = "Country") +
  theme_bw()


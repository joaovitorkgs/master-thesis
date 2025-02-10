# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")


# 2. Big Query -----------------------------------------------------------------

# 2.1. 2022 Census -------------------------------------------------------------

# Loading data from "Base dos Dados"
query_2022 <- "
SELECT
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.sigla_uf as sigla_uf,
    dados.populacao as populacao,
    dados.area as area,
    dados.taxa_alfabetizacao as taxa_alfabetizacao,
    dados.indice_envelhecimento as indice_envelhecimento,
    dados.idade_mediana as idade_mediana,
    dados.razao_sexo as razao_sexo
FROM `basedosdados.br_ibge_censo_2022.municipio` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

# Writing the demographic info per city

pop_city <- read_sql(query_2022, billing_project_id = get_billing_id())

# Saving it as a .csv file
write_csv(pop_city, "./1_raw_data/0_demographics/pop_city.csv")


# 2.2. Ministry of Health Data -------------------------------------------------

# Para carregar o dado direto no R
query_MS <- "
SELECT
    dados.ano as ano,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.sexo as sexo,
    dados.grupo_idade as grupo_idade,
    dados.populacao as populacao
FROM `basedosdados.br_ms_populacao.municipio` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

pop_data_MS <- read_sql(query_MS, billing_project_id = get_billing_id())

if (!file.exists("./1_raw_data/0_demographics/pop_data_MS.csv")) {
  write_csv(pop_data_MS,
            file = "./1_raw_data/0_demographics/pop_data_MS.csv")
} else {
  print("File already exists in the repository")
}

# 3. Aggregated Data per year --------------------------------------------------

yearly_pop_data <- pop_data_MS %>% 
  group_by(ano) %>% 
  summarize(population = sum(populacao))

if (!file.exists("./3_processed_data/yearly_pop_data.csv")) {
  write_csv(yearly_pop_data,
            file = "./3_processed_data/yearly_pop_data.csv")
} else {
  print("File already exists in the repository")
}

  


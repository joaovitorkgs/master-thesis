# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")


# 2. Raw dataframes ------------------------------------------------------------


# Loading data from "Base dos Dados"
query <- "
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

pop_city <- read_sql(query, billing_project_id = get_billing_id())

# Saving it as a .csv file
write_csv(pop_city, "./1_raw_data/0_demographics/pop_city.csv")

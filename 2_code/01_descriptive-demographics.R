

pacman::p_load (readr,        # Ler arquivos .csv
                readxl,       # Ler arquivos .xlsx
                dplyr,        # Manipulacao de dados
                tidyr,        # Manipulacao de dados
                ggplot2,      # Criacao de gráficos
                scales,       # Uso de porcentages
                stringr,      # Manipulacao de strings
                lubridate, 
                basedosdados) # BigQuery from public data on Brazil


# Defina o seu projeto no Google Cloud
set_billing_id("central-stream-297218")

# Para carregar o dado direto no R
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

pop_city <- read_sql(query, billing_project_id = get_billing_id())

write_csv(pop_city, "./1_raw_data/0_demographics/pop_city.csv")

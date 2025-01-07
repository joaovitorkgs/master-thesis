# Packages and raw dataframes -- -----------------------------------------------

pacman::p_load (readr,        # Ler arquivos .csv
                readxl,       # Ler arquivos .xlsx
                dplyr,        # Manipulacao de dados
                tidyr,        # Manipulacao de dados
                ggplot2,      # Criacao de gráficos
                scales,       # Uso de porcentages
                stringr,      # Manipulacao de strings
                lubridate, 
                basedosdados) # BigQuery from public data on Brazil


# Electricity consumption 

set_billing_id("central-stream-297218")


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

df <- read_sql(query, billing_project_id = get_billing_id())

df %>% 
  group_by(tipo_consumo, sigla_uf) %>% 
  summarise(total = sum(consumo))


# Ideas for the way forward:

# Idea 1:
# Use tables on population size for the states base for the year in question for
# comparation of states which consume the most electricity in total and per capita

# Idea 2:
# Describe how tax collection works for electricity in States
# Estimate electricity tax income



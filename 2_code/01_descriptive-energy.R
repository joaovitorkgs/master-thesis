# Packages and raw dataframes -- -----------------------------------------------

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
               RColorBrewer) # Additional color palettes for plotting


# Electricity consumption  -----------------------------------------------------

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





# Install and load required packages
install.packages("readxl")
install.packages("httr")
library(readxl)
library(httr)

# Specify the URL
url <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/vdpb/vecf/2023-vendas-etanol.xlsx"

# Download and save as a temporary file
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

# Read the Excel file starting from a specific row (e.g., row 5)
data <- read_excel(temp_file, skip = 4)  # Skip 4 rows to start reading from row 5
head(data)  # Display first few rows
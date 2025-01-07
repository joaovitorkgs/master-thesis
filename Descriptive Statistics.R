# Packages and raw dataframes -- -----------------------------------------------

pacman::p_load (readr,     # Ler arquivos .csv
                readxl,    # Ler arquivos .xlsx
                dplyr,     # Manipulacao de dados
                tidyr,     # Manipulacao de dados
                ggplot2,   # Criacao de gráficos
                scales,    # Uso de porcentages
                stringr,   # Manipulacao de strings
                lubridate) # Uso de dados


# Tax revenue per industry

arrecadacao_cnae <- read_delim("1_raw_data/arrecadacao-cnae.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

arrecadacao_estado <- read_delim("1_raw_data/arrecadacao-estado.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

arrecadacao_natureza <- read_delim("1_raw_data/arrecadacao-natureza.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

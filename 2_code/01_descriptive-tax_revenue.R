# Packages and raw dataframes -- -----------------------------------------------

pacman::p_load (readr,     # Ler arquivos .csv
                readxl,    # Ler arquivos .xlsx
                dplyr,     # Manipulacao de dados
                tidyr,     # Manipulacao de dados
                ggplot2,   # Criacao de gráficos
                scales,    # Uso de porcentages
                stringr,   # Manipulacao de strings
                lubridate) # Uso de dados


D_Frota_por_UF_Municipio_COMBUSTIVEL_Novembro_2024 <- read_excel("1_raw_data/2_vehicle_fleet/D_Frota_por_UF_Municipio_COMBUSTIVEL_Novembro_2024.xlsx")

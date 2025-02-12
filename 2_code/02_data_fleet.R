# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Files from BigQuery -----------------------------------------------------

# Para carregar o dado direto no R
query_fleet <- "
SELECT
    dados.ano as ano,
    dados.mes as mes,
    dados.sigla_uf as sigla_uf,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.tipo_veiculo as tipo_veiculo,
    dados.quantidade as quantidade
FROM `basedosdados.br_denatran_frota.municipio_tipo` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

fleet_raw_df <- read_sql(query_fleet, billing_project_id = get_billing_id())

if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_bd_all_data_raw_df.csv")) {
  write_csv(fleet_raw_df,
            file = "./1_raw_data/2_vehicle_fleet/fleet_bd_all_data_raw_df.csv")
} else {
  print("File already exists in the repository")
}



## 2.2. Scraping data online ----------------------------------------------------
### 2.2.1. Monthly Fleet --------------------------------------------------------
#### 2024 -------------------------------------------------------------------------

"https://www.gov.br/transportes/pt-br/assuntos/transito/conteudo-Senatran/D_Frota_por_UF_Municipio_COMBUSTIVEL_Dezembro_20241.xlsx"
"https://www.gov.br/transportes/pt-br/assuntos/transito/conteudo-Senatran/D_Frota_por_UF_Municipio_COMBUSTIVEL_Novembro_2024.xlsx"

base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/conteudo-Senatran/D_Frota_por_UF_Municipio_COMBUSTIVEL_"
months <- c("Dezembro", "Novembro", "Outubro", "Setembro", "Agosto", "Julho", "Junho", "Maio", "Abril", "Maro", "Fevereiro", "Janeiro")
year <- "2024"

download_and_process_xlsx <- function(base_url, months, year) {
  # Portuguese month names and their corresponding numbers
  month_map <- c("Janeiro" = 1, "Fevereiro" = 2, "Maro" = 3, "Abril" = 4, "Maio" = 5, "Junho" = 6,
                 "Julho" = 7, "Agosto" = 8, "Setembro" = 9, "Outubro" = 10, "Novembro" = 11, "Dezembro" = 12)
  
  # List to store all dataframes for binding later
  all_dataframes <- list()
  
  for (month in months) {
    # Construct the URL
    url <- paste0(base_url, month, "_", year)
    if (month == "Dezembro") {
      url <- paste0(url, "1")
    }
    url <- paste0(url, ".xlsx")
    
    # Download the file
    temp_file <- tempfile(fileext = ".xlsx")
    tryCatch({
      download.file(url, temp_file, mode = "wb")
      
      # Read the Excel file
      df <- read_excel(temp_file)
      
      # Add month and year columns
      df <- df %>%
        mutate(month = month_map[month],
               year = as.numeric(year),
               date = make_date(year, month_map[month], 1))
      
      # Create a name for the dataframe
      df_name <- paste0("frota_", year, "_", sprintf("%02d", month_map[month]))
      
      # Assign the dataframe to the global environment
      assign(df_name, df, envir = .GlobalEnv)
      
      # Add the dataframe to the list for binding later
      all_dataframes[[df_name]] <- df
      
      cat("Processed:", df_name, "\n")
    }, error = function(e) {
      cat("Failed to process:", url, "\nError:", e$message, "\n")
    })
    
    # Clean up the temporary file
    unlink(temp_file)
  }
}

download_and_process_xlsx(base_url, months, year)

frota_2024_all <- frota_2024_01 %>% 
  mutate(`Qtd. Veículos` = as.numeric(`Qtd. Veículos`)) %>% 
  bind_rows(
  frota_2024_02,
  frota_2024_03,
  frota_2024_04,
  frota_2024_05,
  frota_2024_06,
  frota_2024_07,
  frota_2024_08,
  frota_2024_09,
  frota_2024_10,
  frota_2024_11,
  frota_2024_12)

if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2024_fuel.csv")) {
  write_csv(frota_2024_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2024_fuel.csv")
} else {
  print("File already exists in the repository")
}

#### 2023 -------------------------------------------------------------------------

download_and_process_23_xlsx <- function(base_url, months, year) {
  month_map <- c("janeiro" = 1, "fevereiro" = 2, "marco" = 3, "abril" = 4, "maio" = 5, "junho" = 6,
                 "julho" = 7, "agosto" = 8, "setembro" = 9, "Outubro" = 10, "Novembro" = 11, "Dezembro" = 12)
  
  all_dataframes <- list()
  
  for (month in months) {
    url <- paste0(base_url, year, "/", month, "/d_frota_por_uf_municipio_combustivel_", tolower(month), "_", year, ".xlsx")
    
    temp_file <- tempfile(fileext = ".xlsx")
    tryCatch({
      download.file(url, temp_file, mode = "wb")
      
      df <- read_excel(temp_file)
      
      df <- df %>%
        mutate(month = month_map[month],
               year = as.numeric(year),
               date = make_date(year, month_map[month], 1))
      
      df_name <- paste0("frota_", year, "_", sprintf("%02d", month_map[month]))
      
      assign(df_name, df, envir = .GlobalEnv)
      
      all_dataframes[[df_name]] <- df
      
      cat("Processed:", df_name, "\n")
    }, error = function(e) {
      cat("Failed to process:", url, "\nError:", e$message, "\n")
    })
    
    unlink(temp_file)
  }
}

base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/"
months <- c("Dezembro", "Novembro", "Outubro", "setembro", "agosto", "julho", "junho", "maio", "abril", "marco", "fevereiro", "janeiro")
year <- "2023"

download_and_process_23_xlsx(base_url, months, year)

frota_2023_08 <- frota_2023_08 %>% 
  mutate(`Qtd. Veículos` = as.numeric(`Qtd. Veículos`))

frota_2023_all <- frota_2023_01 %>% 
  bind_rows(
    frota_2023_02,
    frota_2023_03,
    frota_2023_04,
    frota_2023_05,
    frota_2023_06,
    frota_2023_07,
    frota_2023_08,
    frota_2023_09,
    frota_2023_10,
    frota_2023_11,
    frota_2023_12)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2023_fuel.csv")) {
  write_csv(frota_2023_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2023_fuel.csv")
} else {
  print("File already exists in the repository")
}


#### 2022 -------------------------------------------------------------------------

download_and_process_22_xlsx <- function(base_url, months, year) {
  month_map <- c("janeiro" = 1, "fevereiro" = 2, "marco" = 3, "abril" = 4, "maio" = 5, "junho" = 6,
                 "julho" = 7, "agosto" = 8, "setembro" = 9, "outubro" = 10, "novembro" = 11, "dezembro" = 12)
  
  all_dataframes <- list()
  
  for (month in months) {
    url <- paste0(base_url, year, "/", month, "/d_frota_por_uf_municipio_combustivel_", tolower(month), "_", year, ".xlsx")
    
    temp_file <- tempfile(fileext = ".xlsx")
    tryCatch({
      download.file(url, temp_file, mode = "wb")
      
      df <- read_excel(temp_file)
      
      df <- df %>%
        mutate(month = month_map[month],
               year = as.numeric(year),
               date = make_date(year, month_map[month], 1))
      
      df_name <- paste0("frota_", year, "_", sprintf("%02d", month_map[month]))
      
      assign(df_name, df, envir = .GlobalEnv)
      
      all_dataframes[[df_name]] <- df
      
      cat("Processed:", df_name, "\n")
    }, error = function(e) {
      cat("Failed to process:", url, "\nError:", e$message, "\n")
    })
    
    unlink(temp_file)
  }
}

base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/"
months <- c("dezembro", "novembro", "outubro", "setembro", "agosto", "julho", "junho", "maio", "abril", "marco", "fevereiro", "janeiro")
year <- "2022"

download_and_process_22_xlsx(base_url, months, year)

frota_2022_all <- frota_2022_01 %>% 
  bind_rows(
    frota_2022_02,
    frota_2022_03,
    frota_2022_04,
    frota_2022_05,
    frota_2022_06,
    frota_2022_07,
    frota_2022_08,
    frota_2022_09,
    frota_2022_10,
    frota_2022_11,
    frota_2022_12)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2022_fuel.csv")) {
  write_csv(frota_2022_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2022_fuel.csv")
} else {
  print("File already exists in the repository")
}


#### 2021 ------------------------------------------------------------------------

download_and_process_21_xlsx <- function(base_url, months, year) {
  month_map <- c("janeiro" = 1, "fevereiro" = 2, "marco" = 3, "abril" = 4, "maio" = 5, "junho" = 6,
                 "julho" = 7, "agosto" = 8, "setembro" = 9, "outubro" = 10, "novembro" = 11, "dezembro" = 12)
  
  all_dataframes <- list()
  
  for (month in months) {
    url <- paste0(base_url, year, "/", month, "/d_frota_por_uf_municipio_combustivel_", tolower(month), "_", year, ".xlsx")
    
    temp_file <- tempfile(fileext = ".xlsx")
    tryCatch({
      download.file(url, temp_file, mode = "wb")
      
      df <- read_excel(temp_file)
      
      df <- df %>%
        mutate(month = month_map[month],
               year = as.numeric(year),
               date = make_date(year, month_map[month], 1))
      
      df_name <- paste0("frota_", year, "_", sprintf("%02d", month_map[month]))
      
      assign(df_name, df, envir = .GlobalEnv)
      
      all_dataframes[[df_name]] <- df
      
      cat("Processed:", df_name, "\n")
    }, error = function(e) {
      cat("Failed to process:", url, "\nError:", e$message, "\n")
    })
    
    unlink(temp_file)
  }
}

base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/"
months <- c("dezembro", "novembro", "outubro", "setembro", "agosto", "julho", "junho", "maio", "abril", "marco", "fevereiro", "janeiro")
year <- "2021"

download_and_process_21_xlsx(base_url, months, year)

# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Files from BigQuery ----------------------------------------------------

if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_bd_all_data_raw_df.csv")) {
  
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
    ON dados.id_municipio = diretorio_id_municipio.id_municipio"
  
  fleet_raw_df <- read_sql(query_fleet, billing_project_id = get_billing_id())
  write_csv(fleet_raw_df,
            file = "./1_raw_data/2_vehicle_fleet/fleet_bd_all_data_raw_df.csv")
} else {
  print("File already exists in the repository")
}


## 2.2. Scraping data online ---------------------------------------------------
### 2.2.1. Monthly Fleet -------------------------------------------------------
#### 2024 -------------------------------------------------------------------------

download_and_process_24_xlsx <- function(base_url, months, year) {
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

base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/conteudo-Senatran/D_Frota_por_UF_Municipio_COMBUSTIVEL_"
months <- c("Dezembro", "Novembro", "Outubro", "Setembro", "Agosto", "Julho", "Junho", "Maio", "Abril", "Maro", "Fevereiro", "Janeiro")
year <- "2024"

download_and_process_24_xlsx(base_url, months, year)

# Function to rename columns
rename_columns <- function(data) {
  # Rename columns and return the modified data frame
  data <- data %>% 
    rename(UF    = 1,
           city  = 2,
           fuel  = 3,
           total = 4,
           month = 5,
           year  = 6,
           date = 7)
  return(data) # Ensure the renamed data frame is returned
}

compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))

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
  frota_2024_12) %>% 
  rename_columns()
  

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

compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))


# Fixing column type and changing NAs for 0
frota_2023_08 <- rename_columns(frota_2023_08)
frota_2023_08$total <- as.numeric(frota_2023_08$total)
frota_2023_08$total[is.na(frota_2023_08$total)] <- 0

# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

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

compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))

# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

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

compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))

# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

# Correcting distorted values in one specific month in the data series
frota_2021_11$total <- frota_2021_11$total/10

frota_2021_all <- frota_2021_01 %>% 
  bind_rows(
    frota_2021_02,
    frota_2021_03,
    frota_2021_04,
    frota_2021_05,
    frota_2021_06,
    frota_2021_07,
    frota_2021_08,
    frota_2021_09,
    frota_2021_10,
    frota_2021_11,
    frota_2021_12)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2021_fuel.csv")) {
  write_csv(frota_2021_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2021_fuel.csv")
} else {
  print("File already exists in the repository")
}

#### 2020 ------------------------------------------------------------------------

download_and_process_20_xlsx <- function(months, year) {
  month_map <- c("janeiro" = 1, "fevereiro" = 2, "marco" = 3, "abril" = 4, "maio" = 5, "junho" = 6,
                 "julho" = 7, "agosto" = 8, "setembro" = 9, "outubro" = 10, "novembro" = 11, "dezembro" = 12)
  
  all_dataframes <- list()
  
  for (month in months) {
    if (month_map[month] <= 5) {
      base_url <- "https://www.gov.br/transportes/pt-br/centrais-de-conteudo/"
      url <- paste0(base_url, "d-frota-por-uf-municipio-combustivel-", tolower(month), "-", year, "-xlsx")
    } else {
      base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/"
      url <- paste0(base_url, year, "/", tolower(month), "/d_frota_por_uf_municipio_combustivel_", tolower(month), "_", year, ".xlsx")
    }
    
    temp_file <- tempfile(fileext = ".xlsx")
    tryCatch({
      download.file(url, temp_file, mode = "wb")
      
      df <- readxl::read_excel(temp_file)
      
      df <- df %>%
        dplyr::mutate(month = month_map[month],
                      year = as.numeric(year),
                      date = lubridate::make_date(year, month_map[month], 1))
      
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

months <- c("dezembro", "novembro", "outubro", "setembro", "agosto", "julho", "junho", "maio", "abril", "marco", "fevereiro", "janeiro")
year <- "2020"

download_and_process_20_xlsx(months, year)


compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))

# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

frota_2020_all <- frota_2020_01 %>% 
  bind_rows(
    frota_2020_02,
    frota_2020_03,
    frota_2020_04,
    frota_2020_05,
    frota_2020_06,
    frota_2020_07,
    frota_2020_08,
    frota_2020_09,
    frota_2020_10,
    frota_2020_11,
    frota_2020_12)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2020_fuel.csv")) {
  write_csv(frota_2020_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2020_fuel.csv")
} else {
  print("File already exists in the repository")
}

#### 2019 ------------------------------------------------------------------------

download_and_process_19_xlsx <- function(base_url, months, year) {
  month_map <- c("janeito" = 1, "fevereiro" = 2, "marco" = 3, "abril" = 4, "maio" = 5, "junho" = 6,
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
months <- c("dezembro", "novembro", "outubro", "setembro", "agosto", "julho", "junho", "maio", "abril", "marco", "fevereiro", "janeito")
year <- "2019"

download_and_process_19_xlsx(base_url, months, year)

# Typo in the file name; downloaded it manually and imported it 
frota_2019_01 <- read_excel("1_raw_data/2_vehicle_fleet/2019-01-d_frota_por_uf_municipio_combustivel_janeito_2019.xlsx") %>% 
  mutate(month = 1,
         year = 2019,
         date = make_date(2019,1,1))

# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

compare_df_cols(
  frota_2019_02,
  frota_2019_03,
  frota_2019_04,
  frota_2019_05,
  frota_2019_06,
  frota_2019_07,
  frota_2019_08,
  frota_2019_09,
  frota_2019_10,
  frota_2019_11,
  frota_2019_12)

# Fixing column type and changing NAs for 0
frota_2019_08$total <- as.numeric(frota_2019_08$total)
frota_2019_08$total[is.na(frota_2019_08$total)] <- 0

frota_2019_09$total <- as.numeric(frota_2019_09$total)
frota_2019_09$total[is.na(frota_2019_09$total)] <- 0


frota_2019_all <- frota_2019_01 %>% 
  bind_rows(
    frota_2019_02,
    frota_2019_03,
    frota_2019_04,
    frota_2019_05,
    frota_2019_06,
    frota_2019_07,
    frota_2019_08,
    frota_2019_09,
    frota_2019_10,
    frota_2019_11,
    frota_2019_12)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2019_fuel.csv")) {
  write_csv(frota_2019_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2019_fuel.csv")
} else {
  print("File already exists in the repository")
}


#### 2018 ------------------------------------------------------------------------

download_and_process_18_xlsx <- function(base_url, months, year) {
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
year <- "2018"

download_and_process_18_xlsx(base_url, months, year)


# Manual importing due to inconsistent file names

frota_2018_01 <- read_excel("1_raw_data/2_vehicle_fleet/2018_01_frota_por_uf_municipio_combustivel_jan_18.xlsx") %>% 
  mutate(month = 1,
         year = 2018,
         date = make_date(2018,1,1))

frota_2018_02 <- read_excel("1_raw_data/2_vehicle_fleet/2018_02_frota_por_uf_municipio_combustivel_fev_18.xlsx") %>% 
  mutate(month = 2,
         year = 2018,
         date = make_date(2018,2,1))

frota_2018_03 <- read_excel("1_raw_data/2_vehicle_fleet/2018_03_frota_por_uf_municipio_combustivel_mar_18.xlsx") %>% 
  mutate(month = 3,
         year = 2018,
         date = make_date(2018,3,1))

frota_2018_04 <- read_excel("1_raw_data/2_vehicle_fleet/2018_04_d_frota_por_uf_municipio_combustivel_abr_2018.xlsx") %>% 
  mutate(month = 4,
         year = 2018,
         date = make_date(2018,4,1))

frota_2018_05 <- read_excel("1_raw_data/2_vehicle_fleet/2018_05_d_frota_por_uf_municipio_combustivel_mai_2018.xlsx") %>% 
  mutate(month = 5,
         year = 2018,
         date = make_date(2018,6,1))

frota_2018_06 <- read_excel("1_raw_data/2_vehicle_fleet/2018_06_d_frota_por_uf_municipio_combustivel_jun_2018.xlsx") %>% 
  mutate(month = 6,
         year = 2018,
         date = make_date(2018,6,1))

# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))
  

frota_2018_all <- get(paste("frota_", year, "_01", sep = "")) %>% 
  bind_rows(get(paste("frota_", year, "_02", sep = "")),
            get(paste("frota_", year, "_03", sep = "")),
            get(paste("frota_", year, "_04", sep = "")),
            get(paste("frota_", year, "_05", sep = "")),
            get(paste("frota_", year, "_06", sep = "")),
            get(paste("frota_", year, "_07", sep = "")),
            get(paste("frota_", year, "_08", sep = "")),
            get(paste("frota_", year, "_09", sep = "")),
            get(paste("frota_", year, "_10", sep = "")),
            get(paste("frota_", year, "_11", sep = "")),
            get(paste("frota_", year, "_12", sep = "")))


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2018_fuel.csv")) {
  write_csv(frota_2018_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2018_fuel.csv")
} else {
  print("File already exists in the repository")
}


#### 2017 ------------------------------------------------------------------------

download_and_process_17_xlsx <- function(months, year) {
  month_map <- c("janeiro" = 1, "fevereiro" = 2, "marco" = 3, "abril" = 4, "maio" = 5, "junho" = 6,
                 "julho" = 7, "agosto" = 8, "setembro" = 9, "outubro" = 10, "novembro" = 11, "dezembro" = 12)
  
  month_abbr <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  names(month_abbr) <- names(month_map)
  
  year_short <- substr(year, 3, 4)
  
  all_dataframes <- list()
  
  for (month in months) {
    if (month_map[month] <= 6) {
      base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/"
      url <- paste0(base_url, year, "/", tolower(month), "/3_-_combustivel_-_", tolower(month), "_-_", year, ".xlsx")
    } else {
      base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/"
      url <- paste0(base_url, year, "/", tolower(month), "/frota_por_uf_municipio_combustivel_", month_abbr[month], "_", year_short, ".xlsx")
    }
    
    temp_file <- tempfile(fileext = ".xlsx")
    tryCatch({
      download.file(url, temp_file, mode = "wb")
      
      df <- readxl::read_excel(temp_file)
      
      df <- df %>%
        dplyr::mutate(month = month_map[month],
                      year = as.numeric(year),
                      date = lubridate::make_date(year, month_map[month], 1))
      
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

months <- c("dezembro", "novembro", "outubro", "setembro", "agosto", "julho", "junho", "maio", "abril", "marco", "fevereiro", "janeiro")
year <- "2017"

download_and_process_17_xlsx(months, year)

# Manual importing due to inconsistent file names
frota_2017_01 <- read_excel("1_raw_data/2_vehicle_fleet/2017_01_3_-_combustivel_-janeiro_-_2017.xlsx") %>% 
  mutate(month = 1,
         year = 2017,
         date = make_date(2017,1,1))

# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))



frota_2017_all <- get(paste("frota_", year, "_01", sep = "")) %>% 
  bind_rows(get(paste("frota_", year, "_02", sep = "")),
            get(paste("frota_", year, "_03", sep = "")),
            get(paste("frota_", year, "_04", sep = "")),
            get(paste("frota_", year, "_05", sep = "")),
            get(paste("frota_", year, "_06", sep = "")),
            get(paste("frota_", year, "_07", sep = "")),
            get(paste("frota_", year, "_08", sep = "")),
            get(paste("frota_", year, "_09", sep = "")),
            get(paste("frota_", year, "_10", sep = "")),
            get(paste("frota_", year, "_11", sep = "")),
            get(paste("frota_", year, "_12", sep = "")))


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2017_fuel.csv")) {
  write_csv(frota_2017_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2017_fuel.csv")
} else {
  print("File already exists in the repository")
}

#### 2016 ------------------------------------------------------------------------

# Manual importing due to inconsistent file names and formats (.rar)

frota_2016_01 <- read_excel("1_raw_data/2_vehicle_fleet/2016_01_3_Frota_Por_UF_Municipio_Combustivel_JAN_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,1,1))

frota_2016_02 <- read_excel("1_raw_data/2_vehicle_fleet/2016_02_3_Frota_Por_UF_Municipio_Combustivel_FEV_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,2,1))

frota_2016_03 <- read_excel("1_raw_data/2_vehicle_fleet/2016_03_3_Frota_Por_UF_Municipio_Combustivel_MAR_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,3,1))

frota_2016_04 <- read_excel("1_raw_data/2_vehicle_fleet/2016_04_3_Frota_Por_UF_Municipio_Combustivel_ABR_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,4,1))

frota_2016_05 <- read_excel("1_raw_data/2_vehicle_fleet/2016_05_3_Frota_Por_UF_Municipio_Combustivel_MAI_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,5,1))

frota_2016_06 <- read_excel("1_raw_data/2_vehicle_fleet/2016_06_3_Frota_Por_UF_Municipio_Combustivel_JUN_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,6,1))

frota_2016_07 <- read_excel("1_raw_data/2_vehicle_fleet/2016_07_3_Frota_Por_UF_Municipio_Combustivel_JUL_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,7,1))

frota_2016_08 <- read_excel("1_raw_data/2_vehicle_fleet/2016_08_3_Frota_Por_UF_Municipio_Combustivel_AGO_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,8,1))

frota_2016_09 <- read_excel("1_raw_data/2_vehicle_fleet/2016_09_3_Frota_Por_UF_Municipio_Combustivel_SET_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,9,1))

frota_2016_10 <- read_excel("1_raw_data/2_vehicle_fleet/2016_10_3_Frota_Por_UF_Municipio_Combustivel_OUT_2016.xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,10,1))

frota_2016_11 <- read_excel("1_raw_data/2_vehicle_fleet/2016_11_3_-_combustivel_-novembro_-2016 (1).xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,11,1))

frota_2016_12 <- read_excel("1_raw_data/2_vehicle_fleet/2016_12_3_frota_por_uf_municipio_combustivel_dez_2016 (1).xlsx") %>% 
  mutate(month = 1,
         year = 2016,
         date = make_date(2016,12,1))


year <- "2016"

# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))


frota_2016_all <- frota_2016_01 %>% 
  bind_rows(
    frota_2016_02,
    frota_2016_03,
    frota_2016_04,
    frota_2016_05,
    frota_2016_06,
    frota_2016_07,
    frota_2016_08,
    frota_2016_09,
    frota_2016_10,
    frota_2016_11,
    frota_2016_12)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2016_fuel.csv")) {
  write_csv(frota_2016_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2016_fuel.csv")
} else {
  print("File already exists in the repository")
}

#### 2015 ------------------------------------------------------------------------

# Manual importing due to inconsistent file names and formats (.rar)

read_and_process_frota <- function(year) {
  months <- c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")
  
  for (i in 1:12) {
    month_abbr <- months[i]
    file_year <- ifelse(i <= 8, year + 1, year)
    
    file_name <- sprintf("1_raw_data/2_vehicle_fleet/%d_%02d_3_Frota_Por_UF_Municipio_Combustivel_%s_%d.xlsx",
                         file_year, i, month_abbr, file_year)
    
    df <- read_excel(file_name) %>%
      mutate(month = i,
             year = year,
             date = make_date(year, i, 1))
    
    assign(sprintf("frota_%d_%02d", year, i), df, envir = .GlobalEnv)
  }
}

year <- 2015
read_and_process_frota(year)


# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_", year,"_0", i, sep = "") # e.g., "frota_2019_01"
  } else {
    df_name <- paste("frota_", year,"_", i, sep = "") # e.g., "frota_2019_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}

compare_df_cols(
  get(paste("frota_", year, "_01", sep = "")),
  get(paste("frota_", year, "_02", sep = "")),
  get(paste("frota_", year, "_03", sep = "")),
  get(paste("frota_", year, "_04", sep = "")),
  get(paste("frota_", year, "_05", sep = "")),
  get(paste("frota_", year, "_06", sep = "")),
  get(paste("frota_", year, "_07", sep = "")),
  get(paste("frota_", year, "_08", sep = "")),
  get(paste("frota_", year, "_09", sep = "")),
  get(paste("frota_", year, "_10", sep = "")),
  get(paste("frota_", year, "_11", sep = "")),
  get(paste("frota_", year, "_12", sep = "")))


frota_2015_all <- frota_2015_01 %>% 
  bind_rows(
    frota_2015_02,
    frota_2015_03,
    frota_2015_04,
    frota_2015_05,
    frota_2015_06,
    frota_2015_07,
    frota_2015_08,
    frota_2015_09,
    frota_2015_10,
    frota_2015_11,
    frota_2015_12)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2015_fuel.csv")) {
  write_csv(frota_2015_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2015_fuel.csv")
} else {
  print("File already exists in the repository")
}



#### 2014 ------------------------------------------------------------------------

download_and_process_14_xlsx <- function(months, year) {
  # Define month mappings and abbreviations
  month_map <- c("janeiro" = 1, "fevereiro" = 2, "marco" = 3, "abril" = 4, "maio" = 5, "junho" = 6,
                 "julho" = 7, "agosto" = 8, "setembro" = 9, "outubro" = 10, "novembro" = 11, "dezembro" = 12)
  
  month_abbr <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", 
                  "set", "out", "nov", "dez")
  
  names(month_abbr) <- names(month_map)
  
  all_dataframes <- list()
  
  for (month in months) {
    if (month_map[month] <= 8) {
      # Base URL for months 1-8
      base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/"
      url <- paste0(base_url, year, "/", tolower(month), "/3_frotaporufmunicipiocombustivel_", 
                    month_abbr[month], year, ".xlsx")
    } else {
      # Base URL for months 9-12
      base_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renavam/"
      url <- paste0(base_url, year, "/", tolower(month), "/3_frota_uf_municipio_combustivel_", 
                    month_abbr[month], "_", year, ".xlsx")
    }
    
    temp_file <- tempfile(fileext = ".xlsx")
    tryCatch({
      # Download the file
      download.file(url, temp_file, mode = "wb")
      
      # Read the Excel file
      df <- readxl::read_excel(temp_file)
      
      # Add metadata columns to the dataframe
      df <- df %>%
        dplyr::mutate(month = month_map[month],
                      year = as.numeric(year),
                      date = lubridate::make_date(year, month_map[month], 1))
      
      # Assign a name to the dataframe and save it to the global environment
      df_name <- paste0("frota_", year, "_", sprintf("%02d", month_map[month]))
      assign(df_name, df, envir = .GlobalEnv)
      
      # Store the dataframe in a list for further use if needed
      all_dataframes[[df_name]] <- df
      
      cat("Processed:", df_name, "\n")
    }, error = function(e) {
      cat("Failed to process:", url, "\nError:", e$message, "\n")
    })
    
    # Clean up temporary files
    unlink(temp_file)
  }
}

months <- c("dezembro", "novembro", "outubro", "setembro", 
            "agosto", "julho", "junho", 
            "maio", "abril", 
            "marco", 
            "fevereiro",
            "janeiro")
year <- '2014'

download_and_process_14_xlsx(months, year)


frota_2014_08 <- read_excel("1_raw_data/2_vehicle_fleet/2014_08_3_frotaporufmunicipiocombustivel_ago2014.xls") %>% 
  mutate(month = 1,
         year = 2014,
         date = make_date(2014,1,1))


# Loop through months and rename columns for each data frame
for (i in 1:12) {
  # Construct the name of the data frame dynamically
  if (i < 10) {
    df_name <- paste("frota_2014_0", i, sep = "") # e.g., "frota_2014_01"
  } else {
    df_name <- paste("frota_2014_", i, sep = "") # e.g., "frota_2014_10"
  }
  
  # Check if the object exists in the global environment
  if (exists(df_name)) {
    # Get the data frame object dynamically
    df <- get(df_name)
    
    # Rename columns using the rename_columns function
    df <- rename_columns(df)
    
    # Assign the modified data frame back to its original name
    assign(df_name, df, envir = .GlobalEnv)
    
    cat("Renamed columns for:", df_name, "\n")
  } else {
    cat("Data frame does not exist:", df_name, "\n")
  }
}


compare_df_cols(
  frota_2014_01,
  frota_2014_02,
  frota_2014_03,
  frota_2014_04,
  frota_2014_05,
  frota_2014_06,
  frota_2014_07,
  frota_2014_08,
  frota_2014_09,
  frota_2014_10,
  frota_2014_11,
  frota_2014_12)


frota_2014_all <- frota_2014_01 %>% 
  bind_rows(
  frota_2014_02,
  frota_2014_03,
  frota_2014_04,
  frota_2014_05,
  frota_2014_06,
  frota_2014_07,
  frota_2014_08,
  frota_2014_09,
  frota_2014_10,
  frota_2014_11,
  frota_2014_12)


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2014_fuel.csv")) {
  write_csv(frota_2014_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2014_fuel.csv")
} else {
  print("File already exists in the repository")
}


#### 2013 ------------------------------------------------------------------------

# Manual importing due to inconsistent file names and formats (Access Databases exported manually)

read_and_process_frota_13 <- function(year) {
  # Define month abbreviations
  months <- c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")
  
  for (i in 5:12) {  # Process only months 5-12
    month_abbr <- months[i]
    
    # Construct the file path based on the new format
    file_name <- sprintf("1_raw_data/2_vehicle_fleet/%d/%d_%02d_3QuantidadeVeiculosPorUFMunicipioCombustivel.xlsx",
                         year, year, i)
    
    tryCatch({
      # Read the Excel file
      df <- readxl::read_excel(file_name) %>%
        dplyr::mutate(month = i,
                      year = year,
                      date = lubridate::make_date(year, i, 1))
      
      # Assign a name to the dataframe and save it to the global environment
      df_name <- sprintf("frota_%d_%02d", year, i)
      assign(df_name, df, envir = .GlobalEnv)
      
      cat("Processed:", df_name, "\n")
    }, error = function(e) {
      cat("Failed to process:", file_name, "\nError:", e$message, "\n")
    })
  }
}

read_and_process_frota_13(2013)

convert_and_clean_column <- function(data, column) {
  # Replace commas with dots for decimal formatting
  data[[column]] <- gsub(",", ".", data[[column]])
  
  # Convert to numeric, suppress warnings for non-numeric values
  data[[column]] <- suppressWarnings(as.numeric(data[[column]]))
  
  # Replace NA (non-numeric values) with 0
  data[[column]][is.na(data[[column]])] <- 0
  
  return(data)
}

frota_2013_05 <- convert_and_clean_column(frota_2013_05, "QT VEICULOS")

standardize_column_names <- function(data) {
  # Define the standard column names
  standard_names <- c("UF", "municipality", "fuel", "total", "month", "year", "date")
  
  # Rename the columns of the data frame
  colnames(data) <- standard_names
  
  return(data)
}


frota_2013_all <- frota_2013_05 %>%
  standardize_column_names() %>%
  bind_rows(
    frota_2013_06 %>% standardize_column_names(),
    frota_2013_07 %>% standardize_column_names(),
    frota_2013_08 %>% standardize_column_names(),
    frota_2013_09 %>% standardize_column_names(),
    frota_2013_10 %>% standardize_column_names(),
    frota_2013_11 %>% standardize_column_names(),
    frota_2013_12 %>% standardize_column_names()
  ) %>% 
  rename_columns()


if (!file.exists("./1_raw_data/2_vehicle_fleet/fleet_2013_fuel.csv")) {
  write_csv(frota_2013_all,
            file = "./1_raw_data/2_vehicle_fleet/fleet_2013_fuel.csv")
} else {
  print("File already exists in the repository")
}





## 2.3. Combining data as one data frame ---------------------------------------

frota_2013_2024 <- frota_2013_all %>% 
  bind_rows(
    frota_2014_all,
    frota_2015_all,
    frota_2016_all,
    frota_2017_all,
    frota_2018_all,
    frota_2019_all,
    frota_2020_all,
    frota_2021_all,
    frota_2022_all,
    frota_2023_all,
    frota_2024_all,
  )

if (!file.exists("./1_raw_data/2_vehicle_fleet/frota_2013_2024.csv")) {
  write_csv(frota_2013_2024,
            file = "./1_raw_data/2_vehicle_fleet/frota_2013_2024.csv")
} else {
  print("File already exists in the repository")
}




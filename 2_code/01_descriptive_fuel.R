# 1. Packages ------------------------------------------------------------------

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
               httr,
               writexl,
               RColorBrewer) # Additional color palettes for plotting


# 2. Raw dataframes ------------------------------------------------------------

# 2.1. Gasoline  ---------------------------------------------------------------

# Define the function
download_and_combine_gasoline_data <- function(start_year = 2000, end_year = 2023) {
  # Base URL for the files
  base_url <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/arquivos-vendas-de-derivados-de-petroleo-e-biocombustiveis/gasolina-c/gasolina-c-municipio-"
  
  # Initialize an empty list to store datasets
  all_data <- list()
  
  # Loop through each year
  for (year in start_year:end_year) {
    # Construct the URLs for .xlsx and .xls files
    url_xlsx <- paste0(base_url, year, ".xlsx")
    url_xls <- paste0(base_url, year, ".xls")
    
    # Download the file to a temporary location
    temp_file <- tempfile()
    response <- GET(url_xlsx, write_disk(temp_file, overwrite = TRUE))
    
    # Check if the .xlsx download was successful
    if (response$status_code != 200) {
      # Attempt to download the .xls file if .xlsx fails
      response <- GET(url_xls, write_disk(temp_file, overwrite = TRUE))
    }
    
    # Check if either download was successful
    if (response$status_code == 200) {
      # Read the Excel file starting from row 5 and add a "year" column
      data <- read_excel(temp_file, skip = 4) %>%
        na.omit() %>%   # Remove rows with missing values
        mutate(year = year)  # Add a column for the year
      
      # Append the dataset to the list
      all_data[[as.character(year)]] <- data
    } else {
      warning(paste("Failed to download data for year:", year))
    }
  }
  
  # Combine all datasets into a single dataframe
  combined_data <- bind_rows(all_data)
  
  return(combined_data)
}

# Download all data on gasoline consumption using the function above
gasoline_data_raw <- download_and_combine_gasoline_data() %>% 
  mutate(municipio = ifelse(is.na(Município), Municípios, Município)) %>% 
  select(year, `CÓDIGO IBGE`, municipio, Vendas)  %>% 
  na.omit() 

## Important: data above takes about 1 min to run

# Save data to a single xlsx file

if (!file.exists("./3_processed_data/gasoline_data_raw.xlsx")) {
  writexl::write_xlsx(gasoline_data_raw, "./3_processed_data/gasoline_data_raw.xlsx")
  message("File saved successfully.")
} else {
  message("File already exists. Skipping save operation.")
}


# 2.2. Diesel  -----------------------------------------------------------------

# Define the function
download_and_combine_diesel_data <- function(start_year = 2000, end_year = 2023) {
  # Base URL for Diesel files
  base_url <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/arquivos-vendas-de-derivados-de-petroleo-e-biocombustiveis/oleo-diesel/oleo-diesel-municipio-"
  
  # Initialize an empty list to store datasets
  all_data <- list()
  
  # Loop through each year
  for (year in start_year:end_year) {
    # Construct the URLs for .xlsx and .xls files
    url_xlsx <- paste0(base_url, year, ".xlsx")
    url_xls <- paste0(base_url, year, ".xls")
    
    # Download the file to a temporary location
    temp_file <- tempfile()
    response <- GET(url_xlsx, write_disk(temp_file, overwrite = TRUE))
    
    # Check if the .xlsx download was successful
    if (response$status_code != 200) {
      # Attempt to download the .xls file if .xlsx fails
      response <- GET(url_xls, write_disk(temp_file, overwrite = TRUE))
    }
    
    # Check if either download was successful
    if (response$status_code == 200) {
      # Read the Excel file starting from row 5 and add a "year" column
      data <- read_excel(temp_file, skip = 4) %>%
        mutate(`CÓDIGO IBGE` = as.character(`CÓDIGO IBGE`)) %>%  # Ensure 'CÓDIGO IBGE' is a string
        na.omit() %>%   # Remove rows with missing values
        mutate(year = year)  # Add a column for the year
      
      # Append the dataset to the list
      all_data[[as.character(year)]] <- data
    } else {
      warning(paste("Failed to download data for year:", year))
    }
  }
  
  # Combine all datasets into a single dataframe
  combined_data <- bind_rows(all_data)
  
  return(combined_data)
}

# Download all data on diesel consumption using the function above
diesel_data_raw <- download_and_combine_diesel_data(start_year = 2000, end_year = 2023)
## Important: running code above takes about 1 min to run


diesel_data_raw <- diesel_data_raw %>% 
  mutate(municipio = ifelse(is.na(Município), Municípios, Município)) %>% 
  select(year, `CÓDIGO IBGE`, municipio, Vendas)  %>% 
  na.omit() 


# Save data to a single xlsx file

if (!file.exists("./3_processed_data/diesel_data_raw.xlsx")) {
  writexl::write_xlsx(diesel_data_raw, "./3_processed_data/diesel_data_raw.xlsx")
  message("File saved successfully.")
} else {
  message("File already exists. Skipping save operation.")
}


# 2.3. Ethanol  ----------------------------------------------------------------

# Define the function
download_and_combine_ethanol_data <- function(start_year = 2000, end_year = 2023) {
  # Base URL for Ethanol files
  base_url <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/arquivos-vendas-de-derivados-de-petroleo-e-biocombustiveis/etanol-hidratado/etanol-hidratado-municipio-"
  
  # Initialize an empty list to store datasets
  all_data <- list()
  
  # Loop through each year
  for (year in start_year:end_year) {
    # Construct the URLs for .xlsx and .xls files
    url_xlsx <- paste0(base_url, year, ".xlsx")
    url_xls <- paste0(base_url, year, ".xls")
    
    # Download the file to a temporary location
    temp_file <- tempfile()
    response <- GET(url_xlsx, write_disk(temp_file, overwrite = TRUE))
    
    # Check if the .xlsx download was successful
    if (response$status_code != 200) {
      # Attempt to download the .xls file if .xlsx fails
      response <- GET(url_xls, write_disk(temp_file, overwrite = TRUE))
    }
    
    # Check if either download was successful
    if (response$status_code == 200) {
      # Read the Excel file starting from row 5 and add a "year" column
      data <- read_excel(temp_file, skip = 4) %>%
        mutate(`CÓDIGO IBGE` = as.character(`CÓDIGO IBGE`), # Ensure 'CÓDIGO IBGE' is a string
                Vendas = as.numeric(Vendas)) %>%            # Convert 'Vendas' to numeric
        na.omit() %>%   # Remove rows with missing values
        mutate(year = year)  # Add a column for the year
      
      # Append the dataset to the list
      all_data[[as.character(year)]] <- data
    } else {
      warning(paste("Failed to download data for year:", year))
    }
  }
  
  # Combine all datasets into a single dataframe
  combined_data <- bind_rows(all_data)
  
  return(combined_data)
}

# Download all data on ethanol consumption using the function above
ethanol_data_raw <- download_and_combine_ethanol_data(start_year = 2000, end_year = 2023)


ethanol_data_raw <- ethanol_data_raw %>% 
  na.omit() 


# Save data to a single xlsx file

if (!file.exists("./3_processed_data/ethanol_data_raw.xlsx")) {
  writexl::write_xlsx(ethanol_data_raw, "./3_processed_data/ethanol_data_raw.xlsx")
  message("File saved successfully.")
} else {
  message("File already exists. Skipping save operation.")
}



# 3. Summarised data frames ----------------------------------------------------

gasoline_summary <- gasoline_data_raw %>% 
  filter(year >= 2012) %>% 
  group_by(year) %>% 
  summarise(total = sum(Vendas)) %>% 
  mutate(fuel = "gasoline")

diesel_summary <- diesel_data_raw %>% 
  filter(year >= 2012) %>% 
  group_by(year) %>% 
  summarise(total = sum(Vendas)) %>% 
  mutate(fuel = "diesel")

ethanol_summary <- ethanol_data_raw %>% 
  filter(year >= 2012) %>% 
  group_by(year) %>% 
  summarise(total = sum(Vendas)) %>% 
  mutate(fuel = "ethanol")


all_fuels_summary <- bind_rows(gasoline_summary,diesel_summary,ethanol_summary)


# 4. Plots ---------------------------------------------------------------------

## Gasoline
plot_trend_gasoline_consumption <- ggplot(gasoline_summary, aes(x = as.integer(year), y = total / 1e9)) +
  geom_line(color = "orange3", linewidth = 2) +
  geom_point(size = 3, color = "orange3") +
  theme_bw() +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Yearly Sales of Common Gasoline in Liters (Billion)",
    title = "Evolution of Gasoline Consumption in Brazil",
    subtitle = "Source: Brazilian National Agency for Petroleum, Natural Gas and Biofuels, 2025"
  )

plot_trend_gasoline_consumption
ggsave("./4_plots/plot_trend_gasoline_consumption.png",
       plot   = plot_trend_gasoline_consumption)

## Diesel
plot_trend_diesel_consumption <- ggplot(diesel_summary, aes(x = as.integer(year), y = total / 1e9)) +
  geom_line(color = "orange3", linewidth = 2) +
  geom_point(size = 3, color = "orange3") +
  theme_bw() +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Yearly Sales of Diesel in Liters (Billion)",
    title = "Evolution of Diesel Consumption in Brazil",
    subtitle = "Source: Brazilian National Agency for Petroleum, Natural Gas and Biofuels, 2025"
  )

plot_trend_diesel_consumption
ggsave("./4_plots/plot_trend_diesel_consumption.png",
       plot   = plot_trend_diesel_consumption)

## Ethanol
plot_trend_ethanol_consumption <- ggplot(ethanol_summary, aes(x = as.integer(year), y = total / 1e9)) +
  geom_line(color = "orange3", linewidth = 2) +
  geom_point(size = 3, color = "orange3") +
  theme_bw() +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Yearly Sales of Ethanol in Liters (Billion)",
    title = "Evolution of Ethanol Consumption in Brazil",
    subtitle = "Source: Brazilian National Agency for Petroleum, Natural Gas and Biofuels, 2025"
  )

plot_trend_ethanol_consumption 
ggsave("./4_plots/plot_trend_ethanol_consumption.png",
       plot   = plot_trend_ethanol_consumption)


## All fuels
plot_trend_allfuels_consumption <- ggplot(all_fuels_summary, aes(x = as.integer(year), y = total / 1e9, color = fuel)) +
  geom_line(linewidth = 2) +  # Separate lines by fuel type
  geom_point(size = 3) +      # Add points for each data point
  theme_bw() +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Yearly Sales of Fuel in Liters (Billion)",
    title = "Evolution of Fuel Consumption in Brazil",
    subtitle = "Source: Brazilian National Agency for Petroleum, Natural Gas and Biofuels, 2025",
    color = "Fuel Type"       # Legend title for fuel types
  )

plot_trend_allfuels_consumption
ggsave("./4_plots/plot_trend_allfuels_consumption.png",
       plot   = plot_trend_allfuels_consumption)


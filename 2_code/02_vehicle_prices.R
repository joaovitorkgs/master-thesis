# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Downloaded data --------------------------------------------------------

fipe_price_raw_df <- read_delim("1_raw_data/5_vehicle_prices/tabela-fipe-historico-precos.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

# 3. Data cleaning and organizing ----------------------------------------------

## 3.1. Main data frame --------------------------------------------------------

# Whole data frame with tagging of cars per engine traction / fuel type

fipe_price_13_22_df <- fipe_price_raw_df %>% 
  filter(anoReferencia > 2012) %>% 
  mutate(
    diesel = as.integer(str_detect(tolower(modelo), "diesel")),
    electric = as.integer(str_detect(tolower(modelo), "eletric|elétrico| kw ")),
    hybrid_ethanol = as.integer(str_detect(tolower(modelo), "flex")),
    gasoline = as.integer(!(diesel | electric | hybrid_ethanol))
  ) %>% 
  mutate(mesReferencia = str_pad(mesReferencia, width = 2, pad = "0")) %>% 
  mutate(date      = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep="")),
         def_price = deflateBR::deflate(valor, date, "12/2022", index = "inpc"),
         fuel = case_when(
           electric       == 1 ~ "electric",
           hybrid_ethanol == 1 ~ "hybrid",
           gasoline       == 1 ~ "gasoline",
           diesel         == 1 ~ "diesel",
         ))

if (!file.exists(  "./3_processed_data/fipe_price_clean_pt.csv")) {
  write_csv(fipe_price_clean_df,
            file = "./3_processed_data/fipe_price_clean_pt.csv")
} else {
  print("File already exists in the repository")
}

# Exporting a translated and more succint version of the table

fipe_price_clean_en <- fipe_price_13_22_df %>% 
  rename(manufacturer   = marca,
         model          = modelo,
         yearModel      = anoModelo,
         monthReference = mesReferencia,
         yearReference  = anoReferencia,
         priceNominal   = valor,
         priceDeflated  = def_price
         ) %>% 
  select(manufacturer, model, fuel,
         date,
         yearModel,
         monthReference,yearReference,
         priceNominal,priceDeflated)

if (!file.exists(  "./3_processed_data/fipe_price_clean_en.csv")) {
  write_csv(fipe_price_clean_df,
            file = "./3_processed_data/fipe_price_clean_en.csv")
} else {
  print("File already exists in the repository")
}

## 3.2. Individual data frames per fuel type -----------------------------------

# Individual data frames with lists of models and prices
# Prices correspond to each model's average price per year

### Electric -------------------------------------------------------------------

fipe_price_ev <- fipe_price_13_22_df %>% 
  filter(electric == 1) %>% 
  select(codigoFipe:valor, def_price) %>% 
  group_by(anoReferencia,anoModelo,modelo) %>% 
  summarize(nom_price = mean(valor),       # Including both nominal and deflated prices
            def_price = mean(def_price))

### Hybrid ---------------------------------------------------------------------

fipe_price_hybrid <- fipe_price_13_22_df %>% 
  filter(hybrid_ethanol == 1) %>% 
  select(codigoFipe:valor, def_price) %>% 
  group_by(anoReferencia,anoModelo,modelo) %>% 
  summarize(nom_price = mean(valor),       # Including both nominal and deflated prices
            def_price = mean(def_price))

### Gasoline  ------------------------------------------------------------------

fipe_price_gasoline <- fipe_price_13_22_df %>% 
  filter(gasoline == 1) %>% 
  select(codigoFipe:valor, def_price) %>% 
  group_by(anoReferencia,anoModelo,modelo) %>% 
  summarize(nom_price = mean(valor),       # Including both nominal and deflated prices
            def_price = mean(def_price))

### Diesel ---------------------------------------------------------------------

fipe_price_diesel <- fipe_price_13_22_df %>% 
  filter(diesel == 1) %>% 
  select(codigoFipe:valor, def_price) %>% 
  group_by(anoReferencia,anoModelo,modelo) %>% 
  summarize(nom_price = mean(valor),       # Including both nominal and deflated prices
            def_price = mean(def_price))


## 3.3. Summarized data frames -------------------------------------------------
### 3.3.1. Yearly trends -------------------------------------------------------

#### Individual dfs per fuel type ----------------------------------------------

# Price evolution (average, min, max, nr of models) per year
fipe_price_ev_trend <- fipe_price_13_22_df %>% 
  filter(electric == 1) %>% 
  select(codigoFipe:valor,def_price) %>% 
  mutate(date = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep=""))) %>% 
  group_by(anoReferencia) %>%
  summarize(avg_price_e     = mean(valor),
            avg_price_e_def = mean(def_price),
            min_price_e     = min(valor),
            max_price_e     = max(valor),
            nr_models_e     = n())

fipe_price_hybrid_trend <- fipe_price_13_22_df %>% 
  filter(hybrid_ethanol == 1) %>% 
  select(codigoFipe:valor) %>% 
  mutate(date = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep=""))) %>% 
  group_by(anoReferencia) %>%
  summarize(avg_price_h = mean(valor),
            min_price_h     = min(valor),
            max_price_h     = max(valor),
            nr_models_h     = n())

fipe_price_diesel_trend <- fipe_price_13_22_df %>% 
  filter(diesel == 1) %>% 
  select(codigoFipe:valor) %>% 
  mutate(date = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep=""))) %>% 
  group_by(anoReferencia) %>%
  summarize(avg_price_g = mean(valor),
            min_price_g     = min(valor),
            max_price_g     = max(valor),
            nr_models_g     = n())

fipe_price_gasoline_trend <- fipe_price_13_22_df %>% 
  filter(gasoline == 1) %>% 
  select(codigoFipe:valor) %>% 
  mutate(date = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep=""))) %>% 
  group_by(anoReferencia) %>%
  summarize(avg_price_g     = mean(valor),
            min_price_g     = min(valor),
            max_price_g     = max(valor),
            nr_models_g     = n())

years <- tibble(anoReferencia = 2013:2024)

#### Combined df with all fuels  -----------------------------------------------
##### Nominal values -----------------------------------------------------------

# Calculate trends for all fuel types (nominal value)
fipe_price_trends_nominal <- fipe_price_13_22_df %>%
  mutate(
    date = as_date(paste(anoReferencia, str_pad(mesReferencia, 2, pad = "0"), "01", sep = "-"))
  ) %>%
  group_by(anoReferencia, fuel_type = case_when(
    electric == 1 ~ "electric",
    hybrid_ethanol == 1 ~ "hybrid",
    gasoline == 1 ~ "gasoline",
  )) %>%
  drop_na() %>% 
  summarize(
    avg_price = mean(valor, na.rm = TRUE),
    min_price = min (valor, na.rm = TRUE),
    max_price = max (valor, na.rm = TRUE),
    nr_models = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = fuel_type,
    values_from = c(avg_price, min_price, max_price, nr_models),
    names_glue = "{.value}_{fuel_type}"
  ) %>%
  right_join(years, by = "anoReferencia") %>%
  arrange(anoReferencia) %>% 
  filter(anoReferencia < 2023)


##### Deflated values ----------------------------------------------------------

# Calculate trends for all fuel types (deflated value)
fipe_price_trends_deflated <- fipe_price_13_22_df %>%
  mutate(
    date = as_date(paste(anoReferencia, str_pad(mesReferencia, 2, pad = "0"), "01", sep = "-"))
  ) %>%
  group_by(anoReferencia, fuel_type = case_when(
    electric == 1 ~ "electric",
    hybrid_ethanol == 1 ~ "hybrid",
    gasoline == 1 ~ "gasoline",
  )) %>%
  drop_na() %>% 
  summarize(
    avg_price = mean(def_price, na.rm = TRUE),
    min_price = min (def_price, na.rm = TRUE),
    max_price = max (def_price, na.rm = TRUE),
    nr_models = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = fuel_type,
    values_from = c(avg_price, min_price, max_price, nr_models),
    names_glue = "{.value}_{fuel_type}"
  ) %>%
  right_join(years, by = "anoReferencia") %>%
  arrange(anoReferencia) %>% 
  filter(anoReferencia < 2023)



### 3.3.2. Monthly trends ------------------------------------------------------

#### Individual dfs per fuel type ----------------------------------------------

fipe_price_ev_trend_m <- fipe_price_13_22_df %>% 
  filter(electric == 1) %>% 
  select(codigoFipe:valor,def_price) %>% 
  mutate(date = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep=""))) %>% 
  group_by(date) %>%
  summarize(avg_price_e     = mean(valor),
            avg_price_e_def = mean(def_price),
            med_price_e     = median(valor),
            med_price_e_def = median(def_price),
            min_price_e     = min(valor),
            min_price_e_def = min(def_price),
            max_price_e     = max(valor),
            nr_models_e     = n())

fipe_price_gasoline_trend_m <- fipe_price_13_22_df %>% 
  filter(gasoline == 1) %>% 
  select(codigoFipe:valor,def_price) %>% 
  mutate(date = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep=""))) %>% 
  group_by(date) %>%
  summarize(avg_price_e     = mean(valor),
            avg_price_e_def = mean(def_price),
            med_price_e     = median(valor),
            med_price_e_def = median(def_price),
            min_price_e     = min(valor),
            min_price_e_def = min(def_price),
            max_price_e     = max(valor),
            nr_models_e     = n())

fipe_price_hybrid_trend_m <- fipe_price_13_22_df %>% 
  filter(hybrid_ethanol == 1) %>% 
  select(codigoFipe:valor,def_price) %>% 
  mutate(date = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep=""))) %>% 
  group_by(date) %>%
  summarize(avg_price_e     = mean(valor),
            avg_price_e_def = mean(def_price),
            med_price_e     = median(valor),
            med_price_e_def = median(def_price),
            min_price_e     = min(valor),
            min_price_e_def = min(def_price),
            max_price_e     = max(valor),
            nr_models_e     = n())


#### Combined df with all fuels  -----------------------------------------------

fipe_price_monthly_trends_deflated <- fipe_price_13_22_df %>%
  mutate(
    date = as_date(paste(anoReferencia, str_pad(mesReferencia, 2, pad = "0"), "01", sep = "-"))
  ) %>%
  group_by(date, fuel_type = case_when(
    electric == 1 ~ "electric",
    hybrid_ethanol == 1 ~ "hybrid",
    gasoline == 1 ~ "gasoline",
  )) %>%
  drop_na() %>% 
  summarize(
    mean_price   = mean(def_price, na.rm = TRUE),
    median_price = min (def_price, na.rm = TRUE),
    min_price    = min (def_price, na.rm = TRUE),
    max_price    = max (def_price, na.rm = TRUE),
    nr_models = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = fuel_type,
    values_from = c(mean_price, median_price, min_price, max_price, nr_models),
    names_glue = "{.value}_{fuel_type}"
  )

##### Creating the synthetic data for evs missing price values -----------------

# I took sources online that stated that the average value of evs in Brazil in
# 2013 was between 120 and 130k, and just assumed this value as constant for the
# missing data points. I then deflated them to keep all values comparable.

deflateBR::deflate(nominal_values = 130000, 
                   nominal_dates = as.Date("2018-01-01"),
                   real_date = "12/2022",
                   index = "inpc")

dates <- seq(as.Date("2013-01-01"), as.Date("2018-11-01"), by = "month")

# Create the initial dataframe with the date column
synth_ev_price <- data.frame(date = dates)

# Define the nominal value and real date
nominal_value <- 130000
real_date <- as.Date("2022-12-01")

# Calculate deflated values for each date
deflated_values <- sapply(dates, function(d) {
  deflateBR::deflate(nominal_value, 
                     nominal_date = d, 
                     real_date = "12/2022",
                     index = "ipca")
  })

# Add the deflated value columns to the dataframe
synth_ev_price$mean_price_electric   <- deflated_values
synth_ev_price$median_price_electric <- deflated_values
synth_ev_price$min_price_electric    <- deflated_values
synth_ev_price$max_price_electric    <- deflated_values

# Sliced the data to substitute the NA values
fipe_price_0to71   <- head(fipe_price_monthly_trends_deflated,71)
fipe_price_72to116 <- tail(fipe_price_monthly_trends_deflated,45)

fipe_price_0to71 <- fipe_price_0to71 %>%
  mutate(
    mean_price_electric   = coalesce(mean_price_electric,   synth_ev_price$mean_price_electric),
    median_price_electric = coalesce(median_price_electric, synth_ev_price$median_price_electric),
    min_price_electric    = coalesce(min_price_electric,    synth_ev_price$min_price_electric),
    max_price_electric    = coalesce(max_price_electric,    synth_ev_price$max_price_electric),
    nr_models_electric    = 1)

fipe_price_monthly_trends_deflated <- fipe_price_0to71 %>% 
  bind_rows(fipe_price_72to116)


if (!file.exists(  "./3_processed_data/fipe_price_monthly_trends_deflated.csv")) {
  write_csv(fipe_price_monthly_trends_deflated,
            file = "./3_processed_data/fipe_price_monthly_trends_deflated.csv")
} else {
  print("File already exists in the repository")
}


# 4. Exploring the data --------------------------------------------------------

## 4.1. Histograms of price distribution ---------------------------------------

### Electric -------------------------------------------------------------------

ggplot(fipe_price_ev, aes(x = log(def_price))) +
  geom_histogram(bins = 25, fill = "blue", alpha = 0.7) +
  facet_wrap(~ anoReferencia, ncol = 3) +
  scale_x_log10(
    labels = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Vehicle Price (log scale)",
    y = "Frequency",
    title = "Histogram of Vehicle Prices by Year (Log Scale)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Hybrid ---------------------------------------------------------------------

ggplot(fipe_price_hybrid, aes(x = valor)) +
  geom_histogram(bins = 25, fill = "blue", alpha = 0.7) +
  facet_wrap(~ anoReferencia, ncol = 3) +
  scale_x_log10(
    labels = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Vehicle Price (log scale)",
    y = "Frequency",
    title = "Histogram of Vehicle Prices by Year (Log Scale)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Gasoline  ------------------------------------------------------------------

ggplot(fipe_price_gasoline, aes(x = valor)) +
  geom_histogram(bins = 25, fill = "blue", alpha = 0.7) +
  facet_wrap(~ anoReferencia, ncol = 3) +
  scale_x_log10(
    labels = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Vehicle Price (log scale)",
    y = "Frequency",
    title = "Histogram of Vehicle Prices by Year (Log Scale)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 4.2. Line charts (yearly trends) ---------------------------------------------
### Average price (Nominal) ----------------------------------------------------

plot_trend_yearly_avg_prices_nominal <- 
  ggplot(fipe_price_trends_nominal, aes(x = anoReferencia)) +
  geom_line(aes(y = avg_price_electric, color = "Electric"), size = 1) +
  geom_line(aes(y = avg_price_hybrid,   color = "Hybrid"), size = 1) +
  geom_line(aes(y = avg_price_gasoline, color = "Gasoline"), size = 1) +
  scale_color_manual(
    name = "Fuel Type",
    values = c("Electric" = "blue", "Hybrid" = "green", "Gasoline" = "red")
  ) +
  scale_y_log10(
    breaks = c(10000, 20000, 50000, 100000, 200000, 500000), # Logarithmic progression
    labels = function(x) paste0("R$", format(x / 1000, big.mark = ".", decimal.mark = ","), " mil"),
    limits = c(10000, 600000)
  ) +
  scale_x_continuous(
    breaks = 2013:2022, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2022) # Convert to character for cleaner labels
  ) +
  labs(
    x = "Year",
    y = "Average Price (Nominal BRL)",
    title = "Evolution of Average Vehicle Prices by Fuel Type (2013–2022)",
    subtitle = "Source: Peixoto, 2022",
    caption = "\nData scrapped from the Fundação Instituto de Pesquisas Econômicas (FIPE)\ntable for average prices observed nationally in Brazil."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave("./4_plots/plot_trend_yearly_avg_prices_nominal.png",
       plot = plot_trend_yearly_avg_prices_nominal)


### Average price (Deflated) ---------------------------------------------------

plot_trend_yearly_avg_prices_deflated <- 
ggplot(fipe_price_trends_deflated, aes(x = anoReferencia)) +
  geom_line(aes(y = avg_price_electric, color = "Electric"), size = 1) +
  geom_line(aes(y = avg_price_hybrid,   color = "Hybrid"), size = 1) +
  geom_line(aes(y = avg_price_gasoline, color = "Gasoline"), size = 1) +
  scale_color_manual(
    name = "Fuel Type",
    values = c("Electric" = "blue", "Hybrid" = "green", "Gasoline" = "red")
  ) +
  scale_y_log10(
    breaks = c(10000, 20000, 50000, 100000, 200000, 500000), # Logarithmic progression
    labels = function(x) paste0("R$", format(x / 1000, big.mark = ".", decimal.mark = ","), " mil"),
    limits = c(10000, 600000)
  ) +
  scale_x_continuous(
    breaks = 2013:2022, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2022) # Convert to character for cleaner labels
  ) +
  labs(
    x = "Year",
    y = "Average Price (Deflated BRL)",
    title = "Evolution of Average Vehicle Prices by Fuel Type (2013–2022)",
    subtitle = "Source: Peixoto, 2022",
    caption = "\nData scrapped from the Fundação Instituto de Pesquisas Econômicas (FIPE)\ntable for average prices observed nationally in Brazil."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave("./4_plots/plot_trend_yearly_avg_prices_deflated.png",
       plot = plot_trend_yearly_avg_prices_deflated)


### Minimum price (Nominal) ---------------------------------------------------

plot_trend_yearly_min_prices_nominal <- 
  ggplot(fipe_price_trends_nominal, aes(x = anoReferencia)) +
  geom_line(aes(y = min_price_electric, color = "Electric"), size = 1) +
  geom_line(aes(y = min_price_hybrid,   color = "Hybrid"), size = 1) +
  geom_line(aes(y = min_price_gasoline, color = "Gasoline"), size = 1) +
  scale_color_manual(
    name = "Fuel Type",
    values = c("Electric" = "blue", "Hybrid" = "green", "Gasoline" = "red")
  ) +
  scale_y_log10(
    breaks = c(1000, 2500, 5000, 20000, 50000, 100000, 200000, 500000), # Logarithmic progression
    labels = function(x) paste0("R$", format(x / 1000, big.mark = ".", decimal.mark = ","), " mil"),
    limits = c(1000, 600000)
  ) +
  scale_x_continuous(
    breaks = 2013:2022, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2022) # Convert to character for cleaner labels
  ) +
  labs(
    x = "Year",
    y = "Lowest Average Price (Nominal BRL)",
    title = "Evolution of Minimum Value of Vehicle Prices by Fuel Type (2013–2022)",
    subtitle = "Source: Peixoto, 2022",
    caption = "\nData scrapped from the Fundação Instituto de Pesquisas Econômicas (FIPE)\ntable for average prices observed nationally in Brazil."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave("./4_plots/plot_trend_yearly_min_prices_nominal.png",
       plot = plot_trend_yearly_min_prices_nominal)

### Minimum price (Deflated) ---------------------------------------------------

plot_trend_yearly_min_prices_deflated <- 
  ggplot(fipe_price_trends_deflated, aes(x = anoReferencia)) +
  geom_line(aes(y = min_price_electric, color = "Electric"), size = 1) +
  geom_line(aes(y = min_price_hybrid,   color = "Hybrid"), size = 1) +
  geom_line(aes(y = min_price_gasoline, color = "Gasoline"), size = 1) +
  scale_color_manual(
    name = "Fuel Type",
    values = c("Electric" = "blue", "Hybrid" = "green", "Gasoline" = "red")
  ) +
  scale_y_log10(
    breaks = c(1000, 2500, 5000, 20000, 50000, 100000, 200000, 500000), # Logarithmic progression
    labels = function(x) paste0("R$", format(x / 1000, big.mark = ".", decimal.mark = ","), " mil"),
    limits = c(1000, 600000)
  ) +
  scale_x_continuous(
    breaks = 2013:2022, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2022) # Convert to character for cleaner labels
  ) +
  labs(
    x = "Year",
    y = "Lowest Average Price (Deflated BRL)",
    title = "Evolution of Average Vehicle Prices by Fuel Type (2013–2022)",
    subtitle = "Source: Peixoto, 2022",
    caption = "\nData scrapped from the Fundação Instituto de Pesquisas Econômicas (FIPE)\ntable for average prices observed nationally in Brazil."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave("./4_plots/plot_trend_yearly_min_prices_deflated.png",
       plot = plot_trend_yearly_min_prices_deflated)


### Number of models -----------------------------------------------------------

plot_trend_yearly_nr_models <- 
  ggplot(fipe_price_trends_nominal, aes(x = anoReferencia)) +
  geom_line(aes(y = nr_models_electric, color = "Electric"), size = 1) +
  geom_line(aes(y = nr_models_hybrid,   color = "Hybrid"), size = 1) +
  geom_line(aes(y = nr_models_gasoline, color = "Gasoline"), size = 1) +
  scale_color_manual(
    name = "Fuel Type",
    values = c("Electric" = "blue", "Hybrid" = "green", "Gasoline" = "red")
  ) + 
  scale_y_log10(
    breaks = c(1, 100,1000,10000,25000, 50000), # Logarithmic progression
    limits = c(1,50000)
  ) +
  scale_x_continuous(
    breaks = 2013:2022, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2022) # Convert to character for cleaner labels
  ) +
  labs(
    x = "Year",
    y = "Number of models",
    title = "Evolution of the number of models per year per fuel type (2013–2022)",
    subtitle = "Source: Peixoto, 2022",
    caption = "\nData scrapped from the Fundação Instituto de Pesquisas Econômicas (FIPE)\ntable for average prices observed nationally in Brazil."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave("./4_plots/plot_trend_yearly_nr_models.png",
       plot = plot_trend_yearly_nr_models)


## 4.2. Line charts (monthly trends) ---------------------------------------------
### Electric -------------------------------------------------------------------

plot_trend_monthly_ev_prices_deflated <- 
  ggplot(fipe_price_ev_trend_m, aes(x = date)) +
  geom_line(aes(y = avg_price_e,     color = "Mean Price"),   size = 1)+
  geom_line(aes(y = med_price_e,     color = "Median Price"), size = 1)+ 
  geom_line(aes(y = min_price_e_def, color = "Lowest Price"), size = 1)+
  scale_color_manual(
    name = "Price Indicator",
    values = c("Lowest Price" = "darkorange", "Mean Price" = "green3", "Median Price" = "blue2")
  ) +
  scale_y_continuous( 
    labels = function(x) paste0("R$", format(x / 1000, big.mark = ".", decimal.mark = ","), " mil")) +
  labs(
    x = "Year",
    y = "Average Deflated Yearly Price (BRL)",
    title = "Monthly Evolution of Vehicle Prices for Electric Cars (2018–2022)",
    subtitle = "Source: Peixoto, 2022",
    caption = "\nData scrapped from the Fundação Instituto de Pesquisas Econômicas (FIPE)\ntable for average prices observed nationally in Brazil."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )


ggsave("./4_plots/plot_trend_monthly_ev_prices_deflated.png",
       plot = plot_trend_monthly_ev_prices_deflated)

### Gasoline  ------------------------------------------------------------------

plot_trend_monthly_gasoline_prices_deflated <- 
  ggplot(fipe_price_gasoline_trend_m, aes(x = date)) +
  geom_line(aes(y = avg_price_e,     color = "Mean Price"),   size = 1)+
  geom_line(aes(y = med_price_e,     color = "Median Price"), size = 1)+ 
  geom_line(aes(y = min_price_e_def, color = "Lowest Price"), size = 1)+
  scale_color_manual(
    name = "Price Indicator",
    values = c("Lowest Price" = "darkorange", "Mean Price" = "green3", "Median Price" = "blue2")
  ) +
  scale_y_continuous( 
    labels = function(x) paste0("R$", format(x / 1000, big.mark = ".", decimal.mark = ","), " mil")) +
  labs(
    x = "Year",
    y = "Average Deflated Yearly Price (BRL)",
    title = "Monthly Evolution of Vehicle Prices for Cars (Gasoline) (2018–2022)",
    subtitle = "Source: Peixoto, 2022",
    caption = "\nData scrapped from the Fundação Instituto de Pesquisas Econômicas (FIPE)\ntable for average prices observed nationally in Brazil."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave("./4_plots/plot_trend_monthly_gasoline_prices_deflated.png",
       plot = plot_trend_monthly_gasoline_prices_deflated)

### Hybrid ---------------------------------------------------------------------

plot_trend_monthly_hybrid_prices_deflated <- 
  ggplot(fipe_price_hybrid_trend_m, aes(x = date)) +
  geom_line(aes(y = avg_price_e,     color = "Mean Price"),   size = 1)+
  geom_line(aes(y = med_price_e,     color = "Median Price"), size = 1)+ 
  geom_line(aes(y = min_price_e_def, color = "Lowest Price"), size = 1)+
  scale_color_manual(
    name = "Price Indicator",
    values = c("Lowest Price" = "darkorange", "Mean Price" = "green3", "Median Price" = "blue2")
  ) +
  scale_y_continuous( 
    labels = function(x) paste0("R$", format(x / 1000, big.mark = ".", decimal.mark = ","), " mil")) +
  labs(
    x = "Year",
    y = "Average Deflated Yearly Price (BRL)",
    title = "Monthly Evolution of Vehicle Prices for Cars (Hybrid/Ethanol) (2018–2022)",
    subtitle = "Source: Peixoto, 2022",
    caption = "\nData scrapped from the Fundação Instituto de Pesquisas Econômicas (FIPE)\ntable for average prices observed nationally in Brazil."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave("./4_plots/plot_trend_monthly_hybrid_prices_deflated.png",
       plot = plot_trend_monthly_hybrid_prices_deflated)



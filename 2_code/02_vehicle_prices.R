# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Downloaded data --------------------------------------------------------

fipe_price_raw_df <- read_delim("1_raw_data/5_vehicle_prices/tabela-fipe-historico-precos.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


# 3. Data cleaning and organizing ----------------------------------------------

# Whole data frame with tagging of cars per engine traction / fuel type

fipe_price_13_22_df <- fipe_price_raw_df %>% 
  filter(anoReferencia > 2012) %>% 
  mutate(
    diesel = as.integer(str_detect(tolower(modelo), "diesel")),
    electric = as.integer(str_detect(tolower(modelo), "eletric|elétrico| kw ")),
    hybrid_ethanol = as.integer(str_detect(tolower(modelo), "flex")),
    gasoline = as.integer(!(diesel | electric | hybrid_ethanol))
  ) %>% 
  mutate(mesReferencia = str_pad(mesReferencia, width = 2, pad = "0"))


# Individual data frames with lists of models and prices
# Prices correspond to each model's average price per year

fipe_price_ev <- fipe_price_13_22_df %>% 
  filter(electric == 1) %>% 
  select(codigoFipe:valor) %>% 
  group_by(anoReferencia,anoModelo,modelo) %>% 
  summarize(valor = mean(valor))

fipe_price_hybrid <- fipe_price_13_22_df %>% 
  filter(hybrid_ethanol == 1) %>% 
  select(codigoFipe:valor) %>% 
  group_by(anoReferencia,anoModelo,modelo) %>% 
  summarize(valor = mean(valor))

fipe_price_gasoline <- fipe_price_13_22_df %>% 
  filter(gasoline == 1) %>% 
  select(codigoFipe:valor) %>% 
  group_by(anoReferencia,anoModelo,modelo) %>% 
  summarize(valor = mean(valor))

fipe_price_diesel <- fipe_price_13_22_df %>% 
  filter(diesel == 1) %>% 
  select(codigoFipe:valor) %>% 
  group_by(anoReferencia,anoModelo,modelo) %>% 
  summarize(valor = mean(valor))


# Price evolution (average, min, max, nr of models) per year

fipe_price_ev_trend <- fipe_price_13_22_df %>% 
  filter(electric == 1) %>% 
  select(codigoFipe:valor) %>% 
  mutate(date = lubridate::as_date(paste(anoReferencia, mesReferencia,"01", sep=""))) %>% 
  group_by(anoReferencia) %>%
  summarize(avg_price_e = mean(valor),
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



# Calculate trends for all fuel types
fipe_price_trends <- fipe_price_13_22_df %>%
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
    min_price = min(valor, na.rm = TRUE),
    max_price = max(valor, na.rm = TRUE),
    nr_models = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = fuel_type,
    values_from = c(avg_price, min_price, max_price, nr_models),
    names_glue = "{.value}_{fuel_type}"
  ) %>%
  right_join(years, by = "anoReferencia") %>%
  arrange(anoReferencia)
         



# 4. Exploring the data --------------------------------------------------------


ggplot(fipe_price_ev, aes(x = valor)) +
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


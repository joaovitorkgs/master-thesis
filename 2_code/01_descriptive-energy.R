# 1. Packages and raw dataframes -- --------------------------------------------

source("./2_code/00_packages.R")


# 2. Raw Dataframes-------------------------------------------------------------

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

df_electricity <- read_sql(query, billing_project_id = get_billing_id())

# Adjusted main dataframe to include date information

df_electricity_date <- df_electricity %>% 
  mutate(
    mes = as.numeric(as.character(mes)),    # Convert 'mes' to numeric if it's not
    mes = sprintf("%02d", mes),             # Format 'mes' as double digits
    ano_mes = paste(ano, mes, sep = "-"),   # Combine 'ano' and 'mes'
    ano_mes_date = as.Date(paste(ano, mes, "01", sep = "-")) # Create date column
  )

# 3. Electricity Consumption ---------------------------------------------------

# 3.1. Aggregate Electricity consumption

electricity_residential <- df_electricity_date %>%
  group_by(ano_mes) %>%
  summarise(total = sum(consumo))



plot_trend_ev_agg <- ggplot(yearly_results_aggregate, aes(x = as.integer(year), y = Electric)) +
  geom_line(color = "#69b3a2", linewidth = 2) +
  geom_point(size = 3, color = "#69b3a2") +
  theme_bw()+
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles",
    title = "Evolution of Electric Vehicles in Brazil",
    subtitle = "Source: National Traffic Secretariat, 2024")





# Ideas for the way forward:

# Idea 1:
# Use tables on population size for the states base for the year in question for
# comparation of states which consume the most electricity in total and per capita

# Idea 2:
# Describe how tax collection works for electricity in States
# Estimate electricity tax income





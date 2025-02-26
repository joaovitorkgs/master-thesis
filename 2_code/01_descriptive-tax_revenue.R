# 1. Packages and raw dataframes -- --------------------------------------------

source("./2_code/00_packages.R")


# 2. Raw Dataframes-------------------------------------------------------------

# 2.1. Files from BigQuery -----------------------------------------------------

# Federal 

query_federal_cnae <- "
SELECT
    dados.ano as ano,
    dados.mes as mes,
    dados.secao_sigla as secao_sigla,
    dados.imposto_importacao as imposto_importacao,
    dados.imposto_exportacao as imposto_exportacao,
    dados.ipi as ipi,
    dados.irpf as irpf,
    dados.irpj as irpj,
    dados.irrf as irrf,
    dados.iof as iof,
    dados.itr as itr,
    dados.cofins as cofins,
    dados.pis_pasep as pis_pasep,
    dados.csll as csll,
    dados.cide_combustiveis as cide_combustiveis,
    dados.contribuicao_previdenciaria as contribuicao_previdenciaria,
    dados.cpsss as cpsss,
    dados.pagamento_unificado as pagamento_unificado,
    dados.outras_receitas_rfb as outras_receitas_rfb,
    dados.demais_receitas as demais_receitas
FROM `basedosdados.br_rf_arrecadacao.cnae` AS dados
"

df_federal_cnae <- read_sql(query_federal_cnae, billing_project_id = get_billing_id())

query_federal_estados <- "
SELECT
    dados.ano as ano,
    dados.mes as mes,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.imposto_importacao as imposto_importacao,
    dados.imposto_exportacao as imposto_exportacao,
    dados.ipi_fumo as ipi_fumo,
    dados.ipi_bebidas as ipi_bebidas,
    dados.ipi_automoveis as ipi_automoveis,
    dados.ipi_importacoes as ipi_importacoes,
    dados.ipi_outros as ipi_outros,
    dados.irpf as irpf,
    dados.irpj_entidades_financeiras as irpj_entidades_financeiras,
    dados.irpj_demais_empresas as irpj_demais_empresas,
    dados.irrf_rendimentos_trabalho as irrf_rendimentos_trabalho,
    dados.irrf_rendimentos_capital as irrf_rendimentos_capital,
    dados.irrf_remessas_exterior as irrf_remessas_exterior,
    dados.irrf_outros_rendimentos as irrf_outros_rendimentos,
    dados.iof as iof,
    dados.itr as itr,
    dados.ipmf as ipmf,
    dados.cpmf as cpmf,
    dados.cofins as cofins,
    dados.cofins_entidades_financeiras as cofins_entidades_financeiras,
    dados.cofins_demais_empresas as cofins_demais_empresas,
    dados.pis_pasep as pis_pasep,
    dados.pis_pasep_entidades_financeiras as pis_pasep_entidades_financeiras,
    dados.pis_pasep_demais_empresas as pis_pasep_demais_empresas,
    dados.csll as csll,
    dados.csll_entidades_financeiras as csll_entidades_financeiras,
    dados.csll_demais_empresas as csll_demais_empresas,
    dados.cide_combustiveis_parcela_nao_dedutivel as cide_combustiveis_parcela_nao_dedutivel,
    dados.cide_combustiveis as cide_combustiveis,
    dados.cpsss_1 as cpsss_1,
    dados.cpsss_2 as cpsss_2,
    dados.contribuicao_fundaf as contribuicao_fundaf,
    dados.refis as refis,
    dados.paes as paes,
    dados.retencoes_fonte as retencoes_fonte,
    dados.pagamento_unificado as pagamento_unificado,
    dados.outras_receitas_rfb as outras_receitas_rfb,
    dados.demais_receitas as demais_receitas,
    dados.receita_previdenciaria as receita_previdenciaria,
    dados.receita_previdenciaria_propria as receita_previdenciaria_propria,
    dados.receita_previdenciaria_demais as receita_previdenciaria_demais,
    dados.receitas_outros_orgaos as receitas_outros_orgaos
FROM `basedosdados.br_rf_arrecadacao.uf` AS dados
LEFT JOIN (SELECT DISTINCT sigla,nome  FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
"

df_federal_estados <- read_sql(query_federal_estados, billing_project_id = get_billing_id())



# Para carregar o dado direto no R
query_federal_natjuridica <- "
SELECT
    dados.ano as ano,
    dados.mes as mes,
    dados.natureza_juridica_codigo AS natureza_juridica_codigo,
    diretorio_natureza_juridica_codigo.descricao AS natureza_juridica_codigo_descricao,
    dados.imposto_importacao as imposto_importacao,
    dados.imposto_exportacao as imposto_exportacao,
    dados.ipi as ipi,
    dados.irpf as irpf,
    dados.irpj as irpj,
    dados.irrf as irrf,
    dados.iof as iof,
    dados.itr as itr,
    dados.cofins as cofins,
    dados.pis_pasep as pis_pasep,
    dados.csll as csll,
    dados.cide_combustiveis as cide_combustiveis,
    dados.contribuicao_previdenciaria as contribuicao_previdenciaria,
    dados.cpsss as cpsss,
    dados.pagamento_unificado as pagamento_unificado,
    dados.outras_receitas_rfb as outras_receitas_rfb,
    dados.demais_receitas as demais_receitas
FROM `basedosdados.br_rf_arrecadacao.natureza_juridica` AS dados
LEFT JOIN (SELECT DISTINCT id_natureza_juridica,descricao  FROM `basedosdados.br_bd_diretorios_brasil.natureza_juridica`) AS diretorio_natureza_juridica_codigo
    ON dados.natureza_juridica_codigo = diretorio_natureza_juridica_codigo.id_natureza_juridica
"

df_federal_natjuridica <- read_sql(query_federal_natjuridica, billing_project_id = get_billing_id())


# 2.2. Downloaded Files -- -----------------------------------------------------

# Federal

# Tax revenue per State

url_state_sc_municipality <- "https://dados.sc.gov.br/dataset/0eaade9e-98b9-4887-865d-096ccf142f1b/resource/eec848ff-67db-4a8d-9bc6-101e8573c1e2/download/icms_ipva.csv"

download.file(url      = url_state_sc_municipality,
              destfile = "./1_raw_data/1_tax_revenue/2022_SC_tax_revenue_municipality.csv",
              quite    = FALSE)

df_state_sc_municipality <- read.csv("./1_raw_data/1_tax_revenue/2022_SC_tax_revenue_municipality.csv")



# State (Santa Catarina) 

# Tax revenue per Municipality

url_state_sc_municipality <- "https://dados.sc.gov.br/dataset/0eaade9e-98b9-4887-865d-096ccf142f1b/resource/eec848ff-67db-4a8d-9bc6-101e8573c1e2/download/icms_ipva.csv"

download.file(url      = url_state_sc_municipality,
              destfile = "./1_raw_data/1_tax_revenue/2022_SC_tax_revenue_municipality.csv",
              quite    = FALSE)

df_state_sc_municipality <- read.csv("./1_raw_data/1_tax_revenue/2022_SC_tax_revenue_municipality.csv")


# Tax revenue per Economic Sector

url_state_sc_sector <- "https://dados.sc.gov.br/dataset/c4eec6b6-c07f-4c67-b21d-3827ed0da90f/resource/bc4a904c-05ad-440c-b4b8-0f081b22d296/download/setor_economico.csv"

download.file(url      = url_state_sc_sector,
              destfile = "./1_raw_data/1_tax_revenue/2022_SC_tax_revenue_sector.csv",
              quite    = FALSE)

df_state_sc_sector <- read.csv("./1_raw_data/1_tax_revenue/2022_SC_tax_revenue_sector.csv")

sc_sectors <- unique(df_state_sc_sector$DE_SETOR_ECONOMICO)


# 2.3. Hard-coded data ---------------------------------------------------------

# Manual data on economic sectors

cnae_sector_names <- data.frame(
  secao_sigla = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U"),
  subdivisions = c("01 .. 03", "05 .. 09", "10 .. 33", "35 .. 35", "36 .. 39", "41 .. 43", "45 .. 47", "49 .. 53", "55 .. 56", "58 .. 63", "64 .. 66", "68 .. 68", "69 .. 75", "77 .. 82", "84 .. 84", "85 .. 85", "86 .. 88", "90 .. 93", "94 .. 96", "97 .. 97", "99 .. 99"),
  Denominacao = c(
    "AGRICULTURA, PECUÁRIA, PRODUÇÃO FLORESTAL, PESCA E AQÜICULTURA",
    "INDÚSTRIAS EXTRATIVAS",
    "INDÚSTRIAS DE TRANSFORMAÇÃO",
    "ELETRICIDADE E GÁS",
    "ÁGUA, ESGOTO, ATIVIDADES DE GESTÃO DE RESÍDUOS E DESCONTAMINAÇÃO",
    "CONSTRUÇÃO",
    "COMÉRCIO; REPARAÇÃO DE VEÍCULOS AUTOMOTORES E MOTOCICLETAS",
    "TRANSPORTE, ARMAZENAGEM E CORREIO",
    "ALOJAMENTO E ALIMENTAÇÃO",
    "INFORMAÇÃO E COMUNICAÇÃO",
    "ATIVIDADES FINANCEIRAS, DE SEGUROS E SERVIÇOS RELACIONADOS",
    "ATIVIDADES IMOBILIÁRIAS",
    "ATIVIDADES PROFISSIONAIS, CIENTÍFICAS E TÉCNICAS",
    "ATIVIDADES ADMINISTRATIVAS E SERVIÇOS COMPLEMENTARES",
    "ADMINISTRAÇÃO PÚBLICA, DEFESA E SEGURIDADE SOCIAL",
    "EDUCAÇÃO",
    "SAÚDE HUMANA E SERVIÇOS SOCIAIS",
    "ARTES, CULTURA, ESPORTE E RECREAÇÃO",
    "OUTRAS ATIVIDADES DE SERVIÇOS",
    "SERVIÇOS DOMÉSTICOS",
    "ORGANISMOS INTERNACIONAIS E OUTRAS INSTITUIÇÕES EXTRATERRITORIAIS"),
    sectors_en = c(
      "Agriculture",
      "Extractive industries",
      "Manufacturing industries",
      "Electricity and gas",
      "Water and waste management",
      "Construction",
      "Trade; repair of vehicles",
      "Transport, storage and mail",
      "Accommodation and food",
      "Information and communication",
      "Financial and insurance services",
      "Real estate activities",
      "Scientific and technical activities",
      "Administrative activities",
      "Public administration",
      "Education",
      "Human health and social services",
      "Arts, culture, sports & recreation",
      "Other service activities",
      "Domestic services",
      "International organizationss"
    )) %>% 
  mutate(sectors_en = paste0(secao_sigla, sep = " - ", sectors_en))



# Manual data on tax rates per state

# Gasoline

tax_rates_gasoline <- data.frame(
  Estado = c('AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA',
               'MG', 'MS', 'MT', 'PA', 'PB', 'PE', 'PI', 'PR', 'RJ', 'RN',
               'RO', 'RR', 'RS', 'SC', 'SE', 'SP', 'TO'),
  CIDE_Gaso_A = rep(0.1, 27),
  PIS_COFINS_Gaso_A = rep(0.7925, 27),
  PIS_COFINS_AEAC = rep(0.1309, 27),
  AEAC_Mistura = rep('27%', 27),
  CIDE_PIS_COF_Gaso_C = rep(0.6869, 27),
  ICMS = rep(1.47, 27),
  Total_Gaso_A = rep(2.157, 27)
)

tax_rates_ethanol <- data.frame(
  Estado = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT*", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  PIS_COFINS_produtor = rep(0.1309, 27),
  PIS_COFINS_distribuidora = rep(0.1109, 27),
  PIS_COFINS_etanol = rep(0.2418, 27),
  ICMS_percent = c(19.00, 21.00, 20.00, 18.00, 12.86, 20.00, 13.00, 17.00, 14.17, 22.00, 13.08, 11.33, 17.00, 16.96, 15.33, 15.52, 14.90, 12.00, 16.87, 15.33, 17.50, 20.00, 17.00, 17.00, 19.00, 12.00, 20.00),
  PMPF_01_02 = c(5.1203, 4.8097, 4.9639, 5.2900, 4.5900, 4.8696, 4.4000, 4.4095, 4.2630, 4.6300, 4.3529, 4.0526, 4.0601, 4.7045, 3.9832, 4.3800, 4.4000, 4.2766, 4.4900, 4.4500, 5.0870, 4.9500, 4.5116, 4.4471, 4.5410, 3.6700, 4.7400),
  ICMS_total = c(0.9729, 1.0100, 0.9928, 0.9522, 0.5903, 0.9739, 0.5720, 0.7496, 0.6041, 1.0186, 0.5694, 0.4592, 0.3451, 0.7979, 0.6106, 0.6798, 0.6556, 0.5132, 0.7575, 0.6822, 0.8902, 0.9900, 0.7670, 0.7560, 0.8628, 0.4404, 0.9480),
  Total_Ethanol = c(1.215, 1.252, 1.235, 1.194, 0.832, 1.216, 0.814, 0.991, 0.846, 1.260, 0.811, 0.701, 0.587, 1.040, 0.852, 0.922, 0.897, 0.755, 0.999, 0.924, 1.132, 1.232, 1.009, 0.998, 1.105, 0.682, 1.190)
)

tax_rates_diesel_S500 <- data.frame(
  Estado = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  CIDE_Diesel_A = rep(0.0000, 27),
  PIS_COFINS_biodiesel = rep(0.3515, 27),
  biodiesel_mistura = rep(0.1480, 27),
  biodiesel_mistura_percent = rep(14, 27),
  cide_pis_cofins_diesel_S_500 = rep(0.3230, 27),
  ICMS_Total = rep(1.1200, 27),
  Total_DieselS500 = rep(1.443, 27)
)

tax_rates_diesel_S10 <- data.frame(
  Estado = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  CIDE_Diesel_A = rep(0.0000, 27),
  PIS_COFINS_biodiesel = rep(0.3515, 27),
  biodiesel_mistura = rep(0.1480, 27),
  biodiesel_mistura_percent = rep(14, 27),
  cide_pis_cofins_diesel_S_10 = rep(0.3230, 27),
  ICMS_Total = rep(1.1200, 27),
  Total_DieselS10 = rep(1.443, 27)
)

tax_rates_gasoline_premium <- data.frame(
  Estado = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  CIDE_GasoA_Pr = rep(0.1000, 27),
  PIS_COFINS_GasoA_Pr = rep(0.7925, 27),
  PIS_COFINS_AEAC = rep(0.1309, 27),
  AEAC_Mistura_percent = rep(25, 27),
  CIDE_PIS_COF_Gaso_A_Pr = rep(0.7021, 27),
  ICMS_Total = rep(1.4700, 27),
  Total_Gaso_A_Pr = rep(2.172, 27)
)

value_gasoline <- tax_rates_gasoline %>% 
  mutate(All = 1) %>% 
  group_by(All) %>% 
  summarise(
    CIDE_Gaso_A        = mean(CIDE_Gaso_A),
    PIS_COFINS_Gaso_A  = mean(PIS_COFINS_Gaso_A),
    ICMS               = mean(ICMS))

value_diesel <- tax_rates_diesel_S10 %>% 
  mutate(All = 1) %>% 
  group_by(All) %>% 
  summarise(
    CIDE_Diesel_A         = mean(CIDE_Diesel_A),
    PIS_COFINS_biodiesel  = mean(PIS_COFINS_biodiesel),
    ICMS                  = mean(ICMS_Total))

value_ethanol <- tax_rates_ethanol %>% 
  mutate(All = 1) %>% 
  group_by(All) %>% 
  summarise(
    PIS_COFINS_etanol     = mean(PIS_COFINS_etanol),
    ICMS                  = mean(ICMS_total))


fuel_tax_data <- data.frame(
  Fuel_Type = rep(c("Gasoline", "Diesel", "Ethanol"), each = 3),
  Tax_Type = rep(c("Gasoline Tax", "Social Contributions", 
                   "Tax on Circulation of Goods and Services"), 3),
  Rate_BRL_per_L = c(0.100, 0.792, 1.470,
                     0.000, 0.3515, 1.120,
                     0.000, 0.242, 0.747))


barplot_fuel_tax_rates <- ggplot(fuel_tax_data, aes(x = Rate_BRL_per_L, y = Tax_Type, fill = Fuel_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.3f", Rate_BRL_per_L)), 
            position = position_dodge(width = 0.9),
            hjust = -0.2,
            size = 3) +  # Reduced text size here
  labs(title = "Nominal value of tax paid on fuel sales",
       subtitle = "Source: Brazilian National Federation of Fuel Retail (Fecombustiveis), 2025",
       x = "Rate (BRL per L)",
       y = "Tax Type",
       fill = "Fuel Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Diesel" = "#4E79A7", 
                               "Ethanol" = "#F28E2B", 
                               "Gasoline" = "#59A14F")) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.title.position = "plot") +
  expand_limits(x = max(fuel_tax_data$Rate_BRL_per_L) * 1.2)

ggsave("./4_plots/barplot_fuel_tax_rates.png",
       plot = barplot_fuel_tax_rates)



# 3. Tax Revenue analysis ------------------------------------------------------

# 3.1. Federal Tax Revenue per Economic Sector -----------------------------------------------------


# Cleaning the original data set 

df_federal_cnae_date <- df_federal_cnae %>% 
  mutate(
    mes = as.numeric(as.character(mes)),    # Convert 'mes' to numeric if it's not
    mes = sprintf("%02d", mes),             # Format 'mes' as double digits
    ano_mes = paste(ano, mes, sep = "-"),   # Combine 'ano' and 'mes'
    ano_mes_date = as.Date(paste(ano, mes, "01", sep = "-")) # Create date column
  ) 



# 3.1.1 All fuel taxes levied on fuel-related activities -----------------------

# Selecting all taxes which are levied on fuel sales (as per Esteves, 2020, p.6)

federal_cnae_fuel_alltaxes <- df_federal_cnae_date %>% 
  select(ano,                    # Year
         ano_mes_date,           # Year and month in date format
         secao_sigla,            # Economic sector classification
         imposto_importacao,     # Import tax
         pis_pasep,              # Tax to fund social benefits
         imposto_exportacao,     # Export tax
         cofins,                 # Tax to fund social securite
         cide_combustiveis) %>%  # Tax on fuel to fund infrastructure and fuel subsidies 
  rename(ii = imposto_importacao,
         ie = imposto_exportacao) %>% 
  replace(is.na(.),0) %>% 
  mutate(fuel_taxes = ii + pis_pasep + ie + cofins + cide_combustiveis)
         

# Data frames for specific sectors (all federal taxes levied on fuel-related economic activities)

federal_gasstation_fueltaxes <- federal_cnae_fuel_alltaxes %>% 
  select(ano_mes_date, secao_sigla, fuel_taxes) %>%  
  filter(secao_sigla != "G") %>%  # Keeping only firms related to commercializing fuel
  group_by(ano_mes_date) %>% 
  summarise(fuel_taxes = sum(fuel_taxes, na.rm = TRUE))

if (!file.exists("./3_processed_data/taxrev_federal_gasstation_fueltaxes.csv")) {
  write_csv(federal_gasstation_fueltaxes,
            file = "./3_processed_data/taxrev_federal_gasstation_fueltaxes.csv")
} else {
  print("File already exists in the repository")
}




federal_energysector_fueltaxes <- federal_cnae_fuel_alltaxes %>% 
  select(ano_mes_date, secao_sigla, fuel_taxes) %>%  
  filter(secao_sigla != "D") %>%  # Removing all registered companies not in the energy sector
  group_by(ano_mes_date) %>% 
  summarise(fuel_taxes = sum(fuel_taxes, na.rm = TRUE))


if (!file.exists("./3_processed_data/taxrev_federal_energysector_fueltaxes.csv")) {
  write_csv(federal_energysector_fueltaxes,
            file = "./3_processed_data/taxrev_federal_energysector_fueltaxes.csv")
} else {
  print("File already exists in the repository")
}


federal_allsectors_fueltaxes <- federal_cnae_fuel_alltaxes %>% 
  select(ano, ano_mes_date, secao_sigla, fuel_taxes) %>%  
  group_by(ano_mes_date, secao_sigla) %>% 
  summarise(fuel_taxes = sum(fuel_taxes, na.rm = TRUE))

federal_allsectors_fueltaxes <- federal_cnae_fuel_alltaxes %>% 
  left_join(cnae_sector_names, by = "secao_sigla") %>% 
  select(ano_mes_date, secao_sigla, sectors_en, fuel_taxes) %>% 
  drop_na()

if (!file.exists("./3_processed_data/taxrev_federal_allsectors_fueltaxes.csv")) {
  write_csv(federal_allsectors_fueltaxes,
            file = "./3_processed_data/taxrev_federal_allsectors_fueltaxes.csv")
} else {
  print("File already exists in the repository")
}


federal_allsectors_unpivot <- federal_cnae_fuel_alltaxes %>% 
  select(-ano_mes_date) %>% 
  group_by(ano) %>% 
  summarise(
    pis_pasep         = sum(pis_pasep),
    ii                = sum(ii),
    ie                = sum(ie),
    cofins            = sum(cofins),
    cide_combustiveis = sum(cide_combustiveis),
    fuel_taxes        = sum(fuel_taxes)) %>% 
  pivot_longer(
    cols = c(pis_pasep, ii, ie, cofins, cide_combustiveis, fuel_taxes),
    names_to = "tax_type",
    values_to = "value") 

federal_allsectors_unpivot <- federal_allsectors_unpivot %>%
  filter(tax_type != "fuel_taxes") %>% 
  mutate(tax_type = case_when(
    tax_type == "pis_pasep" ~ "PIS/PASEP",
    tax_type == "ii"        ~ "Import Tax",
    tax_type == "ie"        ~ "Export Tax",
    tax_type == "cofins"    ~ "COFINS",
    tax_type == "cide_combustiveis"    ~ "CIDE Combustíveis",
    TRUE ~ tax_type # Keep original value if no match
  )) %>% 
  mutate(ano = as.Date(paste(ano, "01", "01", sep = "-")))

if (!file.exists("./3_processed_data/taxrev_federal_allsectors_unpivot.csv")) {
  write_csv(federal_allsectors_unpivot,
            file = "./3_processed_data/taxrev_federal_allsectors_unpivot.csv")
} else {
  print("File already exists in the repository")
}



# 3.1.2. CIDE Combustíveis only ------------------------------------------------


# Data frames for specific sectors ("CIDE Combustíveis" only)

federal_CIDE_gasstation <- df_federal_cnae_date %>% 
  select(ano_mes_date, ano, mes, secao_sigla, cide_combustiveis) %>%  # "cide_combustiveis" refers to a specific tax paid for fuel
  filter(secao_sigla != "G") %>%  # Removing all registered companies not in the energy sector
  mutate(cide_combustiveis = as.numeric(as.character(cide_combustiveis))) %>% 
  group_by(ano_mes_date) %>% 
  summarise(`CIDE Combustíveis` = sum(cide_combustiveis, na.rm = TRUE))

federal_CIDE_energysector <- df_federal_cnae_date %>% 
  select(ano_mes_date, ano, mes, secao_sigla, cide_combustiveis) %>%  # "cide_combustiveis" refers to a specific tax paid for fuel
  filter(secao_sigla != "D") %>%  # Removing all registered companies not in the energy sector
  mutate(cide_combustiveis = as.numeric(as.character(cide_combustiveis))) %>% 
  group_by(ano_mes_date) %>% 
  summarise(`CIDE Combustíveis` = sum(cide_combustiveis, na.rm = TRUE))

federal_CIDE_allsectors <- df_federal_cnae_date %>% 
  select(ano_mes_date, ano, mes, secao_sigla, cide_combustiveis) %>%  # "cide_combustiveis" refers to a specific tax paid for fuel
  mutate(cide_combustiveis = as.numeric(as.character(cide_combustiveis))) %>% 
  group_by(ano_mes_date, secao_sigla) %>% 
  summarise(`CIDE Combustíveis` = sum(cide_combustiveis, na.rm = TRUE))

federal_CIDE_allsectors <- federal_CIDE_allsectors %>% 
  left_join(cnae_sector_names, by = "secao_sigla") %>% 
  select(ano_mes_date, secao_sigla, sectors_en, `CIDE Combustíveis`) %>% 
  drop_na() 




# 4. Plots ---------------------------------------------------------------------

# 4.1. Federal Tax Revenue -----------------------------------------------------

# 4.1.1. All fuel taxes combined -----------------------------------------------

# Economic Sector D (Electricity and Gas)

plot_trend_fed_revenue_alltaxes_electgas <- ggplot(federal_energysector_fueltaxes,
                                               aes(x = ano_mes_date, 
                                                   y = (fuel_taxes/1000000000))) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Tax Revenue per Month (Billion BRL)",
    title = "Total Federal Tax Revenue levied on Fuel-Related Economic Activities",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024",
    caption = "Note: Data related to firms registered under D category only, which refers to the electricity and gas sector."
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    plot.caption = element_text(hjust = 0))

ggsave("./4_plots/plot_trend_fed_revenue_alltaxes_electgas.png",
       plot = plot_trend_fed_revenue_alltaxes_electgas,
       units = "in")


# Economic Sector G (includes gas stations)

plot_trend_fed_revenue_alltaxes_gasstation <- ggplot(federal_gasstation_fueltaxes, 
                                                 aes(x = ano_mes_date, 
                                                     y = fuel_taxes/1000000000)) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Tax Revenue per Month (Billion BRL)",
    title = "Total Federal Tax Revenue levied on Fuel-Related Economic Activities",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024",
    caption = "Note: Data related to firms registered under G category only, which includes gas stations and retail sale of fuels."
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    plot.caption = element_text(hjust = 0)
    
  )

ggsave("./4_plots/plot_trend_fed_revenue_alltaxes_gasstation.png",
       plot = plot_trend_fed_revenue_alltaxes_gasstation,
       units = "in")



# All sectors

plot_trend_fed_revenue_alltaxes_groups <- ggplot(federal_allsectors_fueltaxes,
                                             aes(x = ano_mes_date, 
                                                 y = fuel_taxes)) +
  geom_line(linewidth = 0.7) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(trans = "log10",
                     breaks = c(1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11),
                     labels = c("100k", "1M", "10M", "100M", "1B", "10B", "100B"))+
  labs(
    x = "Year",
    y = "Total Tax Revenue per Month (BRL) - log transformed",
    title = "Total Federal Tax Revenue levied on Fuel-Related Economic Activities",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~ sectors_en, nrow = 6)


ggsave("./4_plots/plot_trend_fed_revenue_alltaxes_groups.png",
       plot = plot_trend_fed_revenue_alltaxes_groups,
       units = "in",
       width = 10,
       height = 10)

# All sectors split by tax type

plot_trend_fed_revenue_alltaxes_taxtype_panels <- ggplot(federal_allsectors_unpivot,
                                                 aes(x = ano, 
                                                     y = value)) +
  geom_line(linewidth = 0.7) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(trans = "log10",
                     breaks = c(1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11),
                     labels = c("100k", "1M", "10M", "100M", "1B", "10B", "100B"))+
  labs(
    x = "Year",
    y = "Total Tax Revenue per Month (BRL) - log transformed",
    title = "Total Federal Tax Revenue levied on Fuel-Related Economic Activities",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024",
    caption = "Note: COFINS and PIS/PASEP are taxes levied to fund public social security and benefits. CIDE Combustíveis \nis a tax for fuel-related economic activities only, used to fund infrastructure and fuel subsidies."
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0)) +
  facet_wrap(~ tax_type, nrow = 2)

ggsave("./4_plots/plot_trend_fed_revenue_alltaxes_taxtype_panels.png",
       plot = plot_trend_fed_revenue_alltaxes_taxtype_panels)


plot_trend_fed_revenue_alltaxes_taxtype_1panel <- ggplot(federal_allsectors_unpivot,
                                                  aes(x = ano, 
                                                      y = value,
                                                      color = tax_type)) +  # Add color aesthetic
  geom_line(linewidth = 1) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(trans = "log10",
                     breaks = c(1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11),
                     labels = c("100k", "1M", "10M", "100M", "1B", "10B", "100B")) +
  labs(
    x = "Year",
    y = "Total Tax Revenue per Month (BRL) - log transformed",
    title = "Total Federal Tax Revenue levied on Fuel-Related Economic Activities",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024",
    color = "Tax Type",
    caption = "Note: COFINS and PIS/PASEP are taxes levied to fund public social security and benefits. CIDE Combustíveis \nis a tax for fuel-related economic activities only, used to fund infrastructure and fuel subsidies."
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",  
    plot.caption = element_text(hjust = 0)) +
    scale_color_brewer(palette = "Set2")  #

ggsave("./4_plots/plot_trend_fed_revenue_alltaxes_taxtype_1panel.png",
       plot = plot_trend_fed_revenue_alltaxes_taxtype_1panel)



# 4.1.2. CIDE Combustiveis -----------------------------------------------------

# Economic Sector D (Electricity and Gas)

plot_trend_fed_revenue_CIDE_electgas <- ggplot(federal_CIDE_energysector, 
                                                 aes(x = ano_mes_date, 
                                                     y = (`CIDE Combustíveis`)/1000000000)) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Tax Revenue (Billion BRL)",
    title = "Total Monthly Federal Tax Revenue from CIDE Combustíveis",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024",
    caption = "Note: Data related to firms registered under D category only, which refers to the electricity and gas sector."
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    plot.caption = element_text(hjust = 0))

ggsave("./4_plots/plot_trend_fed_revenue_CIDE_electgas.png",
       plot = plot_trend_fed_revenue_CIDE_electgas,
       units = "in")
    

# Economic Sector G (includes gas stations)

plot_trend_fed_revenue_CIDE_gasstation <- ggplot(federal_CIDE_gasstation, 
                                      aes(x = ano_mes_date, 
                                          y = (`CIDE Combustíveis`)/1000000000)) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Tax Revenue (Billion BRL)",
    title = "Total Monthly Federal Tax Revenue from CIDE Combustíveis",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024",
    caption = "Note: Data related to firms registered under G category only, which includes gas stations and retail sale of fuels."
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    plot.caption = element_text(hjust = 0)
                               
  )

ggsave("./4_plots/plot_trend_fed_revenue_CIDE_gasstation.png",
       plot = plot_trend_fed_revenue_CIDE_gasstation,
       units = "in")


# All sectors

plot_trend_fed_revenue_CIDE_groups <- ggplot(federal_CIDE_allsectors,
                                             aes(x = ano_mes_date, y = `CIDE Combustíveis`)) +
    geom_line(linewidth = 0.7) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(trans = "log10",
                     breaks = c(1e5, 1e6, 1e7, 1e8, 1e9, 1e10),
                     labels = c("100k", "1M", "10M", "100M", "1B", "10B")) +
  labs(
    x = "Year",
    y = "Total Tax Revenue (BRL) - log transformed",
    title = "Total Monthly Federal Tax Revenue from CIDE Combustíveis",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~ sectors_en, nrow = 6)

ggsave("./4_plots/plot_trend_fed_revenue_CIDE_groups.png",
       plot = plot_trend_fed_revenue_CIDE_groups,
       units = "in",
       width = 10,
       height = 10)

# Note: there seems to be an issue with the data, as the revenue from 2024 grows
# exponentially for some sectors. It is either a problem with data input, or there
# is a change on the legislation for this tax specifically which I am not aware of



# 4.1.3. CIDE Combustiveis (up to 2023 only) -----------------------------------


# Changing the data sets to remove values after January 2024 

federal_CIDE_energysector_23 <-  federal_CIDE_energysector %>% 
  filter(ano_mes_date < as.Date(paste("2024", "01", "01", sep = "-")))

federal_CIDE_gasstation_23 <- federal_CIDE_gasstation %>% 
  filter(ano_mes_date < as.Date(paste("2024", "01", "01", sep = "-")))

federal_CIDE_allsectors_23 <- federal_CIDE_allsectors %>% 
  filter(ano_mes_date < as.Date(paste("2024", "01", "01", sep = "-")))
  


# Economic Sector D (Electricity and Gas) up to December 2023

plot_trend_fed_revenue_CIDE_electgas_23 <- ggplot(federal_CIDE_energysector_23, 
                                               aes(x = ano_mes_date, 
                                                   y = (`CIDE Combustíveis`)/1000000000)) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Tax Revenue (Billion BRL)",
    title = "Total Monthly Federal Tax Revenue from CIDE Combustíveis up to December 2023",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024",
    caption = "Note: Data related to firms registered under D category only, which refers to the electricity and gas sector."
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    plot.caption = element_text(hjust = 0))

ggsave("./4_plots/plot_trend_fed_revenue_CIDE_electgas_23.png",
       plot = plot_trend_fed_revenue_CIDE_electgas_23,
       units = "in")


# Economic Sector G (includes gas stations) up to December 2023

plot_trend_fed_revenue_CIDE_gasstation_23 <- ggplot(federal_CIDE_gasstation_23, 
                                                 aes(x = ano_mes_date, 
                                                     y = (`CIDE Combustíveis`)/1000000000)) +
  geom_line(linewidth = 1) +  # Line plot for each 'tipo_consumo'
  theme_bw() +                  # Clean theme
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show only the first month of each year
  scale_y_continuous(labels = scales::comma) +               # Format y-axis labels
  labs(
    x = "Year",
    y = "Total Tax Revenue (Billion BRL)",
    title = "Total Monthly Federal Tax Revenue from CIDE Combustíveis up to December 2023",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024",
    caption = "Note: Data related to firms registered under G category only, which includes gas stations and retail sale of fuels."
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    plot.caption = element_text(hjust = 0)
    
  )

ggsave("./4_plots/plot_trend_fed_revenue_CIDE_gasstation_23.png",
       plot = plot_trend_fed_revenue_CIDE_gasstation_23,
       units = "in")


# All sectors up to December 2023

plot_trend_fed_revenue_CIDE_groups_23 <- ggplot(federal_CIDE_allsectors_23,
                                             aes(x = ano_mes_date, y = `CIDE Combustíveis`)) +
  geom_line(linewidth = 0.7) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(trans = "log10",
                     breaks = c(1e5, 1e6, 1e7, 1e8, 1e9, 1e10),
                     labels = c("100k", "1M", "10M", "100M", "1B", "10B")) +
  labs(
    x = "Year",
    y = "Total Tax Revenue (BRL) - log transformed",
    title = "Total Monthly Federal Tax Revenue from CIDE Combustíveis up to December 2023",
    subtitle = "Source: Special Secretariat of the Federal Revenue of Brazil (RFB), 2024"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~ sectors_en, nrow = 6)

ggsave("./4_plots/plot_trend_fed_revenue_CIDE_groups_23.png",
       plot = plot_trend_fed_revenue_CIDE_groups_23,
       units = "in",
       width = 10,
       height = 10)

# Note: now the data is less distorted for the two specific sectors (D and G), but
# the plots suggest that the tax revenue for these two industries are highly correlated.





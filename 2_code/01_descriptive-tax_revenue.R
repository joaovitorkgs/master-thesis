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

# State (Santa Catarian) 

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



# 3. Tax Revenue analysis ------------------------------------------------------

federal_energy <- df_federal_cnae %>% 
  select(ano,mes,secao_sigla,cide_combustiveis) %>% 
  filter(secao_sigla != "D") %>% 
  group_by(ano) %>% 
  summarise(
    total = sum(cide_combustiveis))

federal_fuel <- df_federal_cnae %>% 
  select(ano,mes,secao_sigla,cide_combustiveis) %>% 
  mutate(cide_combustiveis = as.numeric(cide_combustiveis)) %>% 
  group_by(ano) %>% 
  summarise(
    total = sum(cide_combustiveis))

# Noch zu tun: figure out why the grouping function is not working; exploring the
# data per tax type and plot the trendlines over years; 

# Also: look into the literature to see what the tax composition of fuels looks like
# for gasoline




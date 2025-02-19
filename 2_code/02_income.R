# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

income_data_raw <- read_delim("1_raw_data/6_income/distribuicao-renda.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

# 3. Transforming data sets ----------------------------------------------------

rm(income_data)
# Income data used refers to yearly gross taxable income
income_data <- income_data_raw %>% 
  rename(avg_taxable_income = `Rendimentos Tributaveis - Média da RTB do Centil [R$]`,
         sum_taxable_income = `Rendimentos Tributaveis - Soma da RTB do Centil [R$ milhões]`,
         nr_taxpayers       = `Quantidade de Contribuintes`,
         sigla_uf           = `Ente Federativo`,
         year               = `Ano-calendário`,
         percentile         = Centil) %>% 
  select(year, sigla_uf, percentile,
         avg_taxable_income,
         sum_taxable_income)

income_data$avg_taxable_income[is.na(income_data$avg_taxable_income)] = 0
income_data$sum_taxable_income[is.na(income_data$sum_taxable_income)] = 0

income_data <- income_data %>%
  mutate(
    avg_taxable_income = gsub("\\.", "", avg_taxable_income),  # Remove thousands separator (.)
    avg_taxable_income = gsub(",", ".",  avg_taxable_income),  # Replace decimal separator (,) with dot (.)
    avg_taxable_income = as.numeric(     avg_taxable_income),       # Convert to numeric
    
    sum_taxable_income = gsub("\\.", "", sum_taxable_income),  # Remove thousands separator (.)
    sum_taxable_income = gsub(",", ".",  sum_taxable_income),  # Replace decimal separator (,) with dot (.)
    sum_taxable_income = as.numeric(     sum_taxable_income)
  )

income_data_wide <- income_data %>% 
  filter(percentile %in% c(10,50,90,100)) %>% 
  pivot_wider(names_from = percentile, values_from = c(avg_taxable_income,sum_taxable_income)) %>% 
  filter(sigla_uf != "BRASIL") %>% 
  mutate("90_50_sum" = sum_taxable_income_90 / sum_taxable_income_50,
         "90_10_sum" = sum_taxable_income_90 / sum_taxable_income_10,
         "90_50_avg" = avg_taxable_income_90 / avg_taxable_income_50,
         "90_10_avg" = avg_taxable_income_90 / avg_taxable_income_10)

if (!file.exists(  "./3_processed_data/income_data_wide.csv")) {
  write_csv(income_data_wide,
            file = "./3_processed_data/income_data_wide.csv")
  print("File succesfully written.")
} else {
  print("File already exists in the repository.")
}



# 3. Visualizing data sets -----------------------------------------------------


income_data_br_wide <- income_data %>% 
  filter(percentile %in% c(10,50,90,100)) %>% 
  pivot_wider(names_from = percentile, values_from = c(avg_taxable_income,sum_taxable_income)) %>% 
  filter(sigla_uf == "BRASIL",
         year     >= 2013) %>% 
  mutate("90_50_sum" = sum_taxable_income_90 / sum_taxable_income_50,
         "90_10_sum" = sum_taxable_income_90 / sum_taxable_income_10,
         "90_50_avg" = avg_taxable_income_90 / avg_taxable_income_50,
         "90_10_avg" = avg_taxable_income_90 / avg_taxable_income_10)


plot_trend_sum_taxable_income <- 
  ggplot(income_data_br, aes(x = year)) +
  geom_line(aes(y = sum_taxable_income_10,   color = "Bottom 10%"), size = 1) +
  geom_line(aes(y = sum_taxable_income_50,   color = "Top 50%"), size = 1) +
  geom_line(aes(y = sum_taxable_income_90,   color = "Top 90%"), size = 1) +
  geom_line(aes(y = sum_taxable_income_100,  color = "Top 100%"), size = 1) +
  scale_color_manual(
    name = "Percentiles",
    values = c("Bottom 10%" = "blue3", 
               "Top 50%"    = "yellow3", 
               "Top 90%"    = "orange3",
               "Top 100%"   = "red4")) +
  scale_y_continuous(
    trans = "log10",   # Log10 transformation
    breaks = trans_breaks("log10", function(x) 10^x),   # Define breaks in log10 scale
    labels = label_number(scale = 1/1000, big.mark = ",", decimal.mark = ".")
    ) +
  scale_x_continuous(
    breaks = 2013:2020, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2020) # Convert to character for cleaner labels
  ) +
  labs(
    x = "Year",
    y = "Sum of Gross Taxable Income (thousand BRL)",
    title = "Evolution of Total Gross Taxable Income per Percentiles in Brazil (2013-2020)",
    subtitle = "Source: Ministry of Treasury, 2021",
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave(filename = "./4_plots/plot_trend_sum_taxable_income.png",
       plot     = plot_trend_sum_taxable_income,
       width    = 10,
       height   = 5)



plot_trend_avg_taxable_income <- 
  ggplot(income_data_br, aes(x = year)) +
  geom_line(aes(y = avg_taxable_income_10,   color = "Bottom 10%"), size = 1) +
  geom_line(aes(y = avg_taxable_income_50,   color = "Top 50%"), size = 1) +
  geom_line(aes(y = avg_taxable_income_90,   color = "Top 90%"), size = 1) +
  geom_line(aes(y = avg_taxable_income_100,  color = "Top 100%"), size = 1) +
  scale_color_manual(
    name = "Percentiles",
    values = c("Bottom 10%" = "blue3", 
               "Top 50%"    = "yellow3", 
               "Top 90%"    = "orange3",
               "Top 100%"   = "red4")) +
  scale_y_continuous(
    trans = "log10",   # Log10 transformation
    breaks = trans_breaks("log10", function(x) 10^x),   # Define breaks in log10 scale
    labels = label_number(scale = 1/1000, big.mark = ",", decimal.mark = ".")
  ) +
  scale_x_continuous(
    breaks = 2013:2020, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2020) # Convert to character for cleaner labels
  ) +
  labs(
    x = "Year",
    y = "Sum of Gross Taxable Income (thousand BRL)",
    title = "Evolution of Average Gross Taxable Income per Percentiles in Brazil (2013-2020)",
    subtitle = "Source: Ministry of Treasury, 2021",
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave(filename = "./4_plots/plot_trend_avg_taxable_income.png",
       plot     = plot_trend_avg_taxable_income,
       width    = 10,
       height   = 5)


plot_trend_income_dist_905010 <- 
  ggplot(income_data_br_wide, aes(x = year)) +
  geom_line(aes(y = `90_50_avg`,   color = "Ratio 90/50")) +
  geom_line(aes(y = `90_10_avg`,   color = "Ratio 90/10")) +
  scale_color_manual(
    name = "Ratios",
    values = c("Ratio 90/50" = "blue3",
               "Ratio 90/10" = "orange3")) +
  scale_x_continuous(
    breaks = 2013:2020, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2020) # Convert to character for cleaner labels
  ) +
  scale_y_continuous()+
  labs(
    x = "Year",
    y = "Ratio between Income Percentiles",
    title = "Evolution of Income Distribution between Percentiles in Brazil (2013-2020)",
    subtitle = "Source: Ministry of Treasury, 2021",
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#ffffff", color = NA)
  )

ggsave(filename = "./4_plots/plot_trend_income_dist_905010 .png",
       plot     = plot_trend_income_dist_905010 ,
       width    = 10,
       height   = 5)

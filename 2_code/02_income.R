# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

income_data_raw <- read_delim("1_raw_data/6_income/distribuicao-renda.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

# 3. Transforming data sets ----------------------------------------------------

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
  pivot_wider(names_from = percentile, values_from = c(avg_taxable_income,sum_taxable_income)) 

if (!file.exists(  "./3_processed_data/income_data_wide.csv")) {
  write_csv(income_data_wide,
            file = "./3_processed_data/income_data_wide.csv")
  print("File succesfully written.")
} else {
  print("File already exists in the repository.")
}

summary(income_data_wide)


# Given the lack of data for the years between 2021 and 2023, I decided to create
# synthetic data for the missing data points using linear regressions for the 
# available parametres and extrapolate to the missing years.

# Define the years for prediction
future_years <- data.frame(year = c(2021, 2022, 2023))

# Define a function to fit linear regression and predict future values
predict_future_values <- function(data) {
  # Fit linear models for each income column
  avg_models <- lapply(c("avg_taxable_income_10", "avg_taxable_income_50", 
                         "avg_taxable_income_90", "avg_taxable_income_100"), function(col) {
                           lm(as.formula(paste(col, "~ year")), data = data)
                         })
  
  sum_models <- lapply(c("sum_taxable_income_10", "sum_taxable_income_50", 
                         "sum_taxable_income_90", "sum_taxable_income_100"), function(col) {
                           lm(as.formula(paste(col, "~ year")), data = data)
                         })
  
  # Predict future values for each column
  avg_predictions <- sapply(avg_models, function(model) predict(model, newdata = future_years))
  sum_predictions <- sapply(sum_models, function(model) predict(model, newdata = future_years))
  
  # Combine predictions into a single data frame
  predictions <- cbind(future_years,
                       avg_predictions,
                       sum_predictions)
  
  colnames(predictions)[-1] <- c("avg_taxable_income_10", "avg_taxable_income_50",
                                 "avg_taxable_income_90", "avg_taxable_income_100",
                                 "sum_taxable_income_10", "sum_taxable_income_50",
                                 "sum_taxable_income_90", "sum_taxable_income_100")
  
  return(predictions)
}

# Apply the function to each state (sigla_uf)
synthetic_data <- income_data_wide %>%
  group_by(sigla_uf) %>%
  group_split() %>%
  lapply(predict_future_values) %>%
  bind_rows(.id = "group") %>%
  mutate(sigla_uf = unique(income_data_wide$sigla_uf)[as.numeric(group)]) %>%
  select(-group)


# Combine synthetic data with the original dataframe
income_data_wide_syn <- bind_rows(income_data_wide, synthetic_data) %>% 
  mutate("90_50_sum" = sum_taxable_income_90 / sum_taxable_income_50,
         "90_10_sum" = sum_taxable_income_90 / sum_taxable_income_10,
         "90_50_avg" = avg_taxable_income_90 / avg_taxable_income_50,
         "90_10_avg" = avg_taxable_income_90 / avg_taxable_income_10)

income_data_wide_uf <- income_data_wide_syn %>% 
  filter(sigla_uf != "BRASIL",
         year > 2012)

## Visualizing the income distribution data set --------------------------------

ggplot(income_data_wide_uf, aes(x=avg_taxable_income_50)) +
  geom_density() +
  ggtitle("Distribution of the Average Median Income Distribution in Brazilian States")+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +
  facet_wrap(~year, nrow = 5) +
  ylab("Density") +
  xlab("Average Median Taxable Income of the 50 Percentile") 
  
ggplot(income_data_wide_uf, aes(x=avg_taxable_income_90)) +
  geom_density() +
  ggtitle("Distribution of the Income Distribution in Brazilian States (90 Percentile)")+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +
  facet_wrap(~year, nrow = 5) +
  ylab("Density") +
  xlab("Average Taxable Income of the top 10 Percentile") 

income_data_wide <- income_data %>% 
  filter(percentile %in% c(10,50,90,100)) %>% 
  pivot_wider(names_from = percentile, values_from = c(avg_taxable_income,sum_taxable_income)) 

## Exporting the processed file ------------------------------------------------

if (!file.exists(  "./3_processed_data/income_data_wide_uf.csv")) {
  write_csv(income_data_wide_uf,
            file = "./3_processed_data/income_data_wide_uf.csv")
  print("File succesfully written.")
} else {
  print("File already exists in the repository.")
}


# 3. Visualizing data sets -----------------------------------------------------


income_data_br_wide <- income_data_wide_syn %>% 
  filter(sigla_uf == "BRASIL",
         year > 2012)

plot_trend_avg_taxable_income <- 
  ggplot(income_data_br_wide, aes(x = year)) +
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
    breaks = 2013:2023, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2023) # Convert to character for cleaner labels
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


plot_trend_income_dist_9010 <- 
  ggplot(income_data_br_wide, aes(x = year)) +
  geom_line(aes(y = `90_10_avg`,   color = "Ratio 90/10")) +
  scale_color_manual(
    name = "Ratios",
    values = c("Ratio 90/50" = "blue3",
               "Ratio 90/10" = "orange3")) +
  scale_x_continuous(
    breaks = 2013:2023, # Ensure all years from 2013 to 2022 are shown
    labels = as.character(2013:2023) # Convert to character for cleaner labels
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

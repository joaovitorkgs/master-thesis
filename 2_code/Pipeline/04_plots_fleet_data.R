# 1. Packages ------------------------------------------------------------------

setwd("C:/Users/joaov/Dropbox/R Assignments/master-thesis")
source("./2_code/00_packages.R")
source("./2_code/Pipeline/03_data_cleaning.R")

# 2. Plots ---------------------------------------------------------------------

# Reshape the data for ggplot
df_long <- df_fleet_brazil %>%
  rename(
    `ICEV (Diesel)`   = Diesel, 
    `ICEV (Gasoline)` = Gasoline,
    `ICEV (Ethanol)`  = Ethanol,
    `HEV and PHEV`    = PHEV,
  ) %>% 
  pivot_longer(cols = c(`ICEV (Gasoline)`, `ICEV (Ethanol)`,`ICEV (Diesel)`,  `HEV and PHEV`, BEV), names_to = "fuel_type", values_to = "total")

# All powertrain and fuel categories
plot_fleet_trend_cat <- ggplot(df_long, aes(x = date, y = total, color = fuel_type)) +
  geom_line(linewidth = 1) +
  theme_bw() +  # More defined frame compared to theme_minimal
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Yearly breaks for Date axis
  scale_y_continuous(
    trans = "log10",
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = comma  # More precise label control
  ) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    axis.title = element_text(size = 12)
  ) +
  labs(
    x = "Year",
    y = "Registered Vehicles (Log Scale)",
    title = "Evolution of Brazil's Vehicle Fleet by Powertrain and Fuel Type",
    subtitle = "Source: National Traffic Secretariat (2024)",
    color = "Powertrain/Fuel"
  ) +
  scale_color_manual(
    values = c(
      "BEV"             = "#2E8B57",     
      "HEV and PHEV"  = "#20B2AA",   
      "ICEV (Diesel)"   = "#8B0000",   
      "ICEV (Gasoline)" = "#B22222",   
      "ICEV (Ethanol)"  = "gold3",    
      "Gas"             = "#4682B4",        
      "Other"           = "#A9A9A9"       
    )
  )


if (!file.exists("./4_plots/plot_fleet_trend_cat.png")) {
  ggsave("./4_plots/plot_fleet_trend_cat.png",
         plot   = plot_fleet_trend_cat,
         height = 4,
         width  = 8)
} else {
  print("File already exists in the repository")
}

# BEV
plot_fleet_trend_BEV <- df_long %>% 
  filter(fuel_type == "BEV") %>% 
  ggplot(aes(x = date, y = total, color = fuel_type)) +
  geom_line(linewidth = 1) +
  theme_bw() +  # More defined frame compared to theme_minimal
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Yearly breaks for Date axis
  scale_y_continuous(
    trans = "log10",
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = comma  # More precise label control
  ) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    legend.position = "",
    axis.title = element_text(size = 12)
  ) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles (Log Scale)",
    title = "Evolution of Brazil's Vehicle Fleet by Fuel Type",
    subtitle = "Source: National Traffic Secretariat (2024)",
    color = "Powertrain/Fuel"
  ) +
  scale_color_manual(
    values = c(
      "BEV"            = "#2E8B57",     
      "PHEV"           = "#20B2AA",   
      "ICE (Diesel)"   = "#8B0000",   
      "ICE (Gasoline)" = "#B22222",   
      "ICE (Ethanol)"  = "gold3",    
      "Gas"            = "#4682B4",        
      "Other"          = "#A9A9A9"       
    )
  )

if (!file.exists("./4_plots/plot_fleet_trend_BEV.png")) {
  ggsave("./4_plots/plot_fleet_trend_BEV.png",
         plot   = plot_fleet_trend_BEV,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository")
}


# PHEV
plot_fleet_trend_EVs <- df_long %>% 
  filter(fuel_type %in% c("BEV", "PHEV")) %>% 
  ggplot(aes(x = date, y = total, color = fuel_type)) +
  geom_line(linewidth = 1) +
  theme_bw() +  # More defined frame compared to theme_minimal
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Yearly breaks for Date axis
  scale_y_continuous(
    trans = "log10",
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = comma  # More precise label control
  ) +
  theme(
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    axis.title = element_text(size = 12)
  ) +
  labs(
    x = "Year",
    y = "Number of Registered Vehicles (Log Scale)",
    title = "Evolution of Brazil's Electric Vehicle Fleet by Technology",
    subtitle = "Source: National Traffic Secretariat (2024)",
    color = "Powertrain/Fuel"
  ) +
  scale_color_manual(
    values = c(
      "BEV"            = "#2E8B57",     
      "PHEV"           = "#20B2AA",   
      "ICE (Diesel)"   = "#8B0000",   
      "ICE (Gasoline)" = "#B22222",   
      "ICE (Ethanol)"  = "gold3",    
      "Gas"            = "#4682B4",        
      "Other"          = "#A9A9A9"       
    )
  )

if (!file.exists("./4_plots/plot_fleet_trend_EVs.png")) {
  ggsave("./4_plots/plot_fleet_trend_EVs.png",
         plot   = plot_fleet_trend_EVs,
         height = 6,
         width  = 8)
  print("File successfully saved.")
} else {
  print("File already exists in the repository.")
}

# 3. Table summaries -----------------------------------------------------------

### Brazil ---------------------------------------------------------------------

# Some figures and percentages
options(scipen = 999)

df_fleet_brazil %>% 
  tail(1) %>% 
  mutate(ICEVs_percent = 100*(Diesel+Ethanol+Gasoline)/sum(Diesel, Ethanol, Gasoline, Gas, BEV, PHEV)) %>% 
  select(ICEVs_percent)

df_fleet_brazil %>% 
  tail(1) %>% 
  mutate(BEV_percent = 100*BEV/sum(Diesel, Ethanol, Gasoline, Gas, BEV, PHEV)) %>% 
  select(BEV_percent)

df_fleet_brazil %>% 
  tail(1) %>% 
  mutate(PHEV_percent = 100*PHEV/sum(Diesel, Ethanol, Gasoline, Gas, BEV, PHEV)) %>% 
  select(PHEV_percent)

df_fleet_brazil %>% 
  tail(1) %>% 
  mutate(Gasoline_percent = 100*Gasoline/sum(Diesel, Ethanol, Gasoline, Gas, BEV, PHEV)) %>% 
  select(Gasoline_percent)

df_fleet_brazil %>% 
  tail(1) %>% 
  mutate(Ethanol_percent = 100*Ethanol/sum(Diesel, Ethanol, Gasoline, Gas, BEV, PHEV)) %>% 
  select(Ethanol_percent)





### State-level data ------------------------------------------------------------

df_fleet_state %>%
  drop_na() %>% 
  filter(date == max(date)) %>%
  summary()

summary_table_state <- df_fleet_state %>%
  drop_na() %>%
  filter(date == max(date)) %>%
  summarise(
    N = n(),
    Mean_Gasoline = mean(Gasoline, na.rm = TRUE),
    SD_Gasoline = sd(Gasoline, na.rm = TRUE),
    Min_Gasoline = min(Gasoline, na.rm = TRUE),
    Max_Gasoline = max(Gasoline, na.rm = TRUE),
    
    Mean_Ethanol = mean(Ethanol, na.rm = TRUE),
    SD_Ethanol = sd(Ethanol, na.rm = TRUE),
    Min_Ethanol = min(Ethanol, na.rm = TRUE),
    Max_Ethanol = max(Ethanol, na.rm = TRUE),
    
    Mean_Diesel = mean(Diesel, na.rm = TRUE),
    SD_Diesel = sd(Diesel, na.rm = TRUE),
    Min_Diesel = min(Diesel, na.rm = TRUE),
    Max_Diesel = max(Diesel, na.rm = TRUE),
    
    Mean_Other = mean(Other, na.rm = TRUE),
    SD_Other = sd(Other, na.rm = TRUE),
    Min_Other = min(Other, na.rm = TRUE),
    Max_Other = max(Other, na.rm = TRUE),
    
    Mean_Gas = mean(Gas, na.rm = TRUE),
    SD_Gas = sd(Gas, na.rm = TRUE),
    Min_Gas = min(Gas, na.rm = TRUE),
    Max_Gas = max(Gas, na.rm = TRUE),
    
    Mean_PHEV = mean(PHEV, na.rm = TRUE),
    SD_PHEV = sd(PHEV, na.rm = TRUE),
    Min_PHEV = min(PHEV, na.rm = TRUE),
    Max_PHEV = max(PHEV, na.rm = TRUE),
    
    Mean_BEV = mean(BEV, na.rm = TRUE),
    SD_BEV = sd(BEV, na.rm = TRUE),
    Min_BEV = min(BEV, na.rm = TRUE),
    Max_BEV = max(BEV, na.rm = TRUE),
    
    Mean_Population = mean(population, na.rm = TRUE),
    SD_Population = sd(population, na.rm = TRUE),
    Min_Population = min(population, na.rm = TRUE),
    Max_Population = max(population, na.rm = TRUE)
  )

table_data_state <- data.frame(
  Statistic = c(
    "Gasoline", "Ethanol", "Diesel", "Other", "Gas",
    "PHEV Electric", "BEV Electric", "Population"
  ),
  N = formatC(summary_table_state$N, format = "d", big.mark = ","),
  Mean = c(
    summary_table_state$Mean_Gasoline, summary_table_state$Mean_Ethanol, summary_table_state$Mean_Diesel,
    summary_table_state$Mean_Other, summary_table_state$Mean_Gas,
    summary_table_state$Mean_PHEV, summary_table_state$Mean_BEV, summary_table_state$Mean_Population
  ),
  `St. Dev.` = c(
    summary_table_state$SD_Gasoline, summary_table_state$SD_Ethanol, summary_table_state$SD_Diesel,
    summary_table_state$SD_Other, summary_table_state$SD_Gas,
    summary_table_state$SD_PHEV, summary_table_state$SD_BEV, summary_table_state$SD_Population
  ),
  Min = c(
    summary_table_state$Min_Gasoline, summary_table_state$Min_Ethanol, summary_table_state$Min_Diesel,
    summary_table_state$Min_Other, summary_table_state$Min_Gas,
    summary_table_state$Min_PHEV, summary_table_state$Min_BEV, summary_table_state$Min_Population
  ),
  Max = c(
    summary_table_state$Max_Gasoline, summary_table_state$Max_Ethanol, summary_table_state$Max_Diesel,
    summary_table_state$Max_Other, summary_table_state$Max_Gas,
    summary_table_state$Max_PHEV, summary_table_state$Max_BEV, summary_table_state$Max_Population
  ),
  check.names = FALSE
)

table_data_state$Mean <- formatC(table_data_state$Mean, format = "d", big.mark = ",")
table_data_state$`St. Dev.` <- formatC(table_data_state$`St. Dev.`, format = "d", big.mark = ",")
table_data_state$Min <- formatC(table_data_state$Min, format = "d", big.mark = ",")
table_data_state$Max <- formatC(table_data_state$Max, format = "d", big.mark = ",")
summary_table_state <- df_fleet_state %>%
  drop_na() %>%
  filter(date == max(date)) %>%
  summarise(
    N = n(),
    Mean_Gasoline = mean(Gasoline, na.rm = TRUE),
    SD_Gasoline = sd(Gasoline, na.rm = TRUE),
    Min_Gasoline = min(Gasoline, na.rm = TRUE),
    Max_Gasoline = max(Gasoline, na.rm = TRUE),
    
    Mean_Ethanol = mean(Ethanol, na.rm = TRUE),
    SD_Ethanol = sd(Ethanol, na.rm = TRUE),
    Min_Ethanol = min(Ethanol, na.rm = TRUE),
    Max_Ethanol = max(Ethanol, na.rm = TRUE),
    
    Mean_Diesel = mean(Diesel, na.rm = TRUE),
    SD_Diesel = sd(Diesel, na.rm = TRUE),
    Min_Diesel = min(Diesel, na.rm = TRUE),
    Max_Diesel = max(Diesel, na.rm = TRUE),
    
    Mean_Other = mean(Other, na.rm = TRUE),
    SD_Other = sd(Other, na.rm = TRUE),
    Min_Other = min(Other, na.rm = TRUE),
    Max_Other = max(Other, na.rm = TRUE),
    
    Mean_Gas = mean(Gas, na.rm = TRUE),
    SD_Gas = sd(Gas, na.rm = TRUE),
    Min_Gas = min(Gas, na.rm = TRUE),
    Max_Gas = max(Gas, na.rm = TRUE),
    
    Mean_PHEV = mean(PHEV, na.rm = TRUE),
    SD_PHEV = sd(PHEV, na.rm = TRUE),
    Min_PHEV = min(PHEV, na.rm = TRUE),
    Max_PHEV = max(PHEV, na.rm = TRUE),
    
    Mean_BEV = mean(BEV, na.rm = TRUE),
    SD_BEV = sd(BEV, na.rm = TRUE),
    Min_BEV = min(BEV, na.rm = TRUE),
    Max_BEV = max(BEV, na.rm = TRUE),
    
    Mean_Population = mean(population, na.rm = TRUE),
    SD_Population = sd(population, na.rm = TRUE),
    Min_Population = min(population, na.rm = TRUE),
    Max_Population = max(population, na.rm = TRUE)
  )

# Print the table using knitr::kable
kable(table_data_state, caption = "Summary statistics of population and vehicle stock of Brazilian cities")


if (!file.exists("./6_tables/summary_vehicle_population_table_state.csv")) {
  write.csv(summary_table_state,
            file = "./6_tables/summary_vehicle_population_table_state.csv",
            row.names = FALSE)
  print("File successfully saved.")
} else {
  print("File already exists in the repository")
}



### City-level data ------------------------------------------------------------

df_fleet_city %>%
  drop_na() %>% 
  filter(date == max(date)) %>%
  summary()

summary_table <- df_fleet_city %>%
  drop_na() %>%
  filter(date == max(date)) %>%
  summarise(
    N = n(),
    Mean_Gasoline = mean(Gasoline, na.rm = TRUE),
    SD_Gasoline = sd(Gasoline, na.rm = TRUE),
    Min_Gasoline = min(Gasoline, na.rm = TRUE),
    Max_Gasoline = max(Gasoline, na.rm = TRUE),
    
    Mean_Ethanol = mean(Ethanol, na.rm = TRUE),
    SD_Ethanol = sd(Ethanol, na.rm = TRUE),
    Min_Ethanol = min(Ethanol, na.rm = TRUE),
    Max_Ethanol = max(Ethanol, na.rm = TRUE),
    
    Mean_Diesel = mean(Diesel, na.rm = TRUE),
    SD_Diesel = sd(Diesel, na.rm = TRUE),
    Min_Diesel = min(Diesel, na.rm = TRUE),
    Max_Diesel = max(Diesel, na.rm = TRUE),
    
    Mean_Other = mean(Other, na.rm = TRUE),
    SD_Other = sd(Other, na.rm = TRUE),
    Min_Other = min(Other, na.rm = TRUE),
    Max_Other = max(Other, na.rm = TRUE),
    
    Mean_Gas = mean(Gas, na.rm = TRUE),
    SD_Gas = sd(Gas, na.rm = TRUE),
    Min_Gas = min(Gas, na.rm = TRUE),
    Max_Gas = max(Gas, na.rm = TRUE),
    
    Mean_PHEV = mean(PHEV, na.rm = TRUE),
    SD_PHEV = sd(PHEV, na.rm = TRUE),
    Min_PHEV = min(PHEV, na.rm = TRUE),
    Max_PHEV = max(PHEV, na.rm = TRUE),
    
    Mean_BEV = mean(BEV, na.rm = TRUE),
    SD_BEV = sd(BEV, na.rm = TRUE),
    Min_BEV = min(BEV, na.rm = TRUE),
    Max_BEV = max(BEV, na.rm = TRUE),
    
    Mean_Population = mean(populacao, na.rm = TRUE),
    SD_Population = sd(populacao, na.rm = TRUE),
    Min_Population = min(populacao, na.rm = TRUE),
    Max_Population = max(populacao, na.rm = TRUE)
  )

# Create a formatted table
table_data <- data.frame(
  Statistic = c(
    "Gasoline", "Ethanol", "Diesel", "Other", "Gas",
    "PHEV Electric", "BEV Electric", "Population"
  ),
  N = summary_table$N,
  Mean = c(
    summary_table$Mean_Gasoline, summary_table$Mean_Ethanol, summary_table$Mean_Diesel,
    summary_table$Mean_Other, summary_table$Mean_Gas,
    summary_table$Mean_PHEV, summary_table$Mean_BEV, summary_table$Mean_Population
  ),
  `St. Dev.` = c(
    summary_table$SD_Gasoline, summary_table$SD_Ethanol, summary_table$SD_Diesel,
    summary_table$SD_Other, summary_table$SD_Gas,
    summary_table$SD_PHEV, summary_table$SD_BEV, summary_table$SD_Population
  ),
  Min = c(
    summary_table$Min_Gasoline, summary_table$Min_Ethanol, summary_table$Min_Diesel,
    summary_table$Min_Other, summary_table$Min_Gas,
    summary_table$Min_PHEV, summary_table$Min_BEV, summary_table$Min_Population
  ),
  Max = c(
    summary_table$Max_Gasoline, summary_table$Max_Ethanol, summary_table$Max_Diesel,
    summary_table$Max_Other, summary_table$Max_Gas,
    summary_table$Max_PHEV, summary_table$Max_BEV, summary_table$Max_Population
  )
)

# Print the table using knitr::kable
kable(table_data, caption = "Summary statistics of population and vehicle stock of Brazilian cities")

# Saving the data as csv
write.csv(table_data, "summary_vehicle_population_table.csv", row.names = FALSE)


if (!file.exists("./6_tables/summary_vehicle_population_table.csv")) {
       write.csv(table_data,
                 file = "./6_tables/summary_vehicle_population_table.csv",
                 row.names = FALSE)
  print("File successfully saved.")
} else {
  print("File already exists in the repository")
}

# 1. Packages ------------------------------------------------------------------

setwd("C:/Users/joaov/Dropbox/R Assignments/master-thesis")
source("./2_code/00_packages.R")
source("./2_code/Pipeline/03_data_cleaning.R")


### Visualizing the data --------------------------------------------------------

# Reshape the data for ggplot
df_long <- df_fleet_brazil %>%
  rename(
    `ICE (Diesel)`   = Diesel, 
    `ICE (Gasoline)` = Gasoline,
    `ICE (Ethanol)`  = Ethanol,
  ) %>% 
  pivot_longer(cols = c(`ICE (Gasoline)`, `ICE (Ethanol)`,`ICE (Diesel)`,  PHEV, BEV), names_to = "fuel_type", values_to = "total")

options(scipen = 999)
df_fleet_brazil %>% 
  tail(1) %>% 
  mutate(BEV_percent = 100*BEV/sum(Diesel, Ethanol, Gasoline, Gas, BEV, PHEV)) %>% 
  select(BEV_percent)

df_fleet_brazil %>% 
  tail(1) %>% 
  mutate(PHEV_percent = 100*PHEV/sum(Diesel, Ethanol, Gasoline, Gas, BEV, PHEV)) %>% 
  select(PHEV_percent)

9.66e-10

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
    y = "Number of Registered Vehicles (Log Scale)",
    title = "Evolution of Brazil's Vehicle Fleet by Powertrain and Fuel Type",
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


if (!file.exists("./4_plots/plot_fleet_trend_cat.png")) {
  ggsave("./4_plots/plot_fleet_trend_cat.png",
         plot   = plot_fleet_trend_cat,
         height = 6,
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

# 1. Packages ------------------------------------------------------------------

setwd("C:/Users/joaov/Dropbox/R Assignments/master-thesis")
source("./2_code/00_packages.R")
source("./2_code/Pipeline/02_cleaning_fleet_data.R")


### Visualizing the data --------------------------------------------------------

# Reshape the data for ggplot
df_long <- df_fleet_brazil %>%
  pivot_longer(cols = c(BEV, Diesel, Gasoline, Ethanol, PHEV), names_to = "fuel_type", values_to = "total")

# Plotting
ggplot(df_long, aes(x = date, y = total, color = fuel_type)) +
  geom_line(size = 1) +
  scale_y_continuous(
    trans = "log10",  # Log-transform the Y-axis
    labels = label_comma()  # Show non-scientific notation values
  ) +
  labs(
    title = "Evolution of Vehicles per Fuel Type (Log Scale)",
    x = "Date",
    y = "Total Vehicles (Log Scale)",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )


# Plotting
df_long %>% 
  filter(fuel_type == "BEV") %>% 
  ggplot(aes(x = date, y = total, color = fuel_type)) +
  geom_line(size = 1) +
  scale_y_continuous(
    trans = "log10",  # Log-transform the Y-axis
    labels = label_comma()  # Show non-scientific notation values
  ) +
  labs(
    title = "Evolution of Vehicles per Fuel Type (Log Scale)",
    x = "Date",
    y = "Total Vehicles (Log Scale)",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

df_long %>% 
  filter(fuel_type == "PHEV") %>% 
  ggplot(aes(x = date, y = total, color = fuel_type)) +
  geom_line(size = 1) +
  scale_y_continuous(
    trans = "log10",  # Log-transform the Y-axis
    labels = label_comma()  # Show non-scientific notation values
  ) +
  labs(
    title = "Evolution of Vehicles per Fuel Type (Log Scale)",
    x = "Date",
    y = "Total Vehicles (Log Scale)",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )



# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Downloaded data --------------------------------------------------------

edgar_GHG_CO2_per_sector <- read_excel("1_raw_data/7_GHG_emissions/edgar_GHG_CO2_per_sector.xlsx")



# 3. Data manipulation ---------------------------------------------------------

# Filtering Brazil data from the dataset
BRA_CO2_per_sector <- edgar_GHG_CO2_per_sector %>% 
  filter(Country == "Brazil")

# Unpivoting data to use in plots
BRA_CO2_per_sector_longer <- BRA_CO2_per_sector %>% 
  select(-"EDGAR Country Code") %>% 
  pivot_longer(
    cols = "1970":"2023",
    values_to = "emissions"
  ) %>% 
  rename(year = name) %>% 
  mutate(year = as.numeric(year))

# Saving processed .csv
if (!file.exists(  "./3_processed_data/BRA_CO2_per_sector_longer.csv")) {
  write_csv(BRA_CO2_per_sector_longer,
            file = "./3_processed_data/BRA_CO2_per_sector_longer.csv")
  print("File saved in the repository.")
} else {
  print("File already exists in the repository.")
}

# Exploring sectors
BRA_CO2_per_sector_longer %>% 
  group_by(Sector) %>% 
  summarize(total = sum(emissions))




# 4. Plot ----------------------------------------------------------------------

## 4.1. Data on Brazilian Emissions --------------------------------------------

### GHG Emissions per sector ---------------------------------------------------

plot_trend_GHG_per_sector_yearly_BR <- 
  BRA_CO2_per_sector_longer %>% 
  ggplot(aes(x = year, y = emissions, color = Sector)) +
  geom_line(size = 1) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = label_number(accuracy = 0.01) # Adjust accuracy as needed
  ) +
  labs(title = "Yearly CO2 Emissions in Brazil by Sector",
       subtitle = "Source: EDGAR (Emissions Database for Global Atmospheric Research) Community GHG Database",
       x = "Year",
       y = "Emissions (Mt CO2eq/yr)",
       color = "Sector") +
  theme_bw() +
  facet_wrap(~Sector, nrow = 2) +
  theme(legend.position = "none")

if (!file.exists("./4_plots/plot_trend_GHG_per_sector_yearly_BR.png")) {
  ggsave("./4_plots/plot_trend_GHG_per_sector_yearly_BR.png",
         plot = plot_trend_GHG_per_sector_yearly_BR,
         width  = 10,
         height = 5)
  print("File saved in the repository.")
  } else {
    print("File already exists in the repository.")
    }


### Transport Sector only ------------------------------------------------------

plot_trend_GHG_transportation_yearly_BR <- 
  BRA_CO2_per_sector_longer %>% 
  filter(Sector == "Transport") %>% 
  ggplot(aes(x = year, y = emissions, color = Sector)) +
  geom_line(size = 1) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = label_number(accuracy = 0.01) # Adjust accuracy as needed
  ) +
  labs(title = "CO2 Emissions in Brazil by Sector",
       subtitle = "1971 to 2023",
       x = "Year",
       y = "Emissions (CO2)",
       color = "Sector") +
  theme_bw() +
  theme(legend.position = "none")


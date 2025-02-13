# 1. Packages ------------------------------------------------------------------

source("./2_code/00_packages.R")

# 2. Loading data sets ---------------------------------------------------------

## 2.1. Data from project pipeline ---------------------------------------------

fleet_2013_2024_clean <- read_csv("./3_processed_data/fleet_2013_2024_id_clean.csv")


# 3. Models --------------------------------------------------------------------

## 3.1. Regularized regression -------------------------------------------------

# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept



X <- model.matrix(electric ~ ., fleet_2013_2024_clean)[, -1]

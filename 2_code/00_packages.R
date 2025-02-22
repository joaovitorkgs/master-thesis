# Packages ---------------------------------------------------------------------

pacman::p_load(
  # Core Data Manipulation and Tidying
  dplyr,        # Data manipulation: filtering, summarizing, mutating, and more
  tidyr,        # Data tidying and reshaping: pivoting, handling missing values
  janitor,      # Clean messy data (e.g., column names) and create summary tables
  
  # Data Import and Export
  readr,        # Read flat files (CSV, TSV) into tibbles
  readxl,       # Read Excel files (.xls, .xlsx) into data frames
  basedosdados, # Access Brazilian public data via BigQuery
  httr,         # Perform HTTP requests to interact with web APIs
  archive,      # Extract files from compressed archives (.zip, .tar.gz)
  
  # Data Visualization
  ggplot2,      # Create customizable data visualizations using the grammar of graphics
  scales,       # Format axes and legends (e.g., percentages, currencies)
  viridis,      # Color palettes for data visualization (colorblind-friendly)
  RColorBrewer, # Additional color palettes for plots
  
  # String Manipulation
  stringr,      # Simple and consistent string manipulation functions
  stringi,      # Advanced string manipulation with Unicode support
  
  # Date-Time Handling
  lubridate,    # Simplify date and time parsing and manipulation
  
  # Geographic and Spatial Data
  geobr,        # Access Brazilian geographic data (e.g., shapefiles)
  sf,           # Handle spatial/geographic data in R
  
  # Reporting and Tables
  gt,           # Tables for reporting results
  stargazer,    # Generate regression tables in LaTeX/HTML/text formats
  
  # Machine Learning, Statistics, and Metrics
  caret,        # Comprehensive machine learning framework (classification/regression)
  Metrics,      # Evaluate model performance (e.g., RMSE, MAE)
  glmnet,       # Regularized generalized linear models (Lasso, Ridge)
  earth,        # Multivariate Adaptive Regression Splines
  vip,          # Variable Importance Plots
  pdp,          # Partial Dependence Plots
  car,          # Companion to Applied Regression (diagnostic functions)
  DescTools,    # Tools for descriptive statistics
  
  # Specialized Packages
  plm,          # Perform panel data analysis (fixed/random effects models)
  deflateBr,     # Deflate nominal Brazilian Reais using price indexes
  
  rsample
)


print("The packages have been successfully loaded.")


# Project Keys for Big Query imports -------------------------------------------

set_billing_id("central-stream-297218")
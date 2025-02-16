# Packages ---------------------------------------------------------------------

pacman::p_load(
  # Core Data Manipulation and Tidying
  dplyr,        # Data manipulation: filtering, summarizing, mutating, and more
  tidyr,        # Data tidying and reshaping: pivoting, handling missing values
  
  # Data Import
  readr,        # Read flat files (CSV, TSV) into tibbles
  readxl,       # Read Excel files (.xls, .xlsx) into data frames
  basedosdados, # Access Brazilian public data via BigQuery
  httr,         # Perform HTTP requests to interact with web APIs
  
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
  
  # Machine Learning and Metrics
  caret,        # Comprehensive machine learning framework (classification/regression)
  Metrics,      # Evaluate model performance (e.g., RMSE, MAE)
  
  # Utilities
  janitor,      # Clean messy data (e.g., column names) and create summary tables
  archive,      # Extract files from compressed archives (.zip, .tar.gz)
  
  # Specialized Packages
  plm           # Perform panel data analysis (fixed/random effects models)
)

print("The packages have been successfully loaded.")


# Project Keys for Big Query imports -------------------------------------------

set_billing_id("central-stream-297218")
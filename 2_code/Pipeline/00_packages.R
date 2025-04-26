# Packages ---------------------------------------------------------------------

pacman::p_load(
  
  # Core Data Manipulation and Tidying -----------------------------------------
  dplyr,        # Data manipulation: filter, summarize, mutate, and more
  forcats,      # Handle categorical variables (factors)
  janitor,      # Clean messy data and create summary tables
  tidyr,        # Data tidying: pivot, handle missing values, reshape
  
  # String Manipulation --------------------------------------------------------
  stringi,      # Advanced string manipulation with Unicode support
  stringr,      # Simple and consistent string manipulation functions
  
  # Date-Time Handling ---------------------------------------------------------
  lubridate,    # Simplify date and time parsing and manipulation
  
  # Data Import and Export -----------------------------------------------------
  archive,      # Extract files from compressed archives (.zip, .tar.gz)
  basedosdados, # Access Brazilian public data via BigQuery
  httr,         # Perform HTTP requests for web API interactions
  readr,        # Read flat files (CSV, TSV) into tibbles
  readxl,       # Read Excel files (.xls, .xlsx) into data frames
  
  # Geographic and Spatial Data ------------------------------------------------
  deflateBR,    # Deflate nominal Brazilian Reais using price indexes
  geobr,        # Access Brazilian geographic data (e.g., shapefiles)
  sf,           # Handle spatial/geographic data in R
  
  # Data Visualization ---------------------------------------------------------
  ggfortify,    # Unified plotting interface for stats objects
  ggplot2,      # Create customizable visualizations with grammar of graphics
  grid,         # Low-level graphics system
  gridExtra,    # Arrange multiple grid-based plots on a page
  patchwork,    # Combine ggplot2 plots easily
  RColorBrewer, # Additional color palettes for plots
  scales,       # Format axes and legends (percentages, currencies)
  viridis,      # Colorblind-friendly color palettes
  
  # Reporting and Tables -------------------------------------------------------
  gt,           # Create publication-quality tables
  stargazer,    # Generate regression tables in LaTeX/HTML/text formats
  
  # Machine Learning and Statistics --------------------------------------------
  car,          # Companion to Applied Regression (diagnostic functions)
  caret,        # Comprehensive machine learning framework
  DescTools,    # Tools for descriptive statistics
  earth,        # Multivariate Adaptive Regression Splines
  glmnet,       # Regularized generalized linear models (Lasso, Ridge)
  Metrics,      # Evaluate model performance (RMSE, MAE, etc.)
  pdp,          # Partial Dependence Plots
  rsample,      # Functions for resampling data
  vip,          # Variable Importance Plots
  
  # Time Series Analysis -------------------------------------------------------
  astsa,        # Applied statistical time series analysis
  fable,        # Modern time series modeling framework
  fable.prophet,# fable extension for prophet models
  forecast,     # Time series forecasting functions and models
  fpp3,         # Data and functions for "Forecasting: Principles and Practice" (3rd ed)
  plm,          # Panel data analysis (fixed/random effects models)
  prophet,      # Forecasting procedure for time series
  seasonal,     # Seasonal adjustment of time series
  tsibble,      # Tidy temporal data structures
  tsibbledata,  # Example time series datasets in tsibble format
  urca,         # Unit root and cointegration tests
  vars,         # VAR models for multivariate time series
  xts,          # Extensible time series class and methods
  
  # Miscellaneous --------------------------------------------------------------
  conflicted,   # Manage conflicts between functions from different packages
  grateful      # Citing R packages in this project
)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(forecast::gas)
conflicts_prefer(vars::VAR)
conflicts_prefer(fabletools::accuracy)
conflicts_prefer(prophet::prophet)

print("The packages have been successfully loaded.")


# Project Keys for Big Query imports -------------------------------------------

set_billing_id("central-stream-297218")
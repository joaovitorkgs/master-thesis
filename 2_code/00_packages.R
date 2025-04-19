# Packages ---------------------------------------------------------------------

pacman::p_load(
  # Core Data Manipulation and Tidying
  dplyr,        # Data manipulation: filter, summarize, mutate, and more
  tidyr,        # Data tidying: pivot, handle missing values, reshape
  janitor,      # Clean messy data and create summary tables
  forcats,      # Handle categorical variables (factors)
  
  # Data Import and Export
  readr,        # Read flat files (CSV, TSV) into tibbles
  readxl,       # Read Excel files (.xls, .xlsx) into data frames
  basedosdados, # Access Brazilian public data via BigQuery
  httr,         # Perform HTTP requests for web API interactions
  archive,      # Extract files from compressed archives (.zip, .tar.gz)
  
  # Data Visualization
  ggplot2,      # Create customizable visualizations with grammar of graphics
  scales,       # Format axes and legends (percentages, currencies)
  viridis,      # Colorblind-friendly color palettes
  RColorBrewer, # Additional color palettes for plots
  gridExtra,    # Arrange multiple grid-based plots on a page
  ggfortify,    # Unified plotting interface for stats objects
  grid,         # Low-level graphics system
  
  # String Manipulation
  stringr,      # Simple and consistent string manipulation functions
  stringi,      # Advanced string manipulation with Unicode support
  
  # Date-Time Handling
  lubridate,    # Simplify date and time parsing and manipulation
  
  # Geographic and Spatial Data
  geobr,        # Access Brazilian geographic data (e.g., shapefiles)
  deflateBR,    # Deflate nominal Brazilian Reais using price indexes
  sf,           # Handle spatial/geographic data in R
  
  # Reporting and Tables
  gt,           # Create publication-quality tables
  stargazer,    # Generate regression tables in LaTeX/HTML/text formats
  
  # Machine Learning and Statistics
  caret,        # Comprehensive machine learning framework
  Metrics,      # Evaluate model performance (RMSE, MAE, etc.)
  glmnet,       # Regularized generalized linear models (Lasso, Ridge)
  earth,        # Multivariate Adaptive Regression Splines
  vip,          # Variable Importance Plots
  pdp,          # Partial Dependence Plots
  car,          # Companion to Applied Regression (diagnostic functions)
  DescTools,    # Tools for descriptive statistics
  rsample,      # Functions for resampling data
  
  # Time Series Analysis
  plm,          # Panel data analysis (fixed/random effects models)
  forecast,     # Time series forecasting functions and models
  astsa,        # Applied statistical time series analysis
  xts,          # Extensible time series class and methods
  seasonal,     # Seasonal adjustment of time series
  urca,         # Unit root and cointegration tests
  fable,        # Modern time series modeling framework
  tsibble,      # Tidy temporal data structures
  prophet,      # Forecasting procedure for time series
  vars,
  conflicted,
  fable.prophet,
  fable,
  tsibbledata
)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(forecast::gas)
conflicts_prefer(vars::VAR)
conflicts_prefer(fabletools::accuracy)

print("The packages have been successfully loaded.")


# Project Keys for Big Query imports -------------------------------------------

set_billing_id("central-stream-297218")
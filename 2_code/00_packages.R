# Packages ---------------------------------------------------------------------

pacman::p_load(
  readr,        # Read CSV files
  readxl,       # Read Excel files
  dplyr,        # Data manipulation
  tidyr,        # Data tidying and reshaping
  ggplot2,      # Create data visualizations
  scales,       # Format axes and legends (percentages, currencies)
  stringr,      # String manipulation
  lubridate,    # Date and time manipulation
  basedosdados, # Access to Brazilian public data via BigQuery
  geobr,        # Brazilian geographic data
  sf,           # Handle spatial/geographic data
  viridis,      # Color palettes for data visualization
  stringi,      # Advanced string manipulation (e.g., Unicode support)
  gt,           # Create beautiful tables for reporting results
  webshot2,     # Capture screenshots of web pages or save HTML widgets as images
  RColorBrewer, # Additional color palettes for plotting
  httr,
  Metrics,
  caret,
  unrar,
  archive
)

print("The packages have been successfully loaded.")


# Project Keys for Big Query imports -------------------------------------------

set_billing_id("central-stream-297218")
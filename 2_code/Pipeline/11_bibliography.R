# 1. Packages and Pipeline Data ------------------------------------------------

source("./2_code/00_packages.R")

# 2. Generating citations ------------------------------------------------------

grateful::cite_packages(out.dir = "./7_bibliography")

renv::dependencies()

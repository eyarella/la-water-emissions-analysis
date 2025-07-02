# setup.R

# List of required packages
packages <- c("tidyverse", "lubridate", "scales")

# Install only if not already installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load packages
lapply(packages, library, character.only = TRUE)

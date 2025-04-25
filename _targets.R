# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    NULL
  )
)

# Run the R scripts in the R/ folder with custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Prepare the target list:
list(
  NULL
)

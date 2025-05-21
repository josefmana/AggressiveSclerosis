# Load packages required to define the pipeline:
library(tidyverse) |> suppressPackageStartupMessages()
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
  tar_target(
    name = data_file,
    command = here::here("data-raw", "pro Josef Mana imed export 28_3_2025 data.xlsx"),
    format = "file"
  ),
  tar_target(
    name = raw_data,
    command = prepare_data(data_file)
  ),
  tar_target(
    name = classified_data,
    command = determine_aggressive_phenotype(demographics = raw_data$id, relapses = raw_data$relapses, edss = raw_data$edss, eye_check = F) |> suppressWarnings()
  )
)

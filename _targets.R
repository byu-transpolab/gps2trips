library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/gps2trips.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse"))

# End this file with a list of target objects.
list(
  tar_target(raw_data_file,
             "C:/Users/Gillian/Downloads/Health Study_5f5184e73e2fd848eac22aec_passivelocation_65.csv",
             format = "file"),
  tar_target(raw_data, getData(raw_data_file)),
  tar_target(cleaned_data, cleanData(raw_data))
)

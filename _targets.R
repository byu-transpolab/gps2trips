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

input_files <- list.files("data")


# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse", "leaflet", "sf", "lwgeom", "zoo"))

# End this file with a list of target objects.
list(
tar_target(raw_data, getData("data/Health Study_5f5184e73e2fd848eac22aec_passivelocation_65.csv")),
  tar_target(cleaned_data, cleanData(raw_data)),
  tar_target(visual_map,plotData(cleaned_data)),
  tar_target(cumulative_distance,getTotalDistance(cleaned_data)),
  tar_target(distance_vs_time, plotTimeline(cumulative_distance)),
  tar_target(moving_average, getMovingAverage(cumulative_distance)),
  tar_target(list_of_activities, getActivities(moving_average))
)

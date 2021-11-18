library(targets)

source("R/gps2trips.R")
source("R/myAnalysis.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse", "leaflet", "sf", "gpsactivs", "dbscan"))

# End this file with a list of target objects.
list(
  tar_target(caps, makeCaps("data/SensorData-1596231685391.zip")),
  tar_target(clusters_per_date,caps_tr(caps)),
  tar_target(cluster_maps, makeMaps(clusters_per_date))
)


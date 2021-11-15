library(targets)

source("R/gps2trips.R")
source("R/MakeMaps.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse", "leaflet", "sf", "gpsactivs"))

# End this file with a list of target objects.
list(
  tar_target(caps, makeCaps("data")),
  tar_target(clusters_per_date,caps_tr(caps)),
  tar_target(raw_data_maps, makeRawDataMaps(clusters_per_date)),
  tar_target(cluster_maps, makeClusterMaps(clusters_per_date))
)


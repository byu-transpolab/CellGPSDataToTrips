library(targets)

source("R/gps2trips.R")
source("R/MakeMaps.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse", "leaflet", "sf", "gpsactivs"))

# End this file with a list of target objects.
list(
  tar_target(caps, makeCaps("data")),
  tar_target(clusters_per_date,caps_tr(caps, params)),
  tar_target(raw_data_maps, makeRawDataMaps(clusters_per_date)),
  tar_target(cluster_maps, makeClusterMaps(clusters_per_date)),
  # This is where I set the ranges of parameters I want to take random samples from
  tar_target(random_clusters, randomClusters(eps = c(1:40), minpts = c(1:10), delta_t = c(300:1200), 
                                           entr_t = c(0.5:2.50), ndraws = 5))
)


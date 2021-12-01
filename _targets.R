library(targets)

source("R/gps2trips.R")
source("R/MakeMaps.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse", "leaflet", "sf", "gpsactivs", "ggspatial"))

# End this file with a list of target objects.
list(
  tar_target(caps, makeCaps("data/SensorData-1617304845160.zip")),
  tar_target(random_clusters, randomClusters(caps = caps[2:5,], eps = c(1:40), minpts = c(1:10), delta_t = c(300:1200), ndraws = 5)),
  tar_target(params, random_clusters$params),
  tar_target(raw_data_maps, makeRawDataMaps(caps, random_clusters))
  #tar_target(cluster_maps, makeClusterMaps(clusters_per_date)),
  # This is where I set the ranges of parameters I want to take random samples from
  # tar_target(random_clusters, randomClusters(caps = caps[2:5,], eps = c(1:40), minPts = c(1:10), delta_t = c(300:1200), ndraws = 5))
)


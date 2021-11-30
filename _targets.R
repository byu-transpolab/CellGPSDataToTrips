library(targets)

source("R/gps2trips.R")
source("R/MakeMaps.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse", "leaflet", "sf", "gpsactivs"))

# End this file with a list of target objects.
list(
<<<<<<< HEAD
  tar_target(caps, makeCaps("data/SensorData-1617304845160.zip")),
  tar_target(clusters_per_date,caps_tr(caps,params)),
=======
  tar_target(caps, makeCaps("data/SensorData-1596231685391.zip")),
  tar_target(clusters_per_date,caps_tr(caps, params)),
>>>>>>> 679a72febf169a185c2cdc756575605d785f5627
  tar_target(raw_data_maps, makeRawDataMaps(clusters_per_date)),
  tar_target(cluster_maps, makeClusterMaps(clusters_per_date)),
  # This is where I set the ranges of parameters I want to take random samples from
  tar_target(random_clusters, randomClusters(eps = c(1:40), minpts = c(1:10), delta_t = c(300:1200), 
<<<<<<< HEAD
                                             entr_t = c(0.5:2.50), ndraws = 5))
=======
                                           entr_t = c(0.5:2.50), ndraws = 5))
>>>>>>> 679a72febf169a185c2cdc756575605d785f5627
)


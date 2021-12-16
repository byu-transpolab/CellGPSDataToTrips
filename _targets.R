library(targets)

source("R/gps2trips.R")
source("R/MyAnalysis.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse", "leaflet", "sf", "gpsactivs", "ggspatial", "data.table", "plotly"))

# End this file with a list of target objects.
list(
  tar_target(caps, makeCaps("data/SensorData-1596231685391.zip")),
  tar_target(random_clusters, randomClusters(caps, eps = 40, minPts = 20, delta_t = 400, entr_t = 1.0, ndraws = 20)),
  tar_target(params, random_clusters$params),
  tar_target(manualTable, getGeoJson(folder = "manual_clusters")),
  tar_target(matchStats, calculateMatchStats(random_clusters, manualTable)),
  tar_target(errorPlot, pctErrorPlot(matchStats))
)


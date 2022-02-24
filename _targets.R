library(targets)

source("R/gps2trips.R")
source("R/MyAnalysis.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", 
                            "tidyverse", "leaflet", "sf", "gpsactivs", 
                            "ggspatial", "data.table", "plotly"))

# End this file with a list of target objects.
list(
  tar_target(cleaned_data, makeCaps("data")),
  tar_target(manual_table, getGeoJson("manual_clusters")),
  tar_target(alg_manual_table, joinTables(manual_table, cleaned_data)),
  tar_target(optimized_params, optimize(alg_manual_table)),
  tar_target(num_clusters, make_clusters(optimized_params, alg_manual_table))
)


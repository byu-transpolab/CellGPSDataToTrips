library(targets)

source("R/Optimize.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", 
                            "tidyverse", "leaflet", "sf", "gpsactivs", 
                            "ggspatial", "data.table", "plotly"))

# End this file with a list of target objects.
list(
  tar_target(cleaned_data, cleanData("data")),
  tar_target(manual_table, makeManualTable("manual_clusters")),
  tar_target(cleaned_manual_table, joinTables(manual_table, cleaned_data)),
  tar_target(optimized_params, optimize(cleaned_manual_table)),
  tar_target(num_clusters, makeClusters(params = optimized_params$par, cleaned_manual_table))
)


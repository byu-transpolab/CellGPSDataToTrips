library(targets)

source("R/Optimize.R")
source("R/MakeMaps.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", 
                            "tidyverse", "leaflet", "sf", "gpsactivs", 
                            "ggspatial", "data.table", "plotly",
                            "viridis", "pomp"))

# End this file with a list of target objects.
list(
  tar_target(cleaned_data, cleanData("data")),
  tar_target(maps, makeAllMaps(cleaned_data)),
  tar_target(manual_table, makeManualTable("manual_clusters")),
  tar_target(cleaned_manual_table, joinTables(manual_table, cleaned_data)),
  tar_target(optimized_params, optimize(cleaned_manual_table)),
  tar_target(num_clusters, makeClusters(cleaned_manual_table = cleaned_data,
                                        params = optimized_params$par))
)


library(targets)

#source("R/gps2trips.R")
#source("R/MyAnalysis.R")
source("Optimize.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", 
                            "tidyverse", "leaflet", "sf", "gpsactivs", 
                            "ggspatial", "data.table", "plotly"))

# End this file with a list of target objects.
list(
  tar_target(cleaned_data, cleanData("data")),
  tar_target(algorithm_table, makeAlgorithmTable(params = c(1,2,3,4), cleaned_data[2,])),
  tar_target(manual_table, makeManualTable("manual_clusters")),
  tar_target(alg_manual_table, joinTables(manual_table, algorithm_table)),
  tar_target(optimized_params, optimize(alg_manual_table)),
  tar_target(num_clusters, makeClusters(optimized_params, alg_manual_table))
)


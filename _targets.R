library(targets)
library(future)
library(future.apply)
library(furrr)


source("R/Optimize.R")
source("R/MakeMaps.R")

tar_option_set(debug = "optimized_params")

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", 
                            "tidyverse", "leaflet", "sf", "gpsactivs", 
                            "ggspatial", "data.table", "plotly", 
                            "viridis", "pomp"))
plan(multisession)
# End this file with a list of target objects.
list(
  tar_target(cleaned_data, cleanData("data")),
  #tar_target(maps, makeAllMaps(cleaned_data)),
  tar_target(manual_table, makeManualTable("manual_clusters")),
  tar_target(cleaned_manual_table, joinTables(manual_table, cleaned_data)),
  tar_target(optimized_params, optimize(cleaned_manual_table)),
  tar_target(accurate_tibble, makeClusters(cleaned_manual_table = cleaned_data,
                                        params = optimized_params$par)),
  tar_target(num_trips, addNumtrips(accurate_tibble)),
  tar_target(activity_types, addTtripType(num_trips)),
  tar_target(final_table, addMentalHealthResponses(activity_types))
)

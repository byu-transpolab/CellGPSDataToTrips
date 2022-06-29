library(targets)

# parallelization
library(future)
library(future.apply)
library(furrr)
library(future.callr)
plan(callr)
threads <- future::availableCores() - 1



source("R/Optimize.R")
source("R/MakeMaps.R")

tar_option_set(debug = "cleaned_manual_table")


# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","tools", "hms", "lubridate", 
                            "tidyverse", "leaflet", "sf", "gpsactivs", 
                            "ggspatial", "data.table", "plotly", "future.apply",
                            "viridis", "pomp", "stats"))

# End this file with a list of target objects.
list(
  # Read in and clean GPS data
  tar_target(cleaned_data, cleanData("data/caps_data"), resources = tar_resources(
    future = tar_resources_future(resources = list(n_cores = threads))
  )),
  tar_target(manual_table, makeManualTable("data/manual_clusters")),
  
  # Join cleaned data and manual data to allow for calibration
  tar_target(cleaned_manual_table, joinTables(manual_table, cleaned_data)),
  
  
  #tar_target(maps, makeAllMaps(cleaned_data)),
  tar_target(optimized_sann_params, optimize(cleaned_manual_table)),
  tar_target(optimized_optim_params, optimize2(cleaned_manual_table))
  #tar_target(accurate_tibble, makeClusters(cleaned_manual_table = cleaned_data,
                                       #params = optimized_params$par))
  #tar_target(num_trips, addNumtrips(accurate_tibble)),
  #tar_target(activity_types, addTtripType(num_trips)),
  #tar_target(final_table, addMentalHealthResponses(activity_types))
)

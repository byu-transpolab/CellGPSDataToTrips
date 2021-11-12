library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

source("R/gps2trips.R")
source("R/MakeMaps.R")

#files_in_folder <- list.files("data")


# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr","hms", "lubridate", "tidyverse", "leaflet", "sf", "gpsactivs"))

# End this file with a list of target objects.
list(
  tar_target(caps, makeCaps("data")),
  tar_target(clusters_per_date,caps_tr(caps)),
  tar_target(raw_data_maps, makeRawDataMaps(clusters_per_date)),
  tar_target(cluster_maps, makeClusterMaps(clusters_per_date))
)


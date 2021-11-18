source("R/gps2trips.R")

library(targets)
library(tidyverse)

tar_load(cluster_maps)

findBestParams <- function(x){
  
manualClusters <- tibble(
  Date = cluster_maps$date[1:3], # Change this to be [1:10] once I have 10 dates in the data folder
  numClusters = c(1,3,5)) # Manually change these values to be corresponding clusters I see for each date

# Combine my comparison tibble with this manualClusters table by Date
}

source("R/gps2trips.R")

library(targets)
library(tidyverse)

tar_load(raw_data_maps)

findBestParams <- function(x){
  
raw_data_maps %>%
    filter(
      date = c(#list of 10 dates I am using
        )
    )
  
manualClusters <- tibble(
  Date = raw_data_maps$date,
  numClusters = c(1,3,5)) # Manually change these values to be corresponding clusters I see for each date

# Combine my comparison tibble with this manualClusters table by Date
}

source("R/gps2trips.R")

library(targets)
library(tidyverse)

tar_load(clusters_per_date)

clusters_per_date <- clusters_per_date %>% dplyr::filter(date == date)

makeRawDataMaps <- function(clusters_per_date) {
  clusters_per_date <- clusters_per_date %>% dplyr::filter(date == date)
  maps_per_date <- clusters_per_date %>%
    mutate(
      blankMap = map2(data, clusters, ~ ggplot()
                 + geom_sf(data = .x, color = "blue")
                 + labs(
                   title = "Raw GPS Data for",
                   subtitle = date,
                   x = "Longitude",
                   y = "Latitude",
                 )
      )
    )
}

makeClusterMaps <- function(clusters_per_date) {
  clusters_per_date <- clusters_per_date %>% dplyr::filter(date == date)
  maps_per_date <- clusters_per_date %>%
    mutate(
      clusterMap = map2(data, clusters, ~ ggplot()
                 + geom_sf(data = .x, color = "blue")
                 + geom_sf(data = .y, color = "green", size = 8)
                 + labs(
                   title = "Trips made on",
                   subtitle = date,
                   x = "Longitude",
                   y = "Latitude",
                 )
      )
    )
}

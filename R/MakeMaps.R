

makeRawDataMaps <- function(clusters_per_date) {
  clusters_per_date <- clusters_per_date %>% dplyr::filter(date == date)
  maps_per_date <- clusters_per_date %>%
    mutate(
      blankMap = map2(data, clusters, ~ ggplot() +
                        annotation_map_tile(type = "cartolight", zoom = 12) 
                        + theme(
                          axis.line = element_line(color = NA),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank()
                        )
                      + geom_sf(data = .x, color = "blue")
                      + labs(
                        title = "Raw GPS Data for",
                        subtitle = activityDay,
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
      clusterMap = map2(data, clusters, ~ ggplot() +
                          annotation_map_tile(type = "cartolight", zoom = 12) 
                        + theme(
                          axis.line = element_line(color = NA),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank()
                        )
                        + geom_sf(data = .x, color = "blue")
                        + geom_sf(data = .y, color = "green", size = 8)
                        + labs(
                          title = "Trips made on",
                          subtitle = activityDay,
                          x = "Longitude",
                          y = "Latitude",
                        )
      )
    )
}

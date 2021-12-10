

makeRawDataMaps <- function(caps, random_clusters) {
  caps <- caps %>%
    dplyr::filter(date == date) %>%
    filter(date == "2020-09-03")
  maps_per_date <- caps %>% mutate(
    clusters = random_clusters$clusters[1],
    data = random_clusters[[7]][[1]]$data[1]) %>%
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
                        subtitle = date,
                        x = "Longitude",
                        y = "Latitude",
                      )
      )
    )
}

makeClusterMaps <- function(caps, random_clusters) {
  random_clusters <- random_clusters %>% dplyr::filter(date == (random_clusters$clusters[[1]][[1]]))
  maps_per_date <- random_clusters$clusters %>%
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

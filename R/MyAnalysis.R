#' @param folder of GeoJSON files named by date in same format as caps
#' @return tibble of list of manual clusters
#' @details caps can be made and loaded using the _targets.R file
#' 
getGeoJson <- function(folder){
  files <- dir(folder)
  manualList <- lapply(files, function(file) {
    st_read(file.path(folder, file))
  }
  )
  tibble(date = as_date(substr(files, 1, 10)),
         manual = manualList)
}

#' @param random_clusters and manualTable targets
#' @return list of manual and algorithm clusters with errors, dates, and params
#' @details the output of this is also the matchStats target
#' 
calculateMatchStats <- function(random_clusters, manualTable) {
    random_clusters_dataframe <- random_clusters %>%
    mutate(
      joined_clusters = map(clusters, getErrors, manualTable = manualTable)
    ) %>% 
    select(
      eps, minpts, delta_t, entr_t, joined_clusters
    ) %>%
    unnest(cols = c(joined_clusters)) %>%
      setDT()
}

#' @param lists of manual clusters and algorithm clusters
#' @return integer error value
#' 
countClusters <- function(manual, clusters) {
  nrow(manual) - nrow(clusters)

}

#' @param lists of manual clusters and algorithm clusters
#' @return percent error value
#' 
countClustersPct <- function(manual, clusters) {
  (nrow(manual) - nrow(clusters)) / nrow(manual)
}

#' @param tibble of manual clusters and algorithm clusters
#' @return tibble including error integer and percent error
#' @details calculates the errors that get appended in the getMatchStats function
getErrors <- function(clusters, manualTable) {
  inner_join(manualTable, clusters, by = c("date", "id")) %>%
    mutate(
      error = map2(manual, clusters, countClusters),
      pctError = map2(manual, clusters, countClustersPct)
    )
}

#' @param matchStats target which is a list
#' @return interactive line plot of delta_t vs. percent error
#' @details includes a line of best fit/ trend line
pctErrorPlot <- function(matchStats) {
  matchStatsErrorsOnly <- matchStats %>%
    select(
      date, eps, minpts, delta_t, entr_t, error, pctError)
  # Make data long format
  matchStatsLong <- melt(setDT(matchStatsErrorsOnly), id.vars = c("date", "error", "pctError"), variable.name = "parameters")
  
  # Plot percent Error
    plot <- ggplot(matchStatsLong %>% filter(parameters == "delta_t", value <= 400), aes(col = as.factor(date))) +
     geom_point(aes(x = as.numeric(value), y = as.numeric(pctError)), size = 3)+
     geom_line(aes(x = as.numeric(value), y = as.numeric(pctError))) +
    geom_smooth(aes(x = as.numeric(value), y = as.numeric(pctError)), method = 'lm', formula = y ~ x, se = F, col = "black") +
    labs(
      x = "delta_t (seconds)",
      y = "Error (%)"
    ) +
    guides(col = guide_legend(title = "Date"))+
    theme_bw()
   
   # Interactive Plot
   ggplotly(plot)
}

makeMaps <- function(caps,id, mydate) {
  st_as_sf((caps %>% filter(id == id, date == mydate))$data[[1]], 
           coords = c("lon", "lat"), crs = 4326) %>%
    ggplot(aes(color = as.numeric(time))) + annotation_map_tile() + geom_sf() + labs(
      title = id,
      subtitle = mydate
    ) + scale_color_viridis()
}

getRandomDates <- function(caps) {
  myIDs <- unique(caps$id)
  randomID <- sample(myIDs, size = min(10,length(myIDs)))
  randomDate <- caps %>% filter(id %in% randomID) %>%
    group_by(id) %>% slice_sample(n = 3)
  return(randomDate)
}

makeAllMaps <- function(caps){
  myDays <- getRandomDates(caps)
  plots <- list()
  for(i in 1:nrow(myDays)){
    plots[[i]] <- makeMaps(myDays,myDays$id[i], myDays$date[i])
  }
}


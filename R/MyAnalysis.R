
getGeoJson <- function(folder){
  files <- dir(folder)
  manualList <- lapply(files, function(file) {
    st_read(file.path(folder, file))
  }
  )
  tibble(date = as_date(substr(files, 1, 10)),
         manual = manualList)
}

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

countClusters <- function(manual, clusters) {
  nrow(manual) - nrow(clusters)

}

countClustersPct <- function(manual, clusters) {
  (nrow(manual) - nrow(clusters)) / nrow(manual)
}

getErrors <- function(clusters, manualTable) {
  inner_join(manualTable, clusters, by = "date") %>%
    mutate(
      error = map2(manual, clusters, countClusters),
      pctError = map2(manual, clusters, countClustersPct)
    )
}

pctErrorPlot <- function(matchStats) {
  matchStatsErrorsOnly <- matchStats %>%
    select(
      date, eps, minpts, delta_t, entr_t, error, pctError)
  # Make data long format
  matchStatsLong <- melt(setDT(matchStatsErrorsOnly), id.vars = c("date", "error", "pctError"), variable.name = "parameters")
  
  # Plot percent Error
   plot <- ggplot(matchStatsLong %>% filter(parameters == "delta_t"), aes(col = as.factor(date))) +
    geom_point(aes(x = as.numeric(value), y = as.numeric(pctError))) +
    labs(
      x = "delta_t (seconds)",
      y = "Error (%)"
    ) +
    guides(col = guide_legend(title = "Date"))+
    theme_bw()
   
   # Interactive Plot
   ggplotly(plot)
}




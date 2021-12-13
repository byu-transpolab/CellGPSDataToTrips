
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


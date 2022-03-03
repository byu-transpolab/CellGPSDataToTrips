#' @param folder of GeoJSON files named by date in same format as caps
#' @return tibble of list of manual clusters
#' @details caps can be made and loaded using the _targets.R file
#' 
getGeoJson <- function(folder){
  files <- (dir(folder))
  manualList <- lapply(files, function(file) {
    st_read(file.path(folder, file))
  }
  )
  tibble(manual = manualList,
                   name_of_file = file_path_sans_ext(files)) %>% 
    separate(name_of_file, c("chardate", "id"), sep = c("_")) %>%
    mutate(date = as.Date(chardate))
  
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
  abs((nrow(manual) - nrow(clusters))) / nrow(manual)
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

combineTables <- function(manual_table,algorithm_table) {
  inner_join(manual_table, algorithm_table, by = c("date", "id"))
}

#' @param matchStats target which is a list
#' @return interactive line plot of delta_t vs. percent error
#' @details includes a line of best fit/ trend line
pctErrorPlot <- function(matchStats) {
  matchStatsLong <- matchStats %>%
    select(
      date, eps, minpts, delta_t, entr_t, error, pctError) %>%
    pivot_longer(cols = c("eps", "minpts", "delta_t", "entr_t"), names_to = "parameters", values_to = "value") %>%
    mutate(error = as.numeric(error),
           pctError = as.numeric(pctError))
 
  # Plot percent Error
    plot <- ggplot(matchStatsLong, aes(x = value, y = pctError)) +
     geom_point(size = 3)+
    # geom_line() +
    # geom_smooth(method = 'lm', formula = y ~ x, se = F, col = "black") +
    labs(
      x = "delta_t (seconds)",
      y = "Error (%)"
    ) +
      facet_wrap(~parameters, scales = "free_x") +
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

#' @param matchStats target which is a list
#' @return the sum of all the errors calculated from the difference
#' between algorithm clusters and manual_clusters
#' @details this summation is the value we will be minimizing in the optim() function
#' to find the best parameters

sumError <- function(matchStats) {
  # the Reduce() function allows for mathematical operations on and across
  # columns of a list 
  sumErrors <- abs(Reduce("+",matchStats$error))
  sumErrors
}

#' @param initial parameters to optimize params target, sumError function which
#' returns a scalar result (sumErrors), method used to minimize the error, lower
#' and upper limits for the parameters
#' @return a list of optimized parameters that minimize the sumErrors value
#' @details the optimized parameters are the ones to use in the algorithm from
#' here on out
optimizeParams <- function(par = params, fn = sumError, matchStats = matchStats,
                           method = "L-BFGS-B", lower = c(), upper = c()){
  optim(par, fn, method, lower, upper)
}


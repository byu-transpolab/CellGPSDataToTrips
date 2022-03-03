makeMaps <- function(cleaned_data,id, mydate) {
  st_as_sf((cleaned_data %>% filter(id == id, date == mydate))$data[[1]], 
           coords = c("lon", "lat"), crs = 4326) %>%
    ggplot(aes(color = as.numeric(time))) + annotation_map_tile() + geom_sf() + labs(
      title = id,
      subtitle = mydate
    ) + scale_color_viridis()
}

getRandomDates <- function(cleaned_data) {
  myIDs <- unique(cleaned_data$id)
  randomID <- sample(myIDs, size = min(10,length(myIDs)))
  randomDate <- cleaned_data %>% filter(id %in% randomID) %>%
    group_by(id) %>% slice_sample(n = 3)
  return(randomDate)
}

makeAllMaps <- function(cleaned_data){
  myDays <- getRandomDates(cleaned_data)
  plots <- list()
  for(i in 1:nrow(myDays)){
    plots[[i]] <- makeMaps(myDays,myDays$id[i], myDays$date[i])
  }
}



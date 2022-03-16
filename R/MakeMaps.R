#' Function to make maps of the cleaned GPS data
#'
#'
#' @param cleaned_data target
#' @return geom_sf map of raw GPS data
#' @details the GPS points are colored by time to help
#' visualize when activities were made and/or if multiple
#' activities were made to the same place at different times

makeMaps <- function(cleaned_data,id, mydate) {
  st_as_sf((cleaned_data %>% filter(id == id, date == mydate))$data[[1]], 
           coords = c("lon", "lat"), crs = 4326) %>%
    ggplot(aes(color = as.numeric(time))) + annotation_map_tile() + geom_sf() + labs(
      title = id,
      subtitle = mydate
    ) + scale_color_viridis()
}

#' Function to get randon IDs and Dates to map
#'
#'
#' @param cleaned_data target
#' @return tibble of 10 random IDs with 3 dates each
#' @details the returned tibble is the IDs and dates that will
#' be mapped in order to get random days and numbers of 
#' clusters

getRandomDates <- function(cleaned_data) {
  myIDs <- unique(cleaned_data$id)
  randomID <- sample(myIDs, size = min(10,length(myIDs)))
  randomDate <- cleaned_data %>% filter(id %in% randomID) %>%
    group_by(id) %>% slice_sample(n = 3)
  return(randomDate)
}

#' Function to clean GPS data
#'
#'
#' @param cleaned_data target
#' @return list of maps, specifically the "maps" target
#' @details makes a map for each randpm day for each 
#' of the 10 random IDs

makeAllMaps <- function(cleaned_data){
  myDays <- getRandomDates(cleaned_data)
  plots <- list()
  for(i in 1:nrow(myDays)){
    plots[[i]] <- makeMaps(myDays,myDays$id[i], myDays$date[i])
  }
  return(plots)
}



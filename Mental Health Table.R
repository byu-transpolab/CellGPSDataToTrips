#' @param accurate_tibble target
#' @return a table including ID, date, sf, algorithm, and the 
#' number of clusters as its own column

addNumTrips <- function(df){
  tibble <- as_tibble(df) %>%
    select(id, date, sf, algorithm) %>%
    group_by(id,date) %>%
    rowwise() %>%
    # Calculate the total number of trips someone made in a day
    mutate(numTrips = length(algorithm[[1]]))
  return(tibble)
}

#' @param output from addNumTrips function
#' @return a table including ID, date, sf, algorithm, the
#' number of total clusters as its own column, and how many of those
#'  clusters were at a park, library, or grocery store

addTripType <- function(tibble){
  
  ParkTrips <- st_within(tibble$algorithm[[6]], parksSf, sparse = F)
  GroceryTrips <- st_within(tibble$algorithm[[6]], grocerySf, sparse = F)
  LibraryTrips <- st_within(tibble$algoritm[[6]], libarySf, sparse = F)
  
  tibble %>% 
    rowwise() %>%
    mutate(
    numParkTrips = length(ParkTrips[ParkTrips == T]),
    numGroceryTrips = length(GroceryTrips[GroceryTrips == T]),
    numLibraryTrips = length(LibraryTrips[LibraryTrips == T])
  )
}

addMentalHealthResponses <- function(tibble){
  
}
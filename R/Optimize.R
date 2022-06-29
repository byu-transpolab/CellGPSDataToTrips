#' Function to clean GPS data
#' 
#' Read in raw GPS data for participants in the CAPS survey. Each file
#' contains the GPS points for a single day and a single respondent.
#'
#' @param folder of raw GPS data
#' @return tibble of cleaned data. Each row contains a nested sf points object.
#' @details cleaned_data can be made and loaded using the _targets.R file
cleanData <- function(folder, nfiles = NULL) {
  
  # get a list of all the files in the data folder
  files_in_folder <- dir(folder, full.names = T)
  if(is.null(nfiles)){ nfiles <- length(files_in_folder) }
  
  # loop through the files in the folder
  # this is an embarassingly parallel step implemented with the future library
  caps <- future_lapply(sample(files_in_folder, nfiles), function(x){
    
    # read CSV file and rename / simplify table
    readr::read_csv(x, col_types = list(userId = col_character())) %>%
      dplyr::transmute(
        id = userId,
        lat, lon,
        # Separate Date and Time columns
        timestamp = lubridate::as_datetime(timestamp),
        date = lubridate::date(timestamp),   
        minute = str_c(
          str_pad(lubridate::hour(timestamp), width = 2, pad = "0"),
          str_pad(lubridate::minute(timestamp), width = 2, pad = "0")
        )
      ) %>% 
      # Want to sample down, and get at most a few observations per minute
      # create a group for each minute
      arrange(date, minute) %>% group_by(date, minute) %>% 
      # sample 10 rows in that group, or as many rows as exist
      slice_sample(n = 10, replace = FALSE ) 
    
  }, future.seed = NULL) 
    
  caps %>%
    # combine all days for all participants into a single tibble
    dplyr::bind_rows() %>%
    ungroup %>%
    
    # The travel day is not the same as the calendar date, because people 
    # frequently are out past midnight. See the 'yesterday()' function for details.
    mutate(
      activityDay = yesterday(timestamp)
    )  %>%
    
    # want to have each group labeled by a date object, after grouping by the 
    # activityDay determined above
    group_by(activityDay) %>%
    mutate( date = min(date) ) %>%
    
    #' Make a nested tibble for each day / id combination
    arrange(timestamp) %>% group_by(id, date) %>%
    nest() %>% ungroup() %>%
    rename(cleaned = data) %>%
    dplyr::mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
    filter(num_points > 1000)  %>% 
    mutate(cleaned = purrr::map(cleaned, makeSf))
}

#' Function to compute meaningful day
#'
#'
#' @param timestamp vector of timestamp
#' @return the day number of the timestamp
#' @details if a timestamp occurs during midnight or 3 AM, it will be assigned to the
#' previous calendar date

yesterday <- function(timestamp){
  x <- case_when(
    lubridate::hour(timestamp)< 3~lubridate::day(timestamp)-1L,
    lubridate::hour(timestamp)< 3 & lubridate::month(timestamp) %in% c(10,5,7,12) & lubridate::day(timestamp) == 1 ~ 30L,
    lubridate::hour(timestamp)< 3 & lubridate::month(timestamp) %in% c(2,4,6,8,9,11,1) & lubridate::day(timestamp) == 1 ~ 31L,
    lubridate::hour(timestamp)< 3 & lubridate::month(timestamp) ==3 & lubridate::day(timestamp) == 1 ~ 28L,
    lubridate::hour(timestamp)< 3 & lubridate::month(timestamp) ==3 & lubridate::day(timestamp) == 1 & lubridate::leap_year(timestamp) ~ 29L,
    TRUE ~ lubridate::day(timestamp)
  )
  
  str_c(x, lubridate::month(timestamp), sep = "-")
  
}

#' Function to convert lat and long to geometric coordinates
#'
#'
#' @param cleaned tibble of GPS points 
#' @return nested column of GPS data where each GPS data is a tibble including 
#' information about the GPS point and the lat and long columns are replaced with a 
#' single geometric coordinates column called "geometry"

makeSf <- function(df) {
  df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4327) %>%
    st_transform(32612)
}

#' Function to compute number of clusters
#'
#'
#' @param cleaned tibble of GPS points and list of DBSCAN and entropy parameters
#' @return nested column of clusters where each cluster is a tibble including 
#' information about the cluster
#' @details param[1,2,3,4] are eps, minpts, delta_t, and entr_t respectively

makeClusters_1T <- function(df, params) {
  gpsactivs::dbscan_te(df, eps = params[1], minpts = params[2],
                       delta_t = params[3], entr_t = params[4])
}

makeClusters <- function(cleaned_manual_table, params) {
  print(params)
  write.table(params, 
              file = "sannbox_params.csv", 
              append = T, quote = F, row.names = F)
  cleaned_manual_table %>%
    ungroup() %>%
    mutate(algorithm = purrr::map(sf, makeClusters_1T, 
                           params = params))
}

#' Read manually-defined activities
#'
#' @param folder of GeoJSON files named by Date_ID that include the number of clusters
#' @return A nested tibble with person ID, date, and an SF points object.
#' 
#' 
makeManualTable <- function(folder){
  files <- dir(folder, pattern = ".geojson")
  manualList <- lapply(files, function(file) {
    st_read(file.path(folder, file)) %>%
      st_transform(32612)
  })
  
  tibble(manual = manualList,
         name_of_file = file_path_sans_ext(files)) %>% 
    separate(name_of_file, c("chardate", "id"), sep = c("_")) %>%
    mutate(date = as.Date(chardate)) %>%
    select(-c("chardate"))
}

#' Function to join the manual_table with the algorithm_table by ID and date
#'
#'
#' @param manual_table and algorithm_table targets
#' @return alg_manual_table target which includes the id, date
#' and the nested clusters column from both the manual table and algorithm table

# Remember to also group by id once I get manual_clusters that match what is in
# the data folder

joinTables <- function(manual_table,cleaned_data) {
  inner_join(manual_table, cleaned_data, by = c("date", "id")) %>%
    as_tibble()
}

#' Function to calculate the error between algorithm and manual clusters
#'
#'
#' @param alg_manual_table target and initial set of params as defined in the optims 
#' function
#' @return RMSE error integer
calculateError <- function(params, cleaned_manual_table) {
  test <- makeClusters(cleaned_manual_table, params) %>%
    filter(algorithm != "no clusters found")
  T2 <- test %>% mutate(diff = map2_dbl(manual, algorithm, clusterDistance))
  write.table(as.character(sum(T2$diff)), 
              file = "sannbox_error.csv", 
              append = T, row.names = F)
  sum(T2$diff)
}

clusterDistance <- function(manual, algorithm){
  if(nrow(manual)== 0 | nrow(algorithm) == 0) {
    r <- 9000
  } else {
    algorithm <- algorithm %>% arrange(start)
    r <- sum(st_distance(manual, algorithm, by_element = T)) 
  }
  r
}

#' Calculate percent of correctly classified points
#' 
#' @param manual_centers An sf object of activity locations determined through hand-coding
#' @param algorithm_centers An sf object of activity locations determined through applying
#'   the DBSCAN-TE algorithm
#' @param points An sf object containing raw GPS points for the activities and trips 
#'   represented in the centers.
#' @param buffer The radius of the activity locations. This should be the same
#'   units as the projection of each sf (usually meters).
#'   
#' @return The percent of `points` that disagree between inclusion in `manual_centers` and 
#'   `algorithm_centers`
#'   
#' @details This function draws a circular buffer of the prescribed radius around
#'   the activity centers determined by two methods, one manual and one algorithm-based.
#'   The points are identified as being within each of the buffers, and the function
#'   returns the percent of points that are classified differently based on the
#'   the buffers in both methods.
#'   
number_of_points_in_cluster <- function(manual_centers, algorithm_centers, points,
                                        buffer = 50){
  
  # create buffers around activity points
  manual_buffer <- st_buffer(manual_centers, buffer) %>% st_union()
  algorithm_buffer <- st_buffer(algorithm_centers, buffer) %>% st_union()
  
  # determine whether the points are inside each set of buffers
  agree <- points %>% 
    mutate(
      manual = st_within(geometry, manual_buffer, sparse = FALSE, )[, 1],
      algori = st_within(geometry, algorithm_buffer, sparse = FALSE)[, 1],
      
      # are they the same?
      agree = manual == algori
    ) 
  
  # calculate percent of FALSE agreement
  stat <- table(agree$agree)
  stat[1] / sum(stat)
  
  # map (for debugging)
  # pal <- colorFactor("Dark2", agree$agree)
  # leaflet() %>%
  #  addProviderTiles(providers$CartoDB) %>%
  #  addPolygons(data = manual_buffer %>% st_transform(4326), color = "red")  %>%
  #  addPolygons(data = algorithm_buffer%>% st_transform(4326), color = "green")  %>%
  #  addCircles(data = agree %>% st_transform(4326), color = ~pal(agree))
  
}

#' Function to minimize the RMSE between algorithm clusters and manual clusters
#' and find the optimum values for each parameters that does so
#'
#'
#' @param initial vector for params and the calculateError function to be minimized
#' @return vector of optimized parameters 
#' @details param[1,2,3,4] are eps, minpts, delta_t, and entr_t respectively

optimize <- function(cleaned_manual_table, params = c(25,60,320,2)) {
  sannbox(par = params, fn = calculateError, cleaned_manual_table = cleaned_manual_table,
        control = list(upper = c(100, 300, 24 * 3600, 4), lower = c(10,3,300, 1),
                       maxit = 55000))
}
  
optimize2 <- function(cleaned_manual_table, params = c(25,60,320,2)) {
  optim(par = params, fn = calculateError, cleaned_manual_table = cleaned_manual_table,
        method = "L-BFGS-B", upper = c(100, 300, 24 * 3600, 4), lower = c(10,3,300, 1))
}

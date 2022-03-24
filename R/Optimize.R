#' Function to clean GPS data
#'
#'
#' @param folder of raw GPS data
#' @return tibble of cleaned data CAPS
#' @details cleaned_data can be made and loaded using the _targets.R file

cleanData <- function(folder) {
  files_in_folder <- dir(folder, full.names = T)
  
  caps <- future_lapply(files_in_folder, function(x){
    readr::read_csv(x, col_types = list(userId = col_character())) %>%
      dplyr::transmute(
        id = userId,
        lat, lon,
        timestamp = lubridate::as_datetime(timestamp),
        date = date(timestamp),   # Separate Date and Time columns
        hour = hour(timestamp),
        minute = minute(timestamp),
        second = second(timestamp),
        time = hms::as_hms(str_c(hour, minute, second, sep = ":")),
      ) %>% select(-hour, -minute, -second)
  }, future.seed = NULL) %>%
    dplyr::bind_rows() %>%
    mutate(
      activityDay = yesterday(timestamp)
    )# %>%
    
   caps %>% 
    # Want to sample down, and get about 20 observations per minute
    # create a group for each minute
    mutate(min = str_c(str_pad(hour(timestamp), width = 2, pad = "0"),
                       str_pad(minute(timestamp), width = 2, pad = "0"),
                       str_pad(date(timestamp), width = 2, pad = "0"))) %>%
    # sample 20 rows in that group
    group_by(min) %>% slice_sample(n = 20) %>%
    
    # now we want to make sure that all the observations from one day 
    # are in a single sf object. So we group by ID and date. This is
    # the modified date that puts 12 AM to 3 AM on the previous day.
    arrange(timestamp) %>%
    group_by(id, date) %>%
    nest() %>%
    ungroup() %>%
    rename(cleaned = data) %>%
    mutate(n = map_int(cleaned, nrow)) %>%
    filter(n > 400) %>% # Removes dates with less than 400 points of data
    mutate(cleaned = map(cleaned, makeSf)) 
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
  cleaned_manual_table %>%
    ungroup() %>%
    mutate(algorithm = map(data, makeClusters_1T, params = params))
}

#' Function to convert GeoJSON files into manual clusters table
#'
#'
#' @param folder of GeoJSON files named by Date_ID that include the number of clusters
#' @return manual_table target which includes the id, date
#' and the nested clusters column

makeManualTable <- function(folder){
  files <- (dir(folder))
  manualList <- lapply(files, function(file) {
    st_read(file.path(folder, file))
  }
  )
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

#' Function to calculate the RMSE error between algorithm and manual clusters
#'
#'
#' @param alg_manual_table target and initial set of params as defined in the optims 
#' function
#' @return RMSE error integer

## Need to unnest alg_manual_table columns in order for this to work

calculateError <- function(params, cleaned_manual_table) {
  test <- makeClusters(cleaned_manual_table, params)
  T2 <- test %>% mutate(nm = map_int(manual, nrow), 
                  nal = map_int(algorithm, nrow))
  sum(T2$nal - T2$nm)^2
}

#' Function to minimize the RMSE between algorithm clusters and manual clusters
#' and find the optimum values for each parameters that does so
#'
#'
#' @param initial vector for params and the calculateError function to be minimized
#' @return vector of optimized parameters 
#' @details param[1,2,3,4] are eps, minpts, delta_t, and entr_t respectively

optimize <- function(cleaned_manual_table, params = c(10,3,300,1.0)) {
  optim(params, fn = calculateError, cleaned_manual_table = cleaned_manual_table,
        lower = c(10,3,300,1.0), upper = c(50,200,24*3600,4),
        method = "L-BFGS-B", control = list(maxit = 10))
}
  
  
  
  


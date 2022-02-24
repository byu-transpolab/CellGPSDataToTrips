#' Function to clean GPS data
#'
#'
#' @param folder of raw GPS data
#' @return tibble of cleaned data CAPS
#' @details cleaned_data can be made and loaded using the _targets.R file

cleanData <- function(folder) {
  files_in_folder <- dir(folder, full.names = T)
  caps <- lapply(files_in_folder, function(x){
    readr::read_csv(x, col_types = list(userId = col_character())) %>%
      dplyr::transmute(
        id = userId,
        lat, lon,
        timestamp = time,
        date = lubridate::date(timestamp),   # Separate Date and Time columns
        hour = lubridate::hour(timestamp),
        minute = lubridate::minute(timestamp),
        second = lubridate::second(timestamp),
        time = hms::as_hms(str_c(hour, minute, second, sep = ":")),
      ) %>% select(-hour, -minute, -second)
  }) %>%
    dplyr::bind_rows() %>%
    mutate(
      activityDay = yesterday(timestamp)
    ) %>%
    mutate(min = str_c(str_pad(hour(timestamp), width = 2, pad = "0"),
                       str_pad(minute(timestamp), width = 2, pad = "0"),
                       str_pad(date(timestamp), width = 2, pad = "0"))) %>%
    group_by(min) %>% slice_sample(n = 20) %>%
    arrange(timestamp) %>%
    group_by(id, date) %>%
    nest() %>%
    mutate(n = map(data, nrow)) %>%
    filter(n > 400)
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

makeClusters <- function(df, params) {
  gpsactivs::dbscan_te(df, eps = params[1], minpts = params[2],
                       delta_t = params[3], entr_t = params[4])
}

#' Function to mutate nested data and cluster columns onto cleaned tibble CAPS
#'
#'
#' @param cleaned tibble of GPS points and list of DBSCAN and entropy parameters
#' @return algorithm_table target which includes the date, number of GPS points
#' for that date, the nested data column, and the nested clusters column
#' @details the nested data and clusters column come from make_sf() and 
#' make_clusters() respectively

makeAlgorithmTable <- function(params, cleaned_data){
  print(params)
  clusters_tibble <- cleaned_data %>%
    ungroup() %>%
    mutate(data = map(data, makeSf)) %>%
    mutate(clusters = map(data, makeClusters, params = params))
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

joinTables <- function(manual_table,algorithm_table) {
  inner_join(manual_table, algorithm_table, by = c("date"))
}

#' Function to calculate the RMSE error between algorithm and manual clusters
#'
#'
#' @param alg_manual_table target and initial set of params as defined in the optims 
#' function
#' @return RMSE error integer

calculateError <- function(alg_manual_table, params) {
  clusters <- makeClusters(alg_manual_table, params)
  sum(clusters - alg_manual_table$manual)^2
}

#' Function to minimize the RMSE between algorithm clusters and manual clusters
#' and find the optimum values for each paramters that does so
#'
#'
#' @param initial vector for params and the calculateError function to be minimized
#' @return vector of optimized parameters 
#' @details param[1,2,3,4] are eps, minpts, delta_t, and entr_t respectively

optimize <- function(params = c(1,2,3,4), fn = calculateError) {
  optim(params, fn, method, upper = c(0,0,0,0), lower = c(100,100,100,100),
        method = "L-BFGS-B")
}
  
  
  
  


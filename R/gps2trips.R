## code to prepare `DATASET` dataset goes here

# CAPS DATA (CONFIDENTIAL) ==============
#
# The file is really a folder that contains the trace information for
# a single individual. Let's read all the CSV files in that folder


#' Function to clean GPS data
#'
#'
#' @param zip folder of raw GPS data
#' @return tibble of cleaned data CAPS
#' @details caps can be made and loaded using the _targets.R file

makeCaps <- function(folder) {
  files_in_folder <- unzip(folder, list = TRUE)$Name
  caps <- lapply(files_in_folder, function(x){
    readr::read_csv(unz(folder,x) , col_types = list(userId = col_character())) %>%
      dplyr::transmute(
        id = userId,
        lat, lon,
        timestamp,
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
    group_by(date) %>%
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
#' @param cleaned tibble of GPS points C
#' @return nested column of GPS data where each GPS data is a tibble including 
#' information about the GPS point and the lat and long columns are replaced with a 
#' single geometric coordinates column called "geometry"

make_sf <- function(df) {
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

make_clusters <- function(df, params) {
  print(params)
  gpsactivs::dbscan_te(df, eps = params[1], minpts = params[2],
                       delta_t = params[3], entr_t = params[4])
}

#' Function to mutate nested data and cluster columns onto cleaned tibble CAPS
#'
#'
#' @param cleaned tibble of GPS points and list of DBSCAN and entropy parameters
#' @return clusters_per_date target which includes the date, number of GPS points
#' for that date, the nested data column, and the nested clusters column
#' @details the nested data and clusters column come from make_sf() and make_clusters()
#' respectively

caps_tr <- function(params, caps){
  print(params)
  caps %>%
    mutate(data = map(data, make_sf)) %>%
    mutate(clusters = map(data, make_clusters, params = params))
  
  # creates clusters_per_date target
}

param_packer <- function(x, y, z, q){
  list(c(x, y, z, q)) 
}

randomClusters <- function(caps, eps = c(1:40), minPts = c(1:10), delta_t = c(300:1200), 
                           entr_t = c(0.5:2.50), ndraws = 5){
  
  # create a bunch of sets of parameters randomly.
  comparison <- tibble(eps = sample(eps, ndraws, replace = TRUE),
                       minpts = sample(minPts, ndraws, replace = TRUE),
                       delta_t = sample(delta_t, ndraws, replace = TRUE),
                       entr_t = sample(entr_t, ndraws, replace = TRUE)) %>%
    mutate(draw = row_number()) %>%
    rowwise() %>%
    mutate(params = param_packer(eps, minpts, delta_t, entr_t)) %>%
    ungroup() %>%
    mutate(
      clusters = map(params, caps_tr, caps = caps)
    )
  
  cluster_list <- lapply(comparison$params, function(p) {
    str(p)
    caps_tr(caps, p)
  })
  
}



## code to prepare `DATASET` dataset goes here

# CAPS DATA (CONFIDENTIAL) ==============
#
# The file is really a folder that contains the trace information for
# a single individual. Let's read all the CSV files in that folder

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

make_sf <- function(df) {
  df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4327) %>%
    st_transform(32612)
}

# param[1,2,3,4] are eps,minpts, delta_t, and entr_t respectively

 make_clusters <- function(df, params = c(25,4,300,0.5)) {
  gpsactivs::dbscan_te(df, params[1], params[2],
                       params[3], params[4])
}

caps_tr <- function(caps, params){
  caps %>%
  mutate(data = map(data, make_sf),
         clusters = map2(data, params, make_clusters))

  # creates clusters_per_date target
}

randomClusters <- function(caps, eps = c(1:40), minpts = c(1:10), delta_t = c(300:1200), 
                            entr_t = c(0.5:2.50), ndraws = 5){
  
  # create a bunch of sets of parameters randomly.
  comparison <- tibble(eps = sample(eps, ndraws, replace = TRUE),
              minpts = sample(minpts, ndraws, replace = TRUE),
              delta_t = sample(delta_t, ndraws, replace = TRUE),
              entr_t = sample(entr_t, ndraws, replace = TRUE)) %>%
    mutate(draw = row_number())
  
  comparison$param <- lapply(1:ndraws, function(i){
    c(comparison$eps[i], comparison$minpts[i], comparison$delta_t[i], comparison$entr_t[i]) 
  }) 
  caps_tr(caps, params = comparison$param[1])
  
  caps %>% mutate(
    clusters = map2(data, comparison$params[1], caps_tr)
  )
  comparison$cluster <- map2(caps, comparison$param, caps_tr)
}

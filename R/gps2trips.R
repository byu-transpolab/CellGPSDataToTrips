## code to prepare `DATASET` dataset goes here

# CAPS DATA (CONFIDENTIAL) ==============
#
# The file is really a folder that contains the trace information for
# a single individual. Let's read all the CSV files in that folder

makeCaps <- function(folder) {
  files_in_folder <- dir(folder, full.names = TRUE)
  caps <- lapply(files_in_folder, function(x){
    readr::read_csv(x , col_types = list(userId = col_character())) %>%
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
    ) 
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

 make_clusters <- function(df) {
  gpsactivs::dbscan_te(df, eps = 25, minpts = 4,
                       delta_t = 300, entr_t = 0.5)
}

caps_tr <- function(caps){
  caps %>%
  #filter(date(date) %in% as_date(c("2021-02-23", "2021-02-24", "2021-02-16"))) %>%
  mutate(min = str_c(str_pad(hour(timestamp), width = 2, pad = "0"),
                     str_pad(minute(timestamp), width = 2, pad = "0"),
                     str_pad(date(timestamp), width = 2, pad = "0"))) %>%
  group_by(min) %>% slice_sample(n = 20) %>%
  arrange(timestamp) %>%
  group_by(date) %>%
  nest() %>%
  mutate(n = map(data, nrow)) %>%
  filter(n > 400) %>%
  mutate(data = map(data, make_sf),
         clusters = map(data, make_clusters))

  # creates clusters_per_day target
}

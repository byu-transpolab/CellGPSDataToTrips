source("R/gps2trips.R")

library(targets)
library(tidyverse)

tar_load(raw_data_maps)

comparison <- tibble(date = random_clusters[[7]][[1]]$date,
                     algClusters = lapply(date, function(i){
                       nrow((random_clusters[[7]][[1]]$clusters[i]))
                     })
)



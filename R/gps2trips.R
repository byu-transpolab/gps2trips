## code to prepare `DATASET` dataset goes here

# Libraries
library(tidyverse)
library(lubridate)
library(dbscan)
library(sf)
library(leaflet)


# CAPS DATA (CONFIDENTIAL) ==============
#
# The file is really a folder that contains the trace information for
# a single individual. Let's read all the CSV files in that folder

files_in_folder <- dir("C:/Users/Gillian/Downloads/Research/gps2trips/data", full.names = TRUE)
caps <- lapply(files_in_folder, function(x){
  readr::read_csv(x, col_types = list(userId = col_character())) %>%
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
  dplyr::bind_rows()

make_sf <- function(df) {
  df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4327) %>%
    st_transform(32612)
}

 make_clusters <- function(df) {
  gpsactivs::dbscan_te(df, eps = 25, minpts = 4,
                       delta_t = 300, entr_t = 0.5)
}

caps_tr <- caps %>%
  filter(date(date) %in% as_date(c("2021-02-23", "2021-02-24", "2021-02-25"))) %>%
  mutate(min = str_c(str_pad(hour(timestamp), width = 2, pad = "0"),
                     str_pad(minute(timestamp), width = 2, pad = "0"),
                     str_pad(date(timestamp), width = 2, pad = "0"))) %>%
  group_by(min) %>% slice_sample(n = 20) %>%
  arrange(timestamp) %>%
  group_by(date) %>%
  nest() %>%
  mutate(data = map(data, make_sf),
         clusters = map(data, make_clusters))

map_clusters <- function(caps_tr, date) {
  df <- caps_tr %>% filter(date == date)
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(data = df$data[[1]] %>% st_transform(4326)) %>%
  addCircleMarkers(data = df$clusters[[1]] %>% st_transform(4326),color = "red")
}



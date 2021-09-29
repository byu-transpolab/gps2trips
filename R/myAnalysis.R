library(tidyverse)
library(lubridate)
library(dbscan)
library(sf)
library(leaflet)
library(targets)

tar_make()
tar_load(clusters_per_date)

clusters_per_date <- clusters_per_date %>%
  dplyr::filter(date == date)

maps_per_date <- clusters_per_date %>%
  mutate(
    map = map2(data, clusters, ~ ggplot()
               + geom_sf(data = .x, color = "blue")
               + geom_sf(data = .y, color = "green", size = 8)
               + labs(
                 title = "Trips made on",
                 subtitle = date,
                 x = "Longitude",
                 y = "Latitude",
               )
    )
  )

print(maps_per_date$map)

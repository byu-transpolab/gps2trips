# everything this function does goes in here.
# load the necessary packages
# remember to also load the targets package in the console if necessary

library(lubridate)
library(tidyverse)
library(hms)
library(dplyr)
library(sf)
library(leaflet)
library(lwgeom)
library(dbscan)
library(plotly)

#' Function to read GPS points into trips

#' Calculate the distance the person traveled using the latitude and longitude
#' values and Halversine formula
#' @param two latitude and two longitude values from raw data
#' @return distance traveled in meters

distanceTraveled <- function(lat,lon,lat1,lon1) {
  rad <- pi/180
  a1 <- lat*rad
  a2 <- lon*rad
  b1 <- lat1*rad
  b2 <- lon1*rad
  dlon <- b2- a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2*atan2(sqrt(a), sqrt(1 - a))
  R <- 6378137 # Avg radius of earth in m
  d <- R*c  # Distance in meters
  return(d)
}

#' @param raw_file Path to raw file in local directory
#' @return A tibble with raw gps data

getData <- function(input_files) {
  lapply(input_files, function(file){
    read_csv(file)
  }) %>%
    bind_rows()
}

#' @param Raw GPS data as read in with 'getData()'
#' @return A clean tibble with only selected variables and no outliers
cleanData <- function(raw_data) {

  raw_data %>%
    group_by(userId) %>%
    arrange(timestamp) %>%
    #slice(1:8000) %>%
    # clean up times as lubridate objects
    mutate(
      Date = lubridate::date(timestamp),   # Separate Date and Time columns
      hour = lubridate::hour(timestamp),
      minute = lubridate::minute(timestamp),
      second = lubridate::second(timestamp),
      Time = hms::as_hms(str_c(hour, minute, second, sep = ":")),
    ) %>%

    # convert to SF data frame
    ungroup() %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)  %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2],
      lon1 = lead(lon),
      lat1 = lead(lat)
    ) %>%

    # calculate elapsed time and distances
    mutate(
      userId,
      timestamp,
      TimeDifference = lead(timestamp)- timestamp, # Time difference between each GPS data point
      lat, lon, lat1, lon1,
      geometry,
    ) %>%
    rowwise() %>%
    mutate(
      distance_Meters = distanceTraveled(lat, lon, lat1, lon1),
      speed = distance_Meters / as.numeric(TimeDifference)
    ) %>%
    #filter(speed < 36, altitude < 1700) %>%      # Highest speed limit in Utah Valley is 80 mph = 36 m/s
    select(userId,timestamp,Date,Time,TimeDifference,lat, lon, distance_Meters)
}

#' @param cleaned data frame from cleanData function
#' @return world geographic map showing GPS points

plotGPSData <- function(cleaned_data) {
  sf_Data <- st_as_sf(cleaned_data,coords = c("lon","lat"))
  leaflet(sf_Data) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addCircleMarkers()
}

#' Where the slope of this line is zero is likely where a trip destination is
#' @param cleaned data from the cleanData function
#' @return line graph of distance traveled over time

plotTimeline <- function(cleaned_data) { # This is the slope of the plotTimeline curve
  cumulative_distance <- cleaned_data %>%
    ungroup %>%
    group_by(Date) %>%
    arrange(timestamp)%>%
    mutate(totalDistance = cumsum(distance_Meters))
  ggplot(cumulative_distance, aes(x=Time,y=totalDistance, color = factor(Date))) +
    xlab("Time(s)") +ylab("Total Distance(m)") +
    geom_line()
}

#' Transform Lat and Lon into different units and select a particular date to look at
#' @param cleaned data from the cleanData function
#' @return filtered data frame used for plotting clusters

getClusterData <- function(cleaned_data) {
  cluster_data <- cleaned_data %>%
    filter(Date == "2021-02-23") %>%
    st_transform(2280)

  cluster_data$Time = as.numeric(cluster_data$Time)
  cluster_data$x = st_coordinates(cluster_data)[,1]
  cluster_data$y = st_coordinates(cluster_data)[,2]

  cluster_data <- cluster_data %>%
    select(Date,Time,x,y)
}

#' Plot the cluster data in three dimensions
#' @param cluster data from the getClusterData function
#' @return three dimensional plot with color-coded clusters

plot3DClusters <- function(cluster_data) {
  numClusters <- dbscan(data.frame(Time = cluster_data$Time, x = cluster_data$x,
                                   y = cluster_data$y), eps = 200, minPts = 300)

  numClusters$cluster <- as.factor(numClusters$cluster)

  plot_ly(cluster_data, x = cluster_data$x - mean(cluster_data$x),
          y =cluster_data$y - mean(cluster_data$y),
          z = cluster_data$Time/3600,
          color = numClusters$cluster) %>%
    add_markers() %>%
    layout(scene=list(xaxis = list(title = 'Latitude'),
                      yaxis = list(title = 'Longitude'),
                      zaxis = list(title = 'Time')))
}


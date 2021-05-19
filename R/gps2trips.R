# everything this function does goes in here.
# load the necessary packages
library(lubridate)
library(tidyverse)
library(hms)
library(dplyr)
library(sf)
library(leaflet)
library(lwgeom)

#' Function to read GPS points into trips
#'
#' @param df A dataframe containing a stream of GPS points
#' @param x The name of the x-coordinate variable in df
#' @param y The name of the y-coordinate variable in df
#'
#'
#' @examples
#' # examples of how to use this function
#'
#' @export
#'
gps2trips <- function(df, x = "x", y = "y") {}


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
  R <- 6378137 # Avg radius of earth in km
  d <- R*c  # Distancein meters
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
#' @return A clean tibble with only selected variables
cleanData <- function(raw_data) {

    raw_data %>%
    group_by(userId) %>%
    arrange(timestamp) %>%
    slice(1:8000) %>%
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
      distance_Meters = distanceTraveled(lat, lon, lat1, lon1)
    ) %>%
    select(userId,Date,Time,TimeDifference,distance_Meters)
}

#' @param cleaned data frame from cleanData function
#' @return world geographic map showing GPS points

plotData <- function(cleaned_data) {
  sf_Data <- st_as_sf(cleaned_data,coords = c("lon","lat"))
  leaflet(sf_Data) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addCircleMarkers()
}

#' Where the slope of this line is zero is likely where a trip destination is
#' @param cumulative distance calculated from getTotalDistance function
#' @return line graph of distance traveled over time

plotTimeline <- function(cumulative_distance) {
  ggplot(cumulative_distance, aes(x=Time,y=totalDistance)) + xlab("Time(s)") +ylab("Total Distance(m)") +
    geom_line()
}

#' @param cleaned data fram from cleanData function
#' @return total distance to use in plotData function

getTotalDistance <- function(cleaned_data) { # This is the slope of the plotTimeline curve
    cleaned_data %>%
    ungroup %>%
    mutate(
      totalDistance = cumsum(distance_Meters)
    )
}

# Figure out a way to isolate where the slopes are zero or isolate the "columns"
# where the slope is nearly infinite

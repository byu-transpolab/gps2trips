# everything this function does goes in here.
# load the necessary packages
library(lubridate)
library(tidyverse)
library(hms)
library(dplyr)
library(sf)
library(leaflet)

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
#' @param two latitude and two longitude values from the separated date and
#' time tibble as created by 'separateDateandTime()'
#' @return distance traveled in meters
distanceTraveled <- function(lat,lon,lat1,lon1) {
  R <- 6371 # Earth mean radius (km)
  delta.lon <- (lon1-lon)
  delta.lat <- (lat1-lat)
  a <- sin(delta.lat/2)^2 + cos(lat) * cos(lat1) *sin(delta.lon/2)^2
  c <- 2 * a*sin(min(1,sqrt(a)))
  d <- R * c * 1000
  return (d) # Distance in m
}

#' @param raw_file Path to raw file in local directory
#' @return A tibble with raw gps data

getData <- function(input_files) {
  lapply(input_files, function(file){
    read_csv(file)
  }) %>%
    bind_rows()
}

#' Separate date and time into separate columns
#' @param Raw GPS data as read in with 'getData()'
#' @return A clean tibble with only selected variables
cleanData <- function(raw_data) {

  # empty coordinate point to use for lead/lag distances
  empty <- st_as_sfc("POINT(EMPTY)", crs = 4326)

  x <- raw_data %>%
    group_by(userId) %>%
    arrange(timestamp) %>%
    slice(1:100) %>%
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
    transmute(
      userId,
      timestamp,
      TimeDifference = lead(timestamp)- timestamp, # Time difference between each GPS data point
      lat, lon, lat1, lon1,
      distance_new =  sf::st_distance(
         geometry, lead(geometry, default = empty),
         by_element = TRUE),
      #speed = distance_Meters / as.numeric(TimeDifference),
      geometry
    ) %>%
    rowwise() %>%
    mutate(
      distance_Meters = distanceTraveled(lat, lon, lat1, lon1)
    )

  x
}

plotData <- function(cleaned_data) {
  sf_Data <- st_as_sf(cleaned_data,coords = c("lon","lat"))
  leaflet(sf_Data) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircleMarkers()
}

# Where the slope of this line is zero is likely where a trip destination is
plotTimeline <- function(df) {
  ggplot(df, aes(x=Time,y=totalDistance)) + xlab("Time(s)") +ylab("Total Distance(m)") +
    geom_line()
}

getCumSpeed <- function(cleaned_data) { # This is the slope of the plotTimeline curve
  cleaned_data %>%
    ungroup() %>%
    mutate(
      totalDistance = cumsum(distance_Meters),
      cumspeed = lead(totalDistance)-totalDistance/as.integer(TimeDifference)
    ) %>%
    filter(cumspeed >= 0) %>%
    group_by(lat,lon) %>%
    arrange(cumspeed)
}

#Figure out a way to isolate where the slopes are zero or isolate the "columns"
# where the slope is nearly infinite

histCumSpeed <- function(cumulativespeeds) {
  hist(cumulativespeeds, xlab ="Cumulative speeds (m/s)", ylab = "Frequency", col=blues9)
}

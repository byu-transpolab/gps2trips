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

  # everything this function does goes in here.
  # install the necessary packages

library(lubridate)
library(tidyverse)
library(hms)
library(dplyr)

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

  getData <- function(raw_file) {
    read_csv(raw_file)
  }

#' Separate date and time into separate columns
#' @param Raw GPS data as read in with 'getData()'
#' @return A clean tibble with only selected variables

  cleanData <- function(raw_data) {
    raw_data %>%
    arrange(timestamp) %>%
    mutate(
      Date = lubridate::date(timestamp),   # Separate Date and Time columns
      hour = lubridate::hour(timestamp),
      minute = lubridate::minute(timestamp),
      second = lubridate::second(timestamp),
      Time = hms::as_hms(str_c(hour, minute, second, sep = ":")),
    ) %>%
     # group_by(Date, minute) %>%
    #sample_frac(.1) %>%
      group_by(Date) %>%
      arrange(Time) %>%
      mutate(
        lat1=lead(lat),
        lon1 = lead(lon),
        TimeDifference = lead(Time)-Time # Time difference between each GPS data point
      ) %>%
    rowwise %>%
    mutate (
      distance_Meters = distanceTraveled(lat,lon,lat1,lon1), # Distance in meters between in each GPS data point
      actual_speed = distance_Meters/as.integer(TimeDifference)  # Speed at each GPS data point
    ) %>%
    select(userId,deviceId,Date,Time,lat,lon,distance_Meters,TimeDifference,actual_speed)  # Select variables we want
}

plotData <- function(x) {
  sf_Data <- st_as_sf(x,coords = c("lon","lat"))
  leaflet(sf_Data) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircleMarkers()
}

getCumSpeed <- function(cleaned_data) { # This is the slope of the plotTimeline curve
    cleaned_data %>%
    ungroup() %>%
    mutate(
      totalDistance = cumsum(distance_Meters),
      cumspeed = lead(totalDistance)-totalDistance/as.integer(TimeDifference)
    )
}

# Where the slope of this line is zero is likely where a trip destination is
plotTimeline <- function(df) {
  ggplot(df, aes(x=Time,y=totalDistance)) + xlab("Time(s)") +ylab("Total Distance(m)") +
    geom_line()
}

#Figure out a way to isolate where the slopes are zero or isolate the "columns"
# where the slope is nearly infinite

histCumSpeed <- function(cumulativespeeds) {
  hist(cumulativespeeds)
}

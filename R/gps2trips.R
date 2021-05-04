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
gps2trips <- function(df, x = "x", y = "y") {

  # everything this function does goes in here.
  # install the necessary packages

library(lubridate)
library(tidyverse)
library(hms)
library(dplyr)

  # read in the dataset csv file for each person

  individualDataSet <- read_csv("C:/Users/gillian4/Downloads/Student Health Data/Health Study_5f5184e73e2fd848eac22aec_passivelocation_65.csv")

  # separate date and time into separate columns
  # select variables from data we are analyzing

  separateDateandTime <- individualDataSet %>%
    arrange(timestamp) %>%
    mutate(
      Date = lubridate::date(timestamp),
      hour = lubridate::hour(timestamp),
      minute = lubridate::minute(timestamp),
      second = lubridate::second(timestamp),
      Time = hms::as_hms(str_c(hour, minute, second, sep = ":"))
    )

 # Calculate the distance the person traveled using the latitude and longitude values
 # Haversine Formula

    distanceTraveled <- function(lat,lon,lat1,lon1) {
    R <- 6371 # Earth mean radius (km)
    delta.lon <- (lon1-lon)
    delta.lat <- (lat1-lat)
    a <- sin(delta.lat/2)^2 + cos(lat) * cos(lat1) *sin(delta.lon/2)^2
    c <- 2 * a*sin(min(1,sqrt(a)))
    d <- R * c * 1000
    return (d) # Distance in m
    }

  delta.time <- function(Time, time1) {
   t = time1 - Time
   return (t)
  }

  getTimeDifference <- separateDateandTime %>%
      group_by(Date) %>%
      mutate(
        time1 = lead(Time)
      ) %>%
      mutate (
        TimeDifference_Seconds = delta.time(Time,time1)
      )

   getDistance <- getTimeDifference %>%
      mutate(
        lat1 = lead(lat),
        lon1 = lead(lon),
      ) %>%
      rowwise %>%
      mutate (
       distance_Meters = distanceTraveled(lat,lon,lat1,lon1)
      )


   # Calculate the speed by doing the distance traveled/ time
   # Speed is in m/s
   # Append this speed onto the final cleaned data set
   # Cleaned data set only includes all of the variables we are interested in looking at for this project

Cleaned_DataSet <- getDistance %>%
  mutate(
    actual_speed = distance_Meters%/%as.integer(TimeDifference_Seconds)  # speed rounds to the nearest integer
  ) %>%
  select(userId,deviceId,Date,Time,altitude,lat,lon,distance_Meters,TimeDifference_Seconds,actual_speed)

# All the code up until this point is working. The next step is figuring out headways (?)


hist(as.numeric(Cleaned_DataSet$TimeDifference_Seconds))
ggplot(Cleaned_DataSet, aes(x=lon, y=lat, color=actual_speed)) + geom_point()

library(sf)
library(leaflet)
sf_Data <- st_as_sf(Cleaned_DataSet,coords = c("lon","lat"))

leaflet(sf_Data) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircleMarkers()

targets::tar_script()
targets::tar_edit()

1}


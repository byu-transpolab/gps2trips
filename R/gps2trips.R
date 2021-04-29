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

library(lubridate)
library(tidyverse)
library(hms)
library(dplyr)

  # read in the dataset csv file for each person

  individualDataSet <- read_csv("C:/Users/Gillian/Downloads/Health Study_5f5184e73e2fd848eac22aec_passivelocation_65.csv")

  # separate date and time into separate columns
  # select variables from data we are analyzing

  separateDateandTime <- individualDataSet %>%
    mutate(
      Date = lubridate::date(timestamp),
      hour = lubridate::hour(timestamp),
      minute = lubridate::minute(timestamp),
      second = lubridate::second(timestamp),
      Time = hms::as_hms(str_c(hour, minute, second, sep = ":"))
    ) %>%
    select(Date,Time) # See the separated columns

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

    getDistance <- separateDateandTime %>%
      mutate(
        lat1 = lead(lat),
        lon1 = lead(lon),
      ) %>%
      rowwise %>%
      mutate (
        distance_Meters = distanceTraveled(lat,lon,lat1,lon1)
      ) %>%
    select(lat,lon,distance_Meters) # See the latitude, longitude, and distance between each point

   # Find the difference between times

  delta.time <- function(Time, time1) {
   t = time1 - Time
   return (t)
}
    getTimeDifference <- separateDateandTime %>%
      mutate(
        time1 = lead(Time),
      ) %>%
      mutate (
        TimeDifference_Seconds = delta.time(Time,time1)
      ) %>%
      select(Time,TimeDifference_Seconds) # See the time of day and time difference


   # Calculate the speed by doing the distance traveled/ time

     DeltaTime <-getTimeDifference$TimeDifference_Seconds # Giving an error for some reason
     Distance <-getDistance$distance_Meters

  # Add everything back into the original table (How?)

  }








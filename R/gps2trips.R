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

  # read in the dataset csv file for each person

  individualDataSet <- read_csv("C:/Users/Gillian/Downloads/Health Study_5f5184e73e2fd848eac22aec_passivelocation_65.csv")

  # separate date and time into separate columns
  # select variables from data we are analyzing

  cleaned_DataSet <- individualDataSet %>%
    mutate(
      Date = lubridate::date(timestamp),
      hour = lubridate::hour(timestamp),
      minute = lubridate::minute(timestamp),
      second = lubridate::second(timestamp),
      Time = hms::as_hms(str_c(hour, minute, second, sep = ":"))
    ) %>%
    select(userId, deviceId, Date, Time, lat, lon, altitude, speed)

  # A trip is defined as when someone is within the same x
  # range of latitude for x number of minutes

  # This code divides the table up into trips using ^^
  # definition of a trip

  # One degree of latitude is approximately 69 miles

stagnantPoints <- cleaned_DataSet %>%
  filter(speed < 3 & speed > 0)

}







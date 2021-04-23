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

  # the following lines convert the timestamp column into separate date and time columns


  individualDataSet <- read.csv("C:/Users/Gillian/Downloads/Health Study_5f5184e73e2fd848eac22aec_passivelocation_65.csv")
  timeanddateValue <- individualDataSet[,8]
  convertDateAndTime <- data.frame(GivenFormat=c(timeanddateValue))

  convertDateAndTime %>%
  separate(GivenFormat, into = c("Date", "Time"), sep = " ", remove = TRUE) %>%
  mutate(Date = lubridate::as_date(Date,format= "%Y-%m-%d"),
         Time = hms::as_hms(str_c(Time, ":00")))


  # end of date and time column code

  # continue the function below
  # do we want to insert these columns back onto the CSV file? where to go from here?

}




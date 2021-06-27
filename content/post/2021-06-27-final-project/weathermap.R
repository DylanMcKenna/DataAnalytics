# load libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(janitor)
library(gghighlight)
library(leaflet)


# read in data
weather <- read.csv("weather.csv")

#change variable names
weather <- weather %>%
  rename(min_temp_f = TMIN,
         max_temp_f = TMAX,
         avg_temp_f = TAVG,
         avg_day_wind_mph = AWND,
         max_wind_5_dir_deg = WDF5,
         max_wind_5_spd_mph = WSF5,
         snowfall_in = SNOW,
         snowdepth_in = SNWD,
         precip_in = PRCP) 



# convert fahrenheit to celsius for temperature variables
weather <- weather %>%
  mutate(min_temp_c = fahrenheit.to.celsius(min_temp_f, round = 2),
         max_temp_c = fahrenheit.to.celsius(max_temp_f, round = 2),
         avg_temp_c = fahrenheit.to.celsius(avg_temp_f, round = 2)) %>%
  select(-c(min_temp_f, max_temp_f, avg_temp_f))



# convert date to Date
weather <- weather %>%
  mutate(date = ymd(date))

# cumulative snowfall by station

# NOT REALLY USEFUL
weather_group_temp_max <- weather %>%
  group_by(station) %>%
  count(station, max_temp_c, latitude, longitude) %>%
  summarise(max_temp_c = max(max_temp_c),
            latitude = max(latitude),
            longitude = max(longitude)) %>%
  na.omit()

str(weather_group_temp_max)
str(weather)


# building a map


ui <- fluidPage(
  sliderInput(inputId = "date", "")
)


#weather_mapping <- weather %>%
#  leaflet() %>%
#  #addProviderTiles(providers$Stamen.Toner)
#  addTiles() %>%
#  addMarkers()
#weather_mapping


#ui <- fluidPage(
#  sliderInput(inputId = "")
#)

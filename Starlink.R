#package#
library(sf)
library(leaflet)
library(ggplot2)
library(spacetime)
library(tidyverse)
library(raster)

#read data#
library(reticulate)

py_run_string("from sgp4.api import Satrec")
py_run_string("
tle_line1 = '1 25544U 98067A   22312.77469152  .00005815  00000-0  11202-3 0  9993'
tle_line2 = '2 25544  51.6446 148.3262 0006676 232.4788 259.4852 15.50000000  7072'
satellite = Satrec.twoline2rv(tle_line1, tle_line2)
print(satellite)
")

tle_data <- data.frame(
  line1 = "1 25544U 98067A   22312.77469152  .00005815  00000-0  11202-3 0  9993",
  line2 = "2 25544  51.6446 148.3262 0006676 232.4788 259.4852 15.50000000  7072"
)

# Parsing manual (contoh sederhana untuk elemen TLE tertentu)
tle_parsed <- list(
  inclination = as.numeric(substr(tle_data$line2, 9, 16)),  # Sudut inklinasi (derajat)
  right_ascension = as.numeric(substr(tle_data$line2, 18, 25)),  # Ascension node
  eccentricity = as.numeric(paste0("0.", substr(tle_data$line2, 27, 33))),  # Eksentrisitas
  perigee = as.numeric(substr(tle_data$line2, 35, 42)),  # Argumen perigee
  mean_anomaly = as.numeric(substr(tle_data$line2, 44, 51)),  # Anomali rata-rata
  mean_motion = as.numeric(substr(tle_data$line2, 53, 63))  # Gerakan rata-rata
)
print(tle_parsed)

##Visualisasi##
# Contoh data lokasi satelit (fiktif)
satellite_positions <- data.frame(
  sat_name = "Starlink-1",
  latitude = c(51.64),
  longitude = c(148.32),
  altitude = c(550)  # dalam kilometer
)

library(leaflet)
leaflet(data = satellite_positions) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = 6,
    color = "blue",
    label = ~paste(sat_name, "Altitude:", altitude, "km")
  )

library(ggplot2)
library(gganimate)


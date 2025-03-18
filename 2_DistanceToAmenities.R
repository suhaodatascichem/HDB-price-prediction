# Load required packages
pacman::p_load(readxl,writexl,lubridate, tidyr, tidyverse, reshape2, stringr, ggplot2, RColorBrewer, ggmap, geosphere)

setwd("C:/Users/dcyw/Desktop/Sem 2 Dataset/")

hdb_data <- read_excel("resaleflatprices_addr_geocode.xlsx")

# Dhoby Ghaut MRT station coordinates
dhoby_ghaut_lat <- 1.2992  # Example latitude for Dhoby Ghaut MRT station
dhoby_ghaut_lon <- 103.8455  # Example longitude for Dhoby Ghaut MRT station

# Calculate distances from HDB locations to Dhoby Ghaut MRT station
hdb_data$distance_to_dhoby_ghaut <- distGeo(
  matrix(c(hdb_data$geocode_long, hdb_data$geocode_lat), ncol = 2),
  c(dhoby_ghaut_lon, dhoby_ghaut_lat)
)

# Sengkang and Punggol MRT station coordinates
MRT_SK_lat <- 1.3918392323191482   
MRT_SK_lon <- 103.89542380842974  

MRT_Punggol_lat <- 1.4053523339402083  
MRT_Punggol_lon <- 103.90237735260766

hdb_data$distance_to_Sengkang_MRT <- distGeo(
  matrix(c(hdb_data$geocode_long, hdb_data$geocode_lat), ncol = 2),
  c(MRT_SK_lon, MRT_SK_lat)
)

hdb_data$distance_to_Punggol_MRT <- distGeo(
  matrix(c(hdb_data$geocode_long, hdb_data$geocode_lat), ncol = 2),
  c(MRT_Punggol_lon, MRT_Punggol_lat)
)

########**************###############
prisch <- read_excel("primary school.xlsx")

# Calculate distance between lat1, lon1 and lat2, lon2
prisch$distance_to_prisch <- distVincentySphere(
  cbind(prisch$source_long, prisch$source_lat),  # Matrix of lon1, lat1 values
  cbind(prisch$long, prisch$lat)   # Matrix of lon2, lat2 values
)

prisch1 <-prisch[,-c(1:3,5:10)]

# Convert data from long to wide format
# prisch1 <- prisch %>%
#   spread(key = place_id, value = distance_to_prisch)

min_dist_prisch <- prisch1 %>%
  group_by(addr) %>%
  summarise(min_dist_prisch = min(distance_to_prisch))

# subset_df <- subset(prisch1, addr == "213A PUNGGOL WALK")
# print(subset_df)

HDB_FINAL <- left_join(hdb_data, min_dist_prisch, by = "addr")

########**************###############
restaurant <- read_excel("restaurant.xlsx")

restaurant$distance_to_restaurant <- distVincentySphere(
  cbind(restaurant$source_long, restaurant$source_lat),  # Matrix of lon1, lat1 values
  cbind(restaurant$long, restaurant$lat)   # Matrix of lon2, lat2 values
)

restaurant1 <-restaurant[,-c(1:3,5:10)]

min_dist_rest <- restaurant1 %>%
  group_by(addr) %>%
  summarise(min_dist_rest = min(distance_to_restaurant))

HDB_FINAL <- left_join(HDB_FINAL, min_dist_rest, by = "addr")

########**************###############
mall <- read_excel("shopping_mall.xlsx")

mall$distance_to_mall <- distVincentySphere(
  cbind(mall$source_long, mall$source_lat),  # Matrix of lon1, lat1 values
  cbind(mall$long, mall$lat)   # Matrix of lon2, lat2 values
)

mall1 <-mall[,-c(1:3,5:10)]

min_dist_mall <- mall1 %>%
  group_by(addr) %>%
  summarise(min_dist_mall = min(distance_to_mall))

HDB_FINAL <- left_join(HDB_FINAL, min_dist_mall, by = "addr")

########**************###############
spmkt <- read_excel("supermarket.xlsx")

spmkt$distance_to_spmkt <- distVincentySphere(
  cbind(spmkt$source_long, spmkt$source_lat),  # Matrix of lon1, lat1 values
  cbind(spmkt$long, spmkt$lat)   # Matrix of lon2, lat2 values
)

spmkt1 <-spmkt[,-c(1:3,5:10)]

min_dist_spmkt <- spmkt1 %>%
  group_by(addr) %>%
  summarise(min_dist_spmkt = min(distance_to_spmkt))

HDB_FINAL <- left_join(HDB_FINAL, min_dist_spmkt, by = "addr")

write_xlsx(HDB_FINAL, "resaleflats_distance.xlsx")

########secsh########

secsch <- read_excel("secondary school.xlsx")

# Calculate distance between lat1, lon1 and lat2, lon2
secsch$distance_to_secsch <- distVincentySphere(
  cbind(secsch$source_long, secsch$source_lat),  # Matrix of lon1, lat1 values
  cbind(secsch$long, secsch$lat)   # Matrix of lon2, lat2 values
)

colnames(secsch)[colnames(secsch) == "name"] <- "secschname"
secsch1 <-secsch[,-c(1:3,5:10)]

min_dist_secsch <- secsch1 %>%
  group_by(addr) %>%
  summarise(min_dist_secsch = min(distance_to_secsch))

HDB_FINAL <- left_join(HDB_FINAL, min_dist_secsch, by = "addr")

### Appendix of all unique location amenities names ###

unique1 <- prisch %>%
  distinct(name, place_id, .keep_all = TRUE) %>%
  mutate(amenities = "primary school") %>%
  select(name, place_id, amenities)

unique2 <- restaurant %>%
  distinct(name, place_id, .keep_all = TRUE) %>%
  mutate(amenities = "restaurant") %>%
  select(name, place_id, amenities)

unique3 <- mall %>%
  distinct(name, place_id, .keep_all = TRUE) %>%
  mutate(amenities = "mall") %>%
  select(name, place_id, amenities)

unique4 <- spmkt %>%
  distinct(name, place_id, .keep_all = TRUE) %>%
  mutate(amenities = "supermarket") %>%
  select(name, place_id, amenities)

HDB_amenities <- bind_rows(unique1, unique2, unique3, unique4)

write_xlsx(HDB_amenities, "HDB_amenities.xlsx")

############### Appendix - Unused codes ######################

# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  a <- sin(dLat / 2) * sin(dLat / 2) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon / 2) * sin(dLon / 2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371  # Radius of the Earth in km
  distance <- R * c  # Distance in km
  return(distance)
}

# Register your Google Maps API key
register_google(key = "AIzaSyAjXKgah9BFsEG8TjnM7F4YrkXHQVasBGU")

# Print or use the calculated distances as needed
print(data$distance)

# HDB block address
hdb_address <- "Your HDB Block Address, Singapore"

# Get latitude and longitude coordinates for HDB block address
hdb_location <- geocode(hdb_address)

# List of station names and their corresponding addresses
stations <- c("Station 1 Address, Singapore", "Station 2 Address, Singapore", "Station 3 Address, Singapore")

# Get latitude and longitude coordinates for stations
station_locations <- geocode(stations)

# Calculate distances from HDB block to stations
distances <- sapply(1:nrow(station_locations), function(i) {
  haversine_distance(hdb_location$lat, hdb_location$lon, station_locations$lat[i], station_locations$lon[i])
})

# Find the nearest station
nearest_station_index <- which.min(distances)
nearest_station <- stations[nearest_station_index]
nearest_distance <- distances[nearest_station_index]

# Print the nearest station and distance
print(paste("The nearest station to", hdb_address, "is", nearest_station, "and the distance is approximately", round(nearest_distance, 2), "km."))

# Load tmap package
library(tmap)

# Example data with latitude and longitude
locations <- data.frame(
  longitude = c(103.8198, 103.8587, 103.8448), 
  latitude = c(1.3521, 1.2954, 1.2804),
  name = c("Location 1", "Location 2", "Location 3")
)

# Create a simple thematic map
tm_shape(locations) +
  tm_dots(col = "red", size = 0.5) +
  tm_text("name", size = 0.7)

# Load geosphere package
library(geosphere)

# Example coordinates
lat1 <- 40.7128
lon1 <- -74.0060
lat2 <- 34.0522
lon2 <- -118.2437

# Calculate distance in kilometers
distance_km <- distVincentySphere(c(lon1, lat1), c(lon2, lat2), distUnit = "km")
print(distance_km)  # Output in kilometers

# Calculate distance in miles
distance_miles <- distVincentySphere(c(lon1, lat1), c(lon2, lat2), distUnit = "miles")
print(distance_miles)  # Output in miles

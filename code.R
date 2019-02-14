# library(databrew) # install via devtools::install_github('databrew/databrew')
library(raster)
library(sp)
library(tidyverse)

# Get a shapefile of Colombia
colombia <- getData('GADM', country = 'COL', level = 1)

# Keep only cauca
cauca <- colombia[colombia@data$NAME_1 == 'Cauca',]

# Define the date range
date_range <- as.Date(c('2016-07-01', '2018-07-31'))

# Get gsod data
if('data.RData' %in% dir()){
  load('data.RData')
} else {
  load(url('https://github.com/databrew/gsod/blob/master/data/gsod2017.rda?raw=true'))
  load(url('https://github.com/databrew/gsod/blob/master/data/gsod2018.rda?raw=true'))
  load(url('https://github.com/databrew/gsod/blob/master/data/gsod2019.rda?raw=true'))
  save(gsod2017, gsod2018, gsod2019,
       file = 'data.RData')
}

# Combine data
combined <- bind_rows(
  gsod2017,
  gsod2018,
  gsod2019) %>%
  filter(!is.na(lat),
         !is.na(lon),
         ctry == 'CO') %>%
  mutate(x = lon,
         y = lat)


# Make spatial
combined_sp <- combined %>%
  dplyr::distinct(lat, lon, .keep_all = TRUE)
  
coordinates(combined_sp) <- ~x+y
proj4string(combined_sp) <- proj4string(colombia)

# Get locations of interest
library(readxl)
locations <- read_excel('bairros.xlsx') %>%
  mutate(lon = x,
         lat = y)
locations$id <- 1:nrow(locations)
# Make spatial
coordinates(locations) <- ~x+y
proj4string(locations) <- proj4string(colombia)

# Create a matrix of distanes, which we will use to "weight" each stations
# relevance to each location (ie, the closer, the greater the weight)
distance_matrix <- rgeos::gDistance(
  spgeom1 = combined_sp,
  spgeom2 = locations,
  byid = TRUE
)

# Define weight function
weighter <- function(x){1/x}
# Visualize it: 
plot(1:20, weighter(1:20), type = 'l')

# For each day, for each location, get the weighted rainfall
out_list <- list()
unique_days <- sort(unique(combined$date))
unique_locations <- sort(unique(locations$id))

counter <- 0
for(i in 1:length(unique_days)){
  message(i, ' of ', length(unique_days))
  
  for(j in 1:length(unique_locations)){
    counter <- counter + 1
    this_day <- unique_days[i]
    this_location <- unique_locations[j]
    these_obs <- combined %>%
      filter(date == this_day)
    # Get indices of the stations with weather from this day
    indices <- which(combined_sp@data$stn_name %in% these_obs$stn_name)
    # Keep only weather for the stations with data
    these_distances <- distance_matrix[j,indices]
    # Get the rainfall
    rainfall <- these_obs$prcp
    rainfall[is.na(rainfall)] <- 0
    # Get the weighted rainfall for the location in question
    weighted_rainfall <- weighted.mean(rainfall,
                                       weighter(these_distances))
    # Add the result to the list
    result <-
      tibble(date = this_day,
                 id = this_location,
                 rainfall = weighted_rainfall)
    out_list[[counter]] <- result
    
  }
}

# Combine all data elements
final <- bind_rows(out_list)

# Join to location data
final <- left_join(
  final, locations@data
)

# Plot all locations
make_plot <- function(id_number = 1){
  sub_data <- final %>% filter(id == id_number)
  ggplot(data = sub_data,
         aes(x = date,
             y = rainfall)) +
    geom_line()
}
make_plot(40)

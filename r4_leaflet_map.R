# Site visit pririization and data analysis
# r4 leaflet map

# packages
library(readr) # sane way of reading files
library(leaflet) # javascript web mapping
library(shiny)

# Bring in the clean data frame


# Make base map widget
basemap <- leaflet(width = 800, height = 600) %>% # create initial widget (size in pixels)
  leaflet::addProviderTiles(providers$CartoDB.Positron) %>% # add the map to sit behind everything (this one is grey, other options are here: https://leaflet-extras.github.io/leaflet-providers/preview/)
  leaflet::setView(basemap,
                   lng = -120.740135,
                   lat = 47.751076,
                   zoom = 7) %>% # center the map in the middle of Washington State and zoom to see whole state
  leaflet::addMarkers(lat = p_small$Latitude, 
                      lng = p_small$Longitude,
                      clusterOptions = markerClusterOptions(),
                      popup = as.character(p_small$SiteName))

basemap # view the basemap

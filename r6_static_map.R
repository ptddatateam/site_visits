# r6 static map

# Packages
library(readr) # sane way of reading files
library(tidyverse) # For all that is good and holy in this world
library(ggmap) # make map
library(leaflet) # make maps differently

# Set Working directory to Github folder, change path for your machine
setwd("~/GitHub/site_visits")

# Bring in the clean data frame 
grantee_df <- readr::read_csv("data/grantee_df.csv")

# Reduce to indivdual locations
grantee_df <- grantee_df %>%
  dplyr::select(grantee_origin, lat_origin, long_origin) %>%
  dplyr::distinct()

# make a bounding box for the observations
site_visit_bbox <- ggmap::make_bbox(long_origin,
                                    lat_origin,
                                    grantee_df,
                                    f = .05)

# Make a simple map just to see hwat we have
get_stamenmap(site_visit_bbox, 
              zoom = 10, 
              maptype = "toner") %>% 
  ggmap() +
  geom_point(aes(x = long_origin, 
                 y = lat_origin), 
             data = grantee_df, 
             colour = "red", 
             size = 2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggplot2::ggsave("graphics/static_map1.png",
                width = 11,
                height = 8.5,
                units = "in",
                dpi = 300)

# and try it in leaflet
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=grantee_df$long_origin, 
             lat=grantee_df$lat_origin, 
             label=grantee_df$grantee_origin,
             labelOptions = labelOptions(noHide = T))
m  # Print the map

# Simple version of this map is published on RPubs at: 
# http://rpubs.com/bassoka/static_site_visit_map

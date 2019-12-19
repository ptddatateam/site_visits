# Site visit pririization and data analysis
# r3 basic map

# Packages
library(readr) # sane way of reading files
library(tidyverse) # For all that is good and holy in this world
library(ggmap) # make map

# Set Working directory to Github folder, change path for your machine
setwd("~/GitHub/site_visits")

# Bring in the file either from r2 or from data, note that if refreshing for new sites, will need to run r1 and r2
distances_df <- readr::read_csv("data/distances_df.csv")

# Bring the long and lat back to two fields instead of one
# and make new fields numeric
distances_df <- distances_df %>%
  tidyr::separate(latlong1, 
                  c("lat_origin","long_origin"),
                  sep = "\\+") %>%
  dplyr::mutate(long_origin = as.numeric(long_origin)) %>%
  dplyr::mutate(lat_origin = as.numeric(lat_origin)) 

# start with an ugly plot
ugly_plot <- plot(lat_origin ~ long_origin,
                  data = distances_df)

# Tell stupid google about the api key, note, this is the location of mine
api <- readLines("H:/personal/google.api")
ggmap::register_google(key = api, account_type = "standard")  

# make a bounding box for the observations
site_visit_bbox <- ggmap::make_bbox(long_origin,
                                    lat_origin,
                                    distances_df,
                                    f = .05)

# Make a simple map just to see hwat we have
get_stamenmap(site_visit_bbox, 
              zoom = 10, 
              maptype = "toner") %>% 
  ggmap() +
  geom_point(aes(x = long_origin, 
                 y = lat_origin), 
             data = distances_df, 
             colour = "red", 
             size = 1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggplot2::ggsave("graphics/basic_map.png")

# And let's finish cleaning up the data frame for use in leaflet mapping
# make the destination lat long useful again
# Bring the long and lat back to two fields instead of one
# and make new fields numeric
# turn seconds into minutes and round up
distances_df <- distances_df %>%
  tidyr::separate(latlong2, 
                  c("lat_destination","long_destination"),
                  sep = "\\+") %>%
  dplyr::mutate(long_destination = as.numeric(long_destination)) %>%
  dplyr::mutate(lat_destination = as.numeric(lat_destination)) %>%
  dplyr::mutate(minutes = ceiling(seconds/60)) %>%
  dplyr::select(-seconds) %>%
  dplyr::rename(grantee_origin = grantee1) %>%
  dplyr::rename(grantee_destination = grantee2)

# write out DF as grantee to be used in leaflet mapping
write.csv(distances_df, "data/grantee_df.csv", row.names = FALSE)

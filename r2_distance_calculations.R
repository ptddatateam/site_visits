# Site visit pririization and data analysis
# r2 distance calculations between sites

# Packages
library(readr) # sane way of reading files
library(tidyverse) # For all that is good and holy in this world
library(ggmap) # spatial analysis
library(gmapdistance) # distance calculations, install with devtools::install_github("rodazuero/gmapsdistance")

# Set Working directory to Github folder, change path for your machine
setwd("~/GitHub/site_visits")

# Bring in the file either from r1 or from data, note that if refreshing for new sites, will need to run r1
locations_df <- readr::read_csv("data/locations_df.csv")

# Create a field for lat and long that gmapsdistance can work with
locations_df$latlong <- paste('"',locations_df$lat,'+',locations_df$lon,'"')

# Keep just what we want
locations_df <- locations_df %>%
  dplyr::select(Grantee, origin=latlong) %>%
  dplyr::mutate(destination=origin)

# Get all possible pairs
locations_long <- expand.grid(locations_df)






















# Create to and from vectors for future distance calculations
loc_from <- locations_df$full_address
loc_to <- locations_df$full_address

# Create a data frame of all unique pairs
from_to <- as.data.frame(table(loc_from,loc_to)) %>%
  dplyr::select(loc_from, loc_to) %>%
  dplyr::filter(loc_from != loc_to) %>%
  dplyr::arrange(loc_from)

# Characters instead of factors so google understands what we want later
from_to[] <- lapply(from_to, as.character)

# Tell stupid google about the api key, note, this is the location of mine
api <- readLines("H:/personal/google.api")
ggmap::register_google(key = api, account_type = "standard")

# Calculate distances between locations
distances <- ggmap::mapdist(from_to$loc_from,
                            from_to$loc_to,
                            mode = "driving",
                            output = "simple")



TEST_from_to <- head(from_to, 50)

TEST_distances <- ggmap::mapdist(from = TEST_from_to$loc_from,
                            to = TEST_from_to$loc_to,
                            mode = "driving",
                            output = "simple")

TEST_distances2 <- ggmap::mapdist(TEST_from_to$loc_from,TEST_from_to$loc_to)



TEST_gmapdistance <- gmapsdistance(origin = TEST_from_to$loc_from,
                                  destination = TEST_from_to$loc_to,
                                  mode = "driving",
                                  key = api)


# This is the right example from how to go from name and location to 
# O-D name and location

t1 <- c("a","b","c","d")
t2 <- c("e","f","g","h")
t3 <- as.data.frame(cbind(t2,t1)) %>%
  dplyr::rename(name=t2, loc=t1) %>%
  tidyr::unite("nameloc1",name:loc) %>%
  dplyr::mutate(nameloc2 = nameloc1) %>%
  expand.grid() %>%
  dplyr::filter(nameloc1 != nameloc2) %>%
  arrange(nameloc1) %>%
  tidyr::separate(nameloc1,c("name1","loc1")) %>%
  tidyr::separate(nameloc2,c("name2","loc2"))

# Ready to try gmapsdistance again after using the example to reflect the actual data



# Site visit pririization and data analysis
# r2 distance calculations between sites

# Packages
library(readr) # sane way of reading files
library(tidyverse) # For all that is good and holy in this world
library(gmapsdistance) # distance calculations, install with devtools::install_github("rodazuero/gmapsdistance")

# Set Working directory to Github folder, change path for your machine
setwd("~/GitHub/site_visits")

# Bring in the file either from r1 or from data, note that if refreshing for new sites, will need to run r1
locations_df <- readr::read_csv("data/locations_df.csv")

# Create a field for lat and long that gmapsdistance can work with
# locations_df$latlong <- paste('"',locations_df$lat,'+',locations_df$lon,'"')
# previous line gives quotes per documentation, but only needed for calling directly
# If calling from a field than use the following
locations_df$latlong <- paste(locations_df$lat,'+',locations_df$lon)

# get rid of spaces in new field
locations_df$latlong <- gsub('\\s+', '', locations_df$latlong)

# Keep just what we want
locations_df <- locations_df %>%
  dplyr::select(grantee=Grantee, latlong)

# Get all possible pairs
locations_df <- locations_df %>%
  tidyr::unite("granteelatlong1",grantee:latlong) %>%
  dplyr::mutate(granteelatlong2 = granteelatlong1) %>%
  expand.grid() %>%
  dplyr::filter(granteelatlong1 != granteelatlong2) %>%
  arrange(granteelatlong1) %>%
  tidyr::separate(granteelatlong1,c("grantee1","latlong1"),sep="_") %>%
  tidyr::separate(granteelatlong2,c("grantee2","latlong2"),sep="_")

# Tell stupid google about the api key, note, this is the location of mine
api <- readLines("H:/personal/google.api")

# testing with gmapsdistance 
TEST_gmapdistance <- gmapsdistance(origin = locations_df$latlong1,
                                   destination = locations_df$latlong2,
                                   combinations = "pairwise",
                                   mode = "driving",
                                   key = api,
                                   shape = "long",
                                   dep_date = "2019-12-24", # a Tuesday
                                   dep_time = "12:00:00")

# Turn it into a data frame and keep just what is useful
TEST_gmapdistance <- as.data.frame(TEST_gmapdistance)
TEST_gmapdistance <- TEST_gmapdistance %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  dplyr::rename(latlong1 = Time.or) %>%
  dplyr::rename(latlong2 = Time.de) %>%
  dplyr::rename(seconds = Time.Time) %>% # this will return seconds
  dplyr::rename(miles = Distance.Distance) %>% # this will return meters
  dplyr::select(latlong1,latlong2,seconds,miles) %>%
  dplyr::mutate(miles = miles / 1609.344 )

# Combine OD table and distance table
distances_df <- 
  dplyr::left_join(locations_df, 
                   TEST_gmapdistance, 
                   by=c("latlong1" = "latlong1", 
                        "latlong2" = "latlong2"))

# remove duplicates
distances_df <- distances_df %>%
  dplyr::distinct()

# export the table of grantees and distances
write.csv(distances_df, "data/distances_df.csv", row.names = FALSE)

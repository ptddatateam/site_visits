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
                                   shape = "long")

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
write.csv(distances_df, "data/distances_df.csv")





















Test_Distances <- head(locations_df,2)
# following two lines give right answer but won't get held in memory
originT <- cat(paste(Test_Distances$latlong1, collapse=", "))
destinationT <- cat(paste(Test_Distances$latlong2, collapse=", "))
# Let's see if a vector without quotes does the trick...


testing <- gmapsdistance("47.2312078+-122.4659419",
                         "46.4188573+-117.0680059",
                         mode="driving",
                         key = api)

testing <- gmapsdistance(origin = c("47.2312078+-122.4659419","47.2312078+-122.4659419"),
                         destination = c("46.4188573+-117.0680059","46.239185+-119.240989"),
                         combinations = "pairwise",
                         mode="driving",
                         key = api,
                         shape = "long")
testing <- as.data.frame(testing)

testing <- gmapsdistance(origin = Test_Distances$latlong1,
                         destination = Test_Distances$latlong2,
                         combinations = "pairwise",
                         mode="driving",
                         key = api,
                         shape = "long")
testing <- as.data.frame(testing)



# Try calculating the distances with ggmap 
distances <- ggmap::mapdist(locations_df$latlong1,
                            locations_df$latlong2,
                            mode = "driving",
                            output = "simple")
##    ERROR
##    Error: Argument 3 must be length 2, not 1
##    In addition: Warning message:
##      In getdists(df[(half_df + 1L):n, ]) : HTTP 500 Internal Server Error




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



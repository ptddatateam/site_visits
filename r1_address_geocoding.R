# Site visit pririization and data analysis
# r1 address geocoding

# Set Working directory to Github folder, change path for your machine
setwd("~/GitHub/site_visits")

# Packages
library(openxlsx) # open xlsx files, note that readxl doesn't read urls... Come on Hadley...
library(tidyverse) # For all that is good and holy in this world
library(ggmap) # geocoding and mapping

# link to the data from Seth
locations_url <- "http://sharedot/eso/so/pubtcb/Docs/19-21%20S_V%20Planning.xlsx"

# Create a data frame from the data
locations <- openxlsx::read.xlsx(locations_url,
                       startRow = 8)

# Just keep the address (useful) fields
# locations <- locations %>%
#  dplyr::select(Grantee, Mailing.Address, Mailing.Address.City, Mailing.Address.State)
# Cannot use dplyr here because spreadsheet has two identical colums names Grantee, and the tidyverse doesn't accept shitty data, so instead: 

locations <- locations[,c("Grantee",
                          "Mailing.Address",
                          "Mailing.Address.City",
                          "Mailing.Address.State")]

# Clean up the address field to remove extranious information, eg suite numbers
locations$Mailing.Address <- gsub("(.*),.*", "\\1", locations$Mailing.Address)

# Only keep records with complete information as to not have places with no address. Want this to avoid geocoding w/o an address, which google will attempt
locations <- locations %>%
  tidyr::drop_na()

# Create a field with a complete address
locations$full_address <- paste(locations$Mailing.Address, 
                                locations$Mailing.Address.City, 
                                locations$Mailing.Address.State, 
                                sep = ", ")

# Keep only the Grantee name and full address for the sake of a tidyer DF
locations <- locations %>%
  dplyr::select(Grantee, full_address)

# Tell stupid google about the api key, note, this is the location of mine
api <- readLines("H:/personal/google.api")
ggmap::register_google(key = api, account_type = "standard")

# geocode the locations
locations_df <- locations %>%
  ggmap::mutate_geocode(full_address)

# Export list of addresses for giggles
write.csv(locations_df, "data/locations_df.csv", row.names = FALSE)
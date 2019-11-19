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

# Remove large Urban or small Rural grantees that are not subject to the site visit requirements for WSDOT
# per email from Seth Stark and Steven Meyeroff on 20191119
locations_df <- locations_df %>%
  dplyr::filter(!(Grantee == "Ben Franklin Transit" |
                    Grantee == "City of Selah" |
                    Grantee == "City of Yakima" |
                    Grantee == "Clark County Public Transportation Benefit Area (C - Tran)" |
                    Grantee == "Community Action of Skagit County" |
                    Grantee == "Entiat Valley Community Services (EVCS)" |
                    Grantee == "Everett Transit" |
                    Grantee == "King County Metro Transit" |
                    Grantee == "Kitsap County Public Transportation Benefit Area Authority" |
                    Grantee == "Lower Elwha Klallam Tribe" |
                    Grantee == "Pierce Transit" |
                    Grantee == "Samish Indian Nation" |
                    Grantee == "Snohomish County Workforce Development Council" |
                    Grantee == "Spokane Transit Authority" |
                    Grantee == "Stanwood Community & Senior Center" |
                    Grantee == "Thurston County Public Benefit Transportation Area (Intercity Transit)" |
                    Grantee == "Whatcom Transportation Authority" |
                    Grantee == "Yakima Valley Conference of Governments (YVCOG)"))

# Export list of addresses for giggles
write.csv(locations_df, "data/locations_df.csv", row.names = FALSE)

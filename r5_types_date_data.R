# Site visit pririization and data analysis
# r5 types and date since last visit data frame

library(openxlsx) # open xlsx files, note that readxl doesn't read urls... Come on Hadley...
library(tidyverse) # for all that is good and holy in this world

# Set Working directory to Github folder, change path for your machine
setwd("~/GitHub/site_visits")

# Bring in the data on visit type and date since last visit
types <- openxlsx::read.xlsx("http://sharedot/eso/so/pubtcb/Docs/2019-11-08 - S_V Criteria Prioritization Draft.xlsx",
                             sheet = "SV Time Since SV",
                             startRow = 5,
                             cols = 2:6,
                             detectDates = TRUE) %>%
  dplyr::rename(grantee = Grantee,
                admin = Date.of.Last.Site.Visit,
                capital = X3,
                drug = X4, 
                financial = X5) %>%
  dplyr::filter(!(grantee == "Ben Franklin Transit" |
                    grantee == "City of Selah" |
                    grantee == "City of Yakima" |
                    grantee == "Clark County Public Transportation Benefit Area (C - Tran)" |
                    grantee == "Community Action of Skagit County" |
                    grantee == "Entiat Valley Community Services (EVCS)" |
                    grantee == "Everett Transit" |
                    grantee == "King County Metro Transit" |
                    grantee == "Kitsap County Public Transportation Benefit Area Authority" |
                    grantee == "Lower Elwha Klallam Tribe" |
                    grantee == "Pierce Transit" |
                    grantee == "Samish Indian Nation" |
                    grantee == "Snohomish County Workforce Development Council" |
                    grantee == "Spokane Transit Authority" |
                    grantee == "Stanwood Community & Senior Center" |
                    grantee == "Thurston County Public Benefit Transportation Area (Intercity Transit)" |
                    grantee == "Whatcom Transportation Authority" |
                    grantee == "Yakima Valley Conference of Governments (YVCOG)")) %>%
  tidyr::pivot_longer(c("admin","capital","drug","financial"),
                      names_to = "type",
                      values_to = "date_since") %>%
  dplyr::filter(substr(date_since,1,1)=="2") %>%
  dplyr::mutate(date_since = as.Date(date_since)) %>%
  dplyr::mutate(days_since = Sys.Date() - date_since) %>%
  dplyr::select(-date_since) %>%
  tidyr::pivot_wider(names_from = type,
                     values_from = days_since) %>%
  dplyr::select(grantee, admin, capital, drug, financial)

# write out the DF
write.csv(types, "data/types_df.csv", row.names = FALSE)








# Creating a new version that allows for comments in addition to dates and times since the last visit
# Will make two DFs based on whether or not there is a date in them. 
# then put the DFs back together into a final one for display purposes back in the application. 

# types with a date: 
types_number <- openxlsx::read.xlsx("http://sharedot/eso/so/pubtcb/Docs/2019-11-08 - S_V Criteria Prioritization Draft.xlsx",
                             sheet = "SV Time Since SV",
                             startRow = 5,
                             cols = 2:6,
                             detectDates = TRUE) %>%
  dplyr::rename(grantee = Grantee,
                admin = Date.of.Last.Site.Visit,
                capital = X3,
                drug = X4, 
                financial = X5) %>%
  dplyr::filter(!(#Grantee == "Ben Franklin Transit" |
    grantee == "City of Selah" |
      #Grantee == "City of Yakima" |
      grantee == "Clark County Public Transportation Benefit Area (C - Tran)" |
      #Grantee == "Community Action of Skagit County" |
      #Grantee == "Entiat Valley Community Services (EVCS)" |
      #Grantee == "Everett Transit" |
      #Grantee == "King County Metro Transit" |
      #Grantee == "Kitsap County Public Transportation Benefit Area Authority" |
      #Grantee == "Lower Elwha Klallam Tribe" |
      #Grantee == "Pierce Transit" |
      grantee == "Samish Indian Nation" |
      #Grantee == "Snohomish County Workforce Development Council" |
      #Grantee == "Spokane Transit Authority" |
      grantee == "Stanwood Community & Senior Center" |
      #Grantee == "Thurston County Public Benefit Transportation Area (Intercity Transit)" |
      grantee == "Whatcom Transportation Authority" |
      grantee == "Yakima Valley Conference of Governments (YVCOG)")) %>%
  tidyr::pivot_longer(c("admin","capital","drug","financial"),
                      names_to = "type",
                      values_to = "date_since") %>%
  dplyr::filter(substr(date_since,1,1)=="2") %>%
  dplyr::mutate(date_since = as.Date(date_since)) %>%
  dplyr::mutate(days_since = Sys.Date() - date_since) %>%
  dplyr::select(-date_since) %>%
  dplyr::mutate(days_since = as.character(days_since))

# types with text 
types_text <- openxlsx::read.xlsx("http://sharedot/eso/so/pubtcb/Docs/2019-11-08 - S_V Criteria Prioritization Draft.xlsx",
                                    sheet = "SV Time Since SV",
                                    startRow = 5,
                                    cols = 2:6,
                                    detectDates = TRUE) %>%
  dplyr::rename(grantee = Grantee,
                admin = Date.of.Last.Site.Visit,
                capital = X3,
                drug = X4, 
                financial = X5) %>%
  dplyr::filter(!(#Grantee == "Ben Franklin Transit" |
    grantee == "City of Selah" |
      #Grantee == "City of Yakima" |
      grantee == "Clark County Public Transportation Benefit Area (C - Tran)" |
      #Grantee == "Community Action of Skagit County" |
      #Grantee == "Entiat Valley Community Services (EVCS)" |
      #Grantee == "Everett Transit" |
      #Grantee == "King County Metro Transit" |
      #Grantee == "Kitsap County Public Transportation Benefit Area Authority" |
      #Grantee == "Lower Elwha Klallam Tribe" |
      #Grantee == "Pierce Transit" |
      grantee == "Samish Indian Nation" |
      #Grantee == "Snohomish County Workforce Development Council" |
      #Grantee == "Spokane Transit Authority" |
      grantee == "Stanwood Community & Senior Center" |
      #Grantee == "Thurston County Public Benefit Transportation Area (Intercity Transit)" |
      grantee == "Whatcom Transportation Authority" |
      grantee == "Yakima Valley Conference of Governments (YVCOG)")) %>%
  tidyr::pivot_longer(c("admin","capital","drug","financial"),
                      names_to = "type",
                      values_to = "date_since") %>%
  dplyr::filter(substr(date_since,1,1)!="2") %>%
  dplyr::rename(days_since = date_since)

# put them together
types <- 
  rbind(types_number, types_text) %>%
  tidyr::pivot_wider(names_from = type,
                     values_from = days_since) %>%
  dplyr::select(grantee, admin, capital, drug, financial) %>%
  dplyr::arrange(grantee)

# write out the DF
write.csv(types, "data/types_df.csv", row.names = FALSE)

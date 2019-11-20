# Site visit pririization and data analysis
# r4 leaflet map

# packages
library(readr) # sane way of reading files
library(openxlsx) # open xlsx files, note that readxl doesn't read urls... Come on Hadley...
library(tidyverse) # for all that is good and holy in this world
library(leaflet) # javascript web mapping
library(leaflet.extras) # full screen control among other useful things
library(shiny)

# Set Working directory to Github folder, change path for your machine
# setwd("~/GitHub/site_visits")

# Bring in the clean data frame with distances
grantee_df <- readr::read_csv("data/grantee_df.csv")

# Bring in the types and date since last visit data
types_df <- readr::read_csv("data/types_df.csv") # if bringing it in from DF, but updateable version here
#types_df <- openxlsx::read.xlsx("http://sharedot/eso/so/pubtcb/Docs/2019-11-08 - S_V Criteria Prioritization Draft.xlsx",
#                                startRow = 5,
#                                cols = 2:6,
#                                detectDates = TRUE) %>%
#  dplyr::rename(grantee = Grantee,
#                admin = Date.of.Last.Site.Visit,
#                capital = X3,
#                drug = X4, 
#                financial = X5) %>%
#  dplyr::filter(!(grantee == "Ben Franklin Transit" |
#                    grantee == "City of Selah" |
#                    grantee == "City of Yakima" |
#                    grantee == "Clark County Public Transportation Benefit Area (C - Tran)" |
#                    grantee == "Community Action of Skagit County" |
#                    grantee == "Entiat Valley Community Services (EVCS)" |
#                    grantee == "Everett Transit" |
#                    grantee == "King County Metro Transit" |
#                    grantee == "Kitsap County Public Transportation Benefit Area Authority" |
#                    grantee == "Lower Elwha Klallam Tribe" |
#                    grantee == "Pierce Transit" |
#                    grantee == "Samish Indian Nation" |
#                    grantee == "Snohomish County Workforce Development Council" |
#                    grantee == "Spokane Transit Authority" |
#                    grantee == "Stanwood Community & Senior Center" |
#                    grantee == "Thurston County Public Benefit Transportation Area (Intercity Transit)" |
#                    grantee == "Whatcom Transportation Authority" |
#                    grantee == "Yakima Valley Conference of Governments (YVCOG)")) %>%
#  tidyr::pivot_longer(c("admin","capital","drug","financial"),
#                      names_to = "type",
#                      values_to = "date_since") %>%
#  dplyr::filter(substr(date_since,1,1)=="2") %>%
#  dplyr::mutate(date_since = as.Date(date_since)) %>%
#  dplyr::mutate(days_since = Sys.Date() - date_since) %>%
#  dplyr::select(-date_since) %>%
#  tidyr::pivot_wider(names_from = type,
#                     values_from = days_since) %>%
#  dplyr::select(grantee, admin, capital, drug, financial)

# Join grantee and type DFs so can have popups for type and date since last visit
# Hack to join DFs; simpler to do before it needs to be a function on the server side
grantee_df <- merge(grantee_df, types_df, by.x = "grantee_origin", by.y = "grantee", all.x = TRUE)
# And some badness for a stupidly wide data frame, but expediency wins here...
grantee_df <- merge(grantee_df, types_df, by.x = "grantee_destination", by.y = "grantee", all.x = TRUE)

# Make the shiny app
shinyApp(
  
  ui = bootstrapPage(
    tags$style(type="text/css", 
               "html, body {width:100%;height:100%}",
               ".selectize-input { font-size: 12px; line-height: 14px;} .selectize-dropdown { font-size: 12px; line-height: 14px; }"),
    leafletOutput("MapPlot1", width="100%", height="100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                  draggable = TRUE, top = "5", left = "auto", right = "5", bottom = "auto",
                  width = 300, height = "auto",
                  uiOutput("siteSelect"),
                  img(src="wsdot-logo.png", width = 280)
    )
  ),
  
  server = function(input, output) {
    
    filtered <- reactive({
      grantee_df[grantee_df$grantee_origin == input$siteName &
                   grantee_df$minutes < 60,]
    })
    
    filtered_single <- reactive({
      grantee_df[grantee_df$grantee_origin == input$siteName,] 
    }) 
    
    flitered_single <- reactive({
      unique(filtered_single[,c(1:3)],)
    })
    
    output$siteSelect <- renderUI({
      siteNames <- sort(unique(grantee_df$grantee_origin))
      selectInput("siteName", "Grantee", choices = siteNames, selected = siteNames[8])
    })
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(lng = -120.3103, lat = 47.4235, zoom = 7) %>%
        leaflet.extras::addFullscreenControl()
    })
    
    MapPlot1_proxy <- leafletProxy("MapPlot1")
    
    observeEvent(input$siteName, {
      
      fdata <- filtered()
      
      fdata2 <- fdata %>%
        pivot_longer(c("long_origin","long_destination"), names_to = "f2long", values_to = "long") %>%
        pivot_longer(c("lat_origin","lat_destination"), names_to = "f2lat", values_to = "lat")
      
      fdata_single <- filtered_single()
      
      if(nrow(fdata)!=0){
        maxLong = max(fdata2$long)
        maxLat = max(fdata2$lat)
        minLong = min(fdata2$long)
        minLat = min(fdata2$lat)
      }else{
        maxLong = -116.915989
        maxLat = 49.002494
        minLong = -124.763068
        minLat = 45.543541
      }
      
      if(nrow(fdata)==0){ 
        MapPlot1_proxy %>%
          clearMarkers() %>%
          clearMarkerClusters() %>%
          addCircleMarkers(lng = fdata_single$long_origin,
                           lat = fdata_single$lat_origin,
                           radius = 20,
                           color = "#E69F00",
                           popup = paste(fdata_single$grantee_origin, "<br>","<br>",
                                         "Days since last visit:","<br>",
                                         "Admin:", fdata_single$admin.y,"<br>",
                                         "Capital:", fdata_single$capital.y,"<br>",
                                         "Drug & alcohol:", fdata_single$drug.y,"<br>",
                                         "Financial:", fdata_single$financial.y)) %>%
          #popup = as.character(fdata_single$grantee_origin)) %>%
          fitBounds(minLong,minLat,maxLong,maxLat)
      }else{
        MapPlot1_proxy %>%
          clearMarkers() %>%
          clearMarkerClusters() %>%
          addCircleMarkers(lng = fdata$long_origin,
                           lat = fdata$lat_origin,
                           radius = 20,
                           color = "#E69F00",
                           popup = paste(fdata$grantee_origin, "<br>","<br>",
                                         "Days since last visit:","<br>",
                                         "Admin:", fdata$admin.y,"<br>",
                                         "Capital:", fdata$capital.y,"<br>",
                                         "Drug & alcohol:", fdata$drug.y,"<br>",
                                         "Financial:", fdata$financial.y)) %>%
          addCircleMarkers(lng = fdata$long_destination,
                           lat = fdata$lat_destination,
                           radius = 10,
                           color = "#56B4E9",
                           clusterOptions = markerClusterOptions(),
                           popup = paste(fdata$grantee_origin, "<br>","<br>",
                                         "Days since last visit:","<br>",
                                         "Admin:", fdata$admin.x,"<br>",
                                         "Capital:", fdata$capital.x,"<br>",
                                         "Drug & alcohol:", fdata$drug.x,"<br>",
                                         "Financial:", fdata$financial.x)) %>%
          fitBounds(minLong,minLat,maxLong,maxLat)
      }
      
    })
    
  }
  
)


# Not run
# output table
# site_visit_travel_time <- grantee_df %>%
#  dplyr::filter(minutes<60) %>%
#  dplyr::arrange(grantee_origin, minutes) %>%
#  dplyr::select(grantee_origin, grantee_destination, miles, minutes)
#write.csv(site_visit_travel_time, "site_visit_travel_time.csv", row_names = FALSE)


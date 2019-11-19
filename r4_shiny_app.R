# Site visit pririization and data analysis
# r4 leaflet map

# packages
library(readr) # sane way of reading files
library(tidyverse) # for all that is good and holy in this world
library(leaflet) # javascript web mapping
library(leaflet.extras) # full screen control among other useful things
library(shiny)

# Set Working directory to Github folder, change path for your machine
# setwd("~/GitHub/site_visits")

# Bring in the clean data frame
grantee_df <- readr::read_csv("data/grantee_df.csv")

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
                  uiOutput("siteSelect")
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
                           popup = as.character(fdata_single$grantee_origin)) %>%
          fitBounds(minLong,minLat,maxLong,maxLat)
      }else{
        MapPlot1_proxy %>%
          clearMarkers() %>%
          clearMarkerClusters() %>%
          addCircleMarkers(lng = fdata$long_origin,
                           lat = fdata$lat_origin,
                           radius = 20,
                           color = "#E69F00",
                           popup = as.character(fdata$grantee_origin)) %>%
          addCircleMarkers(lng = fdata$long_destination,
                           lat = fdata$lat_destination,
                           radius = 10,
                           color = "#56B4E9",
                           clusterOptions = markerClusterOptions(),
                           popup = as.character(fdata$grantee_destination)) %>%
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
#write.csv(site_visit_travel_time, "site_visit_travel_time.csv")


# Site visit pririization and data analysis
# r4 leaflet map

# packages
library(readr) # sane way of reading files
library(tidyverse) # for all that is good and holy in this world
library(leaflet) # javascript web mapping
library(leaflet.extras) # full screen control among other useful things
library(shiny)

# Set Working directory to Github folder, change path for your machine
setwd("~/GitHub/site_visits")

# Bring in the clean data frame
grantee_df <- readr::read_csv("data/grantee_df.csv")

# Make a compact DF of the grantee site locations
grantee_small <- grantee_df %>%
  distinct(grantee_origin, lat_origin, long_origin)

# Make the shiny app
shinyApp(
  ui = bootstrapPage(
    tags$style(type="text/css", 
               "html, body {width:100%;height:100%}",
               ".selectize-input { font-size: 12px; line-height: 10px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
    leafletOutput("MapPlot1", width="100%", height="100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                  draggable = TRUE, top = "5", left = "auto", right = "5", bottom = "auto",
                  width = 300, height = "auto",
                  selectInput(inputId = "grantee_origin", 
                              label = "Grantee", 
                              choices = sort(unique(grantee_df$grantee_origin))))
  ),
  
  server = function(input, output) {
    
    filtered <- reactive({
      grantee_df[grantee_df$grantee_origin == input$grantee_origin,]
    })
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(lng = -120.3103, lat = 47.4235, zoom = 7) %>%
        leaflet.extras::addFullscreenControl()
    })
    
    MapPlot1_proxy <- leafletProxy("MapPlot1")
    
    observe({
      fdata <- filtered()
      leafletProxy("MapPlot1") %>% clearMarkers() %>% 
        addCircleMarkers(lng = grantee_small$long_origin,
                   lat = grantee_small$lat_origin,
                   clusterOptions = markerClusterOptions(),
                   popup = as.character(grantee_small$grantee_origin))
    })
    
    #observe({
    #  fdata <- filtered()
    #  MapPlot1_proxy %>%
    #    addCircleMarkers(lng = fdata$long_origin,
    #               lat = fdata$lat_origin) %>%
    #    flyTo(lng = fdata$long_origin,
    #          lat = fdata$lat_origin,
    #          zoom = 10)
    #})
  }
  
)


































# Not run
# Make base map widget
basemap <- leaflet(width = 800, height = 600) %>% # create initial widget (size in pixels)
  leaflet::addProviderTiles(providers$CartoDB.Positron) %>% # add the map to sit behind everything (this one is grey, other options are here: https://leaflet-extras.github.io/leaflet-providers/preview/)
  leaflet::setView(basemap,
                   lng = -120.740135,
                   lat = 47.751076,
                   zoom = 7) %>% # center the map in the middle of Washington State and zoom to see whole state
  leaflet::addMarkers(lat = grantee_df$lat_origin, 
                      lng = grantee_df$long_origin,
                      clusterOptions = markerClusterOptions(),
                      popup = as.character(grantee_df$grantee_origin))

basemap # view the basemap


# Simplyfy the world and just keep records where the distance between two grantees is less than an hour,
# we could add a slider select for time if anyone wants in, but simpler to leave out
grantee_df <- grantee_df %>%
  dplyr::filter(minutes<60)










































































shinyApp(
  ui = bootstrapPage(
    tags$style(type="text/css", 
               "html, body {width:100%;height:100%}",
               ".selectize-input { font-size: 12px; line-height: 10px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
    leafletOutput("MapPlot1", width="100%", height="100%"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                  draggable = TRUE, top = "5", left = "auto", right = "5", bottom = "auto",
                  width = 300, height = "auto",
                  selectInput(inputId = "grantee_origin", 
                              label = "Grantee", 
                              choices = sort(unique(grantee_df$grantee_origin))))
  ),
  
  server = function(input, output) {
    
    filtered <- reactive({
      grantee_df[grantee_df$grantee_origin == input$grantee_origin,]
    })
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(lng = -120.3103, lat = 47.4235, zoom = 7) %>%
        leaflet.extras::addFullscreenControl()
    })
    
    MapPlot1_proxy <- leafletProxy("MapPlot1")
    
    #observe({
    #  fdata <- filtered()
    #  leafletProxy("MapPlot1") %>% clearMarkers() %>% 
    #    addCircleMarkers(lng = grantee_small$long_origin,
    #                     lat = grantee_small$lat_origin,
    #                     clusterOptions = markerClusterOptions(),
    #                     popup = as.character(grantee_small$grantee_origin))
    #})
    
    observe({
      fdata <- filtered()
      MapPlot1_proxy %>%
        addCircleMarkers(lng = fdata$long_origin,
                   lat = fdata$lat_origin) %>%
        flyTo(lng = fdata$long_origin,
              lat = fdata$lat_origin,
              zoom = 10)
    })
  }
  
)

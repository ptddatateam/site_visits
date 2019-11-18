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
      MapPlot1_proxy %>% 
        clearMarkers() %>% 
        addCircleMarkers(lng = grantee_small$long_origin,
                   lat = grantee_small$lat_origin,
                   clusterOptions = markerClusterOptions(),
                   popup = as.character(grantee_small$grantee_origin))
    })
    
    #observeEvent(input$grantee_origin, {
    #  addCircleMarkers(lng = input$grantee_origin$long_origin,
    #                   lat = input$grantee_origin$lat_origin,
    #                   radius = 20,
    #                  fillColor = Orange)
      
      #xClick <- input$clickMap$x
      #yClick <- input$clickMap$y
      #state <- which_state(usaMap, xClick, yClick)
      #output$map <- renderPlot(
      #  plotMap + 
      #    geom_polygon(data = usaMap[usaMap$region == state,], fill = "yellow") +
      #    annotate("text", x = xClick, y = yClick, label = state, color = "red")
      #)
      #output$arrest <- renderPlot({
      #  plotArrest +
      #    geom_point(data = USArrests[tolower(rownames(USArrests)) == state,],
      #               size = 6, shape = 1, color = "red")
      #})
    #})
    
    observe({
      
      fdata <- filtered()
      
      MapPlot1_proxy %>%
        addCircleMarkers(lng = fdata$long_origin,
                   lat = fdata$lat_origin) #%>%
        #flyTo(lng = fdata$long_origin,
        #      lat = fdata$lat_origin,
        #      zoom = 10)
    })
    
    #updateSelectInput(session, "grantee_origin",
    #                  choices = input$grantee
    #)
  }
  
)



# output table
site_visit_travel_time <- grantee_df %>%
  dplyr::filter(minutes<60) %>%
  dplyr::arrange(grantee_origin, minutes) %>%
  dplyr::select(grantee_origin, grantee_destination, miles, minutes)
write.csv(site_visit_travel_time, "site_visit_travel_time.csv")

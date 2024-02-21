install.packages("tidyverse")
install.packages("plotly")
install.packages("scales")
install.packages("leaflet")
install.packages("dplyr")
install.packages("leaflet.extras")
library(shiny)
library(tidyverse)
library(plotly) 
library(scales)
library(leaflet)
library(dplyr)
library(leaflet.extras)

# Ensure your datasets are correctly loaded and merged
# Assuming 'state_coords' includes 'latitude' and 'longitude' for each 'state'
disaster_data <- read_csv("DisasterDeclarationsSummaries.csv") %>%
  filter(incidentType == "Fire")

state_coords <- read_csv("US_GeoCode.csv") %>%
  rename(state = `state&teritory`)

merged_data <- disaster_data %>%
  left_join(state_coords, by = "state")

ui <- fluidPage(
  titlePanel("Fire Incident Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("display", "Choose Display:",
                  choices = c("Geographical Location of Fire Incidents" = "geo", 
                              "Fire Incident Types" = "fireType")),
      selectInput("year", "Select Year of Loss:",
                  choices = c("All Years", unique(sort(disaster_data$fyDeclared)))),
      selectInput("state", "Select State:",
                  choices = c("All States", unique(sort(disaster_data$state))))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    # Adjust the filtering logic here
    data_filtered <- merged_data %>%
      filter(fyDeclared == input$year | input$year == "All Years", 
             state == input$state | input$state == "All States")
    
    # Define the clustering options with freezeAtZoom
    cluster_options <- markerClusterOptions(freezeAtZoom = 5)
    
    m <- leaflet(data_filtered) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.583333, lat = 39.833333, zoom = 4) # Center on the U.S.
    
    if(input$display == "geo") {
      m <- m %>%
        addCircleMarkers(~longitude, ~latitude,
                         popup = ~paste("Incident:", declarationTitle, "<br>", 
                                        "State:", state, "<br>", 
                                        "Year:", fyDeclared),
                         clusterOptions = cluster_options,  # Updated this line
                         color = "red", fillOpacity = 0.8, radius = 5)
    } else if(input$display == "fireType") {
      m <- m %>%
        addCircleMarkers(~longitude, ~latitude,
                         popup = ~paste("Type:", incidentType, "<br>",
                                        "Title:", declarationTitle, "<br>",
                                        "State:", state, "<br>", 
                                        "Year:", fyDeclared),
                         clusterOptions = cluster_options,  # Updated this line
                         color = "blue", fillOpacity = 0.8, radius = 5)
    }
    
    m
  })
}


shinyApp(ui, server)
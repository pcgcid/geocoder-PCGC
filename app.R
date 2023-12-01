# Install required packages if not already installed
# install.packages(c("shiny", "leaflet", "dplyr", "sf", "tidygraph", "ggraph"))

# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(tidygraph)
library(ggraph)
library(leaflet.extras)


# icon.glyphicon <- makeAwesomeIcon(icon = "flag", markerColor = "blue",
#                                   iconColor = "black", library = "glyphicon",
#                                   squareMarker =  TRUE)
icon.center <- makeAwesomeIcon(icon = "medkit", markerColor = "blue", library = "ion")
icon.center_clicked <- makeAwesomeIcon(icon = "medkit", markerColor = "red", library = "ion")
icon.nearest_center <- makeAwesomeIcon(icon = "medkit", markerColor = "orange", library = "ion")
icon.ion <- makeAwesomeIcon(icon = "home", markerColor = "green",
                            library = "ion")


# Load data from CSV files
drive_time_output <- read.csv("/app/output.csv")
ctsa_centers <- read.csv("/app/ctsa_centers.csv")

# Define UI
ui <- fluidPage(
  fileInput("file","Upload the file"),
  selectInput("selected_address", "Select Address", choices = drive_time_output$address),
  textInput(inputId = 'new_address', label = 'Address Input', placeholder = "Enter time the address"),
  
  numericInput("new_lat", "Enter Latitude", value = NA),
  numericInput("new_lon", "Enter Longitude", value = NA),
  selectInput("selected_center", "Select Center", choices = ctsa_centers$abbreviation,selected = NULL),
  leafletOutput("map"),
  tableOutput("info_table")
)

# Define server logic
server <- function(input, output, session) {
  # Combine selected address from the drop-down and manually entered coordinates
  selected_coordinates <- reactive({
    if (!is.null(input$selected_address)) {
      selected_data <- drive_time_output %>%
        filter(address == input$selected_address)
      return(list(lat = selected_data$lat, lon = selected_data$lon,nearest_center = selected_data$nearest_center))
    } else if (!is.na(input$new_lat) && !is.na(input$new_lon)) {
      return(list(lat = input$new_lat, lon = input$new_lon))
    } else {
      return(list(lat = NULL, lon = NULL, nearest_center = NULL))
    }
  })
  
  
  
  # Initialize the map
  output$map <- renderLeaflet({
    leaflet(data = ctsa_centers) %>%
      addTiles() %>%
      addAwesomeMarkers(~lon, ~lat, popup = ~abbreviation, label = ~abbreviation, 
                        icon = icon.center, layerId = ~abbreviation
      ) %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4)
  })
  
  # Observe changes in the selected center and update the map
  observe({
    leafletProxy("map") %>%
      clearShapes()
    
    if (!is.null(input$selected_center) && !is.null(selected_coordinates()$lat) && !is.null(selected_coordinates()$lon)) {
      selected_center_info_data <- ctsa_centers %>%
        filter(abbreviation == input$selected_center) %>%
        select(lat, lon, abbreviation, City, State)
      
      # Add a marker for the selected center with red color
      leafletProxy("map") %>%
        clearGroup(group = "awesome_markers") %>%
        addAwesomeMarkers(
          lat = selected_coordinates()$lat,
          lng = selected_coordinates()$lon,
          label = paste("Patient address"), 
          icon = icon.ion, group = "awesome_markers"
        )  
      
      session$userData$prev_add <- input$selected_address 
      
      route_data <- data.frame(
        lat = c(selected_coordinates()$lat, selected_center_info_data$lat),
        lon = c(selected_coordinates()$lon, selected_center_info_data$lon)
      )
      
      # Add a polyline from the selected address to the selected center
      leafletProxy("map") %>%
        addPolylines(
          data = route_data,
          lng = ~lon, lat = ~lat,
          color = "blue",
          weight = 2
        )
    }
  })
  
  
  
  
  
  
  # Observe click events on the map markers
  observeEvent(input$map_marker_click, {
    # Get the clicked marker's ID
    marker_id <- input$map_marker_click$id
    # Update the selectedCenter input
    updateSelectInput(session, "selected_center", selected = ctsa_centers$abbreviation[ctsa_centers$abbreviation == marker_id])
    
    # Remove the previously clicked marker (if any) and add a new one with blue color for the previously clicked marker
    if ((!is.null(session$userData$prev_marker_id) && session$userData$prev_marker_id != marker_id && 
        session$userData$prev_marker_id!= session$userData$nearest_center) || (session$userData$prev_add != input$selected_address)) {
      leafletProxy("map") %>%
        #clearGroup(group = "previous_center") %>%
        addAwesomeMarkers(
          ctsa_centers$lon[ctsa_centers$abbreviation == session$userData$prev_marker_id],
          ctsa_centers$lat[ctsa_centers$abbreviation == session$userData$prev_marker_id],
          popup = ctsa_centers$abbreviation[ctsa_centers$abbreviation == session$userData$prev_marker_id],
          label = ctsa_centers$abbreviation[ctsa_centers$abbreviation == session$userData$prev_marker_id],
          icon = icon.center,
          layerId = session$userData$prev_marker_id,
          group = "previous_center"
        )
    }
    
    if(!is.null(session$userData$prev_marker_id) && session$userData$prev_marker_id == session$userData$nearest_center){
      leafletProxy("map") %>%
        clearGroup(group = "nearest_center") %>%
        addAwesomeMarkers(ctsa_centers$lon[ctsa_centers$abbreviation == session$userData$nearest_center], ctsa_centers$lat[ctsa_centers$abbreviation == session$userData$nearest_center], 
                          popup = ctsa_centers$abbreviation[ctsa_centers$abbreviation == session$userData$nearest_center], label = ctsa_centers$abbreviation[ctsa_centers$abbreviation == session$userData$nearest_center], 
                          icon = icon.nearest_center, layerId = ctsa_centers$abbreviation[ctsa_centers$abbreviation == session$userData$nearest_center], group = "nearest_center")
      
      
    }
    
    
    leafletProxy("map") %>%
      addAwesomeMarkers(ctsa_centers$lon[ctsa_centers$abbreviation == marker_id], ctsa_centers$lat[ctsa_centers$abbreviation == marker_id], 
                        popup = ctsa_centers$abbreviation[ctsa_centers$abbreviation == marker_id], label = ctsa_centers$abbreviation[ctsa_centers$abbreviation == marker_id], 
                        icon = icon.center_clicked, layerId = ctsa_centers$abbreviation[ctsa_centers$abbreviation == marker_id], group = "clicked_center")
    
    
    
    # Store the current marker ID as the previously clicked marker
    session$userData$prev_marker_id <- marker_id    
  })  
  
  
  # Initialize the default selected center based on the nearest center
  observe({
    if (!is.null(input$selected_address)) {
      nearest_center <- drive_time_output %>%
        filter(address == input$selected_address) %>%
        pull(nearest_center)
      
      if (!is.null(nearest_center)) {
        updateSelectInput(session, "selected_center", selected = nearest_center)  
        
        leafletProxy("map") %>%
          clearGroup(group = "nearest_center") %>%
          addAwesomeMarkers(ctsa_centers$lon[ctsa_centers$abbreviation == nearest_center], ctsa_centers$lat[ctsa_centers$abbreviation == nearest_center], 
                            popup = ctsa_centers$abbreviation[ctsa_centers$abbreviation == nearest_center], label = ctsa_centers$abbreviation[ctsa_centers$abbreviation == nearest_center], 
                            icon = icon.nearest_center, layerId = ctsa_centers$abbreviation[ctsa_centers$abbreviation == nearest_center], group = "nearest_center")
        
        
        session$userData$nearest_center <- nearest_center 
      }
    }
  })  
  
  # Render table with information
  output$info_table <- renderTable({
    if (!is.null(selected_coordinates()$lat) && !is.null(selected_coordinates()$lon)) {
      drive_time_output %>%
        filter(lat == selected_coordinates()$lat & lon == selected_coordinates()$lon)
    } else if (!is.null(input$selected_address)) {
      drive_time_output %>%
        filter(address == input$selected_address)
    } else {
      data.frame()
    }
  })
}

# Run the application
shinyApp(ui, server)
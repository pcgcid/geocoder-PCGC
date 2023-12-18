# Install required packages if not already installed
# install.packages(c("shiny", "leaflet", "dplyr", "sf", "tidygraph", "ggraph"))
source("utils.R")

# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(tidygraph)
library(ggraph)
library(leaflet.extras)
library(tibble)



# icon.glyphicon <- makeAwesomeIcon(icon = "flag", markerColor = "blue",
#                                   iconColor = "black", library = "glyphicon",
#                                   squareMarker =  TRUE)
icon.center <- makeAwesomeIcon(icon = "medkit", markerColor = "blue", library = "ion")
icon.center_clicked <- makeAwesomeIcon(icon = "medkit", markerColor = "red", library = "ion")
icon.nearest_center <- makeAwesomeIcon(icon = "medkit", markerColor = "orange", library = "ion")
icon.ion <- makeAwesomeIcon(icon = "home", markerColor = "green",
                            library = "ion")


# Load data from CSV files
# if (file.exists("./output.csv")){
#   drive_time_output <- read.csv("./output.csv")}
ctsa_centers <- read.csv("/app/ctsa_centers.csv")

# Define UI
ui <- fluidPage(
  fileInput("file","Upload the file"),
  selectInput("ID", "Select Participant ID", choices = ""),
  textInput(inputId = 'new_address', label = 'Address Input', placeholder = "Enter the address"),
  textInput(inputId = 'out_filename', label = 'Output File Name', placeholder = "Enter output file name", value = "/app/output.csv"),
  numericInput("score_threshold", "Enter score threshold", value = 0.5),
  
  numericInput("new_lat", "Enter Latitude", value = NA),
  numericInput("new_lon", "Enter Longitude", value = NA),
  selectInput("selected_center", "Select Center", choices = ctsa_centers$abbreviation,selected = NULL),
  actionButton("submit_button", "Submit address or latitude & longtitude"),
  
  leafletOutput("map"),
  tableOutput("info_table")
)





# Define server logic
server <- function(input, output, session) {
  
  #drive_time_output <- reactiveVal(NULL)
  tempfile_path <- reactiveVal("/tmp/temp.csv")

  observeEvent(input$submit_button, {
    # Extract values from inputs
    address <- ifelse(!is.null(input$new_address), input$new_address, NA)
    lon <- ifelse(!is.null(input$new_lon), input$new_lon, NA)
    lat <- ifelse(!is.null(input$new_lat), input$new_lat, NA)
    
    # Add a new row to the dataframe
    if (!is.na(address) & trimws(address) != ""){data <- tibble(ID = 1, address = address)}
    
    else if (!is.na(lat) & !is.na(lon)){
      data <- tibble(ID = 1, address = NA, lon = lon, lat = lat)
    }
    write.csv(data,tempfile_path(),row.names = F)

  })
  
  
  drive_time_output <- reactive({
    req(isTruthy(input$file) || (isTruthy(input$submit_button) && ( isTruthy(input$new_address) || isTruthy(input$new_lat & input$new_lon))))
    filename = ifelse(!is.null(input$file), input$file$datapath, tempfile_path())
    
    out_filename = input$out_filename
    score_threshold = input$score_threshold
  
    drive_time_result = rdcrn_run(list(filename = filename, out_filename = out_filename, score_threshold = score_threshold)) 

      
    if (!is.null(drive_time_result) && nrow(drive_time_result) > 0){
      drive_time_result = drive_time_result %>% 
        dplyr::rename_with(., stringr::str_to_lower)
        
      #assign ID if not available
      if (!"id" %in% colnames(drive_time_result)) {
        drive_time_result$id = rownames(drive_time_result)}        
      drive_time_result = drive_time_result %>%
          dplyr::mutate(id = as.character(id))}
    
    return(drive_time_result)
  })
  
  

  
  observe({
    req(drive_time_output())
    if(!is.null(input$ID)){
      updateSelectInput(session, "ID", choices = drive_time_output()$id, selected = input$ID)}
    if (nrow(drive_time_output()) == 1){
      updateSelectInput(session, "ID", choices = drive_time_output()$id, selected = input$ID)
    }
  })

  
  # Observe block to update output whenever the file is uploaded

  
  
  
  # Combine selected address from the drop-down and manually entered coordinates
  selected_coordinates <- reactive({
    req(drive_time_output())

    if (!is.null(input$ID) && input$ID %in% drive_time_output()$id) {
      selected_data <- drive_time_output() %>%
        filter(id == input$ID)
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
      
      session$userData$prev_ID <- input$ID
      
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
        session$userData$prev_marker_id!= session$userData$nearest_center) || (session$userData$prev_ID != input$ID)) {
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
# Store the current marker ID as the previously clicked marker
session$userData$prev_marker_id <- marker_id

# Update the nearest center to the clicked marker
#session$userData$nearest_center <- input$selected_center
  })  
  
  
  # Initialize the default selected center based on the nearest center
  observeEvent(input$ID,{
    if (!is.null(input$ID)) {
      nearest_center <- drive_time_output() %>%
        filter(id == input$ID) %>%
        pull(nearest_center)
      
      if(!is.null(session$userData$prev_marker_id)){
        leafletProxy("map") %>%
          clearGroup(group = "clicked_center") %>%
          addAwesomeMarkers(ctsa_centers$lon[ctsa_centers$abbreviation == session$userData$prev_marker_id], ctsa_centers$lat[ctsa_centers$abbreviation == session$userData$prev_marker_id], 
                            popup = ctsa_centers$abbreviation[ctsa_centers$abbreviation == session$userData$prev_marker_id], label = ctsa_centers$abbreviation[ctsa_centers$abbreviation == session$userData$prev_marker_id], 
                            icon = icon.center, layerId = ctsa_centers$abbreviation[ctsa_centers$abbreviation == session$userData$prev_marker_id])
        
        
      }

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
  


  output$info_table <- renderTable({
    if (!is.null(selected_coordinates()$lat) && !is.null(selected_coordinates()$lon)) {
      drive_time_output() %>%
        filter(lat == selected_coordinates()$lat & lon == selected_coordinates()$lon) 
    } else if (!is.null(input$ID)) {
      drive_time_output() %>%
        filter(id == input$ID)
    } else {
      data.frame()
    }
  })
  
}

# Run the application
shinyApp(ui, server)
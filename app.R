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
library(DT)
library(shinyjs)
library(shinyBS)



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
cegir_centers <- read.csv("/app/CEGIRSites.csv")
colnames(cegir_centers) <- c("abbreviation", "consortium" ,"name","address","city","state","country",
                             "zipcode","website_url","geometry","lat","lon")

# Define UI
ui <- fluidPage(
  shinyjs::useShinyjs(),

  sidebarLayout(
    sidebarPanel(
    titlePanel("User input"),
    id = "input_fields",
    helpText("Please specify any parameters you wish to change before uploading file or submit an address"),
    helpText("Note: If you want to input an address manually, please hit “Reset data” if a file was previously uploaded"),
    fileInput("file","Upload the file"),
    selectInput("consortium", "Select Consortium", choices = c("CTSA","CEGIR"), selected = NULL),
    selectInput("ID", "Select Participant ID", choices = ""),
    textInput(inputId = 'new_address', label = 'Address Input', placeholder = "Enter the address"),
    textInput(inputId = 'out_filename', label = 'Output File Name', placeholder = "Enter output file name", value = "/tmp/output.csv"),
    textInput("score_threshold", "Enter score threshold","0.5"),
    
    bsTooltip(id = "score_threshold", title = "'all' or a numeric value as a threshold to be classified as 'geocoded'", 
              placement = "right", trigger = "hover"),
    
    numericInput("new_lat", "Enter Latitude", value = NA),
    numericInput("new_lon", "Enter Longitude", value = NA),
    #selectInput("selected_center", "Select Center", choices = "",selected = NULL),
    actionButton("submit_button", "Submit address or latitude & longtitude"),
    actionButton("reset_button", "Reset data")
  ),
  mainPanel(
    # Conditional rendering of text based on the number of rows in the table
    uiOutput("message2"),
    
    div(
      style = "display: flex; height: 10vh; overflow-x:auto; overflow-y:auto;",
      tableOutput("info_table")
    )
  ,

    div(
      style = "display: flex; align-items: center; justify-content: center; height: 55vh;",
      
      leafletOutput("map")
    ),
  uiOutput("message"),
  
  div(
    style = "display: flex; overflow-x:auto; overflow-y:auto;",
    tableOutput("warning_table")
    )
  )

  )
)

  






# Define server logic
server <- function(input, output, session) {
  
  #drive_time_output <- reactiveVal(NULL)
  tempfile_path <- reactiveVal("/tmp/temp.csv")
  
  #reactive values from user input
  out_filename <- reactive(input$out_filename)
  score_threshold <- reactive(type.convert(input$score_threshold, as.is = T))
  consortium <- reactive(tolower(input$consortium))
  centers = reactiveVal(NULL)
  
  selected_center = reactiveVal(NULL)
  
  #reactive values resulting from geocoding functions
  drive_time_output_all <- reactiveVal(NULL)
  drive_time_output <- reactiveVal(NULL)
  d_ctsa_list <- reactiveVal(NULL)
  d_cegir_list <- reactiveVal(NULL)

  
  
  
  
  # Initialize the map
  output$map <- renderLeaflet({
    leaflet() %>%
    addTiles() %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4)
  })
  
  
  observe({
    req(consortium(), drive_time_output())
    if (consortium() == "ctsa"){
      centers(ctsa_centers)
    }else if (consortium() == "cegir"){
      centers(cegir_centers)
      
    } 
    
    #updateSelectInput(session, "selected_center",choices = centers()$abbreviation)
    
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addAwesomeMarkers(centers()$lon, centers()$lat, popup = centers()$abbreviation, label = centers()$abbreviation, 
                                                        icon = icon.center, layerId = centers()$abbreviation
      ) 
    
  })
    
  

  
  observeEvent(input$file, {
  req(input$file)
    updateSelectInput(session, "ID", choices = "")
    updateTextInput(session, "new_address", value = "")
    filename <- input$file$datapath
    
    
    drive_time_result <- rdcrn_run(list(filename = filename, out_filename = out_filename(), score_threshold = score_threshold()))
    
 
    drive_time_output_all(drive_time_result)
    
  })
  

  observe({
    req(consortium(), drive_time_output_all())

    drive_time_result = drive_time_output_all()$output_df %>%
      select(-address, -matches("^matched"))
    
    d_ctsa_list(drive_time_output_all()$d_ctsa_list)
    d_cegir_list(drive_time_output_all()$d_cegir_list)
    

    if (!is.null(drive_time_result) && nrow(drive_time_result) > 0) {
      drive_time_result <- drive_time_result %>%
        dplyr::rename_with(., stringr::str_to_lower)

      # Assign ID if not available
      if (!"id" %in% colnames(drive_time_result)) {
        drive_time_result$id <- rownames(drive_time_result)
      }

      # Define the pattern to create "nearest_center" and "distance" columns based on selected consortium
      pattern <- paste0("_", consortium())
      
      # Extracting and creating new columns for those with the pattern
      drive_time_result <- drive_time_result %>%
        dplyr::mutate(id = as.character(id),
                      !!!setNames(.[, grepl(pattern, names(.))], sub(paste0(pattern, ".*"), "", names(.)[grepl(pattern, names(.))])))
      
      if (!"geocode_result" %in% colnames(drive_time_result)){
        drive_time_result = drive_time_result %>%
          dplyr::mutate(geocode_result = ifelse(!(is.na(lat) & is.na(lon)),"geocoded",NA ))
      }
      
    }

    drive_time_output(drive_time_result)
  })
  
  
  
  observeEvent(input$submit_button, {
    req(isTruthy(input$file) || (isTruthy(input$submit_button) && (isTruthy(input$new_address) || (isTruthy(input$new_lat) & isTruthy(input$new_lon)))))
    
    shinyjs::reset("file")
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
    
    filename <- tempfile_path()
    
    drive_time_result <- rdcrn_run(list(filename = filename, out_filename = out_filename(), score_threshold = score_threshold()))
  
    
    drive_time_output_all(drive_time_result)
    
  })
  

  
  observeEvent(list(drive_time_output(),input$file),{
    req(drive_time_output())
    updateSelectInput(session, "ID", choices = drive_time_output()$id)
    #updateSelectInput(session, "ID", choices = unique(drive_time_output()[drive_time_output()$geocode_result == 'geocoded','id']))
    
  })
  

  # Observe block to update output whenever the file is uploaded
  
  
  
  
  # Combine selected address from the drop-down and manually entered coordinates
  selected_coordinates <- reactive({
    req(drive_time_output())
    
    if (!is.null(input$ID) && input$ID %in% drive_time_output()$id && !is.null(drive_time_output())) {
      selected_data <- drive_time_output() %>%
        filter(id == input$ID)
      return(list(lat = selected_data$lat, lon = selected_data$lon,nearest_center = selected_data$nearest_center))
    } else if (!is.na(input$new_lat) && !is.na(input$new_lon)) {
      return(list(lat = input$new_lat, lon = input$new_lon))
    } else {
      return(list(lat = NULL, lon = NULL, nearest_center = NULL))
    }
  })
  
  
  
  # Observe changes in the selected center and update the map
  observe({
    req(drive_time_output(), centers(),input$ID)
    leafletProxy("map") %>%
      clearShapes()
    
    if (!is.null(selected_coordinates()$lat) && !is.null(selected_coordinates()$lon)) {

      selected_center_info_data <- centers() %>%
        filter(abbreviation == selected_center() ) %>% 
        dplyr::rename_with(., stringr::str_to_lower) %>%
        select(lat, lon, abbreviation, city, state)
      
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
    #updateSelectInput(session, "selected_center", selected = centers()$abbreviation[centers()$abbreviation == marker_id])
    
    selected_center(input$map_marker_click$id)
    
    
    # Remove the previously clicked marker (if any) and add a new one with blue color for the previously clicked marker
    if ((!is.null(session$userData$prev_marker_id) && session$userData$prev_marker_id != marker_id &&
         session$userData$prev_marker_id!= session$userData$nearest_center) || (session$userData$prev_ID != input$ID)) {
      leafletProxy("map") %>%
        #clearGroup(group = "previous_center") %>%
        addAwesomeMarkers(
          centers()$lon[centers()$abbreviation == session$userData$prev_marker_id],
          centers()$lat[centers()$abbreviation == session$userData$prev_marker_id],
          popup = centers()$abbreviation[centers()$abbreviation == session$userData$prev_marker_id],
          label = centers()$abbreviation[centers()$abbreviation == session$userData$prev_marker_id],
          icon = icon.center,
          layerId = session$userData$prev_marker_id,
          group = "previous_center"
        )
    }
    

    if ((!is.null(session$userData$prev_marker_id) && session$userData$prev_marker_id != marker_id &&
         session$userData$prev_marker_id == session$userData$nearest_center) || (session$userData$prev_ID != input$ID)) {
      leafletProxy("map") %>%
        addAwesomeMarkers(centers()$lon[centers()$abbreviation == session$userData$nearest_center], 
                          centers()$lat[centers()$abbreviation == session$userData$nearest_center],
                          popup = centers()$abbreviation[centers()$abbreviation == session$userData$nearest_center], 
                          label = centers()$abbreviation[centers()$abbreviation == session$userData$nearest_center],
                          icon = icon.nearest_center, layerId = centers()$abbreviation[centers()$abbreviation == session$userData$nearest_center], 
                          group = "nearest_center")
      
      
        

    }    

    

    
    
    leafletProxy("map") %>%
      addAwesomeMarkers(centers()$lon[centers()$abbreviation == marker_id], centers()$lat[centers()$abbreviation == marker_id], 
                        popup = centers()$abbreviation[centers()$abbreviation == marker_id], label = centers()$abbreviation[centers()$abbreviation == marker_id], 
                        icon = icon.center_clicked, layerId = centers()$abbreviation[centers()$abbreviation == marker_id], group = "clicked_center")
    
    
    
    # Store the current marker ID as the previously clicked marker
    # Store the current marker ID as the previously clicked marker
    session$userData$prev_marker_id <- marker_id
    
  })  
  
  
  observeEvent(input$consortium,{
    session$userData$prev_marker_id = NULL
    session$userData$nearest_center = NULL
    #selected_center(NULL)
    
  })
  

  
  # Initialize the default selected center based on the nearest center
  observeEvent({input$ID 
              input$consortium
              },{
    req(centers())

    if (!is.null(input$ID) && !is.null(drive_time_output())) {
      nearest_center <- drive_time_output() %>%
        filter(id == input$ID) %>%
        pull(nearest_center)
      
      
      if(!is.null(session$userData$prev_marker_id)){
        leafletProxy("map") %>%
          clearGroup(group = "clicked_center") %>%
          addAwesomeMarkers(centers()$lon[centers()$abbreviation == session$userData$prev_marker_id], centers()$lat[centers()$abbreviation == session$userData$prev_marker_id], 
                            popup = centers()$abbreviation[centers()$abbreviation == session$userData$prev_marker_id], label = centers()$abbreviation[centers()$abbreviation == session$userData$prev_marker_id], 
                            icon = icon.center, layerId = centers()$abbreviation[centers()$abbreviation == session$userData$prev_marker_id])
        
        
      }


      if (!is.null(nearest_center)) {
        #updateSelectInput(session, "selected_center", selected = nearest_center)
        
        if (!is.null(session$userData$nearest_center)){
          leafletProxy("map") %>%
            clearGroup(group = "nearest_center") %>%
            addAwesomeMarkers(centers()$lon[centers()$abbreviation == session$userData$nearest_center], centers()$lat[centers()$abbreviation == session$userData$nearest_center],
                              popup = centers()$abbreviation[centers()$abbreviation == session$userData$nearest_center], label = centers()$abbreviation[centers()$abbreviation == session$userData$nearest_center],
                              icon = icon.center, layerId = centers()$abbreviation[centers()$abbreviation == session$userData$nearest_center]) 
        }
        
        leafletProxy("map") %>%
          addAwesomeMarkers(centers()$lon[centers()$abbreviation == nearest_center], centers()$lat[centers()$abbreviation == nearest_center],
                            popup = centers()$abbreviation[centers()$abbreviation == nearest_center], label = centers()$abbreviation[centers()$abbreviation == nearest_center],
                            icon = icon.nearest_center, layerId = centers()$abbreviation[centers()$abbreviation == nearest_center], group = "nearest_center")
        
        
        session$userData$nearest_center <- nearest_center
        
        selected_center(nearest_center)
        
      }
      
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$reset_button, {
    updateSelectInput(session, "ID", choices = "")
    updateTextInput(session, "new_address", value = "")
    updateNumericInput(session, "new_lat", value = NA)
    updateNumericInput(session, "new_lon", value = NA)
    #updateSelectInput(session, "selected_center", choices = centers()$abbreviation, selected = NULL)
    updateNumericInput(session, "score_threshold", value = 0.5)
    shinyjs::reset("file")
    drive_time_output(NULL)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addAwesomeMarkers(centers()$lon, centers()$lat, popup = centers()$abbreviation, label = centers()$abbreviation, 
                        icon = icon.center, layerId = centers()$abbreviation
      ) %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4) 
      
  })
  
  failed_data <- reactive({
    req(drive_time_output())
    if (!is.null(drive_time_output())){
      d = drive_time_output() %>%
        filter(geocode_result != 'geocoded')
      if (nrow(d) > 0){
        return(d)
      }else(data.frame())

    }else{
      data.frame()
    }
  })
  
  # Output the table
  output$warning_table <- renderTable(failed_data())
  
  output$message = renderUI({
    req(failed_data())
    if (!is.null(failed_data()) ) {
      if (nrow(failed_data()) > 0)
        tags$h4("Please examine the unsuccessfully geocoded data")
    }
  })
  
  
  
  
  
  selected_data <- eventReactive(list(input$ID,selected_center()),{
    if (!is.null(input$ID) && !is.null(drive_time_output())) {
      data = drive_time_output() %>%
        filter(id == input$ID) 
      #if data is geocoded, look for closest center. Otherwise, return empty dataframe
      if (!is.na(data$lat[1])){
        distances = drive_time_output_all()[[paste0('d_',consortium(),'_list')]][[input$ID]]
        #print(distances[[selected_center()]])
        
        data = data %>%
          mutate(selected_center = selected_center() ,
                 d_to_selected_center = distances[[selected_center()]])
      }
      data
        
    } else {
      data.frame()
    }
  })
  
  
  
  output$info_table <- renderTable(selected_data())
  
  output$message2 = renderUI({
    req(selected_data())
    if (!is.null(selected_data()) ) {
      if (nrow(selected_data()) > 0)
        "Selected participant's data:"
    }
  })
  
  
  

}

# Run the application
shinyApp(ui, server)
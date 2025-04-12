#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(httr)
library(jsonlite)
library(duckdb)
library(glue)
library(dplyr)
library(leaflet)
library(purrr)

# Predefined locations and caching setup
coordinates <- list(
     c(34.0522, -118.2437),
     c(33.9806, -117.3755),
     c(34.1495, -117.2345),
     c(33.6103, -114.5964),
     c(33.4484, -112.0740),
     c(35.1983, -111.6513),
     c(35.0844, -106.6504),
     c(34.9333, -104.6876),
     c(35.2210, -101.8313),
     c(35.2161, -100.2491),
     c(35.4676, -97.5164),
     c(36.7538, -95.2206),
     c(37.0842, -94.5133),
     c(38.7480, -90.4390),
     c(39.1200, -88.5435),
     c(39.7684, -86.1581),
     c(39.7589, -84.1916),
     c(40.4406, -79.9959),
     c(39.9995, -78.2341),
     c(40.7357, -74.1724)
)

location_names <- c(
     "Los Angeles, CA",
     "Riverside, CA",
     "San Bernardino, CA",
     "Blythe, CA",
     "Phoenix, AZ",
     "Flagstaff, AZ",
     "Albuquerque, NM",
     "Santa Rosa, NM",
     "Amarillo, TX",
     "Shamrock, TX",
     "Oklahoma City, OK",
     "Miami, OK",
     "Joplin, MO",
     "St. Louis, MO",
     "Effingham, IL",
     "Indianapolis, IN",
     "Dayton, OH",
     "Pittsburgh, PA",
     "Breezewood, PA",
     "Newark, NJ"
)

locations_df <- data.frame(
     name = location_names,
     lat = sapply(coordinates, `[`, 1),
     lon = sapply(coordinates, `[`, 2)
)

# Cache setup (persists for 1 hour)
route_cache <- list()
CACHE_EXPIRY <- 3600  # Seconds

# --------------------------------------
# UI with Map and Loading States
# --------------------------------------
ui <- fluidPage(
     titlePanel("🚛 Logistics Route Planner"),
     sidebarLayout(
          sidebarPanel(
               selectInput("start", "Start Location", choices = locations_df$name),
               selectInput("end", "End Location", choices = locations_df$name[2:20]),
               actionButton("generate", "Generate Route", class = "btn-primary"),
               tags$hr(),
               helpText("Real routing via OSRM | Cached results shown in green"),
               uiOutput("routeStats")
          ),
          mainPanel(
               leafletOutput("routeMap", height = "500px"),
               tableOutput("coordinatesTable")
          )
     ),
     tags$style(
          HTML(
               "
    .shiny-notification {
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
    }
  "
          )
     )
)

# --------------------------------------
# Server with Caching & Error Handling
# --------------------------------------
server <- function(input, output, session) {
     # Reactive values
     state <- reactiveValues(
          route_data = NULL,
          cached = FALSE,
          last_update = NULL,
          selected_indices = list(start = NULL, end = NULL)  # Track selected range
     )
     
     observeEvent(input$generate, {
          req(input$start, input$end)
          
          # Get selected indices
          start_idx <- which(locations_df$name == input$start)
          end_idx <- which(locations_df$name == input$end)
          state$selected_indices <- list(start = start_idx, end = end_idx)
          
          # Validate selection
          if (start_idx >= end_idx) {
               showNotification("Invalid route: End must be after Start!", type = "error")
               return()
          }
          
          selected_locations <- locations_df[start_idx:end_idx, ]
          cache_key <- digest::digest(paste(selected_locations$lon, selected_locations$lat))
          
          # Check cache
          if (!is.null(route_cache[[cache_key]])) {
               if (as.numeric(Sys.time()) - route_cache[[cache_key]]$timestamp < CACHE_EXPIRY) {
                    state$route_data <- route_cache[[cache_key]]$data
                    state$cached <- TRUE
                    state$last_update <- Sys.time()
                    return()
               }
          }
          
          # Show loading state
          showNotification("Routing through OSRM...",
                           duration = NULL,
                           id = "loading")
          
          # Retry logic with exponential backoff
          result <- NULL
          attempt <- 1
          max_attempts <- 3
          
          while (is.null(result) && attempt <= max_attempts) {
               tryCatch({
                    result <- withCallingHandlers({
                         # Build OSRM URL
                         waypoints <- paste(sapply(1:nrow(selected_locations), function(i)
                              paste(
                                   selected_locations$lon[i],
                                   selected_locations$lat[i],
                                   sep = ","
                              )),
                              collapse = ";")
                         
                         osrm_url <- glue(
                              "http://router.project-osrm.org/route/v1/driving/{waypoints}",
                              "?overview=full&geometries=geojson"
                         )
                         
                         # API call with timeout
                         response <- GET(osrm_url, timeout(10))
                         if (status_code(response) != 200)
                              stop("OSRM server error")
                         
                         # Process response
                         route_data <- fromJSON(content(response, "text"))
                         route_coords <- route_data$routes$geometry$coordinates[[1]] |>
                              as.data.frame() |>
                              setNames(c("lon", "lat"))
                         
                         # Update cache
                         route_cache[[cache_key]] <<- list(
                              data = list(
                                   coords = route_coords,
                                   duration = route_data$routes$duration
                              ),
                              timestamp = as.numeric(Sys.time())
                         )
                         
                         state$route_data <- route_cache[[cache_key]]$data
                         state$cached <- FALSE
                         state$last_update <- Sys.time()
                         
                         removeNotification("loading")
                    }, error = function(e) {
                         if (attempt == max_attempts) {
                              showNotification(paste("Routing failed:", e$message),
                                               type = "error")
                              removeNotification("loading")
                         }
                    })
               }, error = function(e) {
                    Sys.sleep(2^attempt)  # Exponential backoff
                    attempt <<- attempt + 1
               })
          }
          
          # Store raw OSRM coordinates
          state$raw_route <- route_coords
          
          # Store coordinates in reactive value
          state$interpolated_coords <- tryCatch({
               con <- dbConnect(duckdb::duckdb())
               dbExecute(con, "INSTALL spatial; LOAD spatial;")
               
               # Write OSRM coordinates to DuckDB
               dbWriteTable(con, "route_coords", state$route_data$coords, overwrite = TRUE)
               
               # Create geometry table
               dbExecute(
                    con,"
                    CREATE OR REPLACE TABLE route_geom AS
                    SELECT ST_MakeLine(LIST(ST_GeomFromText('POINT(' || lon || ' ' || lat || ')')))::GEOMETRY AS geom
                    FROM route_coords;"
                    )
               
               # Calculate hours
               travel_time_hours <- ceiling(state$route_data$duration / 3600)
               
               # FIXED SQL Query
               query <- glue_sql("
                    WITH steps AS (
                    SELECT generate_series AS step
                    FROM generate_series(0, {travel_time_hours}) AS gs
                    ),
                    interpolated AS (
                    SELECT
                    step,
                    ST_LineInterpolatePoint(
                    (SELECT geom FROM route_geom),
                    step::DOUBLE / {travel_time_hours}
                    ) AS point
                    FROM steps
                    )
                    SELECT
                    step,
                    ROUND(ST_X(point), 4) AS lon,
                    ROUND(ST_Y(point), 4) AS lat
                    FROM interpolated;", .con = con)
               
               # Get and return coordinates
               coords <- dbGetQuery(con, query)
               dbDisconnect(con, shutdown = TRUE)
               coords
          },
          error = function(e) {
               showNotification("Error generating coordinates", type = "error")
               NULL
          })
          
          # Update outputs
          output$coordinatesTable <- renderTable({
               req(state$interpolated_coords)
               
               state$interpolated_coords
          })
          
          output$routeMap <- renderLeaflet({
               req(state$raw_route, state$interpolated_coords)
               
               leaflet() |>
                    addTiles() |>
                    # Original OSRM route (detailed geometry)
                    addPolylines(
                         data = state$raw_route,
                         lng = ~lon, lat = ~lat,
                         color = "black", 
                         weight = 3,
                         group = "Actual Route"
                    ) |>
                    # Interpolated hourly points
                    addCircleMarkers(
                         data = state$interpolated_coords,
                         lng = ~lon, lat = ~lat,
                         radius = 5, 
                         color = "blue",
                         popup = ~paste("Hour:", step),
                         group = "Hourly Positions"
                    ) |>
                    addLayersControl(
                         overlayGroups = c("Actual Route", "Hourly Positions"),
                         options = layersControlOptions(collapsed = FALSE)
                    )
          })
          
     })
     
     output$routeStats <- renderUI({
          req(state$last_update)
          
          tagList(tags$div(
               class = ifelse(state$cached, "alert alert-success", "alert alert-info"),
               role = "alert",
               tags$strong(ifelse(
                    state$cached, "Cached Route", "Fresh Route"
               )),
               tags$p(
                    icon("clock"),
                    "Last updated: ",
                    format(state$last_update, "%Y-%m-%d %H:%M:%S")
               )
          ))
     })
     
}

shinyApp(ui, server)

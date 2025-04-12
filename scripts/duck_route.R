library(httr)
library(jsonlite)
library(dplyr)
library(glue)

duck_route <- function(con) {
     # Define start/end points (San Francisco to Los Angeles)
     start_lon <- -122.4194
     start_lat <- 37.7749
     end_lon <- -118.2437
     end_lat <- 34.0522
     
     # Query OSRM API for route geometry
     osrm_url <- glue(
          "http://router.project-osrm.org/route/v1/driving/",
          "{start_lon},{start_lat};{end_lon},{end_lat}?overview=full&geometries=geojson"
     )
     
     response <- GET(osrm_url)
     route_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
     
     # Extract coordinates (OSRM returns [lon, lat] pairs)
     route_coords <- route_data$routes$geometry$coordinates[[1]] |>
          as.data.frame() |>
          setNames(c("lon", "lat"))
     
     # head(route_coords)  # Preview the coordinates
     
     dbExecute(con, "INSTALL spatial; LOAD spatial;")  # Load spatial extension
     
     # Write OSRM coordinates to DuckDB
     dbWriteTable(con, "route_coords", route_coords, overwrite = TRUE)
     
     # Convert coordinates to a LineString geometry
     dbExecute(
          con,"
          CREATE OR REPLACE TABLE route_geom AS
          SELECT
               ST_MakeLine(
               LIST(ST_GeomFromText('POINT(' || lon || ' ' || lat || ')'))
               )::GEOMETRY AS geom
          FROM route_coords;"
          )
     
     # Verify the geometry
     # route_check <- dbGetQuery(con, "SELECT ST_AsText(geom) FROM route_geom;")
     # print(route_check)  # Should output a LINESTRING with many points
     
     # Travel time is calculated from OSRM. Hourly positions are then interpolated. ----
     
     # Get travel time from OSRM (in seconds)
     travel_time_seconds <- route_data$routes$duration
     travel_time_hours <- ceiling(travel_time_seconds / 3600)  # Round up
     
     # Interpolate points along the route
     query <- glue_sql(
          "
     WITH steps AS (
          SELECT
               generate_series AS step
          FROM generate_series(0, {travel_time_hours})
     ),
     interpolated_points AS (
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
          ST_X(point) AS lon,
          ST_Y(point) AS lat
     FROM interpolated_points;
",
          .con = con
     )
     
     coordinates <- dbGetQuery(con, query)
     # print(coordinates)
     
     route_plot <- ggplot(route_coords, aes(x = lon, y = lat)) +
          geom_path(color = "blue") +
          labs(title = "OSRM Route from San Francisco to Los Angeles") +
          theme_minimal()
     
     return(route_plot)
}

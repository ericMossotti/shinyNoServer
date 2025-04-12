
read_parquet_glue <- function(con, dataset_path) {
     # Create parameterized query with glue_sql
     query <- glue::glue_sql(
          "
               CREATE OR REPLACE TABLE historical_data AS
               SELECT *
               FROM read_parquet(
               'data/historical_weather/*/part-0.parquet',
               hive_partitioning = true);",
          .con = con
     )
     
     return(dbExecute(con, query))
}

clean_transform_forecast <- function(con) {
     query <- glue::glue_sql("
     CREATE OR REPLACE TABLE forecast_data AS
     WITH cleaned_data AS (
          SELECT
               date,
               ROUND(temperature_2m::FLOAT, 1) AS temperature_2m,
               precipitation_probability,
               ROUND(precipitation::FLOAT, 3) AS precipitation,
               ROUND(rain::FLOAT, 3) AS rain,
               ROUND(showers::FLOAT, 3) AS showers,
               ROUND(snowfall::FLOAT, 3) AS snowfall,
               ROUND(snow_depth::FLOAT, 3) AS snow_depth,
               weather_code,
               ROUND(visibility::FLOAT, 1) AS visibility,
               ROUND(wind_speed_10m::FLOAT, 2) AS wind_speed_10m,
               wind_direction_10m,
               latitude,
               longitude
          FROM forecast_data
          ),
     
     transformed_data AS (
          SELECT
               *,
               -- Speed bin
               CASE
                    WHEN wind_speed_10m <= 2 THEN CAST('0-2' AS speed_bin_enum)
                    WHEN wind_speed_10m <= 4 THEN CAST('2-4' AS speed_bin_enum)
                    WHEN wind_speed_10m <= 6 THEN CAST('4-6' AS speed_bin_enum)
                    WHEN wind_speed_10m <= 8 THEN CAST('6-8' AS speed_bin_enum)
                    WHEN wind_speed_10m <= 10 THEN CAST('8-10' AS speed_bin_enum)
                    ELSE CAST('10+' AS speed_bin_enum)
               END AS speed_bin,
               -- Cardinal direction
               CASE
                    WHEN wind_direction_10m BETWEEN 0 AND 22.5 THEN CAST('N' AS cardinal_direction_enum)
                    WHEN wind_direction_10m BETWEEN 22.5 AND 67.5 THEN CAST('NE' AS cardinal_direction_enum)
                    WHEN wind_direction_10m BETWEEN 67.5 AND 112.5 THEN CAST('E' AS cardinal_direction_enum)
                    WHEN wind_direction_10m BETWEEN 112.5 AND 157.5 THEN CAST('SE' AS cardinal_direction_enum)
                    WHEN wind_direction_10m BETWEEN 157.5 AND 202.5 THEN CAST('S' AS cardinal_direction_enum)
                    WHEN wind_direction_10m BETWEEN 202.5 AND 247.5 THEN CAST('SW' AS cardinal_direction_enum)
                    WHEN wind_direction_10m BETWEEN 247.5 AND 292.5 THEN CAST('W' AS cardinal_direction_enum)
                    WHEN wind_direction_10m BETWEEN 292.5 AND 337.5 THEN CAST('NW' AS cardinal_direction_enum)
                    WHEN wind_direction_10m BETWEEN 337.5 AND 360 THEN CAST('N' AS cardinal_direction_enum)
                    ELSE NULL
               END AS wind_direction_cardinal,
               -- 15-degree direction bin (numeric)
               FLOOR((wind_direction_10m - 1e-9) / 15) * 15 AS direction_bin
          FROM cleaned_data
          ),

     final_data AS (
          SELECT
               *,
               -- Direction angle
               CASE
                    WHEN wind_direction_cardinal = 'N' THEN 0
                    WHEN wind_direction_cardinal = 'NE' THEN 45
                    WHEN wind_direction_cardinal = 'E' THEN 90
                    WHEN wind_direction_cardinal = 'SE' THEN 135
                    WHEN wind_direction_cardinal = 'S' THEN 180
                    WHEN wind_direction_cardinal = 'SW' THEN 225
                    WHEN wind_direction_cardinal = 'W' THEN 270
                    WHEN wind_direction_cardinal = 'NW' THEN 315
                    ELSE NULL
               END AS direction_angle,
               -- Visibility category
               CASE
                    WHEN visibility > 30000 THEN CAST('Clearest (>30 km)' AS visibility_cat_enum)
                    WHEN visibility > 10000 THEN CAST('Excellent (10-30 km)' AS visibility_cat_enum)
                    WHEN visibility > 5000 THEN CAST('Good (5-10 km)' AS visibility_cat_enum)
                    WHEN visibility > 2000 THEN CAST('Moderate (2-5 km)' AS visibility_cat_enum)
                    WHEN visibility > 1000 THEN CAST('Low (1-2 km)' AS visibility_cat_enum)
                    WHEN visibility <= 1000 THEN CAST('Fog/Haze (<1 km)' AS visibility_cat_enum)
                    ELSE NULL
               END AS visibility_category,
               -- Date parts
               strftime(date, '%Y-%m-%d') AS date_only,
               EXTRACT(YEAR FROM date) AS year,
               EXTRACT(MONTH FROM date) AS month,
               EXTRACT(hour FROM date) AS hour,
               monthname(date)::month_name_enum AS month_name,
               strftime(date, '%b')::month_abb_enum AS month_abb,
               EXTRACT(DAY FROM date) AS day,
               dayname(date)::weekday_name_enum AS weekday_name,
               strftime(date, '%a')::weekday_abb_enum AS weekday_abb,
               strftime(date, '%b %d') AS month_day,
               strftime(date, '%H:%M:%S') AS time_only,
               strptime('1970-01-01 ' || strftime(date, '%H:%M:%S'), '%Y-%m-%d %H:%M:%S') AS common_date
          FROM transformed_data
     )

     -- Final output
     SELECT * FROM final_data;", .con = con)

     dbExecute(con, query)
     
     }

# Creates enums ----
create_more_enums <- function(con) {
     query <- glue::glue_sql(
          "
     CREATE TYPE cardinal_direction_enum AS ENUM (
          'N',
          'NE',
          'E',
          'SE',
          'S',
          'SW',
          'W',
          'NW'
     );

     CREATE TYPE month_name_enum AS ENUM (
          'January',
          'February',
          'March',
          'April',
          'May',
          'June',
          'July',
          'August',
          'September',
          'October',
          'November',
          'December'
     );

     CREATE TYPE month_abb_enum AS ENUM (
          'Jan',
          'Feb',
          'Mar',
          'Apr',
          'May',
          'Jun',
          'Jul',
          'Aug',
          'Sep',
          'Oct',
          'Nov',
          'Dec'
     );

     CREATE TYPE weekday_name_enum AS ENUM (
          'Sunday',
          'Monday',
          'Tuesday',
          'Wednesday',
          'Thursday',
          'Friday',
          'Saturday'
     );

     CREATE TYPE weekday_abb_enum AS ENUM (
          'Sun',
          'Mon',
          'Tue',
          'Wed',
          'Thu',
          'Fri',
          'Sat'
     );

     CREATE TYPE visibility_cat_enum AS ENUM (
          'Clearest (>30 km)',
          'Excellent (10-30 km)',
          'Good (5-10 km)',
          'Moderate (2-5 km)',
          'Low (1-2 km)',
          'Fog/Haze (<1 km)'
     );

     CREATE TYPE speed_bin_enum AS ENUM (
          '0-2',
          '2-4',
          '4-6',
          '6-8',
          '8-10',
          '10+'
     );"
     )
     
     return(dbExecute(duckdb_con, query))
}

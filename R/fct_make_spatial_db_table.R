#' Add right and left tables to PostGIS and prepare for spatial join
#'
#' Add right and left tables to PostGIS, convert to geography type, and create spatial indexes.
#'
#' @param con connection object to PostGIS database
#' @param left_table sf spatial data frame of left table in spatial join. Geometries for this object will be retained.
#' @param left_table_name name of left table created in PostGIS
#' @param right_table sf spatial data frame of right table in spatial join. Geometries for this object are dropped and column names are added to `left_table` based on the largest intersection.
#' @param right_table_name name of right table created in PostGIS
#'
#' @export
#'
make_spatial_db_table <- function(con, left_table, left_table_name, right_table, right_table_name) {

  # create tables in postgis
  sf::st_write(left_table, dsn = con, layer = left_table_name, delete_layer = TRUE)
  sf::st_write(right_table, dsn = con, layer = right_table_name, delete_layer = TRUE)

  DBI::dbExecute(
    con,
    paste0("ALTER TABLE ", left_table_name, " ADD COLUMN geog geography;")
  )

  DBI::dbExecute(
    con,
    paste0("UPDATE ", left_table_name, " SET geog = ST_Transform(geometry, 4326)::geography;")
  )

  DBI::dbExecute(
    con,
    paste0("CREATE INDEX gix_l ON ", left_table_name, " USING GIST (geog);")
  )

  DBI::dbExecute(
    con,
    paste0("VACUUM ANALYZE ", left_table_name, ";")
  )

  DBI::dbExecute(
    con,
    paste0("ALTER TABLE ", right_table_name, " ADD COLUMN geog geography;")
  )

  DBI::dbExecute(
    con,
    paste0("UPDATE ", right_table_name, " SET geog = ST_Transform(geometry, 4326)::geography;")
  )

  DBI::dbExecute(
    con,
    paste0("CREATE INDEX gix_r ON ", right_table_name, " USING GIST (geog);")
  )

  DBI::dbExecute(
    con,
    paste0("VACUUM ANALYZE ", right_table_name, ";")
  )
}

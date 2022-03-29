#' @export

db_spatial_join_tables <- function(con, left_names, right_names, left_table_name, right_table_name) {

  left_select <- c()
  for (i in left_names) {
    left_select <- c(left_select, paste0("l.", i))
  }

  # here we only want to keep geometry associated with left table but keep columns in the right table
  right_select <- c()
  for (i in right_names) {
    if (i == "geometry") {
      next
    } else {
      right_select <- c(right_select, paste0("r.", i))
    }
  }

  left_select <- stringr::str_c(left_select, collapse = ", ")
  right_select <- stringr::str_c(right_select, collapse = ", ")

  sql <- paste0(
    "CREATE TABLE tmp_spatial_join AS SELECT ",
    left_select, ", ",
    right_select,
    " FROM ",
    left_table_name,
    " AS l LEFT JOIN LATERAL (",
    "SELECT b.*,",
    " ST_Area(ST_Intersection(l.geog, b.geog)) AS intersect_area",
    " FROM ",
    right_table_name,
    " AS b",
    " WHERE ST_IsValid(l.geometry) AND ST_IsValid(b.geometry) AND ST_Intersects(l.geog, b.geog)",
    " ORDER BY intersect_area DESC",
    " LIMIT 1)",
    " AS r ON TRUE;"
  )

  print(sql)

  DBI::dbExecute(
    con,
    sql
  )

  tmp_sf <- sf::st_read(con, layer="tmp_spatial_join")

  # tidy up
  DBI::dbExecute(con, "DROP TABLE tmp_spatial_join;")
  DBI::dbExecute(con, paste0("DROP TABLE ", left_table_name, ";"))
  DBI::dbExecute(con, paste0("DROP TABLE ", right_table_name, ";"))

  tmp_sf

}

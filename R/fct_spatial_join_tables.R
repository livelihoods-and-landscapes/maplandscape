#' Spatial join of two layers
#'
#' Perform a spatial inner join or left join operation on two layers based on
#' largest intersection between spatial features. Spatial joins are performed
#' using \href{https://r-spatial.github.io/sf/index.html}{sf}'s
#' \link[sf]{st_join}.
#'
#' Geometries of both spatial tables are checked for validity (using
#' \href{https://r-spatial.github.io/sf/index.html}{sf}'s \code{st_is_valid})
#' and that they are not empty (using
#' \href{https://r-spatial.github.io/sf/index.html}{sf}'s \code{st_is_empty}).
#' Invalid geometries are dropped before performing spatial joins.
#'
#' @param f_left left table in spatial joins - a spatial
#'   (\href{https://r-spatial.github.io/sf/index.html}{sf}) data frame.
#' @param f_right Right table in spatial joins - a spatial
#'   (\href{https://r-spatial.github.io/sf/index.html}{sf}) data frame.
#' @param j_type A character specifying the type of join to perform. Options for
#'   spatial joins include inner and left joins.
#'
#' @return \code{j_df} A layer of class - a spatial
#'   (\href{https://r-spatial.github.io/sf/index.html}{sf}) data frame (i.e.
#'   spatial features and attribute columns) returned by the spatial join.
#' @export

spatial_join_tables <- function(f_left, f_right, j_type) {

  # catch empty or invalid polygons
  l_df <- f_left[sf::st_is_valid(f_left) != FALSE, ]
  l_df <- l_df[sf::st_is_empty(l_df) == FALSE, ]

  # catch empty or invalid polygons
  r_df <- f_right[sf::st_is_valid(f_right) != FALSE, ]
  r_df <- r_df[sf::st_is_empty(r_df) == FALSE, ]

  if (j_type == "spatial_inner") {
    j_df <- sf::st_join(l_df, r_df, join = sf::st_intersects, left = FALSE, largest = TRUE)
  } else if (j_type == "spatial_left") {
    j_df <- sf::st_join(l_df, r_df, join = sf::st_intersects, left = TRUE, largest = TRUE)
  }
  j_df
}

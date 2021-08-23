#' Write a layer to a GeoPackage file
#'
#' Write a (spatial - \href{https://r-spatial.github.io/sf/index.html}{sf}) data frame to layer in a GeoPackage.
#'
#' Designed to be used with Shiny apps where a GeoPackage is in a temporary directory within the app instance.
#'
#' @param df Data frame or spatial data frame (\href{https://r-spatial.github.io/sf/index.html}{sf}) to write to GeoPackage.
#' @param gpkg_dir Data frame generated from \code{list_layers} indicating names, file types, and paths to files uploaded tables.
#' @param lyr User selected layer - this is used for selecting the relevant layer from a GeoPackage with many tables.
#'
#' @import shiny
#' @export

write_tables <- function(df, gpkg_dir, lyr) {
  req(gpkg_dir, lyr)

  a_lyr <- gpkg_dir %>%
    dplyr::filter(gpkg_dir[["layer_disp_name_idx"]] == lyr)

  sf::st_write(
    obj = df,
    dsn = a_lyr$file_path,
    layer = a_lyr$layers,
    delete_layer = TRUE
  )
}

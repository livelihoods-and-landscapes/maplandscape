#' Read tabular data - either spatial layer (i.e. a table in a GeoPackage) or csv file.
#'
#' @param uploads Data frame generated from \code{list_layers} indicating names, file types, and paths to uploaded layers.
#' @param lyr User selected layer - this is used for selecting the relevant layer from a GeoPackage with many layers.
#'
#' @return  a (spatial) data frame of the selected layer.
#'
#' @export

read_tables <- function(uploads, lyr) {
  req(uploads, lyr)
  a_lyr <- uploads %>%
    dplyr::filter(layer_disp_name_idx == lyr)

  if (a_lyr$file_type == "gpkg") {
    a_df <- sf::st_read(a_lyr$file_path, layer = a_lyr$layers, stringsAsFactors = FALSE)
    if ("sf" %in% class(a_df)) {
      a_df <- sf::st_sf(sf::st_set_geometry(a_df, NULL), geometry = sf::st_geometry(a_df))
    }
  } else if (a_lyr$file_type == "csv") {
    a_df <- dplyr::read_csv(a_lyr$file_path)
  }
  a_df
}

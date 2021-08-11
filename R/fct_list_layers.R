#' Return layers from uploaded data
#'
#' \code{list_layers} returns the name and path to layers from data
#' uploaded to the application as a tibble (data frame like object). If a GeoPackage is uploaded which
#' contains more than one layer, each layer and the path to that layer will be
#' recorded on a separate row of the returned tibble.
#'
#' @param f_path A character vector of file paths for GeoPackage(s) uploaded by the user.
#' @param f_name A character vector of file names of GeoPackage(s) uploaded by the user.
#'
#' @return A tibble with three columns: layers - layer name;
#'   layer_disp_name - clean and informative layer name for select
#'   input; file_path - temporary file path to data; and file_type - file type.
#'
#' @export

list_layers <- function(f_path, f_name) {
  f_type <- xfun::file_ext(f_path)

  if (f_type == "zip") {
    tmp_clean <- xfun::sans_ext(f_name)
    tmp_dir <- fs::path_temp(tmp_clean)
    unzip(f_path, exdir = tmp_dir)
    unzipped_fpaths <- fs::dir_ls(tmp_dir)
    tmp_tbl_out <- tibble::tibble()

    for (i in unzipped_fpaths) {
      unzipped_fpath <- i
      unzipped_fname <- basename(unzipped_fpath)
      unzipped_ftype <- xfun::file_ext(unzipped_fname)

      if (unzipped_ftype == "gpkg") {
        tmp_layers <- sf::st_layers(unzipped_fpath)
        tmp_layers <- tmp_layers$name
        y_clean <- xfun::sans_ext(unzipped_fname)
        layer_disp_name <- paste0(tmp_layers, " (", y_clean, ")")
        tbl_tmp <- tibble::tibble(layers = tmp_layers, layer_disp_name = layer_disp_name, file_path = unzipped_fpath, file_type = unzipped_ftype)
        tmp_tbl_out <- dplyr::bind_rows(tmp_tbl_out, tbl_tmp)
      } else if (unzipped_ftype == "csv") {
        y_clean <- xfun::sans_ext(unzipped_fname)
        layer_disp_name <- paste0(y_clean, " (csv)")
        tbl_tmp <- tibble::tibble(layers = unzipped_fname, layer_disp_name = layer_disp_name, file_path = unzipped_fpath, file_type = unzipped_ftype)
        tmp_tbl_out <- dplyr::bind_rows(tmp_tbl_out, tbl_tmp)
      }
    }
    tbl_out <- tmp_tbl_out
  } else if (f_type == "gpkg") {
    tmp_layers <- sf::st_layers(f_path)
    tmp_layers <- tmp_layers$name
    y_clean <- xfun::sans_ext(f_name)
    layer_disp_name <- paste0(tmp_layers, " (", y_clean, ")")
    tbl_out <- tibble::tibble(layers = tmp_layers, layer_disp_name = layer_disp_name, file_path = f_path, file_type = f_type)
  } else if (f_type == "csv") {
    y_clean <- xfun::sans_ext(f_name)
    layer_disp_name <- paste0(y_clean, " (csv)")
    tbl_out <- tibble::tibble(layers = f_name, layer_disp_name = layer_disp_name, file_path = f_path, file_type = f_type)
  }
  tbl_out
}

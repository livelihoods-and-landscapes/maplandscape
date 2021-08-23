#' Sync data collected using QField mobile GIS
#'
#' \code{sync_forms} syncs layers stored in GeoPackages to a template GeoPackage database or the initial "clean" QGS
#' project GeoPackage.
#'
#' @param template A data frame of the format returned by
#'   shiny's \code{fileInput}. This data frame stores the path of a single
#'   GeoPackage which can either be a central database that data collected in
#'   the field is synced to or a "clean" empty
#'   GeoPackage with the same layers as the uploaded forms.
#'
#' @param forms A data frame of the format returned by
#'   shiny's \code{fileInput}.
#'
#' @return A four element list \code{out_gpkg}, The first element is the path
#'   to a temporary GeoPackage storing data from \code{forms} synced with \code{
#'   template}. The second element is the date-time of the call to
#'   \code{sync_forms} - this is used for naming downloads of synced data. The
#'   third element is a data frame of the format returned by
#'   shiny's \code{fileInput} listing all layers returned from syncing
#'   \code{forms} with \code{template}. The fourth element is a path to a
#'   log file.
#'
#' @export


### TO-DOs
### check for duplicate geometries

sync_forms <- function(template, forms) {
  template_layers <- template$layers
  form_layers <- forms$layers

  # date-time that forms were synced - use for downloading datestamped geopackage
  dt <- Sys.time()
  dt <- stringr::str_replace_all(dt, ":", "-")
  dt <- stringr::str_replace(dt, " ", "-")

  # log file for any errors
  log <-
    fs::file_temp(
      pattern = "log",
      tmp_dir = tempdir(),
      ext = "txt"
    )

  # list object to return to storing:
  # element 1 - synced data as a geopackage
  # element 2 - date time of syncing
  # element 3 - data frame storing path to geopackages temp location and layer names
  # element 4 - log file
  out_gpkg <- vector(mode = "list", length = 4)
  out_gpkg[[1]] <- NULL
  out_gpkg[[2]] <- dt

  if (!rlang::is_empty(dplyr::intersect(template_layers, form_layers))) {

    # create temporary geopackage file to write synced data to
    temp_gpkg <-
      fs::file_temp(
        pattern = paste0("synced-forms", dt),
        tmp_dir = tempdir(),
        ext = "gpkg"
      )

    # iterate over each table / layer in template
    for (i in seq_along(template_layers)) {
      print(i)

      # read layer from geopackage or return an error message
      template_df <- tryCatch(
        error = function(cnd) paste0("could not load layer ", template$layer_disp_name[i]),
        {
          sf::st_read(
            template$file_path[i],
            layer = template$layers[i],
            stringsAsFactors = FALSE
          )
        }
      )

      template_colnames <- NULL
      # if sf object set geometry column name
      # if data frame or sf update lyr
      # if character object write error message to log
      if (any(class(template_df) == "sf")) {
        # set geometry to column to name "geometry"
        template_df <-
          sf::st_sf(
            sf::st_set_geometry(template_df, NULL),
            geometry = sf::st_geometry(template_df)
          )
        crs_template_df <- sf::st_crs(template_df)
        lyr <- template$layers[i]
        template_colnames <- colnames(template_df)
      } else if (any(class(template_df) == "data.frame")) {
        lyr <- template$layers[i]
        template_colnames <- colnames(template_df)
      } else if (any(class(template_df) == "character")) {
        readr::write_lines(
          template_df,
          log,
          sep = "\n",
          na = "NA",
          append = TRUE
        )
        lyr <- "layer could not be loaded"
      }

      # find matching tables in forms
      if (!rlang::is_empty(dplyr::intersect(lyr, form_layers))) {
        matched_forms <- forms %>%
          dplyr::filter(layers == lyr)

        # sync data in forms with matching tables to template
        for (k in 1:nrow(matched_forms)) {
          print(matched_forms$layer_disp_name[k])

          # read layer from geopackage or return an error message
          form_df <- tryCatch(
            error = function(cnd) paste0("could not load layer ", matched_forms$layer_disp_name[k]),
            {
              sf::st_read(
                matched_forms$file_path[k],
                layer = matched_forms$layers[k],
                stringsAsFactors = FALSE
              )
            }
          )

          form_colnames <- NULL
          # if sf object set geometry column name
          # if character object write error message to log
          if (any(class(form_df) == "sf")) {
            # set geometry to column to name "geometry"
            form_df <-
              sf::st_sf(sf::st_set_geometry(form_df, NULL),
                geometry = sf::st_geometry(form_df)
              )
            # transform crs to match crs of corresponding table in template
            form_df <- form_df %>%
              sf::st_transform(crs = crs_template_df)
            form_colnames <- colnames(form_df)
          } else if (any(class(form_df) == "data.frame")) {
            form_colnames <- colnames(form_df)
          } else if (any(class(form_df) == "character")) {
            readr::write_lines(
              form_df,
              log,
              sep = "\n",
              na = "NA",
              append = TRUE
            )
          }

          # checking type and colnames match before joining forms data to template
          if (any(class(template_df) == "sf") &
            any(class(form_df) == "sf") &
            !rlang::is_empty(dplyr::intersect(template_colnames, form_colnames))) {

            # keep only matching columns in forms df
            col_match <- form_colnames[form_colnames %in% template_colnames]
            form_df <- form_df %>% dplyr::select(tidyselect::all_of(col_match))

            template_df <-
              dplyr::bind_rows(
                template_df %>% as.data.frame(),
                form_df %>% as.data.frame()
              ) %>% dplyr::distinct()
            template_df <- sf::st_sf(template_df)
          } else if (any(class(template_df) == "data.frame" &
            any(class(form_df) == "data.frame")) &
            !rlang::is_empty(dplyr::intersect(template_colnames, form_colnames))) {

            # keep only matching columns in forms df
            col_match <- form_colnames[form_colnames %in% template_colnames]
            form_df <- form_df %>% dplyr::select(tidyselect::all_of(col_match))

            template_df <- dplyr::bind_rows(template_df, form_df)
            template_df <-
              dplyr::distinct(template_df, .keep_all = TRUE)
          }
        }
      }

      # write to temporary geopackage
      write_message <- NULL
      write_message <- tryCatch(
        error = function(cnd) paste0("could not write layer: ", template$layer_disp_name[i]),
        {
          sf::st_write(
            template_df,
            dsn = temp_gpkg,
            layer = lyr
          )
          paste0("layer written: ", template$layer_disp_name[i])
        }
      )
      readr::write_lines(
        write_message,
        log,
        sep = "\n",
        na = "NA",
        append = TRUE
      )
    }
  }

  out_gpkg[[1]] <- temp_gpkg
  f_name <- "synced_forms"
  layers <- list_layers(f_path = temp_gpkg, f_name = f_name)
  out_gpkg[[3]] <- layers
  out_gpkg[[4]] <- log
  out_gpkg
}

#' Get GeoPackages from Google Cloud Storage bucket
#'
#' Makes a request to a user specified Google Cloud Storage bucket to get an object
#' (GeoPackage) in the bucket.
#'
#' @param token an OAuth2.0 token which allows a user to make authenticated HTTP
#'   requests to the Google Cloud Storage API.
#' @param bucket name of Google Cloud Storage bucket.
#' @param object name of GeoPackage to get.
#'
#' @return two element list. First element is the path to where the GeoPackage
#'   has been written or character vector with an error message. The second
#'   element is the file name to display to users when selecting the GeoPackage.
#'
#' @export

get_gcs_object <- function(token, bucket, object) {


  # Google Cloud Storage JSON API GET object https://cloud.google.com/storage/docs/json_api/v1/objects/get
  # set ?alt-media to get object data
  get_object <- paste0("https://storage.googleapis.com/storage/v1/b/", bucket, "/o/", object, "?alt=media")

  # create a HTTP get request to get object
  req <- httr::GET(
    get_object,
    httr::config(token = token)
  )

  # check HTTP request did not return an error code, if error code returned show message to user
  if (req$status_code > 399) {
    shiny::showNotification(paste0("response error: ", req$status_code), type = "error", duration = 5)
    return()
  }

  # if HTTP response status code < 400 try and extract list of objects in bucket, filter GeoPackages, and return list of GeoPackages
  if (req$status_code < 399) {
    res <- httr::content(req)

    # check response is raw
    if (typeof(res) == "raw") {
      gcs_gpkg <- tryCatch(
        error = function(cnd) {
          "cannot load GeoPackage from Google Cloud Storage"
        },
        {
          gcs_gpkg <-
            fs::file_temp(
              pattern = "",
              tmp_dir = tempdir(),
              ext = "gpkg"
            )
          writeBin(res, gcs_gpkg)

          # check GeoPackage can be read
          check_sf <- try(sf::st_read(gcs_gpkg))
          if ("try-error" %in% class(check_sf)) {
            gcs_gpkg <- "cannot load GeoPackage from Google Cloud Storage"
          }

          gcs_gpkg <- list(
            f_path = gcs_gpkg,
            f_name = object
          )

          gcs_gpkg
        }
      )
    }
  }
  # return a list with path to GeoPackage and name of GeoPackage
  gcs_gpkg
}

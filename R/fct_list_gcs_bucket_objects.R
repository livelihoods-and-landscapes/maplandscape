#' List GeoPackages in Google Cloud Storage bucket
#'
#' Makes a request to a user specified Google Cloud Storage bucket to list all
#' objects in the bucket. Filters the response to return a list of GeoPackages
#' (if any). This function uses the \code{httr} package for making authenticated HTTP requests and processing responses.
#'
#' @param token an OAuth2.0 token which allows a user to make authenticated HTTP requests to the Google Cloud Storage API.
#' @param bucket name of Google Cloud Storage bucket to list objects in.
#'
#' @return items a character vector of length 0 or greater listing the names of GeoPacakges in Google Cloud Storage bucket.


list_gcs_bucket_objects <- function(token, bucket) {

  # check token is not null - if null flash warning to user to login with Google
  if (is.null(token)) {
    shiny::showNotification("login with Google", type = "error", duration = 5)
    return()
  }

  # bucket to list objects in
  bucket_name <- paste0("https://storage.googleapis.com/storage/v1/b/", bucket, "/o")

  # create a HTTP get request to list objects in the bucket
  req <- httr::GET(
    bucket_name,
    httr::config(token = token)
  )

  # check HTTP request did not return an error code, if error code returned show message to user
  if (req$status_code > 399) {
    shiny::showNotification(paste0("response error: ", req$status_code), type = "error", duration = 5)
    return()
  }

  # if HTTP response status code < 400 try and extract list of objects in bucket, filter GeoPackages, and return list of GeoPackages
  items <- NULL
  if (req$status_code < 399) {
    res <- httr::content(req)
    kind <- try(res$kind)

    # check response is Google Cloud Storage object
    # GCS response is a JSON object with:
    # kind property - string and always "storage#objects" when listing bucket objects
    # items - list of objects
    # more info: https://cloud.google.com/storage/docs/json_api/v1/objects/list
    if (kind == "storage#objects") {
      items <- tryCatch(
        error = function(cnd) {
          "no items returned"
        },
        {
          items <- res$items
          if (length(items) > 0) {
            names_gpkg <- c()
            for (i in seq_along(items)) {
              names <- items[[i]]$name
              if (stringr::str_detect(names, ".gpkg$")) {
                names_gpkg <- c(names_gpkg, names)
              }
            }
          }
          names_gpkg
        }
      )
    }
  }

  items
}

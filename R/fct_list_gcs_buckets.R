#' List Google Cloud Storage buckets
#'
#' Makes a request to a user specified Google Cloud Storage bucket to get an object
#' (GeoPackage) in the bucket.
#'
#' @param token an OAuth2.0 token which allows a user to make authenticated HTTP
#'   requests to the Google Cloud Storage API.
#'
#' @return vector of bucket names.

list_gcs_buckets <- function(token, project_id) {

  # Google Cloud Storage JSON API GET object https://cloud.google.com/storage/docs/json_api/v1/objects/get
  # set ?alt-media to get object data
  get_buckets <- paste0("https://storage.googleapis.com/storage/v1/b?project=", project_id)

  # create a HTTP get request to get object
  req <- httr::GET(
    get_buckets,
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
    if (kind == "storage#buckets") {
      items <- tryCatch(
        error = function(cnd) {
          "no items returned"
        },
        {
          items <- res$items
          if (length(items) > 0) {
            names_bucket <- c()
            for (i in seq_along(items)) {
              names <- items[[i]]$name
              names_bucket <- c(names_bucket, names)
            }
          }
          names_bucket
        }
      )
    }
  }

  names_bucket
}

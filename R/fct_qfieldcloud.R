#' QFieldCloud login
#'
#' @param username QFieldCloud email
#' @param password QFieldCloud password
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#'
#' @return session token
#' @export
#'

qfieldcloud_login <- function(username, password, endpoint) {
  credentials <- list(
    email = username,
    password = password
  )

  # todo handle empty / faulty endpoint
  login_url <- paste0("https://", endpoint, "/api/v1/auth/login/")

  httr::handle_reset(login_url)

  token <- httr::POST(
    url = login_url,
    body = credentials,
    encode = "json"
  )

  status_code <- token$status_code

  if (status_code < 399) {
    login_status <- list(
      status = "success",
      token = httr::content(token, as = "parsed")$token
    )
  } else {
    login_status <- list(
      status = "fail",
      token = NULL
    )
  }

  login_status
}


#' Get QFieldCloud projects
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#'
#' @return data.frame with two columns storing project names and project ids
#' @export
#'
#' @examples
get_qfieldcloud_projects <- function(token, endpoint) {

  # todo handle query params to get community projects
  projects_url <- paste0("https://", endpoint, "/api/v1/projects?include-public=false")

  projects <- httr::GET(
    url = projects_url,
    httr::add_headers(Authorization = paste0("token ", token))
  )

  projects_parsed <- httr::content(projects, as = "parsed")

  names <- c()
  id <- c()

  for (i in projects_parsed) {
    names <- c(names, i$name)
    id <- c(id, i$id)
  }

  names <- data.frame(name = names, id = id)

  names
}

#' Get QFieldCloud files in project (GeoPackages only)
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id project id corresponding to project to download file from
#'
#' @return data.frame with two columns storing file names and project ids
#' @export
#'
#' @examples
list_qfieldcloud_gpkg <- function(token, endpoint, project_id) {
  files_url <- paste0("https://", endpoint, "/api/v1/files/", project_id, "/")

  files <- httr::GET(
    url = files_url,
    httr::add_headers(Authorization = paste0("token ", token))
  )

  files_parsed <- httr::content(files, as = "parsed")

  files_list <- c()

  for (i in files_parsed) {
    if (xfun::file_ext(i$name) == "gpkg") {
      files_list <- c(files_list, i$name)
    }
  }

  files_out <- data.frame(name = files_list, id = project_id)

  files_out
}

get_qfieldcloud_gpkg <- function(token, endpoint, project_id, filename) {
  filename_url <- paste0("https://", endpoint, "/api/v1/files/", project_id, "/", filename, "/")

  # need to use followlocation = FALSE to get redirect url
  file_data <- httr::with_config(httr::config(followlocation = FALSE), httr::GET(
    url = filename_url,
    httr::add_headers(Authorization = paste0("token ", token))
  ))

  location <- file_data$headers$location

  file_data <- httr::GET(
    url = location
  )

  # get content as raw and write to gpkg
  f_data <- httr::content(file_data, as = "raw")

  qfield_gpkg <-
    fs::file_temp(
      pattern = "",
      tmp_dir = tempdir(),
      ext = "gpkg"
    )

  writeBin(f_data, qfield_gpkg)

  # check GeoPackage can be read
  check_sf <- try(sf::st_read(qfield_gpkg))
  if ("try-error" %in% class(check_sf)) {
    qfield_gpkg <- "cannot load GeoPackage from QFieldCloud"
  }

  qfield_gpkg <- list(
    f_path = qfield_gpkg,
    f_name = filename
  )

  qfield_gpkg
}

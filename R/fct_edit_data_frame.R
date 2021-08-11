#' Apply edits from user to layers in a GeoPackage
#'
#' \code{edit_data_frame} applies user edits to a DataTables object
#' in a browser to the underlying (spatial) data frame.
#'
#' @param tmp_edits Data frame recording user supplied edits (derived from
#'  editing DataTables object in the UI).
#' @param df_to_edit Data frame or spatial data frame (sf) representing the layer in
#'  GeoPackage to apply user edits to.
#' @param df_to_edit_not_sf Data frame representing the layer in GeoPackage with the sf
#'  geometry object dropped.
#' @param layer Name of layer that edits are being applied to.
#'
#' @return A two-element list. The first element is the data frame with edits
#'  applied to it. The second element is a log recording the status of attempts
#'  to apply user edits to the underlying data frame object.
#'
#' @export

edit_data_frame <- function(tmp_edits, df_to_edit, df_to_edit_not_sf, layer) {
  log <- c()

  # loop over edits
  for (i in 1:nrow(tmp_edits)) {

    # get column type
    col_idx <- tmp_edits[i, 2]
    row_idx <- tmp_edits[i, 1]
    col_type <- class(df_to_edit_not_sf[, col_idx])
    from_user <- tmp_edits[i, 3]
    # cast edit to column type
    if ("character" %in% col_type) {
      from_user <- tryCatch(
        error = function(cnd) {
          "error casting user supplied value to character"
        },
        {
          as.character(from_user)
        }
      )
    } else if ("numeric" %in% col_type) {
      from_user <- tryCatch(
        error = function(cnd) {
          "error casting user supplied value to numeric"
        },
        {
          as.numeric(from_user)
        }
      )
    } else if ("integer" %in% col_type) {
      from_user <- tryCatch(
        error = function(cnd) {
          "error casting user supplied value to integer"
        },
        {
          as.integer(from_user)
        }
      )
    } else if ("double" %in% col_type) {
      from_user <- tryCatch(
        error = function(cnd) {
          "error casting user supplied value to double"
        },
        {
          as.double(from_user)
        }
      )
    } else if ("logical" %in% col_type) {
      from_user <- tryCatch(
        error = function(cnd) {
          "error casting user supplied value to logical"
        },
        {
          as.logical(from_user)
        }
      )
    } else if ("POSIXct" %in% col_type) {
      from_user <- tryCatch(
        error = function(cnd) {
          "error casting user supplied value to POSIXct"
        },
        {
          if (lubridate::is.POSIXct(from_user) | lubridate::is.POSIXt(from_user)) {
            as.POSIXct(from_user)
          } else {
            "error casting user supplied value to POSIXct"
          }
        }
      )
    } else {
      from_user <- "error user supplied value and data frame column type do not match"
    }
    # update column value
    if (((class(from_user) == "character") & (stringr::str_detect(from_user, "^error"))) | (is.na(from_user))) {
      log_message <- paste0("row index: ", row_idx, " col index: ", col_idx, " - ", from_user, " (layer: ", layer, ")")
      log <- c(log, log_message)
    } else {
      try(
        df_to_edit[row_idx, col_idx] <- from_user
      )
      log_message <- paste0("row index: ", row_idx, " col index: ", col_idx, " user supplied edit written (layer: ", layer, ")")
      log <- c(log, log_message)
    }
  }
  edits_out <- vector(mode = "list", length = 2)
  edits_out[[1]] <- df_to_edit
  edits_out[[2]] <- log
  edits_out
}

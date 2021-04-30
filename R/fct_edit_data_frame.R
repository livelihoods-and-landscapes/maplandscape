
edit_data_frame <- function(tmp_edits, df_to_edit, df_to_edit_not_sf) {

    # loop over edits
    for (i in 1:nrow(tmp_edits)){
      
      invalid_from_user <- "no"
      # get column type
      col_idx <- tmp_edits[i, 2]
      row_idx <- tmp_edits[i, 1]
      col_type <- class(df_to_edit_not_sf[, col_idx])
      from_user <- tmp_edits[i, 3]
      # cast edit to column type
      if ("character" %in% col_type) {
        try(
          from_user <- as.character(from_user)
        )
      } else if ("numeric" %in% col_type) {
        try(
          from_user <- as.numeric(from_user)
        )
      } else if ("integer" %in% col_type) {
        try(
          from_user <- as.integer(from_user)
        )
      } else if ("double" %in% col_type) {
        try(
          from_user <- as.double(from_user)
        )
      } else if ("logical" %in% col_type) {
        try(
          from_user <- as.logical(from_user)
        )
      } else if ("POSIXct" %in% col_type) {
        try(
          from_user <- as.POSIXct(from_user)
        )
      } else {
        from_user <- "user supplied value and data frame column type do not match"
        invalid_from_user <- "yes"
      }
      # update column value
      if (invalid_from_user == "no") {
        try(
          df_to_edit[row_idx, col_idx] <- from_user
        )
      }
      
    }
  df_to_edit
}

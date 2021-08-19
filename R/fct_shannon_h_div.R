#' Compute Shannon's H diversity index
#'
#' Shannon's H diversity index is a measure of the richness (abundance) evenness
#' (balance or proportional abundance of each element). Thus, to use the example
#' of a mixed cropping field. A field is more diverse if it has more crops grown
#' in it and an equal area is allocated to each crop.
#'
#' The function takes a series of numeric vectors as arguments. Each vector
#' represents the proportion of species within a geographic unit or group. The
#' function will check all input vectors are numeric. It will also rescale all
#' row wise elements to sum to one. Any species with a zero proportion will be
#' allocated a small proportion (0.00001) to enable Shannon's H to be computed.
#'
#' @param ... Two or more numeric vectors containing the proportion of a
#'   species. Each element (row) in the vector corresponds to a geographic unit
#'   or group that Shannon's H is calculated for.
#'
#' @return The Shannon's H diversity score for each unit; returned as a numeric
#'   vector.

shannon_h_div <- function(...) {
  vals <- list(...)
  df <- dplyr::bind_cols(vals)

  # check only numeric vectors uploaded
  types <- purrr::map_df(df, class)
  if (any(types %in% c("numeric", "integer", "double")) == FALSE) {
    return()
  }

  # rescale all values to add up to 1
  # compute normaliser
  df[df == 0] <- 0.00001
  normaliser <- 1 / (rowSums(df, na.rm = TRUE))
  df_scaled <- df * normaliser

  # compute Shannon's H
  sh <- rowSums(df_scaled * log(df_scaled)) * -1

  sh
}

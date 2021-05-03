#' Compute plant number
#'
#' @param acres The area of the crop in acres.
#' @param plant_spacing The plant spacing of between plants in a row in metres.
#' @param row_spacing The row spacing between rows of plants in metres.
#'
#' @return The number of plants in the field.
#'

plant_number <- function(acres, plant_spacing, row_spacing) {
  a <- acres / 1
  pn <- (a * 4047) / (plant_spacing * row_spacing)
  pn
}

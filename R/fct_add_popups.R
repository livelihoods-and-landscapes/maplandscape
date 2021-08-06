#' Add popup label when feature is clicked on a \code{Leaflet} web map
#'
#' \code{add_popups} creates a popup label containing values of selected
#' variables for a feature selected through an on-click event on a \code{Leaflet}
#' web map.
#'
#' @param in_df A \code{sf} data frame object that is displayed on the leaflet web
#'   map.
#' @param layer_id The layerId of the feature selected by the user.
#' @param label_vars The variables from \code{in_df} selected to be displayed in
#'   the popup.
#'
#' @return \code{popup_text} a html popup label to be displayed on the \code{Leaflet}
#' web map.
#'
#' @importFrom magrittr %>%

add_popups <- function(in_df, layer_id, label_vars) {
  feature <- in_df()[in_df()$layer_id == layer_id, ]
  # feature <- in_df() %>% dplyr::filter(layer_id == layer_id)
  popup_text <- vector()
  popup_text <- c(popup_text, "<p style='color:#2c3e50; background-color:#ecf0f1;'>")

  for (i in seq_along(label_vars())) {
    ii <- label_vars()[i]
    var_label <- paste("<b>", ii, ":</b>", feature[[ii]], "<br>\n")
    popup_text <- c(popup_text, var_label)
  }

  popup_text <- c(popup_text, "</p>")
  popup_text <- paste(popup_text, collapse = "")
  popup_text
}

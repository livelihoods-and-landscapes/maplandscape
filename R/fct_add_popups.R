#' Add a popup label when a feature is clicked on a Leaflet web map
#'
#' \code{add_popups} creates a popup label containing values of selected
#' columns for a feature selected through an on-click event on a \link[leaflet]{leaflet}
#' web map.
#'
#' @param in_df an \href{https://r-spatial.github.io/sf/index.html}{sf} data frame object that is displayed on the \link[leaflet]{leaflet} web
#'   map.
#' @param layer_id The \code{layerId} of the feature selected / clicked on by the user.
#' @param label_vars The column(s) from \code{in_df} selected to be displayed in
#'   the popup.
#'
#' @return a html popup label to be displayed on the Leaflet
#' web map.
#'
#' @export
#'
#' @importFrom magrittr %>%

add_popups <- function(in_df, layer_id, label_vars) {
  feature <- in_df()[in_df()$layer_id == layer_id, ]

  popup_text <- vector()
  popup_text <- c(popup_text, "<p style='color:#17141f; background-color:#fff;'>")

  for (i in seq_along(label_vars())) {
    ii <- label_vars()[i]
    var_label <- paste("<b>", ii, ":</b>", feature[[ii]], "<br>\n")
    popup_text <- c(popup_text, var_label)
  }

  popup_text <- c(popup_text, "</p>")
  popup_text <- paste(popup_text, collapse = "")
  popup_text
}

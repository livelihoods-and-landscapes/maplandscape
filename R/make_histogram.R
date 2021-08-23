#' Histogram
#'
#' Generate and style a histogram from a column in a (spatial -
#' \href{https://r-spatial.github.io/sf/index.html}{sf}) data frame. Designed to
#' be used in a Shiny application to visualise the distribution of continuous
#' variables in layers in a GeoPackage.
#'
#' This is a helper function to pass user defined data, aesthetic mappings, and chart styling parameters to \link[ggplot2]{ggplot} within a Shiny app.
#'
#' @param chart_active_df data frame containing a column whose values will be
#'   used to generate a histogram.
#' @param hist_x_var a single element character vector specifying the name of
#'   the column to use to generate a histogram.
#' @param binwidth numeric value specifying the width of the histogram's bins.
#' @param x_lab a single element character vector specifying the X-axis label.
#' @param y_lab a single element character vector specifying the Y-axis label.
#' @param axis_font_size a numeric value specifying the font size for axis tick
#'   labels.
#' @param lab_font_size a numeric value specifying the font size for axis
#'   labels.
#'
#' @return a \link[ggplot2]{ggplot} object that can be passed to
#'   \link[Shiny]{renderPlot} to display in a Shiny application.
#'
#' @export


make_histogram <- function(chart_active_df, hist_x_var, binwidth, x_lab, y_lab, axis_font_size, lab_font_size) {
  chart <-
    ggplot2::ggplot(chart_active_df, ggplot2::aes(.data[[hist_x_var]])) +
    ggplot2::geom_histogram(
      binwidth = binwidth,
      color = "#78c2ad",
      fill = "#78c2ad"
    ) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.background = ggplot2::element_rect(fill = NA, colour = "#78c2ad"),
      axis.text.x = ggplot2::element_text(
        angle = -45,
        vjust = 1,
        hjust = 0,
        size = axis_font_size
      ),
      axis.text.y = ggplot2::element_text(size = axis_font_size),
      axis.title.x = ggplot2::element_text(size = lab_font_size),
      axis.title.y = ggplot2::element_text(size = lab_font_size)
    )

  chart
}

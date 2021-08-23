#' Barplot
#'
#' Generate and style a bar plot which visualises summary statistics (mean, sum,
#' count) of values in a group. Designed to be used in  a Shiny application to
#' visualise results of \link[dplyr]{group_by} and
#' \link[dplyr]{summarise} operations.
#'
#' This is a helper function to pass user defined data, aesthetic mappings, and chart styling parameters to \link[ggplot2]{ggplot} within a Shiny app.
#'
#' @param col_chart_df a two column data frame. The first column specifies the
#'   groupings. The second column specifies the summary statistic per-group.
#' @param x_lab a single element character vector specifying the X-axis label.
#' @param y_lab a single element character vector specifying the Y-axis label.
#' @param axis_font_size a numeric value specifying the font size for axis tick
#'   labels.
#' @param lab_font_size a numeric value specifying the font size for axis
#'   labels.
#'
#' @return a \link[ggplot2]{ggplot} object that can be passed to
#'   \link[shiny]{renderPlot} to display in a Shiny application.
#'
#' @export
#'
#'
make_barplot <- function(col_chart_df, x_lab, y_lab, axis_font_size, lab_font_size) {
  chart <-
    ggplot2::ggplot(
      col_chart_df,
      ggplot2::aes(col_chart_df[, 1], col_chart_df[, 2])
    ) +
    ggplot2::geom_col(color = "#78c2ad", fill = "#78c2ad") +
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

#' Conditionally filter rows from a (spatial) data frame
#'
#' Designed to be used within Shiny applications. Allows a user to specify
#' conditions (using R or dplyr syntax) to filter rows from a data frame or
#' spatial (\href{https://r-spatial.github.io/sf/index.html}{sf}) data frame. This function uses dplyr's \link[dplyr]{filter} to
#' conditionally subset rows.
#'
#' @param filter_df data frame or spatial (\href{https://r-spatial.github.io/sf/index.html}{sf}) data frame to apply condition
#'   and filter rows from.
#' @param filter_conditions a single element character vector specifying
#'   conditions used to subset rows from \code{filter_df}. Conditions
#'   should be specified in R or dplyr syntax (e.g. \code{column_1 == "xyz"}
#'   will filter all rows from \code{filter_df} where \code{column_1} has a
#'   value of \code{"xyz"}).
#'
#' @section Example filter conditions:
#'
#'   The following are example operations that can used to test conditions to
#'   filter rows from a data frame:
#'
#'   * \code{==}, \code{>}, \code{>=},
#'
#'   * \code{&}, \code{|}, \code{!},
#'
#'   * \code{is.na()}
#'
#'   See \link[dplyr]{filter} for more information and examples on filtering
#'   rows from a data frame.
#'
#' @return a data frame or spatial (\href{https://r-spatial.github.io/sf/index.html}{sf}) data frame containing the records
#'   filtered from \code{filter_df}.
#'
#' @export

filter_rows <- function(filter_df, filter_conditions) {
  filter_expr <- tryCatch(
    error = function(cnd) {
      "filter error"
    },
    {
      filter_expr <-
        call2(
          dplyr::filter,
          rlang::parse_expr("filter_df"),
          rlang::parse_expr(filter_conditions)
        )
    }
  )

  filter_out <- tryCatch(
    error = function(cnd) {
      "filter error"
    },
    {
      filter_out <- eval(filter_expr)
    }
  )

  filter_out
}

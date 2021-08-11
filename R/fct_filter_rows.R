#' Filter rows from a (spatial) data frame based on a condition
#'
#' Designed to be used within Shiny applications. Allows a user to specify
#' conditions (using R or dplyr syntax) to filter rows from a data frame or
#' spatial (sf) data frame. This function uses dplyr's \code{filter()}.
#'
#' @param filter_df data frame or spatial (sf) data frame to apply condition to
#'   and filter rows from.
#' @param filter_conditions conditions used to select rows to filter from
#'   \code{filter_df}. Conditions should be specified in R or dplyr syntax (e.g.
#'   \code{column_1 == "xyz"} will filter all rows from \code{filter_df} where
#'   \code{column_1} has a value of \code{"xyz"}).
#'
#' @return a data frame or spatial (sf) data frame containing the records
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

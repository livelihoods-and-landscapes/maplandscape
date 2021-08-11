#' Add column to a (spatial) data frame
#'
#' Add a column to a (spatial -
#' \href{https://r-spatial.github.io/sf/index.html}{sf}) data frame using dplyr's
#' \link[dplyr]{mutate} function. The values of the new column will be computed
#' by applying operations to values in existing columns. For example,
#' \code{new_column = old_column > 30} will create a new column
#' \code{new_column} of type LOGICAL with values of \code{TRUE} where
#' \code{old_column} is greater than 30.
#'
#' @param mutate_df a (spatial -
#'   \href{https://r-spatial.github.io/sf/index.html}{sf}) data frame to add a
#'   column to using dplyr's \link[dplyr]{mutate} function.
#' @param mutate_conditions a single element character vector of operations used
#'   to compute values for the new column based on values of existing columns in
#'   \code{mutate_df}.
#' @param col_name name of new column to add to \code{mutate_df}.
#'
#' @return appends a new column to \code{mutate_df}.
#'
#' @export
#'

add_column <- function(mutate_df, mutate_conditions, col_name) {

  mutate_expr <- tryCatch(
    error = function(cnd) {
      "mutate error"
    },
    {
      mutate_expr <-
        call2(
          dplyr::mutate,
          rlang::parse_expr("mutate_df"),
          !!col_name := parse_expr(mutate_conditions)
        )
    }
  )

  mutate_out <- tryCatch(
    error = function(cnd) {
      "mutate error"
    },
    {
      mutate_out <- eval(mutate_expr)
    }
  )

  mutate_out
}

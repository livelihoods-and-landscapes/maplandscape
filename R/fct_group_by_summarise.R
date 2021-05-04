#' Group by and summarise
#'
#' Performs group by and summarise operations on user selected grouping
#' variables and variables to compute summary statistics within a group.
#'
#' @param in_df A table / layer (spatial or non-spatial) to summarise.
#' @param group_var A character vector of variable names to group by - uses
#'   \code{\link[dplyr]{group_by}}.
#' @param summ_var A character vector of variable names to summarise - uses
#'   \code{\link[dplyr]{summarise}}. For all numeric variables the mean and sum
#'   are computed. For numeric and non-numeric variables the count of
#'   observations within each group is returned.
#'
#' @return A summary table as a data frame (\code{s_df}) object.
#'
#' @import rlang
#' @import dplyr
#' @importFrom magrittr %>%

group_by_summarise <- function(in_df, group_var, summ_var) {
  funs_list_numeric <- list(
    mean = ~ mean(.x, na.rm = TRUE),
    sum = ~ sum(.x, na.rm = TRUE)
  )

  if (is.null(summ_var)) {
    s_df <- tryCatch(
      error = function(cnd) NULL,
      in_df %>%
        as.data.frame() %>%
        select(!!!group_var) %>%
        group_by(across(any_of(group_var))) %>%
        tally()
    )
  } else {
    s_df <- tryCatch(
      error = function(cnd) NULL,
      in_df %>%
        as.data.frame() %>%
        select(!!!group_var, !!!summ_var) %>%
        group_by(across(any_of(group_var))) %>%
        summarise(across(where(is.numeric), funs_list_numeric), n = n(), .groups = "keep")
    )
  }
  s_df
}

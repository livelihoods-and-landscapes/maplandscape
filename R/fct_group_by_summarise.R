#' Group by and summarise
#'
#' Perform group by and summarise operations on selected grouping columns and
#' compute summary statistics within each group. This function uses dplyr's
#' \link[dplyr]{group_by} function for creating groups within the data frame and
#' \link[dplyr]{summarise} for computing summary statistics.
#'
#' @param in_df a layer (spatial
#'   (\href{https://r-spatial.github.io/sf/index.html}{sf}) or non-spatial data
#'   frame) to summarise.
#' @param group_var a character vector of column names to group by - uses
#'   dplyr's \link[dplyr]{group_by} function.
#' @param summ_var A character vector of columns names to compute grouped
#'   summary statistics for - uses dplyr's \link[dplyr]{summarise}. For all
#'   numeric columns the mean and sum are computed. For numeric and non-numeric
#'   columns the count of observations within each group is returned.
#'
#' @return a summary table as a data frame (\code{s_df}) object.
#'
#' @import rlang
#' @import dplyr
#' @import tidyselect
#' @importFrom magrittr %>%
#'
#' @export

group_by_summarise <- function(in_df, group_var, summ_var) {

  funs_list_numeric <- list(
    mean = ~ mean(.x, na.rm = TRUE),
    sum = ~ sum(.x, na.rm = TRUE)
  )

  if (is.null(summ_var)) {
     s_df <- in_df %>%
        as.data.frame() %>%
        select(!!!group_var) %>%
        group_by(across(any_of(group_var))) %>%
        tally()
  } else {
      s_df <- in_df %>%
        as.data.frame() %>%
        select(!!!group_var, !!!summ_var) %>%
        group_by(across(any_of(group_var))) %>%
        summarise(across(where(is.numeric), funs_list_numeric), n = n(), .groups = "keep")
  }
  s_df
}

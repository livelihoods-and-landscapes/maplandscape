#' Join two layers
#'
#' Perform an inner join or left join operation on two layers based on \emph{n}
#' number of primary and foreign keys. Perform joins using
#'  dplyr's \link[dplyr]{inner_join} and \link[dplyr]{left_join} functions.
#'
#' Primary and foreign keys are converted to character vectors before joining.
#'
#' @param f_left layer with primary keys and left table in joins.
#' @param f_right layer with foreign keys and right table in joins. For
#'   non-spatial joins this must be a non-spatial data frame - if it is a spatial
#'   data frame (i.e. of class \href{https://r-spatial.github.io/sf/index.html}{sf}) it will be converted to non-spatial.
#' @param pk a character vector of primary keys. The primary and foreign keys do
#'   not need to have the same names but the order of the keys matters.
#' @param fk a character vector of foreign keys. The primary and foreign keys do
#'   not need to have the same names but the order of the keys matters.
#' @param j_type a string specifying the type of join to perform. \code{"col_inner"} specifies an inner join and \code{"col_left"} specifies a left join.
#'
#' @return \code{j_df} A layer joined on primary and foreign keys. Will be of
#'   class \href{https://r-spatial.github.io/sf/index.html}{sf}(i.e. spatial features) if f_left is spatial and of class
#'   \href{https://r-spatial.github.io/sf/index.html}{sf}. Otherwise returns a data frame.
#'
#' @import rlang
#' @export

join_tables <- function(f_left, f_right, j_type, pk, fk) {
  if (j_type == "col_inner") {
    if (length(fk) == length(pk)) {
      by <- c()
      for (i in 1:length(pk)) {
        tmp_name <- rlang::set_names(as_name(fk[[i]]), as_name(pk[[i]]))
        by <- c(by, tmp_name)
      }
    }

    # force joining table to be a dataframe
    # when using a join operation on an sf object the other table must be non-spatial
    f_right <- f_right %>%
      as.data.frame()

    # make all pk and fk character type for joining
    for (p in pk) {
      pkk <- p
      f_left[[pkk]] <- as.character(f_left[[pkk]])
    }

    for (f in fk) {
      fkk <- f
      f_right[[fkk]] <- as.character(f_right[[fkk]])
    }

    j_df <- tryCatch(
      error = function(cnd) NULL,
      dplyr::inner_join(f_left, f_right, by = by, na_matches = "never")
    )

    j_df
  } else if (j_type == "col_left") {
    if (length(fk) == length(pk)) {
      by <- c()
      for (i in 1:length(pk)) {
        tmp_name <- rlang::set_names(as_name(fk[[i]]), as_name(pk[[i]]))
        by <- c(by, tmp_name)
      }
    }

    # force joining table to be a dataframe
    # when using a join operation on an sf object the other table must be non-spatial
    f_right <- f_right %>%
      as.data.frame()

    # make all pk and fk character type for joining
    for (p in pk) {
      pkk <- p
      f_left[[pkk]] <- as.character(f_left[[pkk]])
    }

    for (f in fk) {
      fkk <- f
      f_right[[fkk]] <- as.character(f_right[[fkk]])
    }

    j_df <- tryCatch(
      error = function(cnd) NULL,
      dplyr::left_join(f_left, f_right, by = by, na_matches = "never")
    )

    j_df
  }
}

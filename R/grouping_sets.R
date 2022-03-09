#' Grouping sets
#'
#' Functions for working with Grouping Sets, specifically as specified
#' in the [Presto documentation](https://prestodb.io/docs/current/sql/select.html#group-by-clause).
#'
#' @name groupingsets
NULL


#' @rdname groupingsets
#' @param x a logical vector to convert into a grouping set number
#'
#'
#' @return a single number indicating the grouping set corresponding to the
#'   bool
#' @export
#'
#' @details The grouping set number will correspond to the output of
#'  `grouping` in Presto.
#'
#' @examples
#' gs_bool_to_group_number(c(TRUE, FALSE, FALSE))
gs_bool_to_group_number <- function(x) {
  coefs <- as.numeric(!x)
  powers <- (length(x) - 1):0
  sum(coefs * 2^powers)
}

#' @rdname groupingsets
#' @param all_cols all of the grouping columns
#' @param included_cols the group with the desired columns
#'
#'
#' @return the grouping number
#' @export
#'
#' @examples
#' grouping_number(c("origin_state", "origin_zip", "destination_state"),
#' "origin_state")
grouping_number <- function(all_cols, included_cols) {
  stopifnot("included_cols not in all_cols" = all(included_cols %in% all_cols))
  gs_bool_to_group_number(all_cols %in% included_cols)
}

#' @rdname groupingsets
#'
#'
#' @return a function that will parse grouping columns
#' @export
#'
grouping_parser <- function(all_cols) {
  function(x) grouping_number(all_cols, x)
}

#' @rdname groupingsets
#'
#' @param .x a data.frame-like object
#' @param all_grouping_cols the columns included in the grouping sets
#' @param group the columns included in the desired group
#' @param .group_number_col the column containing the grouping number
#'
#'
#' @return the parsed data frame
#' @export
#'
filter_grouping_set <- function(.x, all_grouping_cols, group,
                                .group_number_col = grouping_number) {
  all_cols <- names(tidyselect::eval_select(rlang::enquo(all_grouping_cols), .x))
  included_cols <- names(tidyselect::eval_select(rlang::enquo(group), .x))
  dplyr::filter(.x, {{ .group_number_col }} == grouping_number(all_cols, included_cols))
}

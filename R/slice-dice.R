#' Slice a dataframe to include only the top categories
#'
#' @param .data a dataframe
#' @param .n the number of categories to include
#' @param ... columns which define categories
#' @param .wt an optional weight category
#'
#' @description This slices a dataframe to only include the most frequent
#'
#' @return A dataframe containing only the top categories
#' @export
#'
#' @examples
#' # give the top 1 most occuring value of `cyl`
#' slice_top_categories(mtcars, 1, cyl)
#'
#' # give the top 1 most occuring value of `cyl` weighted by am
#' slice_top_categories(mtcars, 1, cyl, .wt = am)
#'
#' # give the 2 most occuring values of (cyl, gear) pair
#' slice_top_categories(mtcars, 2, cyl, gear)
slice_top_categories <- function(.data, .n, ..., .wt = NULL) {
  tempname <- "slice_top_categories_count"
  by_cols <- as.character(substitute(...()))
  counts <- .data %>%
    dplyr::count(..., wt = {{ .wt }}, name = tempname, sort = TRUE) %>%
    dplyr::slice_head(n = .n) %>%
    dplyr::select(...)
  dplyr::inner_join(.data, counts, by = by_cols)
}

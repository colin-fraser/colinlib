#' Escape regex
#'
#' @param x character
#'
#' @return escaped version of x
#' @export
escape <- function(x) {
  pattern <- r"--((?=[-\[\]{}()\*\+\?\.\,\\\^\$\|\#]))--"
  gsub(pattern, r"(\\)", x, perl = TRUE)
}

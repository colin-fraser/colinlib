# Code generation tools

#' Indent a character vector
#'
#' @param x a character vector
#' @param indent_level the level to indent
#' @param indent_character the character to use for indenting
#'
#' @return an indented character vector
#' @export
#'
cg_indent <- function(x, indent_level = 0, indent_character = ' ') {
  indent <- paste(rep(indent_character, indent_level), collapse = '')
  gsub("(?m)^", indent, x, perl = TRUE)
}


#' Surround a string
#'
#' @param x the string to surround
#' @param surround enclosing characters. See details.
#' @param style the style of surrounding. See details.
#' @param inner_indent the indent of the inner part of the sandwich.
#'   Only used when `style = 'sandwich'`.
#'
#' @details
#' The argument to `surround` should be 1 or 2 characters. If 1, then the
#'   returned string will be enclosed in that character. If 2, then
#'   the returned string will be enclosed by the first on the left and the second
#'   on the right.
#' See examples for the style argument.
#'
#' @return a surrounded string
#' @export
#'
#' @examples
#' x <- month.abb[1:3]
#' cg_surround(x, '"')
#' cg_surround(x, style = 'sandwich')
#' cg_surround(x, '{}', 'sandwich', 2)
cg_surround <- function(x, surround = "()", style = c('single', 'sandwich'),
                        inner_indent = 0) {
  style <- match.arg(style)
  enclosure <- switch(nchar(surround),
                      c(surround, surround),
                      strsplit(surround, '')[[1]],
                      stop("`surround` should be 1 or 2 characters")
                      )
  switch(
    style,
    single = cg_surround_single(x, enclosure),
    sandwich = cg_surround_sandwich(x, enclosure, inner_indent)
  )
}

cg_surround_single <- function(x, enclosure) {
  stringi::stri_c(enclosure[1], x, enclosure[2])
}

cg_surround_sandwich <- function(x, enclosure, inner_indent) {
  stringi::stri_c(enclosure[1], cg_indent(x, inner_indent), enclosure[2], sep = '\n')
}


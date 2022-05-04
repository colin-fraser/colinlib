obs_uri <- function(action = c('new', 'open', 'search', 'hook-get-address'),
                    ...) {
  action <- match.arg(action)
  params <- rlang::list2(...)
  param_names <- names(params)
  param_string <- paste(param_names, params, sep = '=', collapse = '&')
  out <- paste0("obsidian://", action)
  if (length(params) > 0) {
    out <- paste0(out, '?', param_string)
  }
  out
}

parse_citation <- function(x, style = 'Vancouver') {
  authors <- parse_authors(x)
  authors
}

parse_authors <- function(x) {
  parse_up_to_first_period(x) %>%
    stringr::str_split(", ") %>%
    unlist()
}

parse_up_to_first_period <- function(x) {
  stringr::str_extract(x, "^[^\\.]+(?=\\.)")
}

parse_title <- function(x) {
  authors <- parse_up_to_first_period(x)
  stringr::str_remove(x, authors) %>%
    stringr::str_remove(". ") %>%
    parse_up_to_first_period()
}

parse_year <- function(x) {
  title <- parse_title(x)
  x <- stringr::str_remove(x, title)
  stringr::str_extract(x, "\\d{4}")
}

short_cite <- function(x) {
  authors <- parse_authors(x)
  year <- parse_year(x)
  glue::glue("{authors[1]} ({year})")
}

sanitize_title <- function(x) {
  stringr::str_replace_all(x, "[:]", "-")
}

new_paper <- function(x, extra_tags = NULL, directory = "notes") {
  template <- "
  ---
  aliases: [{title}, {short_cite(x)}]
  tags: [{tags}]
  ---
  # {title}

  **Citation**: {x}
  **Status**: #to-read

  ## Abstract

  ## Summary

  ## Detailed Notes

  "
  title <- parse_title(x)
  authors <- parse_authors(x)
  year <- parse_year(x)

  aliases <- short_cite
  tags <- c("paper", extra_tags)
  obs_uri('new', name = fs::path_join(c(directory, sanitize_title(title))), content=urltools::url_encode(glue::glue(template)))
}

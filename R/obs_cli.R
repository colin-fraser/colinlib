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
  author <- stringr::str_extract(parse_authors(x)[1], "[\\w-']+")
  year <- parse_year(x)
  glue::glue("{author} ({year})")
}

sanitize_title <- function(x) {
  stringr::str_replace_all(x, "[:]", "-")
}

to_link <- function(x) {
  paste0("[[", x, "]]")
}

to_md_list <- function(x) {
  paste0("- ", x, collapse = "\n")
}

to_tag <- function(x) {
  paste0("#", x)
}

new_paper_content <- function(x, extra_tags) {
  template <- "
  ---
  aliases: [{title}, {short_cite(x)}]
  ---
  # {title}
  **Citation**: {x}
  **Status**: #to-read
  **Authors**:
  {authors}
  **Tags**:
  {tags}

  ## Abstract

  ## Summary

  ## Detailed Notes

  "
  title <- parse_title(x)
  authors <- parse_authors(x) %>%
    to_link() %>%
    to_md_list()
  year <- parse_year(x)
  aliases <- short_cite(x)
  tags <- paste(c("paper", extra_tags)) %>%
    to_tag() %>%
    to_md_list()
  glue::glue(template)
}

new_paper_uri <- function(x, extra_tags = NULL, directory = "notes") {
  title <- parse_title(x)
  content <- new_paper_content(x, extra_tags)
  obs_uri('new', name = fs::path_join(c(directory, sanitize_title(title))),
          content=urltools::url_encode(content))
}


#' Create a paper entry
#'
#' @param x the citation in Vancouver style
#' @param extra_tags extra tags
#' @param directory directory to create the notes
#'
#' @return The uri
#' @export
#'
new_paper <- function(x, extra_tags = NULL, directory = "notes", browse = TRUE) {
  uri <- new_paper_uri(x, extra_tags, directory)
  if (browse) browseURL(uri)
  uri
}

ls_templates <- function(template_dir) {
  fs::dir_ls(template_dir)
}


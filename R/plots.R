#' ggplot themes
#'
#' @description
#' Just ggplot2 themes that I like and think look good.
#'
#' @param grid_type which grid lines should be displayed?
#'
#' @return a ggplot theme
#' @export
#'
#' @import ggplot2
theme1 <- function(grid_type = c("both", "horizontal", "vertical", "none")) {
  grid_type <- match.arg(grid_type)
  grid_color <- "grey92"

  base_theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  grid <- ggplot2::element_line(grid_color)
  grid_option <- switch(grid_type,
    "both" = ggplot2::theme(panel.grid.major = grid),
    "horizontal" = ggplot2::theme(panel.grid.major.y = grid),
    "vertical" = ggplot2::theme(panel.grid.major.x = grid),
    "none" = ggplot2::theme()
  )

  title_font <- ggplot2::element_text(face = "bold")

  base_theme + grid_option + ggplot2::theme(plot.title = title_font)
}




# Annotation functions ----------------------------------------------------

compute_group_position_annotation <- function(pos) {
  function(data, scales, params, labeler, ...) {
    if (pos < 0) {
      pos <- nrow(data) + pos + 1
    }
    out <- data[pos, , drop = FALSE]
    if (!("annotation_label" %in% names(out))) {
      warning("aes annotation_label not specified. Defaulting to match `y` aesthetic.")
      out$annotation_label <- out$y
    }
    out$label <- labeler(out$annotation_label)
    out
  }
}

compute_summary_annotation <- function(summary_type = c('max', 'min')) {
  summary_type <- match.arg(summary_type)
  fn <- get(paste0('which.', summary_type))
  function(data, scales, params, labeler, ...) {
    i <- fn(data$y)
    out <- data[i, , drop = FALSE]
    if (!("annotation_label" %in% names(out))) {
      warning("aes annotation_label not specified. Defaulting to match `y` aesthetic.")
      out$annotation_label <- out$y
    }
    out$label <- labeler(out$annotation_label)
    out
  }
}


StatAnnotate <- ggproto(
  "StatAnnotate",
  Stat,
  required_aes = c("x", "y"),
  default_aes = aes(annotation_label = stat(y))
)

StatFirst <- ggproto("StatFirst",
                     StatAnnotate,
                     compute_group = compute_group_position_annotation(1))
StatLast <- ggproto("StatLast",
                     StatAnnotate,
                     compute_group = compute_group_position_annotation(-1))

StatMin <- ggproto("StatMin",
                   StatAnnotate,
                   compute_group = compute_summary_annotation('min'))

StatMax <- ggproto("StatMax",
                   StatAnnotate,
                   compute_group = compute_summary_annotation('max'))

#' Guess which labeler to use for stat_annotate_
#'
#' @description Given a vector, guess how to label it. This is used in e.g.
#' stat_annotate_min
#'
#' @param x a vector to be labeled
#'
#' @return
#' @noMd
guess_labeler <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.numeric(x)) {
    return(scales::label_number(big.mark = ',')(x))
  }
  stop(glue::glue("Don't know how to label a vector of type {class(x)}."))
}

stat_annotate_ <- function(stat) {
  function(mapping = NULL,
           data = NULL,
           geom = "text",
           position = "identity",
           na.rm = FALSE,
           inherit.aes = TRUE,
           vjust = "outward",
           hjust = "outward",
           size = 2.8,
           check_overlap = TRUE,
           labeler = guess_labeler,
           ...) {
    ggplot2::layer(
      stat = stat,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        vjust = vjust,
        hjust = hjust,
        size = size,
        check_overlap = check_overlap,
        labeler = labeler,
        ...
      )
    )
  }
}

#' @export
stat_annotate_first <- stat_annotate_(StatFirst)

#' @export
stat_annotate_last <- stat_annotate_(StatLast)

#' @export
stat_annotate_min <- stat_annotate_(StatMin)

#' @export
stat_annotate_max <- stat_annotate_(StatMax)


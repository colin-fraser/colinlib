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
theme1 <- function(grid_type = c('both', 'horizontal', 'vertical', 'none')) {
  grid_type <- match.arg(grid_type)
  grid_color <- "grey92"

  base_theme <- ggplot2::theme_bw() +
    theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank())

  grid <- ggplot2::element_line(grid_color)
  grid_option <- switch(grid_type,
                        'both' = ggplot2::theme(panel.grid.major = grid),
                        'horizontal' = ggplot2::theme(panel.grid.major.y = grid),
                        'vertical' = ggplot2::theme(panel.grid.major.x = grid),
                        'none' = ggplot2::theme())

  title_font <- ggplot2::element_text(face = 'bold')

  base_theme + grid_option + theme(plot.title = title_font)
}



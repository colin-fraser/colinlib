test_that("stat_annotate_", {
  vdiffr::expect_doppelganger(
    'test_annotations',
    ggplot2::ggplot(ggplot2::economics,
                    ggplot2::aes(x = date, y = unemploy,
                                 annotation_label = unemploy)) +
      ggplot2::geom_line() +
      stat_annotate_first() +
      stat_annotate_last() +
      stat_annotate_max() +
      stat_annotate_min()
  )
})

test_that("guessing labeler works", {
  df <- palmerpenguins::penguins %>%
    dplyr::count(year, species, island, sex)

  vdiffr::expect_doppelganger(
    "test_guess_factor_label",
    ggplot(df, aes(x = year, y = n, color = species, linetype = sex)) +
    facet_wrap(vars(island)) +
    geom_line() +
    stat_annotate_first(aes(annotation_label = species))
  )

  vdiffr::expect_doppelganger(
    "test_guess_char_label",
    ggplot(dplyr::mutate(df, species = as.character(species)),
           aes(x = year, y = n, color = species, linetype = sex)) +
      facet_wrap(vars(island)) +
      geom_line() +
      stat_annotate_first(aes(annotation_label = species))
  )
})

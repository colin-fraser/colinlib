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

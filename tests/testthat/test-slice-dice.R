test_that("slice_top_n", {
  actual <- ggplot2::diamonds %>%
    slice_top_categories(cut, .n=2)
  expected <- ggplot2::diamonds %>%
    filter(cut %in% c('Ideal', 'Premium'))
  expect_equal(actual, expected)
})

test_that("slice_top_n with weight", {
  actual <- ggplot2::diamonds %>%
    slice_top_categories(cut, .n=2, .wt = -carat)
  expected <- ggplot2::diamonds %>%
    filter(cut %in% c('Fair', 'Good'))
  expect_equal(actual, expected)
})

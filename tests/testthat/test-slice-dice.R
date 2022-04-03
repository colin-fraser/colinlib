df <- dplyr::tribble(
  ~a, ~b, ~c,
  "A", "A", 1,
  "A", "A", 1,
  "A", "A", 1,
  "A", "B", 1,
  "A", "B", 1,
  "A", "C", 1,
  "B", "A", 1,
  "B", "B", 1,
  "C", "A", 10,
  "D", "A", 10
)
test_that("slice_top_n", {
  actual <- df %>%
    slice_top_categories(a, .n=2)
  expected <- df %>%
    dplyr::filter(a %in% c('A', 'B'))
  expect_equal(actual, expected)
})

test_that("slice_top_n with weight", {
  actual <- df %>%
    slice_top_categories(a, .n = 2, .wt = c)
  expected <- df %>%
    dplyr::filter(a %in% c('C', 'D'))
  expect_equal(actual, expected)
})

test_that("slice_top_n multiple cols", {
  actual <- df %>%
    slice_top_categories(a, b, .n = 2)
  expected <- df %>%
    dplyr::filter(a == "A", b %in% c('A', 'B'))
  expect_equal(actual, expected)
}
)

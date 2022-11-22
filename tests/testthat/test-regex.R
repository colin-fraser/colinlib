test_that("regex_escape", {
  expect_equal(escape("hello\" there!"), "hello\" there!")
})

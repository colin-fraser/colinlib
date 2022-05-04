test_that("author parse works", {
  c1 <- "Bollinger CR, Chandra A. Iatrogenic specification error: A cautionary tale of cleaning data. Journal of Labor Economics. 2005 Apr;23(2):235-57."
  c2 <- "Al-Habian A, Harikumar PE, Stocker CJ, Langlands K, Selway JL. Histochemical and immunohistochemical evaluation of mouse skin histology: comparison of fixation with neutral buffered formalin and alcoholic formalin. J Histotechnol. 2014 Dec;37(4):115-24."
  expect_equal(parse_authors(c1), c("Bollinger CR", "Chandra A"))
  expect_equal(parse_authors(c2), c("Al-Habian A", "Harikumar PE",
                                    "Stocker CJ", "Langlands K",
                                    "Selway JL"))

})

test_that("title parse works", {
  c1 <- "Bollinger CR, Chandra A. Iatrogenic specification error: A cautionary tale of cleaning data. Journal of Labor Economics. 2005 Apr;23(2):235-57."
  c2 <- "Al-Habian A, Harikumar PE, Stocker CJ, Langlands K, Selway JL. Histochemical and immunohistochemical evaluation of mouse skin histology: comparison of fixation with neutral buffered formalin and alcoholic formalin. J Histotechnol. 2014 Dec;37(4):115-24."
  expect_equal(parse_title(c1), "Iatrogenic specification error: A cautionary tale of cleaning data")
  expect_equal(parse_title(c2), "Histochemical and immunohistochemical evaluation of mouse skin histology: comparison of fixation with neutral buffered formalin and alcoholic formalin")
})

test_that("year parse works", {
  c1 <- "Bollinger CR, Chandra A. Iatrogenic specification error: A cautionary tale of cleaning data. Journal of Labor Economics. 2005 Apr;23(2):235-57."
  c2 <- "Al-Habian A, Harikumar PE, Stocker CJ, Langlands K, Selway JL. Histochemical and immunohistochemical evaluation of mouse skin histology: comparison of fixation with neutral buffered formalin and alcoholic formalin. J Histotechnol. 2014 Dec;37(4):115-24."
  expect_equal(parse_year(c1), "2005")
  expect_equal(parse_year(c2), "2014")
})

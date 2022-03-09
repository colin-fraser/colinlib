# test examples borrowed from the documentation at
# https://prestodb.io/docs/current/sql/select.html#group-by-clause

test_that("gs_bool_to_group_number works", {
  expect_equal(gs_bool_to_group_number(c(TRUE, FALSE, FALSE)), 3)
  expect_equal(gs_bool_to_group_number(c(TRUE, TRUE, FALSE)), 1)
  expect_equal(gs_bool_to_group_number(c(FALSE, FALSE, TRUE)), 6)
})

test_that("grouping_number", {
  expect_equal(grouping_number(
    c("origin_state", "origin_zip", "destination_state"),
    "origin_state"
  ), 3)
  expect_equal(grouping_number(
    c("origin_state", "origin_zip", "destination_state"),
    c("origin_state", "origin_zip")
  ), 1)
  expect_equal(grouping_number(
    c("origin_state", "origin_zip", "destination_state"),
    "destination_state"
  ), 6)
})

test_that("grouping_parser", {
  gparse <-
    grouping_parser(c("origin_state", "origin_zip", "destination_state"))
  expect_equal(gparse("origin_state"), 3)
  expect_equal(gparse(c("origin_state", "origin_zip")), 1)
  expect_equal(gparse(c("destination_state")), 6)
})

test_that("filter_grouping_set", {
  # example data from
  # https://prestodb.io/docs/current/sql/select.html#group-by-clause

  df <- tibble::tibble(
    origin_state = c(
      "California",
      "New Jersey",
      "New York",
      "California",
      "New Jersey",
      "California",
      "New York",
      "NULL",
      "NULL",
      "NULL"
    ),
    origin_zip = c(
      "NULL",
      "NULL",
      "NULL",
      "94131",
      "7081",
      "90210",
      "10002",
      "NULL",
      "NULL",
      "NULL"
    ),
    destination_state = c(
      "NULL",
      "NULL",
      "NULL",
      "NULL",
      "NULL",
      "NULL",
      "NULL",
      "New Jersey",
      "Connecticut",
      "Colorado"
    ),
    `_col3` = c(
      "1397", "225", "3", "60",
      "225", "1337", "3", "58", "1562", "5"
    ),
    `_col4` = c(
      "3", "3",
      "3", "1", "1", "1", "1", "6", "6", "6"
    )
  )
  expect_equal(
    filter_grouping_set(df,
      c(origin_state, origin_zip, destination_state),
      origin_state,
      .group_number_col = `_col4`
    ),
    dplyr::filter(df, `_col4` == 3)
  )
  expect_equal(
    filter_grouping_set(df,
      c(origin_state, origin_zip, destination_state),
      c(origin_state, origin_zip),
      .group_number_col = `_col4`
    ),
    dplyr::filter(df, `_col4` == 1)
  )
  expect_equal(
    filter_grouping_set(df,
      c(origin_state, origin_zip, destination_state),
      destination_state,
      .group_number_col = `_col4`
    ),
    dplyr::filter(df, `_col4` == 6)
  )
})

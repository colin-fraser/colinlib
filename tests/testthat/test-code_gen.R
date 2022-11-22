test_that("cg_indent", {
  expect_equal(cg_indent('A'), 'A')
  expect_equal(cg_indent('A', 2), '  A')
  expect_equal(cg_indent('A\nA', 2), '  A\n  A')
})

test_that("cg_surround", {
  expect_equal(cg_surround('hello'), '(hello)')
  expect_equal(cg_surround('hello', '"'), '"hello"')
  expect_error(cg_surround('hello', '---'))
  expect_equal(cg_surround('hello', '[]'), '[hello]')
  expect_equal(cg_surround('hello', style = 'sandwich'), '(\nhello\n)')
  expect_equal(cg_surround('hello\nthere', style = 'sandwich', inner_indent = 2),
               '(\n  hello\n  there\n)')
})

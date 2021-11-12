test_that("str_length of factor is length of level", {
  expect_equal(stringr::str_length(factor("a")), 1)
})

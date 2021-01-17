context("test-duplicate_min")
library(partier)

test_that("duplicate_min for no duplicates", {

  vec1 <- c(1,2,3)
  expect_equal(duplicate_min(vec1), 0)

})

test_that("duplicate_min for duplicates", {

  vec2 <- c(1,2,1)
  expect_equal(duplicate_min(vec2), 1)

})



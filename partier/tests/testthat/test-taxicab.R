context("test-taxicab")
library(partier)

test_that("taxicab works for equal-lengthed vectors", {

  vec1 <- c(1,2,3)
  vec2 <- c(4,5,6)
  output <- 9
  expect_equal(taxicab(vec1,vec2), output)

})

test_that("taxicab throws error for different lengthed vectors", {

  expect_error(taxicab(c(3,4),c(1,2,3)),'x and y must have same length')

})



context("test-coordinate_distances")
library(partier)

test_that("correct dimensions to PartyMatrix object", {

  policies <- matrix(c(1,2,3,4,5,6), nrow=2)
  partymatrix <- coordinate_distances(policies)
  expect_equal(dim(partymatrix), c(100,6))

})


test_that("partymatrix ordered tuples populating correctly", {

  policies <- matrix(c(1,2,3,4,5,6), nrow=2)
  partymatrix <- coordinate_distances(policies)
  expect_equal(mean(partymatrix[,1]), 5.5)

})


test_that("distances being calculated correctly", {

  policies <- matrix(c(1,2,3,4,5,6), nrow=2)
  partymatrix <- coordinate_distances(policies)
  expect_equal(partymatrix[2,3], 0)

})


test_that("minimum being found", {

  policies <- matrix(c(1,2,3,4,5,6), nrow=2)
  partymatrix <- coordinate_distances(policies)
  expect_equal(partymatrix[2,6], 1)

})



test_that("ties coming out correctly", {

  policies <- matrix(c(1,2,3,4,5,6), nrow=2)
  partymatrix <- coordinate_distances(policies)
  expect_equal(partymatrix[22,6], 0)

})

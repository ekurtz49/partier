context("test-partypolicy_numbers")
library(partier)

test_that("partypolicy_numbers gives correct value for party", {

  policies <- matrix(c(1,2,3,4,5,6), nrow=2)
  partymatrix <- coordinate_distances(policies)
  expect_equal(partypolicy_numbers(partymatrix)$party_number, 3)

})


test_that("partypolicy_numbers gives correct value for policy", {

  policies <- matrix(c(1,2,3,4,5,6), nrow=2)
  partymatrix <- coordinate_distances(policies)
  expect_equal(partypolicy_numbers(partymatrix)$policy_number, 2)

})

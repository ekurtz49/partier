context("test-new_policies")
library(partier)

test_that("new_policies makes correct policies - undoes coordinate_distances", {

  policies <- matrix(c(1,2,3,4,5,6), nrow=2)
  partymatrix <- coordinate_distances(policies)
  expect_equal(new_policies(partymatrix), policies)

})



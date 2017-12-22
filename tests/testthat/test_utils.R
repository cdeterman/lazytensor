library(lazytensor)
context("Utilities")

# set seed
set.seed(123)

ORDER <- 4

# Base R objects
A <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)


test_that("Tensor Dimensions", {

  a <- Tensor$new(A)

  expect_equivalent(dim(a), dim(A),
                    info = "Matrix dimensions don't match")
  expect_equivalent(nrow(a), nrow(A),
                    info = "Matrix row counts don't match")
  expect_equivalent(ncol(a), ncol(A),
                    info = "Matrix column counts don't match")

})

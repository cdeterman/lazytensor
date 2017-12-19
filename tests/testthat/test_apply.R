library(lazytensor)
context("Apply Operations")

# basic matrix - 2D
mat <- matrix(1:12, nrow = 4)

## array - 3D
A <- array(1:24, dim = 4:2)


test_that("Sweep Matrix", {

  # columns
  baseC <- sweep(mat, 1, seq(4), FUN = '-')
  # rows
  baseR <- sweep(mat, 2, seq(3), FUN = '-')

  # Tensor computations
  A_tensor <- Tensor$new(mat)
  tensorC <- A_tensor$sweep(1, seq(4), FUN = '-')$compute()
  A_tensor$drop()
  tensorR <- A_tensor$sweep(2, seq(3), FUN = '-')$compute()

  expect_equal(tensorC, baseC, tolerance=.Machine$double.eps ^ 0.5,
               info="columnwise sweep elements not equivalent",
               check.attributes=FALSE)
  expect_equal(tensorR, baseR, tolerance=.Machine$double.eps ^ 0.5,
               info="rowwise sweep elements not equivalent",
               check.attributes=FALSE)
})

test_that("Sweep Array (3D)", {

  # columns
  baseC <- sweep(A, 1, seq(4), FUN = '-')
  # rows
  baseR <- sweep(A, 2, seq(3), FUN = '-')
  # N dimension
  baseN <- sweep(A, 3, seq(2), FUN = '-')

  # Tensor computations
  A_tensor <- Tensor$new(A)
  tensorC <- A_tensor$sweep(1, seq(4), FUN = '-')$compute()
  A_tensor$drop()
  tensorR <- A_tensor$sweep(2, seq(3), FUN = '-')$compute()
  A_tensor$drop()
  tensorN <- A_tensor$sweep(3, seq(2), FUN = '-')$compute()

  expect_equal(tensorC, baseC, tolerance=.Machine$double.eps ^ 0.5,
               info="columnwise sweep elements not equivalent",
               check.attributes=FALSE)
  expect_equal(tensorR, baseR, tolerance=.Machine$double.eps ^ 0.5,
               info="rowwise sweep elements not equivalent",
               check.attributes=FALSE)
  expect_equal(tensorN, baseN, tolerance=.Machine$double.eps ^ 0.5,
               info="N-wise sweep elements not equivalent",
               check.attributes=FALSE)
})




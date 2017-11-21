library(lazytensor)
context("Basic Algebra - gpuR backend")

# set seed
set.seed(123)

ORDER <- 4

# Base R objects
A <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
B <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)

setBackend("gpuR")
options(gpuR.default.type = "float")

test_that("Matrix Multiplication", {

  skip_on_travis()

  A_tensor = Tensor$new(vclMatrix(A))
  B_tensor = Tensor$new(vclMatrix(B))

  C <- A %*% B
  C_tensor <- matmult(A_tensor, B_tensor)

  res <- C_tensor$compute()
  expect_is(res, "vclMatrix")
  expect_equal(res[], C, tolerance=1e-07,
               info="matrix elements not equivalent")
})

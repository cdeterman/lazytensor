library(lazytensor)
context("Basic Algebra")

# set seed
set.seed(123)

ORDER <- 4

# Base R objects
A <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
B <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
A_vec <- rnorm(ORDER)
B_vec <- rnorm(ORDER)


A_vec_tensor = Tensor$new(A_vec)
B_vec_tensor = Tensor$new(B_vec)

test_that("Matrix Multiplication", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A %*% B
  C_tensor <- matmult(A_tensor, B_tensor)

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})

test_that("Matrix Addition", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A + B
  C_tensor <- A_tensor + B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})

test_that("Matrix Subtraction", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A - B
  C_tensor <- A_tensor - B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


test_that("Matrix Elementwise Mutliplication", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A * B
  C_tensor <- A_tensor * B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


test_that("Matrix Division", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A / B
  C_tensor <- A_tensor / B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})

test_that("Matrix Elementwise Power", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A ^ B
  C_tensor <- A_tensor ^ B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})

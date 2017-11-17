library(lazytensor)
context("Logicals")

# set seed
set.seed(123)

ORDER <- 4

# Base R objects
A <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
B <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)

idx <- sample(1:16, 4)
A[idx] <- 42
B[idx] <- 42

test_that("Equality Test", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A == B
  C_tensor <- A_tensor == B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})

test_that("Inequality Test", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A != B
  C_tensor <- A_tensor != B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})

test_that("Greater-than or Equal Test", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A >= B
  C_tensor <- A_tensor >= B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


test_that("Greater-than Test", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A > B
  C_tensor <- A_tensor > B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})

test_that("Less-than or Equal Test", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A <= B
  C_tensor <- A_tensor <= B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


test_that("Less-than Test", {

  A_tensor = Tensor$new(A)
  B_tensor = Tensor$new(B)

  C <- A < B
  C_tensor <- A_tensor < B_tensor

  expect_equal(C_tensor$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


library(lazytensor)
context("Chaining")

# set seed
set.seed(123)

ORDER <- 4

# Base R objects
A <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
B <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
D <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)

# sample function
sigmoid <- function(x){
  1 / (1 + exp(-x))
}

# multi argument function
multi_arg <- function(x, y, z){
  x + y - z
}

test_that("Chain Operation - Basic", {

  a <- Tensor$new(A)
  a$chain(f = sigmoid, name = "sigmoid")

  C <- sigmoid(A)

  expect_equal(a$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


test_that("Chain Operation - Previous Inputs", {

  a <- Tensor$new(A)
  b <- Tensor$new(B)

  a <- a + b
  a$chain(sigmoid, "sigmoid")

  C <- A + B
  C <- sigmoid(C)

  expect_equal(a$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


test_that("Chain Operation - Multiple Arguments Unamed", {

  a <- Tensor$new(A)
  b <- Tensor$new(B)
  d <- Tensor$new(D)

  a$chain(multi_arg, "multi_arg", b, d)

  C <- A + B - D

  expect_equal(a$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


test_that("Chain Operation - Multiple Arguments Named", {

  a <- Tensor$new(A)
  b <- Tensor$new(B)
  d <- Tensor$new(D)

  # allows for different order
  a$chain(multi_arg, "multi_arg", z = b, y = d)

  C <- A + D - B

  expect_equal(a$compute(), C, tolerance=.Machine$double.eps ^ 0.5,
               info="matrix elements not equivalent")
})


library(lazytensor)
context("Math Operations")
options(warn=-1)

# set seed
set.seed(123)

ORDER <- 4

# Base R objects
A <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
B <- matrix(rnorm(ORDER^2), nrow=ORDER, ncol=ORDER)
A_vec <- rnorm(ORDER)
B_vec <- rnorm(ORDER)

test_that("Matrix Trignometry", {

  Sin <- sin(A)
  Asin <- suppressWarnings(asin(A))
  Hsin <- sinh(A)
  Cos <- cos(A)
  Acos <- suppressWarnings(acos(A))
  Hcos <- cosh(A)
  Tan <- tan(A)
  Atan <- atan(A)
  Htan <- tanh(A)

  A_tensor <- Tensor$new(A)

  A_tensorS <- A_tensor$sin()$compute()
  A_tensor$drop()
  A_tensorAS <- suppressWarnings(A_tensor$asin()$compute())
  A_tensor$drop()
  A_tensorHS <- A_tensor$sinh()$compute()
  A_tensor$drop()
  A_tensorC <- A_tensor$cos()$compute()
  A_tensor$drop()
  A_tensorAC <- suppressWarnings(A_tensor$acos()$compute())
  A_tensor$drop()
  A_tensorHC <- A_tensor$cosh()$compute()
  A_tensor$drop()
  A_tensorT <- A_tensor$tan()$compute()
  A_tensor$drop()
  A_tensorAT <- A_tensor$atan()$compute()
  A_tensor$drop()
  A_tensorHT <- A_tensor$tanh()$compute()
  A_tensor$drop()

  expect_equal(A_tensorS[,], Sin, tolerance=.Machine$double.eps ^ 0.5,
               info="sin matrix elements not equivalent",
               check.attributes=FALSE)
  expect_equal(A_tensorAS[,], Asin, tolerance=.Machine$double.eps ^ 0.5,
               info="arc sin matrix elements not equivalent",
               check.attributes=FALSE)
  expect_equal(A_tensorHS[,], Hsin, tolerance=.Machine$double.eps ^ 0.5,
               info="hyperbolic sin matrix elements not equivalent",
               check.attributes=FALSE)
  expect_equal(A_tensorC[,], Cos, tolerance=.Machine$double.eps ^ 0.5,
               info="cos matrix elements not equivalent",
               check.attributes=FALSE)
  expect_equal(A_tensorAC[,], Acos, tolerance=.Machine$double.eps ^ 0.5,
               info="arc cos matrix elements not equivalent",
               check.attributes=FALSE)
  expect_equal(A_tensorHC[,], Hcos, tolerance=.Machine$double.eps ^ 0.5,
               info="hyperbolic cos matrix elements not equivalent",
               check.attributes=FALSE)
  expect_equal(A_tensorT[,], Tan, tolerance=.Machine$double.eps ^ 0.5,
               info="tan matrix elements not equivalent",
               check.attributes=FALSE)
  expect_equal(A_tensorAT[,], Atan, tolerance=.Machine$double.eps ^ 0.5,
               info="arc tan matrix elements not equivalent",
               check.attributes=FALSE)
  expect_equal(A_tensorHT[,], Htan, tolerance=.Machine$double.eps ^ 0.5,
               info="hyperbolic tan matrix elements not equivalent",
               check.attributes=FALSE)
})

options(warn=0)

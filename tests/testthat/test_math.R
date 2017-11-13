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

test_that("Maximum/Minimum", {
  R_max <- max(A)
  R_min <- min(A)

  A_tensor <- Tensor$new(A)

  tensor_max <- A_tensor$max()$compute()
  A_tensor$drop()
  tensor_min <- A_tensor$min()$compute()
  A_tensor$drop()

  expect_equal(tensor_max, R_max, tolerance=.Machine$double.eps^0.5,
               info="max matrix element not equivalent")
  expect_equal(tensor_min, R_min, tolerance=.Machine$double.eps^0.5,
               info="min matrix element not equivalent")

})

test_that("Rounding", {

  R_round <- round(A)
  R_floor <- floor(A)
  R_ceiling <- ceiling(A)

  A_tensor <- Tensor$new(A)

  tensor_round <- A_tensor$round()$compute()
  A_tensor$drop()
  tensor_floor <- A_tensor$floor()$compute()
  A_tensor$drop()
  tensor_ceiling <- A_tensor$ceiling()$compute()
  A_tensor$drop()


  expect_equal(tensor_round, R_round, tolerance=.Machine$double.eps^0.5,
               info="round matrix element not equivalent")
  expect_equal(tensor_floor, R_floor, tolerance=.Machine$double.eps^0.5,
               info="floor matrix element not equivalent")
  expect_equal(tensor_ceiling, R_ceiling, tolerance=.Machine$double.eps^0.5,
               info="ceiling matrix element not equivalent")

})

test_that("Logs and Exponents", {
  R_log <- suppressWarnings(log(A))
  R_log10 <- suppressWarnings(log10(A))
  R_log1p <- suppressWarnings(log1p(A))
  R_exp <- exp(A)
  R_expm1 <- expm1(A)

  A_tensor <- Tensor$new(A)

  tensor_log <- suppressWarnings(A_tensor$log()$compute())
  A_tensor$drop()
  tensor_log10 <- suppressWarnings(A_tensor$log10()$compute())
  A_tensor$drop()
  tensor_log1p <- suppressWarnings(A_tensor$log1p()$compute())
  A_tensor$drop()
  tensor_exp <- A_tensor$exp()$compute()
  A_tensor$drop()
  tensor_expm1 <- A_tensor$expm1()$compute()
  A_tensor$drop()

  expect_equal(tensor_log, R_log, tolerance=.Machine$double.eps^0.5,
               info="log matrix element not equivalent")
  expect_equal(tensor_log10, R_log10, tolerance=.Machine$double.eps^0.5,
               info="log10 matrix element not equivalent")
  expect_equal(tensor_log1p, R_log1p, tolerance=.Machine$double.eps^0.5,
               info="log1p matrix element not equivalent")
  expect_equal(tensor_exp, R_exp, tolerance=.Machine$double.eps^0.5,
               info="exp matrix element not equivalent")
  expect_equal(tensor_expm1, R_expm1, tolerance=.Machine$double.eps^0.5,
               info="expm1 matrix element not equivalent")

})

test_that("Absolute Value", {

  R_abs <- abs(A)

  A_tensor <- Tensor$new(A)

  tensor_abs <- A_tensor$abs()$compute()

  expect_equal(tensor_abs, R_abs, tolerance=.Machine$double.eps^0.5,
               info="abs matrix element not equivalent")
})

test_that("sqrt", {

  R_sqrt <- suppressWarnings(sqrt(A))

  A_tensor <- Tensor$new(A)

  tensor_sqrt <- suppressWarnings(A_tensor$sqrt()$compute())

  expect_equal(tensor_sqrt, R_sqrt, tolerance=.Machine$double.eps^0.5,
               info="sqrt matrix element not equivalent")
})

test_that("Sign", {

  R_abs <- sign(A)

  A_tensor <- Tensor$new(A)

  tensor_abs <- A_tensor$sign()$compute()

  expect_equal(tensor_abs, R_abs, tolerance=.Machine$double.eps^0.5,
               info="sign matrix element not equivalent")
})

options(warn=0)

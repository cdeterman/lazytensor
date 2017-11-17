

#' @export
log.Tensor <- function(x, base){
  x$log(base)
  return(invisible(x))
}

#' @export
log10.default <- function(x) base::log10(x)

#' @export
log10.Tensor <- function(x){
  x$log10()
  return(invisible(x))
}

#' @export
log1p.Tensor <- function(x){
  x$log1p()
  return(invisible(x))
}

#' @export
log2.Tensor <- function(x){
  x$log2()
  return(invisible(x))
}

#' @export
exp.Tensor <- function(x){
  x$exp()
  return(invisible(x))
}

#' @export
expm1.Tensor <- function(x){
  x$expm1()
  return(invisible(x))
}

#' @export
sin.Tensor <- function(x){
  x$sin()
  return(invisible(x))
}

#' @export
asin.Tensor <- function(x){
  x$asin()
  return(invisible(x))
}

#' @export
sinh.Tensor <- function(x){
  x$sinh()
  return(invisible(x))
}

#' @export
cos.Tensor <- function(x){
  x$cos()
  return(invisible(x))
}

#' @export
acos.Tensor <- function(x){
  x$acos()
  return(invisible(x))
}

#' @export
cosh.Tensor <- function(x){
  x$cosh()
  return(invisible(x))
}

#' @export
tan.Tensor <- function(x){
  x$tan()
  return(invisible(x))
}

#' @export
atan.Tensor <- function(x){
  x$atan()
  return(invisible(x))
}

#' @export
tanh.Tensor <- function(x){
  x$tanh()
  return(invisible(x))
}

#' @export
max.Tensor <- function(..., na.rm = FALSE){
  dots <- list(...)
  if(length(dots) > 1){
    stop("multiple object max not yet implemented")
  }
  x <- dots[[1]]
  x$max(na.rm)
  return(invisible(x))
}

#' @export
min.Tensor <- function(..., na.rm = FALSE){
  dots <- list(...)
  if(length(dots) > 1){
    stop("multiple object min not yet implemented")
  }
  x <- dots[[1]]
  x$min(na.rm)
  return(invisible(x))
}

#' #' @export
#' mean.Tensor <- function(x, trim = 0, na.rm = FALSE){
#'   x$mean(trim, na.rm)
#'   return(invisible(x))
#' }

#' @export
mean.Tensor <- function(x, ...){
  dots <- list(...)

  trim <- 0
  na.rm <- FALSE

  if('trim' %in% names(dots)){
    trim <- dots$trim
  }

  if('na.rm' %in% names(dots)){
    na.rm = dots$na.rm
  }

  x$mean(trim=trim, na.rm=na.rm)

  return(invisible(x))
}

#' @export
round.Tensor <- function(x, digits = 0){
  x$round(digits)
  return(invisible(x))
}

#' @export
floor.Tensor <- function(x){
  x$floor()
  return(invisible(x))
}

#' @export
ceiling.Tensor <- function(x){
  x$ceiling()
  return(invisible(x))
}

#' @export
sqrt.Tensor <- function(x){
  x$sqrt()
  return(invisible(x))
}

#' @export
sign.Tensor <- function(x){
  x$sign
  return(invisible(x))
}

#' @export
sum.Tensor <- function(...){
  dots <- list(...)
  if(length(dots) > 1){
    stop("multiple object sums not yet implemented")
  }
  x <- dots[[1]]
  x$sum()
  return(invisible(x))
}

#' @export
cumsum.Tensor <- function(x){
  x$cumsum()
  return(invisible(x))
}

#' @export
prod.Tensor <- function(...){
  dots <- list(...)
  if(length(dots) > 1){
    stop("multiple object prods not yet implemented")
  }
  x <- dots[[1]]
  x$prod()
  return(invisible(x))
}

#' @export
cumprod.Tensor <- function(x){
  x$cumprod()
  return(invisible(x))
}

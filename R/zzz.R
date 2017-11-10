.onAttach <- function(libname, pkgname) {
  where <- as.environment("package:lazytensor")
  clss <- list(
    c("Tensor", "R6")
  )
  ## Ensure clean initial state for subsequent package loads
  ## while developing //
  sapply(clss, function(cls) {
    idx <- sapply(cls, isClass)
    suppressWarnings(try(sapply(cls[idx], removeClass,
                                where = where), silent = TRUE))
  })
  ## Set formal class equivalent //
  sapply(clss, function(cls) {
    try(setOldClass(cls, where = where), silent = TRUE)
  })
}

.onLoad <- function(...){
  registerS3method("%*%", "Tensor", matmult)
  registerS3method("+", "Tensor", add$new)
  registerS3method("-", "Tensor", subtract$new)
  registerS3method("*", "Tensor", elem_mult$new)
  registerS3method("==", "Tensor", equality$new)
}

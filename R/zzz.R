# @importFrom methods is isClass removeClass setOldClass
# .onAttach <- function(libname, pkgname) {
#   where <- as.environment("package:lazytensor")
#   clss <- list(
#     c("Tensor", "R6")
#   )
#   ## Ensure clean initial state for subsequent package loads
#   ## while developing //
#   sapply(clss, function(cls) {
#     idx <- sapply(cls, isClass)
#     suppressWarnings(try(sapply(cls[idx], removeClass,
#                                 where = where), silent = TRUE))
#   })
#   ## Set formal class equivalent //
#   sapply(clss, function(cls) {
#     try(setOldClass(cls, where = where), silent = TRUE)
#   })
# }

.onLoad <- function(...){
  # %*% S3 method doesn't work
  # registerS3method("%*%", "Tensor", matmult)
  registerS3method("+", "Tensor", add)
  registerS3method("-", "Tensor", subtract)
  registerS3method("*", "Tensor", elem_mult)
  registerS3method("/", "Tensor", elem_div)
  registerS3method("==", "Tensor", eq)
  registerS3method("!=", "Tensor", neq)
  registerS3method(">=", "Tensor", gte)
  registerS3method(">", "Tensor", gt)
  registerS3method("<=", "Tensor", lte)
  registerS3method("<", "Tensor", lt)
}


# Generics for S3 methods that aren't implemented in base

nrow <- function(x) UseMethod("nrow")
ncol <- function(x) UseMethod("ncol")

log10 <- function(x) UseMethod("log10")
log2 <- function(x) UseMethod("log2")

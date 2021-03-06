
is.scalar <- function(x) is.atomic(x) && length(x) == 1L

#' @title The Number of Rows/Columns of a Tensor matrix
#' @description \code{nrow} and \code{ncol} return the number of rows or columns
#' present in \code{x} respectively.
#' @param x A Tensor object
#' @return An integer of length 1
#' @docType methods
#' @rdname nrow-Tensor
#' @author Charles Determan Jr.
#' @export
nrow = function(x){UseMethod("nrow", x)}

#' @rdname nrow-Tensor
#' @export
ncol = function(x){UseMethod("ncol", x)}


nrow.default <- base::nrow
ncol.default <- base::ncol


nrow.Tensor <- function(x){
  return(x$nrow)
}


ncol.Tensor <- function(x){
  return(x$ncol)
}


#' @title Tensor dim method
#' @description Retrieve dimension of object
#' @param x A Tensor object
#' @return A length 2 vector of the number of rows and columns respectively.
#' @note This function returns the output shape of the tensor.  If you wish
#' to guarantee the value is the initial shape of the Tensor you should
#' access the dimensions directly with \code{x$shape}.
#' @docType methods
#' @rdname dim-methods
#' @author Charles Determan Jr.
#' @aliases dim-Tensor
#' @export
dim.Tensor <- function(x){
  return(x$output_shape)
}



#' @export
nrow.Tensor <- function(tensor){
  return(tensor$nrow)
}

#' @export
ncol.Tensor <- function(tensor){
  return(tensor$ncol)
}

#' @export
dim.Tensor <- function(tensor){
  return(tensor$shape)
}

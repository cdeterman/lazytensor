nrow.Tensor <- function(tensor){
  return(tensor$nrow)
}

ncol.Tensor <- function(tensor){
  return(tensor$ncol)
}

dim.Tensor <- function(tensor){
  return(tensor$shape)
}


# setOldClass(c("Tensor", "R6"))




# @export
# setMethod("%*%", c(x = "Tensor", y = "Tensor"),
#           function(x, y){
#             print('the matmult operation')
#             # if(length(x$ops) > 0){
#             #   a = x$compute()
#             # }
#             # if(length(y$ops) > 0){
#             #   b = y$compute()
#             # }
#
#             return(matmult(x, y))
#           },
#           valueClass = "Tensor"
# )

#' @title Tensor Matrix Multiplication
#' @description A higher level interface that may
#' be more comfortable that using the internal \code{.dot}
#' method for dot products.
#' @param x base R or Tensor object
#' @param y base R or Tensor object
#' @return The \code{x} Tensor with the updated internal graph (invisible)
#' @export
matmult <- function(x, y){
  x$.dot(y)
  return(invisible(x))
  # return(dot$new(x, y))
}


# Op <- R6Class(
#   "Op",
#   inherit = Tensor,
#   public = list(
#
#     x = NULL,
#     y = NULL,
#
#     initialize = function(x, y){
#
#       e1 = if(missing(x)) 0 else x
#       e2 = if(missing(y)) 0 else y
#
#       if(missing(y)){
#         self$y = if(!is(e1, "Tensor")) Tensor$new(e1) else e1
#         self$x = if(!is(e2, "Tensor")) Tensor$new(e2) else e2
#       }else{
#         self$x = if(!is(e1, "Tensor")) Tensor$new(e1) else e1
#         self$y = if(!is(e2, "Tensor")) Tensor$new(e2) else e2
#       }
#
#       self$shape = c(nrow(self$x), ncol(self$y))
#       if(length(self$x$ops) > 0 | length(self$y$ops) > 0){
#         private$.has_history = TRUE
#       }
#       private$.input_tensors = list(self$x, self$y)
#     }
#   )
# )

# # @export
# dot <- R6Class(
#   "dot",
#   inherit = Op,
#   public = list(
#     compute = function(feed_list = NA){
#       output = self$x$compute(feed_list) %*% self$y$compute(feed_list)
#       return(output)
#     }
#   )
# )


#' @importFrom methods is
add <- function(x, y){
  if(!is(x, "Tensor")){
    y$.add(x)
    return(invisible(y))
  }else{
    x$.add(y)
    return(invisible(x))
  }
}


subtract <- function(x, y){

  if(missing(y)){
    x$.mult(-1)
    return(invisible(x))
  }else{
    if(!is(x, "Tensor")){
      y$.sub(x, order = 1)
      return(invisible(y))
    }else{
      x$.sub(y)
      return(invisible(x))
    }
  }


}


elem_mult <- function(x, y){
  if(!is(x, "Tensor")){
    y$.mult(x)
    return(invisible(y))
  }else{
    x$.mult(y)
    return(invisible(x))
  }
}


elem_div <- function(x, y){
  if(!is(x, "Tensor")){
    y$.div(x, order = 1)
    return(invisible(y))
  }else{
    x$.div(y)
    return(invisible(x))
  }
}

elem_pow <- function(x, val){
  x$pow(val)
  return(invisible(x))
}


# Comparison Operators
eq <- function(x, y){
  x$.eq(y)
  return(invisible(x))
}

neq <- function(x, y){
  x$.neq(y)
  return(invisible(x))
}

gte <- function(x, y){
  x$.gte(y)
  return(invisible(x))
}

gt <- function(x, y){
  x$.gt(y)
  return(invisible(x))
}

lte <- function(x, y){
  x$.lte(y)
  return(invisible(x))
}

lt <- function(x, y){
  x$.lt(y)
  return(invisible(x))
}

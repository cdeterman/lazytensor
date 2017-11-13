
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

#' @export
matmult <- function(x, y){
  x$.dot(y)
  return(invisible(x))
  # return(dot$new(x, y))
}


Op <- R6Class(
  "Op",
  inherit = Tensor,
  public = list(

    x = NULL,
    y = NULL,

    initialize = function(x, y){

      e1 = if(missing(x)) 0 else x
      e2 = if(missing(y)) 0 else y

      if(missing(y)){
        self$y = if(!is(e1, "Tensor")) Tensor$new(e1) else e1
        self$x = if(!is(e2, "Tensor")) Tensor$new(e2) else e2
      }else{
        self$x = if(!is(e1, "Tensor")) Tensor$new(e1) else e1
        self$y = if(!is(e2, "Tensor")) Tensor$new(e2) else e2
      }

      self$shape = c(nrow(self$x), ncol(self$y))
      if(length(self$x$ops) > 0 | length(self$y$ops) > 0){
        private$.has_history = TRUE
      }
      private$.input_tensors = list(self$x, self$y)
    }
  )
)

# @export
dot <- R6Class(
  "dot",
  inherit = Op,
  public = list(
    compute = function(feed_list = NA){
      output = self$x$compute(feed_list) %*% self$y$compute(feed_list)
      return(output)
    }
  )
)


add <- function(x, y){
  x$.add(y)
  return(invisible(x))
}


subtract <- function(x, y){

  if(missing(y)){
    x$.mult(-1)
  }else{
    x$.sub(y)
  }

  return(invisible(x))
}


elem_mult <- function(x, y){
  x$.mult(y)
  return(invisible(x))
}

elem_div <- function(x, y){
  x$.div(y)
  return(invisible(x))
}



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

  return(mean_tensor$new(x, trim, na.rm))
}


mean_tensor <- R6Class(
  'mean_tensor',
  inherit = Tensor,
  public = list(

    x = NULL,

    initialize = function(x, trim = 0, na.rm = FALSE){
      self$x = x
      self$shape = dim(x)
      if(length(x$ops) > 0){
        private$.has_history = TRUE
      }
      private$.input_tensors = list(x)
      private$.mean_args = list(trim = trim, na.rm = na.rm)
    },

    compute = function(feed_list = NA){
      output = do.call(mean, list(x = self$x$compute(feed_list),
                                  trim = private$.mean_args$trim,
                                  na.rm = private$.mean_args$na.rm))
      return(output)
    }
  ),

  private = list(
    .mean_args = NULL
  )
)

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


# setOldClass(c("Tensor", "R6"))

# @export
# setMethod("%*%", signature(x = "Tensor", y = "Tensor"),
#           function(x, y){
#             if(length(x$ops) > 0){
#               a = x$compute()
#             }
#             if(length(y$ops) > 0){
#               b = y$compute()
#             }
#
#             return(a %*% b)
#           })

#' @export
matmult <- function(x, y){
  return(dot$new(x, y))
}


# @export
dot <- R6Class(
  "dot",
  inherit = Tensor,
  public = list(

    x = NULL,
    y = NULL,

    initialize = function(x, y){
      self$x = x
      self$y = y
      self$shape = c(nrow(x), ncol(y))
      if(length(x$ops) > 0 | length(y$ops) > 0){
        private$.has_history = TRUE
      }
      private$.input_tensors = list(x, y)
    },

    compute = function(feed_list = NA){
      output = self$x$compute(feed_list) %*% self$y$compute(feed_list)
      return(output)
    }
  )
)


#' @export
add <- R6Class(
  'add',
  inherit = Tensor,
  public = list(

    x = NULL,
    y = NULL,

    initialize = function(x,y){
      self$x = x
      self$y = y
      self$shape = dim(x)
      if(length(x$ops) > 0 | length(y$ops) > 0){
        private$.has_history = TRUE
      }
      private$.input_tensors = list(x, y)
    },

    compute = function(feed_list = NA){
      output = self$x$compute(feed_list) + self$y$compute(feed_list)
      return(output)
    }
  )
)


#' @export
subtract <- R6Class(
  'subtract',
  inherit = Tensor,
  public = list(

    x = NULL,
    y = NULL,

    initialize = function(x,y){
      self$x = x
      self$y = y
      self$shape = dim(x)
      if(length(x$ops) > 0 | length(y$ops) > 0){
        private$.has_history = TRUE
      }
      private$.input_tensors = list(x, y)
    },

    compute = function(feed_list = NA){
      output = self$x$compute(feed_list) - self$y$compute(feed_list)
      return(output)
    }
  )
)

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

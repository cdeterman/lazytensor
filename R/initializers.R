
# Initializers
get_initializer <- function(initializer){
  switch(initializer,
         "zeros" = Zeros,
         "ones" = Ones,
         "Constant" = Constant,
         "RandomNormal" = RandomNormal,
         stop("unimplemented initializer function")
  )
}


Initializer <- R6Class("Initializer")

Zeros <- R6Class("Zeros",
                 inherit = Initializer,
                 public = list(
                   initialize = function(shape){
                     if(length(shape) == 1){
                       return(rep(0, shape))
                     }else{
                       return(matrix(0, nrow = shape[1], ncol = shape[2]))
                     }
                   }
                 )
)

Ones <- R6Class("Ones",
                 inherit = Initializer,
                 public = list(
                   initialize = function(shape){
                     if(length(shape) == 1){
                       return(rep(1, shape))
                     }else{
                       return(matrix(1, nrow = shape[1], ncol = shape[2]))
                     }
                   }
                 )
)

Constant <- R6Class("Constant",
                inherit = Initializer,
                public = list(
                  initialize = function(shape, constant){
                    if(length(shape) == 1){
                      return(rep(constant, shape))
                    }else{
                      return(matrix(constant, nrow = shape[1], ncol = shape[2]))
                    }
                  }
                )
)

RandomNormal <- R6Class("RandomNormal",
                    inherit = Initializer,
                    public = list(

                      output = NULL,

                      initialize = function(shape){
                        if(length(shape) == 1){
                          self$output = rnorm(shape)
                          return(invisible(self))
                        }else{
                          self$output = matrix(rnorm(prod(shape)), nrow = shape[1], ncol = shape[2])
                          return(invisible(self))
                        }
                      },

                      compute = function(){
                        return(self$output)
                      }
                    )
)


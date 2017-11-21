
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


#' @title Initializer
#' @description Initializer for a tensor
#' @section Usage:
#' \preformatted{
#' z = Zeros$new(shape = c(4,4))
#' o = Ones$new(shape = c(4,4))
#' c = Constant$new(shape = c(4,4), constant = 2)
#' r = RandomNormal$new(shape = c(4,4))
#' }
#' @section Public Methods:
#' \describe{
#'   \item{\code{compute()}}{Evaluate Initializer}
#' }
#' @return Object of class relevant to defined backend
#' @rdname Initializer-class
#' @author Charles Determan Jr.
#' @import R6
Initializer <- R6Class("Initializer",
                       public = list(

                         shape = NULL,

                         initialize = function(shape){
                           self$shape = shape
                         }
                       )
)


#' @rdname Initializer-class
#' @export
Zeros <- R6Class("Zeros",
                 inherit = Initializer,
                 public = list(
                   compute = function(){
                     if(length(self$shape) == 1){

                       switch(options(lazytensor.backend),
                              "base" = return(rep(0, self$shape)),
                              "gpuR" = return(vclVector(0, length = self$shape))
                       )

                     }else{

                       switch(options(lazytensor.backend),
                         "base" = return(matrix(0, nrow = self$shape[1], ncol = self$shape[2])),
                         "gpuR" = return(vclMatrix(0, nrow = self$shape[1], ncol = self$shape[2]))
                       )

                     }
                   }
                 )
)


#' @rdname Initializer-class
#' @export
Ones <- R6Class("Ones",
                 inherit = Initializer,
                 public = list(
                   compute = function(){
                     if(length(self$shape) == 1){

                       switch(options(lazytensor.backend),
                              "base" = return(rep(1, self$shape)),
                              "gpuR" = return(vclVector(1, length = self$shape))
                       )

                     }else{

                       switch(options(lazytensor.backend),
                              "base" = return(matrix(1, nrow = self$shape[1], ncol = self$shape[2])),
                              "gpuR" = return(vclMatrix(1, nrow = self$shape[1], ncol = self$shape[2]))
                       )

                     }
                   }
                 )
)

#' @rdname Initializer-class
#' @export
Constant <- R6Class("Constant",
                inherit = Initializer,
                public = list(

                  constant = NULL,

                  initialize = function(shape, constant){
                    self$shape = shape
                    self$constant = constant
                  },

                  compute = function(){
                    if(length(self$shape) == 1){

                      switch(options(lazytensor.backend),
                             "base" = return(rep(self$constant, self$shape)),
                             "gpuR" = return(vclVector(self$constant, length = self$shape))
                      )

                    }else{

                      switch(options(lazytensor.backend),
                             "base" = return(matrix(self$constant, nrow = self$shape[1], ncol = self$shape[2])),
                             "gpuR" = return(vclMatrix(self$constant, nrow = self$shape[1], ncol = self$shape[2]))
                      )

                    }
                  }
                )
)

#' @rdname Initializer-class
#' @export
RandomNormal <- R6Class("RandomNormal",
                    inherit = Initializer,
                    public = list(

                      mean = NULL,
                      sd = NULL,

                      initialize = function(shape, mean = 0, sd = 1, seed = NA){
                        self$shape = shape
                        self$mean = mean
                        self$sd = sd
                        self$seed = seed
                      },

                      compute = function(){

                        if(!is.na(self$seed)) set.seed(self$seed)

                        if(length(self$shape) == 1){

                          switch(options(lazytensor.backend),
                                 "base" = return(rnorm(self$shape, mean = self$mean, sd = self$sd)),
                                 "gpuR" = return(vclVector(rnorm(self$shape, mean = self$mean, sd = self$sd)))
                          )

                        }else{

                          switch(options(lazytensor.backend),
                                 "base" = return(matrix(rnorm(prod(self$shape), mean = self$mean, sd = self$sd), nrow = shape[1], ncol = shape[2])),
                                 "gpuR" = return(vclMatrix(rnorm(prod(self$shape), mean = self$mean, sd = self$sd), nrow = self$shape[1], ncol = self$shape[2]))
                          )
                        }
                      }
                    )
)


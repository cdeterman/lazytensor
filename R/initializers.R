
#' @title Retrive Initializer Function
#' @description Returns an initializer function to
#' generate a Tensor object.
#' @param initializer Character scalar defining initializer Tensor to return
#' @export
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
                       inherit = Tensor,
                       public = list(

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
                   initialize = function(shape){

                     self$shape = shape
                     private$.initializer = TRUE

                     if(length(shape) == 1){

                       self$tensor = switch(getOption("lazytensor.backend"),
                              "base" = rep(0, shape),
                              "gpuR" = vclVector(0, length = shape)
                       )

                     }else{

                       self$tensor = switch(getOption("lazytensor.backend"),
                         "base" = matrix(0, nrow = shape[1], ncol = shape[2]),
                         "gpuR" = vclMatrix(0, nrow = shape[1], ncol = shape[2])
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
                   initialize = function(shape){

                     self$shape = shape

                     if(length(shape) == 1){

                       self$tensor = switch(getOption("lazytensor.backend"),
                              "base" = rep(1, shape),
                              "gpuR" = vclVector(1, length = shape)
                       )

                     }else{

                       self$tensor = switch(getOption("lazytensor.backend"),
                              "base" = matrix(1, nrow = shape[1], ncol = shape[2]),
                              "gpuR" = vclMatrix(1, nrow = shape[1], ncol = shape[2])
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

                    if(length(shape) == 1){

                      self$tensor = switch(getOption("lazytensor.backend"),
                             "base" = rep(self$constant, shape),
                             "gpuR" = vclVector(self$constant, length = shape)
                      )

                    }else{

                      self$tensor = switch(getOption("lazytensor.backend"),
                             "base" = matrix(self$constant, nrow = shape[1], ncol = shape[2]),
                             "gpuR" = vclMatrix(self$constant, nrow = shape[1], ncol = shape[2])
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

                      rmean = NULL,
                      sd = NULL,
                      seed = NULL,

                      initialize = function(shape, mean = 0, sd = 1, seed = NA){
                        self$shape = shape
                        self$rmean = mean
                        self$sd = sd
                        self$seed = seed

                        if(!is.na(self$seed)) set.seed(self$seed)

                        if(length(shape) == 1){

                          self$tensor = switch(getOption("lazytensor.backend"),
                                 "base" = rnorm(shape, mean = self$rmean, sd = self$sd),
                                 "gpuR" = vclVector(rnorm(shape, mean = self$rmean, sd = self$sd))
                          )

                        }else{

                          self$tensor = switch(getOption("lazytensor.backend"),
                                 "base" = matrix(rnorm(prod(shape), mean = self$rmean, sd = self$sd), nrow = shape[1], ncol = shape[2]),
                                 "gpuR" = vclMatrix(rnorm(prod(shape), mean = self$rmean, sd = self$sd), nrow = shape[1], ncol = shape[2])
                          )
                        }
                      }
                    )
)


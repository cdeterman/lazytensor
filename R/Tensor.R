
#' @import R6
#' @export
Tensor <- R6Class("Tensor",
                  active = list(
                    shape = function(value){
                      if(missing(value)) return(private$.shape)
                      private$.shape = value
                    },

                    nrow = function(value){
                      if(missing(value)) return(private$.shape[1])
                      private$.shape[1] = value
                    },

                    ncol = function(value){
                      if(missing(value)) return(private$.shape[2])
                      private$.shape[2] = value
                    }

                  ),
                  public = list(
                    tensor = NULL,
                    name = NULL,
                    ops = list(),

                    initialize = function(initializer, shape){
                      if(is.character(initializer)){
                        self$tensor = get_initializer(initializer)
                        self$shape = shape
                        private$.initializer = TRUE
                      }else{
                        self$tensor = initializer
                        self$shape = private$.get_shape(initializer)
                        private$.initializer = FALSE
                      }
                    },

                    # add = function(x){
                    #   self$ops[[length(self$ops) + 1]] = "`+`"
                    #   private$.has_history = TRUE
                    #   private$.input_tensors = c(private$.input_tensors, x)
                    #   invisible(self)
                    # },
                    #
                    # subtract = function(x){
                    #   self$ops[[length(self$ops) + 1]] = "`-`"
                    #   private$.has_history = TRUE
                    #   private$.input_tensors = c(private$.input_tensors, x)
                    #   invisible(self)
                    # },

                    pow = function(val){
                      self$ops[[length(self$ops) + 1]] = "`^`"
                      invisible(self)
                    },

                    sin = function(){
                      self$ops[[length(self$ops) + 1]] = "sin"
                      invisible(self)
                    },

                    asin = function(){
                      self$ops[[length(self$ops) + 1]] = "asin"
                      invisible(self)
                    },

                    sinh = function(){
                      self$ops[[length(self$ops) + 1]] = "sinh"
                      invisible(self)
                    },

                    cos = function(){
                      self$ops[[length(self$ops) + 1]] = "cos"
                      invisible(self)
                    },

                    acos = function(){
                      self$ops[[length(self$ops) + 1]] = "acos"
                      invisible(self)
                    },

                    cosh = function(){
                      self$ops[[length(self$ops) + 1]] = "cosh"
                      invisible(self)
                    },

                    tan = function(){
                      self$ops[[length(self$ops) + 1]] = "tan"
                      invisible(self)
                    },

                    atan = function(){
                      self$ops[[length(self$ops) + 1]] = "atan"
                      invisible(self)
                    },

                    tanh = function(){
                      self$ops[[length(self$ops) + 1]] = "tanh"
                      invisible(self)
                    },

                    max = function(){
                      self$ops[[length(self$ops) + 1]] = "max"
                      invisible(self)
                    },

                    min = function(){
                      self$ops[[length(self$ops) + 1]] = "min"
                      invisible(self)
                    },

                    compute = function(feed_list = NA){
                      if(private$.initializer){

                        # tensor is in initializer object

                        if(length(self$ops) == 0){
                          print("returning initializer")
                          if(length(private$.input_tensors) > 0){
                            stop("shouldn't be providing inputs to initializer")
                            # for(i in seq_along(private$.input_tensors)){
                            #   private$.input_tensors[i]$compute(feed_list)
                            # }
                          }
                          return(self$tensor$new(self$shape)$compute())
                        }else{
                          print('initializer with ops')
                          if(length(private$.input_tensors) > 0){
                            # for(i in seq_along(private$.input_tensors)){
                            #   private$.input_tensors[i]$compute()
                            # }
                            stop("shouldn't be providing inputs to initializer")
                          }
                          output = self$tensor$new(self$shape)$compute()
                          for(f_str in self$ops){
                            f = eval(parse(text = f_str))
                            output = f(output)
                          }
                          return(output)
                        }

                      }else{

                        # tensor is either a previously initialized R object
                        # or it is the output tensor of another operation

                        if(class(self)[1] == 'Placeholder'){

                          if(any(is.na(feed_list)) | length(names(feed_list)) == 0){
                            stop(paste0("you must provide named list of inputs for placeholder: ", self$name))
                          }

                          if(!self$name %in% names(feed_list)){
                            stop(paste0("must provide named element for placeholder: ", self$name))
                          }
                          input_tensor = feed_list[[self$name]]
                          if(!all(dim(input_tensor) == self$shape)){
                            stop(paste0("input object for placeholder: ", self$name,
                                        "doesn't match shape: ", self$shape))
                          }
                          self$tensor = input_tensor
                        }

                        if(length(self$ops) == 0){
                          # no operations on this tensor
                          if(length(private$.input_tensors) > 0){
                            for(i in seq_along(private$.input_tensors)){
                              private$.input_tensors[i]$compute(feed_list)
                            }
                          }
                          return(self$tensor)
                        }else{
                          # operations to be completed
                          if(length(private$.input_tensors) > 0){
                            # input tensors require evaluation
                            for(i in seq_along(private$.input_tensors)){
                              private$.input_tensors[i]$compute(feed_list)
                            }
                          }
                          output = self$tensor
                          for(f_str in self$ops){
                            # print(paste0('evaluating: ', f_str))
                            f = eval(parse(text = f_str))
                            output = f(output)
                          }
                          return(output)
                        }
                      }
                    },

                    has_history = function(){
                      return(private$.has_history)
                    },

                    input_tensors = function(){
                      return(private$.input_tensors)
                    }
                  ),
                  private = list(
                    .shape = NULL,
                    .initializer = FALSE,
                    .has_history = NULL,
                    .input_tensors = NULL,

                    .get_shape = function(value){
                      out = switch(class(value),
                                   "integer" = length(value),
                                   "numeric" = length(value),
                                   "matrix" = dim(value),
                                   stop("unrecognized class"))
                      return(out)
                    }
                  )
)


#' @export
Placeholder <- R6Class("Placeholder",
                       inherit = Tensor,
                       public = list(

                         initialize = function(shape, name){
                           self$shape = shape
                           if(missing(name)){
                             counter = private$.getCounter()
                             private$.shared_env$counter = counter + 1
                             self$name = paste0('ph_', counter)
                           }else{
                             self$name = name
                           }
                         }
                         # ,
                         #
                         # finalize = function(){
                         #   private$.shared_env$counter = counter - 1
                         # }

                       ),
                       private = list(
                         .shared_env = new.env(),

                         .getCounter = function(){
                           counter = private$.shared_env$counter
                           if( is.null( counter ) )
                             counter =  1
                           return( counter )
                         }
                       )
)

# #' @export
# Variable <- R6Class(
#   "Variable",
#   inherit = Tensor,
#   public = list(
#     initialize = function()
#   )
#)

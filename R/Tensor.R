
Operation <- R6Class(
  "Operation",
  public = list(

    op = NULL,
    args = NULL,
    input_indices = NULL,
    has_history = FALSE,

    initialize = function(op, args = NULL, input_indices = NA){
      self$op = op
      self$args = args

      if(!is.na(input_indices)) self$has_history = TRUE
      self$input_indices = input_indices
    },

    get_op = function(){
      return(self$op)
    },

    get_args = function(){
      if(!is.null(self$args)){
        return(paste(self$args, sep = ","))
      }
      return(NULL)
    },

    get_input_indices = function(){
      return(self$input_indices)
    }
  )
)


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
                      switch(class(initializer),
                             "Tensor" = {
                               self$tensor = initializer$tensor
                               self$shape = self$tensor$shape
                               private$.initializer = FALSE
                             },
                             "character" = {
                               self$tensor = get_initializer(initializer)
                               self$shape = shape
                               private$.initializer = TRUE
                             },
                             {
                               self$tensor = initializer
                               if(missing(shape)){
                                 private$.shape = private$.get_shape(initializer)
                               }
                               private$.initializer = FALSE
                             }
                      )
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

                    .dot = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`%*%`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .add = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`+`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .sub = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`-`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .mult = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`*`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .div = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`/`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    # logical operators

                    .eq = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`==`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .neq = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`!=`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .gte = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`>=`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .gt = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`>`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .lte = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`<=`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    .lt = function(x){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      private$.input_tensors = c(private$.input_tensors, x_tensor)
                      self$ops[[length(self$ops) + 1]] = Operation$new("`<`", input_indices = length(private$.input_tensors))
                      invisible(self)
                    },

                    pow = function(val){
                      self$ops[[length(self$ops) + 1]] = Operation$new("`^`", args = as.character(val))
                      invisible(self)
                    },

                    log = function(base){
                      if(missing(base)){
                        self$ops[[length(self$ops) + 1]] = Operation$new("log", args = paste0("base = exp(1)"))
                      }else{
                        self$ops[[length(self$ops) + 1]] = Operation$new("log", args = paste0("base = ", base))
                      }

                      invisible(self)
                    },

                    log10 = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("log10")
                      invisible(self)
                    },

                    log1p = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("log1p")
                      invisible(self)
                    },

                    log2 = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("log2")
                      invisible(self)
                    },

                    exp = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("exp")
                      invisible(self)
                    },

                    expm1 = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("expm1")
                      invisible(self)
                    },

                    sin = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("sin")
                      invisible(self)
                    },

                    asin = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("asin")
                      invisible(self)
                    },

                    sinh = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("sinh")
                      invisible(self)
                    },

                    cos = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("cos")
                      invisible(self)
                    },

                    acos = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("acos")
                      invisible(self)
                    },

                    cosh = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("cosh")
                      invisible(self)
                    },

                    tan = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("tan")
                      invisible(self)
                    },

                    atan = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("atan")
                      invisible(self)
                    },

                    tanh = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("tanh")
                      invisible(self)
                    },

                    max = function(na.rm = FALSE){
                      self$ops[[length(self$ops) + 1]] = Operation$new("max", args = paste0("na.rm = ", na.rm))
                      invisible(self)
                    },

                    min = function(na.rm = FALSE){
                      self$ops[[length(self$ops) + 1]] = Operation$new("min", args = paste0("na.rm = ", na.rm))
                      invisible(self)
                    },

                    mean = function(trim = 0, na.rm = FALSE){
                      # self$ops[[length(self$ops) + 1]] = c("mean", paste0("trim = ", trim), paste0("na.rm = ", na.rm))
                      self$ops[[length(self$ops) + 1]] = Operation$new("round", args = c(paste0("trim = ", trim), paste0("na.rm = ", na.rm)))
                      invisible(self)
                    },

                    abs = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("abs")
                      invisible(self)
                    },

                    round = function(digits = 0){
                      self$ops[[length(self$ops) + 1]] = Operation$new("round", args = paste0("digits = ", digits))
                      invisible(self)
                    },

                    floor = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("floor")
                      invisible(self)
                    },

                    ceiling = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("ceiling")
                      invisible(self)
                    },

                    sqrt = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("sqrt")
                      invisible(self)
                    },

                    sign = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("sign")
                      invisible(self)
                    },

                    sum = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("sum")
                      invisible(self)
                    },

                    cumsum = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("cumsum")
                      invisible(self)
                    },

                    prod = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("prod")
                      invisible(self)
                    },

                    cumprod = function(){
                      self$ops[[length(self$ops) + 1]] = Operation$new("cumprod")
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
                          # if(length(private$.input_tensors) > 0){
                          #   # input tensors require evaluation
                          #   for(i in seq_along(private$.input_tensors)){
                          #     private$.input_tensors[i]$compute(feed_list)
                          #   }
                          # }
                          output = self$tensor

                          ### NEED BETTER ERROR/WARNING MESSAGES HERE!!!

                          for(f_str in self$ops){

                            if(is(f_str, "Operation")){

                              idx = f_str$get_input_indices()

                              if(is.na(idx)){
                                args = f_str$get_args()
                                if(!is.null(args)){
                                  f = parse(text = paste(f_str$get_op(), '(output,', args, ')'))
                                }else{
                                  f = parse(text = paste(f_str$get_op(), '(output)'))
                                }

                                output = eval(f)
                              }else{

                                args = f_str$get_args()
                                inputs = paste(sapply(idx, function(x) paste0('private$.input_tensors[[', x, ']]$compute(feed_list)')), collapse = ", ")

                                if(!is.null(args)){
                                  f = parse(text = paste(f_str$get_op(), '(output,', inputs, ", ", args, ')'))
                                }else{
                                  f = parse(text = paste(f_str$get_op(), '(output,', inputs, ')'))
                                }
                                # print(output)
                                # print(private$.input_tensors[[idx]]$compute(feed_list))
                                # print(f)
                                output = eval(f)
                              }

                            }else{
                              stop("shouldn't be in this place anymore")
                              # if(length(f_str) == 1){
                              #   f = eval(parse(text = f_str))
                              #   output = f(output)
                              # }else{
                              #   output = eval(parse(text = paste(f_str[1], '(output, ', as.character(f_str[2:length(f_str)]), ')')))
                              # }
                            }
                          }
                          return(output)
                        }
                      }
                    },

                    drop = function(idx = NA, name = NA){
                      # remove an operation by index, name, or 'pop'
                      if(!is.na(idx)){
                        stop("index dropping not yet supported")
                      }
                      if(!is.na(name)){
                        stop("name dropping not yet supported")
                      }
                      self$ops[[length(self$ops)]] = NULL
                      return(invisible(self))
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
                    },
                    .args = NULL
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

#' @export
variable <- R6Class(
  "variable",
  inherit = Tensor,
  public = list(
    initialize = function(value, name = NA){

      if(is.vector(value, mode = "numeric")){
        shape = length(value)
      }else{
        shape = dim(value)
      }

      if(!is.na(name)) self$name = name

      super$initialize(value, shape)

      # make shape immutable
      lockBinding('.shape', private)
    }
  )
)


#' @export
constant <- R6Class(
  "constant",
  inherit = Tensor,

  public = list(
    initialize = function(value, name = NA){

      if(is.vector(value, mode = "numeric")){
        shape = length(value)
      }else{
        shape = dim(value)
      }

      super$initialize(value, shape)

      # make tensor and shape immutable
      lockBinding("tensor", self)
      lockBinding(".shape", private)
    }
  )
)

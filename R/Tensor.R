
Operation <- R6Class(
  "Operation",
  public = list(

    op = NULL,
    args = NULL,
    input_indices = NULL,
    order = NULL,
    has_history = FALSE,

    initialize = function(op, args = NULL, input_indices = NA, order = NA){
      self$op = op
      self$args = args
      self$order = order

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


#' @title Tensor
#' @description Tensor class providing methods for chaining operations to be
#' evaluated lazily.
#' @section Usage:
#' \preformatted{
#' rmat = matrix(rnorm(16), 4)
#' a = Tensor$new(rmat)
#' init = Tensor$new("RandomNormal", shape = c(4,4))
#' v = variable$new(rmat)
#' c = constant$new(rmat)
#' }
#' @field tensor Stores the tensor object
#' @field name The name of the Tensor object
#' @field graph The graph containing the registered Nodes and
#' Operations to be evaluated
#' @section Public Methods:
#' \describe{
#'   \item{\code{.dot(name = NA)}}{Dot product of two Tensors}
#'   \item{\code{.add(name = NA)}}{Addition of two Tensors}
#'   \item{\code{.sub(name = NA)}}{Subtraction of two Tensors}
#'   \item{\code{.mult(name = NA)}}{Elementwise multiplication of two Tensors}
#'   \item{\code{.div(name = NA)}}{Elementwise division of two Tensors}
#'   \item{\code{.eq(name = NA)}}{Elementwise equality of Tensors}
#'   \item{\code{.neq(name = NA)}}{Elementwise inequality of Tensors}
#'   \item{\code{.gte(name = NA)}}{Elementwise >= of Tensors}
#'   \item{\code{.gt(name = NA)}}{Elementwise > of Tensors}
#'   \item{\code{.lte(name = NA)}}{Elementwise <= of Tensors}
#'   \item{\code{.lt(name = NA)}}{Elementwise < of Tensors}
#'   \item{\code{pow(val,name = NA)}}{Power (\code{^}) of Tensor elements}
#'   \item{\code{log(base = exp(1))(name = NA)}}{Natural log of Tensor elements}
#'   \item{\code{log10(name = NA)}}{Log base 10 of Tensor elements}
#'   \item{\code{log1p(name = NA)}}{\code{log(1+x)} of Tensor elements}
#'   \item{\code{log2(name = NA)}}{Log base 2 of Tensor elements}
#'   \item{\code{exp(name = NA)}}{Exponential of Tensor elements}
#'   \item{\code{expm1(name = NA)}}{\code{exp(x) - 1} of Tensor elements}
#'   \item{\code{sin(name = NA)}}{sin of Tensor elements}
#'   \item{\code{asin(name = NA)}}{asin of Tensor elements}
#'   \item{\code{sinh(name = NA)}}{sinh of Tensor elements}
#'   \item{\code{cos(name = NA)}}{cos of Tensor elements}
#'   \item{\code{acos(name = NA)}}{acos of Tensor elements}
#'   \item{\code{cosh(name = NA)}}{cosh of Tensor elements}
#'   \item{\code{tan(name = NA)}}{tan of Tensor elements}
#'   \item{\code{atan(name = NA)}}{atan of Tensor elements}
#'   \item{\code{tanh(name = NA)}}{tanh of Tensor elements}
#'   \item{\code{max(name = NA)}}{max of Tensor elements}
#'   \item{\code{min(name = NA)}}{min of Tensor elements}
#'   \item{\code{mean(name = NA)}}{mean of Tensor elements}
#'   \item{\code{abs(name = NA)}}{abs of Tensor elements}
#'   \item{\code{round(digits, name = NA)}}{round of Tensor elements}
#'   \item{\code{floor(name = NA)}}{floor of Tensor elements}
#'   \item{\code{ceiling(name = NA)}}{ceiling of Tensor elements}
#'   \item{\code{sqrt(name = NA)}}{sqrt of Tensor elements}
#'   \item{\code{sign(name = NA)}}{sign of Tensor elements}
#'   \item{\code{sum(name = NA)}}{sum of Tensor elements}
#'   \item{\code{cumsum(name = NA)}}{cumulative sum of Tensor elements}
#'   \item{\code{prod(name = NA)}}{product of Tensor elements}
#'   \item{\code{cumprod(name = NA)}}{cumulative product of Tensor elements}
#'   \item{\code{compute(feed_list)}}{Compute/Evaluate the Tensor graph}
#'   \item{\code{chain(f)(name = NA)}}{Process a function to be evaluated within the Tensor graph}
#'   \item{\code{drop(idx = NA, name = NA)}}{Drop a previously added operation from the Tensor graph}
#'   \item{\code{has_history}}{returns boolean indicating of additional Tensor involved in graph}
#' }
#' @section Details:
#' \code{Tensor} is the base class of this package (based upon R6).  The
#' \code{Placeholder}, \code{variable}, and \code{constant} are all child
#' class of the \code{Tensor} class.
#'
#' There are only minor differences with the \code{variable} and \code{constant}
#' child classes.  \code{variable} instances have their \code{shape} fixed.  The
#' internal data values may be changed but the shape of the Tensor is not allowed.
#' As can be assumed, the \code{constant} child class does not allow any change to
#' the underlying Tensor or the Tensor shape.
#'
#' The \code{Placeholder} class is unique in that it contains NO underlying data.
#' The class simply contains the shape of the intended Tensor.  All chained methods
#' are still applicable.  The user is recommended to provide a \code{name} to the
#' initialized \code{Placeholder} as when the final \code{compute} method is called,
#' the operations will look for a passed named list element in the
#' \code{compute{feed_list = list()}} call.  This allows the user to provide alternate
#' datasets to a previously prototyped process.  This mirrors the functionality of
#' libraries such as Tensorflow.
#'
#' @return Object of \code{\link{R6Class}} with methods for symbolic operations
#' @format \code{\link{R6Class}} object
#' @rdname Tensor-class
#' @author Charles Determan Jr.
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
                    name = list(),
                    # ops = list(),

                    # list of list for ops?

                    # list of operation names and indices to actual operations in `ops`
                    graph = list(),
                    # outbound_nodes = list(),

                    initialize = function(initializer, shape){

                      self$graph = list()

                      if(is(initializer, "R6")){
                        cls = tail(class(initializer), 2)[1]
                        if(missing(shape)){
                          shape = initializer$shape
                        }
                      }else{
                        cls = class(initializer)
                      }

                      switch(cls,
                             # "Initializer" = {
                             #   self$tensor = initializer
                             #   if(missing(shape)){
                             #     stop("must provide shape with an initializer")
                             #   }
                             #   self$shape = shape
                             #   private$.initializer = TRUE
                             # },
                             "Tensor" = {
                               self$tensor = initializer$tensor
                               self$shape = self$tensor$shape
                               private$.initializer = FALSE
                             },
                             "character" = {
                               self$tensor = get_initializer(initializer)
                               if(missing(shape)){
                                 stop("must provide shape with an initializer")
                               }
                               self$shape = shape
                               private$.initializer = TRUE
                             },
                             "matrix" = {
                               self$tensor = initializer
                               if(missing(shape)){
                                 private$.shape = private$.get_shape(initializer)
                               }
                               private$.initializer = FALSE
                             },
                             "array" = {
                               self$tensor = initializer
                               if(missing(shape)){
                                 private$.shape = private$.get_shape(initializer)
                               }
                               private$.initializer = FALSE
                             },
                             "numeric" = {
                               self$tensor = initializer
                               if(missing(shape)){
                                 private$.shape = private$.get_shape(initializer)
                               }
                               private$.initializer = FALSE
                             },
                             "integer" = {
                               self$tensor = initializer
                               if(missing(shape)){
                                 private$.shape = private$.get_shape(initializer)
                               }
                               private$.initializer = FALSE
                             },
                             {
                               if(inherits(initializer, "gpuMatrix") | inherits(initializer, "vclMatrix")){

                                 if(getOption("lazytensor.backend") != "gpuR"){
                                   stop("Please setBackend('gpuR') to use gpuR objects")
                                 }

                                 self$tensor = initializer
                                 if(missing(shape)){
                                   private$.shape = private$.get_shape(initializer)
                                 }
                                 private$.initializer = FALSE
                               }else{
                                 stop(paste0("unimplemented class: ", class(initializer)))
                               }
                             }
                      )
                    },

                    .dot = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # matrix multiplication may change shape
                      output_shapes = switch(length(input_shapes),
                                             list(1),
                                             list(c(self$nrow, input_shapes[2])),
                                             stop("unimplemented for ndim > 2")
                      )

                      Node$new(self,
                               ops = list(Operation$new("`%*%`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)

                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`%*%`", input_indices = length(private$.input_tensors))
                      # invisible(self)
                    },

                    .add = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # addition doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`+`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    .sub = function(x, order = NA, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`-`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # subtraction doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`-`", order = order)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    .mult = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`*`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # elementwise mutliplication doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`*`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    .div = function(x, order = NA, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`/`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # division doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`/`", order = order)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    # logical operators

                    .eq = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`==`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # logical checks don't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`==`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    .neq = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`!=`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # logical checks don't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`!=`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    .gte = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`>=`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # logical checks don't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`>=`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    .gt = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`>`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # logical checks don't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`>`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    .lte = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`<=`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # logical checks don't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`<=`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    .lt = function(x, name = NA){
                      private$.has_history = TRUE
                      x_tensor = if(!is(x, "Tensor")) Tensor$new(x) else x
                      # private$.input_tensors = c(private$.input_tensors, x_tensor)
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`<`", input_indices = length(private$.input_tensors))
                      # invisible(self)

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = list(x_tensor$shape)
                      # logical checks don't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`<`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    pow = function(val, name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("`^`", args = as.character(val))
                      # invisible(self)

                      name = private$.createName(name)

                      x_tensor = if(!is(val, "Tensor")) Tensor$new(val) else val

                      # if Tensor wasn't provided, there still is an internal Tensor now
                      private$.has_history = TRUE

                      # Functions is a single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()

                      # Function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("`^`")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) c(tail(self$graph, 1), x_tensor$nodes) else list(tail(x_tensor$nodes, 1)),
                               output_nodes = list(),
                               input_tensors = list(x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    log = function(base, name = NA){
                      # if(missing(base)){
                      #   self$ops[[length(self$ops) + 1]] = Operation$new("log", args = paste0("base = exp(1)"))
                      # }else{
                      #   self$ops[[length(self$ops) + 1]] = Operation$new("log", args = paste0("base = ", base))
                      # }
                      # invisible(self)

                      args = if(missing(base)) paste0("base = exp(1)") else paste0("base = ", base)

                      name = private$.createName(name)

                      # Functions is a single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # Function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("log", args = args)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    log10 = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("log10")
                      # invisible(self)

                      name = private$.createName(name)

                      # Functions is a single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # Function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("log10")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    log1p = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("log1p")
                      # invisible(self)

                      name = private$.createName(name)

                      # Functions is a single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # Function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("log1p")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    log2 = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("log2")
                      # invisible(self)

                      name = private$.createName(name)

                      # Functions is a single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # Function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("log2")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    exp = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("exp")
                      # invisible(self)

                      name = private$.createName(name)

                      # Functions is a single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # Function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("exp")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    expm1 = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("expm1")
                      # invisible(self)

                      name = private$.createName(name)

                      # Functions is a single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # Function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("expm1")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    sin = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("sin")

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("sin")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    asin = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("asin")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("asin")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    sinh = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("sinh")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("sinh")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    cos = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("cos")

                      name = private$.createName(name)

                      # trig functions are single input operations, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # cos doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("cos")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      # self$graph[[name]] = length(self$ops)
                      invisible(self)
                    },

                    acos = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("acos")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("acos")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    cosh = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("cosh")
                      # invisible(self)
                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("cosh")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    tan = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("tan")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("tan")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    atan = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("atan")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("atan")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    tanh = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("tanh")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("tanh")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    max = function(na.rm = FALSE, name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("max", args = paste0("na.rm = ", na.rm))
                      # invisible(self)

                      name = private$.createName(name)

                      args = paste0("na.rm = ", na.rm)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function returns scalar
                      output_shapes = list(1)

                      Node$new(self,
                               ops = list(Operation$new("max", args = args)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    min = function(na.rm = FALSE, name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("min", args = paste0("na.rm = ", na.rm))
                      # invisible(self)

                      name = private$.createName(name)

                      args = paste0("na.rm = ", na.rm)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function returns scalar
                      output_shapes = list(1)

                      Node$new(self,
                               ops = list(Operation$new("min", args = args)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    pmax = function(..., na.rm = FALSE, name = NA){
                      name = private$.createName(name)

                      dots = list(...)
                      args = paste0("na.rm = ", na.rm)

                      # get inputs
                      x_tensors = lapply(dots, function(x) if(!is(x, "Tensor")) Tensor$new(x) else x)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function returns object of the same dimensions
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("pmax", args = args)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(x_tensors),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    pmin = function(..., na.rm = FALSE, name = NA){
                      name = private$.createName(name)

                      dots = list(...)
                      args = paste0("na.rm = ", na.rm)

                      # get inputs
                      x_tensors = lapply(dots, function(x) if(!is(x, "Tensor")) Tensor$new(x) else x)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function returns object of the same dimensions
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("pmin", args = args)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(x_tensors),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    mean = function(trim = 0, na.rm = FALSE, name = NA){
                      # self$ops[[length(self$ops) + 1]] = c("mean", paste0("trim = ", trim), paste0("na.rm = ", na.rm))
                      # self$ops[[length(self$ops) + 1]] = Operation$new("mean", args = c(paste0("trim = ", trim), paste0("na.rm = ", na.rm)))
                      # invisible(self)

                      name = private$.createName(name)

                      args = c(paste0("trim = ", trim), paste0("na.rm = ", na.rm))

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function returns scalar
                      output_shapes = list(1)

                      Node$new(self,
                               ops = list(Operation$new("mean", args = args)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    abs = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("abs")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("abs")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)

                    },

                    round = function(digits = 0, name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("round", args = paste0("digits = ", digits))
                      # invisible(self)

                      name = private$.createName(name)

                      args = paste0("digits = ", digits)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("round", args = args)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    floor = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("floor")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("floor")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    ceiling = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("ceiling")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("ceiling")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    sqrt = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("sqrt")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("sqrt")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    sign = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("sign")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("sign")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    sum = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("sum")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function returns a scalar
                      output_shapes = list(1)

                      Node$new(self,
                               ops = list(Operation$new("sum")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    cumsum = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("cumsum")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = list(1, prod(input_shapes[[1]]))

                      Node$new(self,
                               ops = list(Operation$new("cumsum")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    prod = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("prod")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function returns a scalar
                      output_shapes = list(1)

                      Node$new(self,
                               ops = list(Operation$new("prod")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    cumprod = function(name = NA){
                      # self$ops[[length(self$ops) + 1]] = Operation$new("cumprod")
                      # invisible(self)

                      name = private$.createName(name)

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = list(1, prod(input_shapes[[1]]))

                      Node$new(self,
                               ops = list(Operation$new("cumprod")),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list(),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    sweep = function(MARGIN, STATS, FUN, name = NA){
                      name = private$.createName(name)

                      args = c(paste0("MARGIN = ", MARGIN),
                               paste0("FUN = '", FUN, "'"))

                      x_tensor = if(!is(STATS, "Tensor")) Tensor$new(STATS) else STATS

                      # function is single input operation, so take last node
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()
                      # function doesn't change shape
                      output_shapes = input_shapes

                      Node$new(self,
                               ops = list(Operation$new("sweep", args = args)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = list("STATS" = x_tensor),
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      invisible(self)
                    },

                    compute = function(feed_list = NA){
                      if(private$.initializer){

                        # tensor is in initializer object

                        if(length(self$ops) == 0){

                          print("returning initializer")
                          return(self$tensor$new(self$shape)$compute())

                        }else{

                          print('initializer with ops')
                          output = self$tensor$new(self$shape)$compute()

                          # for(f_str in self$ops){
                          #   f = eval(parse(text = f_str))
                          #   output = f(output)
                          # }
                          # return(output)
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
                          output = input_tensor
                        }else{
                          output = self$tensor
                        }

                        if(length(self$graph) == 0){
                          ## no operations on this tensor
                          # if(length(private$.input_tensors) > 0){
                          #   for(i in seq_along(private$.input_tensors)){
                          #     private$.input_tensors[i]$compute(feed_list)
                          #   }
                          # }
                          return(self$tensor)
                        }else{
                          # operations to be completed

                          # output = self$tensor

                          ### NEED BETTER ERROR/WARNING MESSAGES HERE!!!

                          for(node in self$graph){

                            for(o in seq_along(node$ops)){

                              op = node$ops[[o]]

                              if(is(op, "Operation")){

                                # check for any input tensors
                                input_flag = length(node$input_tensors) > 0
                                input_tensors = if(input_flag) node$input_tensors[[o]] else NA
                                idx <- if(input_flag) seq(length(input_tensors)) else NA

                                if(!input_flag){
                                  # no additional tensors

                                  # get method arguments
                                  args = op$get_args()
                                  func = op$get_op()

                                  # print('operation')
                                  # print(func)

                                  if(is.function(func)){

                                    if(!is.null(args)){
                                      stop('args not implemented yet')
                                    }else{
                                      output = func(output)
                                    }

                                  }else{
                                    if(!is.null(args)){
                                      f = parse(text = paste(func, '(output,', paste0(args, collapse = ", "), ')'))
                                    }else{
                                      f = parse(text = paste(func, '(output)'))
                                    }

                                    # print('args')
                                    # print(args)
                                    # print('expression')
                                    # print(f)

                                    output = eval(f)

                                  }

                                }else{

                                  # get method arguments
                                  args = op$get_args()

                                  # need to make sure order of inputs is correct
                                  # get input tensors (this likely should be cached)
                                  if(is.list(node$input_tensors[[o]])){
                                    inames = names(node$input_tensors[[o]])
                                    if(!is.null(inames)){
                                      inputs = paste(sapply(idx, function(x) paste0(inames[x], '=node$input_tensors[[o]][[', x, ']]$compute(feed_list)')), collapse = ",")
                                    }else{
                                      inputs = paste(sapply(idx, function(x) paste0('node$input_tensors[[o]][[', x, ']]$compute(feed_list)')), collapse = ",")
                                    }

                                  }else{
                                    inputs = paste0('node$input_tensors[[o]]$compute(feed_list)')
                                  }

                                  # get function for operation
                                  func = op$get_op()

                                  # set prefix as cannot pass function definition as string
                                  prefix = ifelse(is.function(func), "func", func)

                                  if(!is.null(args)){
                                    # print('args not null')
                                    if(is.na(op$order)){
                                      f = parse(text = paste(prefix, '(output,',
                                                             inputs, ", ",
                                                             paste0(args, collapse = ", "),
                                                             ')'))
                                    }else{
                                      f = parse(text = paste(prefix, '(',
                                                             inputs,
                                                             ", output,",
                                                             paste0(args, collapse = ", "),
                                                             ')'))
                                    }
                                  }else{

                                    if(is.na(op$order)){
                                      # print('operation')
                                      # print(op$get_op())
                                      # print("inputs")
                                      # print(inputs)
                                      f = parse(text = paste(prefix, '(output,', inputs, ')'))
                                    }else{
                                      f = parse(text = paste(prefix, '(', inputs, ', output)'))
                                    }
                                  }

                                  # print('args')
                                  # print(args)
                                  # print('expression')
                                  # print(f)

                                  output = eval(f)
                                }

                              }else{

                                if(is(op, "function")){


                                }else{
                                  stop("shouldn't be in this place anymore")
                                }

                                stop("shouldn't be in this legacy place anymore")
                                # if(length(op) == 1){
                                #   f = eval(parse(text = op))
                                #   output = f(output)
                                # }else{
                                #   output = eval(parse(text = paste(op[1], '(output, ', as.character(op[2:length(op)]), ')')))
                                # }
                              }
                            }
                          }

                          return(output)
                        }
                      }
                    },

                    chain = function(f, name, ...){
                      # ops_start = length(self$ops) + 1
                      # f(self)
                      # ops_end = length(self$ops)
                      # self$ops_names[[name]] = c(ops_start:ops_end)

                      theDots = list(...)
                      if(length(theDots) > 0){
                        theDots = list(lapply(theDots, function(x) if(is(x, "Tensor")) x else Tensor$new(x)))
                      }

                      # function is single input operation, so take last node
                      # need to get this before all the subsequent nodes added
                      input_shapes = if(length(self$graph) > 0) tail(self$graph, 1)[[1]]$output_shapes else list()

                      # node_start = length(self$graph) + 1
                      # f(self)
                      # node_end = length(self$graph)
                      #
                      # # consolidate operations to single node
                      # ops = unlist(lapply(self$graph[node_start:node_end], function(node) node$ops))
                      #

                      ### The output_shapes needs to somehow be dynamically determined

                      # # function may change shape
                      # output_shapes = list(tail(self$graph)[[1]]$output_shapes)
                      output_shapes = input_shapes

                      input_tensors = if(length(theDots) == 0){
                        # lapply(self$graph, function(x) unlist(x$input_tensors))
                        list()
                      }else{
                        # c(theDots, lapply(self$graph, function(x) unlist(x$input_tensors)))
                        theDots
                      }

                      Node$new(self,
                               ops = list(Operation$new(f)),
                               name = name,
                               input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                               output_nodes = list(),
                               input_tensors = input_tensors,
                               input_shapes = input_shapes,
                               output_shapes = output_shapes)

                      # remove redundant Nodes
                      # self$graph[node_start:node_end] = NULL

                      invisible(self)

                      # name = private$.createName(name)
                      #
                      # # function is single input operation, so take last node
                      # input_shapes = if(length(self$graph) > 0) tail(self$graph$output_shapes, 1) else list()
                      # # function doesn't change shape
                      # output_shapes = list(1, prod(input_shapes[[1]]))
                      #
                      # Node$new(self,
                      #          ops = list(Operation$new("cumsum")),
                      #          name,
                      #          input_nodes = if(length(self$graph) > 0) tail(self$graph, 1) else list(),
                      #          output_nodes = list(),
                      #          input_tensors = list(),
                      #          input_shapes = input_shapes,
                      #          output_shapes = output_shapes)
                      #
                      # invisible(self)
                    },

                    drop = function(idx = NA, name = NA){
                      # remove an operation by index, name, or 'pop'
                      if(!is.na(idx)){
                        stop("index dropping not yet supported")
                      }
                      if(!is.na(name)){
                        stop("name dropping not yet supported")
                      }
                      self$graph[[length(self$graph)]] = NULL
                      if(length(self$graph) > 0){
                        # update output_nodes if any precursor nodes
                        self$graph[[length(self$graph)]]$output_nodes = NULL
                      }
                      return(invisible(self))
                    },

                    has_history = function(){
                      return(private$.has_history)
                    }
                  ),
                  private = list(
                    .shape = NULL,
                    .initializer = FALSE,
                    .has_history = NULL,

                    .get_shape = function(value){

                      if(is(value, "R6")){
                        cls = tail(class(value), 2)[1]
                        if(missing(shape)){
                          shape = value$shape
                        }
                      }else{
                        cls = class(value)
                      }

                      out = switch(cls,
                                   "Tensor" = value$shape,
                                   "integer" = length(value),
                                   "numeric" = length(value),
                                   "matrix" = dim(value),
                                   "array" = dim(value),
                                   {
                                     if(is(value, "gpuMatrix") | is(value, "vclMatrix")){
                                       dim(value)
                                     }else{
                                       stop(paste0("unrecognized class: ", class(value)))
                                     }
                                   }
                      )
                      return(out)
                    },
                    .args = NULL,

                    .shared_env = new.env(),

                    .getCounter = function(){
                      counter = private$.shared_env$counter
                      if( is.null( counter ) )
                        counter =  1
                      return( counter )
                    },

                    .createName = function(name){
                      if(is.na(name)){
                        counter = private$.getCounter()
                        private$.shared_env$counter = counter + 1
                        name = tail(paste0(sub("(.*)\\$", "", as.list(sys.call(-1))[[1]]), "_", counter), 1)
                      }
                      return(name)
                    }
                  )
)


#' @title Placeholder Tensor
#' @rdname Tensor-class
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


#' @rdname Tensor-class
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


#' @rdname Tensor-class
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

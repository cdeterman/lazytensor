Node <- R6Class(
  "Node",
  public = list(

    name = NULL,
    ops = NULL,
    input_nodes = NULL,
    output_nodes = NULL,
    input_tensors = NULL,
    input_shapes = NULL,
    output_shapes = NULL,

    initialize = function(tensor,
                          ops,
                          input_nodes,
                          output_nodes,
                          input_tensors,
                          input_shapes,
                          output_shapes,
                          name){
      # set node name
      self$name = name

      # set operations
      self$ops = ops

      # connections
      self$input_nodes = input_nodes
      self$output_nodes = output_nodes

      # tensors
      self$input_tensors = input_tensors
      # self$output_tensors = output_tensors

      # shapes
      self$input_shapes = input_shapes
      self$output_shapes = output_shapes

      for(input in input_nodes){
        input$output_nodes[[length(input$outbound_nodes) + 1]] = self
      }
      # tensor$inbound_nodes[[length(tensor$inbound_nodes) + 1]] = self
      tensor$graph[[length(tensor$graph) + 1]] = self
    }
  )
)

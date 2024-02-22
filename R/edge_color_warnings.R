edge_color_warnings <- function(edges_color,neg_edges_color){
  #Add warning messages.
    # Check whether each element of edges_color and net_edges color is a valid color

  if (length(edges_color) > 2) {stop("edges_color has more then 2 elements")}
  if (length(neg_edges_color) > 2) {stop("neg_edges_color has more then 2 elements")}
  if (is_valid_color(color_v = edges_color) == FALSE){stop("one or more colors in edges_color are not valid")}
  if (is_valid_color(color_v = neg_edges_color) == FALSE){stop("one or more colors in neg_edges_color are not valid")}}
